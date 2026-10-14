# Helpers for animating transitions between ggplot2 graphics.
#
# The approach splits each plot into parts that can be recombined in the
# browser so the first and last frames match the static ggplot output exactly:
#
# * chrome: everything outside the panels (axes, strips, labels), drawn by R.
# * panel grill: each panel's background and grid lines, drawn by R in place.
# * layers: the geom data (in data units) and panel geometry (in SVG units),
#   drawn and animated in the browser.
#
# Every plot must be drawn at the same size so their coordinates line up.

# Make the book's font available to R for accurate text metrics, downloading
# it if it is not installed
morph_font_setup <- function(family = "Fira Sans") {
  systemfonts::require_font(family, verbose = FALSE)
  invisible(family)
}

# Draw a grob into an SVG string of the given size (in inches)
morph_svg_string <- function(grob, width, height, bg = "#fafafa") {
  s <- svglite::svgstring(width = width, height = height, bg = bg)
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()
  svg <- utils::tail(as.character(s()), 1)
  # Strip the XML declaration so the SVG can be inlined into HTML
  sub("^<\\?xml[^>]*>\\s*", "", svg)
}

# Convert ggplot2 linewidths (mm) into SVG stroke widths (pt), rounded as
# svglite does
morph_stroke_width <- function(linewidth) {
  round(linewidth * ggplot2::.pt * 72 / 96, 2)
}

#' Extract the pieces of a ggplot needed to animate it in the browser
#'
#' @param p A ggplot object.
#' @param width,height Size of the plot in inches.
#'
#' @return A list containing the SVG size (pt), the chrome SVG, a full
#'   reference SVG, and per-panel geometry, grill SVGs and layer data.
morph_plot_parts <- function(p, width = 9, height = 5.6) {
  # Build the gtable while the SVG device is open so text metrics match
  s <- svglite::svgstring(width = width, height = height)
  built <- ggplot2::ggplot_build(p)
  gt <- ggplot2::ggplot_gtable(built)
  grid::grid.newpage()
  grid::grid.draw(gt)

  # Panels ordered to match PANEL (row-major)
  panel_idx <- which(grepl("^panel", gt$layout$name))
  panel_idx <- panel_idx[order(gt$layout$t[panel_idx], gt$layout$l[panel_idx])]

  layout <- built$layout$layout
  facet_vars <- setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD"))

  # Locate each panel on the device (SVG y increases downwards)
  panels <- lapply(seq_along(panel_idx), function(i) {
    l <- gt$layout[panel_idx[i], ]
    vp <- sprintf("%s.%i-%i-%i-%i", l$name, l$t, l$r, l$b, l$l)
    grid::seekViewport(vp)
    lo <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"))
    hi <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"))
    grid::upViewport(0)
    pp <- built$layout$panel_params[[i]]
    list(
      panel = i,
      label = paste(vapply(facet_vars, function(v) format(layout[[v]][i]), ""), collapse = ", "),
      x = as.numeric(lo$x) * 72,
      y = (height - as.numeric(hi$y)) * 72,
      w = (as.numeric(hi$x) - as.numeric(lo$x)) * 72,
      h = (as.numeric(hi$y) - as.numeric(lo$y)) * 72,
      xrange = pp$x$continuous_range,
      yrange = pp$y$continuous_range
    )
  })
  grDevices::dev.off()
  full <- sub("^<\\?xml[^>]*>\\s*", "", utils::tail(as.character(s()), 1))

  # Panel backgrounds and grid lines, each drawn alone in its place on the
  # full plot so coordinates are rounded exactly as in the static plot
  grills <- lapply(seq_along(panel_idx), function(i) {
    grill <- gt
    grill$grobs <- rep(list(ggplot2::zeroGrob()), length(gt$grobs))
    panel <- gt$grobs[[panel_idx[i]]]
    panel$children <- panel$children[grep("^grill", names(panel$children))[1]]
    grill$grobs[[panel_idx[i]]] <- panel
    morph_svg_string(grill, width, height, bg = "transparent")
  })

  # Everything except the panels
  chrome <- gt
  chrome$grobs[panel_idx] <- list(ggplot2::zeroGrob())
  chrome <- morph_svg_string(chrome, width, height)

  # Layer data in data units, with SVG stroke widths
  layers <- lapply(built$data, function(d) {
    if ("linewidth" %in% names(d)) {
      d$stroke_width <- morph_stroke_width(d$linewidth)
    }
    d$colour <- grDevices::rgb(t(grDevices::col2rgb(d$colour)), maxColorValue = 255)
    as.data.frame(d[intersect(
      names(d),
      c("x", "y", "ymin", "ymax", "yintercept", "PANEL", "group", "colour", "fill", "stroke_width")
    )])
  })

  list(
    width = width * 72,
    height = height * 72,
    panels = panels,
    grills = grills,
    chrome = chrome,
    full = full,
    layers = layers
  )
}

#' Create an HTML slide animation, stepped by revealjs fragments, between plots
#'
#' The steps of the animation are the `.morph-step` divs inside the
#' slide's `.fragment.morph-step` divs (step 0 is a plain `.morph-step`).
#'
#' @param id A unique identifier for the animation on the page.
#' @param scene The name of the JavaScript scene that animates the plots,
#'   defined in `R/morph/<scene>.js`.
#' @param data A list of data for the scene, converted to JSON.
#' @param n_steps The number of steps in the scene (including the first).
#' @param durations Optional time (ms) to animate between each pair of
#'   consecutive steps, of length `n_steps - 1`.
#' @param alt Alternative text describing the animation.
morph_widget <- function(id, scene, data, n_steps, durations = NULL, alt = NULL) {
  # Resolved relative to the slide deck's directory (materials/)
  read <- function(...) paste(readLines(file.path("R", "morph", ...)), collapse = "\n")
  css <- read("common", "morph.css")
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, digits = NA, dataframe = "columns")
  html <- c(
    "```{=html}",
    sprintf("<style>%s</style>", css),
    sprintf(
      "<figure class=\"morph-figure\" id=\"%s\" data-scene=\"%s\" data-steps=\"%i\"%s role=\"img\"%s></figure>",
      id, scene, n_steps,
      if (is.null(durations)) "" else sprintf(" data-durations=\"%s\"", paste(round(durations), collapse = ",")),
      if (is.null(alt)) "" else sprintf(" aria-label=\"%s\"", htmltools::htmlEscape(alt, attribute = TRUE))
    ),
    sprintf("<script type=\"application/json\" id=\"%s-data\">%s</script>", id, json),
    sprintf("<script>%s</script>", read("common", "morph.js")),
    sprintf("<script>%s</script>", read(paste0(scene, ".js"))),
    "```"
  )
  knitr::asis_output(paste(html, collapse = "\n"))
}
