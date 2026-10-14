#' Animate a time plot becoming a horizon plot
#'
#' The first frame is `autoplot()` of one series and the last frame is
#' `ggHoriPlot::geom_horizon()` of every series, faceted by key. The series
#' is split into bands about the origin, the bands below the origin are
#' mirrored above it, and the bands are collapsed onto the origin. The
#' horizon plot of the series then takes its place among the others.
#' The steps between them are described by `.morph-step` divs in the document.
#'
#' @param data A tsibble containing time series identified by a single key.
#' @param y The variable to plot.
#' @param focus The key value of the series used to explain the horizon plot.
#' @param origin,horizonscale The origin and cutpoints of the bands, passed
#'   to `ggHoriPlot::geom_horizon()`.
#' @param palette The `ggHoriPlot::scale_fill_hcl()` palette for the bands.
#' @param id A unique identifier for the animation on the page.
#' @param width,height Size of the plot in inches.
#' @param alt Alternative text describing the animation.
horizon_morph <- function(
  data,
  y,
  focus,
  origin = "midpoint",
  horizonscale = 6,
  palette = "RdBu",
  id = "horizon-morph",
  width = 9,
  height = 5.6,
  alt = NULL
) {
  y <- rlang::enquo(y)
  key <- tsibble::key_vars(data)
  stopifnot(length(key) == 1)
  index <- tsibble::index(data)
  morph_font_setup()

  focal <- data[data[[key]] == focus, ]
  focal <- tsibble::update_tsibble(focal, key = NULL)
  from_p <- autoplot(focal, !!y)

  to_p <- ggplot2::ggplot(data, ggplot2::aes(!!index, !!y)) +
    ggHoriPlot::geom_horizon(
      ggplot2::aes(fill = ggplot2::after_stat(Cutpoints)),
      origin = origin,
      horizonscale = horizonscale
    ) +
    ggHoriPlot::scale_fill_hcl(palette = palette) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data[[key]])) +
    ggplot2::labs(y = NULL, fill = rlang::as_label(y)) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(2, "pt"),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0)
    )
  # ggHoriPlot uses the deprecated `size` aesthetic
  to <- suppressWarnings(morph_plot_parts(to_p, width, height))
  bands <- to$layers[[1]]
  built <- suppressWarnings(ggplot2::ggplot_build(to_p))
  cutpoints <- built$plot$scales$get_scales("fill")$get_limits()

  # The time plot with the horizon plot's fill legend
  keys <- data.frame(
    x = min(from_p$data[[rlang::as_name(index)]]),
    y = rlang::eval_tidy(y, from_p$data)[1],
    band = factor(cutpoints, levels = cutpoints)
  )
  mid_p <- from_p +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = x, xmax = x, ymin = y, ymax = y, fill = band),
      data = keys,
      alpha = 0,
      inherit.aes = FALSE
    ) +
    to_p$scales$get_scales("fill") +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::labs(fill = rlang::as_label(y))
  # ... and without its y-axis, which is wrong while the bands collapse
  stack_p <- mid_p +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = "transparent"),
      axis.ticks.y = ggplot2::element_line(colour = "transparent"),
      axis.title.y = ggplot2::element_text(colour = "transparent"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  from <- morph_plot_parts(from_p, width, height)
  mid <- morph_plot_parts(mid_p, width, height)
  stack <- morph_plot_parts(stack_p, width, height)

  # The cutpoints of the focal series, as geom_horizon() computes them
  focus_panel <- match(format(focus), vapply(to$panels, `[[`, "", "label"))
  cuts <- ggHoriPlot:::compute_ori_sca(
    data.frame(y = rlang::eval_tidy(y, focal), outlier = TRUE),
    origin,
    horizonscale
  )
  ori <- cuts$ori
  pos <- cuts$vec_cut[cuts$vec_cut > ori]
  neg <- cuts$vec_cut[cuts$vec_cut < ori]
  # Each band as a sign, the distance of its inner edge from the origin, and
  # its width
  band_shape <- function(group) {
    i <- as.integer(sub("^y(pos|neg)", "", group))
    edges <- if (startsWith(group, "ypos")) c(ori, pos) else c(ori, neg)
    list(
      sign = if (startsWith(group, "ypos")) 1 else -1,
      offset = abs(edges[i] - ori),
      step = abs(edges[i + 1] - edges[i])
    )
  }

  # Bands of each panel, in the order they are drawn, with the shape of the
  # focal series' bands
  panel_bands <- lapply(seq_along(to$panels), function(k) {
    d <- bands[as.integer(bands$PANEL) == k, ]
    lapply(sort(unique(d$group)), function(g) {
      b <- d[d$group == g, ]
      b <- b[order(b$x), ]
      stopifnot(all(b$ymin == 0))
      band <- list(x = b$x, v = b$ymax, fill = b$fill[1])
      if (k == focus_panel) band <- c(band, band_shape(g))
      band
    })
  })

  panel <- function(parts) {
    list(chrome = parts$chrome, grill = parts$grills[[1]], panel = parts$panels[[1]])
  }
  obs <- from$layers[[1]]
  obs <- obs[order(obs$x), ]
  scene_data <- list(
    width = from$width,
    height = from$height,
    from = panel(from),
    mid = panel(mid),
    stack = panel(stack),
    to = list(chrome = to$chrome, grills = to$grills, panels = to$panels),
    focus = focus_panel - 1,
    origin = ori,
    series = list(
      x = obs$x,
      y = obs$y,
      colour = obs$colour[1],
      stroke_width = obs$stroke_width[1]
    ),
    bands = panel_bands
  )

  morph_widget(id, "horizon", scene_data, n_steps = 5, alt = alt)
}
