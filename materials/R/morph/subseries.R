#' Animate a time plot becoming a seasonal subseries plot
#'
#' The first frame is `autoplot()` and the last frame is `gg_subseries()`, and
#' the steps between them are described by `.morph-step` divs in the document.
#'
#' @param data A tsibble containing a single time series.
#' @param y The variable to plot.
#' @param id A unique identifier for the animation on the page.
#' @param width,height Size of the plot in inches.
#' @param palette Colours used to identify each season during the animation.
#' @param alt Alternative text describing the animation.
subseries_morph <- function(
  data,
  y,
  id = "subseries-morph",
  width = 9,
  height = 5.6,
  palette = c("#D55E00", "#0072B2", "#009E73", "#CC79A7"),
  alt = NULL
) {
  y <- rlang::enquo(y)
  morph_font_setup()
  from <- morph_plot_parts(autoplot(data, !!y), width, height)
  to <- morph_plot_parts(gg_subseries(data, !!y), width, height)

  # The observations, with their season from the facet they appear in
  obs <- from$layers[[1]]
  sub <- to$layers[[1]]
  # Matched on the values in time order, as gg_subseries() may place x in a
  # different unit. Each facet's observations are in time order too, so an
  # observation belongs to the facet whose next unused value equals its own.
  obs <- obs[order(obs$x), ]
  sub <- sub[order(as.integer(sub$PANEL), sub$x), ]
  queue <- split(seq_len(nrow(sub)), as.integer(sub$PANEL))
  obs$season <- NA_integer_
  obs$sx <- NA_real_
  for (i in seq_len(nrow(obs))) {
    hit <- vapply(queue, function(q) length(q) > 0 && isTRUE(all.equal(sub$y[q[1]], obs$y[i])), NA)
    k <- which(hit)[1]
    stopifnot(!is.na(k))
    obs$season[i] <- k
    obs$sx[i] <- sub$x[queue[[k]][1]]
    queue[[k]] <- queue[[k]][-1]
  }
  n_seasons <- length(to$panels)
  if (n_seasons > length(palette)) {
    palette <- grDevices::hcl.colors(n_seasons, "Dark 3")
  }

  means <- to$layers[[2]]
  means <- means[match(seq_len(n_seasons), as.integer(means$PANEL)), ]

  scene_data <- list(
    width = from$width,
    height = from$height,
    from = list(chrome = from$chrome, grill = from$grills[[1]], panel = from$panels[[1]]),
    to = list(chrome = to$chrome, grills = to$grills, panels = to$panels),
    series = list(
      x = obs$x,
      y = obs$y,
      season = obs$season,
      sx = obs$sx,
      colour = obs$colour[1],
      stroke_width = obs$stroke_width[1]
    ),
    means = list(
      y = means$yintercept,
      colour = means$colour[1],
      stroke_width = means$stroke_width[1]
    ),
    palette = palette[seq_len(n_seasons)]
  )

  morph_widget(id, "subseries", scene_data, n_steps = 5, alt = alt)
}
