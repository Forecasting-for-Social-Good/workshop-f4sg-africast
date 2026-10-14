#' Animate a time plot becoming a seasonal plot
#'
#' The first frame is `autoplot()` and the last frame is `gg_season()`, with
#' the lines between seasonal periods kept and split half-way between them.
#' The periods are stacked one at a time while the x range narrows to fit
#' them, ending at the seasonal plot's range.
#' The steps between them are described by `.morph-step` divs in the document.
#'
#' @param data A tsibble containing a single time series.
#' @param y The variable to plot.
#' @param id A unique identifier for the animation on the page.
#' @param width,height Size of the plot in inches.
#' @param stack_duration Time (ms) to stack each period onto the first.
#' @param alt Alternative text describing the animation.
season_morph <- function(
  data,
  y,
  id = "season-morph",
  width = 9,
  height = 5.6,
  stack_duration = 250,
  alt = NULL
) {
  y <- rlang::enquo(y)
  morph_font_setup()
  from_p <- autoplot(data, !!y)
  to_p <- gg_season(data, !!y)

  # The time plot with the seasonal plot's colour legend
  data$.period <- rlang::eval_tidy(to_p$mapping$colour, to_p$data)
  mid_p <- autoplot(data, !!y) +
    ggplot2::geom_line(ggplot2::aes(colour = .data$.period), alpha = 0) +
    to_p$scales$get_scales("colour") +
    ggplot2::labs(colour = NULL)
  # ... and without its time axis, which is wrong while the range narrows
  stack_p <- mid_p +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = "transparent"),
      axis.ticks.x = ggplot2::element_line(colour = "transparent"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )

  from <- morph_plot_parts(from_p, width, height)
  mid <- morph_plot_parts(mid_p, width, height)
  stack <- morph_plot_parts(stack_p, width, height)
  to <- morph_plot_parts(to_p, width, height)

  # The observations in time order, with the period they appear in and their
  # position within it
  obs <- from$layers[[1]]
  obs <- obs[order(obs$x), ]
  season <- to$layers[[1]]
  stopifnot(nrow(obs) == nrow(season), isTRUE(all.equal(obs$y, season$y)))

  # gg_season() draws the periods end to end on its x scale and wraps them
  # onto the seasonal axis when drawing, so wrap x within the panel range, and
  # find each period from the colour of its line.
  xr <- to$panels[[1]]$xrange
  season$x <- xr[1] + (season$x - xr[1]) %% diff(xr)
  season$group <- cumsum(c(TRUE, season$colour[-1] != season$colour[-nrow(season)]))
  # The seasonal x axis may be in different units to the time axis (such as
  # quarters rather than days), so convert it to time units. The scene only
  # moves each period by a shift, plus a small residual for uneven lengths.
  same <- season$group[-1] == season$group[-nrow(season)]
  unit <- stats::median((diff(obs$x) / diff(season$x))[same])
  season$x <- season$x * unit
  to$panels[[1]]$xrange <- xr * unit

  panel <- function(parts) {
    list(chrome = parts$chrome, grill = parts$grills[[1]], panel = parts$panels[[1]])
  }
  period <- match(season$group, unique(season$group))
  scene_data <- list(
    width = from$width,
    height = from$height,
    from = panel(from),
    mid = panel(mid),
    stack = panel(stack),
    to = panel(to),
    series = list(
      x = obs$x,
      y = obs$y,
      sx = season$x,
      period = period,
      colour = obs$colour[1],
      stroke_width = obs$stroke_width[1]
    ),
    periods = list(
      colour = season$colour[!duplicated(season$group)],
      stroke_width = season$stroke_width[1]
    )
  )

  n_periods <- max(period)
  morph_widget(
    id, "season", scene_data,
    n_steps = 4,
    durations = c(1100, max(1100, stack_duration * (n_periods - 1)), 1100),
    alt = alt
  )
}
