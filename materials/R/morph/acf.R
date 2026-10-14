#' Animate a time plot becoming an ACF plot
#'
#' The first frame is `autoplot()` of the series and the last frame is
#' `autoplot()` of its `ACF()`. The series is paired with a lagged copy of
#' itself, the pairs become a lag plot beside a growing ACF plot, and the
#' autocorrelation at each lag is added as its lag plot is shown. The lag plot
#' is then removed, leaving the ACF plot.
#' The steps between them are described by `.morph-step` divs in the document.
#'
#' @param data A tsibble containing a single regular time series.
#' @param y The variable to plot.
#' @param id A unique identifier for the animation on the page.
#' @param width,height Size of the plot in inches.
#' @param lag_duration Time (ms) to show the lag plot of each lag after the
#'   first.
#' @param lag_colour Colour of the lagged series and the current lag's bar.
#' @param alt Alternative text describing the animation.
acf_morph <- function(
  data,
  y,
  id = "acf-morph",
  width = 9,
  height = 5.6,
  lag_duration = 450,
  lag_colour = "#D55E00",
  alt = NULL
) {
  y <- rlang::enquo(y)
  morph_font_setup()
  from_p <- autoplot(data, !!y)
  to_p <- autoplot(feasts::ACF(data, !!y))
  from <- morph_plot_parts(from_p, width, height)
  to <- morph_plot_parts(to_p, width, height)

  obs <- from$layers[[1]]
  obs <- obs[order(obs$x), ]

  # The lag plot and the ACF plot being built, side by side. Both axes of the
  # lag plot span the series, so they stay fixed as the lag changes.
  limits <- range(obs$y)
  lag_p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = limits) +
    ggplot2::scale_y_continuous(limits = limits) +
    ggplot2::labs(
      x = paste("Lagged", rlang::as_label(y)),
      y = from_p$labels$y %||% rlang::as_label(y)
    )
  # Both have an empty title, to make room for the lag and autocorrelation
  lag <- morph_plot_parts(lag_p + ggplot2::labs(title = " "), width / 2, height)
  bars <- morph_plot_parts(to_p + ggplot2::labs(title = " "), width / 2, height)

  panel <- function(parts, offset = 0) {
    list(
      chrome = parts$chrome,
      grill = parts$grills[[1]],
      panel = parts$panels[[1]],
      width = parts$width,
      offset = offset
    )
  }
  acf <- to$layers[[1]]
  scene_data <- list(
    width = from$width,
    height = from$height,
    from = panel(from),
    lag = panel(lag),
    bars = panel(bars, from$width / 2),
    to = panel(to),
    series = list(
      x = obs$x,
      y = obs$y,
      colour = obs$colour[1],
      stroke_width = obs$stroke_width[1]
    ),
    acf = list(
      lag = acf$x,
      value = acf$y,
      colour = acf$colour[1],
      stroke_width = acf$stroke_width[1]
    ),
    zero = list(
      y = to$layers[[2]]$yintercept,
      colour = to$layers[[2]]$colour,
      stroke_width = to$layers[[2]]$stroke_width
    ),
    ci = list(
      y = to$layers[[3]]$yintercept,
      colour = to$layers[[3]]$colour[1],
      stroke_width = to$layers[[3]]$stroke_width[1],
      # svglite draws dashes 4 line widths long
      dasharray = sprintf("%.2f", 4 * ggplot2::ggplot_build(to_p)$data[[3]]$linewidth[1] * ggplot2::.pt)
    ),
    lag_colour = lag_colour
  )

  n_lags <- nrow(acf)
  morph_widget(
    id, "acf", scene_data,
    n_steps = 6,
    durations = c(1100, 1400, 1100, max(1100, lag_duration * (n_lags - 1)), 1400),
    alt = alt
  )
}
