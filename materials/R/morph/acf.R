#' Animate a time plot becoming an ACF plot
#'
#' The first frame is `autoplot()` of the series and the last frame is
#' `autoplot()` of its `ACF()`. The time plot shrinks into a strip along the
#' top, where the series is paired with a lagged copy of itself. The pairs
#' become a lag plot below it, beside a growing ACF plot, and the
#' autocorrelation at each lag is added as the lagged copy shifts along and
#' its lag plot is shown. The lags pause at the seasonal lag. The time plot
#' and lag plot are then removed, leaving the ACF plot.
#' The steps between them are described by `.morph-step` divs in the document.
#'
#' @param data A tsibble containing a single regular time series.
#' @param y The variable to plot.
#' @param id A unique identifier for the animation on the page.
#' @param width,height Size of the plot in inches.
#' @param strip Proportion of the height taken by the time plot, above the lag
#'   and ACF plots.
#' @param pause_lag The lag to pause at, by default the seasonal period.
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
  strip = 0.4,
  pause_lag = tsibble::guess_frequency(data[[tsibble::index_var(data)]]),
  lag_duration = 450,
  lag_colour = "#D55E00",
  alt = NULL
) {
  y <- rlang::enquo(y)
  morph_font_setup()
  from_p <- autoplot(data, !!y)
  # Lags are broken at multiples of the seasonal period, as the development
  # ggtime does for mixtime lags
  to_p <- autoplot(feasts::ACF(data, !!y)) +
    ggplot2::scale_x_continuous(breaks = function(x) seq(0, x[2], by = pause_lag))
  from <- morph_plot_parts(from_p, width, height)
  to <- morph_plot_parts(to_p, width, height)

  obs <- from$layers[[1]]
  obs <- obs[order(obs$x), ]
  y_label <- from_p$labels$y %||% rlang::as_label(y)

  # The lag plot and the ACF plot being built, side by side. Both axes of the
  # lag plot span the series, so they stay fixed as the lag changes.
  limits <- range(obs$y)
  lag_p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = limits) +
    ggplot2::scale_y_continuous(limits = limits) +
    ggplot2::labs(x = paste("Lagged", rlang::as_label(y)), y = y_label)
  # All have an empty title, to make room for the lag and autocorrelation
  top <- strip * height
  time <- morph_plot_parts(from_p + ggplot2::labs(title = " "), width, top)
  lag <- morph_plot_parts(lag_p + ggplot2::labs(title = " "), width / 2, height - top)
  bars <- morph_plot_parts(to_p + ggplot2::labs(title = " "), width / 2, height - top)

  # Each view is placed at (x, y) on the full plot, in pt
  panel <- function(parts, x = 0, y = 0) {
    list(
      chrome = parts$chrome,
      grill = parts$grills[[1]],
      panel = parts$panels[[1]],
      width = parts$width,
      height = parts$height,
      x = x,
      y = y
    )
  }
  # The unit of the lags, such as "quarter", from the mixtime time unit class
  # or else the non-zero field of a tsibble interval
  interval <- tsibble::interval(data)
  unit <- if (S7::S7_inherits(interval)) {
    sub("^tu_", "", S7::S7_class(interval)@name)
  } else {
    Filter(function(f) vctrs::field(interval, f) != 0, vctrs::fields(interval))[1]
  }

  acf <- to$layers[[1]]
  scene_data <- list(
    width = from$width,
    height = from$height,
    from = panel(from),
    time = panel(time),
    lag = panel(lag, 0, top * 72),
    bars = panel(bars, from$width / 2, top * 72),
    to = panel(to),
    series = list(
      x = obs$x,
      y = obs$y,
      label = y_label,
      colour = obs$colour[1],
      stroke_width = obs$stroke_width[1]
    ),
    acf = list(
      lag = acf$x,
      value = acf$y,
      unit = unit,
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
    lag_colour = lag_colour,
    pause_lag = pause_lag
  )

  n_lags <- nrow(acf)
  stopifnot(pause_lag > 1, pause_lag < n_lags)
  morph_widget(
    id, "acf", scene_data,
    n_steps = 7,
    durations = c(
      1400, 1400, 1100,
      max(1100, lag_duration * (pause_lag - 1)),
      max(1100, lag_duration * (n_lags - pause_lag)),
      1400
    ),
    alt = alt
  )
}
