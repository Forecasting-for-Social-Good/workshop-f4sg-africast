#' Animate the windows made by rolling a tsibble
#'
#' The last frame is a plot of every window (one row per `.id`), with the
#' observations in each window in colour and the rest in grey. The animation
#' starts from the observations alone, colours the first window, and then
#' adds each window as a copy of the one before it that moves down a row
#' while its observations change.
#' The steps between them are described by `.morph-step` divs in the document.
#'
#' @param windows A tsibble from `stretch_tsibble()`, `slide_tsibble()` or
#'   `tile_tsibble()`, with an integer index starting at 1.
#' @param n The number of observations in the rolled tsibble.
#' @param id A unique identifier for the animation on the page.
#' @param width,height Size of the plot in inches.
#' @param row_duration Time (ms) to add each window after the second.
#' @param alt Alternative text describing the animation.
roll_morph <- function(
  windows,
  n,
  id = "roll-morph",
  width = 9,
  height = 5,
  row_duration = 500,
  alt = NULL
) {
  morph_font_setup()
  index <- tsibble::index_var(windows)
  windows <- tibble::as_tibble(windows)
  ids <- unique(windows$.id)
  stopifnot(length(ids) >= 3)

  all_points <- tidyr::expand_grid(.id = ids, t = seq_len(n))
  in_window <- data.frame(.id = windows$.id, t = windows[[index]])
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = .id)) +
    ggplot2::geom_point(data = all_points, colour = "grey80", size = 3) +
    ggplot2::geom_point(data = in_window, colour = "#0072B2", size = 3) +
    ggplot2::scale_y_reverse(breaks = ids) +
    ggplot2::labs(x = "Time", y = "Window (.id)") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  parts <- morph_plot_parts(p, width, height)

  # The point radius, as svglite draws it
  radius <- as.numeric(sub(".*?<circle [^>]*r='([0-9.]+)'.*", "\\1", parts$full))

  # Each window's row (on the transformed y scale) and its observations
  grey <- parts$layers[[1]]
  blue <- parts$layers[[2]]
  rows <- lapply(ids, function(k) {
    y <- grey$y[grey$y == -k][1]
    list(y = y, members = sort(blue$x[blue$y == y]))
  })

  scene_data <- list(
    width = parts$width,
    height = parts$height,
    chrome = parts$chrome,
    grill = parts$grills[[1]],
    panel = parts$panels[[1]],
    x = seq_len(n),
    rows = rows,
    radius = radius,
    grey = grey$colour[1],
    colour = blue$colour[1]
  )

  morph_widget(
    id, "roll", scene_data,
    n_steps = 4,
    durations = c(1100, 1100, row_duration * (length(ids) - 2)),
    alt = alt
  )
}
