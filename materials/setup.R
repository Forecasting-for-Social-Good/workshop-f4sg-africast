# Set some defaults
options(digits = 3, width = 88)

# Colours to be viridis for continuous scales and Okabe for discrete scales
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
knitr::opts_chunk$set(
  dev.args = list(pointsize = 11, bg = "transparent"),
  # Keep figures outside *_files/, which is removed after embedding resources,
  # so cached chunks still have their figures on re-render
  fig.path = file.path(
    "_figures",
    tools::file_path_sans_ext(knitr::current_input()),
    ""
  ),
  # Reduce plots to 256 colours (visually identical, about half the size)
  fig.process = function(path) {
    if (grepl("\\.png$", path)) {
      magick::image_read(path) |>
        magick::image_quantize(256, colorspace = "sRGB", dither = FALSE) |>
        magick::image_write(path, format = "png")
    }
    path
  }
)

# Graphics in the AFRICAST palette: Fira Sans, slate text, sage gridlines and a
# transparent background so plots sit on the slide's gradient
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = "Fira Sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#434A5C"),
      axis.text = ggplot2::element_text(colour = "#434A5C"),
      panel.grid.major = ggplot2::element_line(colour = "#D9E2C8"),
      panel.grid.minor = ggplot2::element_line(colour = "#E8EEDD"),
      strip.text = ggplot2::element_text(colour = "#434A5C", face = "bold"),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
)
# Render a gganimate animation to an MP4 video that reveal.js autoplays (and
# loops) when its slide is shown. Much smaller than the equivalent GIF.
anim_video <- function(p, width = knitr::opts_current$get("fig.width"),
                       height = knitr::opts_current$get("fig.height"),
                       res = 144, ...) {
  path <- paste0(knitr::fig_path(".mp4"))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  gganimate::animate(
    p, width = width, height = height, units = "in", res = res,
    device = "ragg_png", renderer = gganimate::ffmpeg_renderer(), ...
  ) |>
    gganimate::anim_save(filename = path)
  knitr::asis_output(sprintf(
    '<video data-autoplay loop muted playsinline style="display: block; margin: 0 auto; max-width: 100%%; max-height: 600px;" src="%s"></video>',
    path
  ))
}

# Make Fira Sans available to the ragg graphics device
try(systemfonts::require_font("Fira Sans"), silent = TRUE)

library(fpp3)
library(patchwork)

global_economy <- global_economy |>
  select(Year, Country, GDP, Imports, Exports, Population)
tourism <- tsibble::tourism |>
  mutate(
    State = recode(State,
      "Australian Capital Territory" = "ACT",
      "New South Wales" = "NSW",
      "Northern Territory" = "NT",
      "Queensland" = "QLD",
      "South Australia" = "SA",
      "Tasmania" = "TAS",
      "Victoria" = "VIC",
      "Western Australia" = "WA"
    )
  )  |>
  # Remove "Australia's" from region
  mutate(Region = stringr::str_replace(Region, "Australia's ", ""))
