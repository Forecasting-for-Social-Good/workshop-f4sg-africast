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
  dev.args = list(pointsize = 11),
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

# Font for graphics to be Fira Sans
ggplot2::theme_set(
  ggplot2::theme_get() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      plot.background = ggplot2::element_rect(fill = "#fafafa", color = "#fafafa")
    )
)
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
