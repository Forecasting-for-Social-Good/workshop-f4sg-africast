zip(
  "slides.zip",
  list.files("_site/materials/", pattern = "\\.pdf$", full.names = TRUE),
  flags = "-j"
)

setwd("exercises/")
unlink("../exercises.zip")
zip(
  "../exercises.zip",
  list.files(".", recursive = TRUE, full.names = TRUE)
)
setwd("../")
