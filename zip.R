zip(
  "slides.zip",
  list.files("materials", pattern = "^[0-9].*\\.html$", full.names = TRUE),
  flags = "-j"
)

setwd("exercises/")
unlink("../exercises.zip")
zip(
  "../exercises.zip",
  list.files(".", recursive = TRUE, full.names = TRUE)
)
setwd("../")
