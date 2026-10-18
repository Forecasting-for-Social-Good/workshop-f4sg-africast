setwd("exercises/")
unlink("../exercises.zip")
zip(
  "../exercises.zip",
  list.files(".", recursive = TRUE, full.names = TRUE)
)
setwd("../")
