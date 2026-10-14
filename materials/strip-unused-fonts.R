# Remove the Source Sans Pro @font-face rules that Quarto's revealjs theme
# always imports. The decks use Fira Sans, but embed-resources inlines every
# font file of the unused family (~3MB per deck). The rules sit URL-encoded
# inside the theme's data:text/css stylesheet.
files <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n")[[1]]
files <- files[grepl("\\.html$", files) & file.exists(files)]
font_face <- "%40font%2Dface%20%7B(?:(?!%7D)[^\"])*%7D"
for (f in files) {
  html <- readr::read_file(f)
  rules <- regmatches(html, gregexpr(font_face, html, perl = TRUE))[[1]]
  unused <- unique(rules[grepl("Source%20Sans%20Pro", rules, fixed = TRUE)])
  if (length(unused) == 0) next
  for (rule in unused) html <- gsub(rule, "", html, fixed = TRUE)
  readr::write_file(html, f)
}
