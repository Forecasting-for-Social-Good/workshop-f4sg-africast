# Capture PDF handouts of the revealjs slides with decktape (run in docker)
# and bundle them into slides.zip.
#
# Each PDF is cached in materials/pdf/ against the md5 of the HTML it was
# captured from (materials/pdf/checksums.md5), so decktape only re-runs for
# decks whose rendered HTML (or the capture settings below) has changed.
# slides.zip is only rebuilt when a PDF changes.
decktape_image <- "astefanutti/decktape:3.12.0"
decktape_args <- c("reveal", "--size", "1280x720", "--pause", "500")

# decktape de-duplicates images across slides by hashing only their colour
# data, ignoring the alpha mask. Plots drawn in one colour on a transparent
# background (e.g. base graphics) then all collapse into the first such plot.
# Patch the digest to also hash the soft mask.
decktape_patch <- c(
  "const digest = crypto.createHash('SHA1').update(object.contents).digest('hex');" =
  paste(
    "const hash = crypto.createHash('SHA1').update(object.contents);",
    "const smask = object.dict.get(PDFName.of('SMask'));",
    "if (smask) hash.update(page.node.context.lookup(smask).contents);",
    "const digest = hash.digest('hex');"
  )
)

html <- list.files("materials", pattern = "^[0-9].*\\.html$", full.names = TRUE)
pdf_dir <- file.path("materials", "pdf")
pdf <- file.path(pdf_dir, sub("\\.html$", ".pdf", basename(html)))
dir.create(pdf_dir, showWarnings = FALSE)

# Cached checksums, keyed by HTML file name
checksum_file <- file.path(pdf_dir, "checksums.md5")
cached <- character()
if (file.exists(checksum_file)) {
  lines <- strsplit(readLines(checksum_file), "  ", fixed = TRUE)
  cached <- setNames(
    vapply(lines, `[`, "", 1),
    vapply(lines, `[`, "", 2)
  )
}
current <- setNames(unname(tools::md5sum(html)), basename(html))

# Re-capture everything when the capture settings change
settings <- tempfile()
writeLines(c(decktape_image, decktape_args, names(decktape_patch), decktape_patch), settings)
current <- c(".settings" = unname(tools::md5sum(settings)), current)
if (!identical(cached[".settings"], current[".settings"])) cached <- character()

# Drop PDFs for decks that no longer exist
unlink(setdiff(list.files(pdf_dir, "\\.pdf$", full.names = TRUE), pdf))

stale <- !file.exists(pdf) | is.na(cached[basename(html)]) |
  cached[basename(html)] != current[basename(html)]
stale[is.na(stale)] <- TRUE

if (any(stale) && Sys.which("docker") == "") {
  warning(
    "docker is not available; PDFs are out of date for: ",
    paste(basename(html[stale]), collapse = ", "),
    call. = FALSE
  )
} else if (any(stale)) {
  decktape_js <- tempfile(fileext = ".js")
  src <- system2("docker", c(
    "run", "--rm", "--entrypoint", "cat", decktape_image, "/decktape/decktape.js"
  ), stdout = TRUE)
  for (old in names(decktape_patch)) {
    if (!any(grepl(old, src, fixed = TRUE))) {
      stop("Could not patch decktape.js; check decktape_patch against ", decktape_image)
    }
    src <- sub(old, decktape_patch[[old]], src, fixed = TRUE)
  }
  writeLines(src, decktape_js)
  Sys.chmod(decktape_js, "644")

  for (i in which(stale)) {
    message("Capturing ", pdf[i], " with decktape")
    status <- system2("docker", c(
      "run", "--rm",
      "-u", paste0(system("id -u", intern = TRUE), ":", system("id -g", intern = TRUE)),
      "-v", paste0(normalizePath("materials"), ":/slides:ro"),
      "-v", paste0(normalizePath(pdf_dir), ":/out"),
      "-v", paste0(decktape_js, ":/decktape/decktape.js:ro"),
      decktape_image, decktape_args,
      shQuote(file.path("/slides", basename(html[i]))),
      shQuote(file.path("/out", basename(pdf[i])))
    ), stdout = FALSE)
    if (status != 0 || !file.exists(pdf[i])) {
      warning("decktape failed for ", html[i], call. = FALSE)
      current[basename(html[i])] <- NA
    }
  }
  current <- current[!is.na(current)]
  writeLines(paste0(current, "  ", names(current)), checksum_file)
}

# Rebuild slides.zip when its PDFs have changed
pdf <- pdf[file.exists(pdf)]
rezip <- !file.exists("slides.zip") ||
  !setequal(utils::unzip("slides.zip", list = TRUE)$Name, basename(pdf)) ||
  any(file.mtime(pdf) > file.mtime("slides.zip"))
if (rezip) {
  unlink("slides.zip")
  zip("slides.zip", pdf, flags = "-j9X")
}
