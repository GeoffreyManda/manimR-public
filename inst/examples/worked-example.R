library(manimR)

# A short scene with a growing circle. No TeX installation is required.
s <- scene("growing-circle")
add_circle(s, radius = 1, color = "#007AFF")
grow_from_center(s, duration = 0.5)
stopifnot(s$get_duration() > 0)

# Render a GIF into a disposable path; supply your own path to retain it.
local({
  path <- tempfile(fileext = ".gif")
  on.exit(unlink(path), add = TRUE)
  render(s, output = path, format = "gif", quality = "low", fps = 4,
         open = FALSE, progress = FALSE, cache = FALSE)
  stopifnot(file.exists(path), file.info(path)$size > 100)
  cat("Growing-circle GIF rendered successfully.\n")
})
