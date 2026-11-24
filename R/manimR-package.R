#' @keywords internal
#' @aliases manimR-package
"_PACKAGE"

#' manimR: A Grammar of Mathematical Animation for R
#'
#' @description
#' Create mathematical animations using a tidyverse-friendly grammar of motion.
#' manimR makes it as easy to create stunning mathematical visualizations as it
#' is to create plots with ggplot2.
#'
#' @section Getting Started:
#' The core workflow in manimR follows a simple pattern:
#'
#' 1. Create a scene with [scene()]
#' 2. Add objects with [add_text()], [add_latex()], [add_shape()], etc.
#' 3. Animate objects with [write_in()], [fade_in()], [transform_to()], etc.
#' 4. Render with [render()] or preview with [preview()]
#'
#' @section Example:
#' ```r
#' library(manimR)
#'
#' scene("my_first_animation") %>%
#'   add_latex("E = mc^2") %>%
#'   write_in(duration = 2) %>%
#'   pause(1) %>%
#'   highlight_term("c^2", color = "yellow") %>%
#'   render()
#' ```
#'
#' @section Learn More:
#' * `vignette("getting-started")` - Introduction to manimR
#' * `vignette("mathematical-notation")` - Working with LaTeX
#' * `vignette("tidyverse-integration")` - Data-driven animations
#'
#' @section Package Options:
#' manimR uses the following options:
#' * `manimR.render_quality` - Default render quality ("low", "medium", "high", "4K")
#' * `manimR.fps` - Default frames per second (default: 30)
#' * `manimR.cache` - Enable caching (default: TRUE
#' * `manimR.verbose` - Show progress messages (default: TRUE)
#'
#' @docType package
#' @name manimR
NULL

## usethis namespace: start
#' @useDynLib manimR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
