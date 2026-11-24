#' @title Utility Functions for manimR
#' @description Internal utility functions used throughout the package.
#' @name utils
#' @keywords internal
NULL

#' Pipe operator
#'
#' @description
#' See `magrittr::\code{\link[magrittr:pipe]{\%>\%}}` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Null coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Validate position vector
#'
#' @param position A numeric vector of length 2 or 3
#' @param allow_2d Allow 2D positions (default: TRUE)
#' @return A normalized position vector of length 3
#' @keywords internal
#' @noRd
validate_position <- function(position, allow_2d = TRUE) {
  if (is.null(position)) return(c(0, 0, 0))
  if (!is.numeric(position)) cli::cli_abort("Position must be a numeric vector")
  if (length(position) == 2 && allow_2d) return(c(position, 0))
  if (length(position) == 3) return(position)
  cli::cli_abort("Position must be a vector of length 2 or 3, not {length(position)}")
}

#' Validate duration
#'
#' @param duration A numeric value
#' @param arg_name Name of the argument for error messages
#' @return The validated duration
#' @keywords internal
#' @noRd
validate_duration <- function(duration, arg_name = "duration") {
  if (!is.numeric(duration) || length(duration) != 1)
    cli::cli_abort("{.arg {arg_name}} must be a single numeric value")
  if (duration < 0)
    cli::cli_abort("{.arg {arg_name}} must be non-negative")
  duration
}

#' Generate unique object ID
#'
#' @param prefix Optional prefix for the ID
#' @return A unique string identifier
#' @keywords internal
#' @noRd
generate_id <- function(prefix = "obj") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
         sample(1000:9999, 1))
}

#' Linear interpolation
#'
#' @param start Starting value (scalar or vector)
#' @param end Ending value (scalar or vector)
#' @param t Interpolation parameter (0 to 1)
#' @return Interpolated value
#' @keywords internal
#' @noRd
lerp <- function(start, end, t) {
  start + (end - start) * t
}

#' Normalize a vector to unit length
#'
#' @param v Numeric vector
#' @return Unit vector, or zero vector if input is zero
#' @keywords internal
#' @noRd
normalize <- function(v) {
  len <- sqrt(sum(v^2))
  if (len < 1e-10) return(rep(0, length(v)))
  v / len
}

#' Cross product of two 3D vectors
#'
#' @param a Numeric vector of length 3
#' @param b Numeric vector of length 3
#' @return Cross product vector of length 3
#' @keywords internal
#' @noRd
cross3d <- function(a, b) {
  c(a[2]*b[3] - a[3]*b[2],
    a[3]*b[1] - a[1]*b[3],
    a[1]*b[2] - a[2]*b[1])
}

#' Clamp a value to a range
#'
#' @param x Numeric value or vector
#' @param lo Lower bound
#' @param hi Upper bound
#' @return Clamped value(s)
#' @keywords internal
#' @noRd
clamp <- function(x, lo = 0, hi = 1) {
  pmax(lo, pmin(hi, x))
}

#' Angle between two vectors
#'
#' @param v1 First vector
#' @param v2 Second vector
#' @return Angle in radians
#' @keywords internal
#' @noRd
angle_between_vectors <- function(v1, v2) {
  cos_angle <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
  acos(clamp(cos_angle, -1, 1))
}

#' Check if LaTeX is available
#' @return Logical indicating LaTeX availability
#' @keywords internal
#' @noRd
has_latex <- function() {
  nchar(Sys.which("pdflatex")) > 0
}

#' Check if ffmpeg is available
#' @return Logical indicating ffmpeg availability
#' @keywords internal
#' @noRd
has_ffmpeg <- function() {
  nchar(Sys.which("ffmpeg")) > 0
}

#' manimR package options
#'
#' @description Get or set manimR package options.
#'
#' @param ... Options to set (name = value pairs) or get (names only)
#' @return If getting options, returns the option values. If setting, returns
#'   invisible NULL.
#'
#' @examples
#' \dontrun{
#' manimR_options()
#' manimR_options("fps")
#' manimR_options(fps = 60, verbose = FALSE)
#' }
#'
#' @export
manimR_options <- function(...) {
  dots <- list(...)

  defaults <- list(
    render_quality = "medium",
    fps = 30,
    cache = TRUE,
    verbose = TRUE,
    background_color = "#000000",
    default_font = "sans",
    latex_compiler = "pdflatex",
    backend = "auto"
  )

  if (length(dots) == 0) {
    opts <- lapply(names(defaults), function(nm) {
      getOption(paste0("manimR.", nm), defaults[[nm]])
    })
    names(opts) <- names(defaults)
    return(opts)
  }

  if (is.null(names(dots))) {
    opts <- lapply(dots, function(nm) {
      getOption(paste0("manimR.", nm), defaults[[nm]])
    })
    if (length(opts) == 1) return(opts[[1]])
    names(opts) <- unlist(dots)
    return(opts)
  }

  for (nm in names(dots)) {
    options(setNames(list(dots[[nm]]), paste0("manimR.", nm)))
  }
  invisible(NULL)
}
