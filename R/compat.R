#' @title API Compatibility Shims
#' @description
#' Convenience wrappers and aliases that match the API shown on the manimR
#' project page (https://reyanda.github.io/projects/manimr/).
#'
#' These shims allow code like:
#' ```r
#' scene <- Scene$new(bg = "#050510")
#' scene$play(Rotate(circle, PI))
#' ```
#' @name compat
#' @keywords internal
NULL


# ── Class aliases ──────────────────────────────────────────────────────────

#' Scene3D class (alias for ThreeDScene)
#'
#' @description
#' `Scene3D` is an alias for [`ThreeDScene`] so that the API shown in the
#' project documentation works directly.
#'
#' @export
Scene3D <- ThreeDScene  # nolint: object_name


# ── Animation convenience functions ────────────────────────────────────────
# Allow Rotate(mob, PI) instead of RotateAnim$new(mob, angle = PI)

#' Rotate animation (convenience function)
#'
#' @description
#' Convenience wrapper for [`RotateAnim`]. Allows `Rotate(mob, PI)` instead
#' of `RotateAnim$new(mob, angle = PI)`.
#'
#' @param mobject Mobject to rotate
#' @param angle Rotation angle in radians (default PI)
#' @param ... Additional args passed to `RotateAnim$new()`
#' @return A RotateAnim Animation R6 object
#' @rdname Rotate-class
#' @export
#' @examples
#' \dontrun{
#' circle <- Circle$new(color = BLUE)
#' scene <- Scene$new()
#' scene$add(circle)
#' scene$play(Rotate(circle, PI))
#' }
Rotate <- function(mobject, angle = PI, ...) {
  RotateAnim$new(mobject = mobject, angle = angle, ...)
}

# ── Fourier / advanced animation stubs ─────────────────────────────────────

#' FourierSeries mobject (stub)
#'
#' @description
#' Placeholder for a Fourier series animation object. Full implementation
#' (rotating circles tracing a path) is planned.
#'
#' @param harmonics Number of Fourier harmonics
#' @param color Color of the circles/trace
#' @param ... Additional parameters
#' @return A named list describing the Fourier series (stub)
#' @export
FourierSeries <- function(harmonics = 8, color = "#a855f7", ...) {
  cli::cli_warn(c(
    "{.fn FourierSeries} is not yet implemented.",
    "i" = "This is a planned feature.",
    "i" = "Track progress at {.url https://github.com/Reyanda/manimR/issues}"
  ))
  structure(
    list(harmonics = harmonics, color = color),
    class = c("FourierSeries", "list")
  )
}

#' DrawCircles animation (stub)
#'
#' @description
#' Placeholder for the Fourier decomposition circle-drawing animation.
#'
#' @param fourier_series A FourierSeries object
#' @param ... Additional parameters
#' @return NULL (invisibly)
#' @export
DrawCircles <- function(fourier_series, ...) {
  cli::cli_warn(c(
    "{.fn DrawCircles} is not yet implemented.",
    "i" = "This is a planned feature for Fourier decomposition animations."
  ))
  invisible(NULL)
}

#' Trace animation (stub)
#'
#' @description
#' Placeholder for tracing the path described by a Fourier series.
#'
#' @param fourier_series A FourierSeries object
#' @param color Trace color
#' @param ... Additional parameters
#' @return NULL (invisibly)
#' @export
Trace <- function(fourier_series, color = "#06b6d4", ...) {
  cli::cli_warn(c(
    "{.fn Trace} is not yet implemented.",
    "i" = "This is a planned feature for Fourier decomposition animations."
  ))
  invisible(NULL)
}

#' OrbitCamera animation (stub)
#'
#' @description
#' Placeholder for a camera orbit animation in 3D scenes. Use
#' `ThreeDScene$begin_ambient_rotation()` as an alternative for now.
#'
#' @param mobject Mobject to orbit around (optional)
#' @param angle Total orbit angle in radians (default TAU = full rotation)
#' @param ... Additional parameters
#' @return NULL (invisibly)
#' @export
OrbitCamera <- function(mobject = NULL, angle = TAU, ...) {
  cli::cli_warn(c(
    "{.fn OrbitCamera} is not yet implemented as a standalone animation.",
    "i" = "Use {.code scene$begin_ambient_rotation(rate = 0.3)} instead."
  ))
  invisible(NULL)
}
