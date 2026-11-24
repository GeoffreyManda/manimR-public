#' @title Bezier Curve Object
#' @description Explicit cubic Bezier curve from control points.
#' @name geometry_bezier
NULL

#' CubicBezier R6 Class
#'
#' @description A cubic Bezier curve defined by four control points.
#' @export
CubicBezier <- R6::R6Class(
  "CubicBezier",
  inherit = VMobject,
  public = list(
    #' @description Create a new CubicBezier
    #' @param p0 Start point
    #' @param p1 First control handle
    #' @param p2 Second control handle
    #' @param p3 End point
    #' @param color Color
    #' @param ... Additional args
    initialize = function(p0 = c(-1, 0, 0), p1 = c(-0.5, 1, 0),
                          p2 = c(0.5, 1, 0), p3 = c(1, 0, 0),
                          color = WHITE, ...) {
      super$initialize(color = color, ...)
      self$add_cubic_bezier(
        validate_position(p0),
        validate_position(p1),
        validate_position(p2),
        validate_position(p3)
      )
    }
  )
)
