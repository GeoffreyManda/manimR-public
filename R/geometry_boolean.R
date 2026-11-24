#' @title Boolean Geometry Operations
#' @description VMobject subclasses for constructive solid geometry (CSG)
#'   operations: union, intersection, and difference of 2D shapes.
#'   Requires the \pkg{polyclip} package for polygon clipping.
#' @name geometry_boolean
NULL

#' Extract polygon vertices from a VMobject
#'
#' @description Samples the Bezier curves of a VMobject to get a polygon
#'   approximation suitable for polyclip operations.
#' @param vmobject A VMobject with points
#' @param precision Samples per Bezier curve
#' @return A list with components `x` and `y` (polyclip format)
#' @keywords internal
vmobject_to_polygon <- function(vmobject, precision = 20) {
  if (!vmobject$has_points()) {
    return(list(x = numeric(0), y = numeric(0)))
  }

  pts <- vmobject$get_draw_points(precision = precision)
  # Remove NA breaks
  valid <- !is.na(pts[, 1])
  pts <- pts[valid, , drop = FALSE]

  if (nrow(pts) < 3) {
    return(list(x = numeric(0), y = numeric(0)))
  }

  # Offset by position
  pts[, 1] <- pts[, 1] + vmobject$position[1]
  pts[, 2] <- pts[, 2] + vmobject$position[2]

  list(x = pts[, 1], y = pts[, 2])
}

#' Convert polyclip result back to VMobject points
#'
#' @description Takes polyclip output (list of polygons) and converts them
#'   into Bezier control point format.
#' @param polys List of polygon lists (polyclip output)
#' @return A VMobject with the polygon paths
#' @keywords internal
polygon_result_to_vmobject <- function(polys, color = WHITE,
                                       fill_color = NULL, fill_opacity = 0.5) {
  result <- VMobject$new(color = color)

  if (length(polys) == 0) return(result)

  for (poly in polys) {
    x <- poly$x
    y <- poly$y
    n <- length(x)
    if (n < 3) next

    # Create line segments between consecutive vertices
    for (i in seq_len(n)) {
      j <- if (i == n) 1 else i + 1
      start <- c(x[i], y[i], 0)
      end <- c(x[j], y[j], 0)
      result$add_line(start, end)
    }
  }

  if (!is.null(fill_color)) {
    result$fill_color <- validate_color(fill_color)
    result$fill_opacity <- fill_opacity
  }

  result
}

#' Union R6 Class
#'
#' @description The union (OR) of two VMobject shapes. Produces the area
#'   covered by either shape. Requires the \pkg{polyclip} package.
#' @export
Union <- R6::R6Class(
  "Union",
  inherit = VMobject,
  public = list(
    #' @description Create a union of two shapes
    #' @param vmobject1 First VMobject
    #' @param vmobject2 Second VMobject
    #' @param color Stroke color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param precision Bezier sampling precision
    #' @param ... Additional args
    initialize = function(vmobject1, vmobject2, color = WHITE,
                          fill_color = NULL, fill_opacity = 0.5,
                          precision = 30, ...) {
      super$initialize(color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)

      if (!requireNamespace("polyclip", quietly = TRUE)) {
        cli::cli_warn("polyclip package required for boolean operations. Install with: install.packages('polyclip')")
        return()
      }

      poly1 <- vmobject_to_polygon(vmobject1, precision)
      poly2 <- vmobject_to_polygon(vmobject2, precision)

      if (length(poly1$x) < 3 || length(poly2$x) < 3) return()

      result <- polyclip::polyclip(
        list(poly1), list(poly2),
        op = "union"
      )

      self$apply_polygon_result(result)
    },

    #' @description Apply polygon clipping result to this VMobject
    #' @param polys List of polygon results from polyclip
    apply_polygon_result = function(polys) {
      self$clear_points()
      if (length(polys) == 0) return(invisible(self))

      for (poly in polys) {
        x <- poly$x
        y <- poly$y
        n <- length(x)
        if (n < 3) next

        for (i in seq_len(n)) {
          j <- if (i == n) 1 else i + 1
          start <- c(x[i], y[i], 0)
          end <- c(x[j], y[j], 0)
          self$add_line(start, end)
        }
      }
      invisible(self)
    }
  )
)

#' Intersection R6 Class
#'
#' @description The intersection (AND) of two VMobject shapes. Produces the
#'   area covered by both shapes. Requires the \pkg{polyclip} package.
#' @export
Intersection <- R6::R6Class(
  "Intersection",
  inherit = VMobject,
  public = list(
    #' @description Create an intersection of two shapes
    #' @param vmobject1 First VMobject
    #' @param vmobject2 Second VMobject
    #' @param color Stroke color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param precision Bezier sampling precision
    #' @param ... Additional args
    initialize = function(vmobject1, vmobject2, color = WHITE,
                          fill_color = NULL, fill_opacity = 0.5,
                          precision = 30, ...) {
      super$initialize(color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)

      if (!requireNamespace("polyclip", quietly = TRUE)) {
        cli::cli_warn("polyclip package required for boolean operations. Install with: install.packages('polyclip')")
        return()
      }

      poly1 <- vmobject_to_polygon(vmobject1, precision)
      poly2 <- vmobject_to_polygon(vmobject2, precision)

      if (length(poly1$x) < 3 || length(poly2$x) < 3) return()

      result <- polyclip::polyclip(
        list(poly1), list(poly2),
        op = "intersection"
      )

      self$clear_points()
      if (length(result) == 0) return()

      for (poly in result) {
        x <- poly$x
        y <- poly$y
        n <- length(x)
        if (n < 3) next

        for (i in seq_len(n)) {
          j <- if (i == n) 1 else i + 1
          start <- c(x[i], y[i], 0)
          end <- c(x[j], y[j], 0)
          self$add_line(start, end)
        }
      }
    }
  )
)

#' Difference R6 Class
#'
#' @description The difference (A - B) of two VMobject shapes. Produces the
#'   area of shape A not covered by shape B. Requires the \pkg{polyclip} package.
#' @export
Difference <- R6::R6Class(
  "Difference",
  inherit = VMobject,
  public = list(
    #' @description Create a difference of two shapes
    #' @param vmobject1 First VMobject (A)
    #' @param vmobject2 Second VMobject (B, subtracted from A)
    #' @param color Stroke color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param precision Bezier sampling precision
    #' @param ... Additional args
    initialize = function(vmobject1, vmobject2, color = WHITE,
                          fill_color = NULL, fill_opacity = 0.5,
                          precision = 30, ...) {
      super$initialize(color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)

      if (!requireNamespace("polyclip", quietly = TRUE)) {
        cli::cli_warn("polyclip package required for boolean operations. Install with: install.packages('polyclip')")
        return()
      }

      poly1 <- vmobject_to_polygon(vmobject1, precision)
      poly2 <- vmobject_to_polygon(vmobject2, precision)

      if (length(poly1$x) < 3 || length(poly2$x) < 3) return()

      result <- polyclip::polyclip(
        list(poly1), list(poly2),
        op = "minus"
      )

      self$clear_points()
      if (length(result) == 0) return()

      for (poly in result) {
        x <- poly$x
        y <- poly$y
        n <- length(x)
        if (n < 3) next

        for (i in seq_len(n)) {
          j <- if (i == n) 1 else i + 1
          start <- c(x[i], y[i], 0)
          end <- c(x[j], y[j], 0)
          self$add_line(start, end)
        }
      }
    }
  )
)
