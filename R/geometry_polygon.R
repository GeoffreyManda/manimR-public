#' @title Polygon-Based Geometry Objects
#' @description VMobject subclasses for polygons, rectangles, and stars.
#' @name geometry_polygon
NULL

#' Polygon R6 Class
#'
#' @description A closed polygon defined by vertex positions.
#' @export
Polygon <- R6::R6Class(
  "Polygon",
  inherit = VMobject,
  public = list(
    #' @field vertices Matrix of vertices
    vertices = NULL,

    #' @description Create a new Polygon
    #' @param ... Vertex positions as vectors, or a single matrix
    #' @param color Color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    initialize = function(..., color = WHITE, fill_color = NULL,
                          fill_opacity = 0) {
      super$initialize(color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity)
      args <- list(...)
      if (length(args) == 1 && is.matrix(args[[1]])) {
        self$vertices <- args[[1]]
        if (ncol(self$vertices) == 2) {
          self$vertices <- cbind(self$vertices, 0)
        }
      } else {
        self$vertices <- do.call(rbind, lapply(args, function(v) {
          v <- as.numeric(v)
          if (length(v) == 2) v <- c(v, 0)
          v
        }))
      }
      self$generate_points()
    },

    #' @description Generate Bezier path from vertices
    generate_points = function() {
      self$clear_points()
      if (is.null(self$vertices) || nrow(self$vertices) < 2) return(invisible(self))
      n <- nrow(self$vertices)
      for (i in seq_len(n)) {
        j <- if (i < n) i + 1 else 1
        self$add_line(self$vertices[i, ], self$vertices[j, ])
      }
      invisible(self)
    }
  )
)

#' RegularPolygon R6 Class
#'
#' @description A regular polygon with n equal sides.
#' @export
RegularPolygon <- R6::R6Class(
  "RegularPolygon",
  inherit = Polygon,
  public = list(
    #' @field n Number of sides
    n = 6,

    #' @description Create a new RegularPolygon
    #' @param n Number of sides
    #' @param radius Circumradius
    #' @param color Color
    #' @param start_angle Angle of first vertex (default: points up)
    #' @param ... Additional args
    initialize = function(n = 6, radius = 1, color = WHITE,
                          start_angle = pi / 2, ...) {
      self$n <- n
      angles <- seq(start_angle, start_angle + TAU, length.out = n + 1)[-(n + 1)]
      vertices <- cbind(
        radius * cos(angles),
        radius * sin(angles),
        0
      )
      super$initialize(vertices, color = color, ...)
    }
  )
)

#' Triangle R6 Class
#'
#' @description An equilateral triangle.
#' @export
Triangle <- R6::R6Class(
  "Triangle",
  inherit = RegularPolygon,
  public = list(
    #' @description Create a new Triangle
    #' @param color Color
    #' @param ... Additional args
    initialize = function(color = WHITE, ...) {
      super$initialize(n = 3, color = color, ...)
    }
  )
)

#' Rectangle R6 Class
#'
#' @description A rectangle with given width and height.
#' @export
Rectangle <- R6::R6Class(
  "Rectangle",
  inherit = Polygon,
  public = list(
    #' @field width Rectangle width
    width = 4,
    #' @field height Rectangle height
    height = 2,

    #' @description Create a new Rectangle
    #' @param width Width
    #' @param height Height
    #' @param color Color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param ... Additional args
    initialize = function(width = 4, height = 2, color = WHITE,
                          fill_color = NULL, fill_opacity = 0, ...) {
      self$width <- width
      self$height <- height
      w <- width / 2
      h <- height / 2
      vertices <- rbind(
        c(-w,  h, 0),
        c(-w, -h, 0),
        c( w, -h, 0),
        c( w,  h, 0)
      )
      super$initialize(vertices, color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)
    }
  )
)

#' Square R6 Class
#'
#' @description A square (rectangle with equal sides).
#' @export
Square <- R6::R6Class(
  "Square",
  inherit = Rectangle,
  public = list(
    #' @field side_length Side length
    side_length = 2,

    #' @description Create a new Square
    #' @param side_length Side length (also accepted as `side`)
    #' @param side Alias for `side_length` (project-page API compatibility)
    #' @param color Color
    #' @param ... Additional args
    initialize = function(side_length = 2, side = NULL, color = WHITE, ...) {
      if (!is.null(side)) side_length <- side
      self$side_length <- side_length
      super$initialize(width = side_length, height = side_length,
                       color = color, ...)
    }
  )
)

#' RoundedRectangle R6 Class
#'
#' @description A rectangle with rounded corners.
#' @export
RoundedRectangle <- R6::R6Class(
  "RoundedRectangle",
  inherit = VMobject,
  public = list(
    #' @field width Width
    width = 4,
    #' @field height Height
    height = 2,
    #' @field corner_radius Corner radius
    corner_radius = 0.5,

    #' @description Create a new RoundedRectangle
    #' @param width Width
    #' @param height Height
    #' @param corner_radius Corner radius
    #' @param color Color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param ... Additional args
    initialize = function(width = 4, height = 2, corner_radius = 0.5,
                          color = WHITE, fill_color = NULL,
                          fill_opacity = 0, ...) {
      super$initialize(color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)
      self$width <- width
      self$height <- height
      self$corner_radius <- min(corner_radius, width / 2, height / 2)
      self$generate_points()
    },

    #' @description Generate rounded rectangle path
    generate_points = function() {
      self$clear_points()
      w <- self$width / 2
      h <- self$height / 2
      r <- self$corner_radius

      # Bezier handle constant for quarter circle
      k <- 0.55191502449 * r

      # Top-right corner
      self$add_line(c(w - r, h, 0), c(w - r, h, 0))  # Move to start
      self$add_cubic_bezier(
        c(w - r, h, 0), c(w - r + k, h, 0),
        c(w, h - r + k, 0), c(w, h - r, 0)
      )
      # Right side
      self$add_line(c(w, h - r, 0), c(w, -(h - r), 0))
      # Bottom-right corner
      self$add_cubic_bezier(
        c(w, -(h - r), 0), c(w, -(h - r + k), 0),
        c(w - r + k, -h, 0), c(w - r, -h, 0)
      )
      # Bottom side
      self$add_line(c(w - r, -h, 0), c(-(w - r), -h, 0))
      # Bottom-left corner
      self$add_cubic_bezier(
        c(-(w - r), -h, 0), c(-(w - r + k), -h, 0),
        c(-w, -(h - r + k), 0), c(-w, -(h - r), 0)
      )
      # Left side
      self$add_line(c(-w, -(h - r), 0), c(-w, h - r, 0))
      # Top-left corner
      self$add_cubic_bezier(
        c(-w, h - r, 0), c(-w, h - r + k, 0),
        c(-(w - r + k), h, 0), c(-(w - r), h, 0)
      )
      # Top side (close)
      self$add_line(c(-(w - r), h, 0), c(w - r, h, 0))

      invisible(self)
    }
  )
)

#' Star R6 Class
#'
#' @description A star polygon with alternating outer and inner vertices.
#' @export
Star <- R6::R6Class(
  "Star",
  inherit = Polygon,
  public = list(
    #' @field n Number of points
    n = 5,
    #' @field outer_radius Outer radius
    outer_radius = 1,
    #' @field inner_radius Inner radius
    inner_radius = NULL,

    #' @description Create a new Star
    #' @param n Number of points
    #' @param outer_radius Outer radius
    #' @param inner_radius Inner radius (default: outer_radius / 2)
    #' @param color Color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param ... Additional args
    initialize = function(n = 5, outer_radius = 1, inner_radius = NULL,
                          color = WHITE, fill_color = NULL,
                          fill_opacity = 0, ...) {
      self$n <- n
      self$outer_radius <- outer_radius
      self$inner_radius <- inner_radius %||% (outer_radius / 2)

      # Generate alternating outer/inner vertices
      angles <- seq(pi / 2, pi / 2 + TAU, length.out = 2 * n + 1)[-(2 * n + 1)]
      radii <- rep(c(outer_radius, self$inner_radius), length.out = 2 * n)

      vertices <- cbind(
        radii * cos(angles),
        radii * sin(angles),
        0
      )

      super$initialize(vertices, color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)
    }
  )
)
