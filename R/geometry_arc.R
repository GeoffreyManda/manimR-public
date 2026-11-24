#' @title Arc-Based Geometry Objects
#' @description VMobject subclasses for arcs, circles, ellipses, and sectors.
#' @name geometry_arc
NULL

#' Arc R6 Class
#'
#' @description A circular arc defined by radius, start angle, and sweep angle.
#' @export
Arc <- R6::R6Class(
  "Arc",
  inherit = VMobject,
  public = list(
    #' @field radius Arc radius
    radius = 1,
    #' @field start_angle Starting angle in radians
    start_angle = 0,
    #' @field angle Sweep angle in radians
    angle = TAU / 4,
    #' @field num_components Number of Bezier segments
    num_components = 8,

    #' @description Create a new Arc
    #' @param radius Radius
    #' @param start_angle Starting angle in radians
    #' @param angle Sweep angle in radians
    #' @param num_components Number of Bezier segments
    #' @param color Color
    #' @param ... Additional args
    initialize = function(radius = 1, start_angle = 0, angle = TAU / 4,
                          num_components = 8, color = WHITE, ...) {
      super$initialize(color = color, ...)
      self$radius <- radius
      self$start_angle <- start_angle
      self$angle <- angle
      self$num_components <- num_components
      self$generate_points()
    },

    #' @description Generate arc points using cubic Bezier approximation
    generate_points = function() {
      self$clear_points()
      if (abs(self$angle) < 1e-10) return(invisible(self))

      # Split arc into segments, each <= PI/2
      n_segs <- max(1, ceiling(abs(self$angle) / (pi / 2)))
      seg_angle <- self$angle / n_segs

      for (i in 0:(n_segs - 1)) {
        theta1 <- self$start_angle + i * seg_angle
        theta2 <- theta1 + seg_angle
        self$add_arc_segment(theta1, theta2)
      }
      invisible(self)
    },

    #' @description Add a single arc segment as cubic Bezier
    #' @param theta1 Start angle
    #' @param theta2 End angle
    add_arc_segment = function(theta1, theta2) {
      # Bezier approximation for circular arc
      alpha <- (theta2 - theta1) / 2
      cos_alpha <- cos(alpha)
      # Handle length for best approximation
      k <- (4 / 3) * (1 - cos_alpha) / sin(alpha)
      if (!is.finite(k)) k <- 0

      r <- self$radius
      mid <- (theta1 + theta2) / 2

      p0 <- r * c(cos(theta1), sin(theta1), 0)
      p3 <- r * c(cos(theta2), sin(theta2), 0)

      # Tangent directions
      t1 <- c(-sin(theta1), cos(theta1), 0)
      t2 <- c(-sin(theta2), cos(theta2), 0)

      p1 <- p0 + k * r * t1
      p2 <- p3 - k * r * t2

      self$add_cubic_bezier(p0, p1, p2, p3)
    }
  )
)

#' ArcBetweenPoints R6 Class
#'
#' @description An arc passing through two points with a given curvature.
#' @export
ArcBetweenPoints <- R6::R6Class(
  "ArcBetweenPoints",
  inherit = Arc,
  public = list(
    #' @description Create an arc between two points
    #' @param start Start point
    #' @param end End point
    #' @param angle Arc sweep angle (PI/2 = quarter circle)
    #' @param color Color
    #' @param ... Additional args
    initialize = function(start = c(-1, 0, 0), end = c(1, 0, 0),
                          angle = pi / 2, color = WHITE, ...) {
      start <- validate_position(start)
      end <- validate_position(end)

      midpoint <- (start + end) / 2
      diff <- end - start
      dist <- sqrt(sum(diff^2))

      if (dist < 1e-10 || abs(angle) < 1e-10) {
        super$initialize(radius = 0, angle = 0, color = color, ...)
        self$add_line(start, end)
        return()
      }

      radius <- dist / (2 * sin(angle / 2))
      start_angle <- atan2(diff[2], diff[1]) - angle / 2 + pi / 2

      super$initialize(
        radius = abs(radius),
        start_angle = start_angle,
        angle = angle,
        color = color,
        ...
      )
      # Position so arc connects the two points
      arc_start <- self$get_start() - self$position
      desired_start <- start
      self$position <- desired_start - arc_start
    }
  )
)

#' Circle R6 Class
#'
#' @description A full circle using 4-segment cubic Bezier approximation.
#' @export
Circle <- R6::R6Class(
  "Circle",
  inherit = Arc,
  public = list(
    #' @description Create a new Circle
    #' @param radius Circle radius
    #' @param color Stroke color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param stroke_width Stroke width
    #' @param ... Additional args
    initialize = function(radius = 1, color = WHITE, fill_color = NULL,
                          fill_opacity = 0, stroke_width = DEFAULT_STROKE_WIDTH,
                          ...) {
      super$initialize(
        radius = radius,
        start_angle = 0,
        angle = TAU,
        color = color,
        ...
      )
      self$stroke_width <- stroke_width
      if (!is.null(fill_color)) {
        self$fill_color <- validate_color(fill_color)
        self$fill_opacity <- fill_opacity
      }
    }
  )
)

#' Dot R6 Class
#'
#' @description A small filled circle (point).
#' @export
Dot <- R6::R6Class(
  "Dot",
  inherit = Circle,
  public = list(
    #' @description Create a new Dot
    #' @param point Position
    #' @param radius Dot radius
    #' @param color Dot color
    #' @param ... Additional args
    initialize = function(point = ORIGIN, radius = DEFAULT_DOT_RADIUS,
                          color = WHITE, ...) {
      super$initialize(radius = radius, color = color,
                       fill_color = color, fill_opacity = 1, ...)
      self$stroke_width <- 0
      self$position <- validate_position(point)
    }
  )
)

#' Ellipse R6 Class
#'
#' @description An ellipse with independent x and y radii.
#' @export
Ellipse <- R6::R6Class(
  "Ellipse",
  inherit = Circle,
  public = list(
    #' @field width Ellipse width (2 * x_radius)
    width = 2,
    #' @field height Ellipse height (2 * y_radius)
    height = 1,

    #' @description Create a new Ellipse
    #' @param width Width
    #' @param height Height
    #' @param color Color
    #' @param ... Additional args
    initialize = function(width = 2, height = 1, color = WHITE, ...) {
      super$initialize(radius = 1, color = color, ...)
      self$width <- width
      self$height <- height
      # Scale the unit circle to ellipse dimensions
      if (self$has_points()) {
        self$points[, 1] <- self$points[, 1] * (width / 2)
        self$points[, 2] <- self$points[, 2] * (height / 2)
      }
    }
  )
)

#' Annulus R6 Class
#'
#' @description A ring (donut) shape - area between two concentric circles.
#' @export
Annulus <- R6::R6Class(
  "Annulus",
  inherit = VMobject,
  public = list(
    #' @field inner_radius Inner circle radius
    inner_radius = 0.5,
    #' @field outer_radius Outer circle radius
    outer_radius = 1,

    #' @description Create a new Annulus
    #' @param inner_radius Inner radius
    #' @param outer_radius Outer radius
    #' @param color Color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param ... Additional args
    initialize = function(inner_radius = 0.5, outer_radius = 1,
                          color = WHITE, fill_color = NULL,
                          fill_opacity = 0.5, ...) {
      super$initialize(color = color, fill_color = fill_color,
                       fill_opacity = fill_opacity, ...)
      self$inner_radius <- inner_radius
      self$outer_radius <- outer_radius

      # Create outer circle
      outer <- Circle$new(radius = outer_radius)
      inner <- Circle$new(radius = inner_radius)
      self$submobjects <- list(outer, inner)
    }
  )
)

#' Sector R6 Class
#'
#' @description A filled sector (pie slice) of a circle.
#' @export
Sector <- R6::R6Class(
  "Sector",
  inherit = Arc,
  public = list(
    #' @description Create a new Sector
    #' @param radius Radius
    #' @param start_angle Start angle
    #' @param angle Sweep angle
    #' @param color Color
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param ... Additional args
    initialize = function(radius = 1, start_angle = 0, angle = TAU / 4,
                          color = WHITE, fill_color = NULL,
                          fill_opacity = 0.5, ...) {
      super$initialize(radius = radius, start_angle = start_angle,
                       angle = angle, color = color, ...)
      if (is.null(fill_color)) fill_color <- color
      self$fill_color <- validate_color(fill_color)
      self$fill_opacity <- fill_opacity

      # Close the sector: add lines from end to center and center to start
      end_pt <- self$points[nrow(self$points), ]
      start_pt <- self$points[1, ]
      center <- c(0, 0, 0)

      self$add_line(end_pt, center)
      self$add_line(center, start_pt)
    }
  )
)
