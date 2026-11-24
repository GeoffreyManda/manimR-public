#' @title Line-Based Geometry Objects
#' @description VMobject subclasses for lines, arrows, and vectors.
#' @name geometry_line
NULL

#' Line R6 Class
#'
#' @description A straight line between two points.
#' @export
Line <- R6::R6Class(
  "Line",
  inherit = VMobject,
  public = list(
    #' @description Create a new Line
    #' @param start Start point
    #' @param end End point
    #' @param color Color
    #' @param stroke_width Stroke width
    #' @param ... Additional args
    initialize = function(start = LEFT, end = RIGHT, color = WHITE,
                          stroke_width = DEFAULT_STROKE_WIDTH, ...) {
      super$initialize(color = color, stroke_width = stroke_width, ...)
      self$add_line(validate_position(start), validate_position(end))
    },

    #' @description Update endpoints
    #' @param start New start
    #' @param end New end
    put_start_and_end_on = function(start, end) {
      self$clear_points()
      self$add_line(validate_position(start), validate_position(end))
      invisible(self)
    },

    #' @description Get line length
    get_length = function() {
      if (!self$has_points()) return(0)
      s <- self$points[1, ]
      e <- self$points[nrow(self$points), ]
      sqrt(sum((e - s)^2))
    },

    #' @description Get unit vector along the line
    get_unit_vector = function() {
      if (!self$has_points()) return(RIGHT)
      s <- self$points[1, ]
      e <- self$points[nrow(self$points), ]
      normalize(e - s)
    },

    #' @description Get angle of the line
    get_angle = function() {
      v <- self$get_unit_vector()
      atan2(v[2], v[1])
    }
  )
)

#' DashedLine R6 Class
#'
#' @description A dashed line between two points.
#' @export
DashedLine <- R6::R6Class(
  "DashedLine",
  inherit = VMobject,
  public = list(
    #' @field dash_length Length of each dash
    dash_length = DEFAULT_DASH_LENGTH,
    #' @field num_dashes Number of dashes
    num_dashes = 15,

    #' @description Create a new DashedLine
    #' @param start Start point
    #' @param end End point
    #' @param dash_length Length of each dash
    #' @param num_dashes Number of dashes (overrides dash_length)
    #' @param color Color
    #' @param ... Additional args
    initialize = function(start = LEFT, end = RIGHT,
                          dash_length = DEFAULT_DASH_LENGTH,
                          num_dashes = NULL, color = WHITE, ...) {
      super$initialize(color = color, ...)
      start <- validate_position(start)
      end <- validate_position(end)
      self$dash_length <- dash_length

      total_length <- sqrt(sum((end - start)^2))
      if (is.null(num_dashes)) {
        num_dashes <- max(2, round(total_length / (2 * dash_length)))
      }
      self$num_dashes <- num_dashes

      # Create individual dash segments
      for (i in seq_len(num_dashes)) {
        t1 <- (2 * (i - 1)) / (2 * num_dashes)
        t2 <- (2 * i - 1) / (2 * num_dashes)
        t2 <- min(t2, 1)
        p1 <- lerp(start, end, t1)
        p2 <- lerp(start, end, t2)
        dash <- Line$new(start = p1, end = p2, color = color)
        self$submobjects <- c(self$submobjects, list(dash))
      }
    }
  )
)

#' ArrowTip R6 Class
#'
#' @description A triangular arrow tip (filled).
#' @export
ArrowTip <- R6::R6Class(
  "ArrowTip",
  inherit = VMobject,
  public = list(
    #' @field tip_length Length of the tip
    tip_length = DEFAULT_ARROW_TIP_LENGTH,
    #' @field tip_width Width of the tip base
    tip_width = DEFAULT_ARROW_TIP_WIDTH,

    #' @description Create a new ArrowTip
    #' @param length Tip length
    #' @param width Tip width
    #' @param color Color
    #' @param ... Additional args
    initialize = function(length = DEFAULT_ARROW_TIP_LENGTH,
                          width = DEFAULT_ARROW_TIP_WIDTH,
                          color = WHITE, ...) {
      super$initialize(color = color, fill_color = color, fill_opacity = 1, ...)
      self$tip_length <- length
      self$tip_width <- width
      self$stroke_width <- 0

      # Triangle pointing right
      tip <- c(length, 0, 0)
      base_up <- c(0, width / 2, 0)
      base_down <- c(0, -width / 2, 0)

      self$add_line(tip, base_up)
      self$add_line(base_up, base_down)
      self$add_line(base_down, tip)
    }
  )
)

#' Arrow R6 Class
#'
#' @description A line with an arrowhead tip at the end.
#' @export
Arrow <- R6::R6Class(
  "Arrow",
  inherit = Line,
  public = list(
    #' @field tip_length Arrow tip length
    tip_length = DEFAULT_ARROW_TIP_LENGTH,
    #' @field tip The ArrowTip submobject
    tip = NULL,

    #' @description Create a new Arrow
    #' @param start Start point
    #' @param end End point (where tip points)
    #' @param tip_length Tip length
    #' @param color Color
    #' @param stroke_width Stroke width
    #' @param ... Additional args
    initialize = function(start = LEFT, end = RIGHT,
                          tip_length = DEFAULT_ARROW_TIP_LENGTH,
                          color = WHITE,
                          stroke_width = DEFAULT_STROKE_WIDTH, ...) {
      start <- validate_position(start)
      end <- validate_position(end)
      self$tip_length <- tip_length

      # Shorten line to make room for tip
      direction <- normalize(end - start)
      shortened_end <- end - direction * tip_length

      super$initialize(start = start, end = shortened_end,
                       color = color, stroke_width = stroke_width, ...)

      # Create and position the tip
      self$tip <- ArrowTip$new(length = tip_length, color = color)

      # Rotate tip to match line direction
      angle <- atan2(direction[2], direction[1])
      if (self$tip$has_points()) {
        self$tip$points <- rotate_points_3d(self$tip$points, angle)
      }
      self$tip$position <- end - direction * tip_length

      self$submobjects <- c(self$submobjects, list(self$tip))
    }
  )
)

#' DoubleArrow R6 Class
#'
#' @description A line with arrowhead tips at both ends.
#' @export
DoubleArrow <- R6::R6Class(
  "DoubleArrow",
  inherit = VMobject,
  public = list(
    #' @description Create a new DoubleArrow
    #' @param start Start point
    #' @param end End point
    #' @param tip_length Tip length
    #' @param color Color
    #' @param ... Additional args
    initialize = function(start = LEFT, end = RIGHT,
                          tip_length = DEFAULT_ARROW_TIP_LENGTH,
                          color = WHITE, ...) {
      super$initialize(color = color, ...)
      start <- validate_position(start)
      end <- validate_position(end)

      direction <- normalize(end - start)
      inner_start <- start + direction * tip_length
      inner_end <- end - direction * tip_length

      # Main line
      self$add_line(inner_start, inner_end)

      # Tips at both ends
      tip_end <- ArrowTip$new(length = tip_length, color = color)
      tip_start <- ArrowTip$new(length = tip_length, color = color)

      angle_fwd <- atan2(direction[2], direction[1])
      angle_bwd <- angle_fwd + pi

      if (tip_end$has_points()) {
        tip_end$points <- rotate_points_3d(tip_end$points, angle_fwd)
      }
      tip_end$position <- inner_end

      if (tip_start$has_points()) {
        tip_start$points <- rotate_points_3d(tip_start$points, angle_bwd)
      }
      tip_start$position <- inner_start

      self$submobjects <- list(tip_end, tip_start)
    }
  )
)

#' Vector R6 Class
#'
#' @description An arrow starting from the origin (or a given point).
#' @export
Vector <- R6::R6Class(
  "Vector",
  inherit = Arrow,
  public = list(
    #' @description Create a new Vector
    #' @param direction Direction vector
    #' @param color Color
    #' @param ... Additional args
    initialize = function(direction = RIGHT, color = WHITE, ...) {
      direction <- validate_position(direction)
      super$initialize(start = ORIGIN, end = direction, color = color, ...)
    }
  )
)

#' CurvedArrow R6 Class
#'
#' @description An arc with an arrowhead tip.
#' @export
CurvedArrow <- R6::R6Class(
  "CurvedArrow",
  inherit = VMobject,
  public = list(
    #' @description Create a new CurvedArrow
    #' @param start Start point
    #' @param end End point
    #' @param angle Arc angle
    #' @param color Color
    #' @param ... Additional args
    initialize = function(start = LEFT, end = RIGHT,
                          angle = TAU / 4, color = WHITE, ...) {
      super$initialize(color = color, ...)

      arc <- ArcBetweenPoints$new(start = start, end = end,
                                   angle = angle, color = color)
      self$submobjects <- list(arc)

      # Add tip at the end
      if (arc$has_points()) {
        end_pt <- arc$points[nrow(arc$points), ]
        n <- nrow(arc$points)
        if (n >= 2) {
          prev_pt <- arc$points[n - 1, ]
          direction <- normalize(end_pt - prev_pt)
          tip <- ArrowTip$new(color = color)
          tip_angle <- atan2(direction[2], direction[1])
          if (tip$has_points()) {
            tip$points <- rotate_points_3d(tip$points, tip_angle)
          }
          tip$position <- end_pt + arc$position
          self$submobjects <- c(self$submobjects, list(tip))
        }
      }
    }
  )
)

#' TangentLine R6 Class
#'
#' @description A line tangent to a VMobject at a given proportion.
#' @export
TangentLine <- R6::R6Class(
  "TangentLine",
  inherit = Line,
  public = list(
    #' @description Create a tangent line
    #' @param vmobject The VMobject to be tangent to
    #' @param alpha Proportion along the path (0-1)
    #' @param length Line length
    #' @param color Color
    #' @param ... Additional args
    initialize = function(vmobject, alpha, length = 1,
                          color = WHITE, ...) {
      p1 <- point_from_proportion(vmobject$points, clamp(alpha - 0.01))
      p2 <- point_from_proportion(vmobject$points, clamp(alpha + 0.01))
      direction <- normalize(p2 - p1)
      center <- point_from_proportion(vmobject$points, alpha)

      start <- center - direction * length / 2 + vmobject$position
      end <- center + direction * length / 2 + vmobject$position

      super$initialize(start = start, end = end, color = color, ...)
    }
  )
)
