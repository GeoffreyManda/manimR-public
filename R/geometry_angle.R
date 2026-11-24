#' @title Angle Geometry Objects
#' @description VMobject subclasses for angle indicators and right-angle marks.
#' @name geometry_angle
NULL

#' Angle R6 Class
#'
#' @description An angle arc between two lines meeting at a common point.
#' @export
Angle <- R6::R6Class(
  "Angle",
  inherit = Arc,
  public = list(
    #' @field line1 First line/vector
    line1 = NULL,
    #' @field line2 Second line/vector
    line2 = NULL,
    #' @field dot Whether to show a dot for small angles
    dot = FALSE,
    #' @field dot_radius Radius of the angle dot
    dot_radius = DEFAULT_DOT_RADIUS,
    #' @field elbow Whether to draw a right-angle elbow
    elbow = FALSE,

    #' @description Create a new Angle indicator
    #' @param line1 First Line object or direction vector
    #' @param line2 Second Line object or direction vector
    #' @param radius Radius of the angle arc
    #' @param quadrant Which quadrant to draw the angle in (c(1,1) default)
    #' @param other_angle Whether to show the reflex angle
    #' @param dot Show a dot in the angle
    #' @param dot_radius Dot radius
    #' @param elbow Draw right-angle elbow instead of arc
    #' @param color Color
    #' @param ... Additional args
    initialize = function(line1 = NULL, line2 = NULL, radius = 0.5,
                          quadrant = c(1, 1), other_angle = FALSE,
                          dot = FALSE, dot_radius = DEFAULT_DOT_RADIUS,
                          elbow = FALSE, color = WHITE, ...) {
      # Determine directions from lines or vectors
      if (inherits(line1, "Line")) {
        dir1 <- line1$get_unit_vector()
      } else if (is.numeric(line1)) {
        dir1 <- validate_position(line1)
      } else {
        dir1 <- c(1, 0, 0)
      }

      if (inherits(line2, "Line")) {
        dir2 <- line2$get_unit_vector()
      } else if (is.numeric(line2)) {
        dir2 <- validate_position(line2)
      } else {
        dir2 <- c(0, 1, 0)
      }

      # Apply quadrant flipping
      dir1[1] <- dir1[1] * quadrant[1]
      dir1[2] <- dir1[2] * quadrant[2]
      dir2[1] <- dir2[1] * quadrant[1]
      dir2[2] <- dir2[2] * quadrant[2]

      # Calculate angles
      angle1 <- atan2(dir1[2], dir1[1])
      angle2 <- atan2(dir2[2], dir2[1])

      # Compute sweep angle
      sweep <- angle2 - angle1
      # Normalize to (-PI, PI]
      while (sweep > pi) sweep <- sweep - TAU
      while (sweep <= -pi) sweep <- sweep + TAU

      if (other_angle) {
        if (sweep > 0) sweep <- sweep - TAU
        else sweep <- sweep + TAU
      }

      self$line1 <- line1
      self$line2 <- line2
      self$dot <- dot
      self$dot_radius <- dot_radius
      self$elbow <- elbow

      if (elbow) {
        # Draw right-angle mark (two line segments forming an L)
        super$initialize(radius = 0, angle = 0, color = color, ...)
        self$clear_points()

        # Corner directions
        d1 <- normalize(dir1[1:2])
        d2 <- normalize(dir2[1:2])

        p_start <- radius * c(d1, 0)
        p_corner <- radius * c(d1 + d2, 0)
        p_end <- radius * c(d2, 0)

        self$add_line(p_start, p_corner)
        self$add_line(p_corner, p_end)
      } else {
        super$initialize(
          radius = radius,
          start_angle = angle1,
          angle = sweep,
          color = color,
          ...
        )
      }

      # Position at the intersection point of the two lines
      if (inherits(line1, "Line") && inherits(line2, "Line")) {
        # Find intersection (approximate: use start of line1)
        self$position <- line1$get_start()
      }
    },

    #' @description Get the value of the angle in radians
    get_value = function() {
      abs(self$angle)
    },

    #' @description Get the value of the angle in degrees
    get_value_degrees = function() {
      abs(self$angle) * 180 / pi
    }
  )
)

#' RightAngle R6 Class
#'
#' @description A right-angle (90 degree) indicator with an elbow mark.
#' @export
RightAngle <- R6::R6Class(
  "RightAngle",
  inherit = Angle,
  public = list(
    #' @description Create a right-angle indicator
    #' @param line1 First Line object or direction vector
    #' @param line2 Second Line object or direction vector
    #' @param length Side length of the right-angle mark
    #' @param color Color
    #' @param ... Additional args
    initialize = function(line1 = NULL, line2 = NULL, length = 0.3,
                          color = WHITE, ...) {
      super$initialize(
        line1 = line1,
        line2 = line2,
        radius = length,
        elbow = TRUE,
        color = color,
        ...
      )
    }
  )
)
