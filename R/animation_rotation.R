#' @title Rotation Animation
#' @description Animation that rotates a mobject around an axis.
#' @name animation_rotation
NULL

#' Rotate R6 Class (Animation)
#'
#' @description Rotates a mobject by a given angle over time.
#'   Distinct from `VMobject$rotate()` which is an immediate transform.
#' @export
RotateAnim <- R6::R6Class(
  "RotateAnim",
  inherit = Animation,
  public = list(
    #' @field angle Total rotation angle in radians
    angle = PI,
    #' @field axis Rotation axis (for 3D)
    axis = NULL,
    #' @field about_point Center of rotation (NULL = mobject center)
    about_point = NULL,

    #' @description Create a Rotate animation
    #' @param mobject Mobject to rotate
    #' @param angle Angle in radians (default: PI = half turn)
    #' @param axis Rotation axis c(x, y, z) (default: c(0, 0, 1) = Z-axis)
    #' @param about_point Center of rotation
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, angle = PI, axis = c(0, 0, 1),
                          about_point = NULL, run_time = 1,
                          rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$angle <- angle
      self$axis <- validate_position(axis)
      self$about_point <- if (!is.null(about_point)) {
        validate_position(about_point)
      } else {
        NULL
      }
    },

    #' @description Save starting state
    begin = function() {
      private$start_rotation <- self$mobject$rotation
      if (inherits(self$mobject, "VMobject") && self$mobject$has_points()) {
        private$start_points <- self$mobject$points
      }
      super$begin()
    },

    #' @description Interpolate rotation
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      current_angle <- alpha * self$angle

      # Update rotation field

      mob$rotation <- (private$start_rotation %||% 0) + current_angle

      # Rotate actual points if VMobject
      if (inherits(mob, "VMobject") && !is.null(private$start_points)) {
        mob$points <- rotate_points_3d(private$start_points, current_angle,
                                       self$axis)
      }

      invisible(self)
    }
  ),

  private = list(
    start_rotation = NULL,
    start_points = NULL
  )
)

#' Rotating R6 Class
#'
#' @description Continuously rotates a mobject (for use with updaters/looping).
#' @export
Rotating <- R6::R6Class(
  "Rotating",
  inherit = Animation,
  public = list(
    #' @field angular_velocity Radians per second
    angular_velocity = TAU / 4,
    #' @field axis Rotation axis
    axis = NULL,
    #' @field about_point Center of rotation
    about_point = NULL,

    #' @description Create a Rotating animation
    #' @param mobject Mobject to rotate
    #' @param angular_velocity Radians per second
    #' @param axis Rotation axis
    #' @param about_point Center of rotation
    #' @param run_time Duration
    #' @param ... Additional args
    initialize = function(mobject, angular_velocity = TAU / 4,
                          axis = c(0, 0, 1), about_point = NULL,
                          run_time = 2, ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = "linear", ...)
      self$angular_velocity <- angular_velocity
      self$axis <- validate_position(axis)
      self$about_point <- if (!is.null(about_point)) {
        validate_position(about_point)
      } else {
        NULL
      }
    },

    #' @description Save starting state
    begin = function() {
      private$start_rotation <- self$mobject$rotation
      if (inherits(self$mobject, "VMobject") && self$mobject$has_points()) {
        private$start_points <- self$mobject$points
      }
      super$begin()
    },

    #' @description Apply continuous rotation
    #' @param alpha Linear progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      total_angle <- alpha * self$angular_velocity * self$run_time
      mob$rotation <- (private$start_rotation %||% 0) + total_angle

      if (inherits(mob, "VMobject") && !is.null(private$start_points)) {
        mob$points <- rotate_points_3d(private$start_points, total_angle,
                                       self$axis)
      }

      invisible(self)
    }
  ),

  private = list(
    start_rotation = NULL,
    start_points = NULL
  )
)
