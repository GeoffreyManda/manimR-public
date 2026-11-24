#' @title Growing Animations
#' @description Animations that grow mobjects from a point, edge, or nothing.
#' @name animation_growing
NULL

#' GrowFromCenter R6 Class
#'
#' @description Grows a mobject from a single point at its center.
#' @export
GrowFromCenter <- R6::R6Class(
  "GrowFromCenter",
  inherit = Animation,
  public = list(
    #' @description Create a GrowFromCenter animation
    #' @param mobject Mobject to grow
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, run_time = 1, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$is_introduced <- TRUE
    },

    #' @description Begin: save target state, collapse to center
    begin = function() {
      private$target_scale <- self$mobject$scale_factor
      private$target_opacity <- self$mobject$opacity
      self$mobject$scale_factor <- 0
      self$mobject$opacity <- 0
      super$begin()
    },

    #' @description Grow from zero scale to full
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$scale_factor <- alpha * (private$target_scale %||% 1)
      mob$opacity <- alpha * (private$target_opacity %||% 1)
      invisible(self)
    }
  ),

  private = list(
    target_scale = NULL,
    target_opacity = NULL
  )
)

#' GrowFromEdge R6 Class
#'
#' @description Grows a mobject from one of its edges.
#' @export
GrowFromEdge <- R6::R6Class(
  "GrowFromEdge",
  inherit = Animation,
  public = list(
    #' @field edge Direction of the edge to grow from
    edge = NULL,

    #' @description Create a GrowFromEdge animation
    #' @param mobject Mobject to grow
    #' @param edge Direction vector (e.g., DOWN, LEFT)
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, edge = DOWN, run_time = 1,
                          rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$edge <- validate_position(edge)
      self$is_introduced <- TRUE
    },

    #' @description Begin: save state and collapse towards edge
    begin = function() {
      private$target_scale <- self$mobject$scale_factor
      private$target_opacity <- self$mobject$opacity
      private$target_position <- self$mobject$position
      self$mobject$scale_factor <- 0
      self$mobject$opacity <- 0
      super$begin()
    },

    #' @description Grow from edge
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$scale_factor <- alpha * (private$target_scale %||% 1)
      mob$opacity <- alpha * (private$target_opacity %||% 1)
      # Anchor at edge position
      mob$position <- private$target_position
      invisible(self)
    }
  ),

  private = list(
    target_scale = NULL,
    target_opacity = NULL,
    target_position = NULL
  )
)

#' GrowFromPoint R6 Class
#'
#' @description Grows a mobject from a specific point.
#' @export
GrowFromPoint <- R6::R6Class(
  "GrowFromPoint",
  inherit = Animation,
  public = list(
    #' @field point The point to grow from
    point = NULL,

    #' @description Create a GrowFromPoint animation
    #' @param mobject Mobject to grow
    #' @param point Source point
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, point = ORIGIN, run_time = 1,
                          rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$point <- validate_position(point)
      self$is_introduced <- TRUE
    },

    #' @description Save target state, move to point and collapse
    begin = function() {
      private$target_position <- self$mobject$position
      private$target_scale <- self$mobject$scale_factor
      private$target_opacity <- self$mobject$opacity
      self$mobject$position <- self$point
      self$mobject$scale_factor <- 0
      self$mobject$opacity <- 0
      super$begin()
    },

    #' @description Grow from point to final position/scale
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$position <- lerp(self$point, private$target_position, alpha)
      mob$scale_factor <- alpha * (private$target_scale %||% 1)
      mob$opacity <- alpha * (private$target_opacity %||% 1)
      invisible(self)
    }
  ),

  private = list(
    target_position = NULL,
    target_scale = NULL,
    target_opacity = NULL
  )
)

#' GrowArrow R6 Class
#'
#' @description Grows an Arrow from its start point to its end.
#' @export
GrowArrow <- R6::R6Class(
  "GrowArrow",
  inherit = Animation,
  public = list(
    #' @description Create a GrowArrow animation
    #' @param arrow An Arrow mobject
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(arrow, run_time = 1, rate_func = "smooth", ...) {
      super$initialize(mobject = arrow, run_time = run_time,
                       rate_func = rate_func, ...)
      self$is_introduced <- TRUE
    },

    #' @description Save the arrow state
    begin = function() {
      mob <- self$mobject
      private$target_opacity <- mob$opacity
      mob$opacity <- 0
      super$begin()
    },

    #' @description Progressively reveal the arrow
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$opacity <- alpha * (private$target_opacity %||% 1)

      if (inherits(mob, "VMobject") && !is.null(self$starting_mobject)) {
        n_curves <- self$starting_mobject$get_num_curves()
        if (n_curves > 0) {
          curves_to_show <- max(1, ceiling(alpha * n_curves))
          total_pts <- nrow(self$starting_mobject$points)
          pts_to_show <- min(curves_to_show * 4, total_pts)
          mob$points <- self$starting_mobject$points[1:pts_to_show, , drop = FALSE]
        }
      }
      invisible(self)
    }
  ),

  private = list(
    target_opacity = NULL
  )
)

#' SpinInFromNothing R6 Class
#'
#' @description Grows from a point while spinning.
#' @export
SpinInFromNothing <- R6::R6Class(
  "SpinInFromNothing",
  inherit = Animation,
  public = list(
    #' @field angle Total rotation during spin-in
    angle = TAU,

    #' @description Create a SpinInFromNothing animation
    #' @param mobject Mobject to spin in
    #' @param angle Total spin angle (default: full turn)
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, angle = TAU, run_time = 1,
                          rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$angle <- angle
      self$is_introduced <- TRUE
    },

    #' @description Save state, collapse
    begin = function() {
      private$target_scale <- self$mobject$scale_factor
      private$target_opacity <- self$mobject$opacity
      private$start_rotation <- self$mobject$rotation
      self$mobject$scale_factor <- 0
      self$mobject$opacity <- 0
      super$begin()
    },

    #' @description Grow and rotate simultaneously
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$scale_factor <- alpha * (private$target_scale %||% 1)
      mob$opacity <- alpha * (private$target_opacity %||% 1)
      mob$rotation <- (private$start_rotation %||% 0) + alpha * self$angle
      invisible(self)
    }
  ),

  private = list(
    target_scale = NULL,
    target_opacity = NULL,
    start_rotation = NULL
  )
)

#' SpiralIn R6 Class
#'
#' @description Spirals a mobject in from outside the frame.
#' @export
SpiralIn <- R6::R6Class(
  "SpiralIn",
  inherit = Animation,
  public = list(
    #' @field spiral_radius Starting distance from center
    spiral_radius = 3,
    #' @field n_turns Number of spiral turns
    n_turns = 1,

    #' @description Create a SpiralIn animation
    #' @param mobject Mobject
    #' @param spiral_radius Starting distance from final position
    #' @param n_turns Number of spiral turns
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, spiral_radius = 3, n_turns = 1,
                          run_time = 1.5, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$spiral_radius <- spiral_radius
      self$n_turns <- n_turns
      self$is_introduced <- TRUE
    },

    #' @description Save target, move to spiral start
    begin = function() {
      private$target_position <- self$mobject$position
      private$target_opacity <- self$mobject$opacity
      self$mobject$opacity <- 0
      super$begin()
    },

    #' @description Spiral inward
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      t <- 1 - alpha  # decreasing radius
      angle <- alpha * self$n_turns * TAU
      radius <- t * self$spiral_radius

      offset <- c(radius * cos(angle), radius * sin(angle), 0)
      mob$position <- private$target_position + offset
      mob$opacity <- alpha * (private$target_opacity %||% 1)

      invisible(self)
    }
  ),

  private = list(
    target_position = NULL,
    target_opacity = NULL
  )
)
