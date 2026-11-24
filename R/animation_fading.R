#' @title Fading Animations
#' @description Animations that fade mobjects in or out, optionally with directional movement.
#' @name animation_fading
NULL

#' FadeIn R6 Class
#'
#' @description Fades a mobject in from transparent, optionally sliding
#'   from a direction.
#' @export
FadeIn <- R6::R6Class(
  "FadeIn",
  inherit = Animation,
  public = list(
    #' @field shift_vector Direction to fade in from
    shift_vector = NULL,
    #' @field scale_factor Initial scale (for zoom-in effect)
    scale_factor = 1,
    #' @field target_position Saved target position
    target_position = NULL,

    #' @description Create a FadeIn animation
    #' @param mobject Mobject to fade in
    #' @param shift Direction to slide from (NULL = fade in place)
    #' @param scale Starting scale factor (< 1 = grow in, > 1 = shrink in)
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, shift = NULL, scale = 1,
                          run_time = 1, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$shift_vector <- if (!is.null(shift)) validate_position(shift) else NULL
      self$scale_factor <- scale
      self$is_introduced <- TRUE
    },

    #' @description Save target state and set initial transparent state
    begin = function() {
      self$target_position <- self$mobject$position
      private$target_opacity <- self$mobject$opacity
      private$target_scale <- self$mobject$scale_factor

      # Set starting state: invisible
      self$mobject$opacity <- 0
      if (inherits(self$mobject, "VMobject")) {
        private$target_fill_opacity <- self$mobject$fill_opacity
        private$target_stroke_opacity <- self$mobject$stroke_opacity
        self$mobject$fill_opacity <- 0
        self$mobject$stroke_opacity <- 0
      }

      # Offset position if shift specified
      if (!is.null(self$shift_vector)) {
        self$mobject$position <- self$target_position - self$shift_vector
      }

      super$begin()
    },

    #' @description Interpolate opacity and position
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$opacity <- alpha * (private$target_opacity %||% 1)

      if (inherits(mob, "VMobject")) {
        mob$fill_opacity <- alpha * (private$target_fill_opacity %||% 0)
        mob$stroke_opacity <- alpha * (private$target_stroke_opacity %||% 1)
      }

      # Interpolate position from offset to target
      if (!is.null(self$shift_vector)) {
        start_pos <- self$target_position - self$shift_vector
        mob$position <- lerp(start_pos, self$target_position, alpha)
      }

      # Scale interpolation
      if (self$scale_factor != 1) {
        current_scale <- lerp(self$scale_factor * (private$target_scale %||% 1),
                              private$target_scale %||% 1, alpha)
        mob$scale_factor <- current_scale
      }

      invisible(self)
    },

    #' @description Ensure final state
    finish = function() {
      mob <- self$mobject
      mob$opacity <- private$target_opacity %||% 1
      mob$position <- self$target_position
      mob$scale_factor <- private$target_scale %||% 1
      if (inherits(mob, "VMobject")) {
        mob$fill_opacity <- private$target_fill_opacity %||% 0
        mob$stroke_opacity <- private$target_stroke_opacity %||% 1
      }
      invisible(self)
    }
  ),

  private = list(
    target_opacity = NULL,
    target_scale = NULL,
    target_fill_opacity = NULL,
    target_stroke_opacity = NULL
  )
)

#' FadeOut R6 Class
#'
#' @description Fades a mobject out to transparent, optionally sliding
#'   towards a direction.
#' @export
FadeOut <- R6::R6Class(
  "FadeOut",
  inherit = Animation,
  public = list(
    #' @field shift_vector Direction to fade out towards
    shift_vector = NULL,
    #' @field target_scale Final scale factor
    target_scale = 1,

    #' @description Create a FadeOut animation
    #' @param mobject Mobject to fade out
    #' @param shift Direction to slide towards (NULL = fade in place)
    #' @param scale Ending scale factor
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, shift = NULL, scale = 1,
                          run_time = 1, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$shift_vector <- if (!is.null(shift)) validate_position(shift) else NULL
      self$target_scale <- scale
      self$remover <- TRUE
    },

    #' @description Save starting state
    begin = function() {
      private$start_opacity <- self$mobject$opacity
      private$start_position <- self$mobject$position
      private$start_scale <- self$mobject$scale_factor
      if (inherits(self$mobject, "VMobject")) {
        private$start_fill_opacity <- self$mobject$fill_opacity
        private$start_stroke_opacity <- self$mobject$stroke_opacity
      }
      super$begin()
    },

    #' @description Interpolate opacity and position
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$opacity <- (1 - alpha) * (private$start_opacity %||% 1)

      if (inherits(mob, "VMobject")) {
        mob$fill_opacity <- (1 - alpha) * (private$start_fill_opacity %||% 0)
        mob$stroke_opacity <- (1 - alpha) * (private$start_stroke_opacity %||% 1)
      }

      # Position shift
      if (!is.null(self$shift_vector)) {
        mob$position <- lerp(private$start_position,
                             private$start_position + self$shift_vector, alpha)
      }

      # Scale
      if (self$target_scale != 1) {
        mob$scale_factor <- lerp(private$start_scale %||% 1,
                                 self$target_scale * (private$start_scale %||% 1),
                                 alpha)
      }

      invisible(self)
    }
  ),

  private = list(
    start_opacity = NULL,
    start_position = NULL,
    start_scale = NULL,
    start_fill_opacity = NULL,
    start_stroke_opacity = NULL
  )
)

# ---- Convenience Direction Variants ----

#' @rdname animation_fading
#' @description Fade in from a specific direction.
#' @param mobject Mobject to animate
#' @param run_time Duration
#' @param ... Additional args
#' @export
FadeInFromDown <- function(mobject, run_time = 1, ...) {
  FadeIn$new(mobject, shift = DOWN, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @export
FadeInFromUp <- function(mobject, run_time = 1, ...) {
  FadeIn$new(mobject, shift = UP, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @export
FadeInFromLeft <- function(mobject, run_time = 1, ...) {
  FadeIn$new(mobject, shift = LEFT, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @export
FadeInFromRight <- function(mobject, run_time = 1, ...) {
  FadeIn$new(mobject, shift = RIGHT, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @export
FadeOutToDown <- function(mobject, run_time = 1, ...) {
  FadeOut$new(mobject, shift = DOWN, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @export
FadeOutToUp <- function(mobject, run_time = 1, ...) {
  FadeOut$new(mobject, shift = UP, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @param scale Starting scale factor (> 1 shrinks in, < 1 grows in)
#' @export
FadeInFromLarge <- function(mobject, scale = 1.5, run_time = 1, ...) {
  FadeIn$new(mobject, scale = scale, run_time = run_time, ...)
}

#' @rdname animation_fading
#' @export
FadeInFromSmall <- function(mobject, scale = 0.5, run_time = 1, ...) {
  FadeIn$new(mobject, scale = scale, run_time = run_time, ...)
}
