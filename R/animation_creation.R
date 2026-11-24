#' @title Creation Animations
#' @description Animations that introduce mobjects by drawing them into the scene.
#' @name animation_creation
NULL

#' Create R6 Class
#'
#' @description Draws a VMobject by progressively revealing its strokes,
#'   then filling it in. The canonical "write-on" effect.
#' @export
Create <- R6::R6Class(
  "Create",
  inherit = Animation,
  public = list(
    #' @description Create a Create animation
    #' @param mobject VMobject to create
    #' @param run_time Duration
    #' @param rate_func Easing function
    #' @param ... Additional args
    initialize = function(mobject, run_time = 2, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$is_introduced <- TRUE
    },

    #' @description Save starting state
    begin = function() {
      if (inherits(self$mobject, "VMobject")) {
        private$target_fill_opacity <- self$mobject$fill_opacity
        private$target_stroke_opacity <- self$mobject$stroke_opacity
        self$mobject$fill_opacity <- 0
        self$mobject$stroke_opacity <- 0
      }
      super$begin()
    },

    #' @description Progressive reveal of stroke then fill
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject") || !mob$has_points()) return(invisible(self))

      n_curves <- mob$get_num_curves()
      if (n_curves == 0) return(invisible(self))

      # Phase 1 (0 to 0.5): Draw strokes
      # Phase 2 (0.5 to 1.0): Fill in
      if (alpha <= 0.5) {
        stroke_alpha <- alpha * 2
        mob$stroke_opacity <- stroke_alpha * (private$target_stroke_opacity %||% 1)
        mob$fill_opacity <- 0

        # Reveal curves progressively
        curves_to_show <- max(1, ceiling(stroke_alpha * n_curves))
        total_pts <- nrow(self$starting_mobject$points)
        pts_to_show <- min(curves_to_show * 4, total_pts)
        mob$points <- self$starting_mobject$points[1:pts_to_show, , drop = FALSE]
      } else {
        fill_alpha <- (alpha - 0.5) * 2
        mob$stroke_opacity <- private$target_stroke_opacity %||% 1
        mob$fill_opacity <- fill_alpha * (private$target_fill_opacity %||% 0)
        mob$points <- self$starting_mobject$points
      }

      invisible(self)
    },

    #' @description Ensure final state is correct
    finish = function() {
      mob <- self$mobject
      if (inherits(mob, "VMobject") && !is.null(self$starting_mobject)) {
        mob$points <- self$starting_mobject$points
        mob$stroke_opacity <- private$target_stroke_opacity %||% 1
        mob$fill_opacity <- private$target_fill_opacity %||% 0
      }
      invisible(self)
    }
  ),

  private = list(
    target_fill_opacity = NULL,
    target_stroke_opacity = NULL
  )
)

#' Write R6 Class
#'
#' @description Similar to Create but optimised for text/LaTeX: draws each
#'   sub-glyph sequentially.
#' @export
Write <- R6::R6Class(
  "Write",
  inherit = Create,
  public = list(
    #' @description Create a Write animation
    #' @param mobject Text or LaTeX mobject
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, run_time = 2, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
    }
  )
)

#' DrawBorderThenFill R6 Class
#'
#' @description Draws the border of a VMobject, then fills it in.
#' @export
DrawBorderThenFill <- R6::R6Class(
  "DrawBorderThenFill",
  inherit = Animation,
  public = list(
    #' @field stroke_color Color used for the border drawing phase
    stroke_color = NULL,

    #' @description Create a DrawBorderThenFill animation
    #' @param mobject VMobject to animate
    #' @param run_time Duration
    #' @param stroke_color Border color during draw phase
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, run_time = 2, stroke_color = NULL,
                          rate_func = "double_smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$stroke_color <- stroke_color
      self$is_introduced <- TRUE
    },

    #' @description Save starting state and clear
    begin = function() {
      if (inherits(self$mobject, "VMobject")) {
        private$target_fill_color <- self$mobject$fill_color
        private$target_fill_opacity <- self$mobject$fill_opacity
        private$target_stroke_color <- self$mobject$stroke_color %||% self$mobject$color
        private$target_stroke_width <- self$mobject$stroke_width
        private$target_stroke_opacity <- self$mobject$stroke_opacity

        self$mobject$fill_opacity <- 0
        self$mobject$stroke_opacity <- 0
        if (!is.null(self$stroke_color)) {
          self$mobject$stroke_color <- validate_color(self$stroke_color)
        }
      }
      super$begin()
    },

    #' @description Draw border then fill
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject")) return(invisible(self))

      if (alpha <= 0.5) {
        # Phase 1: Draw border
        stroke_progress <- alpha * 2
        mob$stroke_opacity <- stroke_progress * (private$target_stroke_opacity %||% 1)
        mob$fill_opacity <- 0
        mob$stroke_width <- private$target_stroke_width %||% DEFAULT_STROKE_WIDTH
      } else {
        # Phase 2: Fill in + transition stroke color
        fill_progress <- (alpha - 0.5) * 2
        mob$stroke_opacity <- private$target_stroke_opacity %||% 1
        mob$fill_opacity <- fill_progress * (private$target_fill_opacity %||% 0)

        # Transition stroke color back to target
        if (!is.null(self$stroke_color) && !is.null(private$target_stroke_color)) {
          mob$stroke_color <- interpolate_color(
            self$stroke_color, private$target_stroke_color, fill_progress
          )
        }
      }

      invisible(self)
    }
  ),

  private = list(
    target_fill_color = NULL,
    target_fill_opacity = NULL,
    target_stroke_color = NULL,
    target_stroke_width = NULL,
    target_stroke_opacity = NULL
  )
)

#' Uncreate R6 Class
#'
#' @description Reverse of Create - progressively removes the mobject.
#' @export
Uncreate <- R6::R6Class(
  "Uncreate",
  inherit = Create,
  public = list(
    #' @description Create an Uncreate animation
    #' @param mobject VMobject to remove
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, run_time = 2, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$remover <- TRUE
      self$is_introduced <- FALSE
    },

    #' @description Reverse the Create interpolation
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      # Run Create in reverse
      super$interpolate_mobject(1 - alpha)
    }
  )
)

#' ShowIncreasingSubsets R6 Class
#'
#' @description Progressively shows submobjects of a VGroup one at a time.
#' @export
ShowIncreasingSubsets <- R6::R6Class(
  "ShowIncreasingSubsets",
  inherit = Animation,
  public = list(
    #' @description Create a ShowIncreasingSubsets animation
    #' @param group A VGroup or Group
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(group, run_time = 2, rate_func = "linear", ...) {
      super$initialize(mobject = group, run_time = run_time,
                       rate_func = rate_func, ...)
      self$is_introduced <- TRUE
    },

    #' @description Show progressively more submobjects
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject") && !inherits(mob, "Group")) {
        return(invisible(self))
      }

      n <- length(mob$submobjects)
      if (n == 0) return(invisible(self))

      n_to_show <- max(0, ceiling(alpha * n))
      for (i in seq_len(n)) {
        sub <- mob$submobjects[[i]]
        if (i <= n_to_show) {
          sub$opacity <- 1
        } else {
          sub$opacity <- 0
        }
      }

      invisible(self)
    }
  )
)

#' ShowSubmobjectsOneByOne R6 Class
#'
#' @description Shows submobjects one at a time, hiding previous ones.
#' @export
ShowSubmobjectsOneByOne <- R6::R6Class(
  "ShowSubmobjectsOneByOne",
  inherit = Animation,
  public = list(
    #' @description Create a ShowSubmobjectsOneByOne animation
    #' @param group A VGroup or Group
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(group, run_time = 2, rate_func = "linear", ...) {
      super$initialize(mobject = group, run_time = run_time,
                       rate_func = rate_func, ...)
      self$is_introduced <- TRUE
    },

    #' @description Show exactly one submobject at a time
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject") && !inherits(mob, "Group")) {
        return(invisible(self))
      }

      n <- length(mob$submobjects)
      if (n == 0) return(invisible(self))

      current_idx <- max(1, min(ceiling(alpha * n), n))
      for (i in seq_len(n)) {
        sub <- mob$submobjects[[i]]
        sub$opacity <- if (i == current_idx) 1 else 0
      }

      invisible(self)
    }
  )
)
