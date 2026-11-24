#' @title Updater Animations & Value Tracking
#' @description Provides ValueTracker for animating numeric values, and
#'   updater-based animations that re-evaluate functions every frame.
#' @name animation_updaters
NULL

#' ValueTracker R6 Class
#'
#' @description A "virtual" mobject that holds a single numeric value.
#'   Used with updaters to drive animations based on changing parameters.
#' @export
ValueTracker <- R6::R6Class(
  "ValueTracker",
  inherit = MObject,
  public = list(
    #' @field value The tracked numeric value
    value = 0,

    #' @description Create a ValueTracker
    #' @param value Initial value
    initialize = function(value = 0) {
      super$initialize()
      self$value <- value
    },

    #' @description Get the current value
    get_value = function() self$value,

    #' @description Set the value
    #' @param new_value New numeric value
    set_value = function(new_value) {
      self$value <- new_value
      invisible(self)
    },

    #' @description Increment the value
    #' @param delta Amount to add
    increment_value = function(delta) {
      self$value <- self$value + delta
      invisible(self)
    },

    #' @description Animate the value from current to target
    #' @param target_value Final value
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @return An Animation that changes the value
    animate_value = function(target_value, run_time = 1,
                             rate_func = "smooth") {
      ValueAnimation$new(
        tracker = self,
        target_value = target_value,
        run_time = run_time,
        rate_func = rate_func
      )
    }
  )
)

#' ValueAnimation R6 Class
#'
#' @description Animation that interpolates a ValueTracker's value.
#' @keywords internal
ValueAnimation <- R6::R6Class(
  "ValueAnimation",
  inherit = Animation,
  public = list(
    #' @field tracker The ValueTracker to animate
    tracker = NULL,
    #' @field target_value The target value
    target_value = 0,

    #' @description Create a ValueAnimation
    #' @param tracker ValueTracker
    #' @param target_value Target value
    #' @param run_time Duration
    #' @param rate_func Easing
    initialize = function(tracker, target_value, run_time = 1,
                          rate_func = "smooth") {
      super$initialize(mobject = tracker, run_time = run_time,
                       rate_func = rate_func)
      self$tracker <- tracker
      self$target_value <- target_value
    },

    #' @description Save start value
    begin = function() {
      private$start_value <- self$tracker$value
      super$begin()
    },

    #' @description Interpolate value
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      self$tracker$value <- lerp(private$start_value, self$target_value, alpha)
      invisible(self)
    }
  ),

  private = list(
    start_value = NULL
  )
)

#' UpdateFromFunc R6 Class
#'
#' @description Animation that calls a function on the mobject each frame.
#' @export
UpdateFromFunc <- R6::R6Class(
  "UpdateFromFunc",
  inherit = Animation,
  public = list(
    #' @field update_func Function(mobject) called each frame
    update_func = NULL,

    #' @description Create an UpdateFromFunc animation
    #' @param mobject Mobject to update
    #' @param update_func Function(mobject) that modifies the mobject
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, update_func, run_time = 1,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$update_func <- update_func
    },

    #' @description Call update function
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      if (!is.null(self$update_func)) {
        self$update_func(self$mobject)
      }
      invisible(self)
    }
  )
)

#' UpdateFromAlphaFunc R6 Class
#'
#' @description Animation that calls a function(mobject, alpha) each frame,
#'   where alpha is the current progress.
#' @export
UpdateFromAlphaFunc <- R6::R6Class(
  "UpdateFromAlphaFunc",
  inherit = Animation,
  public = list(
    #' @field update_func Function(mobject, alpha) called each frame
    update_func = NULL,

    #' @description Create an UpdateFromAlphaFunc animation
    #' @param mobject Mobject to update
    #' @param update_func Function(mobject, alpha) that modifies the mobject
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, update_func, run_time = 1,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$update_func <- update_func
    },

    #' @description Call update function with alpha
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      if (!is.null(self$update_func)) {
        self$update_func(self$mobject, alpha)
      }
      invisible(self)
    }
  )
)

#' Create an always-redrawing mobject
#'
#' @description Creates a VMobject that is redrawn every frame by calling
#'   a constructor function. Useful for objects that depend on dynamic values.
#'
#' @param constructor A function() that returns a VMobject
#' @return A VMobject with an updater attached
#' @export
always_redraw <- function(constructor) {
  mob <- constructor()
  mob$add_updater(function(m, dt = 0) {
    new_mob <- constructor()
    if (inherits(new_mob, "VMobject") && inherits(m, "VMobject")) {
      m$points <- new_mob$points
      m$position <- new_mob$position
      m$color <- new_mob$color
      m$fill_color <- new_mob$fill_color
      m$fill_opacity <- new_mob$fill_opacity
      m$stroke_color <- new_mob$stroke_color
      m$stroke_opacity <- new_mob$stroke_opacity
      m$stroke_width <- new_mob$stroke_width
      m$submobjects <- new_mob$submobjects
    } else {
      m$position <- new_mob$position
      m$color <- new_mob$color
      m$opacity <- new_mob$opacity
    }
  })
  mob
}

#' DecimalNumber R6 Class
#'
#' @description A text mobject that displays a number, tracking a ValueTracker.
#' @export
DecimalNumber <- R6::R6Class(
  "DecimalNumber",
  inherit = MObject,
  public = list(
    #' @field number Current number value
    number = 0,
    #' @field num_decimal_places Decimal places to show
    num_decimal_places = 2,
    #' @field include_sign Whether to show + for positive numbers
    include_sign = FALSE,
    #' @field font_size Font size
    font_size = DEFAULT_FONT_SIZE,

    #' @description Create a DecimalNumber
    #' @param number Initial number
    #' @param num_decimal_places Decimal places
    #' @param include_sign Show + sign
    #' @param font_size Font size
    #' @param color Color
    #' @param ... Additional args
    initialize = function(number = 0, num_decimal_places = 2,
                          include_sign = FALSE, font_size = DEFAULT_FONT_SIZE,
                          color = WHITE, ...) {
      super$initialize(color = color, ...)
      self$type <- "text"
      self$number <- number
      self$num_decimal_places <- num_decimal_places
      self$include_sign <- include_sign
      self$font_size <- font_size
    },

    #' @description Get the formatted text representation
    get_text = function() {
      txt <- formatC(self$number, digits = self$num_decimal_places,
                     format = "f")
      if (self$include_sign && self$number > 0) {
        txt <- paste0("+", txt)
      }
      txt
    },

    #' @description Set the number value
    #' @param number New number
    set_value = function(number) {
      self$number <- number
      invisible(self)
    },

    #' @description Track a ValueTracker (add updater)
    #' @param tracker A ValueTracker
    track = function(tracker) {
      self$add_updater(function(m, dt = 0) {
        m$number <- tracker$get_value()
      })
      invisible(self)
    }
  )
)

#' Variable R6 Class
#'
#' @description Displays a variable name and its tracked value, e.g. "x = 3.14".
#' @export
Variable <- R6::R6Class(
  "Variable",
  inherit = MObject,
  public = list(
    #' @field tracker The ValueTracker
    tracker = NULL,
    #' @field label The variable label text
    label = "x",
    #' @field decimal The DecimalNumber display
    decimal = NULL,

    #' @description Create a Variable display
    #' @param var_name Variable name string
    #' @param value Initial value
    #' @param num_decimal_places Decimal places
    #' @param color Color
    #' @param ... Additional args
    initialize = function(var_name = "x", value = 0, num_decimal_places = 2,
                          color = WHITE, ...) {
      super$initialize(color = color, ...)
      self$type <- "text"
      self$label <- var_name
      self$tracker <- ValueTracker$new(value)
      self$decimal <- DecimalNumber$new(
        number = value,
        num_decimal_places = num_decimal_places,
        color = color
      )
      self$decimal$track(self$tracker)
    },

    #' @description Get the formatted display text
    get_text = function() {
      paste0(self$label, " = ", self$decimal$get_text())
    },

    #' @description Get the ValueTracker
    get_tracker = function() self$tracker
  )
)
