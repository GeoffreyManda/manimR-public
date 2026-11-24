#' @title Speed Control Animation
#' @description Animation wrapper that modifies the playback speed of
#'   another animation using a custom time-remapping function.
#' @name animation_speed
NULL

#' ChangeSpeed R6 Class
#'
#' @description Wraps another animation and applies a custom speed profile.
#'   The speed function maps normalised input time \[0,1\] to normalised
#'   output time \[0,1\], allowing slow-motion, fast-forward, or custom
#'   time curves.
#' @export
ChangeSpeed <- R6::R6Class(
  "ChangeSpeed",
  inherit = Animation,
  public = list(
    #' @field inner_animation The wrapped animation
    inner_animation = NULL,
    #' @field speed_func Function mapping t -> t' (both in \[0,1\])
    speed_func = NULL,
    #' @field affects_speed_updaters Whether speed changes affect updaters
    affects_speed_updaters = TRUE,

    #' @description Create a ChangeSpeed wrapper
    #' @param animation The animation to wrap
    #' @param speed_func Function(t) -> t' mapping time, where both
    #'   input and output are in \[0, 1\]
    #' @param run_time Override total run time
    #' @param affects_speed_updaters Whether updaters are affected
    initialize = function(animation, speed_func = NULL,
                          run_time = NULL, affects_speed_updaters = TRUE) {
      self$inner_animation <- animation
      self$speed_func <- speed_func %||% function(t) t
      self$affects_speed_updaters <- affects_speed_updaters

      rt <- run_time %||% animation$run_time
      super$initialize(mobject = animation$mobject, run_time = rt,
                       rate_func = "linear")
    },

    #' @description Delegate begin to inner animation
    begin = function() {
      self$inner_animation$begin()
      invisible(self)
    },

    #' @description Delegate finish to inner animation
    finish = function() {
      self$inner_animation$finish()
      invisible(self)
    },

    #' @description Apply speed remapping and delegate
    #' @param alpha Raw progress in \[0, 1\]
    interpolate = function(alpha) {
      remapped <- clamp(self$speed_func(alpha), 0, 1)
      self$inner_animation$interpolate(remapped)
      invisible(self)
    },

    #' @description No-op (delegation handled in interpolate)
    #' @param alpha Unused
    interpolate_mobject = function(alpha) {
      invisible(self)
    }
  )
)

# ---- Convenience Speed Functions ----

#' Create a linear speed ramp
#'
#' @description Returns a speed function that linearly ramps between
#'   two speed multipliers.
#' @param start_speed Starting speed multiplier
#' @param end_speed Ending speed multiplier
#' @return A function(t) -> t' for use with ChangeSpeed
#' @export
speed_ramp <- function(start_speed = 1, end_speed = 1) {
  function(t) {
    # Average speed determines the output mapping
    # speed(t) = start_speed + (end_speed - start_speed) * t
    # position(t) = integral of speed from 0 to t
    # = start_speed * t + (end_speed - start_speed) * t^2 / 2
    raw <- start_speed * t + (end_speed - start_speed) * t^2 / 2
    # Normalize so position(1) = 1
    total <- start_speed + (end_speed - start_speed) / 2
    if (abs(total) < 1e-10) return(t)
    clamp(raw / total, 0, 1)
  }
}

#' Create a slow-motion section
#'
#' @description Returns a speed function that slows down in a given interval.
#' @param slow_start Start of slow section (0-1)
#' @param slow_end End of slow section (0-1)
#' @param slow_factor Speed multiplier during slow section (< 1 = slower)
#' @return A function(t) -> t' for use with ChangeSpeed
#' @export
speed_with_slow_section <- function(slow_start = 0.3, slow_end = 0.7,
                                     slow_factor = 0.25) {
  function(t) {
    # Piecewise linear speed profile
    # Normal speed outside [slow_start, slow_end], reduced inside
    fast_duration <- 1 - (slow_end - slow_start)
    slow_duration <- slow_end - slow_start

    total_time <- fast_duration + slow_duration * slow_factor
    if (total_time < 1e-10) return(t)

    if (t < slow_start) {
      # Before slow section
      clamp(t / total_time, 0, 1)
    } else if (t < slow_end) {
      # In slow section
      time_in_slow <- (t - slow_start) * slow_factor
      clamp((slow_start + time_in_slow) / total_time, 0, 1)
    } else {
      # After slow section
      time_after <- t - slow_end
      time_in_slow_total <- slow_duration * slow_factor
      clamp((slow_start + time_in_slow_total + time_after) / total_time, 0, 1)
    }
  }
}
