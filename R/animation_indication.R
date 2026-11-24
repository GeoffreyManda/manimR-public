#' @title Indication Animations
#' @description Animations that draw attention to existing mobjects without
#'   permanently changing them. Used for emphasis and highlighting.
#' @name animation_indication
NULL

#' Flash R6 Class
#'
#' @description Creates a flash effect: expanding lines radiating from a point.
#' @rdname Flash-class
#' @export
Flash <- R6::R6Class(
  "Flash",
  inherit = Animation,
  public = list(
    #' @field point Center of the flash
    point = NULL,
    #' @field num_lines Number of radial lines
    num_lines = 12,
    #' @field line_length Length of flash lines
    line_length = 0.3,
    #' @field flash_radius Maximum expansion radius
    flash_radius = 0.4,
    #' @field flash_color Color of the flash
    flash_color = YELLOW,

    #' @description Create a Flash animation
    #' @param point Center point (or mobject to flash at)
    #' @param num_lines Number of radial lines
    #' @param line_length Length of each flash line
    #' @param flash_radius Maximum radius
    #' @param color Flash color
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(point = ORIGIN, num_lines = 12, line_length = 0.3,
                          flash_radius = 0.4, color = YELLOW,
                          run_time = 0.5, rate_func = "smooth", ...) {
      if (inherits(point, "MObject")) {
        point <- point$get_center()
      }
      self$point <- validate_position(point)
      self$num_lines <- num_lines
      self$line_length <- line_length
      self$flash_radius <- flash_radius
      self$flash_color <- validate_color(color)

      # Create the flash lines as a VGroup
      flash_mob <- VGroup$new()
      for (i in seq_len(num_lines)) {
        angle <- (i - 1) * TAU / num_lines
        dir <- c(cos(angle), sin(angle), 0)
        line <- Line$new(
          start = self$point + dir * flash_radius,
          end = self$point + dir * (flash_radius + line_length),
          color = self$flash_color
        )
        flash_mob$add(line)
      }

      super$initialize(mobject = flash_mob, run_time = run_time,
                       rate_func = rate_func, ...)
      self$is_introduced <- TRUE
      self$remover <- TRUE
    },

    #' @description Expand and fade flash lines
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      # Scale up and fade out
      scale <- 1 + alpha * 2
      opacity <- 1 - alpha

      mob$scale_factor <- scale
      mob$opacity <- opacity
      for (sub in mob$submobjects) {
        sub$opacity <- opacity
        if (inherits(sub, "VMobject")) {
          sub$stroke_opacity <- opacity
        }
      }
      invisible(self)
    }
  )
)

#' Indicate R6 Class
#'
#' @description Briefly scales up and recolors a mobject to draw attention.
#' @rdname Indicate-class
#' @export
Indicate <- R6::R6Class(
  "Indicate",
  inherit = Animation,
  public = list(
    #' @field indicate_color Color during indication
    indicate_color = YELLOW,
    #' @field scale_factor How much to scale up
    scale_factor = 1.2,

    #' @description Create an Indicate animation
    #' @param mobject Mobject to indicate
    #' @param color Highlight color
    #' @param scale_factor Scale multiplier at peak
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, color = YELLOW, scale_factor = 1.2,
                          run_time = 0.75, rate_func = "there_and_back", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$indicate_color <- validate_color(color)
      self$scale_factor <- scale_factor
    },

    #' @description Save original state
    begin = function() {
      private$original_color <- self$mobject$color
      private$original_scale <- self$mobject$scale_factor
      super$begin()
    },

    #' @description Scale and recolor, then return
    #' @param alpha Eased progress (rate_func handles there-and-back)
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      mob$scale_factor <- lerp(private$original_scale %||% 1,
                               self$scale_factor * (private$original_scale %||% 1),
                               alpha)
      mob$color <- interpolate_color(private$original_color %||% WHITE,
                                     self$indicate_color, alpha)
      invisible(self)
    },

    #' @description Restore original state
    finish = function() {
      self$mobject$color <- private$original_color %||% WHITE
      self$mobject$scale_factor <- private$original_scale %||% 1
      invisible(self)
    }
  ),

  private = list(
    original_color = NULL,
    original_scale = NULL
  )
)

#' Circumscribe R6 Class
#'
#' @description Draws a shape (rectangle or circle) around a mobject to
#'   highlight it, then removes the shape.
#' @rdname Circumscribe-class
#' @export
Circumscribe <- R6::R6Class(
  "Circumscribe",
  inherit = Animation,
  public = list(
    #' @field shape Type of surrounding shape: "rectangle" or "circle"
    shape = "rectangle",
    #' @field buff Buffer around the mobject
    buff = SMALL_BUFF,
    #' @field surrounding_mob The shape mobject drawn around the target
    surrounding_mob = NULL,

    #' @description Create a Circumscribe animation
    #' @param mobject Mobject to circumscribe
    #' @param shape "rectangle" or "circle"
    #' @param color Color of the circumscription
    #' @param buff Buffer distance
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, shape = "rectangle", color = YELLOW,
                          buff = SMALL_BUFF, run_time = 1,
                          rate_func = "double_smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$shape <- shape
      self$buff <- buff

      # Create the surrounding shape
      bb <- mobject$get_bounding_box()
      w <- bb$max[1] - bb$min[1] + 2 * buff
      h <- bb$max[2] - bb$min[2] + 2 * buff
      center <- (bb$min + bb$max) / 2

      if (shape == "circle") {
        r <- max(w, h) / 2
        self$surrounding_mob <- Circle$new(radius = r, color = color)
        self$surrounding_mob$position <- center
      } else {
        self$surrounding_mob <- Rectangle$new(width = w, height = h, color = color)
        self$surrounding_mob$position <- center
      }
      self$surrounding_mob$stroke_width <- 3
    },

    #' @description Draw and erase the surrounding shape
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      if (is.null(self$surrounding_mob)) return(invisible(self))

      # Phase 1 (0-0.5): Draw
      # Phase 2 (0.5-1): Erase
      if (alpha <= 0.5) {
        draw_progress <- alpha * 2
        self$surrounding_mob$stroke_opacity <- draw_progress
      } else {
        erase_progress <- (alpha - 0.5) * 2
        self$surrounding_mob$stroke_opacity <- 1 - erase_progress
      }

      invisible(self)
    }
  )
)

#' Wiggle R6 Class
#'
#' @description Wiggles a mobject back and forth.
#' @rdname Wiggle-class
#' @export
Wiggle <- R6::R6Class(
  "Wiggle",
  inherit = Animation,
  public = list(
    #' @field scale_value Peak scale during wiggle
    scale_value = 1.1,
    #' @field rotation_angle Peak rotation angle
    rotation_angle = 0.05,
    #' @field n_wiggles Number of back-and-forth cycles
    n_wiggles = 6,

    #' @description Create a Wiggle animation
    #' @param mobject Mobject to wiggle
    #' @param scale_value Peak scale
    #' @param rotation_angle Peak rotation (radians)
    #' @param n_wiggles Number of wiggle cycles
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, scale_value = 1.1, rotation_angle = 0.05,
                          n_wiggles = 6, run_time = 2,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$scale_value <- scale_value
      self$rotation_angle <- rotation_angle
      self$n_wiggles <- n_wiggles
    },

    #' @description Save original state
    begin = function() {
      private$original_scale <- self$mobject$scale_factor
      private$original_rotation <- self$mobject$rotation
      super$begin()
    },

    #' @description Apply wiggle oscillation
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      # Sinusoidal wiggle with envelope
      envelope <- sin(pi * alpha)  # 0 at start/end, 1 at middle
      wiggle <- sin(self$n_wiggles * TAU * alpha)

      mob$scale_factor <- (private$original_scale %||% 1) *
        (1 + (self$scale_value - 1) * envelope)
      mob$rotation <- (private$original_rotation %||% 0) +
        self$rotation_angle * wiggle * envelope

      invisible(self)
    },

    #' @description Restore original state
    finish = function() {
      self$mobject$scale_factor <- private$original_scale %||% 1
      self$mobject$rotation <- private$original_rotation %||% 0
      invisible(self)
    }
  ),

  private = list(
    original_scale = NULL,
    original_rotation = NULL
  )
)

#' ApplyWave R6 Class
#'
#' @description Applies a wave deformation that passes through the mobject.
#' @export
ApplyWave <- R6::R6Class(
  "ApplyWave",
  inherit = Animation,
  public = list(
    #' @field direction Direction of the wave displacement
    direction = NULL,
    #' @field amplitude Wave amplitude
    amplitude = 0.2,
    #' @field wave_length Wavelength as proportion of mobject width
    wave_length = 1,

    #' @description Create an ApplyWave animation
    #' @param mobject VMobject to deform
    #' @param direction Direction of displacement
    #' @param amplitude Wave height
    #' @param wave_length Relative wavelength
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, direction = UP, amplitude = 0.2,
                          wave_length = 1, run_time = 2,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$direction <- validate_position(direction)
      self$amplitude <- amplitude
      self$wave_length <- wave_length
    },

    #' @description Apply travelling wave to points
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject") || is.null(self$starting_mobject)) {
        return(invisible(self))
      }
      if (!self$starting_mobject$has_points()) return(invisible(self))

      start_pts <- self$starting_mobject$points
      new_pts <- start_pts

      # Get extent along propagation direction (x by default)
      bb <- self$starting_mobject$get_bounding_box()
      width <- bb$max[1] - bb$min[1]
      if (width < 1e-10) width <- 1

      dir <- normalize(self$direction)
      for (i in seq_len(nrow(start_pts))) {
        # Position along propagation axis
        x_norm <- (start_pts[i, 1] - bb$min[1]) / width
        # Travelling wave: phase depends on position and time
        phase <- TAU * (x_norm / self$wave_length - alpha * 2)
        # Envelope: wave passes through, zero at start/end
        envelope <- sin(pi * alpha) * sin(pi * x_norm)
        displacement <- dir * self$amplitude * sin(phase) * envelope
        new_pts[i, ] <- start_pts[i, ] + displacement
      }

      mob$points <- new_pts
      invisible(self)
    }
  )
)

#' FocusOn R6 Class
#'
#' @description Draws a shrinking circle to focus attention on a point.
#' @export
FocusOn <- R6::R6Class(
  "FocusOn",
  inherit = Animation,
  public = list(
    #' @field point Focus point
    point = NULL,
    #' @field focus_color Color of the focus circle
    focus_color = NULL,

    #' @description Create a FocusOn animation
    #' @param point Point or mobject to focus on
    #' @param color Circle color
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(point = ORIGIN, color = GREY,
                          run_time = 0.5, rate_func = "smooth", ...) {
      if (inherits(point, "MObject")) {
        point <- point$get_center()
      }
      self$point <- validate_position(point)
      self$focus_color <- validate_color(color)

      focus_circle <- Circle$new(radius = FRAME_X_RADIUS, color = color)
      focus_circle$position <- self$point
      focus_circle$stroke_width <- 5

      super$initialize(mobject = focus_circle, run_time = run_time,
                       rate_func = rate_func, ...)
      self$remover <- TRUE
    },

    #' @description Shrink circle to point
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      target_radius <- FRAME_X_RADIUS * (1 - alpha)
      mob$scale_factor <- max(0.01, 1 - alpha)
      mob$stroke_opacity <- 1 - alpha * 0.5
      invisible(self)
    }
  )
)

#' ShowPassingFlash R6 Class
#'
#' @description A brief flash that travels along the path of a VMobject.
#' @export
ShowPassingFlash <- R6::R6Class(
  "ShowPassingFlash",
  inherit = Animation,
  public = list(
    #' @field time_width Width of the flash as proportion of path
    time_width = 0.3,

    #' @description Create a ShowPassingFlash animation
    #' @param mobject VMobject whose path the flash follows
    #' @param time_width Width of flash (0-1)
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, time_width = 0.3, run_time = 1,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$time_width <- time_width
    },

    #' @description Save starting state
    begin = function() {
      private$original_stroke_opacity <- self$mobject$stroke_opacity
      self$mobject$stroke_opacity <- 0
      super$begin()
    },

    #' @description Show flash travelling along path
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject") || !mob$has_points()) {
        return(invisible(self))
      }

      # The flash is visible only in a window of [alpha - tw/2, alpha + tw/2]
      tw <- self$time_width
      n_curves <- mob$get_num_curves()
      if (n_curves == 0) return(invisible(self))

      # Set all strokes to zero, then brighten the active window
      mob$stroke_opacity <- 0

      # For simplicity, modulate opacity per-curve
      for (i in seq_len(n_curves)) {
        curve_center <- (i - 0.5) / n_curves
        dist <- abs(curve_center - alpha)
        if (dist < tw / 2) {
          brightness <- 1 - (dist / (tw / 2))
          # We can't set per-curve opacity easily, so approximate
          mob$stroke_opacity <- max(mob$stroke_opacity, brightness)
        }
      }

      invisible(self)
    },

    #' @description Restore original opacity
    finish = function() {
      self$mobject$stroke_opacity <- private$original_stroke_opacity %||% 1
      invisible(self)
    }
  ),

  private = list(
    original_stroke_opacity = NULL
  )
)
