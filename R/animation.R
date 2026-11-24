#' @title Animation Base Class
#' @description
#' The core Animation class hierarchy. Every animation in manimR is an R6
#' object that knows how to interpolate a mobject from its starting state to
#' its final state over a normalised alpha in \[0, 1\].
#'
#' @name animation
#' @keywords internal
NULL

#' Animation R6 Class
#'
#' @description
#' Base class for all animations. An Animation takes a mobject through a
#' continuous transformation parameterised by alpha in \[0, 1\].
#'
#' Subclasses override `interpolate_mobject(alpha)` to define the motion.
#' The lifecycle is: `begin()` -> repeated `interpolate(alpha)` -> `finish()`.
#'
#' @export
Animation <- R6::R6Class(

"Animation",
  public = list(
    #' @field mobject The mobject being animated
    mobject = NULL,
    #' @field run_time Duration in seconds
    run_time = 1,
    #' @field rate_func Easing function name or function
    rate_func = "smooth",
    #' @field name Human-readable name
    name = NULL,
    #' @field remover Whether to remove the mobject after animation
    remover = FALSE,
    #' @field suspend_mobject_updating Pause updaters during animation
    suspend_mobject_updating = TRUE,
    #' @field starting_mobject Snapshot of mobject at begin()
    starting_mobject = NULL,
    #' @field is_introduced Whether this animation introduces the mobject
    is_introduced = FALSE,

    #' @description Create a new Animation
    #' @param mobject The mobject to animate
    #' @param run_time Duration in seconds
    #' @param rate_func Easing function (name or function)
    #' @param name Optional name
    #' @param remover Remove mobject when done
    #' @param suspend_mobject_updating Pause updaters
    #' @param ... Additional parameters stored in private$params
    initialize = function(mobject = NULL, run_time = 1,
                          rate_func = "smooth", name = NULL,
                          remover = FALSE,
                          suspend_mobject_updating = TRUE, ...) {
      self$mobject <- mobject
      self$run_time <- validate_duration(run_time)
      self$rate_func <- rate_func
      self$name <- name %||% class(self)[1]
      self$remover <- remover
      self$suspend_mobject_updating <- suspend_mobject_updating
      private$params <- list(...)
    },

    #' @description Called before the animation starts.
    #'   Saves a copy of the starting mobject state.
    begin = function() {
      if (!is.null(self$mobject) && inherits(self$mobject, "MObject")) {
        self$starting_mobject <- self$mobject$copy()
      }
      self$interpolate(0)
      invisible(self)
    },

    #' @description Called after the animation ends.
    #'   Ensures the mobject is in its final state.
    finish = function() {
      self$interpolate(1)
      invisible(self)
    },

    #' @description Apply easing and delegate to interpolate_mobject.
    #' @param alpha Raw progress in \[0, 1\]
    interpolate = function(alpha) {
      alpha <- clamp(alpha, 0, 1)
      eased <- private$apply_rate_func(alpha)
      self$interpolate_mobject(eased)
      invisible(self)
    },

    #' @description Override in subclasses to define the actual transformation.
    #' @param alpha Eased progress in \[0, 1\]
    interpolate_mobject = function(alpha) {
      # Default: no-op (Wait animation)
      invisible(self)
    },

    #' @description Get all mobjects involved in this animation
    #' @return List of mobjects
    get_all_mobjects = function() {
      result <- list()
      if (!is.null(self$mobject)) result <- c(result, list(self$mobject))
      if (!is.null(self$starting_mobject)) {
        result <- c(result, list(self$starting_mobject))
      }
      result
    },

    #' @description Create a copy of this animation
    copy = function() {
      anim <- self$clone(deep = TRUE)
      anim
    },

    #' @description Check if this animation is an introducer (adds mobject to scene)
    is_introducer = function() {
      self$is_introduced
    },

    #' @description Check if this animation is a remover (removes mobject from scene)
    is_remover = function() {
      self$remover
    },

    #' @description Convert to a timeline-compatible animation spec (legacy bridge)
    #' @param start_time Start time in the scene timeline
    #' @return A list compatible with the existing rendering pipeline
    to_anim_spec = function(start_time = 0) {
      list(
        type = tolower(self$name),
        target = if (!is.null(self$mobject)) self$mobject$id else NULL,
        start_time = start_time,
        duration = self$run_time,
        easing = if (is.character(self$rate_func)) self$rate_func else "smooth",
        animation_object = self,
        params = private$params
      )
    },

    #' @description Print animation summary
    print = function() {
      cli::cli_text("{.cls {class(self)[1]}} animation")
      cli::cli_ul()
      cli::cli_li("Mobject: {if (!is.null(self$mobject)) class(self$mobject)[1] else 'NULL'}")
      cli::cli_li("Duration: {self$run_time}s")
      cli::cli_li("Rate func: {if (is.character(self$rate_func)) self$rate_func else 'custom'}")
      cli::cli_end()
      invisible(self)
    }
  ),

  private = list(
    params = list(),

    apply_rate_func = function(alpha) {
      if (is.function(self$rate_func)) {
        return(self$rate_func(alpha))
      }
      if (is.character(self$rate_func)) {
        fn <- get_easing(self$rate_func)
        if (!is.null(fn)) return(fn(alpha))
      }
      alpha
    }
  )
)

# ---- Animation Composition Classes ----

#' AnimationGroup R6 Class
#'
#' @description Plays multiple animations simultaneously (in parallel).
#' @export
AnimationGroup <- R6::R6Class(
  "AnimationGroup",
  inherit = Animation,
  public = list(
    #' @field animations List of child animations
    animations = NULL,
    #' @field group The VGroup of all animated mobjects
    group = NULL,
    #' @field lag_ratio Stagger ratio (0 = all together, 1 = sequential)
    lag_ratio = 0,

    #' @description Create an AnimationGroup
    #' @param ... Animation objects to play together
    #' @param run_time Override total run time (NULL = max of children)
    #' @param rate_func Rate function applied to the group
    #' @param lag_ratio Stagger ratio between animations (0-1)
    initialize = function(..., run_time = NULL, rate_func = "linear",
                          lag_ratio = 0) {
      self$animations <- list(...)
      self$lag_ratio <- clamp(lag_ratio, 0, 1)

      # Compute effective run time
      if (is.null(run_time)) {
        if (length(self$animations) == 0) {
          run_time <- 1
        } else {
          max_rt <- max(vapply(self$animations,
                               function(a) a$run_time, numeric(1)))
          if (lag_ratio > 0 && length(self$animations) > 1) {
            n <- length(self$animations)
            run_time <- max_rt + (n - 1) * lag_ratio * max_rt
          } else {
            run_time <- max_rt
          }
        }
      }

      super$initialize(run_time = run_time, rate_func = rate_func)
    },

    #' @description Begin all child animations
    begin = function() {
      for (anim in self$animations) {
        anim$begin()
      }
      invisible(self)
    },

    #' @description Finish all child animations
    finish = function() {
      for (anim in self$animations) {
        anim$finish()
      }
      invisible(self)
    },

    #' @description Interpolate all child animations with lag
    #' @param alpha Overall progress in \[0,1\]
    interpolate_mobject = function(alpha) {
      n <- length(self$animations)
      if (n == 0) return(invisible(self))

      for (i in seq_len(n)) {
        anim <- self$animations[[i]]

        if (self$lag_ratio > 0 && n > 1) {
          # Calculate start and end alpha for this animation
          anim_duration <- anim$run_time / self$run_time
          anim_start <- (i - 1) * self$lag_ratio * (anim$run_time / self$run_time)
          anim_end <- anim_start + anim_duration

          if (alpha < anim_start) {
            sub_alpha <- 0
          } else if (alpha > anim_end) {
            sub_alpha <- 1
          } else {
            sub_alpha <- (alpha - anim_start) / (anim_end - anim_start)
          }
        } else {
          # Scale alpha to child's run time
          sub_alpha <- alpha
        }

        anim$interpolate(clamp(sub_alpha, 0, 1))
      }
      invisible(self)
    },

    #' @description Get all mobjects from child animations
    get_all_mobjects = function() {
      mobjects <- list()
      for (anim in self$animations) {
        mobjects <- c(mobjects, anim$get_all_mobjects())
      }
      unique(mobjects)
    },

    #' @description Convert to timeline spec for rendering
    #' @param start_time Start time offset
    to_anim_spec = function(start_time = 0) {
      # Return a group spec containing all child animation specs
      child_specs <- list()
      for (i in seq_along(self$animations)) {
        anim <- self$animations[[i]]
        # Calculate per-child start time accounting for lag
        if (self$lag_ratio > 0 && length(self$animations) > 1) {
          child_start <- start_time + (i - 1) * self$lag_ratio * anim$run_time
        } else {
          child_start <- start_time
        }
        child_specs <- c(child_specs, list(anim$to_anim_spec(start_time = child_start)))
      }
      list(
        type = "animation_group",
        children = child_specs,
        start_time = start_time,
        duration = self$run_time,
        easing = if (is.character(self$rate_func)) self$rate_func else "smooth",
        animation_object = self
      )
    }
  )
)

#' Succession R6 Class
#'
#' @description Plays animations one after another (in sequence).
#' @export
Succession <- R6::R6Class(
  "Succession",
  inherit = Animation,
  public = list(
    #' @field animations List of sequential animations
    animations = NULL,
    #' @field cumulative_times Cumulative start times for each animation
    cumulative_times = NULL,

    #' @description Create a Succession of animations
    #' @param ... Animation objects to play in sequence
    #' @param rate_func Rate function applied to the overall sequence
    initialize = function(..., rate_func = "linear") {
      self$animations <- list(...)
      n <- length(self$animations)

      if (n == 0) {
        total_time <- 1
        self$cumulative_times <- c(0, 1)
      } else {
        times <- vapply(self$animations, function(a) a$run_time, numeric(1))
        total_time <- sum(times)
        self$cumulative_times <- c(0, cumsum(times)) / total_time
      }

      super$initialize(run_time = total_time, rate_func = rate_func)
    },

    #' @description Begin the first animation
    begin = function() {
      if (length(self$animations) > 0) {
        self$animations[[1]]$begin()
      }
      invisible(self)
    },

    #' @description Finish the last animation
    finish = function() {
      n <- length(self$animations)
      if (n > 0) {
        self$animations[[n]]$finish()
      }
      invisible(self)
    },

    #' @description Interpolate the appropriate child animation
    #' @param alpha Overall progress in \[0,1\]
    interpolate_mobject = function(alpha) {
      n <- length(self$animations)
      if (n == 0) return(invisible(self))

      # Find which animation is active
      active_idx <- findInterval(alpha, self$cumulative_times)
      active_idx <- max(1, min(active_idx, n))

      # Finish all prior animations
      if (active_idx > 1) {
        for (i in seq_len(active_idx - 1)) {
          self$animations[[i]]$finish()
        }
      }

      # Calculate sub-alpha for active animation
      start_t <- self$cumulative_times[active_idx]
      end_t <- self$cumulative_times[active_idx + 1]
      duration <- end_t - start_t

      if (duration > 1e-10) {
        sub_alpha <- (alpha - start_t) / duration
      } else {
        sub_alpha <- 1
      }

      self$animations[[active_idx]]$interpolate(clamp(sub_alpha, 0, 1))
      invisible(self)
    },

    #' @description Get all mobjects from child animations
    get_all_mobjects = function() {
      mobjects <- list()
      for (anim in self$animations) {
        mobjects <- c(mobjects, anim$get_all_mobjects())
      }
      unique(mobjects)
    }
  )
)

#' LaggedStart R6 Class
#'
#' @description Plays animations with a stagger/lag between each start.
#'   Equivalent to AnimationGroup with a non-zero lag_ratio.
#' @export
LaggedStart <- R6::R6Class(
  "LaggedStart",
  inherit = AnimationGroup,
  public = list(
    #' @description Create a LaggedStart animation
    #' @param ... Animation objects
    #' @param lag_ratio Proportion of overlap (default 0.5)
    #' @param run_time Override total run time
    #' @param rate_func Rate function
    initialize = function(..., lag_ratio = 0.5, run_time = NULL,
                          rate_func = "linear") {
      super$initialize(..., run_time = run_time, rate_func = rate_func,
                       lag_ratio = lag_ratio)
    }
  )
)

#' LaggedStartMap R6 Class
#'
#' @description Applies the same animation constructor to each element of
#'   a VGroup/list with a stagger.
#' @export
LaggedStartMap <- R6::R6Class(
  "LaggedStartMap",
  inherit = AnimationGroup,
  public = list(
    #' @description Create a LaggedStartMap
    #' @param anim_class Animation class (e.g., FadeIn)
    #' @param mobjects List of mobjects or a VGroup
    #' @param lag_ratio Stagger ratio
    #' @param run_time Override total run time
    #' @param rate_func Rate function
    #' @param ... Additional args passed to each animation constructor
    initialize = function(anim_class, mobjects, lag_ratio = 0.5,
                          run_time = NULL, rate_func = "linear", ...) {
      # Extract mobjects from VGroup if needed
      if (inherits(mobjects, "VGroup") || inherits(mobjects, "Group")) {
        mob_list <- mobjects$submobjects
      } else if (is.list(mobjects)) {
        mob_list <- mobjects
      } else {
        mob_list <- list(mobjects)
      }

      # Create an animation for each mobject
      anims <- lapply(mob_list, function(mob) {
        anim_class$new(mobject = mob, ...)
      })

      do.call(super$initialize,
              c(anims, list(lag_ratio = lag_ratio, run_time = run_time,
                            rate_func = rate_func)))
    }
  )
)

#' Wait R6 Class
#'
#' @description A no-op animation that holds the scene for a duration.
#' @rdname Wait-class
#' @export
Wait <- R6::R6Class(
  "Wait",
  inherit = Animation,
  public = list(
    #' @description Create a Wait animation
    #' @param run_time Duration to wait
    initialize = function(run_time = 1) {
      super$initialize(run_time = run_time, rate_func = "linear")
    },

    #' @description No-op interpolation
    #' @param alpha Progress (unused)
    interpolate_mobject = function(alpha) {
      invisible(self)
    }
  )
)

# ---- AnimationBuilder (for mobject$animate chaining) ----

#' AnimationBuilder R6 Class
#'
#' @description
#' Enables method chaining syntax for animations:
#' `square$animate$shift(RIGHT)$set_color(RED)` produces an animation
#' that simultaneously shifts and recolors the square.
#'
#' This is used internally by `MObject$animate` (active binding).
#'
#' @export
AnimationBuilder <- R6::R6Class(
  "AnimationBuilder",
  public = list(
    #' @field mobject The mobject to animate
    mobject = NULL,
    #' @field run_time Animation duration
    run_time = 1,
    #' @field rate_func Easing function
    rate_func = "smooth",

    #' @description Create an AnimationBuilder
    #' @param mobject The mobject to animate
    #' @param run_time Duration
    #' @param rate_func Easing function
    initialize = function(mobject, run_time = 1, rate_func = "smooth") {
      self$mobject <- mobject
      self$run_time <- run_time
      self$rate_func <- rate_func
      private$method_calls <- list()
    },

    #' @description Record a method call for later playback
    #' @param method_name Name of the method
    #' @param ... Arguments to the method
    record = function(method_name, ...) {
      private$method_calls <- c(private$method_calls,
                                 list(list(method = method_name, args = list(...))))
      invisible(self)
    },

    #' @description Build the animation from recorded calls
    #' @return An Animation object
    build = function() {
      # Create a target copy and apply all recorded methods
      target <- self$mobject$copy()
      for (call in private$method_calls) {
        do.call(target[[call$method]], call$args)
      }

      # Return a MethodAnimation that interpolates between current and target
      MethodAnimation$new(
        mobject = self$mobject,
        target = target,
        run_time = self$run_time,
        rate_func = self$rate_func
      )
    },

    # -- Chainable transform methods --

    #' @description Animate a shift
    #' @param direction Direction vector
    shift = function(direction) {
      self$record("shift", direction)
      invisible(self)
    },

    #' @description Animate a move_to
    #' @param point Target position
    move_to = function(point) {
      self$record("move_to", point)
      invisible(self)
    },

    #' @description Animate a rotation
    #' @param angle Angle in radians
    #' @param ... Additional args
    rotate = function(angle, ...) {
      self$record("rotate", angle, ...)
      invisible(self)
    },

    #' @description Animate a scale
    #' @param factor Scale factor
    #' @param ... Additional args
    scale = function(factor, ...) {
      self$record("scale", factor, ...)
      invisible(self)
    },

    #' @description Animate a color change
    #' @param color New color
    set_color = function(color) {
      self$record("set_color", color)
      invisible(self)
    },

    #' @description Animate fill change
    #' @param color Fill color
    #' @param opacity Fill opacity
    set_fill = function(color = NULL, opacity = NULL) {
      self$record("set_fill", color, opacity)
      invisible(self)
    },

    #' @description Animate stroke change
    #' @param color Stroke color
    #' @param width Stroke width
    #' @param opacity Stroke opacity
    set_stroke = function(color = NULL, width = NULL, opacity = NULL) {
      self$record("set_stroke", color, width, opacity)
      invisible(self)
    },

    #' @description Animate set_opacity
    #' @param opacity New opacity
    set_opacity = function(opacity) {
      self$record("set_opacity", opacity)
      invisible(self)
    },

    #' @description Animate next_to
    #' @param other Target mobject or position
    #' @param direction Direction
    #' @param buff Buffer distance
    next_to = function(other, direction = RIGHT, buff = MED_SMALL_BUFF) {
      self$record("next_to", other, direction, buff)
      invisible(self)
    },

    #' @description Animate to_edge
    #' @param edge Edge direction
    #' @param buff Buffer
    to_edge = function(edge, buff = MED_LARGE_BUFF) {
      self$record("to_edge", edge, buff)
      invisible(self)
    }
  ),

  private = list(
    method_calls = list()
  )
)

#' MethodAnimation (internal)
#'
#' @description Animation created by AnimationBuilder that interpolates
#'   a mobject from its current state to a target state.
#' @keywords internal
MethodAnimation <- R6::R6Class(
  "MethodAnimation",
  inherit = Animation,
  public = list(
    #' @field target The target state
    target = NULL,

    #' @description Create a MethodAnimation
    #' @param mobject Current mobject
    #' @param target Target state (copy with methods applied)
    #' @param run_time Duration
    #' @param rate_func Easing
    initialize = function(mobject, target, run_time = 1,
                          rate_func = "smooth") {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func)
      self$target <- target
    },

    #' @description Interpolate between starting state and target
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      if (is.null(self$starting_mobject) || is.null(self$target)) {
        return(invisible(self))
      }

      mob <- self$mobject

      # Interpolate position
      mob$position <- lerp(self$starting_mobject$position,
                           self$target$position, alpha)

      # Interpolate opacity
      mob$opacity <- lerp(self$starting_mobject$opacity,
                          self$target$opacity, alpha)

      # Interpolate rotation
      mob$rotation <- lerp(self$starting_mobject$rotation,
                           self$target$rotation, alpha)

      # Interpolate scale
      mob$scale_factor <- lerp(self$starting_mobject$scale_factor,
                               self$target$scale_factor, alpha)

      # Interpolate color
      if (!is.null(self$starting_mobject$color) && !is.null(self$target$color)) {
        mob$color <- interpolate_color(self$starting_mobject$color,
                                       self$target$color, alpha)
      }

      # Interpolate VMobject-specific properties
      if (inherits(mob, "VMobject")) {
        # Interpolate points
        if (self$starting_mobject$has_points() && self$target$has_points()) {
          matched <- match_curves(self$starting_mobject$points,
                                  self$target$points)
          mob$points <- interpolate_matrices(matched[[1]], matched[[2]], alpha)
        }

        # Interpolate fill/stroke
        mob$fill_opacity <- lerp(
          self$starting_mobject$fill_opacity %||% 0,
          self$target$fill_opacity %||% 0,
          alpha
        )
        mob$stroke_opacity <- lerp(
          self$starting_mobject$stroke_opacity %||% 1,
          self$target$stroke_opacity %||% 1,
          alpha
        )
        mob$stroke_width <- lerp(
          self$starting_mobject$stroke_width %||% DEFAULT_STROKE_WIDTH,
          self$target$stroke_width %||% DEFAULT_STROKE_WIDTH,
          alpha
        )
        if (!is.null(self$starting_mobject$fill_color) &&
            !is.null(self$target$fill_color)) {
          mob$fill_color <- interpolate_color(
            self$starting_mobject$fill_color,
            self$target$fill_color,
            alpha
          )
        }
      }

      invisible(self)
    }
  )
)
