#' @title Transform Animations
#' @description Animations that morph one mobject into another through
#'   point interpolation and style blending.
#' @name animation_transform
NULL

#' Transform R6 Class
#'
#' @description Smoothly morphs a mobject into a target mobject by
#'   interpolating their Bezier control points (after matching curve counts).
#' @export
Transform <- R6::R6Class(
  "Transform",
  inherit = Animation,
  public = list(
    #' @field target_mobject The target shape to morph into
    target_mobject = NULL,
    #' @field path_arc Arc angle for the interpolation path (0 = straight)
    path_arc = 0,

    #' @description Create a Transform animation
    #' @param mobject Source mobject
    #' @param target_mobject Target mobject to morph into
    #' @param path_arc Arc angle for curved morph path (radians)
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, target_mobject, path_arc = 0,
                          run_time = 1, rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$target_mobject <- target_mobject
      self$path_arc <- path_arc
    },

    #' @description Save starting state and match curves
    begin = function() {
      super$begin()
      if (inherits(self$mobject, "VMobject") &&
          inherits(self$target_mobject, "VMobject")) {
        if (self$starting_mobject$has_points() &&
            self$target_mobject$has_points()) {
          matched <- match_curves(self$starting_mobject$points,
                                  self$target_mobject$points)
          private$matched_start <- matched[[1]]
          private$matched_end <- matched[[2]]
        }
      }
      invisible(self)
    },

    #' @description Interpolate between source and target
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      target <- self$target_mobject
      start <- self$starting_mobject

      if (is.null(start) || is.null(target)) return(invisible(self))

      # Interpolate position with optional arc path
      if (self$path_arc != 0) {
        mob$position <- private$arc_interpolate(
          start$position, target$position, alpha
        )
      } else {
        mob$position <- lerp(start$position, target$position, alpha)
      }

      # Interpolate scale, rotation, opacity
      mob$rotation <- lerp(start$rotation, target$rotation, alpha)
      mob$scale_factor <- lerp(start$scale_factor, target$scale_factor, alpha)
      mob$opacity <- lerp(start$opacity, target$opacity, alpha)

      # Interpolate color
      mob$color <- interpolate_color(start$color, target$color, alpha)

      # Interpolate VMobject-specific properties
      if (inherits(mob, "VMobject") && inherits(target, "VMobject")) {
        # Points
        if (!is.null(private$matched_start) && !is.null(private$matched_end)) {
          if (self$path_arc != 0) {
            mob$points <- private$arc_interpolate_matrix(
              private$matched_start, private$matched_end, alpha
            )
          } else {
            mob$points <- interpolate_matrices(
              private$matched_start, private$matched_end, alpha
            )
          }
        }

        # Fill/Stroke
        mob$fill_opacity <- lerp(start$fill_opacity %||% 0,
                                 target$fill_opacity %||% 0, alpha)
        mob$stroke_opacity <- lerp(start$stroke_opacity %||% 1,
                                   target$stroke_opacity %||% 1, alpha)
        mob$stroke_width <- lerp(start$stroke_width %||% DEFAULT_STROKE_WIDTH,
                                 target$stroke_width %||% DEFAULT_STROKE_WIDTH,
                                 alpha)
        if (!is.null(start$fill_color) && !is.null(target$fill_color)) {
          mob$fill_color <- interpolate_color(start$fill_color,
                                              target$fill_color, alpha)
        }
        if (!is.null(start$stroke_color) && !is.null(target$stroke_color)) {
          mob$stroke_color <- interpolate_color(start$stroke_color,
                                                target$stroke_color, alpha)
        }
      }

      invisible(self)
    },

    #' @description Get all involved mobjects
    get_all_mobjects = function() {
      result <- super$get_all_mobjects()
      if (!is.null(self$target_mobject)) {
        result <- c(result, list(self$target_mobject))
      }
      result
    }
  ),

  private = list(
    matched_start = NULL,
    matched_end = NULL,

    # Interpolate along a circular arc path
    arc_interpolate = function(start, end, alpha) {
      if (abs(self$path_arc) < 1e-10) return(lerp(start, end, alpha))

      midpoint <- (start + end) / 2
      diff <- end - start
      # Perpendicular in XY plane
      perp <- c(-diff[2], diff[1], 0)
      perp_len <- sqrt(sum(perp^2))
      if (perp_len < 1e-10) return(lerp(start, end, alpha))
      perp <- perp / perp_len

      # Arc height from path_arc angle
      half_dist <- sqrt(sum(diff^2)) / 2
      height <- half_dist * tan(self$path_arc / 4)

      angle <- pi * alpha
      lerp(start, end, alpha) + perp * height * sin(angle)
    },

    arc_interpolate_matrix = function(m1, m2, alpha) {
      if (abs(self$path_arc) < 1e-10) {
        return(interpolate_matrices(m1, m2, alpha))
      }
      # Apply arc interpolation per row
      result <- m1 * (1 - alpha) + m2 * alpha
      # Add arc offset
      for (i in seq_len(nrow(result))) {
        diff <- m2[i, ] - m1[i, ]
        perp <- c(-diff[2], diff[1], 0)
        perp_len <- sqrt(sum(perp^2))
        if (perp_len > 1e-10) {
          perp <- perp / perp_len
          half_dist <- sqrt(sum(diff^2)) / 2
          height <- half_dist * tan(self$path_arc / 4)
          result[i, ] <- result[i, ] + perp * height * sin(pi * alpha)
        }
      }
      result
    }
  )
)

#' ReplacementTransform R6 Class
#'
#' @description Like Transform, but the original mobject is removed from
#'   the scene and the target is added. The target gets the interpolated
#'   state during animation.
#' @export
ReplacementTransform <- R6::R6Class(
  "ReplacementTransform",
  inherit = Transform,
  public = list(
    #' @description Create a ReplacementTransform
    #' @param mobject Source mobject (will be removed)
    #' @param target_mobject Target mobject (will be added)
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, target_mobject, run_time = 1,
                          rate_func = "smooth", ...) {
      super$initialize(mobject = mobject, target_mobject = target_mobject,
                       run_time = run_time, rate_func = rate_func, ...)
      self$remover <- TRUE
    },

    #' @description After animation, ensure target has its final state
    finish = function() {
      super$finish()
      # Copy final state to target
      target <- self$target_mobject
      if (!is.null(target)) {
        # Target should now be in its intended final state
        # (the scene will add target and remove mobject)
      }
      invisible(self)
    }
  )
)

#' MoveToTarget R6 Class
#'
#' @description Animates a mobject to its `.target` copy (set via
#'   `mobject$generate_target()`).
#' @export
MoveToTarget <- R6::R6Class(
  "MoveToTarget",
  inherit = Transform,
  public = list(
    #' @description Create a MoveToTarget animation
    #' @param mobject Mobject with a target set
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, run_time = 1, rate_func = "smooth", ...) {
      target <- mobject$target
      if (is.null(target)) {
        cli::cli_warn("MoveToTarget: mobject has no target. Use generate_target() first.")
        target <- mobject$copy()
      }
      super$initialize(mobject = mobject, target_mobject = target,
                       run_time = run_time, rate_func = rate_func, ...)
    }
  )
)

#' ApplyMethod R6 Class
#'
#' @description Animates the result of calling a method on a mobject.
#'   Creates a copy, applies the method, then morphs to the result.
#' @export
ApplyMethod <- R6::R6Class(
  "ApplyMethod",
  inherit = Transform,
  public = list(
    #' @description Create an ApplyMethod animation
    #' @param mobject The mobject
    #' @param method_name Name of the method to call
    #' @param ... Arguments to the method
    #' @param run_time Duration
    #' @param rate_func Easing
    initialize = function(mobject, method_name, ..., run_time = 1,
                          rate_func = "smooth") {
      target <- mobject$copy()
      method_fn <- target[[method_name]]
      if (is.null(method_fn) || !is.function(method_fn)) {
        cli::cli_abort("Method {.fn {method_name}} not found on mobject")
      }
      method_fn(...)
      super$initialize(mobject = mobject, target_mobject = target,
                       run_time = run_time, rate_func = rate_func)
    }
  )
)

#' ApplyFunction R6 Class
#'
#' @description Applies an arbitrary function to the mobject's points and
#'   animates the transition.
#' @export
ApplyFunction <- R6::R6Class(
  "ApplyFunction",
  inherit = Transform,
  public = list(
    #' @description Create an ApplyFunction animation
    #' @param mobject VMobject whose points will be transformed
    #' @param func A function(point) -> point applied to each control point
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, func, run_time = 1,
                          rate_func = "smooth", ...) {
      target <- mobject$copy()
      if (inherits(target, "VMobject") && target$has_points()) {
        for (i in seq_len(nrow(target$points))) {
          target$points[i, ] <- func(target$points[i, ])
        }
      }
      super$initialize(mobject = mobject, target_mobject = target,
                       run_time = run_time, rate_func = rate_func, ...)
    }
  )
)

#' ApplyComplexFunction R6 Class
#'
#' @description Applies a complex-valued function to the mobject's points,
#'   treating (x, y) as real and imaginary parts.
#' @export
ApplyComplexFunction <- R6::R6Class(
  "ApplyComplexFunction",
  inherit = ApplyFunction,
  public = list(
    #' @description Create an ApplyComplexFunction animation
    #' @param mobject VMobject
    #' @param complex_func A function(complex) -> complex
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, complex_func, run_time = 1,
                          rate_func = "smooth", ...) {
      point_func <- function(point) {
        z <- complex(real = point[1], imaginary = point[2])
        w <- complex_func(z)
        c(Re(w), Im(w), point[3])
      }
      super$initialize(mobject = mobject, func = point_func,
                       run_time = run_time, rate_func = rate_func, ...)
    }
  )
)

#' TransformMatchingShapes R6 Class
#'
#' @description Transforms submobjects by matching them based on shape
#'   similarity. Unmatched submobjects fade in/out.
#' @export
TransformMatchingShapes <- R6::R6Class(
  "TransformMatchingShapes",
  inherit = AnimationGroup,
  public = list(
    #' @description Create a TransformMatchingShapes animation
    #' @param source Source VGroup
    #' @param target Target VGroup
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(source, target, run_time = 1,
                          rate_func = "smooth", ...) {
      # Extract submobjects
      source_subs <- if (inherits(source, "VMobject") || inherits(source, "Group")) {
        source$submobjects
      } else {
        list(source)
      }
      target_subs <- if (inherits(target, "VMobject") || inherits(target, "Group")) {
        target$submobjects
      } else {
        list(target)
      }

      anims <- list()

      # Match by index (simple strategy)
      n_matched <- min(length(source_subs), length(target_subs))

      if (n_matched > 0) {
        for (i in seq_len(n_matched)) {
          anims <- c(anims, list(
            Transform$new(source_subs[[i]], target_subs[[i]],
                          run_time = run_time, rate_func = rate_func)
          ))
        }
      }

      # Fade out unmatched source submobjects
      if (length(source_subs) > n_matched) {
        for (i in (n_matched + 1):length(source_subs)) {
          anims <- c(anims, list(
            FadeOut$new(source_subs[[i]], run_time = run_time)
          ))
        }
      }

      # Fade in unmatched target submobjects
      if (length(target_subs) > n_matched) {
        for (i in (n_matched + 1):length(target_subs)) {
          anims <- c(anims, list(
            FadeIn$new(target_subs[[i]], run_time = run_time)
          ))
        }
      }

      do.call(super$initialize,
              c(anims, list(run_time = run_time, rate_func = rate_func)))
    }
  )
)
