#' @title Animation Pipe Functions
#' @description
#' Pipe-compatible animation functions that wrap the Animation R6 class
#' hierarchy. These functions operate on the last-added object in a Scene
#' and provide the idiomatic R pipe API.
#' @name animations
NULL

#' Animate an object (generic)
#'
#' @param scene A Scene object
#' @param animation_type Type of animation
#' @param duration Duration in seconds
#' @param easing Easing function name
#' @param ... Additional animation parameters
#' @return The Scene object (invisibly) for piping
#' @export
animate <- function(scene, animation_type = "custom", duration = 1,
                    easing = "smooth", ...) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)
  scene$add_animation(list(
    type = animation_type,
    object_id = last_obj$id,
    start_time = scene$current_time,
    duration = duration,
    easing = easing,
    params = list(...)
  ))
  invisible(scene)
}

#' Write-in animation (typing effect)
#'
#' @description Animates text or shapes as if being written/drawn.
#'   Wraps the \code{Create} / \code{Write} Animation classes.
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param lag_ratio Stagger between characters (0-1)
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("write_demo") %>%
#'   add_latex("f(x) = x^2") %>%
#'   write_in(duration = 2) %>%
#'   render()
#' }
#' @export
write_in <- function(scene, duration = 1, easing = "smooth",
                     lag_ratio = 0.05) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)
  last_obj$visible <- TRUE

  # Use R6 Animation internally
  if (inherits(last_obj, "VMobject")) {
    anim <- Create$new(last_obj, run_time = duration, rate_func = easing)
    scene$play(anim)
  } else {
    scene$add_animation(list(
      type = "write_in",
      object_id = last_obj$id,
      start_time = scene$current_time,
      duration = duration,
      easing = easing,
      lag_ratio = lag_ratio
    ))
  }
  invisible(scene)
}

#' Fade-in animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param direction Direction to fade from ("up", "down", "left", "right")
#' @param shift Amount to shift during fade
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("fade_demo") %>%
#'   add_text("Hello!") %>%
#'   fade_in(duration = 1) %>%
#'   render()
#' }
#' @export
fade_in <- function(scene, duration = 1, easing = "smooth",
                    direction = NULL, shift = 0.5) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)
  last_obj$visible <- TRUE

  # Compute shift vector from direction
  shift_vector <- NULL
  if (!is.null(direction)) {
    direction <- match.arg(direction, c("up", "down", "left", "right"))
    shift_vector <- switch(direction,
      up = DOWN * shift,
      down = UP * shift,
      left = RIGHT * shift,
      right = LEFT * shift
    )
  }

  anim <- FadeIn$new(last_obj, shift = shift_vector, run_time = duration,
                     rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Fade-out animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param direction Direction to fade towards
#' @param shift Amount to shift during fade
#' @param target_object Optional ID of specific object to fade out
#' @return The Scene object (invisibly) for piping
#' @export
fade_out <- function(scene, duration = 1, easing = "smooth",
                     direction = NULL, shift = 0.5,
                     target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) {
    obj <- scene$get_object(target_object)
  }
  if (is.null(obj)) {
    obj <- scene$get_last_object()
  }
  if (is.null(obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)

  shift_vector <- NULL
  if (!is.null(direction)) {
    direction <- match.arg(direction, c("up", "down", "left", "right"))
    shift_vector <- switch(direction,
      up = UP * shift,
      down = DOWN * shift,
      left = LEFT * shift,
      right = RIGHT * shift
    )
  }

  anim <- FadeOut$new(obj, shift = shift_vector, run_time = duration,
                      rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Grow from center animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param point_color Color of the initial point
#' @return The Scene object (invisibly) for piping
#' @export
grow_from_center <- function(scene, duration = 1, easing = "smooth",
                             point_color = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)
  last_obj$visible <- TRUE

  anim <- GrowFromCenter$new(last_obj, run_time = duration, rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Shrink to center animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param target_object Optional ID of specific object
#' @return The Scene object (invisibly) for piping
#' @export
shrink_to_center <- function(scene, duration = 1, easing = "smooth",
                             target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)

  # Shrink is FadeOut with scale
  anim <- FadeOut$new(obj, scale = 0.01, run_time = duration,
                      rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Draw border animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param rate How much of the border to draw (0-1)
#' @return The Scene object (invisibly) for piping
#' @export
draw_border <- function(scene, duration = 1, easing = "smooth", rate = 1) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)
  last_obj$visible <- TRUE

  anim <- DrawBorderThenFill$new(last_obj, run_time = duration,
                                  rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Flash animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param color Flash color
#' @param flash_radius How far the flash extends
#' @return The Scene object (invisibly) for piping
#' @export
flash <- function(scene, duration = 0.5, color = "#FFFFFF",
                  flash_radius = 0.5) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)

  anim <- Flash$new(last_obj$get_center(), color = color,
                    flash_radius = flash_radius, run_time = duration)
  scene$play(anim)
  invisible(scene)
}

#' Indicate/highlight animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param color Highlight color
#' @param scale_factor How much to scale during highlight
#' @return The Scene object (invisibly) for piping
#' @export
indicate <- function(scene, duration = 0.5, color = "#FFFF00",
                     scale_factor = 1.2) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)

  anim <- Indicate$new(last_obj, color = color, scale_factor = scale_factor,
                       run_time = duration)
  scene$play(anim)
  invisible(scene)
}

#' Pulse animation
#'
#' @param scene A Scene object
#' @param duration Duration of one pulse cycle
#' @param scale_factor Maximum scale during pulse
#' @param n_pulses Number of pulse cycles
#' @return The Scene object (invisibly) for piping
#' @export
pulse <- function(scene, duration = 0.5, scale_factor = 1.1, n_pulses = 2) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)

  anim <- Wiggle$new(last_obj, scale_value = scale_factor,
                     rotation_angle = 0, n_wiggles = n_pulses,
                     run_time = duration * n_pulses)
  scene$play(anim)
  invisible(scene)
}

#' Wiggle animation
#'
#' @param scene A Scene object
#' @param duration Duration in seconds
#' @param amplitude Wiggle amplitude
#' @param n_wiggles Number of wiggles
#' @return The Scene object (invisibly) for piping
#' @export
wiggle <- function(scene, duration = 0.5, amplitude = 0.1, n_wiggles = 4) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to animate. Add an object first.")

  duration <- validate_duration(duration)

  anim <- Wiggle$new(last_obj, rotation_angle = amplitude,
                     n_wiggles = n_wiggles, run_time = duration)
  scene$play(anim)
  invisible(scene)
}
