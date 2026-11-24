#' @title Transformation Pipe Functions
#' @description
#' Pipe-compatible transformation functions that wrap the Transform Animation
#' R6 class hierarchy. These change objects from one state to another.
#' @name transformations
NULL

#' Transform one object into another
#'
#' @param scene A Scene object
#' @param to Target LaTeX expression, text string, or MObject
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param path_arc Arc of the transformation path in radians
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("transform_demo") %>%
#'   add_latex("(a+b)^2") %>%
#'   write_in() %>%
#'   pause(1) %>%
#'   transform_to("a^2 + 2ab + b^2", duration = 2) %>%
#'   render()
#' }
#' @export
transform_to <- function(scene, to, duration = 1, easing = "smooth",
                         path_arc = 0) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  last_obj <- scene$get_last_object()
  if (is.null(last_obj))
    cli::cli_abort("No object to transform. Add an object first.")

  duration <- validate_duration(duration)

  # Build the target mobject from various input types
  target_obj <- NULL

  if (is.character(to) && inherits(last_obj, "LaTeXObject")) {
    target_obj <- LaTeXObject$new(
      latex = to, position = last_obj$position,
      size = last_obj$size, color = last_obj$color,
      opacity = last_obj$opacity
    )
  } else if (is.character(to) && inherits(last_obj, "TextObject")) {
    target_obj <- TextObject$new(
      text = to, position = last_obj$position,
      size = last_obj$size, color = last_obj$color,
      font = last_obj$font, weight = last_obj$weight,
      opacity = last_obj$opacity
    )
  } else if (inherits(to, "MObject")) {
    target_obj <- to
  } else {
    cli::cli_abort("Cannot transform to {.cls {class(to)[1]}}")
  }

  # Add target to scene
  scene$add_object(target_obj, show_immediately = FALSE)

  # Use R6 Transform animation
  anim <- Transform$new(last_obj, target_obj, path_arc = path_arc,
                        run_time = duration, rate_func = easing)
  scene$play(anim)

  invisible(scene)
}

#' Morph between objects
#'
#' @param scene A Scene object
#' @param to Target object or expression
#' @param duration Duration in seconds
#' @param easing Easing function
#' @return The Scene object (invisibly) for piping
#' @export
morph <- function(scene, to, duration = 1, easing = "smooth") {
  transform_to(scene, to, duration, easing, path_arc = 0)
}

#' Move an object to a new position
#'
#' @param scene A Scene object
#' @param position Target position as c(x, y) or c(x, y, z)
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param path Type of path ("linear", "arc")
#' @param target_object Optional ID of specific object to move
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("move_demo") %>%
#'   add_circle(radius = 0.5, position = c(-3, 0, 0)) %>%
#'   grow_from_center() %>%
#'   move_to(c(3, 0, 0), duration = 2) %>%
#'   render()
#' }
#' @export
move_to <- function(scene, position, duration = 1, easing = "smooth",
                    path = "linear", target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to move. Add an object first.")

  duration <- validate_duration(duration)
  target_pos <- validate_position(position)

  # Use ApplyMethod to animate move_to
  anim <- ApplyMethod$new(obj, "move_to", target_pos,
                          run_time = duration, rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Shift an object by a vector
#'
#' @param scene A Scene object
#' @param direction Direction vector as c(dx, dy) or c(dx, dy, dz)
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param target_object Optional ID of specific object to shift
#' @return The Scene object (invisibly) for piping
#' @export
shift <- function(scene, direction, duration = 1, easing = "smooth",
                  target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to shift. Add an object first.")

  duration <- validate_duration(duration)
  shift_vector <- validate_position(direction)

  anim <- ApplyMethod$new(obj, "shift", shift_vector,
                          run_time = duration, rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Rotate an object
#'
#' @param scene A Scene object
#' @param angle Rotation angle in radians (or degrees if degrees = TRUE)
#' @param about_point Point to rotate around
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param degrees If TRUE, interpret angle as degrees
#' @param target_object Optional ID of specific object to rotate
#' @return The Scene object (invisibly) for piping
#' @export
rotate <- function(scene, angle, about_point = NULL, duration = 1,
                   easing = "smooth", degrees = FALSE,
                   target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to rotate. Add an object first.")

  duration <- validate_duration(duration)
  if (degrees) angle <- angle * pi / 180

  anim <- RotateAnim$new(obj, angle = angle, about_point = about_point,
                         run_time = duration, rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Scale an object
#'
#' @param scene A Scene object
#' @param factor Scale factor
#' @param about_point Point to scale about
#' @param duration Duration in seconds
#' @param easing Easing function
#' @param target_object Optional ID of specific object to scale
#' @return The Scene object (invisibly) for piping
#' @export
scale_object <- function(scene, factor, about_point = NULL, duration = 1,
                         easing = "smooth", target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to scale. Add an object first.")

  duration <- validate_duration(duration)
  if (!is.numeric(factor) || factor <= 0)
    cli::cli_abort("{.arg factor} must be a positive number")

  anim <- ApplyMethod$new(obj, "scale", factor,
                          run_time = duration, rate_func = easing)
  scene$play(anim)
  invisible(scene)
}

#' Highlight a term in a LaTeX expression
#'
#' @param scene A Scene object
#' @param term The term to highlight
#' @param color Highlight color
#' @param duration Duration
#' @param target_object Optional ID of specific LaTeX object
#' @return The Scene object (invisibly) for piping
#' @export
highlight_term <- function(scene, term, color = "#FFFF00", duration = 0.5,
                           target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj_id <- target_object
  if (is.null(obj_id)) {
    last_obj <- scene$get_last_object()
    if (is.null(last_obj))
      cli::cli_abort("No object to highlight. Add an object first.")
    obj_id <- last_obj$id
  }

  duration <- validate_duration(duration)

  scene$add_animation(list(
    type = "highlight_term",
    object_id = obj_id,
    start_time = scene$current_time,
    duration = duration,
    term = term,
    color = validate_color(color)
  ))
  invisible(scene)
}

#' Change object color
#'
#' @param scene A Scene object
#' @param color Target color
#' @param duration Duration in seconds
#' @param target_object Optional ID of specific object
#' @return The Scene object (invisibly) for piping
#' @export
change_color <- function(scene, color, duration = 0.5,
                         target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to color. Add an object first.")

  duration <- validate_duration(duration)

  anim <- ApplyMethod$new(obj, "set_color", validate_color(color),
                          run_time = duration)
  scene$play(anim)
  invisible(scene)
}

#' Circumscribe an object
#'
#' @param scene A Scene object
#' @param shape Shape to draw ("rectangle", "circle")
#' @param color Outline color
#' @param stroke_width Line width
#' @param duration Duration
#' @param buff Padding around the object
#' @param fade_out Whether to fade out after drawing
#' @param target_object Optional ID of specific object
#' @return The Scene object (invisibly) for piping
#' @export
circumscribe <- function(scene, shape = "rectangle", color = "#FFFF00",
                         stroke_width = 4, duration = 1, buff = 0.1,
                         fade_out = TRUE, target_object = NULL) {
  if (!inherits(scene, "Scene"))
    cli::cli_abort("{.arg scene} must be a Scene object")

  obj <- NULL
  if (!is.null(target_object)) obj <- scene$get_object(target_object)
  if (is.null(obj)) obj <- scene$get_last_object()
  if (is.null(obj))
    cli::cli_abort("No object to circumscribe. Add an object first.")

  duration <- validate_duration(duration)

  anim <- Circumscribe$new(obj, shape = shape, color = color,
                            buff = buff, run_time = duration)
  scene$play(anim)
  invisible(scene)
}
