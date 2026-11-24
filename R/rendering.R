#' @title Rendering Functions
#' @description
#' Functions for rendering scenes to video files or previewing animations.
#' @name rendering
NULL

#' Render a scene to video
#'
#' @description
#' Renders the scene to a video file. Supports multiple output formats
#' and quality settings.
#'
#' @param scene A Scene object
#' @param output Output filename (without extension, or with extension to specify format)
#' @param format Output format ("mp4", "gif", "webm", "png_sequence")
#' @param quality Quality preset ("low", "medium", "high", "4K")
#' @param fps Frames per second (overrides scene fps if specified)
#' @param preview Open preview after rendering
#' @param open Open file after rendering
#' @param background_color Override background color
#' @param transparent Render with transparent background (for gif/webm)
#' @param progress Show progress bar
#' @param cache Use cached frames if available
#'
#' @return Invisibly returns the path to the output file
#'
#' @examples
#' \dontrun{
#' scene("demo") %>%
#'   add_text("Hello") %>%
#'   write_in() %>%
#'   render(output = "hello_animation", format = "mp4")
#'
#' # Render high-quality GIF
#' scene("demo") %>%
#'   add_circle() %>%
#'   grow_from_center() %>%
#'   render(format = "gif", quality = "high")
#' }
#'
#' @export
render <- function(scene,
                   output = NULL,
                   format = "mp4",
                   quality = "medium",
                   fps = NULL,
                   preview = FALSE,
                   open = FALSE,
                   background_color = NULL,
                   transparent = FALSE,
                   progress = TRUE,
                   cache = TRUE) {

  # If a Template is passed, build it into a Scene first.
  # Also handle R6 objects that have a $build() method returning a Scene
  # (duck-typing for subclasses or alternative template implementations).
  if (inherits(scene, "Template")) {
    scene <- scene$build()
  } else if (inherits(scene, "R6") && is.function(scene$build) &&
             !inherits(scene, "Scene")) {
    built <- tryCatch(scene$build(), error = function(e) NULL)
    if (inherits(built, "Scene")) scene <- built
  }

  if (!inherits(scene, "Scene")) {
    cli::cli_abort(c(
      "{.arg scene} must be a Scene object",
      "i" = "Got {.cls {class(scene)}}",
      "i" = "Did you forget to pipe through {.fn render} directly after building the scene?"
    ))
  }

  # Validate format
  format <- match.arg(format, c("mp4", "gif", "webm", "png_sequence"))

  # Validate quality and get dimensions
  quality_settings <- get_quality_settings(quality)

  # Set output filename
  if (is.null(output)) {
    output <- scene$name
  }

  # Add extension if not present
  if (!grepl("\\.[a-z0-9]+$", output, ignore.case = TRUE)) {
    ext <- if (format == "png_sequence") "" else paste0(".", format)
    output <- paste0(output, ext)
  }

  # Get absolute path
  output_path <- normalizePath(output, mustWork = FALSE)
  output_dir <- dirname(output_path)

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Use scene fps or override
  render_fps <- fps %||% scene$fps

  # Background color
  bg_color <- background_color %||% scene$background_color

  # Calculate frame count
  duration <- scene$get_duration()
  frame_count <- ceiling(duration * render_fps)

  if (frame_count == 0) {
    cli::cli_warn("Scene has no duration. Add animations before rendering.")
    return(invisible(NULL))
  }

  if (manimR_options("verbose")) {
    cli::cli_h2("Rendering: {scene$name}")
    cli::cli_alert_info("Duration: {round(duration, 2)}s @ {render_fps}fps = {frame_count} frames")
    cli::cli_alert_info("Quality: {quality} ({quality_settings$width}x{quality_settings$height})")
    cli::cli_alert_info("Output: {.path {output_path}}")
  }

  # Create temp directory here so we can clean it up AFTER encoding
  temp_dir <- tempfile(pattern = "manimr_frames_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Render frames
  frames <- render_frames(
    scene = scene,
    frame_count = frame_count,
    fps = render_fps,
    width = quality_settings$width,
    height = quality_settings$height,
    background_color = bg_color,
    transparent = transparent,
    progress = progress,
    cache = cache,
    temp_dir = temp_dir
  )

  # Encode video (frames still exist in temp_dir at this point)
  encode_output(
    frames = frames,
    output_path = output_path,
    format = format,
    fps = render_fps,
    quality = quality,
    progress = progress
  )
  # on.exit above deletes temp_dir after this function returns

  if (manimR_options("verbose")) {
    cli::cli_alert_success("Rendered to {.path {output_path}}")
  }

  # Open/preview if requested
  if (preview || open) {
    if (interactive()) {
      utils::browseURL(output_path)
    }
  }

  invisible(output_path)
}

#' Preview a scene
#'
#' @description
#' Quickly renders and displays a low-quality preview of the scene.
#' Useful for iterating on animations without waiting for full renders.
#'
#' @param scene A Scene object
#' @param fps Preview fps (default: 15 for speed)
#' @param quality Preview quality (default: "low")
#'
#' @return Invisibly returns the preview file path
#'
#' @examples
#' \dontrun{
#' scene("demo") %>%
#'   add_text("Quick preview") %>%
#'   write_in() %>%
#'   preview()
#' }
#'
#' @export
preview <- function(scene, fps = 15, quality = "low") {
  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  # Create temp file for preview
  temp_file <- tempfile(fileext = ".gif")

  render(
    scene = scene,
    output = temp_file,
    format = "gif",
    quality = quality,
    fps = fps,
    preview = TRUE,
    progress = FALSE,
    cache = FALSE
  )

  invisible(temp_file)
}

#' Get quality settings
#'
#' @description
#' Returns width, height, and bitrate settings for a quality preset.
#'
#' @param quality Quality preset name
#' @return List with width, height, bitrate
#' @keywords internal
#' @noRd
get_quality_settings <- function(quality) {
  quality <- match.arg(quality, c("low", "medium", "high", "4K", "preview"))

  settings <- list(
    preview = list(width = 640, height = 360, bitrate = "1M"),
    low = list(width = 854, height = 480, bitrate = "2M"),
    medium = list(width = 1280, height = 720, bitrate = "4M"),
    high = list(width = 1920, height = 1080, bitrate = "8M"),
    `4K` = list(width = 3840, height = 2160, bitrate = "20M")
  )

  settings[[quality]]
}

#' Render individual frames
#'
#' @description
#' Renders all frames of the animation.
#'
#' @param scene Scene object
#' @param frame_count Number of frames
#' @param fps Frames per second
#' @param width Frame width
#' @param height Frame height
#' @param background_color Background color
#' @param transparent Transparent background
#' @param progress Show progress
#' @param cache Use caching
#' @return List of frame file paths
#' @keywords internal
#' @noRd
render_frames <- function(scene,
                          frame_count,
                          fps,
                          width,
                          height,
                          background_color,
                          transparent,
                          progress,
                          cache,
                          temp_dir = NULL) {

  # Use provided temp_dir (from render()) or create a local one.
  # When temp_dir comes from render(), cleanup happens there after encoding.
  own_temp_dir <- is.null(temp_dir)
  if (own_temp_dir) {
    temp_dir <- tempfile(pattern = "manimr_frames_")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  }

  frame_paths <- character(frame_count)

  # Check if we can use batch rendering via compiled backend
  backend <- get_backend()
  can_use_batch <- backend %in% c("cpp", "rust") && length(scene$timeline) > 0

  if (can_use_batch && requireNamespace("Rcpp", quietly = TRUE)) {
    # Use accelerated batch rendering
    frame_times <- seq(0, by = 1/fps, length.out = frame_count)

    if (manimR_options("verbose")) {
      cli::cli_alert_info("Using {backend} backend for frame rendering")
    }

    return(render_frames_batch_accelerated(
      scene, frame_times, fps, width, height, background_color,
      transparent, progress, temp_dir, frame_paths
    ))
  }

  # Fall back to sequential rendering
  render_frames_sequential(
    scene, frame_count, fps, width, height, background_color,
    transparent, progress, temp_dir, frame_paths
  )
}

#' Render frames using accelerated backend
#' @keywords internal
render_frames_batch_accelerated <- function(scene, frame_times, fps, width, height,
                                            background_color, transparent, progress,
                                            temp_dir, frame_paths) {

  # Convert scene data for C++ backend
  object_data <- prepare_object_data_for_backend(scene)
  animation_data <- prepare_animation_data_for_backend(scene)

  # Progress setup
  if (progress && manimR_options("verbose")) {
    pb <- cli::cli_progress_bar(
      "Rendering frames",
      total = length(frame_times),
      format = "{cli::pb_spin} Computing frame states {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
    )
  }

  # Use backend dispatch for frame state computation
  # The C++ backend only computes dynamic animation properties (position, opacity,
  # scale, rotation, color). Static object data (points, image_path, stroke/fill
  # properties) must be merged back in from the original scene objects.
  frame_states <- dispatch_render_frames(
    object_data = object_data,
    animation_data = animation_data,
    frame_times = frame_times,
    width = width,
    height = height,
    background_color = background_color
  )

  # Build a lookup of static object data from the scene
  static_data <- list()
  for (obj in scene$objects) {
    obj_static <- list()
    # VMobject data (points, stroke, fill) - needed for LaTeX and shapes
    if (inherits(obj, "VMobject")) {
      obj_static$points <- obj$points
      obj_static$stroke_width <- obj$stroke_width
      obj_static$stroke_color <- obj$stroke_color
      obj_static$stroke_opacity <- obj$stroke_opacity
      obj_static$fill_color <- obj$fill_color
      obj_static$fill_opacity <- obj$fill_opacity
    }
    # LaTeX-specific
    if (inherits(obj, "LaTeXObject")) {
      obj_static$image_path <- obj$image_path
      obj_static$data <- list(path = obj$image_path)
    }
    static_data[[obj$id]] <- obj_static
  }

  # Merge static data into each frame's object states
  frame_states <- lapply(frame_states, function(frame) {
    lapply(frame, function(obj_state) {
      obj_id <- obj_state$id
      if (!is.null(obj_id) && obj_id %in% names(static_data)) {
        # Merge static fields that the C++ backend doesn't return
        for (key in names(static_data[[obj_id]])) {
          if (is.null(obj_state[[key]])) {
            obj_state[[key]] <- static_data[[obj_id]][[key]]
          }
        }
      }
      obj_state
    })
  })

  if (progress && manimR_options("verbose")) {
    cli::cli_progress_done(id = pb)
    pb <- cli::cli_progress_bar(
      "Drawing frames",
      total = length(frame_times),
      format = "{cli::pb_spin} Drawing frame {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
    )
  }

  # Draw each frame — run scene updaters per frame so cameras (e.g. ambient
  # rotation in ThreeDScene) are correctly advanced before drawing.
  dt <- if (length(frame_times) > 1) diff(frame_times)[1] else 1 / fps
  for (i in seq_along(frame_times)) {
    frame_path <- file.path(temp_dir, sprintf("frame_%05d.png", i))

    # Advance scene updaters (e.g. ambient camera rotation)
    if (is.function(scene$update)) {
      scene$update(dt = dt)
    }

    draw_frame_from_state(
      frame_states[[i]],
      output_path = frame_path,
      width = width,
      height = height,
      background_color = background_color,
      transparent = transparent,
      camera = scene$camera
    )

    frame_paths[i] <- frame_path

    if (progress && manimR_options("verbose")) {
      cli::cli_progress_update(id = pb)
    }
  }

  if (progress && manimR_options("verbose")) {
    cli::cli_progress_done(id = pb)
  }

  frame_paths
}

#' Render frames sequentially (fallback)
#' @keywords internal
render_frames_sequential <- function(scene, frame_count, fps, width, height,
                                     background_color, transparent, progress,
                                     temp_dir, frame_paths) {

  # Progress setup
  if (progress && manimR_options("verbose")) {
    pb <- cli::cli_progress_bar(
      "Rendering frames",
      total = frame_count,
      format = "{cli::pb_spin} Rendering frame {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
    )
  }

  for (i in seq_len(frame_count)) {
    frame_time <- (i - 1) / fps
    frame_path <- file.path(temp_dir, sprintf("frame_%05d.png", i))

    # Tick updaters so mobject-level update functions run each frame
    if (is.function(scene$update)) {
      scene$update(dt = 1 / fps)
    }

    render_single_frame(
      scene = scene,
      time = frame_time,
      output_path = frame_path,
      width = width,
      height = height,
      background_color = background_color,
      transparent = transparent
    )

    frame_paths[i] <- frame_path

    if (progress && manimR_options("verbose")) {
      cli::cli_progress_update(id = pb)
    }
  }

  if (progress && manimR_options("verbose")) {
    cli::cli_progress_done(id = pb)
  }

  frame_paths
}

#' Prepare object data for backend processing
#' @keywords internal
prepare_object_data_for_backend <- function(scene) {
  lapply(scene$objects, function(obj) {
    result <- list(
      id = obj$id,
      type = obj$type,
      position = obj$position,
      rotation = obj$rotation,
      scale = obj$scale_factor,
      color = obj$color,
      opacity = obj$opacity
    )

    if (inherits(obj, "TextObject")) {
      result$text <- obj$text
      result$font_size <- obj$size
    } else if (inherits(obj, "LaTeXObject")) {
      result$latex <- obj$latex
      result$size <- obj$size
    }

    # VMobject properties (covers all new geometry classes)
    if (inherits(obj, "VMobject")) {
      result$points <- obj$points
      result$stroke_width <- obj$stroke_width
      result$stroke_color <- obj$stroke_color
      result$stroke_opacity <- obj$stroke_opacity
      result$fill_color <- obj$fill_color
      result$fill_opacity <- obj$fill_opacity
    }

    # Legacy ShapeObject support
    if (inherits(obj, "ShapeObject")) {
      result$shape_type <- obj$shape_type
      result$radius <- obj$radius
      result$width <- obj$width
      result$height <- obj$height
      result$stroke_width <- obj$stroke_width
    }

    result
  })
}

#' Prepare animation data for backend processing
#' @keywords internal
prepare_animation_data_for_backend <- function(scene) {
  # Get the last object ID as default for animations that don't specify one
  last_object_id <- if (length(scene$objects) > 0) {
    scene$objects[[length(scene$objects)]]$id
  } else {
    ""
  }

  # Map R6 Animation type names to C++ backend names
  type_map <- c(
    "create" = "write_in",
    "write" = "write_in",
    "fadein" = "fade_in",
    "fadeout" = "fade_out",
    "growfromcenter" = "grow_from_center",
    "shrinktocenter" = "shrink_to_center",
    "transform" = "move_to",
    "replacementtransform" = "move_to",
    "highlight_term" = "change_color"
  )

  lapply(scene$timeline, function(anim) {
    # Skip pause/wait animations — they have no object_id and the C++ backend
    # doesn't need them (they just add time gaps)
    if (anim$type %in% c("pause", "wait")) {
      return(NULL)
    }

    # Resolve object_id: R6 animations use "target", legacy uses "object_id"
    obj_id <- anim$object_id %||% anim$target %||% last_object_id

    # Skip animations with no resolvable object ID
    if (is.null(obj_id) || identical(obj_id, "")) {
      return(NULL)
    }

    # Normalize animation type
    anim_type <- anim$type
    if (anim_type %in% names(type_map)) {
      anim_type <- type_map[[anim_type]]
    }

    result <- list(
      type = anim_type,
      object_id = obj_id,
      start_time = anim$start_time,
      duration = anim$duration,
      easing = anim$easing %||% "smooth"
    )
    # Only add optional fields if they exist and are not NULL
    if (!is.null(anim$target_position)) result$target_position <- anim$target_position
    if (!is.null(anim$shift_vector)) result$shift_vector <- anim$shift_vector
    if (!is.null(anim$factor)) result$factor <- anim$factor
    if (!is.null(anim$angle)) result$angle <- anim$angle
    if (!is.null(anim$color)) result$color <- anim$color
    result
  }) |> Filter(f = Negate(is.null))
}

#' Draw a frame from pre-computed state
#' @keywords internal
#' @noRd
draw_frame_from_state <- function(object_states, output_path, width, height,
                                  background_color, transparent,
                                  camera = NULL) {
  renderer <- Renderer$new(
    width = width,
    height = height,
    background_color = background_color,
    camera = camera
  )

  renderer$begin_frame(output_path)
  on.exit(renderer$end_frame(), add = TRUE)

  for (state in object_states) {
    if (isTRUE(state$visible) && state$opacity > 0) {
      renderer$draw_state(state)
    }
  }
}

# Legacy drawing functions have been moved to Renderer class
# get_object_states_at_time and apply_animation_to_state are still used by Renderer/helpers

#' Render a single frame
#'
#' @description
#' Renders a single frame at a specific time.
#'
#' @param scene Scene object
#' @param time Time in seconds
#' @param output_path Output file path
#' @param width Frame width
#' @param height Frame height
#' @param background_color Background color
#' @param transparent Transparent background
#' @keywords internal
#' @noRd
render_single_frame <- function(scene,
                                time,
                                output_path,
                                width,
                                height,
                                background_color,
                                transparent) {

  renderer <- Renderer$new(
    width = width,
    height = height,
    background_color = background_color,
    camera = scene$camera
  )

  renderer$render_frame(scene, time, output_path)
}

#' Get object states at a specific time
#'
#' @description
#' Calculates the state of all objects at a given time, accounting
#' for all animations.
#'
#' @param scene Scene object
#' @param time Time in seconds
#' @return List of object states
#' @keywords internal
#' @noRd
get_object_states_at_time <- function(scene, time) {
  states <- list()

  for (obj_id in names(scene$objects)) {
    obj <- scene$objects[[obj_id]]

    # Start with base object state
    state <- list(
      id = obj$id,
      type = obj$type,
      position = obj$position,
      rotation = obj$rotation,
      scale_factor = obj$scale_factor,
      color = obj$color,
      opacity = 0,
      visible = FALSE,
      z_index = obj$z_index %||% 0,
      data = if (!is.null(obj$data)) obj$data else list()
    )

    # Copy type-specific properties
    if (inherits(obj, "TextObject")) {
      state$text <- obj$text
      state$font <- obj$font
      state$size <- obj$size
      state$weight <- obj$weight
    } else if (inherits(obj, "LaTeXObject")) {
      state$latex <- obj$latex
      state$size <- obj$size
      state$points <- obj$points
      state$stroke_width <- obj$stroke_width
      state$fill_color <- obj$fill_color
    }

    # VMobject properties (covers Arc, Circle, Line, Polygon, etc.)
    if (inherits(obj, "VMobject")) {
      state$points <- obj$points
      state$stroke_width <- obj$stroke_width
      state$stroke_color <- obj$stroke_color
      state$stroke_opacity <- obj$stroke_opacity
      state$fill_color <- obj$fill_color
      state$fill_opacity <- obj$fill_opacity
      if (is.null(state$points)) state$points <- matrix(numeric(0), ncol = 3)
    }

    # Legacy ShapeObject support
    if (inherits(obj, "ShapeObject")) {
      state$shape_type <- obj$shape_type
      state$width <- obj$width
      state$height <- obj$height
      state$radius <- obj$radius
      state$fill_color <- obj$fill_color
      state$fill_opacity <- obj$fill_opacity
      state$stroke_width <- obj$stroke_width
    }

    # Apply animations from timeline
    for (anim in scene$timeline) {
      # Handle R6 Animation objects stored in timeline
      if (!is.null(anim$animation_object) &&
          inherits(anim$animation_object, "Animation")) {
        anim_obj <- anim$animation_object
        target_id <- anim$target %||% anim$object_id
        if (!is.null(target_id) && target_id == obj_id) {
          start_time <- anim$start_time %||% 0
          duration <- anim$duration %||% 1
          end_time <- start_time + duration

          if (time >= start_time) {
            state$visible <- TRUE
            progress <- if (time >= end_time) 1
                        else (time - start_time) / duration
            progress <- clamp(progress, 0, 1)

            # Let the Animation R6 object handle interpolation
            # Call begin() once when the animation first starts (not just at exact progress==0)
            if (is.null(anim_obj$starting_mobject)) {
              anim_obj$begin()
            }
            anim_obj$interpolate(progress)

            # Copy updated state from the mobject
            mob <- anim_obj$mobject
            if (!is.null(mob)) {
              state$position <- mob$position
              state$rotation <- mob$rotation
              state$scale_factor <- mob$scale_factor
              state$color <- mob$color
              state$opacity <- mob$opacity
              if (inherits(mob, "VMobject")) {
                state$points <- mob$points
                state$fill_color <- mob$fill_color
                state$fill_opacity <- mob$fill_opacity
                state$stroke_color <- mob$stroke_color
                state$stroke_opacity <- mob$stroke_opacity
                state$stroke_width <- mob$stroke_width
              }
            }

            if (progress >= 1 && anim_obj$is_remover()) {
              state$visible <- FALSE
              state$opacity <- 0
            }
          }
        }
      } else {
        # Legacy list-based animation specs
        if (!is.null(anim$object_id) && anim$object_id == obj_id) {
          state <- apply_animation_to_state(state, anim, time, scene)
        }
      }
    }

    # Check deferred removals: hide objects past their removal time
    if (!is.null(scene$pending_removals) &&
        obj_id %in% names(scene$pending_removals)) {
      removal_time <- scene$pending_removals[[obj_id]]
      if (time >= removal_time) {
        state$visible <- FALSE
        state$opacity <- 0
      }
    }

    states[[obj_id]] <- state
  }

  states
}

#' Apply an animation to an object state
#'
#' @param state Object state
#' @param anim Animation specification
#' @param time Current time
#' @param scene Scene object (needed for transform lookups)
#' @return Modified state
#' @keywords internal
#' @noRd
apply_animation_to_state <- function(state, anim, time, scene = NULL) {
  start_time <- anim$start_time %||% 0
  duration <- anim$duration %||% 1
  end_time <- start_time + duration

  # Not yet started
  if (time < start_time) {
    return(state)
  }

  # Calculate progress (0 to 1)
  if (time >= end_time) {
    progress <- 1
  } else {
    progress <- (time - start_time) / duration
  }

  # Apply easing
  easing_fn <- get_easing(anim$easing %||% "smooth")
  progress <- easing_fn(progress)

  # Apply animation based on type
  switch(anim$type,
    "show" = {
      state$visible <- TRUE
      state$opacity <- 1
    },
    "write_in" = {
      state$visible <- TRUE
      state$opacity <- progress
      state$write_progress <- progress
    },
    "fade_in" = {
      state$visible <- TRUE
      state$opacity <- progress
      if (!is.null(anim$shift_vector)) {
        shift <- anim$shift_vector * (1 - progress)
        state$position <- state$position + shift
      }
    },
    "fade_out" = {
      state$opacity <- 1 - progress
      if (!is.null(anim$shift_vector)) {
        shift <- anim$shift_vector * progress
        state$position <- state$position + shift
      }
      if (progress >= 1) state$visible <- FALSE
    },
    "grow_from_center" = {
      state$visible <- TRUE
      state$opacity <- 1
      state$scale_factor <- progress
    },
    "shrink_to_center" = {
      state$scale_factor <- 1 - progress
      if (progress >= 1) state$visible <- FALSE
    },
    "move_to" = {
      if (!is.null(anim$target_position)) {
        # Get starting position (would need to track this properly in full engine)
        start_pos <- state$position
        state$position <- lerp(start_pos, anim$target_position, progress)
      }
    },
    "shift" = {
      if (!is.null(anim$shift_vector)) {
        state$position <- state$position + anim$shift_vector * progress
      }
    },
    "rotate" = {
      if (!is.null(anim$angle)) {
        state$rotation <- state$rotation + anim$angle * progress
      }
    },
    "scale" = {
      if (!is.null(anim$factor)) {
        target_scale <- anim$factor
        state$scale_factor <- lerp(1, target_scale, progress)
      }
    },
    "transform" = {
      state$visible <- TRUE
      if (progress >= 1) {
        # At end of transform, source disappears (replaced by target)
        state$visible <- FALSE
      } else {
        # Interpolate points
        if (!is.null(scene) && !is.null(anim$target_id)) {
          target_obj <- scene$objects[[anim$target_id]]
          if (!is.null(target_obj) && !is.null(state$points) && !is.null(target_obj$points)) {
             # Match curves (assumes vector_utils loaded)
             matched <- match_curves(state$points, target_obj$points)
             
             # Interpolate
             new_points <- interpolate_matrices(matched[[1]], matched[[2]], progress)
             state$points <- new_points
             
             # Also interpolate color/opacity?
             # Simple color mix (if RGB)
             # Keeping it simple for points primarily
          }
        }
      }
    },
    "change_color" = {
      if (!is.null(anim$color)) {
        # Gradually interpolate color rather than jumping at progress==1
        original_color <- state$color %||% "#FFFFFF"
        state$color <- interpolate_color(original_color, anim$color, progress)
        state$visible <- TRUE
      }
    },
    "highlight_term" = {
      # Same as change_color: gradually tint toward the highlight color
      if (!is.null(anim$color)) {
        original_color <- state$color %||% "#FFFFFF"
        state$color <- interpolate_color(original_color, anim$color, progress)
        state$visible <- TRUE
      }
    },
    "custom" = {
      # Generic custom animation: treat as show + fade-in
      state$visible <- TRUE
      state$opacity <- progress
    },
    "apply_matrix" = {
       if (!is.null(anim$matrix) && !is.null(state$points) && nrow(state$points) > 0) {
         # Linear Interpolation of Transformation
         # P(t) = P_start * (1-t) + (P_start * M') * t
         #      = P_start * (1-t + M'*t)
         #      = P_start * (I*(1-t) + M'*t)
         
         mat <- anim$matrix
         if (!is.matrix(mat)) mat <- matrix(mat, ncol=3)
         
         # Interpolate matrix
         ident <- diag(3)
         interp_mat <- ident * (1 - progress) + mat * progress
         
         # Apply to points (Rows * Matrix Transpose)
         # Actually: (P * M_interp_transpose)
         # If M is the transform matrix, usually v_new = M v. 
         # In row form: v_new' = v' M'.
         state$points <- state$points %*% t(interp_mat)
         
         # Also update position center if needed (for rotation logic etc)
         if (!is.null(state$position)) {
           # Position is length 3 vector
           # Convert to 1x3 matrix
           pos_mat <- matrix(state$position, nrow=1)
           new_pos <- pos_mat %*% t(interp_mat)
           state$position <- as.vector(new_pos)
         }
       }
    }
  )

  state
}

#' Encode output video
#'
#' @param frames Frame file paths
#' @param output_path Output file path
#' @param format Output format
#' @param fps Frames per second
#' @param quality Quality preset
#' @param progress Show progress
#' @keywords internal
#' @noRd
encode_output <- function(frames, output_path, format, fps, quality, progress) {
  if (format == "png_sequence") {
    # Just copy frames to output directory
    output_dir <- output_path
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    for (i in seq_along(frames)) {
      file.copy(frames[i], file.path(output_dir, sprintf("frame_%05d.png", i)))
    }
    return()
  }

  if (format == "gif") {
    # Use magick for GIF
    if (requireNamespace("magick", quietly = TRUE)) {
      if (progress && manimR_options("verbose")) {
        cli::cli_alert_info("Encoding GIF...")
      }

      img <- magick::image_read(frames)
      # Use delay (1/100th of a second) instead of fps to avoid factor-of-100 restriction
      delay_hundredths <- max(1L, round(100 / fps))
      img <- magick::image_animate(img, delay = delay_hundredths)
      magick::image_write(img, output_path)
    } else {
      cli::cli_abort("Package {.pkg magick} is required for GIF output")
    }
    return()
  }

  # Use av for video
  if (requireNamespace("av", quietly = TRUE)) {
    if (progress && manimR_options("verbose")) {
      cli::cli_alert_info("Encoding video...")
    }

    av::av_encode_video(
      input = frames,
      output = output_path,
      framerate = fps,
      verbose = FALSE
    )
  } else {
    cli::cli_abort("Package {.pkg av} is required for video output")
  }
}
