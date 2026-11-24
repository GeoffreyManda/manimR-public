#' @title Scene Class and Functions
#' @description
#' The Scene is the fundamental container for all manimR animations. It holds
#' mobjects, manages the timeline, coordinates rendering, and provides both
#' a Manim-style `play()` API and a pipe-based API.
#' @name scene
NULL

#' Scene R6 Class
#'
#' @description
#' Core R6 class representing an animation scene. Supports two APIs:
#'
#' **Pipe API** (R-idiomatic):
#' ```
#' scene("demo") %>% add_circle() %>% fade_in() %>% render()
#' ```
#'
#' **Play API** (Manim-style):
#' ```
#' s <- Scene$new("demo")
#' c <- Circle$new()
#' s$add(c)
#' s$play(FadeIn$new(c))
#' s$render()
#' ```
#'
#' @rdname Scene-class
#' @export
#' @importFrom R6 R6Class
Scene <- R6::R6Class(
  "Scene",

  public = list(
    #' @field name Scene name/identifier
    name = NULL,

    #' @field width Scene width in pixels
    width = 1920,

    #' @field height Scene height in pixels
    height = 1080,

    #' @field fps Frames per second
    fps = 30,

    #' @field background_color Background color
    background_color = "#000000",

    #' @field objects Named list of objects in the scene
    objects = NULL,

    #' @field timeline Animation timeline (list of animation specs)
    timeline = NULL,

    #' @field current_time Current time in the scene
    current_time = 0,

    #' @field dimensions "2D" or "3D"
    dimensions = "2D",

    #' @field camera Camera object for the scene
    camera = NULL,

    #' @field updaters List of scene-level updater functions
    updaters = NULL,

    #' @field mobjects Ordered list of visible mobjects (draw order)
    mobjects = NULL,

    #' @field pending_removals Named list mapping object IDs to their removal time
    pending_removals = NULL,

    # ---- Constructor ----

    #' @description Create a new Scene
    #' @param name Scene name
    #' @param width Width in pixels
    #' @param height Height in pixels
    #' @param fps Frames per second
    #' @param background_color Background color (hex string). Also accepted as `bg`.
    #' @param bg Alias for `background_color`
    #' @param dimensions "2D" or "3D"
    initialize = function(name = "untitled",
                          width = 1920,
                          height = 1080,
                          fps = 30,
                          background_color = "#000000",
                          bg = NULL,
                          dimensions = "2D") {
      self$name <- name
      self$width <- width
      self$height <- height
      self$fps <- fps
      # Allow `bg` as an alias for `background_color` (project page API)
      if (!is.null(bg)) background_color <- bg
      self$background_color <- validate_color(background_color)
      self$dimensions <- match.arg(dimensions, c("2D", "3D"))
      self$objects <- list()
      self$timeline <- list()
      self$current_time <- 0
      self$updaters <- list()
      self$mobjects <- list()
      self$pending_removals <- list()

      # Create a default 2D camera using the scene's pixel dimensions.
      # ThreeDScene overrides self$camera with a ThreeDCamera after calling super().
      if (is.null(self$camera)) {
        self$camera <- Camera$new(
          pixel_width = width,
          pixel_height = height,
          background_color = self$background_color
        )
      }

      if (manimR_options("verbose")) {
        cli::cli_alert_info("Created scene: {.val {name}}")
      }
    },

    # ---- Mobject Management (Manim-style API) ----

    #' @description Add mobject(s) to the scene (makes them visible).
    #'   Manim-compatible: `scene$add(circle, square)`.
    #' @param ... MObjects to add to the scene
    add = function(...) {
      objs <- list(...)
      for (obj in objs) {
        if (inherits(obj, "MObject")) {
          id <- obj$id
          self$objects[[id]] <- obj
          # Add to draw-order list if not already present
          if (!any(vapply(self$mobjects, function(m) identical(m$id, id),
                          logical(1)))) {
            self$mobjects <- c(self$mobjects, list(obj))
          }
        }
      }
      invisible(self)
    },

    #' @description Remove mobject(s) from the scene.
    #' @param ... MObjects to remove
    remove = function(...) {
      objs <- list(...)
      for (obj in objs) {
        if (inherits(obj, "MObject")) {
          id <- obj$id
          self$objects[[id]] <- NULL
          self$mobjects <- Filter(function(m) !identical(m$id, id),
                                  self$mobjects)
        }
      }
      invisible(self)
    },

    #' @description Play one or more Animation objects.
    #'   This is the primary Manim-style API for adding animations.
    #' @param ... Animation objects to play simultaneously
    play = function(...) {
      anims <- list(...)

      for (anim in anims) {
        if (inherits(anim, "AnimationBuilder")) {
          anim <- anim$build()
        }

        if (!inherits(anim, "Animation")) {
          cli::cli_warn("play() received non-Animation object, skipping")
          next
        }

        # If animation introduces a mobject, add it to the scene
        if (anim$is_introducer() && !is.null(anim$mobject)) {
          self$add(anim$mobject)
        }

        # Convert Animation R6 to timeline spec
        spec <- anim$to_anim_spec(start_time = self$current_time)
        self$timeline <- append(self$timeline, list(spec))
      }

      # Advance time by the max run_time of all played animations
      if (length(anims) > 0) {
        max_rt <- max(vapply(anims, function(a) {
          if (inherits(a, "Animation")) a$run_time
          else if (inherits(a, "AnimationBuilder")) a$run_time
          else 1
        }, numeric(1)))
        self$current_time <- self$current_time + max_rt
      }

      # Handle removers: track deferred removal time instead of removing immediately.
      # The object stays in the scene so the fade-out animation can render,
      # and get_object_states_at_time() hides it once removal_time is past.
      for (anim in anims) {
        if (inherits(anim, "Animation") && anim$is_remover() &&
            !is.null(anim$mobject)) {
          mob <- anim$mobject
          mob_id <- mob$id %||% mob$name
          if (!is.null(mob_id)) {
            self$pending_removals[[mob_id]] <- self$current_time
          }
        }
      }

      invisible(self)
    },

    #' @description Wait (hold) for a duration. Manim-compatible.
    #' @param duration Duration in seconds
    wait = function(duration = 1) {
      duration <- validate_duration(duration)
      self$timeline <- append(self$timeline, list(list(
        type = "pause",
        id = generate_id("anim"),
        start_time = self$current_time,
        duration = duration
      )))
      self$current_time <- self$current_time + duration
      invisible(self)
    },

    # ---- Legacy Pipe API ----

    #' @description Add an object to the scene (legacy pipe API)
    #' @param object A MObject to add
    #' @param show_immediately Whether to show the object immediately
    add_object = function(object, show_immediately = FALSE) {
      id <- object$id
      self$objects[[id]] <- object
      # Also add to mobjects draw list
      if (!any(vapply(self$mobjects, function(m) identical(m$id, id),
                        logical(1)))) {
        self$mobjects <- c(self$mobjects, list(object))
      }

      if (show_immediately) {
        self$add_animation(list(
          type = "show",
          object_id = id,
          start_time = self$current_time,
          duration = 0
        ))
      }

      invisible(self)
    },

    #' @description Add an animation to the timeline (legacy pipe API)
    #' @param animation Animation specification (list)
    add_animation = function(animation) {
      animation$id <- generate_id("anim")
      self$timeline <- append(self$timeline, list(animation))

      if (!is.null(animation$duration)) {
        self$current_time <- self$current_time + animation$duration
      }

      invisible(self)
    },

    # ---- Queries ----

    #' @description Get an object by ID
    #' @param id Object ID
    get_object = function(id) {
      self$objects[[id]]
    },

    #' @description Get the most recently added object
    get_last_object = function() {
      if (length(self$objects) == 0) return(NULL)
      self$objects[[length(self$objects)]]
    },

    #' @description Get all mobjects currently in the scene
    get_mobjects = function() {
      self$mobjects
    },

    #' @description Calculate total duration
    get_duration = function() {
      if (length(self$timeline) == 0) return(0)

      max_end <- 0
      for (anim in self$timeline) {
        end_time <- (anim$start_time %||% 0) + (anim$duration %||% 0)
        max_end <- max(max_end, end_time)
      }
      max_end
    },

    #' @description Get total number of frames
    get_frame_count = function() {
      ceiling(self$get_duration() * self$fps)
    },

    #' @description Render the scene to a video file (project-page API)
    #' @param output Output file path (without extension). Defaults to scene name.
    #' @param fps Frames per second override (uses scene fps if NULL)
    #' @param width Width override (use `quality` preset in standalone render() instead)
    #' @param height Height override
    #' @param quality Quality preset ("low", "medium", "high", "4K")
    #' @param format Output format ("mp4", "gif", "webm")
    #' @param ... Additional args passed to the standalone `render()` function
    #' @return Output file path (invisibly)
    render = function(output = NULL, fps = NULL, width = NULL, height = NULL,
                      quality = "medium", format = "mp4", ...) {
      # If width/height are given, pick the closest quality preset
      if (!is.null(width) || !is.null(height)) {
        w <- width %||% 1920
        if      (w >= 3840) quality <- "4K"
        else if (w >= 1920) quality <- "high"
        else if (w >= 1280) quality <- "medium"
        else                quality <- "low"
      }
      render(
        scene = self,
        output = output,
        fps = fps,
        quality = quality,
        format = format,
        ...
      )
    },

    # ---- Scene Updaters ----

    #' @description Add a scene-level updater function
    #' @param func Function(scene, dt) called every frame
    add_updater = function(func) {
      self$updaters <- c(self$updaters, list(func))
      invisible(self)
    },

    #' @description Remove all scene updaters
    clear_updaters = function() {
      self$updaters <- list()
      invisible(self)
    },

    #' @description Process one frame of updaters
    #' @param dt Time step (1/fps)
    update = function(dt = NULL) {
      if (is.null(dt)) dt <- 1 / self$fps

      # Scene-level updaters
      for (func in self$updaters) {
        func(self, dt)
      }

      # Object-level updaters
      for (mob in self$mobjects) {
        if (!is.null(mob$updaters) && length(mob$updaters) > 0) {
          for (func in mob$updaters) {
            func(mob, dt)
          }
        }
      }

      invisible(self)
    },

    # ---- Camera Convenience API ----

    #' @description Zoom the camera in/out
    #' @param factor Zoom factor (> 1 = zoom in, < 1 = zoom out)
    #' @return Self (invisibly) for piping
    camera_zoom = function(factor) {
      if (!is.null(self$camera)) self$camera$set_zoom(factor)
      invisible(self)
    },

    #' @description Pan the camera to a position
    #' @param x X coordinate in world space
    #' @param y Y coordinate in world space
    #' @return Self (invisibly) for piping
    camera_move_to = function(x, y) {
      if (!is.null(self$camera)) self$camera$move_to(c(x, y, 0))
      invisible(self)
    },

    #' @description Shift the camera by a vector
    #' @param dx Horizontal shift
    #' @param dy Vertical shift
    #' @return Self (invisibly) for piping
    camera_shift = function(dx, dy) {
      if (!is.null(self$camera)) self$camera$shift(c(dx, dy, 0))
      invisible(self)
    },

    #' @description Rotate the camera (2D rotation around frame center)
    #' @param angle Rotation in radians
    #' @return Self (invisibly) for piping
    camera_rotate = function(angle) {
      if (!is.null(self$camera)) self$camera$rotation <- self$camera$rotation + angle
      invisible(self)
    },

    # ---- Convenience Methods ----

    #' @description Clear the scene of all objects and animations
    clear = function() {
      self$objects <- list()
      self$mobjects <- list()
      self$timeline <- list()
      self$current_time <- 0
      invisible(self)
    },

    #' @description Get the coordinate-to-pixel conversion factors
    #' @return List with x_scale and y_scale
    get_coord_to_pixel = function() {
      list(
        x_scale = self$width / FRAME_WIDTH,
        y_scale = self$height / FRAME_HEIGHT
      )
    },

    # ---- Print ----

    #' @description Print scene summary
    print = function() {
      cli::cli_h1("manimR Scene: {self$name}")
      cli::cli_ul()
      cli::cli_li("Dimensions: {self$width}x{self$height} ({self$dimensions})")
      cli::cli_li("FPS: {self$fps}")
      cli::cli_li("Duration: {round(self$get_duration(), 2)}s")
      cli::cli_li("Objects: {length(self$objects)}")
      cli::cli_li("Mobjects (draw order): {length(self$mobjects)}")
      cli::cli_li("Animations: {length(self$timeline)}")
      cli::cli_li("Updaters: {length(self$updaters)}")
      cli::cli_end()
      invisible(self)
    }
  ),

  private = list(
    .render_cache = NULL,
    .is_dirty = TRUE
  )
)

# ---- Factory Functions (Pipe API) ----

#' Create a new animation scene
#'
#' @description
#' Creates a new animation scene, which is the container for all objects and
#' animations in manimR.
#'
#' @param name A name for the scene (used for output filenames)
#' @param width Width in pixels (default: 1920 for 1080p)
#' @param height Height in pixels (default: 1080 for 1080p)
#' @param fps Frames per second (default: 30)
#' @param background_color Background color (default: black)
#' @param dimensions Either "2D" or "3D"
#'
#' @return A Scene object that can be piped to other manimR functions
#'
#' @examples
#' \dontrun{
#' # Pipe API
#' scene("my_animation") %>%
#'   add_text("Hello, World!") %>%
#'   render()
#'
#' # Play API
#' s <- Scene$new("demo")
#' c <- Circle$new(color = BLUE)
#' s$play(Create$new(c))
#' s$play(FadeOut$new(c))
#' }
#'
#' @export
scene <- function(name = "untitled",
                  width = 1920,
                  height = 1080,
                  fps = 30,
                  background_color = "#000000",
                  dimensions = "2D") {

  fps <- fps %||% manimR_options("fps")
  background_color <- background_color %||% manimR_options("background_color")

  Scene$new(
    name = name,
    width = width,
    height = height,
    fps = fps,
    background_color = background_color,
    dimensions = dimensions
  )
}

# NOTE: scene_3d() is defined in scene_types.R (returns ThreeDScene)

#' Pause the animation
#'
#' @description
#' Adds a pause to the animation timeline. During the pause, all visible
#' objects remain static.
#'
#' @param scene A Scene object
#' @param duration Duration of the pause in seconds
#'
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("demo") %>%
#'   add_text("First") %>%
#'   write_in() %>%
#'   pause(2) %>%
#'   add_text("Second") %>%
#'   write_in() %>%
#'   render()
#' }
#'
#' @export
pause <- function(scene, duration = 1) {
  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  duration <- validate_duration(duration)
  scene$wait(duration)
  invisible(scene)
}

#' @rdname pause
#' @export
wait <- pause

#' Chain animations sequentially
#'
#' @description
#' Explicitly indicates that the next animation should happen after the
#' previous one completes.
#'
#' @param scene A Scene object
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
then <- function(scene) {
  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }
  invisible(scene)
}
