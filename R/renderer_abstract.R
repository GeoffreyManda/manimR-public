#' @title Abstract Renderer Interface
#' @description
#' Defines the abstract interface that all rendering backends must implement.
#' Provides factory function for creating renderers and shared utilities.
#' @name renderer_abstract
#' @keywords internal
NULL

#' AbstractRenderer R6 Class
#'
#' @description
#' Base class that defines the rendering interface. All backends (ragg, SVG,
#' grid) must implement these methods. Handles coordinate system setup,
#' camera integration, and mobject dispatch.
#'
#' @keywords internal
AbstractRenderer <- R6::R6Class(
  "AbstractRenderer",

  public = list(
    #' @field width Canvas width in pixels
    width = 1280,

    #' @field height Canvas height in pixels
    height = 720,

    #' @field background_color Background color
    background_color = "#000000",

    #' @field camera Camera object for coordinate transforms
    camera = NULL,

    #' @field backend_name Name of this backend
    backend_name = "abstract",

    #' @description Initialize renderer
    #' @param width Width in pixels
    #' @param height Height in pixels
    #' @param background_color Background color
    #' @param camera Optional Camera object
    initialize = function(width = 1280, height = 720,
                          background_color = "#000000",
                          camera = NULL) {
      self$width <- width
      self$height <- height
      self$background_color <- background_color
      self$camera <- camera
    },

    # ---- Abstract methods (must be overridden) ----

    #' @description Begin a new frame (open device/canvas)
    #' @param output_path Path to save the frame
    begin_frame = function(output_path = NULL) {
      stop("AbstractRenderer$begin_frame() must be overridden")
    },

    #' @description End the current frame (close device/finalize)
    end_frame = function() {
      stop("AbstractRenderer$end_frame() must be overridden")
    },

    #' @description Draw a filled + stroked polygon path
    #' @param x X coordinates
    #' @param y Y coordinates
    #' @param fill_color Fill color (NULL = no fill)
    #' @param fill_opacity Fill opacity
    #' @param stroke_color Stroke color
    #' @param stroke_opacity Stroke opacity
    #' @param stroke_width Stroke width
    draw_path = function(x, y, fill_color = NULL, fill_opacity = 0,
                         stroke_color = "#FFFFFF", stroke_opacity = 1,
                         stroke_width = 2) {
      stop("AbstractRenderer$draw_path() must be overridden")
    },

    #' @description Draw text at a position
    #' @param text Text string
    #' @param x X coordinate
    #' @param y Y coordinate
    #' @param color Text color
    #' @param font_size Font size
    #' @param font_family Font family
    #' @param opacity Opacity
    draw_text = function(text, x, y, color = "#FFFFFF", font_size = 24,
                         font_family = "sans", opacity = 1) {
      stop("AbstractRenderer$draw_text() must be overridden")
    },

    #' @description Draw a raster image
    #' @param raster Raster data
    #' @param xleft Left x
    #' @param ybottom Bottom y
    #' @param xright Right x
    #' @param ytop Top y
    #' @param opacity Opacity
    draw_raster = function(raster, xleft, ybottom, xright, ytop, opacity = 1) {
      stop("AbstractRenderer$draw_raster() must be overridden")
    },

    # ---- Concrete methods (shared by all backends) ----

    #' @description Render a complete frame with all mobjects
    #' @param scene Scene object
    #' @param time Current time
    #' @param output_path Output file path
    render_frame = function(scene, time, output_path) {
      self$begin_frame(output_path)
      on.exit(self$end_frame())

      # Get states
      states <- get_object_states_at_time(scene, time)

      # Sort by z_index
      z_indices <- vapply(states, function(s) s$z_index %||% 0, numeric(1))
      draw_order <- order(z_indices)

      for (idx in draw_order) {
        state <- states[[idx]]
        if (isTRUE(state$visible) && (state$opacity %||% 0) > 0) {
          self$draw_state(state)
        }
      }
    },

    #' @description Render live mobjects (for Manim-style play())
    #' @param mobjects List of MObject instances
    #' @param output_path Output file path
    render_mobjects = function(mobjects, output_path = NULL) {
      self$begin_frame(output_path)
      on.exit(self$end_frame())

      # Sort by z_index
      z_idx <- vapply(mobjects, function(m) m$z_index %||% 0, numeric(1))
      for (i in order(z_idx)) {
        mob <- mobjects[[i]]
        if ((mob$opacity %||% 0) > 0) {
          self$draw_mobject(mob)
        }
      }
    },

    #' @description Draw a state dict (from get_object_states_at_time)
    #' @param state Object state list
    draw_state = function(state) {
      type <- state$type %||% "unknown"

      # Handle R6 Animation objects
      if (!is.null(state$animation_object) &&
          inherits(state$animation_object, "Animation")) {
        mob <- state$animation_object$mobject
        if (!is.null(mob)) {
          self$draw_mobject(mob)
          return()
        }
      }

      switch(type,
        "text" = self$draw_text_state(state),
        "latex" = self$draw_latex_state(state),
        "shape" = self$draw_shape_state(state),
        "image" = self$draw_image_state(state),
        "vmobject" = self$draw_vmobject_state(state),
        {
          # Default: try VMobject path if points exist
          if (!is.null(state$points) && length(state$points) > 0) {
            self$draw_vmobject_state(state)
          }
        }
      )
    },

    #' @description Draw a live MObject
    #' @param mob An MObject instance
    draw_mobject = function(mob) {
      if (inherits(mob, "VMobject")) {
        self$draw_vmobject_live(mob)
      } else if (inherits(mob, "TextObject")) {
        pos2d <- private$apply_camera_to_position(mob$position)
        self$draw_text(
          text = mob$text, x = pos2d[1], y = pos2d[2],
          color = mob$color, font_size = mob$size %||% 24,
          opacity = mob$opacity
        )
      } else if (inherits(mob, "ImageMobject")) {
        pos2d <- private$apply_camera_to_position(mob$position)
        self$draw_image_state(list(
          position = c(pos2d, 0), opacity = mob$opacity,
          scale_factor = mob$scale_factor,
          data = list(path = mob$image_path)
        ))
      }
    },

    #' @description Draw a live VMobject (recursive with submobjects)
    #' @param mob VMobject instance
    draw_vmobject_live = function(mob) {
      opacity <- mob$opacity %||% 1
      if (opacity <= 0) return()

      pos <- mob$position
      scale <- mob$scale_factor %||% 1

      # Draw own curves
      if (mob$has_points()) {
        pts <- mob$points

        if (scale != 1) pts <- pts * scale
        if (!is.null(mob$rotation) && mob$rotation != 0) {
          pts <- rotate_points_3d(pts, mob$rotation, axis = c(0, 0, 1))
        }

        pts[, 1] <- pts[, 1] + pos[1]
        pts[, 2] <- pts[, 2] + pos[2]

        draw_pts <- sample_bezier(pts, precision = 20)
        if (!is.null(draw_pts) && nrow(draw_pts) > 0) {
          # Apply camera projection (3D → 2D or 2D camera rotation)
          draw_pts2d <- private$apply_camera_to_points(draw_pts)
          segments <- private$split_at_breaks(draw_pts2d)
          for (seg in segments) {
            if (nrow(seg) < 2) next
            fill_op <- (mob$fill_opacity %||% 0) * opacity
            stroke_op <- (mob$stroke_opacity %||% 1) * opacity
            self$draw_path(
              x = seg[, 1], y = seg[, 2],
              fill_color = mob$fill_color,
              fill_opacity = fill_op,
              stroke_color = mob$stroke_color %||% mob$color %||% WHITE,
              stroke_opacity = stroke_op,
              stroke_width = mob$stroke_width %||% DEFAULT_STROKE_WIDTH
            )
          }
        }
      }

      # Draw submobjects
      for (sub in mob$submobjects) {
        if (inherits(sub, "VMobject")) {
          self$draw_vmobject_live(sub)
        } else if (inherits(sub, "MObject")) {
          self$draw_mobject(sub)
        }
      }
    },

    #' @description Draw VMobject from state dict
    #' @param state State list
    draw_vmobject_state = function(state) {
      if (is.null(state$points) || nrow(state$points) == 0) return()

      pts <- state$points
      scale <- state$scale_factor %||% 1
      pos <- state$position
      opacity <- state$opacity %||% 1

      if (scale != 1) pts[, 1:2] <- pts[, 1:2] * scale
      if (!is.null(state$rotation) && state$rotation != 0) {
        pts <- rotate_points_3d(pts, state$rotation, axis = c(0, 0, 1))
      }

      pts[, 1] <- pts[, 1] + pos[1]
      pts[, 2] <- pts[, 2] + pos[2]

      draw_pts <- sample_bezier(pts, precision = 20)
      if (is.null(draw_pts) || nrow(draw_pts) == 0) return()

      # Apply camera projection (3D → 2D or 2D camera rotation)
      draw_pts2d <- private$apply_camera_to_points(draw_pts)
      segments <- private$split_at_breaks(draw_pts2d)
      for (seg in segments) {
        if (nrow(seg) < 2) next
        self$draw_path(
          x = seg[, 1], y = seg[, 2],
          fill_color = state$fill_color,
          fill_opacity = (state$fill_opacity %||% 0) * opacity,
          stroke_color = state$stroke_color %||% state$color %||% WHITE,
          stroke_opacity = (state$stroke_opacity %||% 1) * opacity,
          stroke_width = state$stroke_width %||% DEFAULT_STROKE_WIDTH
        )
      }
    },

    #' @description Draw text from state dict
    #' @param state State list
    draw_text_state = function(state) {
      pos2d <- private$apply_camera_to_position(state$position)
      self$draw_text(
        text = state$text %||% "",
        x = pos2d[1], y = pos2d[2],
        color = state$color %||% WHITE,
        font_size = state$size %||% 24,
        font_family = state$font %||% "sans",
        opacity = state$opacity %||% 1
      )
    },

    #' @description Draw LaTeX from state dict
    #' @param state State list
    draw_latex_state = function(state) {
      if (!is.null(state$points) && nrow(state$points) > 0) {
        self$draw_vmobject_state(state)
      } else if (!is.null(state$data$path) && file.exists(state$data$path)) {
        self$draw_image_state(state)
      } else {
        self$draw_text_state(list(
          position = state$position, color = state$color,
          opacity = state$opacity, text = state$latex,
          size = 48 * (state$size %||% 1)
        ))
      }
    },

    #' @description Draw shape from state dict (legacy)
    #' @param state State list
    draw_shape_state = function(state) {
      pos2d <- private$apply_camera_to_position(state$position)
      pos <- c(pos2d, 0)  # Re-expand to 3-element for downstream code
      scale <- state$scale_factor %||% 1
      opacity <- state$opacity %||% 1

      if (state$shape_type == "circle") {
        radius <- (state$radius %||% 1) * scale
        theta <- seq(0, TAU, length.out = 100)
        x <- pos[1] + radius * cos(theta)
        y <- pos[2] + radius * sin(theta)
        self$draw_path(
          x, y,
          fill_color = state$fill_color,
          fill_opacity = (state$fill_opacity %||% 0.5) * opacity,
          stroke_color = state$color,
          stroke_opacity = opacity,
          stroke_width = (state$stroke_width %||% 2) * scale
        )
      } else if (state$shape_type == "rectangle") {
        w <- (state$width %||% 2) * scale / 2
        h <- (state$height %||% 1) * scale / 2
        x <- c(pos[1]-w, pos[1]+w, pos[1]+w, pos[1]-w, pos[1]-w)
        y <- c(pos[2]-h, pos[2]-h, pos[2]+h, pos[2]+h, pos[2]-h)
        self$draw_path(x, y,
          fill_color = state$fill_color,
          fill_opacity = (state$fill_opacity %||% 0.5) * opacity,
          stroke_color = state$color, stroke_opacity = opacity,
          stroke_width = (state$stroke_width %||% 2) * scale
        )
      } else if (state$shape_type == "line") {
        end_pt <- state$data$end %||% (pos + c(1, 0, 0))
        self$draw_path(
          c(pos[1], end_pt[1]), c(pos[2], end_pt[2]),
          stroke_color = state$color, stroke_opacity = opacity,
          stroke_width = (state$stroke_width %||% 2) * scale
        )
      } else if (state$shape_type == "path") {
        path_x <- state$data$x
        path_y <- state$data$y
        if (!is.null(path_x) && !is.null(path_y) && length(path_x) >= 2) {
          self$draw_path(
            path_x * scale + pos[1], path_y * scale + pos[2],
            stroke_color = state$color, stroke_opacity = opacity,
            stroke_width = (state$stroke_width %||% 2) * scale
          )
        }
      }
    },

    #' @description Draw image from state dict
    #' @param state State list
    draw_image_state = function(state) {
      # Default implementation - backends override for native image support
    }
  ),

  private = list(
    # Apply camera transform to a world-space point matrix (N x 3) returning
    # a 2D matrix (N x 2).
    #
    # For a 2D Camera: rotates points around the camera centre and returns
    # (x, y) in world coordinates (the plot.window/viewport already handles
    # the translation via xlim/ylim).
    #
    # For a ThreeDCamera: performs full perspective projection to 2D.
    #
    # If no camera is set, returns the first two columns unchanged.
    apply_camera_to_points = function(pts) {
      if (is.null(pts) || nrow(pts) == 0) return(pts[, 1:2, drop = FALSE])

      cam <- self$camera

      if (is.null(cam)) {
        return(pts[, 1:2, drop = FALSE])
      }

      # 3D perspective camera
      if (inherits(cam, "ThreeDCamera")) {
        return(cam$project_points(pts))
      }

      # 2D camera: apply rotation around the camera centre
      if (abs(cam$rotation) > 1e-10) {
        center <- cam$frame_center
        rel <- sweep(pts[, 1:2, drop = FALSE], 2, center[1:2], "-")
        ca <- cos(-cam$rotation)
        sa <- sin(-cam$rotation)
        rotated <- cbind(
          rel[, 1] * ca - rel[, 2] * sa,
          rel[, 1] * sa + rel[, 2] * ca
        )
        out <- sweep(rotated, 2, center[1:2], "+")
        return(out)
      }

      # No rotation needed — keep world coords as-is (xlim/ylim handle translation)
      pts[, 1:2, drop = FALSE]
    },

    # Apply camera transform to a single position vector (length ≥ 2)
    apply_camera_to_position = function(pos) {
      if (is.null(self$camera)) return(pos[1:2])

      mat <- matrix(c(pos[1], pos[2], if (length(pos) >= 3) pos[3] else 0),
                    nrow = 1, ncol = 3)
      res <- private$apply_camera_to_points(mat)
      c(res[1, 1], res[1, 2])
    },

    split_at_breaks = function(pts) {
      if (is.null(pts) || nrow(pts) == 0) return(list())
      na_rows <- which(is.na(pts[, 1]))
      if (length(na_rows) == 0) return(list(pts))

      segments <- list()
      start <- 1
      for (br in na_rows) {
        if (br > start) {
          segments <- c(segments, list(pts[start:(br - 1), , drop = FALSE]))
        }
        start <- br + 1
      }
      if (start <= nrow(pts)) {
        segments <- c(segments, list(pts[start:nrow(pts), , drop = FALSE]))
      }
      segments
    }
  )
)


#' Create a renderer for the specified backend
#'
#' @description
#' Factory function that creates the appropriate renderer based on the
#' backend name. Falls back to ragg if the requested backend is unavailable.
#'
#' @param backend Backend name: "ragg", "svg", "grid"
#' @param width Width in pixels
#' @param height Height in pixels
#' @param background_color Background color
#' @param camera Optional Camera object
#' @return An AbstractRenderer subclass instance
#' @export
create_renderer <- function(backend = "ragg",
                            width = 1280,
                            height = 720,
                            background_color = "#000000",
                            camera = NULL) {
  backend <- match.arg(backend, c("ragg", "svg", "grid"))

  renderer <- switch(backend,
    "ragg" = {
      if (!requireNamespace("ragg", quietly = TRUE)) {
        cli::cli_warn("ragg not available, falling back to grid backend")
        GridRenderer$new(width = width, height = height,
                        background_color = background_color, camera = camera)
      } else {
        AggRenderer$new(width = width, height = height,
                       background_color = background_color, camera = camera)
      }
    },
    "svg" = SVGRenderer$new(width = width, height = height,
                            background_color = background_color, camera = camera),
    "grid" = GridRenderer$new(width = width, height = height,
                              background_color = background_color, camera = camera)
  )

  renderer
}
