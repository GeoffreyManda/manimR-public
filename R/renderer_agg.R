#' @title Agg Renderer Backend
#' @description
#' High-performance renderer using the Anti-Grain Geometry (Agg) library via
#' the ragg package. Inherits from AbstractRenderer and implements the
#' concrete rendering methods for raster output.
#' @name renderer_agg
#' @keywords internal
NULL

#' AggRenderer R6 Class
#'
#' @description
#' Renders manimR scenes to raster (PNG) frames using the ragg backend.
#' Supports VMobject Bezier paths with fill and stroke, submobject
#' recursion, text, images, and the full R6 Animation system.
#'
#' @export
#' @importFrom R6 R6Class
AggRenderer <- R6::R6Class(
  "AggRenderer",
  inherit = AbstractRenderer,

  public = list(
    #' @description Create AggRenderer
    #' @param ... Arguments passed to AbstractRenderer
    initialize = function(...) {
      super$initialize(...)
      self$backend_name <- "ragg"
    },

    #' @description Begin a new frame using ragg
    #' @param output_path Path to save PNG file
    begin_frame = function(output_path = NULL) {
      path <- output_path %||% tempfile(fileext = ".png")
      ragg::agg_png(
        filename = path,
        width = self$width,
        height = self$height,
        units = "px",
        bg = self$background_color,
        res = 144
      )

      par(mar = c(0, 0, 0, 0))

      # Use camera viewport if available; otherwise fall back to FRAME_Y_RADIUS defaults
      if (!is.null(self$camera)) {
        bounds <- self$camera$get_visible_bounds()
        xlim <- c(bounds$min[1], bounds$max[1])
        ylim <- c(bounds$min[2], bounds$max[2])
      } else {
        aspect_ratio <- self$width / self$height
        x_range <- FRAME_Y_RADIUS * aspect_ratio
        xlim <- c(-x_range, x_range)
        ylim <- c(-FRAME_Y_RADIUS, FRAME_Y_RADIUS)
      }

      plot.new()
      plot.window(xlim = xlim, ylim = ylim, asp = 1)
    },

    #' @description End the current frame
    end_frame = function() {
      dev.off()
    },

    #' @description Draw a path (polygon/polyline) with fill and stroke
    draw_path = function(x, y, fill_color = NULL, fill_opacity = 0,
                         stroke_color = "#FFFFFF", stroke_opacity = 1,
                         stroke_width = 2) {
      if (length(x) < 2) return()

      # Draw fill
      if (!is.null(fill_color) && fill_opacity > 0) {
        fill_col <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)
        graphics::polygon(x, y, col = fill_col, border = NA)
      }

      # Draw stroke
      if (stroke_opacity > 0 && stroke_width > 0) {
        stroke_col <- grDevices::adjustcolor(stroke_color, alpha.f = stroke_opacity)
        graphics::lines(x, y, col = stroke_col, lwd = stroke_width)
      }
    },

    #' @description Draw text
    draw_text = function(text, x, y, color = "#FFFFFF", font_size = 24,
                         font_family = "sans", opacity = 1) {
      col <- grDevices::adjustcolor(color, alpha.f = opacity)
      graphics::text(
        x = x, y = y,
        labels = text,
        col = col,
        cex = font_size / 24,
        family = font_family
      )
    },

    #' @description Draw a raster image
    draw_raster = function(raster, xleft, ybottom, xright, ytop, opacity = 1) {
      graphics::rasterImage(raster, xleft, ybottom, xright, ytop)
    },

    #' @description Draw image from state dict (ragg-specific with magick)
    draw_image_state = function(state) {
      if (is.null(state$data$path) || !file.exists(state$data$path)) return()

      tryCatch({
        img <- magick::image_read(state$data$path)
        info <- magick::image_info(img)
        raster_data <- as.raster(img)

        opacity <- state$opacity %||% 1
        if (opacity < 1) {
          raster_data <- grDevices::adjustcolor(raster_data, alpha.f = opacity)
          raster_data <- as.raster(
            matrix(raster_data, nrow = info$height, ncol = info$width)
          )
        }

        # Apply camera projection to position
        pos2d <- private$apply_camera_to_position(state$position)
        scale <- state$scale_factor %||% 1
        aspect <- info$width / info$height
        h <- 0.5 * scale * (state$size %||% 1)
        w <- h * aspect

        self$draw_raster(
          raster_data,
          pos2d[1] - w/2, pos2d[2] - h/2,
          pos2d[1] + w/2, pos2d[2] + h/2,
          opacity
        )
      }, error = function(e) {
        # Silently skip image drawing errors
      })
    }
  )
)


#' Legacy Renderer alias (backward compatibility)
#'
#' @description
#' The original Renderer class is now an alias for AggRenderer.
#' Existing code using Renderer$new() will continue to work.
#'
#' @export
Renderer <- AggRenderer
