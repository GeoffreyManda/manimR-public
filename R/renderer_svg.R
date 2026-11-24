#' @title SVG Renderer Backend
#' @description
#' Renders scenes to SVG files for vector-quality output. Each frame
#' is a standalone SVG document. Ideal for web embedding and print.
#' @name renderer_svg
#' @keywords internal
NULL

#' SVGRenderer R6 Class
#'
#' @description
#' Renders manimR scenes to SVG format. Produces scalable vector
#' graphics that can be embedded in web pages or printed without
#' loss of quality.
#'
#' @export
SVGRenderer <- R6::R6Class(
  "SVGRenderer",
  inherit = AbstractRenderer,

  public = list(
    #' @field svg_lines Accumulated SVG element strings
    svg_lines = NULL,

    #' @field output_path Current output file path
    output_path = NULL,

    #' @field viewbox_x_range X range in scene units
    viewbox_x_range = NULL,

    #' @field viewbox_y_range Y range in scene units
    viewbox_y_range = NULL,

    #' @description Create SVGRenderer
    #' @param ... Arguments passed to AbstractRenderer
    initialize = function(...) {
      super$initialize(...)
      self$backend_name <- "svg"

      aspect_ratio <- self$width / self$height
      self$viewbox_x_range <- FRAME_Y_RADIUS * aspect_ratio
      self$viewbox_y_range <- FRAME_Y_RADIUS
    },

    #' @description Begin a new SVG frame
    #' @param output_path Path to save SVG file
    begin_frame = function(output_path = NULL) {
      self$output_path <- output_path %||% tempfile(fileext = ".svg")
      self$svg_lines <- character(0)

      # Use camera viewport bounds when available
      if (!is.null(self$camera)) {
        bounds <- self$camera$get_visible_bounds()
        self$viewbox_x_range <- (bounds$max[1] - bounds$min[1]) / 2
        self$viewbox_y_range <- (bounds$max[2] - bounds$min[2]) / 2
      }

      # SVG viewBox uses scene coordinates (flipped Y)
      vb_x <- self$viewbox_x_range
      vb_y <- self$viewbox_y_range

      self$svg_lines <- c(self$svg_lines, sprintf(
        '<?xml version="1.0" encoding="UTF-8"?>',
      ))
      self$svg_lines <- c(self$svg_lines, sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="%f %f %f %f">',
        self$width, self$height,
        -vb_x, -vb_y, 2 * vb_x, 2 * vb_y
      ))

      # Background
      self$svg_lines <- c(self$svg_lines, sprintf(
        '<rect x="%f" y="%f" width="%f" height="%f" fill="%s"/>',
        -vb_x, -vb_y, 2 * vb_x, 2 * vb_y, self$background_color
      ))

      # Flip Y axis (SVG Y goes down, scene Y goes up)
      self$svg_lines <- c(self$svg_lines,
        '<g transform="scale(1,-1)">'
      )
    },

    #' @description End the SVG frame and write to file
    end_frame = function() {
      self$svg_lines <- c(self$svg_lines, '</g>', '</svg>')
      writeLines(self$svg_lines, self$output_path)
    },

    #' @description Draw a path in SVG
    draw_path = function(x, y, fill_color = NULL, fill_opacity = 0,
                         stroke_color = "#FFFFFF", stroke_opacity = 1,
                         stroke_width = 2) {
      if (length(x) < 2) return()

      # Build SVG path data
      d <- paste0("M ", x[1], " ", y[1])
      for (i in 2:length(x)) {
        d <- paste0(d, " L ", x[i], " ", y[i])
      }
      d <- paste0(d, " Z")

      # Fill
      fill_attr <- if (!is.null(fill_color) && fill_opacity > 0) {
        sprintf('fill="%s" fill-opacity="%f"', fill_color, fill_opacity)
      } else {
        'fill="none"'
      }

      # Stroke
      stroke_attr <- if (stroke_opacity > 0 && stroke_width > 0) {
        # Convert scene-unit stroke width to SVG units
        svg_sw <- stroke_width * 0.01  # Scale down for SVG viewBox
        sprintf('stroke="%s" stroke-opacity="%f" stroke-width="%f" stroke-linecap="round" stroke-linejoin="round"',
                stroke_color, stroke_opacity, svg_sw)
      } else {
        'stroke="none"'
      }

      self$svg_lines <- c(self$svg_lines,
        sprintf('<path d="%s" %s %s/>', d, fill_attr, stroke_attr)
      )
    },

    #' @description Draw text in SVG
    draw_text = function(text, x, y, color = "#FFFFFF", font_size = 24,
                         font_family = "sans", opacity = 1) {
      # SVG font size needs to be in viewBox units
      svg_fs <- font_size * 0.02  # Scale for viewBox

      self$svg_lines <- c(self$svg_lines, sprintf(
        paste0(
          '<text x="%f" y="%f" fill="%s" fill-opacity="%f" font-size="%f" ',
          'font-family="%s" text-anchor="middle" dominant-baseline="central" ',
          'transform="scale(1,-1) translate(0,%f)">%s</text>'
        ),
        x, -y, color, opacity, svg_fs, font_family, 2 * y,
        private$escape_xml(text)
      ))
    },

    #' @description Draw raster image in SVG (base64 embedded)
    draw_raster = function(raster, xleft, ybottom, xright, ytop, opacity = 1) {
      # Raster images would need base64 encoding
      # This is a placeholder for full implementation
    },

    #' @description Get the SVG content as a string
    get_svg_string = function() {
      paste(self$svg_lines, collapse = "\n")
    }
  ),

  private = list(
    escape_xml = function(text) {
      text <- gsub("&", "&amp;", text, fixed = TRUE)
      text <- gsub("<", "&lt;", text, fixed = TRUE)
      text <- gsub(">", "&gt;", text, fixed = TRUE)
      text <- gsub('"', "&quot;", text, fixed = TRUE)
      text <- gsub("'", "&apos;", text, fixed = TRUE)
      text
    }
  )
)
