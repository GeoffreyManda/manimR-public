#' @title Grid Graphics Renderer Backend
#' @description
#' Renders scenes using R's built-in grid graphics system. No external
#' packages required beyond base R. Suitable as a universal fallback.
#' @name renderer_grid
#' @keywords internal
NULL

#' GridRenderer R6 Class
#'
#' @description
#' Renders manimR scenes using the grid graphics package (part of base R).
#' This is the fallback renderer when ragg is not available.
#'
#' @export
GridRenderer <- R6::R6Class(
  "GridRenderer",
  inherit = AbstractRenderer,

  public = list(
    #' @field output_path Current output file path
    output_path = NULL,

    #' @field viewport The grid viewport
    viewport = NULL,

    #' @description Create GridRenderer
    #' @param ... Arguments passed to AbstractRenderer
    initialize = function(...) {
      super$initialize(...)
      self$backend_name <- "grid"
    },

    #' @description Begin a new frame using grid graphics
    #' @param output_path Path to save frame (PNG via grDevices)
    begin_frame = function(output_path = NULL) {
      self$output_path <- output_path %||% tempfile(fileext = ".png")

      grDevices::png(
        filename = self$output_path,
        width = self$width,
        height = self$height,
        units = "px",
        bg = self$background_color,
        res = 144
      )

      # Create viewport with scene coordinate system
      # Use camera viewport bounds when available
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

      grid::grid.newpage()

      # Push a viewport matching scene coordinates
      self$viewport <- grid::viewport(
        xscale = xlim,
        yscale = ylim,
        name = "manimr_scene"
      )
      grid::pushViewport(self$viewport)
    },

    #' @description End the current frame
    end_frame = function() {
      grid::popViewport()
      grDevices::dev.off()
    },

    #' @description Draw a path using grid graphics
    draw_path = function(x, y, fill_color = NULL, fill_opacity = 0,
                         stroke_color = "#FFFFFF", stroke_opacity = 1,
                         stroke_width = 2) {
      if (length(x) < 2) return()

      # Convert scene coordinates to grid native units
      x_unit <- grid::unit(x, "native")
      y_unit <- grid::unit(y, "native")

      # Determine graphical parameters
      gp_args <- list()

      if (!is.null(fill_color) && fill_opacity > 0) {
        gp_args$fill <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)
      } else {
        gp_args$fill <- NA
      }

      if (stroke_opacity > 0 && stroke_width > 0) {
        gp_args$col <- grDevices::adjustcolor(stroke_color, alpha.f = stroke_opacity)
        gp_args$lwd <- stroke_width
        gp_args$lineend <- "round"
        gp_args$linejoin <- "round"
      } else {
        gp_args$col <- NA
      }

      gp <- do.call(grid::gpar, gp_args)

      # Draw as polygon (filled) or polyline (stroked only)
      if (!is.null(fill_color) && fill_opacity > 0) {
        grid::grid.polygon(
          x = x_unit, y = y_unit,
          gp = gp,
          default.units = "native"
        )
      } else {
        grid::grid.polyline(
          x = x_unit, y = y_unit,
          gp = gp,
          default.units = "native"
        )
      }
    },

    #' @description Draw text using grid graphics
    draw_text = function(text, x, y, color = "#FFFFFF", font_size = 24,
                         font_family = "sans", opacity = 1) {
      col <- grDevices::adjustcolor(color, alpha.f = opacity)

      # Convert font_size to grid units (approximate)
      gp <- grid::gpar(
        col = col,
        fontsize = font_size * 0.6,  # Scale for viewport
        fontfamily = font_family
      )

      grid::grid.text(
        label = text,
        x = grid::unit(x, "native"),
        y = grid::unit(y, "native"),
        gp = gp,
        just = "centre"
      )
    },

    #' @description Draw raster image using grid graphics
    draw_raster = function(raster, xleft, ybottom, xright, ytop, opacity = 1) {
      grid::grid.raster(
        raster,
        x = grid::unit((xleft + xright) / 2, "native"),
        y = grid::unit((ybottom + ytop) / 2, "native"),
        width = grid::unit(xright - xleft, "native"),
        height = grid::unit(ytop - ybottom, "native")
      )
    }
  )
)
