#' @title NumberPlane and Grid Classes
#' @description
#' 2D grid coordinate systems with background grid lines.
#' NumberPlane provides a full Cartesian grid overlay similar to
#' graph paper, with configurable styling and fading.
#' @name number_plane
#' @keywords internal
NULL

#' NumberPlane R6 Class
#'
#' @description
#' A 2D coordinate plane with background grid lines. Each grid line
#' is a VMobject submobject, allowing individual animation. Grid lines
#' can optionally fade by distance from axes.
#'
#' @export
NumberPlane <- R6::R6Class(
  "NumberPlane",
  inherit = Axes,

  public = list(
    #' @field background_lines VGroup of background grid lines
    background_lines = NULL,

    #' @field faded_lines Number of fading levels (0 = no fading)
    faded_lines = 1,

    #' @field background_line_color Color for grid lines
    background_line_color = NULL,

    #' @field background_line_width Width of background grid lines
    background_line_width = 1,

    #' @field background_line_opacity Opacity of background grid lines
    background_line_opacity = 0.5,

    #' @description Create a NumberPlane
    #' @param x_range c(min, max, step)
    #' @param y_range c(min, max, step)
    #' @param x_length Width in scene units
    #' @param y_length Height in scene units
    #' @param background_line_color Color for grid lines
    #' @param background_line_width Width of grid lines
    #' @param background_line_opacity Opacity of grid lines
    #' @param faded_lines Number of fading sub-divisions (0=none)
    #' @param ... Additional args passed to Axes
    initialize = function(x_range = c(-10, 10, 1),
                          y_range = c(-6, 6, 1),
                          x_length = 10 * FRAME_X_RADIUS / 5,
                          y_length = 6 * FRAME_Y_RADIUS / 3,
                          background_line_color = BLUE_D,
                          background_line_width = 1,
                          background_line_opacity = 0.5,
                          faded_lines = 1,
                          ...) {
      self$background_line_color <- background_line_color
      self$background_line_width <- background_line_width
      self$background_line_opacity <- background_line_opacity
      self$faded_lines <- faded_lines

      super$initialize(
        x_range = x_range,
        y_range = y_range,
        x_length = x_length,
        y_length = y_length,
        ...
      )

      self$type <- "number_plane"
      private$add_grid_lines()
    },

    #' @description Get all background grid lines as a VGroup
    get_background_lines = function() {
      self$background_lines
    },

    #' @description Prepare the plane for a phase portrait or vector field display
    #' @param line_opacity Opacity for grid lines
    prepare_for_field = function(line_opacity = 0.3) {
      if (!is.null(self$background_lines)) {
        for (line in self$background_lines$submobjects) {
          line$stroke_opacity <- line_opacity
        }
      }
      invisible(self)
    }
  ),

  private = list(
    add_grid_lines = function() {
      self$background_lines <- VGroup$new()

      x_step <- self$x_range[3]
      y_step <- self$y_range[3]

      # Compute sub-steps for fading
      if (self$faded_lines > 0) {
        x_sub_step <- x_step / (self$faded_lines + 1)
        y_sub_step <- y_step / (self$faded_lines + 1)
      } else {
        x_sub_step <- x_step
        y_sub_step <- y_step
      }

      y_min_scene <- self$c2p(0, self$y_range[1])[2]
      y_max_scene <- self$c2p(0, self$y_range[2])[2]
      x_min_scene <- self$c2p(self$x_range[1], 0)[1]
      x_max_scene <- self$c2p(self$x_range[2], 0)[1]

      # Vertical grid lines
      x_vals <- seq(self$x_range[1], self$x_range[2], by = x_sub_step)
      for (x in x_vals) {
        pt <- self$c2p(x, 0)
        is_main <- abs(x %% x_step) < 1e-6 || abs(x %% x_step - x_step) < 1e-6

        opacity <- if (is_main) self$background_line_opacity
                   else self$background_line_opacity * 0.4

        if (abs(x) < 1e-6) next  # Skip axis line

        line <- Line$new(
          start = c(pt[1], y_min_scene, 0),
          end = c(pt[1], y_max_scene, 0),
          color = self$background_line_color,
          stroke_width = self$background_line_width
        )
        line$stroke_opacity <- opacity
        line$z_index <- -2
        self$background_lines$add(line)
      }

      # Horizontal grid lines
      y_vals <- seq(self$y_range[1], self$y_range[2], by = y_sub_step)
      for (y in y_vals) {
        pt <- self$c2p(0, y)
        is_main <- abs(y %% y_step) < 1e-6 || abs(y %% y_step - y_step) < 1e-6

        opacity <- if (is_main) self$background_line_opacity
                   else self$background_line_opacity * 0.4

        if (abs(y) < 1e-6) next

        line <- Line$new(
          start = c(x_min_scene, pt[2], 0),
          end = c(x_max_scene, pt[2], 0),
          color = self$background_line_color,
          stroke_width = self$background_line_width
        )
        line$stroke_opacity <- opacity
        line$z_index <- -2
        self$background_lines$add(line)
      }

      # Add background lines as submobjects (rendered behind axes)
      for (line in self$background_lines$submobjects) {
        self$add_submobject(line)
      }
    }
  )
)
