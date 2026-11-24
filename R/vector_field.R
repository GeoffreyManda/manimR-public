#' @title Vector Field Classes
#' @description
#' Arrow-based and stream-line-based vector field visualization.
#' ArrowVectorField places arrows on a grid; StreamLines traces
#' particle paths through a velocity field.
#' @name vector_field
#' @keywords internal
NULL

#' ArrowVectorField R6 Class
#'
#' @description
#' A grid of arrows representing a 2D vector field. Each arrow is placed
#' at a grid point and oriented/scaled according to the vector function
#' f(x, y) -> c(vx, vy).
#'
#' @export
ArrowVectorField <- R6::R6Class(
  "ArrowVectorField",
  inherit = VMobject,

  public = list(
    #' @field func Vector field function f(x, y) -> c(vx, vy)
    func = NULL,

    #' @field x_range c(min, max, step) for sampling grid
    x_range = NULL,

    #' @field y_range c(min, max, step) for sampling grid
    y_range = NULL,

    #' @field length_func Function mapping vector magnitude to arrow length
    length_func = NULL,

    #' @field max_length Maximum arrow length in scene units
    max_length = 0.5,

    #' @field color_scheme Color scheme ("magnitude", "angle", or single color)
    color_scheme = "magnitude",

    #' @field colors Gradient colors for coloring
    colors = NULL,

    #' @field arrows List of Arrow submobjects
    arrows = NULL,

    #' @description Create an ArrowVectorField
    #' @param func Function(x, y) returning c(vx, vy)
    #' @param x_range c(min, max, step) grid range
    #' @param y_range c(min, max, step) grid range
    #' @param max_length Maximum arrow length
    #' @param color_scheme "magnitude", "angle", or a single hex color
    #' @param colors Gradient colors (for magnitude/angle scheme)
    #' @param opacity Arrow opacity
    #' @param stroke_width Arrow stroke width
    #' @param ... Additional args
    initialize = function(func,
                          x_range = c(-5, 5, 0.5),
                          y_range = c(-3, 3, 0.5),
                          max_length = 0.5,
                          color_scheme = "magnitude",
                          colors = c(BLUE, GREEN, YELLOW, RED),
                          opacity = 1,
                          stroke_width = 2,
                          ...) {
      super$initialize(opacity = opacity, stroke_width = stroke_width, ...)
      self$func <- func
      self$x_range <- x_range
      self$y_range <- y_range
      self$max_length <- max_length
      self$color_scheme <- color_scheme
      self$colors <- colors
      self$type <- "arrow_vector_field"

      private$build_field()
    },

    #' @description Animate the field by nudging points along the flow
    #' @param dt Time step
    #' @return Self (for chaining/updater use)
    nudge = function(dt = 0.05) {
      for (arrow in self$arrows) {
        pos <- arrow$position
        vel <- self$func(pos[1], pos[2])
        new_pos <- pos + c(vel, 0) * dt
        arrow$move_to(new_pos)
      }
      invisible(self)
    },

    #' @description Get the flow at a specific point
    #' @param x X coordinate
    #' @param y Y coordinate
    #' @return c(vx, vy) velocity vector
    get_vector = function(x, y) {
      self$func(x, y)
    }
  ),

  private = list(
    build_field = function() {
      self$arrows <- list()

      x_vals <- seq(self$x_range[1], self$x_range[2], by = self$x_range[3])
      y_vals <- seq(self$y_range[1], self$y_range[2], by = self$y_range[3])

      # First pass: compute all magnitudes for normalization
      all_mags <- numeric(0)
      all_angles <- numeric(0)
      grid_data <- list()

      for (x in x_vals) {
        for (y in y_vals) {
          v <- self$func(x, y)
          mag <- sqrt(sum(v^2))
          ang <- atan2(v[2], v[1])
          all_mags <- c(all_mags, mag)
          all_angles <- c(all_angles, ang)
          grid_data[[length(grid_data) + 1]] <- list(
            x = x, y = y, vx = v[1], vy = v[2], mag = mag, angle = ang
          )
        }
      }

      max_mag <- max(all_mags, 1e-8)
      grad <- color_gradient(self$colors, 256)

      # Second pass: create arrows
      for (gd in grid_data) {
        if (gd$mag < 1e-8) next  # Skip zero vectors

        # Arrow length proportional to magnitude
        arrow_length <- (gd$mag / max_mag) * self$max_length
        arrow_length <- max(arrow_length, 0.05)  # minimum visible size

        # Arrow direction
        dir_x <- gd$vx / gd$mag
        dir_y <- gd$vy / gd$mag

        start <- c(gd$x, gd$y, 0)
        end_pt <- c(gd$x + dir_x * arrow_length,
                    gd$y + dir_y * arrow_length, 0)

        # Color
        if (self$color_scheme == "magnitude") {
          t <- gd$mag / max_mag
          idx <- max(1, ceiling(t * 256))
          col <- grad[idx]
        } else if (self$color_scheme == "angle") {
          t <- (gd$angle + PI) / TAU
          idx <- max(1, ceiling(t * 256))
          col <- grad[idx]
        } else {
          col <- self$color_scheme
        }

        arrow <- Vector$new(
          direction = end_pt - start,
          color = col,
          stroke_width = self$stroke_width
        )
        arrow$shift(start)
        arrow$position <- start

        self$arrows[[length(self$arrows) + 1]] <- arrow
        self$add_submobject(arrow)
      }
    }
  )
)


#' StreamLines R6 Class
#'
#' @description
#' Visualizes a vector field using stream lines (particle traces).
#' Particles are seeded at random or grid positions and traced through
#' the velocity field using Euler integration.
#'
#' @export
StreamLines <- R6::R6Class(
  "StreamLines",
  inherit = VMobject,

  public = list(
    #' @field func Vector field function f(x, y) -> c(vx, vy)
    func = NULL,

    #' @field x_range x range for seeding
    x_range = NULL,

    #' @field y_range y range for seeding
    y_range = NULL,

    #' @field n_lines Number of stream lines
    n_lines = 50,

    #' @field dt Integration time step
    dt = 0.05,

    #' @field max_steps Maximum integration steps per line
    max_steps = 100,

    #' @field stroke_width Line stroke width
    stroke_width_val = 2,

    #' @field lines List of stream line VMobjects
    lines = NULL,

    #' @description Create StreamLines
    #' @param func Function(x, y) -> c(vx, vy)
    #' @param x_range c(min, max)
    #' @param y_range c(min, max)
    #' @param n_lines Number of lines
    #' @param dt Integration step
    #' @param max_steps Max steps per line
    #' @param color Line color or color gradient
    #' @param stroke_width Stroke width
    #' @param ... Additional args
    initialize = function(func,
                          x_range = c(-5, 5),
                          y_range = c(-3, 3),
                          n_lines = 50,
                          dt = 0.05,
                          max_steps = 100,
                          color = BLUE,
                          stroke_width = 2,
                          ...) {
      super$initialize(color = color, stroke_width = stroke_width, ...)
      self$func <- func
      self$x_range <- x_range
      self$y_range <- y_range
      self$n_lines <- n_lines
      self$dt <- dt
      self$max_steps <- max_steps
      self$stroke_width_val <- stroke_width
      self$type <- "stream_lines"

      private$build_streams()
    },

    #' @description Start the streaming animation
    #' @param scene The scene to animate in
    #' @param run_time Animation duration
    start_animation = function(scene = NULL, run_time = 3) {
      # Return animation specs for progressive reveal
      lapply(self$lines, function(line) {
        Create$new(line, run_time = run_time / self$n_lines)
      })
    }
  ),

  private = list(
    build_streams = function() {
      self$lines <- list()

      # Seed points (stratified random sampling)
      seeds <- private$generate_seeds()

      for (si in seq_len(nrow(seeds))) {
        x <- seeds[si, 1]
        y <- seeds[si, 2]

        # Trace path via Euler integration
        path <- matrix(c(x, y), ncol = 2, nrow = 1)

        for (step in seq_len(self$max_steps)) {
          v <- self$func(x, y)
          if (any(is.na(v)) || any(is.infinite(v))) break
          mag <- sqrt(sum(v^2))
          if (mag < 1e-6) break  # Stagnation point

          x <- x + v[1] * self$dt
          y <- y + v[2] * self$dt

          # Check bounds
          if (x < self$x_range[1] || x > self$x_range[2] ||
              y < self$y_range[1] || y > self$y_range[2]) break

          path <- rbind(path, c(x, y))
        }

        if (nrow(path) < 2) next

        # Convert to Bezier VMobject
        line <- VMobject$new(
          color = self$color,
          stroke_width = self$stroke_width_val
        )

        scene_pts <- cbind(path, 0)  # add z=0
        pts <- matrix(numeric(0), ncol = 3)

        i <- 1
        while (i + 3 <= nrow(scene_pts)) {
          pts <- rbind(pts,
            scene_pts[i, ], scene_pts[i + 1, ],
            scene_pts[i + 2, ], scene_pts[i + 3, ]
          )
          i <- i + 3
        }
        if (i <= nrow(scene_pts)) {
          p0 <- scene_pts[i, ]
          p3 <- scene_pts[nrow(scene_pts), ]
          p1 <- p0 + (p3 - p0) / 3
          p2 <- p0 + 2 * (p3 - p0) / 3
          pts <- rbind(pts, p0, p1, p2, p3)
        }

        if (nrow(pts) > 0) {
          line$points <- pts
          self$lines[[length(self$lines) + 1]] <- line
          self$add_submobject(line)
        }
      }
    },

    generate_seeds = function() {
      n <- self$n_lines
      # Stratified: divide into grid cells and place one seed per cell
      cols <- ceiling(sqrt(n * (self$x_range[2] - self$x_range[1]) /
                             (self$y_range[2] - self$y_range[1])))
      rows <- ceiling(n / cols)

      dx <- (self$x_range[2] - self$x_range[1]) / cols
      dy <- (self$y_range[2] - self$y_range[1]) / rows

      seeds <- matrix(numeric(0), ncol = 2)
      count <- 0
      for (r in seq_len(rows)) {
        for (c_idx in seq_len(cols)) {
          if (count >= n) break
          x <- self$x_range[1] + (c_idx - 0.5 + stats::runif(1, -0.3, 0.3)) * dx
          y <- self$y_range[1] + (r - 0.5 + stats::runif(1, -0.3, 0.3)) * dy
          seeds <- rbind(seeds, c(x, y))
          count <- count + 1
        }
        if (count >= n) break
      }
      seeds
    }
  )
)
