#' @title Coordinate System Classes
#' @description
#' Full-featured coordinate systems: NumberLine, Axes, ThreeDAxes.
#' Includes tick marks, tick labels, axis labels, arrow tips,
#' coordinate-to-point mapping, and function graphing.
#' @name axes
#' @keywords internal
NULL

#' NumberLine R6 Class
#'
#' @description
#' A 1D number line with tick marks, optional tick labels, and tips.
#' Maps numeric values to scene coordinates via n2p (number to point).
#'
#' @export
NumberLine <- R6::R6Class(
  "NumberLine",
  inherit = VMobject,

  public = list(
    #' @field x_range c(min, max, step)
    x_range = NULL,

    #' @field length Length in scene units
    length = 10,

    #' @field include_ticks Whether to include tick marks
    include_ticks = TRUE,

    #' @field tick_size Height of tick marks
    tick_size = 0.1,

    #' @field include_numbers Whether to include number labels
    include_numbers = FALSE,

    #' @field numbers_to_include Specific numbers to label (NULL = auto from step)
    numbers_to_include = NULL,

    #' @field include_tip Whether to include arrow tip at max end
    include_tip = FALSE,

    #' @field font_size Font size for number labels
    font_size = 24,

    #' @field label_direction Direction to place labels relative to line
    label_direction = NULL,

    #' @field tick_marks List of tick mark VMobjects
    tick_marks = NULL,

    #' @field number_labels List of number label mobjects
    number_labels = NULL,

    #' @description Create a NumberLine
    #' @param x_range c(min, max, step)
    #' @param length Line length in scene units
    #' @param color Line color
    #' @param include_ticks Include tick marks
    #' @param tick_size Tick mark height
    #' @param include_numbers Include number labels
    #' @param numbers_to_include Specific numbers to label
    #' @param include_tip Include arrow tip
    #' @param font_size Label font size
    #' @param label_direction Direction for labels (default DOWN)
    #' @param ... Additional args
    initialize = function(x_range = c(-5, 5, 1),
                          length = 10,
                          color = WHITE,
                          include_ticks = TRUE,
                          tick_size = 0.1,
                          include_numbers = FALSE,
                          numbers_to_include = NULL,
                          include_tip = FALSE,
                          font_size = 24,
                          label_direction = NULL,
                          ...) {
      super$initialize(color = color, ...)
      self$x_range <- x_range
      self$length <- length
      self$include_ticks <- include_ticks
      self$tick_size <- tick_size
      self$include_numbers <- include_numbers
      self$numbers_to_include <- numbers_to_include
      self$include_tip <- include_tip
      self$font_size <- font_size
      self$label_direction <- label_direction %||% DOWN
      self$type <- "number_line"

      private$build_line()
    },

    #' @description Number to Point: map a numeric value to scene coordinates
    #' @param value Numeric value
    #' @return Scene coordinate c(x, y, z)
    n2p = function(value) {
      x_min <- self$x_range[1]
      x_max <- self$x_range[2]
      alpha <- (value - x_min) / (x_max - x_min)
      x_scene <- (alpha - 0.5) * self$length + self$position[1]
      c(x_scene, self$position[2], self$position[3])
    },

    #' @description Point to Number: inverse of n2p
    #' @param point Scene coordinate c(x, y, z)
    #' @return Numeric value
    p2n = function(point) {
      x_min <- self$x_range[1]
      x_max <- self$x_range[2]
      alpha <- (point[1] - self$position[1]) / self$length + 0.5
      x_min + alpha * (x_max - x_min)
    },

    #' @description Get tick mark at a specific value
    #' @param value The value
    get_tick = function(value) {
      pt <- self$n2p(value)
      tick <- Line$new(
        start = pt + c(0, -self$tick_size, 0),
        end = pt + c(0, self$tick_size, 0),
        color = self$color,
        stroke_width = self$stroke_width
      )
      tick
    },

    #' @description Add a specific label at a value
    #' @param value Numeric value
    #' @param label Text label (defaults to formatted value)
    #' @param direction Direction offset from tick
    add_label = function(value, label = NULL, direction = NULL) {
      if (is.null(label)) label <- private$format_number(value)
      if (is.null(direction)) direction <- self$label_direction

      pt <- self$n2p(value)
      txt <- Text$new(
        text = label,
        font_size = self$font_size,
        color = self$color,
        position = pt + direction * (self$tick_size + 0.15)
      )
      self$add_submobject(txt)
      txt
    }
  ),

  private = list(
    build_line = function() {
      # Main line
      start <- c(-self$length / 2 + self$position[1], self$position[2], self$position[3])
      end_pt <- c(self$length / 2 + self$position[1], self$position[2], self$position[3])
      self$add_line(start, end_pt)

      # Ticks
      if (self$include_ticks) {
        self$tick_marks <- list()
        vals <- seq(self$x_range[1], self$x_range[2], by = self$x_range[3])
        for (v in vals) {
          pt <- self$n2p(v)
          tick_start <- pt + c(0, -self$tick_size, 0)
          tick_end <- pt + c(0, self$tick_size, 0)
          self$add_line(tick_start, tick_end)
        }
      }

      # Number labels
      if (self$include_numbers) {
        self$number_labels <- list()
        nums <- if (!is.null(self$numbers_to_include)) {
          self$numbers_to_include
        } else {
          seq(self$x_range[1], self$x_range[2], by = self$x_range[3])
        }

        for (v in nums) {
          self$add_label(v)
        }
      }

      # Arrow tip
      if (self$include_tip) {
        end_pt <- c(self$length / 2 + self$position[1], self$position[2], self$position[3])
        tip <- ArrowTip$new(
          position = end_pt,
          angle = 0,
          length = DEFAULT_ARROW_TIP_LENGTH * 0.6,
          color = self$color
        )
        self$add_submobject(tip)
      }
    },

    format_number = function(x) {
      if (x == floor(x)) format(as.integer(x))
      else format(round(x, 2))
    }
  )
)


#' Axes R6 Class
#'
#' @description
#' A 2D coordinate system with x and y axes. Supports tick marks,
#' axis labels, function plotting, area shading, and coordinate mapping.
#'
#' @export
Axes <- R6::R6Class(
  "Axes",
  inherit = VMobject,

  public = list(
    #' @field x_range c(min, max, step) for x-axis
    x_range = NULL,

    #' @field y_range c(min, max, step) for y-axis
    y_range = NULL,

    #' @field x_length Width in scene units
    x_length = NULL,

    #' @field y_length Height in scene units
    y_length = NULL,

    #' @field x_axis The x-axis NumberLine
    x_axis = NULL,

    #' @field y_axis The y-axis NumberLine
    y_axis = NULL,

    #' @field include_ticks Include tick marks
    include_ticks = TRUE,

    #' @field include_numbers Include number labels
    include_numbers = FALSE,

    #' @field tips Include arrow tips on axes
    tips = TRUE,

    #' @field axis_config Config list for both axes
    axis_config = NULL,

    #' @description Create Axes
    #' @param x_range c(min, max, step)
    #' @param y_range c(min, max, step)
    #' @param x_length Width in scene units
    #' @param y_length Height in scene units
    #' @param tips Include arrow tips
    #' @param include_ticks Include tick marks
    #' @param include_numbers Include number labels
    #' @param axis_config Additional axis configuration
    #' @param color Axis color
    #' @param ... Additional args
    initialize = function(x_range = c(-5, 5, 1),
                          y_range = c(-3, 3, 1),
                          x_length = 10,
                          y_length = 6,
                          tips = TRUE,
                          include_ticks = TRUE,
                          include_numbers = FALSE,
                          axis_config = NULL,
                          color = WHITE,
                          ...) {
      super$initialize(color = color, ...)
      self$x_range <- x_range
      self$y_range <- y_range
      self$x_length <- x_length
      self$y_length <- y_length
      self$tips <- tips
      self$include_ticks <- include_ticks
      self$include_numbers <- include_numbers
      self$axis_config <- axis_config %||% list()
      self$type <- "axes"

      private$build_axes()
    },

    #' @description Coordinate to Point: map data coordinates to scene
    #' @param x Data x value
    #' @param y Data y value
    #' @param z Data z value (default 0)
    #' @return Scene coordinate c(x, y, z)
    c2p = function(x, y, z = 0) {
      # Map x
      x_min <- self$x_range[1]; x_max <- self$x_range[2]
      x_alpha <- (x - x_min) / (x_max - x_min)
      x_scene <- (x_alpha - 0.5) * self$x_length + self$position[1]

      # Map y
      y_min <- self$y_range[1]; y_max <- self$y_range[2]
      y_alpha <- (y - y_min) / (y_max - y_min)
      y_scene <- (y_alpha - 0.5) * self$y_length + self$position[2]

      c(x_scene, y_scene, z)
    },

    #' @description Point to Coordinate: inverse of c2p
    #' @param point Scene coordinate
    #' @return c(x_data, y_data)
    p2c = function(point) {
      x_alpha <- (point[1] - self$position[1]) / self$x_length + 0.5
      y_alpha <- (point[2] - self$position[2]) / self$y_length + 0.5

      x_data <- self$x_range[1] + x_alpha * (self$x_range[2] - self$x_range[1])
      y_data <- self$y_range[1] + y_alpha * (self$y_range[2] - self$y_range[1])

      c(x_data, y_data)
    },

    #' @description Alias for c2p (coords_to_point)
    coords_to_point = function(x, y, z = 0) self$c2p(x, y, z),

    #' @description Alias for p2c (point_to_coords)
    point_to_coords = function(point) self$p2c(point),

    #' @description Plot a function y = f(x) as a VMobject curve
    #' @param func Function(x) -> y
    #' @param x_range Optional c(min, max) to override axis range
    #' @param color Curve color
    #' @param stroke_width Stroke width
    #' @param num_points Number of sample points
    #' @return VMobject representing the graph
    plot = function(func,
                    x_range = NULL,
                    color = YELLOW,
                    stroke_width = DEFAULT_STROKE_WIDTH,
                    num_points = 200) {
      xr <- x_range %||% self$x_range[1:2]
      x_vals <- seq(xr[1], xr[2], length.out = num_points)
      y_vals <- vapply(x_vals, func, numeric(1))

      # Clamp y values to axis range for display
      y_min <- self$y_range[1]; y_max <- self$y_range[2]

      graph <- VMobject$new(color = color, stroke_width = stroke_width)
      graph$type <- "graph_curve"

      # Convert to scene coordinates
      scene_pts <- matrix(0, nrow = num_points, ncol = 3)
      for (i in seq_along(x_vals)) {
        y_clamped <- max(y_min, min(y_max, y_vals[i]))
        scene_pts[i, ] <- self$c2p(x_vals[i], y_clamped)
      }

      # Fit Bezier curves through points (groups of 4)
      pts <- matrix(numeric(0), ncol = 3)
      i <- 1
      while (i + 3 <= num_points) {
        pts <- rbind(pts,
          scene_pts[i, ],
          scene_pts[i + 1, ],
          scene_pts[i + 2, ],
          scene_pts[i + 3, ]
        )
        i <- i + 3
      }
      if (i <= num_points) {
        p0 <- scene_pts[i, ]
        p3 <- scene_pts[num_points, ]
        p1 <- p0 + (p3 - p0) / 3
        p2 <- p0 + 2 * (p3 - p0) / 3
        pts <- rbind(pts, p0, p1, p2, p3)
      }

      graph$points <- pts
      graph$data <- list(func = func, x_range = xr,
                         axes = self, y_values = y_vals)
      graph
    },

    #' @description Alias for plot (Manim compatibility)
    get_graph = function(func, ...) self$plot(func, ...),

    #' @description Get the area between a graph and the x-axis
    #' @param graph A graph VMobject from plot()
    #' @param x_range c(min, max) for the shaded region
    #' @param color Fill color
    #' @param fill_opacity Fill opacity
    #' @return VMobject representing the shaded area
    get_area = function(graph,
                        x_range = NULL,
                        color = BLUE,
                        fill_opacity = 0.5) {
      xr <- x_range %||% graph$data$x_range
      func <- graph$data$func

      n <- 100
      x_vals <- seq(xr[1], xr[2], length.out = n)

      area <- VMobject$new(
        fill_color = color,
        fill_opacity = fill_opacity,
        stroke_width = 0
      )

      # Build polygon: top curve + bottom line along x-axis
      top_pts <- matrix(0, nrow = n, ncol = 3)
      for (i in seq_along(x_vals)) {
        y <- func(x_vals[i])
        y <- max(self$y_range[1], min(self$y_range[2], y))
        top_pts[i, ] <- self$c2p(x_vals[i], y)
      }

      # Bottom line (along x=0 or bottom of range)
      bottom_y <- max(0, self$y_range[1])
      bottom_left <- self$c2p(xr[1], bottom_y)
      bottom_right <- self$c2p(xr[2], bottom_y)

      # Build Bezier approximation of the area polygon
      pts <- matrix(numeric(0), ncol = 3)

      # Top curve
      i <- 1
      while (i + 3 <= n) {
        pts <- rbind(pts, top_pts[i,], top_pts[i+1,], top_pts[i+2,], top_pts[i+3,])
        i <- i + 3
      }
      if (i <= n) {
        p0 <- top_pts[i,]
        p3 <- top_pts[n,]
        pts <- rbind(pts, p0, p0 + (p3-p0)/3, p0 + 2*(p3-p0)/3, p3)
      }

      # Right side down to bottom
      tr <- top_pts[n,]
      pts <- rbind(pts, tr, tr + (bottom_right-tr)/3,
                   tr + 2*(bottom_right-tr)/3, bottom_right)

      # Bottom line back to left
      pts <- rbind(pts, bottom_right, bottom_right + (bottom_left-bottom_right)/3,
                   bottom_right + 2*(bottom_left-bottom_right)/3, bottom_left)

      # Left side up to start
      tl <- top_pts[1,]
      pts <- rbind(pts, bottom_left, bottom_left + (tl-bottom_left)/3,
                   bottom_left + 2*(tl-bottom_left)/3, tl)

      area$points <- pts
      area
    },

    #' @description Plot a line graph from data vectors
    #' @param x_values Vector of x
    #' @param y_values Vector of y
    #' @param color Line color
    #' @param stroke_width Stroke width
    #' @return VMobject
    plot_line_graph = function(x_values, y_values,
                               color = YELLOW,
                               stroke_width = DEFAULT_STROKE_WIDTH) {
      vm <- VMobject$new(color = color, stroke_width = stroke_width)

      pts <- matrix(numeric(0), ncol = 3)
      scene_pts <- matrix(0, nrow = length(x_values), ncol = 3)
      for (i in seq_along(x_values)) {
        scene_pts[i, ] <- self$c2p(x_values[i], y_values[i])
      }

      i <- 1
      while (i + 3 <= nrow(scene_pts)) {
        pts <- rbind(pts, scene_pts[i,], scene_pts[i+1,],
                     scene_pts[i+2,], scene_pts[i+3,])
        i <- i + 3
      }
      if (i <= nrow(scene_pts)) {
        p0 <- scene_pts[i,]
        p3 <- scene_pts[nrow(scene_pts),]
        pts <- rbind(pts, p0, p0+(p3-p0)/3, p0+2*(p3-p0)/3, p3)
      }

      vm$points <- pts
      vm
    },

    #' @description Add axis labels
    #' @param x_label Label for x-axis
    #' @param y_label Label for y-axis
    #' @param font_size Label font size
    add_axis_labels = function(x_label = "x", y_label = "y", font_size = 36) {
      # X label at right end
      x_end <- self$c2p(self$x_range[2], 0)
      x_txt <- Text$new(
        text = x_label, font_size = font_size, color = self$color,
        position = x_end + c(0.3, -0.3, 0)
      )
      self$add_submobject(x_txt)

      # Y label at top end
      y_end <- self$c2p(0, self$y_range[2])
      y_txt <- Text$new(
        text = y_label, font_size = font_size, color = self$color,
        position = y_end + c(-0.3, 0.3, 0)
      )
      self$add_submobject(y_txt)

      invisible(self)
    },

    #' @description Get the origin point in scene coordinates
    get_origin = function() {
      self$c2p(0, 0)
    }
  ),

  private = list(
    build_axes = function() {
      # X Axis
      x_start <- self$c2p(self$x_range[1], 0)
      x_end <- self$c2p(self$x_range[2], 0)
      self$add_line(x_start, x_end)

      # Y Axis
      y_start <- self$c2p(0, self$y_range[1])
      y_end <- self$c2p(0, self$y_range[2])
      self$add_line(y_start, y_end)

      # Ticks
      if (self$include_ticks) {
        # X ticks
        x_vals <- seq(self$x_range[1], self$x_range[2], by = self$x_range[3])
        for (v in x_vals) {
          if (v == 0) next
          pt <- self$c2p(v, 0)
          self$add_line(pt + c(0, -0.1, 0), pt + c(0, 0.1, 0))
        }

        # Y ticks
        y_vals <- seq(self$y_range[1], self$y_range[2], by = self$y_range[3])
        for (v in y_vals) {
          if (v == 0) next
          pt <- self$c2p(0, v)
          self$add_line(pt + c(-0.1, 0, 0), pt + c(0.1, 0, 0))
        }
      }

      # Number labels
      if (self$include_numbers) {
        x_vals <- seq(self$x_range[1], self$x_range[2], by = self$x_range[3])
        for (v in x_vals) {
          if (v == 0) next
          pt <- self$c2p(v, 0)
          label <- if (v == floor(v)) as.character(as.integer(v))
                   else as.character(round(v, 2))
          txt <- Text$new(
            text = label, font_size = 20, color = self$color,
            position = pt + c(0, -0.3, 0)
          )
          self$add_submobject(txt)
        }

        y_vals <- seq(self$y_range[1], self$y_range[2], by = self$y_range[3])
        for (v in y_vals) {
          if (v == 0) next
          pt <- self$c2p(0, v)
          label <- if (v == floor(v)) as.character(as.integer(v))
                   else as.character(round(v, 2))
          txt <- Text$new(
            text = label, font_size = 20, color = self$color,
            position = pt + c(-0.3, 0, 0)
          )
          self$add_submobject(txt)
        }
      }

      # Arrow tips
      if (self$tips) {
        x_tip <- ArrowTip$new(
          position = self$c2p(self$x_range[2], 0),
          angle = 0,
          length = DEFAULT_ARROW_TIP_LENGTH * 0.6,
          color = self$color
        )
        self$add_submobject(x_tip)

        y_tip <- ArrowTip$new(
          position = self$c2p(0, self$y_range[2]),
          angle = PI / 2,
          length = DEFAULT_ARROW_TIP_LENGTH * 0.6,
          color = self$color
        )
        self$add_submobject(y_tip)
      }
    }
  )
)


#' ThreeDAxes R6 Class
#'
#' @description
#' A 3D coordinate system with x, y, and z axes. Extends Axes with
#' a z-axis and 3D coordinate mapping.
#'
#' @export
ThreeDAxes <- R6::R6Class(
  "ThreeDAxes",
  inherit = Axes,

  public = list(
    #' @field z_range c(min, max, step) for z-axis
    z_range = NULL,

    #' @field z_length Length of z-axis in scene units
    z_length = NULL,

    #' @description Create ThreeDAxes
    #' @param x_range c(min, max, step)
    #' @param y_range c(min, max, step)
    #' @param z_range c(min, max, step)
    #' @param x_length X axis length
    #' @param y_length Y axis length
    #' @param z_length Z axis length
    #' @param ... Additional args
    initialize = function(x_range = c(-5, 5, 1),
                          y_range = c(-5, 5, 1),
                          z_range = c(-3, 3, 1),
                          x_length = 10,
                          y_length = 10,
                          z_length = 6,
                          ...) {
      self$z_range <- z_range
      self$z_length <- z_length

      super$initialize(
        x_range = x_range,
        y_range = y_range,
        x_length = x_length,
        y_length = y_length,
        ...
      )

      # Add Z axis
      z_start <- self$c2p(0, 0) + c(0, 0, -z_length / 2)
      z_end <- self$c2p(0, 0) + c(0, 0, z_length / 2)
      self$add_line(z_start, z_end)

      # Z ticks
      if (self$include_ticks) {
        z_vals <- seq(z_range[1], z_range[2], by = z_range[3])
        origin <- self$c2p(0, 0)
        for (v in z_vals) {
          if (v == 0) next
          z_alpha <- (v - z_range[1]) / (z_range[2] - z_range[1])
          z_pos <- -z_length / 2 + z_alpha * z_length
          pt <- origin + c(0, 0, z_pos)
          self$add_line(pt + c(-0.1, 0, 0), pt + c(0.1, 0, 0))
        }
      }

      # Z tip
      if (self$tips) {
        z_tip_pos <- self$c2p(0, 0) + c(0, 0, z_length / 2)
        # Store metadata for renderer (tip along z-axis)
        z_tip <- VMobject$new(color = self$color)
        z_tip$data <- list(type = "arrow_tip_3d", direction = c(0, 0, 1))
        z_tip$position <- z_tip_pos
        self$add_submobject(z_tip)
      }

      self$type <- "3d_axes"
    },

    #' @description 3D coordinate to point mapping
    #' @param x Data x
    #' @param y Data y
    #' @param z Data z
    c2p = function(x, y, z = 0) {
      pt <- super$c2p(x, y)

      if (!is.null(self$z_range) && !is.null(self$z_length)) {
        z_alpha <- (z - self$z_range[1]) / (self$z_range[2] - self$z_range[1])
        z_scene <- (z_alpha - 0.5) * self$z_length
        pt[3] <- z_scene
      }

      pt
    }
  )
)


#' PolarPlane R6 Class
#'
#' @description
#' A polar coordinate system with radial and angular grid lines.
#'
#' @export
PolarPlane <- R6::R6Class(
  "PolarPlane",
  inherit = VMobject,

  public = list(
    #' @field radius_max Maximum radius
    radius_max = 3,

    #' @field radius_step Step between radial grid lines
    radius_step = 1,

    #' @field azimuth_step Angular step in radians for grid lines
    azimuth_step = NULL,

    #' @field size Scene size of the full plot
    size = 6,

    #' @description Create a PolarPlane
    #' @param radius_max Maximum radius value
    #' @param radius_step Step for radial circles
    #' @param azimuth_step Angular step (default PI/6 = 30 degrees)
    #' @param size Diameter in scene units
    #' @param color Grid color
    #' @param ... Additional args
    initialize = function(radius_max = 3,
                          radius_step = 1,
                          azimuth_step = PI / 6,
                          size = 6,
                          color = GREY_B,
                          ...) {
      super$initialize(color = color, stroke_width = 1, ...)
      self$radius_max <- radius_max
      self$radius_step <- radius_step
      self$azimuth_step <- azimuth_step
      self$size <- size
      self$type <- "polar_plane"

      private$build_grid()
    },

    #' @description Polar to Point: (r, theta) -> scene (x, y, z)
    #' @param r Radius
    #' @param theta Angle in radians
    pr2p = function(r, theta) {
      scale <- self$size / (2 * self$radius_max)
      c(r * cos(theta) * scale + self$position[1],
        r * sin(theta) * scale + self$position[2],
        self$position[3])
    },

    #' @description Point to Polar: scene (x, y) -> (r, theta)
    #' @param point Scene coordinate
    p2pr = function(point) {
      scale <- self$size / (2 * self$radius_max)
      x <- (point[1] - self$position[1]) / scale
      y <- (point[2] - self$position[2]) / scale
      r <- sqrt(x^2 + y^2)
      theta <- atan2(y, x)
      c(r, theta)
    }
  ),

  private = list(
    build_grid = function() {
      scale <- self$size / (2 * self$radius_max)

      # Radial circles
      r_vals <- seq(self$radius_step, self$radius_max, by = self$radius_step)
      for (r in r_vals) {
        circle <- Arc$new(
          radius = r * scale,
          start_angle = 0,
          angle = TAU,
          color = self$color,
          stroke_width = 1,
          position = self$position
        )
        self$add_submobject(circle)
      }

      # Angular lines
      if (!is.null(self$azimuth_step)) {
        angles <- seq(0, TAU - self$azimuth_step / 2, by = self$azimuth_step)
        for (a in angles) {
          end_pt <- self$pr2p(self$radius_max, a)
          line <- Line$new(
            start = self$position,
            end = end_pt,
            color = self$color,
            stroke_width = 1
          )
          self$add_submobject(line)
        }
      }
    }
  )
)


#' ComplexPlane R6 Class
#'
#' @description
#' A coordinate plane for complex numbers. Extends Axes with labels
#' in a+bi format and complex number operations.
#'
#' @export
ComplexPlane <- R6::R6Class(
  "ComplexPlane",
  inherit = Axes,

  public = list(
    #' @description Create a ComplexPlane
    #' @param x_range Real axis range
    #' @param y_range Imaginary axis range
    #' @param ... Additional args
    initialize = function(x_range = c(-5, 5, 1),
                          y_range = c(-3, 3, 1),
                          ...) {
      super$initialize(
        x_range = x_range,
        y_range = y_range,
        include_numbers = FALSE,
        ...
      )
      self$type <- "complex_plane"

      # Add custom labels in a+bi format
      private$add_complex_labels()
    },

    #' @description Convert complex number to scene point
    #' @param z Complex number
    n2p = function(z) {
      self$c2p(Re(z), Im(z))
    },

    #' @description Convert scene point to complex number
    #' @param point Scene coordinate
    p2n = function(point) {
      coords <- self$p2c(point)
      complex(real = coords[1], imaginary = coords[2])
    },

    #' @description Plot a complex function f(z) as a transformation
    #' @param func Function(complex) -> complex
    #' @param domain_range c(re_min, re_max, im_min, im_max)
    #' @param num_lines Number of grid lines in each direction
    #' @return VGroup of transformed grid lines
    plot_complex_function = function(func,
                                     domain_range = NULL,
                                     num_lines = 10) {
      dr <- domain_range %||% c(self$x_range[1], self$x_range[2],
                                self$y_range[1], self$y_range[2])

      grp <- VGroup$new()
      n_pts <- 50

      # Horizontal lines (constant Im)
      im_vals <- seq(dr[3], dr[4], length.out = num_lines)
      for (im in im_vals) {
        curve <- VMobject$new(color = BLUE, stroke_width = 2)
        re_vals <- seq(dr[1], dr[2], length.out = n_pts)
        scene_pts <- matrix(0, nrow = n_pts, ncol = 3)
        for (i in seq_along(re_vals)) {
          z_in <- complex(real = re_vals[i], imaginary = im)
          z_out <- func(z_in)
          scene_pts[i, ] <- self$c2p(Re(z_out), Im(z_out))
        }
        # Bezier fit
        pts <- matrix(numeric(0), ncol = 3)
        j <- 1
        while (j + 3 <= n_pts) {
          pts <- rbind(pts, scene_pts[j,], scene_pts[j+1,],
                       scene_pts[j+2,], scene_pts[j+3,])
          j <- j + 3
        }
        curve$points <- pts
        grp$add(curve)
      }

      # Vertical lines (constant Re)
      re_vals <- seq(dr[1], dr[2], length.out = num_lines)
      for (re in re_vals) {
        curve <- VMobject$new(color = YELLOW, stroke_width = 2)
        im_vals2 <- seq(dr[3], dr[4], length.out = n_pts)
        scene_pts <- matrix(0, nrow = n_pts, ncol = 3)
        for (i in seq_along(im_vals2)) {
          z_in <- complex(real = re, imaginary = im_vals2[i])
          z_out <- func(z_in)
          scene_pts[i, ] <- self$c2p(Re(z_out), Im(z_out))
        }
        pts <- matrix(numeric(0), ncol = 3)
        j <- 1
        while (j + 3 <= n_pts) {
          pts <- rbind(pts, scene_pts[j,], scene_pts[j+1,],
                       scene_pts[j+2,], scene_pts[j+3,])
          j <- j + 3
        }
        curve$points <- pts
        grp$add(curve)
      }

      grp
    }
  ),

  private = list(
    add_complex_labels = function() {
      # Real axis labels
      x_vals <- seq(self$x_range[1], self$x_range[2], by = self$x_range[3])
      for (v in x_vals) {
        if (v == 0) next
        pt <- self$c2p(v, 0)
        label <- if (v == floor(v)) as.character(as.integer(v))
                 else as.character(round(v, 2))
        txt <- Text$new(
          text = label, font_size = 18, color = self$color,
          position = pt + c(0, -0.3, 0)
        )
        self$add_submobject(txt)
      }

      # Imaginary axis labels
      y_vals <- seq(self$y_range[1], self$y_range[2], by = self$y_range[3])
      for (v in y_vals) {
        if (v == 0) next
        label <- if (v == 1) "i"
                 else if (v == -1) "-i"
                 else paste0(if (v == floor(v)) as.integer(v) else round(v, 2), "i")
        pt <- self$c2p(0, v)
        txt <- Text$new(
          text = label, font_size = 18, color = self$color,
          position = pt + c(-0.4, 0, 0)
        )
        self$add_submobject(txt)
      }
    }
  )
)
