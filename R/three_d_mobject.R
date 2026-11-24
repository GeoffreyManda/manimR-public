#' @title 3D Base Classes
#' @description
#' Foundation classes for three-dimensional objects in manimR.
#' ThreeDVMobject extends VMobject with 3D-specific rendering metadata.
#' Surface represents parametric UV-mapped meshes.
#' @name three_d_mobject
#' @keywords internal
NULL

#' ThreeDVMobject R6 Class
#'
#' @description
#' Base class for all 3D vector mobjects. Extends VMobject with:
#' - Shade/lighting metadata (ambient, diffuse, specular)
#' - Normal vectors for lighting calculations
#' - Depth sorting hints for the renderer
#'
#' @export
ThreeDVMobject <- R6::R6Class(
  "ThreeDVMobject",
  inherit = VMobject,

  public = list(
    #' @field shade Whether to apply lighting
    shade = TRUE,

    #' @field gloss Specular shininess (0-1)
    gloss = 0.2,

    #' @field shadow Shadow intensity (0-1)
    shadow = 0.4,

    #' @field ambient Ambient light factor (0-1)
    ambient = 0.3,

    #' @field normal Normal vector for flat surfaces
    normal = c(0, 0, 1),

    #' @description Create a ThreeDVMobject
    #' @param shade Apply shading
    #' @param gloss Specular gloss
    #' @param shadow Shadow intensity
    #' @param ... Passed to VMobject
    initialize = function(shade = TRUE, gloss = 0.2, shadow = 0.4, ...) {
      super$initialize(...)
      self$shade <- shade
      self$gloss <- gloss
      self$shadow <- shadow
      self$type <- "3d_vmobject"
    },

    #' @description Apply shading to a color based on normal and light direction
    #' @param base_color Hex color
    #' @param light_dir Light direction vector (x,y,z)
    #' @return Shaded hex color
    apply_shading = function(base_color, light_dir = c(1, 1, 2)) {
      if (!self$shade) return(base_color)

      light_dir <- light_dir / sqrt(sum(light_dir^2))
      n <- self$normal / sqrt(sum(self$normal^2))

      # Lambertian diffuse
      diffuse <- max(sum(n * light_dir), 0)

      # Combined intensity
      intensity <- self$ambient + (1 - self$ambient) * diffuse

      # Apply to color
      rgb_vals <- col2rgb(base_color) / 255
      shaded <- pmin(rgb_vals * intensity, 1)

      grDevices::rgb(shaded[1], shaded[2], shaded[3])
    },

    #' @description Get depth (average z) for depth sorting
    get_depth = function() {
      if (is.null(self$points) || nrow(self$points) == 0) {
        return(self$position[3])
      }
      mean(self$points[, 3]) + self$position[3]
    }
  )
)


#' Surface R6 Class
#'
#' @description
#' A parametric 3D surface defined by a function f(u, v) -> (x, y, z).
#' The surface is tessellated into a grid of quadrilateral patches, each
#' represented as VMobject submobjects for rendering.
#'
#' @details
#' Based on Manim's Surface class. The surface is created by evaluating
#' the parametric function on a regular (u, v) grid and creating a mesh
#' of flat quadrilateral patches. Each patch can be individually colored
#' using a color function.
#'
#' @export
Surface <- R6::R6Class(
  "Surface",
  inherit = ThreeDVMobject,

  public = list(
    #' @field func Parametric function f(u, v) -> c(x, y, z)
    func = NULL,

    #' @field u_range Range for u parameter c(min, max)
    u_range = NULL,

    #' @field v_range Range for v parameter c(min, max)
    v_range = NULL,

    #' @field u_resolution Number of subdivisions in u
    u_resolution = 32,

    #' @field v_resolution Number of subdivisions in v
    v_resolution = 32,

    #' @field color_func Optional function(x,y,z) -> hex color
    color_func = NULL,

    #' @field checkerboard_colors Two-color checkerboard pattern
    checkerboard_colors = NULL,

    #' @field mesh_lines Whether to draw mesh lines
    mesh_lines = FALSE,

    #' @description Create a parametric Surface
    #' @param func Function(u, v) returning c(x, y, z)
    #' @param u_range c(min, max) for u parameter
    #' @param v_range c(min, max) for v parameter
    #' @param u_resolution Grid resolution in u
    #' @param v_resolution Grid resolution in v
    #' @param color Surface color
    #' @param fill_opacity Surface opacity
    #' @param color_func Optional per-point coloring function
    #' @param checkerboard_colors Two colors for checkerboard pattern
    #' @param mesh_lines Draw wireframe mesh
    #' @param ... Additional args
    initialize = function(func,
                          u_range = c(0, TAU),
                          v_range = c(0, PI),
                          u_resolution = 32,
                          v_resolution = 32,
                          color = BLUE,
                          fill_opacity = 0.8,
                          color_func = NULL,
                          checkerboard_colors = NULL,
                          mesh_lines = FALSE,
                          ...) {
      super$initialize(
        color = color,
        fill_color = color,
        fill_opacity = fill_opacity,
        stroke_width = if (mesh_lines) 0.5 else 0,
        ...
      )

      self$func <- func
      self$u_range <- u_range
      self$v_range <- v_range
      self$u_resolution <- u_resolution
      self$v_resolution <- v_resolution
      self$color_func <- color_func
      self$checkerboard_colors <- checkerboard_colors
      self$mesh_lines <- mesh_lines
      self$type <- "surface"

      private$build_surface()
    },

    #' @description Set a color function for the surface
    #' @param func Function(x, y, z) -> hex color string
    set_color_by_function = function(func) {
      self$color_func <- func
      private$apply_color_function()
      invisible(self)
    },

    #' @description Color the surface by z-value using a gradient
    #' @param colors Vector of colors for the gradient
    #' @param z_range Optional c(min, max) range
    set_color_by_z = function(colors = c(BLUE, GREEN, YELLOW, RED),
                              z_range = NULL) {
      if (is.null(z_range)) {
        # Compute from patches
        all_z <- unlist(lapply(self$submobjects, function(patch) {
          if (!is.null(patch$points)) mean(patch$points[, 3])
          else 0
        }))
        z_range <- range(all_z, na.rm = TRUE)
      }

      grad <- color_gradient(colors, 256)

      self$color_func <- function(x, y, z) {
        t <- (z - z_range[1]) / max(z_range[2] - z_range[1], 1e-8)
        t <- max(0, min(1, t))
        idx <- max(1, ceiling(t * 256))
        grad[idx]
      }
      private$apply_color_function()
      invisible(self)
    },

    #' @description Get the mesh grid points (u_res+1 x v_res+1 x 3 array)
    get_mesh = function() {
      private$.mesh
    }
  ),

  private = list(
    .mesh = NULL,

    build_surface = function() {
      nu <- self$u_resolution
      nv <- self$v_resolution
      u_vals <- seq(self$u_range[1], self$u_range[2], length.out = nu + 1)
      v_vals <- seq(self$v_range[1], self$v_range[2], length.out = nv + 1)

      # Evaluate grid
      mesh <- array(0, dim = c(nu + 1, nv + 1, 3))
      for (i in seq_along(u_vals)) {
        for (j in seq_along(v_vals)) {
          mesh[i, j, ] <- self$func(u_vals[i], v_vals[j])
        }
      }
      private$.mesh <- mesh

      # Create quadrilateral patches
      use_checker <- !is.null(self$checkerboard_colors) &&
                     length(self$checkerboard_colors) >= 2

      for (i in seq_len(nu)) {
        for (j in seq_len(nv)) {
          # Four corners of the quad
          p00 <- mesh[i,     j,     ]
          p10 <- mesh[i + 1, j,     ]
          p11 <- mesh[i + 1, j + 1, ]
          p01 <- mesh[i,     j + 1, ]

          # Create patch as VMobject with 4 Bezier curves (straight lines)
          patch <- private$make_quad_patch(p00, p10, p11, p01)

          # Color
          if (use_checker) {
            ci <- ((i + j) %% 2) + 1
            patch$fill_color <- self$checkerboard_colors[ci]
          } else {
            patch$fill_color <- self$fill_color %||% self$color
          }
          patch$fill_opacity <- self$fill_opacity
          patch$stroke_width <- if (self$mesh_lines) 0.5 else 0
          patch$stroke_color <- self$stroke_color %||% self$color
          patch$color <- self$color

          # Normal for shading
          v1 <- p10 - p00
          v2 <- p01 - p00
          n <- c(v1[2]*v2[3] - v1[3]*v2[2],
                 v1[3]*v2[1] - v1[1]*v2[3],
                 v1[1]*v2[2] - v1[2]*v2[1])
          n_len <- sqrt(sum(n^2))
          if (n_len > 1e-8) n <- n / n_len

          # Store normal on the patch
          patch$data <- list(normal = n)

          self$add_submobject(patch)
        }
      }

      # Apply color function if set
      if (!is.null(self$color_func)) {
        private$apply_color_function()
      }
    },

    make_quad_patch = function(p0, p1, p2, p3) {
      vm <- VMobject$new()
      # 4 straight-line Bezier curves forming a quadrilateral
      edges <- list(
        list(p0, p1),
        list(p1, p2),
        list(p2, p3),
        list(p3, p0)
      )

      pts <- matrix(numeric(0), ncol = 3)
      for (edge in edges) {
        a <- edge[[1]]
        b <- edge[[2]]
        pts <- rbind(pts,
          a,
          a + (b - a) / 3,
          a + 2 * (b - a) / 3,
          b
        )
      }
      vm$points <- pts
      vm
    },

    apply_color_function = function() {
      if (is.null(self$color_func)) return()

      for (patch in self$submobjects) {
        if (!is.null(patch$points) && nrow(patch$points) > 0) {
          center <- colMeans(patch$points)
          col <- self$color_func(center[1], center[2], center[3])
          patch$fill_color <- col
          patch$color <- col
        }
      }
    }
  )
)


#' ParametricFunction3D R6 Class
#'
#' @description
#' A 3D parametric curve defined by f(t) -> (x, y, z).
#'
#' @export
ParametricFunction3D <- R6::R6Class(
  "ParametricFunction3D",
  inherit = ThreeDVMobject,

  public = list(
    #' @field func Parametric function f(t) -> c(x, y, z)
    func = NULL,

    #' @field t_range Range c(min, max, step)
    t_range = NULL,

    #' @description Create a 3D parametric curve
    #' @param func Function(t) returning c(x, y, z)
    #' @param t_range c(min, max) or c(min, max, step)
    #' @param color Curve color
    #' @param stroke_width Stroke width
    #' @param num_points Number of sample points for Bezier fitting
    #' @param ... Additional args
    initialize = function(func,
                          t_range = c(0, 1),
                          color = YELLOW,
                          stroke_width = DEFAULT_STROKE_WIDTH,
                          num_points = 200,
                          ...) {
      super$initialize(
        color = color,
        stroke_width = stroke_width,
        shade = FALSE,
        ...
      )
      self$func <- func
      self$t_range <- t_range
      self$type <- "parametric_3d"

      private$build_curve(num_points)
    }
  ),

  private = list(
    build_curve = function(num_points) {
      t_min <- self$t_range[1]
      t_max <- self$t_range[2]
      t_vals <- seq(t_min, t_max, length.out = num_points)

      # Sample points
      sampled <- matrix(0, nrow = num_points, ncol = 3)
      for (i in seq_along(t_vals)) {
        sampled[i, ] <- self$func(t_vals[i])
      }

      # Fit cubic Bezier curves through sampled points
      # Group into segments of 4 for cubic Bezier
      n <- nrow(sampled)
      pts <- matrix(numeric(0), ncol = 3)

      # Use every 3 sampled points to define one Bezier segment
      step <- max(1, floor((n - 1) / floor(n / 3)))
      i <- 1
      while (i + 3 <= n) {
        p0 <- sampled[i, ]
        p3 <- sampled[min(i + 3, n), ]
        p1 <- sampled[min(i + 1, n), ]
        p2 <- sampled[min(i + 2, n), ]
        pts <- rbind(pts, p0, p1, p2, p3)
        i <- i + 3
      }

      # Handle remaining points
      if (i <= n) {
        p0 <- sampled[i, ]
        p3 <- sampled[n, ]
        p1 <- p0 + (p3 - p0) / 3
        p2 <- p0 + 2 * (p3 - p0) / 3
        pts <- rbind(pts, p0, p1, p2, p3)
      }

      self$points <- pts
    }
  )
)
