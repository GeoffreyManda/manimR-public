#' @title 3D Shape Primitives
#' @description
#' Common 3D shapes built on the Surface/ThreeDVMobject foundation:
#' Sphere, Cube, Cylinder, Cone, Torus, Prism, and regular polyhedra.
#' @name three_d_shapes
#' @keywords internal
NULL

#' Sphere R6 Class
#'
#' @description
#' A UV-mapped sphere of given radius, centered at the origin.
#'
#' @export
Sphere <- R6::R6Class(
 "Sphere",
  inherit = Surface,

  public = list(
    #' @field radius Sphere radius
    radius = 1,

    #' @description Create a Sphere
    #' @param radius Sphere radius
    #' @param u_resolution Longitude subdivisions
    #' @param v_resolution Latitude subdivisions
    #' @param color Surface color
    #' @param ... Additional args passed to Surface
    initialize = function(radius = 1,
                          u_resolution = 32,
                          v_resolution = 24,
                          color = BLUE_D,
                          ...) {
      r <- radius
      self$radius <- r

      sphere_func <- function(u, v) {
        c(
          r * sin(v) * cos(u),
          r * sin(v) * sin(u),
          r * cos(v)
        )
      }

      super$initialize(
        func = sphere_func,
        u_range = c(0, TAU),
        v_range = c(0, PI),
        u_resolution = u_resolution,
        v_resolution = v_resolution,
        color = color,
        checkerboard_colors = c(BLUE_D, BLUE_E),
        ...
      )
    }
  )
)


#' Torus R6 Class
#'
#' @description
#' A torus (donut shape) defined by major and minor radii.
#'
#' @export
Torus <- R6::R6Class(
  "Torus",
  inherit = Surface,

  public = list(
    #' @field major_radius Distance from center to tube center
    major_radius = 2,

    #' @field minor_radius Tube radius
    minor_radius = 0.5,

    #' @description Create a Torus
    #' @param major_radius Ring radius
    #' @param minor_radius Tube radius
    #' @param u_resolution Subdivisions around the ring
    #' @param v_resolution Subdivisions around the tube
    #' @param color Surface color
    #' @param ... Additional args
    initialize = function(major_radius = 2,
                          minor_radius = 0.5,
                          u_resolution = 40,
                          v_resolution = 24,
                          color = GREEN_D,
                          ...) {
      R <- major_radius
      r <- minor_radius
      self$major_radius <- R
      self$minor_radius <- r

      torus_func <- function(u, v) {
        c(
          (R + r * cos(v)) * cos(u),
          (R + r * cos(v)) * sin(u),
          r * sin(v)
        )
      }

      super$initialize(
        func = torus_func,
        u_range = c(0, TAU),
        v_range = c(0, TAU),
        u_resolution = u_resolution,
        v_resolution = v_resolution,
        color = color,
        ...
      )
    }
  )
)


#' Cylinder R6 Class
#'
#' @description
#' A cylinder along the z-axis with given radius and height.
#'
#' @export
Cylinder <- R6::R6Class(
  "Cylinder",
  inherit = Surface,

  public = list(
    #' @field radius Cylinder radius
    radius = 1,

    #' @field height Cylinder height
    height = 2,

    #' @description Create a Cylinder
    #' @param radius Cylinder radius
    #' @param height Cylinder height
    #' @param u_resolution Angular subdivisions
    #' @param v_resolution Height subdivisions
    #' @param color Surface color
    #' @param show_caps Whether to add top/bottom caps
    #' @param ... Additional args
    initialize = function(radius = 1,
                          height = 2,
                          u_resolution = 32,
                          v_resolution = 1,
                          color = TEAL_D,
                          show_caps = TRUE,
                          ...) {
      r <- radius
      h <- height
      self$radius <- r
      self$height <- h

      cyl_func <- function(u, v) {
        z <- -h/2 + v * h
        c(r * cos(u), r * sin(u), z)
      }

      super$initialize(
        func = cyl_func,
        u_range = c(0, TAU),
        v_range = c(0, 1),
        u_resolution = u_resolution,
        v_resolution = v_resolution,
        color = color,
        ...
      )

      # Add caps
      if (show_caps) {
        top_cap <- private$make_cap(r, h/2, u_resolution, color)
        bottom_cap <- private$make_cap(r, -h/2, u_resolution, color)
        self$add_submobject(top_cap)
        self$add_submobject(bottom_cap)
      }
    }
  ),

  private = list(
    make_cap = function(r, z, n_seg, color) {
      cap <- VMobject$new(
        fill_color = color,
        fill_opacity = self$fill_opacity,
        stroke_width = 0
      )

      # Disc as triangular fan approximation using quad patches
      pts <- matrix(numeric(0), ncol = 3)
      angles <- seq(0, TAU, length.out = n_seg + 1)

      for (i in seq_len(n_seg)) {
        p0 <- c(0, 0, z)
        p1 <- c(r * cos(angles[i]), r * sin(angles[i]), z)
        p2 <- c(r * cos(angles[i + 1]), r * sin(angles[i + 1]), z)

        # Triangle as 3 Bezier line segments
        for (edge in list(list(p0, p1), list(p1, p2), list(p2, p0))) {
          a <- edge[[1]]
          b <- edge[[2]]
          pts <- rbind(pts, a, a + (b - a)/3, a + 2*(b - a)/3, b)
        }
      }

      cap$points <- pts
      cap
    }
  )
)


#' Cone R6 Class
#'
#' @description
#' A cone with a circular base and apex at the top.
#'
#' @export
Cone <- R6::R6Class(
  "Cone",
  inherit = Surface,

  public = list(
    #' @field base_radius Base circle radius
    base_radius = 1,

    #' @field height Cone height
    height = 2,

    #' @description Create a Cone
    #' @param base_radius Base radius
    #' @param height Cone height
    #' @param u_resolution Angular subdivisions
    #' @param v_resolution Height subdivisions
    #' @param color Surface color
    #' @param ... Additional args
    initialize = function(base_radius = 1,
                          height = 2,
                          u_resolution = 32,
                          v_resolution = 8,
                          color = GOLD_D,
                          ...) {
      r <- base_radius
      h <- height
      self$base_radius <- r
      self$height <- h

      cone_func <- function(u, v) {
        # v goes from 0 (base) to 1 (apex)
        radius_at_v <- r * (1 - v)
        c(radius_at_v * cos(u), radius_at_v * sin(u), -h/2 + v * h)
      }

      super$initialize(
        func = cone_func,
        u_range = c(0, TAU),
        v_range = c(0, 1),
        u_resolution = u_resolution,
        v_resolution = v_resolution,
        color = color,
        ...
      )
    }
  )
)


#' Cube R6 Class
#'
#' @description
#' An axis-aligned cube (or rectangular prism) composed of 6 face submobjects.
#'
#' @export
Cube <- R6::R6Class(
  "Cube",
  inherit = ThreeDVMobject,

  public = list(
    #' @field side_length Side length of the cube
    side_length = 2,

    #' @field face_colors Colors for each face
    face_colors = NULL,

    #' @description Create a Cube
    #' @param side_length Length of each side
    #' @param color Base color (all faces)
    #' @param fill_opacity Opacity
    #' @param face_colors Optional vector of 6 colors for each face
    #' @param ... Additional args
    initialize = function(side_length = 2,
                          color = BLUE,
                          fill_opacity = 0.8,
                          face_colors = NULL,
                          ...) {
      super$initialize(
        color = color,
        fill_color = color,
        fill_opacity = fill_opacity,
        ...
      )
      self$side_length <- side_length
      self$face_colors <- face_colors
      self$type <- "cube"

      private$build_faces()
    },

    #' @description Get a specific face (1-6)
    #' @param i Face index
    get_face = function(i) {
      if (i < 1 || i > 6) stop("Face index must be 1-6")
      self$submobjects[[i]]
    }
  ),

  private = list(
    build_faces = function() {
      s <- self$side_length / 2

      # 6 faces: +x, -x, +y, -y, +z, -z
      face_defs <- list(
        list(corners = list(c(s,-s,-s), c(s,s,-s), c(s,s,s), c(s,-s,s)),
             normal = c(1,0,0)),
        list(corners = list(c(-s,-s,-s), c(-s,-s,s), c(-s,s,s), c(-s,s,-s)),
             normal = c(-1,0,0)),
        list(corners = list(c(-s,s,-s), c(-s,s,s), c(s,s,s), c(s,s,-s)),
             normal = c(0,1,0)),
        list(corners = list(c(-s,-s,-s), c(s,-s,-s), c(s,-s,s), c(-s,-s,s)),
             normal = c(0,-1,0)),
        list(corners = list(c(-s,-s,s), c(s,-s,s), c(s,s,s), c(-s,s,s)),
             normal = c(0,0,1)),
        list(corners = list(c(-s,-s,-s), c(-s,s,-s), c(s,s,-s), c(s,-s,-s)),
             normal = c(0,0,-1))
      )

      default_colors <- c(BLUE, BLUE_D, BLUE_E, TEAL, TEAL_D, TEAL_E)

      for (fi in seq_along(face_defs)) {
        fd <- face_defs[[fi]]
        corners <- fd$corners

        face <- VMobject$new(
          fill_color = if (!is.null(self$face_colors)) self$face_colors[fi]
                       else if (!is.null(self$fill_color)) self$fill_color
                       else default_colors[fi],
          fill_opacity = self$fill_opacity,
          stroke_width = 1,
          stroke_color = WHITE
        )
        face$data <- list(normal = fd$normal)

        # Build 4-edge Bezier quad
        pts <- matrix(numeric(0), ncol = 3)
        for (ei in 1:4) {
          a <- corners[[ei]]
          b <- corners[[(ei %% 4) + 1]]
          pts <- rbind(pts, a, a + (b-a)/3, a + 2*(b-a)/3, b)
        }
        face$points <- pts
        self$add_submobject(face)
      }
    }
  )
)


#' Prism R6 Class
#'
#' @description
#' A rectangular prism (box) with independent width, height, and depth.
#'
#' @export
Prism <- R6::R6Class(
  "Prism",
  inherit = ThreeDVMobject,

  public = list(
    #' @field dimensions c(width, height, depth)
    dimensions = c(2, 2, 2),

    #' @description Create a Prism
    #' @param dimensions c(width, height, depth)
    #' @param color Color
    #' @param fill_opacity Opacity
    #' @param ... Additional args
    initialize = function(dimensions = c(3, 2, 1),
                          color = BLUE,
                          fill_opacity = 0.8,
                          ...) {
      super$initialize(color = color, fill_color = color,
                       fill_opacity = fill_opacity, ...)
      self$dimensions <- dimensions
      self$type <- "prism"

      private$build_faces()
    }
  ),

  private = list(
    build_faces = function() {
      w <- self$dimensions[1] / 2
      h <- self$dimensions[2] / 2
      d <- self$dimensions[3] / 2

      face_defs <- list(
        list(c(w,-h,-d), c(w,h,-d), c(w,h,d), c(w,-h,d)),
        list(c(-w,-h,-d), c(-w,-h,d), c(-w,h,d), c(-w,h,-d)),
        list(c(-w,h,-d), c(-w,h,d), c(w,h,d), c(w,h,-d)),
        list(c(-w,-h,-d), c(w,-h,-d), c(w,-h,d), c(-w,-h,d)),
        list(c(-w,-h,d), c(w,-h,d), c(w,h,d), c(-w,h,d)),
        list(c(-w,-h,-d), c(-w,h,-d), c(w,h,-d), c(w,-h,-d))
      )

      for (corners in face_defs) {
        face <- VMobject$new(
          fill_color = self$fill_color %||% self$color,
          fill_opacity = self$fill_opacity,
          stroke_width = 1
        )
        pts <- matrix(numeric(0), ncol = 3)
        for (ei in 1:4) {
          a <- corners[[ei]]
          b <- corners[[(ei %% 4) + 1]]
          pts <- rbind(pts, a, a + (b-a)/3, a + 2*(b-a)/3, b)
        }
        face$points <- pts
        self$add_submobject(face)
      }
    }
  )
)


#' Dot3D R6 Class
#'
#' @description
#' A small sphere used as a 3D point indicator.
#'
#' @export
Dot3D <- R6::R6Class(
  "Dot3D",
  inherit = Sphere,

  public = list(
    #' @description Create a 3D dot
    #' @param point Position c(x, y, z)
    #' @param radius Dot radius
    #' @param color Dot color
    #' @param ... Additional args
    initialize = function(point = ORIGIN,
                          radius = DEFAULT_DOT_RADIUS,
                          color = WHITE,
                          ...) {
      super$initialize(
        radius = radius,
        color = color,
        u_resolution = 8,
        v_resolution = 6,
        ...
      )
      self$move_to(point)
    }
  )
)


#' Line3D R6 Class
#'
#' @description
#' A 3D line (thin cylinder) between two points.
#'
#' @export
Line3D <- R6::R6Class(
  "Line3D",
  inherit = ThreeDVMobject,

  public = list(
    #' @field start_point Start of line
    start_point = NULL,

    #' @field end_point End of line
    end_point = NULL,

    #' @field thickness Line thickness (cylinder radius)
    thickness = 0.02,

    #' @description Create a 3D line
    #' @param start Start point c(x,y,z)
    #' @param end End point c(x,y,z)
    #' @param thickness Cylinder thickness
    #' @param color Line color
    #' @param ... Additional args
    initialize = function(start = ORIGIN,
                          end = c(1, 1, 1),
                          thickness = 0.02,
                          color = WHITE,
                          ...) {
      super$initialize(color = color, stroke_width = DEFAULT_STROKE_WIDTH, ...)
      self$start_point <- start
      self$end_point <- end
      self$thickness <- thickness
      self$type <- "line_3d"

      # Simple approach: straight Bezier curve in 3D
      p0 <- start
      p3 <- end
      p1 <- p0 + (p3 - p0) / 3
      p2 <- p0 + 2 * (p3 - p0) / 3
      self$points <- rbind(p0, p1, p2, p3)
    },

    #' @description Get line length
    get_length = function() {
      sqrt(sum((self$end_point - self$start_point)^2))
    },

    #' @description Get unit direction vector
    get_direction = function() {
      d <- self$end_point - self$start_point
      len <- sqrt(sum(d^2))
      if (len < 1e-8) return(c(0, 0, 1))
      d / len
    }
  )
)


#' Arrow3D R6 Class
#'
#' @description
#' A 3D arrow (line with a cone tip).
#'
#' @export
Arrow3D <- R6::R6Class(
  "Arrow3D",
  inherit = Line3D,

  public = list(
    #' @field tip_length Length of the arrowhead cone
    tip_length = 0.3,

    #' @description Create a 3D arrow
    #' @param start Start point
    #' @param end End point
    #' @param tip_length Length of arrow tip
    #' @param color Arrow color
    #' @param ... Additional args
    initialize = function(start = ORIGIN,
                          end = c(1, 1, 1),
                          tip_length = 0.3,
                          color = YELLOW,
                          ...) {
      self$tip_length <- tip_length

      # Shorten the line to make room for tip
      dir <- end - start
      len <- sqrt(sum(dir^2))
      if (len > tip_length) {
        line_end <- start + dir * (1 - tip_length / len)
      } else {
        line_end <- start
      }

      super$initialize(start = start, end = line_end, color = color, ...)

      # Add cone tip as submobject
      # The cone would need to be oriented along the arrow direction
      # For now, store tip metadata for the renderer
      self$data <- list(
        has_tip = TRUE,
        tip_start = line_end,
        tip_end = end,
        tip_length = tip_length
      )
      self$type <- "arrow_3d"
    }
  )
)


#' Tetrahedron R6 Class
#'
#' @description
#' A regular tetrahedron (4 equilateral triangular faces).
#'
#' @export
Tetrahedron <- R6::R6Class(
  "Tetrahedron",
  inherit = ThreeDVMobject,

  public = list(
    #' @field edge_length Edge length
    edge_length = 2,

    #' @description Create a regular Tetrahedron
    #' @param edge_length Edge length
    #' @param color Color
    #' @param fill_opacity Opacity
    #' @param ... Additional args
    initialize = function(edge_length = 2,
                          color = GREEN,
                          fill_opacity = 0.8,
                          ...) {
      super$initialize(color = color, fill_color = color,
                       fill_opacity = fill_opacity, ...)
      self$edge_length <- edge_length
      self$type <- "tetrahedron"

      private$build_tetrahedron()
    }
  ),

  private = list(
    build_tetrahedron = function() {
      a <- self$edge_length
      # Vertices of a regular tetrahedron centered at origin
      s <- a / sqrt(3)
      v1 <- c( s,  s,  s)
      v2 <- c( s, -s, -s)
      v3 <- c(-s,  s, -s)
      v4 <- c(-s, -s,  s)

      # 4 triangular faces
      faces <- list(
        list(v1, v2, v3),
        list(v1, v2, v4),
        list(v1, v3, v4),
        list(v2, v3, v4)
      )

      face_colors <- c(GREEN, GREEN_D, GREEN_E, TEAL)

      for (fi in seq_along(faces)) {
        corners <- faces[[fi]]
        face <- VMobject$new(
          fill_color = face_colors[fi],
          fill_opacity = self$fill_opacity,
          stroke_width = 1
        )
        pts <- matrix(numeric(0), ncol = 3)
        for (ei in 1:3) {
          a_pt <- corners[[ei]]
          b_pt <- corners[[(ei %% 3) + 1]]
          pts <- rbind(pts, a_pt, a_pt + (b_pt - a_pt)/3,
                       a_pt + 2*(b_pt - a_pt)/3, b_pt)
        }
        face$points <- pts
        self$add_submobject(face)
      }
    }
  )
)


#' Octahedron R6 Class
#'
#' @description
#' A regular octahedron (8 equilateral triangular faces).
#'
#' @export
Octahedron <- R6::R6Class(
  "Octahedron",
  inherit = ThreeDVMobject,

  public = list(
    #' @field edge_length Edge length
    edge_length = 2,

    #' @description Create a regular Octahedron
    #' @param edge_length Edge length
    #' @param color Color
    #' @param fill_opacity Opacity
    #' @param ... Additional args
    initialize = function(edge_length = 2,
                          color = PURPLE,
                          fill_opacity = 0.8,
                          ...) {
      super$initialize(color = color, fill_color = color,
                       fill_opacity = fill_opacity, ...)
      self$edge_length <- edge_length
      self$type <- "octahedron"

      private$build_octahedron()
    }
  ),

  private = list(
    build_octahedron = function() {
      a <- self$edge_length / sqrt(2)
      # 6 vertices
      verts <- list(
        c(a, 0, 0), c(-a, 0, 0),
        c(0, a, 0), c(0, -a, 0),
        c(0, 0, a), c(0, 0, -a)
      )

      # 8 triangular faces (indices 1-based)
      face_indices <- list(
        c(1, 3, 5), c(1, 5, 4), c(1, 4, 6), c(1, 6, 3),
        c(2, 5, 3), c(2, 4, 5), c(2, 6, 4), c(2, 3, 6)
      )

      colors8 <- rep(c(PURPLE, PURPLE_D), 4)

      for (fi in seq_along(face_indices)) {
        idx <- face_indices[[fi]]
        corners <- list(verts[[idx[1]]], verts[[idx[2]]], verts[[idx[3]]])
        face <- VMobject$new(
          fill_color = colors8[fi],
          fill_opacity = self$fill_opacity,
          stroke_width = 1
        )
        pts <- matrix(numeric(0), ncol = 3)
        for (ei in 1:3) {
          a_pt <- corners[[ei]]
          b_pt <- corners[[(ei %% 3) + 1]]
          pts <- rbind(pts, a_pt, a_pt + (b_pt - a_pt)/3,
                       a_pt + 2*(b_pt - a_pt)/3, b_pt)
        }
        face$points <- pts
        self$add_submobject(face)
      }
    }
  )
)


#' Icosahedron R6 Class
#'
#' @description
#' A regular icosahedron (20 equilateral triangular faces).
#'
#' @export
Icosahedron <- R6::R6Class(
  "Icosahedron",
  inherit = ThreeDVMobject,

  public = list(
    #' @field edge_length Edge length
    edge_length = 2,

    #' @description Create a regular Icosahedron
    #' @param edge_length Edge length
    #' @param color Color
    #' @param fill_opacity Opacity
    #' @param ... Additional args
    initialize = function(edge_length = 2,
                          color = TEAL,
                          fill_opacity = 0.8,
                          ...) {
      super$initialize(color = color, fill_color = color,
                       fill_opacity = fill_opacity, ...)
      self$edge_length <- edge_length
      self$type <- "icosahedron"

      private$build_icosahedron()
    }
  ),

  private = list(
    build_icosahedron = function() {
      # Golden ratio
      phi <- (1 + sqrt(5)) / 2
      s <- self$edge_length / 2

      # 12 vertices
      verts <- list(
        c(-s, phi*s, 0), c(s, phi*s, 0), c(-s, -phi*s, 0), c(s, -phi*s, 0),
        c(0, -s, phi*s), c(0, s, phi*s), c(0, -s, -phi*s), c(0, s, -phi*s),
        c(phi*s, 0, -s), c(phi*s, 0, s), c(-phi*s, 0, -s), c(-phi*s, 0, s)
      )

      # 20 triangular faces
      face_indices <- list(
        c(1,12,6), c(1,6,2), c(1,2,8), c(1,8,11), c(1,11,12),
        c(2,6,10), c(6,12,5), c(12,11,3), c(11,8,7), c(8,2,9),
        c(4,10,5), c(4,5,3), c(4,3,7), c(4,7,9), c(4,9,10),
        c(5,10,6), c(3,5,12), c(7,3,11), c(9,7,8), c(10,9,2)
      )

      colors20 <- rep(c(TEAL, TEAL_D, TEAL_E, GREEN_D), 5)

      for (fi in seq_along(face_indices)) {
        idx <- face_indices[[fi]]
        corners <- list(verts[[idx[1]]], verts[[idx[2]]], verts[[idx[3]]])
        face <- VMobject$new(
          fill_color = colors20[fi],
          fill_opacity = self$fill_opacity,
          stroke_width = 0.5
        )
        pts <- matrix(numeric(0), ncol = 3)
        for (ei in 1:3) {
          a_pt <- corners[[ei]]
          b_pt <- corners[[(ei %% 3) + 1]]
          pts <- rbind(pts, a_pt, a_pt + (b_pt - a_pt)/3,
                       a_pt + 2*(b_pt - a_pt)/3, b_pt)
        }
        face$points <- pts
        self$add_submobject(face)
      }
    }
  )
)


#' Dodecahedron R6 Class
#'
#' @description
#' A regular dodecahedron (12 regular pentagonal faces).
#'
#' @export
Dodecahedron <- R6::R6Class(
  "Dodecahedron",
  inherit = ThreeDVMobject,

  public = list(
    #' @field edge_length Edge length
    edge_length = 2,

    #' @description Create a regular Dodecahedron
    #' @param edge_length Edge length
    #' @param color Color
    #' @param fill_opacity Opacity
    #' @param ... Additional args
    initialize = function(edge_length = 2,
                          color = GOLD,
                          fill_opacity = 0.8,
                          ...) {
      super$initialize(color = color, fill_color = color,
                       fill_opacity = fill_opacity, ...)
      self$edge_length <- edge_length
      self$type <- "dodecahedron"

      private$build_dodecahedron()
    }
  ),

  private = list(
    build_dodecahedron = function() {
      phi <- (1 + sqrt(5)) / 2
      s <- self$edge_length / 2

      # 20 vertices of a dodecahedron
      verts <- list(
        # Cube vertices
        c(s, s, s), c(s, s, -s), c(s, -s, s), c(s, -s, -s),
        c(-s, s, s), c(-s, s, -s), c(-s, -s, s), c(-s, -s, -s),
        # Rectangle vertices (golden ratio)
        c(0, phi*s, s/phi), c(0, phi*s, -s/phi),
        c(0, -phi*s, s/phi), c(0, -phi*s, -s/phi),
        c(s/phi, 0, phi*s), c(-s/phi, 0, phi*s),
        c(s/phi, 0, -phi*s), c(-s/phi, 0, -phi*s),
        c(phi*s, s/phi, 0), c(phi*s, -s/phi, 0),
        c(-phi*s, s/phi, 0), c(-phi*s, -s/phi, 0)
      )

      # 12 pentagonal faces
      face_indices <- list(
        c(1,13,14,5,9), c(1,9,10,2,17), c(2,10,6,16,15),
        c(1,17,18,3,13), c(4,18,17,2,15), c(3,18,4,12,11),
        c(5,14,7,20,19), c(6,10,9,5,19), c(7,14,13,3,11),
        c(4,15,16,8,12), c(8,16,6,19,20), c(7,11,12,8,20)
      )

      colors12 <- rep(c(GOLD, GOLD_D, GOLD_E, YELLOW_D), 3)

      for (fi in seq_along(face_indices)) {
        idx <- face_indices[[fi]]
        corners <- lapply(idx, function(i) verts[[i]])

        face <- VMobject$new(
          fill_color = colors12[fi],
          fill_opacity = self$fill_opacity,
          stroke_width = 1
        )

        # Pentagon as 5 Bezier line segments
        n_corners <- length(corners)
        pts <- matrix(numeric(0), ncol = 3)
        for (ei in seq_len(n_corners)) {
          a_pt <- corners[[ei]]
          b_pt <- corners[[(ei %% n_corners) + 1]]
          pts <- rbind(pts, a_pt, a_pt + (b_pt - a_pt)/3,
                       a_pt + 2*(b_pt - a_pt)/3, b_pt)
        }
        face$points <- pts
        self$add_submobject(face)
      }
    }
  )
)
