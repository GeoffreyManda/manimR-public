#' @title Specialized Scene Types
#' @description
#' Pre-configured Scene subclasses for specific use cases:
#' ThreeDScene, VectorScene, GraphScene, StatScene.
#' @name scene_types
#' @keywords internal
NULL

#' ThreeDScene R6 Class
#'
#' @description
#' A scene pre-configured for 3D rendering with a ThreeDCamera.
#' Provides convenience methods for setting camera angles and
#' ambient lighting.
#'
#' @export
ThreeDScene <- R6::R6Class(
  "ThreeDScene",
  inherit = Scene,

  public = list(
    #' @field camera3d ThreeDCamera instance
    camera3d = NULL,

    #' @description Create a ThreeDScene
    #' @param name Scene name
    #' @param phi Initial azimuth angle
    #' @param theta Initial elevation angle
    #' @param gamma Roll angle
    #' @param ... Additional args passed to Scene
    initialize = function(name = "3d_scene",
                          phi = PI / 4,
                          theta = -PI / 4,
                          gamma = 0,
                          ...) {
      super$initialize(name = name, dimensions = "3D", ...)
      self$camera3d <- ThreeDCamera$new(
        phi = phi,
        theta = theta,
        gamma = gamma
      )
      self$camera <- self$camera3d
    },

    #' @description Set camera orientation
    #' @param phi Azimuth angle
    #' @param theta Elevation angle
    #' @param gamma Roll angle
    set_camera_orientation = function(phi = NULL, theta = NULL, gamma = NULL) {
      if (!is.null(phi)) self$camera3d$phi <- phi
      if (!is.null(theta)) self$camera3d$theta <- theta
      if (!is.null(gamma)) self$camera3d$gamma <- gamma
      invisible(self)
    },

    #' @description Begin ambient rotation of the camera
    #' @param rate Rotation rate in radians per second
    #' @param about Which axis to rotate about ("theta" or "phi")
    begin_ambient_rotation = function(rate = 0.1, about = "theta") {
      self$add_updater(function(scene, dt) {
        if (about == "theta") {
          scene$camera3d$theta <- scene$camera3d$theta + rate * dt
        } else {
          scene$camera3d$phi <- scene$camera3d$phi + rate * dt
        }
      })
      invisible(self)
    },

    #' @description Stop ambient rotation
    stop_ambient_rotation = function() {
      self$clear_updaters()
      invisible(self)
    },

    #' @description Add 3D axes to the scene
    #' @param ... Arguments passed to ThreeDAxes
    add_axes = function(...) {
      ax <- ThreeDAxes$new(...)
      self$add(ax)
      ax
    }
  )
)


#' VectorScene R6 Class
#'
#' @description
#' A scene pre-configured for vector field and linear algebra
#' visualizations. Includes a NumberPlane and helper methods
#' for adding vectors, matrices, and transformations.
#'
#' @export
VectorScene <- R6::R6Class(
  "VectorScene",
  inherit = Scene,

  public = list(
    #' @field plane NumberPlane for the background grid
    plane = NULL,

    #' @description Create a VectorScene
    #' @param name Scene name
    #' @param x_range X range for the plane
    #' @param y_range Y range for the plane
    #' @param ... Additional args
    initialize = function(name = "vector_scene",
                          x_range = c(-5, 5, 1),
                          y_range = c(-3, 3, 1),
                          ...) {
      super$initialize(name = name, ...)
      self$plane <- NumberPlane$new(
        x_range = x_range,
        y_range = y_range
      )
      self$add(self$plane)
    },

    #' @description Add a vector arrow from origin
    #' @param direction Vector direction c(x, y)
    #' @param color Arrow color
    #' @return The Vector mobject
    add_vector = function(direction, color = YELLOW) {
      v <- Vector$new(direction = c(direction, 0), color = color)
      self$add(v)
      v
    },

    #' @description Apply a linear transformation matrix to all vectors
    #' @param matrix 2x2 transformation matrix
    #' @param duration Animation duration
    apply_matrix = function(matrix, duration = 2) {
      # Expand 2x2 to 3x3 if needed
      if (is.matrix(matrix) && all(dim(matrix) == c(2, 2))) {
        mat3 <- diag(3)
        mat3[1:2, 1:2] <- matrix
      } else {
        mat3 <- matrix
      }

      # Create apply_matrix animations for all current mobjects
      for (mob in self$mobjects) {
        mob_id <- mob$id %||% mob$name
        if (!is.null(mob_id)) {
          spec <- list(
            type = "apply_matrix",
            object_id = mob_id,
            start_time = self$current_time,
            duration = duration,
            easing = "smooth",
            matrix = mat3
          )
          self$timeline <- append(self$timeline, list(spec))
        }
      }

      self$current_time <- self$current_time + duration
      invisible(self)
    },

    #' @description Show basis vectors (i-hat, j-hat)
    #' @param color_i Color for i-hat
    #' @param color_j Color for j-hat
    show_basis_vectors = function(color_i = GREEN, color_j = RED) {
      i_hat <- Vector$new(direction = c(1, 0, 0), color = color_i)
      j_hat <- Vector$new(direction = c(0, 1, 0), color = color_j)

      i_label <- Text$new(text = "i", font_size = 24, color = color_i,
                           position = c(0.5, -0.3, 0))
      j_label <- Text$new(text = "j", font_size = 24, color = color_j,
                           position = c(-0.3, 0.5, 0))

      self$add(i_hat)
      self$add(j_hat)
      self$add(i_label)
      self$add(j_label)
      self$play(AnimationGroup$new(
        GrowArrow$new(i_hat),
        GrowArrow$new(j_hat),
        FadeIn$new(i_label),
        FadeIn$new(j_label)
      ))

      list(i_hat = i_hat, j_hat = j_hat)
    }
  )
)


#' GraphScene R6 Class
#'
#' @description
#' A scene pre-configured for graph theory visualizations.
#' Provides methods for adding graphs, running algorithms,
#' and highlighting paths.
#'
#' @export
GraphScene <- R6::R6Class(
  "GraphScene",
  inherit = Scene,

  public = list(
    #' @field graph The main Graph object
    graph = NULL,

    #' @description Create a GraphScene
    #' @param name Scene name
    #' @param ... Additional args
    initialize = function(name = "graph_scene", ...) {
      super$initialize(name = name, ...)
    },

    #' @description Add a graph to the scene
    #' @param vertices Vertex ids
    #' @param edges Edge list
    #' @param layout Layout algorithm
    #' @param directed Whether the graph is directed
    #' @param ... Additional args for Graph/DiGraph
    #' @return The Graph mobject
    add_graph = function(vertices, edges, layout = "spring",
                         directed = FALSE, ...) {
      if (directed) {
        self$graph <- DiGraph$new(vertices, edges, layout = layout, ...)
      } else {
        self$graph <- Graph$new(vertices, edges, layout = layout, ...)
      }
      self$add(self$graph)
      self$play(Create$new(self$graph, run_time = 1))
      self$graph
    },

    #' @description Highlight a path in the graph
    #' @param path Vector of vertex ids forming the path
    #' @param color Highlight color
    #' @param duration Animation duration per edge
    highlight_path = function(path, color = YELLOW, duration = 0.5) {
      for (i in seq_len(length(path) - 1)) {
        from <- path[i]
        to <- path[i + 1]

        # Highlight vertex
        self$graph$highlight_vertex(from, color = color)
        v_mob <- self$graph$get_vertex(from)
        if (!is.null(v_mob)) {
          self$play(Indicate$new(v_mob, run_time = duration * 0.3))
        }

        # Highlight edge
        self$graph$highlight_edge(from, to, color = color)
        e_mob <- self$graph$get_edge(from, to)
        if (!is.null(e_mob)) {
          self$play(ShowPassingFlash$new(e_mob, run_time = duration * 0.7))
        }
      }

      # Highlight last vertex
      last <- path[length(path)]
      self$graph$highlight_vertex(last, color = color)
      v_mob <- self$graph$get_vertex(last)
      if (!is.null(v_mob)) {
        self$play(Indicate$new(v_mob, run_time = duration * 0.3))
      }

      invisible(self)
    },

    #' @description Animate BFS traversal
    #' @param start_vertex Starting vertex id
    #' @param color Traversal highlight color
    #' @param duration Duration per vertex
    animate_bfs = function(start_vertex, color = YELLOW, duration = 0.3) {
      if (is.null(self$graph)) {
        warning("No graph added to scene")
        return(invisible(self))
      }

      # Build adjacency list
      adj <- list()
      for (id in self$graph$vertex_ids) adj[[id]] <- character(0)
      for (el in self$graph$edge_list) {
        adj[[el[1]]] <- c(adj[[el[1]]], el[2])
        adj[[el[2]]] <- c(adj[[el[2]]], el[1])
      }

      visited <- character(0)
      queue <- start_vertex

      while (length(queue) > 0) {
        current <- queue[1]
        queue <- queue[-1]
        if (current %in% visited) next
        visited <- c(visited, current)

        self$graph$highlight_vertex(current, color = color)
        v_mob <- self$graph$get_vertex(current)
        if (!is.null(v_mob)) {
          self$play(Flash$new(v_mob, run_time = duration))
        }

        for (nb in adj[[current]]) {
          if (!(nb %in% visited)) {
            queue <- c(queue, nb)
          }
        }
      }

      invisible(self)
    }
  )
)


#' StatScene R6 Class
#'
#' @description
#' A scene pre-configured for statistical visualizations with
#' helper methods for common statistical animations.
#'
#' @export
StatScene <- R6::R6Class(
  "StatScene",
  inherit = Scene,

  public = list(
    #' @field axes Default axes for the scene
    axes = NULL,

    #' @description Create a StatScene
    #' @param name Scene name
    #' @param x_range X range
    #' @param y_range Y range
    #' @param ... Additional args
    initialize = function(name = "stat_scene",
                          x_range = c(-5, 5, 1),
                          y_range = c(0, 5, 1),
                          ...) {
      super$initialize(name = name, ...)
      self$axes <- Axes$new(
        x_range = x_range,
        y_range = y_range,
        include_numbers = TRUE
      )
      self$add(self$axes)
    },

    #' @description Quick histogram animation
    #' @param data Numeric vector
    #' @param bins Number of bins
    #' @param color Bar color
    add_histogram = function(data, bins = 30, color = BLUE) {
      sc <- animate_histogram(data, bins = bins, color = color,
                               axes = self$axes, duration = 2)
      # Merge timeline
      for (obj_id in names(sc$objects)) {
        if (!(obj_id %in% names(self$objects))) {
          self$add(sc$objects[[obj_id]])
        }
      }
      invisible(self)
    },

    #' @description Quick regression animation
    #' @param data Data.frame
    #' @param x X column
    #' @param y Y column
    add_regression = function(data, x, y) {
      sc <- animate_regression(data, x, y, axes = self$axes, duration = 3)
      invisible(self)
    }
  )
)


#' Create a ThreeDScene
#'
#' @description Convenience factory function for ThreeDScene.
#' @param name Scene name
#' @param ... Arguments passed to ThreeDScene$new()
#' @return A ThreeDScene object
#' @export
scene_3d <- function(name = "3d_scene", ...) {
  ThreeDScene$new(name = name, ...)
}


#' Add a 3D surface to a scene (stub)
#'
#' @description
#' Placeholder for 3D surface rendering. This feature is planned
#' but not yet implemented. Currently returns the scene unchanged
#' with an informative warning.
#'
#' @param scene A Scene object
#' @param formula Formula describing the surface (e.g., `z ~ sin(x) * cos(y)`)
#' @param ... Additional surface parameters
#' @return The scene object (invisibly)
#' @export
add_surface <- function(scene, formula, ...) {
  cli::cli_warn(c(
    "3D surface rendering is not yet implemented.",
    "i" = "The scene is returned unchanged.",
    "i" = "Track progress at {.url https://github.com/Reyanda/manimR/issues}"
  ))
  invisible(scene)
}


#' Orbit the camera around the scene
#'
#' @description
#' For ThreeDScene objects, begins ambient camera rotation (theta axis) and
#' waits for the specified duration so the orbit is captured in the render.
#' For plain Scene objects, issues an informative warning.
#'
#' @param scene A Scene object (preferably ThreeDScene)
#' @param duration Duration of the orbit in seconds
#' @param rate Rotation rate in radians per second (default TAU / duration =
#'   one full revolution over `duration` seconds)
#' @param about Axis of rotation: "theta" (azimuth, default) or "phi" (elevation)
#' @param ... Additional parameters (currently unused)
#' @return The scene object (invisibly)
#' @export
camera_orbit <- function(scene, duration = 5,
                          rate = NULL, about = "theta", ...) {
  if (inherits(scene, "ThreeDScene")) {
    orbit_rate <- rate %||% (TAU / duration)
    scene$begin_ambient_rotation(rate = orbit_rate, about = about)
    scene$wait(duration)
    scene$stop_ambient_rotation()
  } else {
    cli::cli_warn(c(
      "{.fn camera_orbit} requires a {.cls ThreeDScene}.",
      "i" = "Use {.code ThreeDScene$new()} or {.code scene_3d()} to create a 3D scene.",
      "i" = "For 2D camera pan/zoom, see {.code scene$camera_shift()} and {.code scene$camera_zoom()}."
    ))
  }
  invisible(scene)
}
