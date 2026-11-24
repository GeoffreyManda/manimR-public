#' @title Graph and Network Classes
#' @description
#' Graph theory visualization: vertices, edges, directed graphs,
#' labeled graphs, and layout algorithms. Optionally integrates with
#' the igraph package for advanced layouts and algorithms.
#' @name graph
#' @keywords internal
NULL

#' Graph R6 Class
#'
#' @description
#' An undirected graph composed of vertex dots and edge lines. Supports
#' multiple layout algorithms (spring, circular, grid, tree, spectral)
#' with optional igraph integration for more advanced layouts.
#'
#' @export
Graph <- R6::R6Class(
  "Graph",
  inherit = VMobject,

  public = list(
    #' @field vertices Named list of vertex MObjects keyed by vertex id
    vertices = NULL,

    #' @field edges List of edge VMobjects
    edges = NULL,

    #' @field vertex_ids Character vector of vertex identifiers
    vertex_ids = NULL,

    #' @field edge_list List of c(from, to) pairs
    edge_list = NULL,

    #' @field layout_name Name of the layout used
    layout_name = "spring",

    #' @field positions Named list of vertex positions
    positions = NULL,

    #' @field vertex_config Default vertex appearance
    vertex_config = NULL,

    #' @field edge_config Default edge appearance
    edge_config = NULL,

    #' @field labels Named list of vertex label MObjects
    labels = NULL,

    #' @description Create a Graph
    #' @param vertices Character vector of vertex ids (or integer count)
    #' @param edges List of c(from, to) pairs or 2-column matrix
    #' @param layout Layout algorithm ("spring", "circular", "grid", "tree",
    #'   "spectral", "random", "kamada_kawai", or a named list of positions)
    #' @param vertex_config List of default vertex appearance options
    #' @param edge_config List of default edge appearance options
    #' @param labels Named list of labels or TRUE for automatic labels
    #' @param ... Additional args
    initialize = function(vertices,
                          edges,
                          layout = "spring",
                          vertex_config = NULL,
                          edge_config = NULL,
                          labels = NULL,
                          ...) {
      super$initialize(...)

      # Normalize vertices
      if (is.numeric(vertices) && length(vertices) == 1) {
        self$vertex_ids <- as.character(seq_len(vertices))
      } else {
        self$vertex_ids <- as.character(vertices)
      }

      # Normalize edges
      if (is.matrix(edges)) {
        self$edge_list <- lapply(seq_len(nrow(edges)), function(i) {
          as.character(edges[i, ])
        })
      } else {
        self$edge_list <- lapply(edges, as.character)
      }

      self$vertex_config <- vertex_config %||% list(
        radius = DEFAULT_DOT_RADIUS * 2,
        color = BLUE,
        fill_color = BLUE,
        fill_opacity = 1
      )
      self$edge_config <- edge_config %||% list(
        color = GREY_B,
        stroke_width = 2
      )

      self$type <- "graph"

      # Compute layout
      if (is.list(layout) && !is.null(names(layout))) {
        self$positions <- layout
        self$layout_name <- "custom"
      } else {
        self$layout_name <- layout
        self$positions <- private$compute_layout(layout)
      }

      # Build vertex and edge objects
      private$build_graph()

      # Add labels
      if (!is.null(labels)) {
        private$add_labels(labels)
      }
    },

    #' @description Get a vertex mobject by id
    #' @param id Vertex identifier
    get_vertex = function(id) {
      id <- as.character(id)
      self$vertices[[id]]
    },

    #' @description Get an edge mobject by vertex pair
    #' @param from Source vertex id
    #' @param to Target vertex id
    get_edge = function(from, to) {
      from <- as.character(from)
      to <- as.character(to)
      key <- paste(from, to, sep = "->")
      alt_key <- paste(to, from, sep = "->")
      self$edges[[key]] %||% self$edges[[alt_key]]
    },

    #' @description Change the layout of the graph
    #' @param layout New layout name or position list
    #' @return Self (for chaining)
    change_layout = function(layout) {
      if (is.list(layout) && !is.null(names(layout))) {
        self$positions <- layout
      } else {
        self$positions <- private$compute_layout(layout)
      }

      # Update vertex positions
      for (id in self$vertex_ids) {
        pos <- self$positions[[id]]
        if (!is.null(pos) && !is.null(self$vertices[[id]])) {
          self$vertices[[id]]$move_to(pos)
        }
      }

      # Rebuild edges
      for (el in self$edge_list) {
        key <- paste(el[1], el[2], sep = "->")
        if (!is.null(self$edges[[key]])) {
          from_pos <- self$positions[[el[1]]]
          to_pos <- self$positions[[el[2]]]
          if (!is.null(from_pos) && !is.null(to_pos)) {
            # Rebuild line points
            p0 <- from_pos
            p3 <- to_pos
            p1 <- p0 + (p3 - p0) / 3
            p2 <- p0 + 2 * (p3 - p0) / 3
            self$edges[[key]]$points <- rbind(p0, p1, p2, p3)
          }
        }
      }

      invisible(self)
    },

    #' @description Highlight a vertex
    #' @param id Vertex identifier
    #' @param color Highlight color
    #' @param scale Scale factor
    highlight_vertex = function(id, color = YELLOW, scale = 1.5) {
      v <- self$get_vertex(id)
      if (!is.null(v)) {
        v$set_color(color)
        v$scale(scale)
      }
      invisible(self)
    },

    #' @description Highlight an edge
    #' @param from Source vertex
    #' @param to Target vertex
    #' @param color Highlight color
    #' @param stroke_width Highlighted stroke width
    highlight_edge = function(from, to, color = YELLOW, stroke_width = 4) {
      e <- self$get_edge(from, to)
      if (!is.null(e)) {
        e$color <- color
        e$stroke_width <- stroke_width
      }
      invisible(self)
    },

    #' @description Convert to igraph object (requires igraph package)
    to_igraph = function() {
      if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("igraph package required for to_igraph()")
      }
      edge_vec <- unlist(self$edge_list)
      igraph::make_graph(edge_vec, directed = FALSE)
    }
  ),

  private = list(
    build_graph = function() {
      self$vertices <- list()
      self$edges <- list()

      # Create edges first (so they're behind vertices)
      for (el in self$edge_list) {
        from_pos <- self$positions[[el[1]]]
        to_pos <- self$positions[[el[2]]]
        if (is.null(from_pos) || is.null(to_pos)) next

        edge <- Line$new(
          start = from_pos,
          end = to_pos,
          color = self$edge_config$color,
          stroke_width = self$edge_config$stroke_width
        )
        edge$z_index <- -1
        key <- paste(el[1], el[2], sep = "->")
        self$edges[[key]] <- edge
        self$add_submobject(edge)
      }

      # Create vertices
      for (id in self$vertex_ids) {
        pos <- self$positions[[id]]
        if (is.null(pos)) pos <- ORIGIN

        v <- Dot$new(
          point = pos,
          radius = self$vertex_config$radius,
          color = self$vertex_config$color
        )
        v$fill_color <- self$vertex_config$fill_color
        v$fill_opacity <- self$vertex_config$fill_opacity
        v$z_index <- 1

        self$vertices[[id]] <- v
        self$add_submobject(v)
      }
    },

    add_labels = function(labels) {
      self$labels <- list()

      for (id in self$vertex_ids) {
        label_text <- if (isTRUE(labels)) {
          id
        } else if (is.list(labels) || is.character(labels)) {
          labels[[id]] %||% id
        } else {
          next
        }

        pos <- self$positions[[id]]
        if (is.null(pos)) next

        txt <- Text$new(
          text = as.character(label_text),
          font_size = 18,
          color = WHITE,
          position = pos
        )
        txt$z_index <- 2
        self$labels[[id]] <- txt
        self$add_submobject(txt)
      }
    },

    compute_layout = function(algorithm) {
      n <- length(self$vertex_ids)
      if (n == 0) return(list())

      # Try igraph first if available
      if (requireNamespace("igraph", quietly = TRUE) &&
          algorithm %in% c("spring", "kamada_kawai", "tree", "spectral")) {
        return(private$igraph_layout(algorithm))
      }

      # Built-in layouts
      switch(algorithm,
        "spring" = private$spring_layout(),
        "circular" = private$circular_layout(),
        "grid" = private$grid_layout(),
        "random" = private$random_layout(),
        "tree" = private$tree_layout(),
        private$circular_layout()  # default fallback
      )
    },

    circular_layout = function() {
      n <- length(self$vertex_ids)
      positions <- list()
      for (i in seq_len(n)) {
        angle <- (i - 1) / n * TAU
        positions[[self$vertex_ids[i]]] <- c(2 * cos(angle), 2 * sin(angle), 0)
      }
      positions
    },

    grid_layout = function() {
      n <- length(self$vertex_ids)
      cols <- ceiling(sqrt(n))
      positions <- list()
      for (i in seq_len(n)) {
        row <- (i - 1) %/% cols
        col <- (i - 1) %% cols
        x <- (col - (cols - 1) / 2) * 1.5
        y <- -(row - (ceiling(n / cols) - 1) / 2) * 1.5
        positions[[self$vertex_ids[i]]] <- c(x, y, 0)
      }
      positions
    },

    random_layout = function() {
      n <- length(self$vertex_ids)
      positions <- list()
      for (i in seq_len(n)) {
        positions[[self$vertex_ids[i]]] <- c(
          stats::runif(1, -3, 3),
          stats::runif(1, -2, 2),
          0
        )
      }
      positions
    },

    spring_layout = function() {
      # Fruchterman-Reingold force-directed layout
      n <- length(self$vertex_ids)
      if (n == 0) return(list())
      if (n == 1) return(stats::setNames(list(c(0, 0, 0)), self$vertex_ids))

      # Initialize randomly
      pos <- matrix(stats::runif(n * 2, -2, 2), ncol = 2)

      k <- sqrt(16 / n)  # optimal distance
      temp <- 2.0

      for (iter in 1:50) {
        disp <- matrix(0, nrow = n, ncol = 2)

        # Repulsive forces
        for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
            delta <- pos[i, ] - pos[j, ]
            dist <- max(sqrt(sum(delta^2)), 0.01)
            force <- k^2 / dist
            disp[i, ] <- disp[i, ] + (delta / dist) * force
            disp[j, ] <- disp[j, ] - (delta / dist) * force
          }
        }

        # Attractive forces (edges)
        for (el in self$edge_list) {
          i <- match(el[1], self$vertex_ids)
          j <- match(el[2], self$vertex_ids)
          if (is.na(i) || is.na(j)) next
          delta <- pos[i, ] - pos[j, ]
          dist <- max(sqrt(sum(delta^2)), 0.01)
          force <- dist^2 / k
          disp[i, ] <- disp[i, ] - (delta / dist) * force
          disp[j, ] <- disp[j, ] + (delta / dist) * force
        }

        # Apply displacement with temperature
        for (i in seq_len(n)) {
          d <- sqrt(sum(disp[i, ]^2))
          if (d > 0) {
            pos[i, ] <- pos[i, ] + (disp[i, ] / d) * min(d, temp)
          }
        }

        temp <- temp * 0.95
      }

      # Center
      pos[, 1] <- pos[, 1] - mean(pos[, 1])
      pos[, 2] <- pos[, 2] - mean(pos[, 2])

      # Scale to fit
      max_coord <- max(abs(pos))
      if (max_coord > 0) pos <- pos * (3 / max_coord)

      positions <- list()
      for (i in seq_len(n)) {
        positions[[self$vertex_ids[i]]] <- c(pos[i, ], 0)
      }
      positions
    },

    tree_layout = function() {
      # Simple hierarchical layout (BFS from first vertex)
      n <- length(self$vertex_ids)
      if (n == 0) return(list())

      # Build adjacency list
      adj <- list()
      for (id in self$vertex_ids) adj[[id]] <- character(0)
      for (el in self$edge_list) {
        adj[[el[1]]] <- c(adj[[el[1]]], el[2])
        adj[[el[2]]] <- c(adj[[el[2]]], el[1])
      }

      # BFS to assign levels
      root <- self$vertex_ids[1]
      visited <- character(0)
      queue <- list(list(id = root, level = 0))
      levels <- list()

      while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]
        if (current$id %in% visited) next
        visited <- c(visited, current$id)

        lev <- as.character(current$level)
        if (is.null(levels[[lev]])) levels[[lev]] <- character(0)
        levels[[lev]] <- c(levels[[lev]], current$id)

        for (nb in adj[[current$id]]) {
          if (!(nb %in% visited)) {
            queue[[length(queue) + 1]] <- list(id = nb, level = current$level + 1)
          }
        }
      }

      # Position by level
      positions <- list()
      max_level <- max(as.integer(names(levels)))
      for (lev_str in names(levels)) {
        lev <- as.integer(lev_str)
        ids <- levels[[lev_str]]
        n_in_level <- length(ids)
        for (i in seq_along(ids)) {
          x <- (i - (n_in_level + 1) / 2) * 2
          y <- -lev * 1.5 + max_level * 0.75
          positions[[ids[i]]] <- c(x, y, 0)
        }
      }

      # Add any unvisited vertices
      for (id in self$vertex_ids) {
        if (is.null(positions[[id]])) {
          positions[[id]] <- c(stats::runif(1, -2, 2), stats::runif(1, -2, 2), 0)
        }
      }

      positions
    },

    igraph_layout = function(algorithm) {
      g <- self$to_igraph()
      layout_func <- switch(algorithm,
        "spring" = igraph::layout_with_fr,
        "kamada_kawai" = igraph::layout_with_kk,
        "tree" = igraph::layout_as_tree,
        "spectral" = igraph::layout_with_drl,
        igraph::layout_with_fr
      )

      coords <- layout_func(g)
      # Scale to fit nicely
      max_coord <- max(abs(coords))
      if (max_coord > 0) coords <- coords * (3 / max_coord)

      positions <- list()
      for (i in seq_along(self$vertex_ids)) {
        positions[[self$vertex_ids[i]]] <- c(coords[i, ], 0)
      }
      positions
    }
  )
)


#' DiGraph R6 Class
#'
#' @description
#' A directed graph with Arrow edges instead of Lines.
#'
#' @export
DiGraph <- R6::R6Class(
  "DiGraph",
  inherit = Graph,

  public = list(
    #' @description Create a directed Graph
    #' @param vertices Vertex ids
    #' @param edges Directed edges (list of c(from, to))
    #' @param layout Layout algorithm
    #' @param ... Additional args
    initialize = function(vertices, edges, layout = "spring", ...) {
      super$initialize(vertices = vertices, edges = edges,
                       layout = layout, ...)
      self$type <- "digraph"

      # Replace Line edges with Arrow edges
      for (key in names(self$edges)) {
        old_edge <- self$edges[[key]]
        if (is.null(old_edge$points) || nrow(old_edge$points) < 4) next

        p0 <- old_edge$points[1, ]
        p3 <- old_edge$points[nrow(old_edge$points), ]

        arrow <- Arrow$new(
          start = p0,
          end = p3,
          color = self$edge_config$color,
          stroke_width = self$edge_config$stroke_width,
          buff = self$vertex_config$radius
        )
        arrow$z_index <- -1

        # Replace in submobjects
        idx <- which(vapply(self$submobjects, identical, logical(1), old_edge))
        if (length(idx) > 0) {
          self$submobjects[[idx[1]]] <- arrow
        }
        self$edges[[key]] <- arrow
      }
    },

    #' @description Convert to igraph object (directed)
    to_igraph = function() {
      if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("igraph package required")
      }
      edge_vec <- unlist(self$edge_list)
      igraph::make_graph(edge_vec, directed = TRUE)
    }
  )
)
