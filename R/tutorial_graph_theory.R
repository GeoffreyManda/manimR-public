#' @title Graph Theory Tutorial Module
#' @description
#' Interactive tutorial visualizing graph theory concepts including
#' graphs, traversals, shortest paths, and graph algorithms.
#'
#' @name tutorial_graph_theory
NULL

#' Tutorial: Graph Theory
#'
#' @keywords internal
#' @noRd
tutorial_graph_theory <- function() {
  cli::cli_h1("Tutorial: Graph Theory through Visualization")

  cli::cli_h2("What we'll explore:")
  cli::cli_ul(
    "Graphs, vertices, and edges",
    "Types of graphs (directed, weighted, bipartite)",
    "Graph traversals (BFS, DFS)",
    "Shortest path algorithms",
    "Minimum spanning trees",
    "Graph coloring"
  )
  cli::cli_text("")

  lessons <- c(
    "Introduction to Graphs",
    "Graph Representations",
    "Breadth-First Search (BFS)",
    "Depth-First Search (DFS)",
    "Dijkstra's Shortest Path",
    "Minimum Spanning Tree",
    "Return to Main Menu"
  )

  while (TRUE) {
    choice <- utils::menu(
      title = "Select a lesson:",
      choices = lessons
    )

    switch(choice,
      lesson_graph_intro(),
      lesson_graph_representations(),
      lesson_bfs(),
      lesson_dfs(),
      lesson_dijkstra(),
      lesson_mst(),
      return(invisible(NULL))
    )
  }
}

#' Lesson: Graph Introduction
#'
#' @keywords internal
#' @noRd
lesson_graph_intro <- function() {
  cli::cli_h2("Lesson: Introduction to Graphs")

  cli::cli_text("A graph is a collection of:")
  cli::cli_dl(
    Vertices = "Nodes or points (also called 'nodes')",
    Edges = "Connections between vertices"
  )
  cli::cli_text("")

  cli::cli_h3("Types of Graphs:")
  cli::cli_ul(
    "Undirected: edges have no direction (like a friendship)",
    "Directed: edges have direction (like a one-way street)",
    "Weighted: edges have values/costs (like distances)",
    "Unweighted: all edges are equal"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("graph_intro", width = 854, height = 480)

  # Create a simple undirected graph
  vertices <- list(
    A = c(-2, 1),
    B = c(0, 2),
    C = c(2, 1),
    D = c(1, -1),
    E = c(-1, -1)
  )

  edges <- list(
    c("A", "B"), c("B", "C"), c("C", "D"), c("D", "E"),
    c("E", "A"), c("A", "D"), c("B", "E")
  )

  # Draw edges
  for (e in edges) {
    v1 <- vertices[[e[1]]]
    v2 <- vertices[[e[2]]]
    s <- s %>% add_line(start = v1, end = v2, color = "#666666", stroke_width = 2)
  }

  # Draw vertices
  for (name in names(vertices)) {
    pos <- vertices[[name]]
    s <- s %>%
      add_circle(radius = 0.3, position = pos, fill_color = "#4ECDC4", fill_opacity = 1) %>%
      add_text(name, position = pos, color = "#000000", size = 20)
  }

  s <- s %>% add_text("Undirected Graph", position = c(0, 3), size = 32)

  preview(s)

  cli::cli_h3("Key Terminology:")
  cli::cli_dl(
    Degree = "Number of edges connected to a vertex",
    Path = "Sequence of vertices connected by edges",
    Cycle = "Path that starts and ends at the same vertex",
    Connected = "Graph where every vertex can reach every other"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to continue...")
  readline()

  # Directed graph example
  cli::cli_h3("Directed Graph (Digraph)")
  cli::cli_text("Edges have arrows indicating direction")
  cli::cli_text("")

  s2 <- scene("digraph_intro", width = 854, height = 480)

  # Draw directed edges with arrows
  dir_edges <- list(
    c("A", "B"), c("B", "C"), c("C", "D"), c("D", "A"), c("A", "E")
  )

  for (e in dir_edges) {
    v1 <- vertices[[e[1]]]
    v2 <- vertices[[e[2]]]
    s2 <- s2 %>% add_arrow(start = v1, end = v2, color = "#FF6B6B", stroke_width = 2)
  }

  for (name in names(vertices)) {
    pos <- vertices[[name]]
    s2 <- s2 %>%
      add_circle(radius = 0.3, position = pos, fill_color = "#FFEAA7", fill_opacity = 1) %>%
      add_text(name, position = pos, color = "#000000", size = 20)
  }

  s2 <- s2 %>% add_text("Directed Graph", position = c(0, 3), size = 32)

  preview(s2)

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Graph Representations
#'
#' @keywords internal
#' @noRd
lesson_graph_representations <- function() {
  cli::cli_h2("Lesson: Graph Representations")

  cli::cli_text("How do we store graphs in a computer?")
  cli::cli_text("")

  cli::cli_h3("1. Adjacency Matrix")
  cli::cli_text("A VxV matrix where M[i,j] = 1 if edge (i,j) exists")
  cli::cli_text("")
  cli::cli_text("Example for a 4-vertex graph:")
  cli::cli_code(
    "    A  B  C  D",
    "A [ 0  1  0  1 ]",
    "B [ 1  0  1  0 ]",
    "C [ 0  1  0  1 ]",
    "D [ 1  0  1  0 ]"
  )
  cli::cli_text("")
  cli::cli_text("Pros: Fast edge lookup O(1)")
  cli::cli_text("Cons: Uses O(V^2) space")
  cli::cli_text("")

  cli::cli_h3("2. Adjacency List")
  cli::cli_text("Each vertex stores a list of its neighbors")
  cli::cli_text("")
  cli::cli_code(
    "A -> [B, D]",
    "B -> [A, C]",
    "C -> [B, D]",
    "D -> [A, C]"
  )
  cli::cli_text("")
  cli::cli_text("Pros: Space efficient O(V + E)")
  cli::cli_text("Cons: Slower edge lookup")
  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: BFS
#'
#' @keywords internal
#' @noRd
lesson_bfs <- function() {
  cli::cli_h2("Lesson: Breadth-First Search (BFS)")

  cli::cli_text("BFS explores a graph level by level.")
  cli::cli_text("Starting from a source vertex, it visits all neighbors,")
  cli::cli_text("then their neighbors, and so on.")
  cli::cli_text("")

  cli::cli_h3("Algorithm:")
  cli::cli_ol(
    "Start at source vertex",
    "Visit all unvisited neighbors",
    "For each neighbor, add their unvisited neighbors to queue",
    "Repeat until queue is empty"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize BFS...")
  readline()

  s <- scene("bfs_demo", width = 854, height = 480)

  # Create tree-like graph
  vertices <- list(
    A = c(0, 2),      # root
    B = c(-2, 0),     # level 1
    C = c(2, 0),      # level 1
    D = c(-3, -2),    # level 2
    E = c(-1, -2),    # level 2
    F = c(1, -2),     # level 2
    G = c(3, -2)      # level 2
  )

  edges <- list(
    c("A", "B"), c("A", "C"),
    c("B", "D"), c("B", "E"),
    c("C", "F"), c("C", "G")
  )

  # Draw all edges lightly
  for (e in edges) {
    v1 <- vertices[[e[1]]]
    v2 <- vertices[[e[2]]]
    s <- s %>% add_line(start = v1, end = v2, color = "#333333", stroke_width = 1)
  }

  # Draw vertices with different colors for BFS levels
  colors <- c("#FF6B6B", "#4ECDC4", "#FFEAA7")
  levels <- c(A = 0, B = 1, C = 1, D = 2, E = 2, F = 2, G = 2)

  for (name in names(vertices)) {
    pos <- vertices[[name]]
    level <- levels[[name]]
    s <- s %>%
      add_circle(radius = 0.25, position = pos,
                 fill_color = colors[level + 1], fill_opacity = 1) %>%
      add_text(name, position = pos, color = "#000000", size = 18)
  }

  # Add BFS order labels
  s <- s %>%
    add_text("Level 0", position = c(0.5, 2), color = "#FF6B6B", size = 14) %>%
    add_text("Level 1", position = c(-2.5, 0), color = "#4ECDC4", size = 14) %>%
    add_text("Level 2", position = c(-3.5, -2), color = "#FFEAA7", size = 14) %>%
    add_text("BFS explores level by level", position = c(0, 3), size = 28)

  preview(s)

  cli::cli_h3("Applications of BFS:")
  cli::cli_ul(
    "Finding shortest path in unweighted graphs",
    "Web crawling (explore pages level by level)",
    "Social networks (finding friends of friends)",
    "GPS navigation (finding shortest route)"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: DFS
#'
#' @keywords internal
#' @noRd
lesson_dfs <- function() {
  cli::cli_h2("Lesson: Depth-First Search (DFS)")

  cli::cli_text("DFS explores as far as possible along each branch")
  cli::cli_text("before backtracking.")
  cli::cli_text("")

  cli::cli_h3("Algorithm (recursive):")
  cli::cli_ol(
    "Mark current vertex as visited",
    "For each unvisited neighbor:",
    "  Recursively visit that neighbor",
    "Backtrack when no unvisited neighbors remain"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize DFS...")
  readline()

  s <- scene("dfs_demo", width = 854, height = 480)

  # Create a graph
  vertices <- list(
    A = c(0, 2),
    B = c(-2, 0),
    C = c(2, 0),
    D = c(-3, -2),
    E = c(-1, -2),
    F = c(0, -2)
  )

  edges <- list(
    c("A", "B"), c("A", "C"),
    c("B", "D"), c("B", "E"),
    c("E", "F")
  )

  # Draw edges showing DFS path in highlight
  dfs_path <- list(c("A", "B"), c("B", "D"), c("B", "E"), c("E", "F"), c("A", "C"))

  for (e in edges) {
    v1 <- vertices[[e[1]]]
    v2 <- vertices[[e[2]]]
    is_in_path <- any(sapply(dfs_path, function(p) all(p == e)))

    color <- if (is_in_path) "#4ECDC4" else "#333333"
    width <- if (is_in_path) 4 else 1

    s <- s %>% add_line(start = v1, end = v2, color = color, stroke_width = width)
  }

  for (name in names(vertices)) {
    pos <- vertices[[name]]
    s <- s %>%
      add_circle(radius = 0.25, position = pos,
                 fill_color = "#FFEAA7", fill_opacity = 1) %>%
      add_text(name, position = pos, color = "#000000", size = 18)
  }

  # Add traversal order
  s <- s %>%
    add_text("DFS Order: A -> B -> D -> E -> F -> C",
             position = c(0, -3), color = "#4ECDC4", size = 18) %>%
    add_text("DFS goes deep before going wide", position = c(0, 3), size = 28)

  preview(s)

  cli::cli_h3("Applications of DFS:")
  cli::cli_ul(
    "Detecting cycles in graphs",
    "Topological sorting",
    "Finding connected components",
    "Solving mazes and puzzles",
    "Expression parsing"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Dijkstra
#'
#' @keywords internal
#' @noRd
lesson_dijkstra <- function() {
  cli::cli_h2("Lesson: Dijkstra's Shortest Path Algorithm")

  cli::cli_text("Finds the shortest path from a source to all other vertices")
  cli::cli_text("in a weighted graph with non-negative edge weights.")
  cli::cli_text("")

  cli::cli_h3("Key Idea:")
  cli::cli_text("Always expand the vertex with the smallest known distance.")
  cli::cli_text("Use a priority queue (min-heap) for efficiency.")
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("dijkstra_demo", width = 854, height = 480)

  # Create weighted graph
  vertices <- list(
    S = c(-3, 0),   # source
    A = c(-1, 1),
    B = c(-1, -1),
    C = c(1, 1),
    D = c(1, -1),
    T = c(3, 0)     # target
  )

  # Draw edges with weights
  weighted_edges <- list(
    list(v = c("S", "A"), w = 4),
    list(v = c("S", "B"), w = 2),
    list(v = c("A", "C"), w = 1),
    list(v = c("B", "C"), w = 3),
    list(v = c("B", "D"), w = 2),
    list(v = c("C", "T"), w = 3),
    list(v = c("D", "T"), w = 1)
  )

  # Highlight shortest path
  shortest_path <- c("S", "B", "D", "T")

  for (edge in weighted_edges) {
    v1 <- vertices[[edge$v[1]]]
    v2 <- vertices[[edge$v[2]]]
    is_in_path <- any(sapply(1:(length(shortest_path)-1), function(i) {
      all(edge$v == c(shortest_path[i], shortest_path[i+1]))
    }))

    color <- if (is_in_path) "#50FA7B" else "#666666"
    width <- if (is_in_path) 4 else 2

    s <- s %>% add_line(start = v1, end = v2, color = color, stroke_width = width)

    # Add weight label
    mid <- (v1 + v2) / 2
    s <- s %>% add_text(as.character(edge$w), position = mid + c(0.1, 0.1),
                        color = "#FFFFFF", size = 14)
  }

  for (name in names(vertices)) {
    pos <- vertices[[name]]
    is_source <- name == "S"
    is_target <- name == "T"

    fill <- if (is_source) "#FF6B6B" else if (is_target) "#4ECDC4" else "#FFEAA7"

    s <- s %>%
      add_circle(radius = 0.3, position = pos, fill_color = fill, fill_opacity = 1) %>%
      add_text(name, position = pos, color = "#000000", size = 20)
  }

  s <- s %>%
    add_text("Shortest Path: S->B->D->T (cost: 5)",
             position = c(0, -2.5), color = "#50FA7B", size = 20) %>%
    add_text("Dijkstra's Algorithm", position = c(0, 2.5), size = 28)

  preview(s)

  cli::cli_h3("How it works:")
  cli::cli_ol(
    "Initialize: distance[source] = 0, all others = infinity",
    "Add source to priority queue",
    "While queue not empty:",
    "  Extract vertex with minimum distance",
    "  For each neighbor: relax edge (update if shorter path found)",
    "  Add neighbor to queue if updated"
  )

  cli::cli_text("")
  cli::cli_text("Time complexity: O((V + E) log V) with binary heap")

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: MST
#'
#' @keywords internal
#' @noRd
lesson_mst <- function() {
  cli::cli_h2("Lesson: Minimum Spanning Tree (MST)")

  cli::cli_text("A spanning tree connects all vertices with minimum total edge weight.")
  cli::cli_text("Useful for network design (cables, roads, pipelines).")
  cli::cli_text("")

  cli::cli_h3("Kruskal's Algorithm:")
  cli::cli_ol(
    "Sort all edges by weight (ascending)",
    "Add edges one by one, skipping those that form cycles",
    "Stop when we have V-1 edges"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize MST...")
  readline()

  s <- scene("mst_demo", width = 854, height = 480)

  # Create graph
  vertices <- list(
    A = c(-2, 1),
    B = c(0, 2),
    C = c(2, 1),
    D = c(2, -1),
    E = c(0, -2),
    F = c(-2, -1)
  )

  # All edges
  all_edges <- list(
    list(v = c("A", "B"), w = 4, in_mst = TRUE),
    list(v = c("B", "C"), w = 8, in_mst = FALSE),
    list(v = c("C", "D"), w = 7, in_mst = TRUE),
    list(v = c("D", "E"), w = 9, in_mst = FALSE),
    list(v = c("E", "F"), w = 10, in_mst = FALSE),
    list(v = c("F", "A"), w = 2, in_mst = TRUE),
    list(v = c("B", "F"), w = 1, in_mst = TRUE),
    list(v = c("C", "F"), w = 3, in_mst = TRUE),
    list(v = c("D", "F"), w = 5, in_mst = FALSE)
  )

  # Draw all edges
  for (edge in all_edges) {
    v1 <- vertices[[edge$v[1]]]
    v2 <- vertices[[edge$v[2]]]

    color <- if (edge$in_mst) "#50FA7B" else "#333333"
    width <- if (edge$in_mst) 4 else 1

    s <- s %>% add_line(start = v1, end = v2, color = color, stroke_width = width)
  }

  for (name in names(vertices)) {
    pos <- vertices[[name]]
    s <- s %>%
      add_circle(radius = 0.25, position = pos,
                 fill_color = "#FFEAA7", fill_opacity = 1) %>%
      add_text(name, position = pos, color = "#000000", size = 18)
  }

  s <- s %>%
    add_text("Green edges = MST (total weight: 17)",
             position = c(0, 3), color = "#50FA7B", size = 20) %>%
    add_text("Minimum Spanning Tree", position = c(0, -3), size = 28)

  preview(s)

  cli::cli_h3("Properties of MST:")
  cli::cli_ul(
    "Always contains exactly V-1 edges",
    "No cycles (it's a tree!)",
    "Minimizes total edge weight among all spanning trees",
    "May not be unique if equal edge weights exist"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}
