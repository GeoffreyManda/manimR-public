#' @title Linear Algebra Tutorial Module
#' @description
#' Interactive tutorial visualizing linear algebra concepts including
#' vectors, matrices, transformations, and eigen-decomposition.
#'
#' @name tutorial_linear_algebra
NULL

#' Tutorial: Linear Algebra
#'
#' @keywords internal
#' @noRd
tutorial_linear_algebra <- function() {
  cli::cli_h1("Tutorial: Linear Algebra through Visualization")

  cli::cli_h2("What we'll explore:")
  cli::cli_ul(
    "Vectors and their operations",
    "Matrix multiplication as transformation",
    "Eigenvalues and eigenvectors",
    "Linear transformations and their effects",
    "Vector spaces and bases"
  )
  cli::cli_text("")

  lessons <- c(
    "Vectors - What they are and how they work",
    "Vector Operations - Addition, scaling, dot product",
    "Matrices as Transformations",
    "Eigenvalues and Eigenvectors",
    "Linear Independence and Bases",
    "Return to Main Menu"
  )

  while (TRUE) {
    choice <- utils::menu(
      title = "Select a lesson:",
      choices = lessons
    )

    switch(choice,
      lesson_vectors(),
      lesson_vector_operations(),
      lesson_matrix_transformations(),
      lesson_eigenvalues(),
      lesson_bases(),
      return(invisible(NULL))
    )
  }
}

#' Lesson: Vectors
#'
#' @keywords internal
#' @noRd
lesson_vectors <- function() {
  cli::cli_h2("Lesson: Vectors")

  cli::cli_text("A vector is an object with magnitude (length) and direction.")
  cli::cli_text("In 2D, we write v = (x, y) or v = [x, y]")
  cli::cli_text("")
  cli::cli_text("Key concepts:")
  cli::cli_ul(
    "Origin: The tail of the vector (usually at (0,0))",
    "Head: The tip/point of the vector",
    "Components: The x and y coordinates",
    "Magnitude: Length of the vector = sqrt(x^2 + y^2)",
    "Direction: Angle from positive x-axis"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize vectors...")
  readline()

  s <- scene("vectors_intro", width = 854, height = 480)

  # Draw grid and axes
  s <- s %>%
    add_line(start = c(-4, 0), end = c(4, 0), color = "#666666", stroke_width = 1) %>%
    add_line(start = c(0, -3), end = c(0, 3), color = "#666666", stroke_width = 1)

  # Draw some example vectors
  vectors <- list(
    list(pos = c(3, 2), color = "#FF6B6B", label = "v1 = (3, 2)"),
    list(pos = c(-2, 1), color = "#4ECDC4", label = "v2 = (-2, 1)"),
    list(pos = c(1, -2), color = "#FFEAA7", label = "v3 = (1, -2)")
  )

  for (v in vectors) {
    s <- s %>%
      add_arrow(start = c(0, 0), end = v$pos, color = v$color, stroke_width = 4) %>%
      add_text(v$label, position = v$pos + c(0.3, 0.3), color = v$color, size = 18)
  }

  s <- s %>% add_text("Vectors in 2D Space", position = c(0, 3.5), size = 32)

  preview(s)

  cli::cli_h3("Magnitude Examples:")
  cli::cli_text("|v1| = sqrt(3^2 + 2^2) = sqrt13 ~ 3.61")
  cli::cli_text("|v2| = sqrt((-2)^2 + 1^2) = sqrt5 ~ 2.24")
  cli::cli_text("")

  cli::cli_text("Press Enter to continue...")
  readline()
  invisible(TRUE)
}

#' Lesson: Vector Operations
#'
#' @keywords internal
#' @noRd
lesson_vector_operations <- function() {
  cli::cli_h2("Lesson: Vector Operations")

  cli::cli_text("Vectors can be added, scaled, and combined!")
  cli::cli_text("")

  cli::cli_h3("1. Vector Addition (Triangle/Parallelogram Law)")
  cli::cli_text("v1 + v2 = (x1 + x2, y1 + y2)")
  cli::cli_text("Geometrically: place tail of v2 at head of v1")
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("vector_addition", width = 854, height = 480)

  v1 <- c(3, 1)
  v2 <- c(1, 2)
  v_sum <- v1 + v2

  s <- s %>%
    add_line(start = c(-1, 0), end = c(5, 0), color = "#666666") %>%
    add_line(start = c(0, -1), end = c(0, 4), color = "#666666") %>%
    # v1
    add_arrow(start = c(0, 0), end = v1, color = "#FF6B6B", stroke_width = 4) %>%
    add_text("v1", position = v1 + c(0.2, 0), color = "#FF6B6B", size = 20) %>%
    # v2 starting from origin
    add_arrow(start = c(0, 0), end = v2, color = "#4ECDC4", stroke_width = 4) %>%
    add_text("v2", position = v2 + c(0, 0.2), color = "#4ECDC4", size = 20) %>%
    # v2 starting from v1 (triangle law)
    add_arrow(start = v1, end = v_sum, color = "#4ECDC4", stroke_width = 2) %>%
    # sum
    add_arrow(start = c(0, 0), end = v_sum, color = "#FFEAA7", stroke_width = 5) %>%
    add_text("v1 + v2", position = v_sum + c(0.3, 0.3), color = "#FFEAA7", size = 20) %>%
    add_text("Vector Addition", position = c(2, 4), size = 32)

  preview(s)

  cli::cli_text("")
  cli::cli_h3("2. Scalar Multiplication")
  cli::cli_text("c*v = (c*x, c*y)")
  cli::cli_text("Changes magnitude, may flip direction if c < 0")
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s2 <- scene("scalar_multiplication", width = 854, height = 480)

  v_base <- c(1, 1)
  scales <- c(0.5, 1, 2, -1)
  colors <- c("#96CEB4", "#FFFFFF", "#FF6B6B", "#4ECDC4")

  s2 <- s2 %>%
    add_line(start = c(-3, 0), end = c(3, 0), color = "#666666") %>%
    add_line(start = c(0, -3), end = c(0, 3), color = "#666666")

  for (i in seq_along(scales)) {
    v_scaled <- scales[i] * v_base
    label <- sprintf("%.1f*v", scales[i])
    s2 <- s2 %>%
      add_arrow(start = c(0, 0), end = v_scaled, color = colors[i], stroke_width = 3) %>%
      add_text(label, position = v_scaled + c(0.2, 0.2), color = colors[i], size = 18)
  }

  s2 <- s2 %>% add_text("Scalar Multiplication", position = c(0, 3.5), size = 32)

  preview(s2)

  cli::cli_h3("3. Dot Product")
  cli::cli_text("v1 * v2 = x1*x2 + y1*y2 = |v1||v2|cos(theta)")
  cli::cli_text("Measures alignment between vectors")
  cli::cli_ul(
    "v1 * v2 > 0: vectors point in similar directions",
    "v1 * v2 = 0: vectors are perpendicular (orthogonal)",
    "v1 * v2 < 0: vectors point in opposite directions"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Matrix Transformations
#'
#' @keywords internal
#' @noRd
lesson_matrix_transformations <- function() {
  cli::cli_h2("Lesson: Matrices as Transformations")

  cli::cli_text("A matrix is a linear transformation!")
  cli::cli_text("When you multiply a matrix by a vector, you transform that vector.")
  cli::cli_text("")

  cli::cli_h3("Common 2D Transformations:")
  cli::cli_dl(
    "Rotation" = "[cos theta  -sin theta; sin theta  cos theta]",
    "Scaling" = "[sx  0; 0  sy]",
    "Shear" = "[1  sh; 0  1]",
    "Reflection" = "[1  0; 0  -1] (over x-axis)"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to see rotation...")
  readline()

  # Rotation demo
  s <- scene("matrix_rotation", width = 854, height = 480)

  # Unit circle points
  angles <- seq(0, 2*pi, length.out = 12)
  unit_points <- lapply(angles, function(a) c(cos(a), sin(a)))

  # Draw original grid
  s <- s %>%
    add_line(start = c(-2, 0), end = c(2, 0), color = "#444444") %>%
    add_line(start = c(0, -2), end = c(0, 2), color = "#444444")

  # Draw original vectors
  for (p in unit_points) {
    s <- s %>% add_line(start = c(0, 0), end = p * 1.5, color = "#666666", stroke_width = 1)
  }

  # Apply 45 degree rotation
  theta <- pi/4
  rotation_matrix <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)

  # Draw transformed vectors
  for (p in unit_points) {
    p_transformed <- as.vector(rotation_matrix %*% p)
    s <- s %>% add_arrow(start = c(0, 0), end = p_transformed * 1.5, color = "#4ECDC4", stroke_width = 2)
  }

  s <- s %>%
    add_text("Rotation by 45deg", position = c(0, 2.5), size = 32) %>%
    add_text("Original grid (gray) -> Rotated (cyan)", position = c(0, -2.5), size = 18)

  preview(s)

  cli::cli_text("")
  cli::cli_text("Notice: The grid lines stay straight and parallel!")
  cli::cli_text("This is the definition of LINEAR transformation.")
  cli::cli_text("")
  cli::cli_text("Press Enter to see scaling...")
  readline()

  # Scaling demo
  s2 <- scene("matrix_scaling", width = 854, height = 480)

  # Unit square
  square <- list(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))

  s2 <- s2 %>%
    add_line(start = c(-3, 0), end = c(4, 0), color = "#666666") %>%
    add_line(start = c(0, -1), end = c(0, 4), color = "#666666")

  # Draw original square
  for (i in 1:4) {
    s2 <- s2 %>% add_line(
      start = square[[i]],
      end = square[[i + 1]],
      color = "#666666",
      stroke_width = 2
    )
  }
  s2 <- s2 %>% add_text("Original", position = c(0.5, -0.3), color = "#666666", size = 16)

  # Apply scaling: stretch x by 2, y by 1.5
  scale_matrix <- matrix(c(2, 0, 0, 1.5), nrow = 2)
  square_scaled <- lapply(square, function(p) as.vector(scale_matrix %*% p))

  for (i in 1:4) {
    s2 <- s2 %>% add_line(
      start = square_scaled[[i]],
      end = square_scaled[[i + 1]],
      color = "#FF6B6B",
      stroke_width = 3
    )
  }
  s2 <- s2 %>% add_text("Scaled 2x in x, 1.5x in y", position = c(2, 1.5), color = "#FF6B6B", size = 18)

  preview(s2)

  cli::cli_h3("Key Insight")
  cli::cli_text("Matrix multiplication is composition of transformations!")
  cli::cli_text("A*B means: first apply B, then apply A")
  cli::cli_text("(Remember: matrix multiplication is not commutative)")

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Eigenvalues and Eigenvectors
#'
#' @keywords internal
#' @noRd
lesson_eigenvalues <- function() {
  cli::cli_h2("Lesson: Eigenvalues and Eigenvectors")

  cli::cli_text("Eigenvectors are special vectors that only get SCALED by a transformation.")
  cli::cli_text("They don't change direction (or flip 180deg if eigenvalue is negative).")
  cli::cli_text("")
  cli::cli_text("Definition: A*v = lambda*v")
  cli::cli_text("Where:")
  cli::cli_ul(
    "A = transformation matrix",
    "v = eigenvector (special direction)",
    "lambda (lambda) = eigenvalue (scaling factor)"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("eigenvalues", width = 854, height = 480)

  # Example: stretching transformation
  # Matrix [[2, 0], [0, 0.5]] stretches x by 2, compresses y by 0.5
  # Eigenvectors are (1, 0) with lambda=2 and (0, 1) with lambda=0.5

  s <- s %>%
    add_line(start = c(-3, 0), end = c(3, 0), color = "#666666") %>%
    add_line(start = c(0, -2), end = c(0, 2), color = "#666666")

  # Regular vector (will change direction)
  v_regular <- c(1, 1)
  v_regular_transformed <- c(2, 0.5)  # A %*% v_regular

  s <- s %>%
    add_arrow(start = c(0, 0), end = v_regular, color = "#888888", stroke_width = 2) %>%
    add_text("v", position = v_regular + c(0.1, 0.1), color = "#888888", size = 18) %>%
    add_arrow(start = c(0, 0), end = v_regular_transformed, color = "#FF6B6B", stroke_width = 3) %>%
    add_text("Av", position = v_regular_transformed + c(0.1, 0.1), color = "#FF6B6B", size = 18) %>%
    add_text("Regular vector: changes direction", position = c(1.5, 1.8), color = "#FF6B6B", size = 14)

  # Eigenvector 1: x-axis, lambda=2
  s <- s %>%
    add_arrow(start = c(0, 0), end = c(2.5, 0), color = "#4ECDC4", stroke_width = 4) %>%
    add_text("Eigenvector 1: lambda=2", position = c(1.5, -0.4), color = "#4ECDC4", size = 16)

  # Eigenvector 2: y-axis, lambda=0.5
  s <- s %>%
    add_arrow(start = c(0, 0), end = c(0, 1.5), color = "#FFEAA7", stroke_width = 4) %>%
    add_text("Eigenvector 2: lambda=0.5", position = c(0.5, 1), color = "#FFEAA7", size = 16)

  s <- s %>% add_text("Eigenvalues and Eigenvectors", position = c(0, 2.5), size = 28)

  preview(s)

  cli::cli_h3("Why Eigenvalues Matter:")
  cli::cli_ul(
    "Principal Component Analysis (PCA) - find important directions",
    "Google's PageRank - eigenvector of the web graph",
    "Stability analysis - eigenvalues determine system behavior",
    "Quantum mechanics - eigenvalues are observable quantities"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Bases
#'
#' @keywords internal
#' @noRd
lesson_bases <- function() {
  cli::cli_h2("Lesson: Linear Independence and Bases")

  cli::cli_text("A basis is a set of vectors that can 'reach' every point in space.")
  cli::cli_text("")

  cli::cli_h3("Standard Basis in 2D:")
  cli::cli_text("e1 = (1, 0) and e2 = (0, 1)")
  cli::cli_text("Any vector v = (x, y) = x*e1 + y*e2")
  cli::cli_text("")

  cli::cli_text("But other bases exist! Any two non-parallel vectors form a basis.")
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("bases", width = 854, height = 480)

  s <- s %>%
    add_line(start = c(-4, 0), end = c(4, 0), color = "#666666") %>%
    add_line(start = c(0, -3), end = c(0, 3), color = "#666666")

  # Standard basis
  s <- s %>%
    add_arrow(start = c(0, 0), end = c(2, 0), color = "#FF6B6B", stroke_width = 3) %>%
    add_text("e1", position = c(2.2, 0), color = "#FF6B6B", size = 20) %>%
    add_arrow(start = c(0, 0), end = c(0, 2), color = "#4ECDC4", stroke_width = 3) %>%
    add_text("e2", position = c(0, 2.2), color = "#4ECDC4", size = 20)

  # Alternative basis
  b1 <- c(2, 1)
  b2 <- c(1, 2)

  s <- s %>%
    add_arrow(start = c(0, 0), end = b1, color = "#FFEAA7", stroke_width = 3) %>%
    add_text("b1", position = b1 + c(0.2, 0.1), color = "#FFEAA7", size = 20) %>%
    add_arrow(start = c(0, 0), end = b2, color = "#96CEB4", stroke_width = 3) %>%
    add_text("b2", position = b2 + c(0.1, 0.2), color = "#96CEB4", size = 20)

  s <- s %>% add_text("Two Different Bases", position = c(0, 3.5), size = 32)

  preview(s)

  cli::cli_h3("Linear Independence")
  cli::cli_text("Vectors are linearly independent if none can be written as")
  cli::cli_text("a combination of the others.")
  cli::cli_text("")
  cli::cli_text("In 2D: Two vectors are independent if they're not parallel.")
  cli::cli_text("In 3D: Three vectors are independent if they don't lie in the same plane.")

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}
