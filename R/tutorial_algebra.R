#' @title Algebra Tutorial Module
#' @description
#' Interactive tutorial visualizing algebraic concepts including
#' equations, functions, and transformations.
#'
#' @name tutorial_algebra
NULL

#' Tutorial: Algebra
#'
#' @keywords internal
#' @noRd
tutorial_algebra <- function() {
  cli::cli_h1("Tutorial: Algebra through Visualization")

  cli::cli_h2("What we'll explore:")
  cli::cli_ul(
    "Visualizing functions and their properties",
    "Understanding equations geometrically",
    "Transformations and their effects",
    "Building intuition for algebraic operations"
  )
  cli::cli_text("")

  # Lesson selection
  lessons <- c(
    "Linear Functions - y = mx + b",
    "Quadratic Functions - y = ax^2 + bx + c",
    "Polynomial Functions",
    "Exponential Growth",
    "Function Transformations",
    "Solving Equations Visually",
    "Return to Main Menu"
  )

  while (TRUE) {
    choice <- utils::menu(
      title = "Select a lesson:",
      choices = lessons
    )

    switch(choice,
      lesson_linear_functions(),
      lesson_quadratic_functions(),
      lesson_polynomials(),
      lesson_exponential(),
      lesson_transformations(),
      lesson_solving_equations(),
      return(invisible(NULL))
    )
  }
}

#' Lesson: Linear Functions
#'
#' @keywords internal
#' @noRd
lesson_linear_functions <- function() {
  cli::cli_h2("Lesson: Linear Functions (y = mx + b)")

  cli::cli_text("A linear function has two key parameters:")
  cli::cli_dl(
    m = "Slope - how steep the line is",
    b = "Y-intercept - where the line crosses the y-axis"
  )
  cli::cli_text("")

  # Interactive exploration
  cli::cli_text("Let's explore how changing m affects the line.")
  cli::cli_text("Press Enter to visualize...")
  readline()

  # Create visualization of different slopes
  colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7")
  slopes <- c(0.5, 1, 2, -1, -0.5)

  s <- scene("linear_slopes", width = 854, height = 480)

  # Add axes
  s <- s %>%
    add_line(start = c(-5, 0), end = c(5, 0), color = "#666666") %>%
    add_line(start = c(0, -3), end = c(0, 3), color = "#666666")

  # Add labels
  s <- s %>% add_text("x", position = c(5.2, 0), size = 24)
  s <- s %>% add_text("y", position = c(0, 3.2), size = 24)

  # Add multiple lines with different slopes
  for (i in seq_along(slopes)) {
    m <- slopes[i]
    label <- sprintf("m = %.1f", m)

    s <- s %>%
      add_line(
        start = c(-4, m * (-4)),
        end = c(4, m * 4),
        color = colors[i],
        stroke_width = 3
      ) %>%
      add_text(label, position = c(3, m * 3 + 0.3), color = colors[i], size = 20)
  }

  s <- s %>%
    add_text("Linear Functions: y = mx", position = c(0, 3.5), size = 32)

  preview(s)
  cli::cli_alert_success("Visualization created!")
  cli::cli_text("")

  # Y-intercept demonstration
  cli::cli_text("Now let's see how 'b' (y-intercept) shifts the line up/down.")
  cli::cli_text("Press Enter to continue...")
  readline()

  s2 <- scene("linear_intercepts", width = 854, height = 480)

  intercepts <- c(-2, -1, 0, 1, 2)

  for (i in seq_along(intercepts)) {
    b <- intercepts[i]
    label <- sprintf("b = %.0f", b)

    s2 <- s2 %>%
      add_line(
        start = c(-4, b + (-4)),
        end = c(4, b + 4),
        color = colors[i],
        stroke_width = 3
      ) %>%
      add_text(label, position = c(3, b + 3 + 0.3), color = colors[i], size = 20)
  }

  s2 <- s2 %>%
    add_text("Parallel Lines: y = x + b", position = c(0, 3.5), size = 32)

  preview(s2)

  cli::cli_h3("Key Takeaways:")
  cli::cli_ul(
    "Slope (m) controls the steepness and direction",
    "Positive slope: line goes up to the right",
    "Negative slope: line goes down to the right",
    "Y-intercept (b) shifts the entire line up or down",
    "Parallel lines have the same slope but different intercepts"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return to the Algebra menu...")
  readline()
  invisible(TRUE)
}

#' Lesson: Quadratic Functions
#'
#' @keywords internal
#' @noRd
lesson_quadratic_functions <- function() {
  cli::cli_h2("Lesson: Quadratic Functions (y = ax^2 + bx + c)")

  cli::cli_text("Quadratic functions produce parabolas.")
  cli::cli_text("The coefficient 'a' determines the shape:")
  cli::cli_ul(
    "a > 0: opens upward",
    "a < 0: opens downward",
    "|a| > 1: narrower (steeper)",
    "|a| < 1: wider (flatter)"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize different parabolas...")
  readline()

  # Create visualization
  x_vals <- seq(-3, 3, length.out = 100)

  s <- scene("quadratic_shapes", width = 854, height = 480)

  # Add axes
  s <- s %>%
    add_line(start = c(-4, 0), end = c(4, 0), color = "#666666") %>%
    add_line(start = c(0, -1), end = c(0, 5), color = "#666666")

  # Different 'a' values
  a_values <- c(0.5, 1, 2, -1)
  colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")

  for (i in seq_along(a_values)) {
    a <- a_values[i]
    y_vals <- a * x_vals^2

    s <- s %>% add_path(
      x = x_vals,
      y = y_vals,
      color = colors[i],
      stroke_width = 3
    )

    label <- sprintf("a = %.1f", a)
    s <- s %>% add_text(label, position = c(2.5, a * 6), color = colors[i], size = 20)
  }

  s <- s %>% add_text("Quadratic Functions: y = ax^2", position = c(0, 5.5), size = 32)

  preview(s)

  cli::cli_h3("The Vertex Form: y = a(x - h)^2 + k")
  cli::cli_text("The vertex is at point (h, k)")
  cli::cli_text("This form makes it easy to see transformations!")
  cli::cli_text("")
  cli::cli_text("Press Enter to see vertex transformations...")
  readline()

  # Vertex transformation demo
  s2 <- scene("quadratic_vertex", width = 854, height = 480)

  # Start with basic parabola
  s2 <- s2 %>%
    add_function_plot(fun = function(x) x^2, xlim = c(-3, 3)) %>%
    add_text("y = x^2", position = c(2, 4), size = 24) %>%
    fade_in()

  # Shift right
  s2 <- s2 %>%
    pause(1) %>%
    add_function_plot(fun = function(x) (x-1)^2, xlim = c(-2, 4), color = "#FF6B6B") %>%
    add_text("y = (x-1)^2", position = c(3, 4), color = "#FF6B6B", size = 24) %>%
    fade_in()

  # Shift up
  s2 <- s2 %>%
    pause(1) %>%
    add_function_plot(fun = function(x) x^2 + 2, xlim = c(-3, 3), color = "#4ECDC4") %>%
    add_text("y = x^2 + 2", position = c(2, 6), color = "#4ECDC4", size = 24) %>%
    fade_in()

  preview(s2)

  cli::cli_h3("Key Takeaways:")
  cli::cli_ul(
    "'a' controls width and direction of opening",
    "Vertex form shows transformations clearly",
    "h shifts horizontally (opposite sign!)",
    "k shifts vertically",
    "The vertex is the minimum or maximum point"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Polynomials
#'
#' @keywords internal
#' @noRd
lesson_polynomials <- function() {
  cli::cli_h2("Lesson: Polynomial Functions")

  cli::cli_text("Polynomials are functions of the form:")
  cli::cli_text("P(x) = a_n*xn + a_{n-1}*x^{n-1} + ... + a1*x + a0")
  cli::cli_text("")
  cli::cli_text("The degree (n) determines many properties:")
  cli::cli_ul(
    "Degree 1: Linear (straight line, 1 root)",
    "Degree 2: Quadratic (parabola, up to 2 roots)",
    "Degree 3: Cubic (S-shape, up to 3 roots)",
    "Degree n: Up to n roots, n-1 turning points"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("polynomial_degrees", width = 854, height = 480)

  # Add axes
  s <- s %>%
    add_line(start = c(-3, 0), end = c(3, 0), color = "#666666") %>%
    add_line(start = c(0, -3), end = c(0, 3), color = "#666666")

  x_vals <- seq(-2, 2, length.out = 100)

  # Linear
  y_linear <- 0.5 * x_vals + 0.5
  s <- s %>% add_path(x_vals, y_linear, color = "#FF6B6B", stroke_width = 3)
  s <- s %>% add_text("Linear (deg 1)", position = c(-2, -1), color = "#FF6B6B", size = 18)

  # Quadratic
  y_quad <- x_vals^2 - 1
  s <- s %>% add_path(x_vals, y_quad, color = "#4ECDC4", stroke_width = 3)
  s <- s %>% add_text("Quadratic (deg 2)", position = c(1.5, 3), color = "#4ECDC4", size = 18)

  # Cubic
  y_cubic <- 0.3 * x_vals^3 - x_vals
  s <- s %>% add_path(x_vals, y_cubic, color = "#45B7D1", stroke_width = 3)
  s <- s %>% add_text("Cubic (deg 3)", position = c(-2, 2), color = "#45B7D1", size = 18)

  s <- s %>% add_text("Polynomials by Degree", position = c(0, 3.5), size = 32)

  preview(s)

  cli::cli_h3("Roots (Zeros)")
  cli::cli_text("Roots are where P(x) = 0 (where the curve crosses the x-axis)")
  cli::cli_text("The Fundamental Theorem of Algebra states:")
  cli::cli_text("A degree n polynomial has exactly n complex roots")
  cli::cli_text("(counting multiplicity)")

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Exponential Growth
#'
#' @keywords internal
#' @noRd
lesson_exponential <- function() {
  cli::cli_h2("Lesson: Exponential Growth")

  cli::cli_text("Exponential functions have the form: y = a*b^x")
  cli::cli_text("where b > 0, b != 1")
  cli::cli_text("")
  cli::cli_text("Key properties:")
  cli::cli_ul(
    "b > 1: Exponential growth",
    "0 < b < 1: Exponential decay",
    "a is the initial value (y-intercept)",
    "The function doubles/halves in constant time intervals"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("exponential", width = 854, height = 480)

  x_vals <- seq(-3, 3, length.out = 100)

  # Growth
  y_growth <- 2^x_vals
  # Cap for visualization
  y_growth <- pmin(y_growth, 10)

  # Decay
  y_decay <- 0.5^x_vals

  s <- s %>%
    add_line(start = c(-3, 0), end = c(3, 0), color = "#666666") %>%
    add_line(start = c(0, -0.5), end = c(0, 10), color = "#666666") %>%
    add_path(x_vals, y_growth, color = "#50FA7B", stroke_width = 3) %>%
    add_text("Growth: y = 2^x", position = c(2, 8), color = "#50FA7B", size = 20) %>%
    add_path(x_vals, y_decay, color = "#FF6B6B", stroke_width = 3) %>%
    add_text("Decay: y = (1/2)^x", position = c(2, 0.3), color = "#FF6B6B", size = 20) %>%
    add_text("Exponential Functions", position = c(0, 10.5), size = 32)

  preview(s)

  cli::cli_h3("The Magic Number: e")
  cli::cli_text("e ~ 2.71828 is the natural base")
  cli::cli_text("The function y = e^x is its own derivative!")
  cli::cli_text("This makes it incredibly important in calculus and nature")

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Transformations
#'
#' @keywords internal
#' @noRd
lesson_transformations <- function() {
  cli::cli_h2("Lesson: Function Transformations")

  cli::cli_text("Given a base function f(x), we can transform it:")
  cli::cli_dl(
    "f(x) + k" = "Shift up by k units",
    "f(x) - k" = "Shift down by k units",
    "f(x + h)" = "Shift left by h units",
    "f(x - h)" = "Shift right by h units",
    "a*f(x)" = "Vertical stretch by factor a",
    "f(b*x)" = "Horizontal compression by factor b",
    "-f(x)" = "Reflect over x-axis",
    "f(-x)" = "Reflect over y-axis"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to see transformations in action...")
  readline()

  s <- scene("transformations", width = 854, height = 480)

  # Base function: sine wave
  x_vals <- seq(0, 4*pi, length.out = 200)
  y_base <- sin(x_vals)

  s <- s %>%
    add_line(start = c(0, 0), end = c(4*pi, 0), color = "#666666") %>%
    add_path(x_vals, y_base, color = "#FFFFFF", stroke_width = 2) %>%
    add_text("y = sin(x) [base]", position = c(10, 0.5), color = "#FFFFFF", size = 18) %>%
    fade_in()

  # Vertical shift
  s <- s %>%
    pause(0.5) %>%
    add_path(x_vals, y_base + 1.5, color = "#4ECDC4", stroke_width = 2) %>%
    add_text("y = sin(x) + 1.5", position = c(10, 2), color = "#4ECDC4", size = 18) %>%
    fade_in()

  # Amplitude change
  s <- s %>%
    pause(0.5) %>%
    add_path(x_vals, 2 * y_base, color = "#FF6B6B", stroke_width = 2) %>%
    add_text("y = 2*sin(x)", position = c(10, -1.5), color = "#FF6B6B", size = 18) %>%
    fade_in()

  # Frequency change
  s <- s %>%
    pause(0.5) %>%
    add_path(x_vals, sin(2 * x_vals), color = "#FFEAA7", stroke_width = 2) %>%
    add_text("y = sin(2x)", position = c(10, -2.5), color = "#FFEAA7", size = 18) %>%
    fade_in()

  s <- s %>% add_text("Function Transformations", position = c(6, 3), size = 32)

  preview(s)

  cli::cli_h3("Key Insight")
  cli::cli_text("Transformations INSIDE f() affect x (horizontal)")
  cli::cli_text("Transformations OUTSIDE f() affect y (vertical)")
  cli::cli_text("And they often work opposite to intuition (x + h shifts left!)")

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}

#' Lesson: Solving Equations Visually
#'
#' @keywords internal
#' @noRd
lesson_solving_equations <- function() {
  cli::cli_h2("Lesson: Solving Equations Visually")

  cli::cli_text("An equation like f(x) = g(x) can be solved by:")
  cli::cli_text("Finding where the two functions intersect!")
  cli::cli_text("")
  cli::cli_text("Example: Find x where x^2 = 2x + 3")
  cli::cli_text("")
  cli::cli_text("Press Enter to visualize...")
  readline()

  s <- scene("solving_equations", width = 854, height = 480)

  x_vals <- seq(-3, 4, length.out = 100)
  y_quad <- x_vals^2
  y_linear <- 2 * x_vals + 3

  # Find intersection points (x = -1 and x = 3)
  intersect_x <- c(-1, 3)
  intersect_y <- c(1, 9)

  s <- s %>%
    add_line(start = c(-3, 0), end = c(4, 0), color = "#666666") %>%
    add_line(start = c(0, -1), end = c(0, 10), color = "#666666") %>%
    add_path(x_vals, y_quad, color = "#4ECDC4", stroke_width = 3) %>%
    add_text("y = x^2", position = c(3.5, 9), color = "#4ECDC4", size = 24) %>%
    add_path(x_vals, y_linear, color = "#FF6B6B", stroke_width = 3) %>%
    add_text("y = 2x + 3", position = c(3.5, 8), color = "#FF6B6B", size = 24) %>%
    add_circle(radius = 0.15, position = c(-1, 1), color = "#FFEAA7", fill_opacity = 1) %>%
    add_circle(radius = 0.15, position = c(3, 9), color = "#FFEAA7", fill_opacity = 1) %>%
    add_text("Solutions: x = -1, x = 3", position = c(0.5, -0.8), color = "#FFEAA7", size = 22)

  preview(s)

  cli::cli_h3("Visual Problem Solving")
  cli::cli_text("This geometric view helps understand:")
  cli::cli_ul(
    "Why some equations have 0, 1, or 2 solutions",
    "Approximate solutions when exact is hard",
    "System of equations (find mutual intersections)",
    "Inequalities (which function is on top?)"
  )

  cli::cli_text("")
  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}
