#' @title ggplot2 and Data Integration
#' @description
#' Functions for integrating with ggplot2, creating data-driven animations,
#' and working with tidy data.
#' @name data-integration
NULL

#' Convert a ggplot to a manimR scene
#'
#' @description
#' Converts a ggplot2 plot object into a manimR scene that can be animated.
#'
#' @param plot A ggplot2 plot object
#' @param name Scene name
#' @param animate_build Whether to animate the plot being built
#' @param duration Duration for build animation
#'
#' @return A Scene object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#'
#' convert_ggplot_to_manim(p) %>%
#'   render()
#' }
#'
#' @export
convert_ggplot_to_manim <- function(plot,
                                    name = "ggplot_animation",
                                    animate_build = TRUE,
                                    duration = 2) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for this function")
  }

  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a ggplot2 object")
  }

  # Create scene
  s <- scene(name)

  # Extract plot data
  plot_data <- ggplot2::ggplot_build(plot)

  # For now, we'll add the plot as an image

  # Full implementation would extract layers and animate them
  temp_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(temp_file, plot, width = 10, height = 6, dpi = 150)

  s %>%
    add_image(temp_file, width = 6, height = 4)

  if (animate_build) {
    s %>% fade_in(duration = duration)
  }

  s
}

#' Create an animated scatter plot
#'
#' @description
#' Creates an animated scatter plot from a data frame.
#'
#' @param data A data frame
#' @param x Column for x-axis (unquoted)
#' @param y Column for y-axis (unquoted)
#' @param size Column for point size (optional)
#' @param color Column for point color (optional)
#' @param group Column for grouping (optional)
#' @param transition Type of transition animation
#' @param duration Animation duration
#' @param name Scene name
#'
#' @return A Scene object
#'
#' @examples
#' \dontrun{
#' mtcars %>%
#'   animate_scatter(mpg, wt, color = cyl) %>%
#'   render()
#' }
#'
#' @export
animate_scatter <- function(data,
                            x,
                            y,
                            size = NULL,
                            color = NULL,
                            group = NULL,
                            transition = "grow",
                            duration = 2,
                            name = "scatter_animation") {

  # Capture column names
  x_col <- rlang::enquo(x)
  y_col <- rlang::enquo(y)
  size_col <- rlang::enquo(size)
  color_col <- rlang::enquo(color)

  # Extract data
  x_data <- rlang::eval_tidy(x_col, data)
  y_data <- rlang::eval_tidy(y_col, data)

  # Normalize to scene coordinates (-6 to 6)
  x_norm <- (x_data - min(x_data)) / (max(x_data) - min(x_data)) * 10 - 5
  y_norm <- (y_data - min(y_data)) / (max(y_data) - min(y_data)) * 6 - 3

  # Create scene
  s <- scene(name)

  # Add axis labels
  x_label <- rlang::as_name(x_col)
  y_label <- rlang::as_name(y_col)

  s %>%
    add_text(x_label, position = c(0, -4, 0), size = 24) %>%
    fade_in(duration = 0.5) %>%
    add_text(y_label, position = c(-6, 0, 0), size = 24) %>%
    fade_in(duration = 0.5)

  # Calculate stagger delay
  n_points <- length(x_norm)
  delay_per_point <- duration / n_points

  # Add points
  for (i in seq_along(x_norm)) {
    point_radius <- 0.1  # Could be dynamic based on size_col

    s %>%
      add_circle(
        radius = point_radius,
        position = c(x_norm[i], y_norm[i], 0),
        fill_color = "#4CAF50",
        color = "#4CAF50"
      ) %>%
      grow_from_center(duration = delay_per_point)
  }

  s
}

#' Create an animated bar chart
#'
#' @description
#' Creates an animated bar chart from a data frame.
#'
#' @param data A data frame
#' @param x Column for categories (x-axis)
#' @param y Column for values (y-axis)
#' @param fill Column for bar fill color (optional)
#' @param transition Type of transition ("grow", "fade")
#' @param duration Animation duration
#' @param name Scene name
#'
#' @return A Scene object
#'
#' @examples
#' \dontrun{
#' data.frame(category = c("A", "B", "C"), value = c(10, 25, 15)) %>%
#'   animate_bar_chart(category, value) %>%
#'   render()
#' }
#'
#' @export
animate_bar_chart <- function(data,
                              x,
                              y,
                              fill = NULL,
                              transition = "grow",
                              duration = 2,
                              name = "bar_animation") {

  x_col <- rlang::enquo(x)
  y_col <- rlang::enquo(y)

  x_data <- rlang::eval_tidy(x_col, data)
  y_data <- rlang::eval_tidy(y_col, data)

  n_bars <- length(x_data)

  # Calculate bar positions and sizes
  bar_width <- 10 / (n_bars * 1.5)
  spacing <- bar_width * 0.5
  total_width <- n_bars * bar_width + (n_bars - 1) * spacing
  start_x <- -total_width / 2 + bar_width / 2

  # Normalize heights
  max_y <- max(y_data)
  y_norm <- y_data / max_y * 5

  # Create scene
  s <- scene(name)

  # Add title
  x_label <- rlang::as_name(x_col)
  y_label <- rlang::as_name(y_col)

  s %>%
    add_text(paste(y_label, "by", x_label),
             position = c(0, 4, 0), size = 36) %>%
    fade_in(duration = 0.5)

  # Add bars
  delay_per_bar <- (duration - 0.5) / n_bars

  for (i in seq_along(x_data)) {
    bar_x <- start_x + (i - 1) * (bar_width + spacing)
    bar_height <- y_norm[i]
    bar_y <- -3 + bar_height / 2

    # Add bar
    s %>%
      add_rectangle(
        width = bar_width,
        height = bar_height,
        position = c(bar_x, bar_y, 0),
        fill_color = "#2196F3",
        color = "#1976D2"
      ) %>%
      grow_from_center(duration = delay_per_bar * 0.8)

    # Add label
    s %>%
      add_text(as.character(x_data[i]),
               position = c(bar_x, -3.5, 0),
               size = 18) %>%
      fade_in(duration = delay_per_bar * 0.2)
  }

  s
}

#' Create an animated line chart
#'
#' @description
#' Creates an animated line chart that draws progressively.
#'
#' @param data A data frame
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param group Column for grouping multiple lines (optional)
#' @param color Color for the line
#' @param duration Animation duration
#' @param name Scene name
#'
#' @return A Scene object
#'
#' @export
animate_line_chart <- function(data,
                               x,
                               y,
                               group = NULL,
                               color = "#4CAF50",
                               duration = 2,
                               name = "line_animation") {

  x_col <- rlang::enquo(x)
  y_col <- rlang::enquo(y)

  x_data <- rlang::eval_tidy(x_col, data)
  y_data <- rlang::eval_tidy(y_col, data)

  # Sort by x
  ord <- order(x_data)
  x_data <- x_data[ord]
  y_data <- y_data[ord]

  # Normalize
  x_norm <- (x_data - min(x_data)) / (max(x_data) - min(x_data)) * 10 - 5
  y_norm <- (y_data - min(y_data)) / (max(y_data) - min(y_data)) * 6 - 3

  # Create scene
  s <- scene(name)

  # Add axis labels
  s %>%
    add_text(rlang::as_name(x_col), position = c(0, -4, 0), size = 24) %>%
    fade_in(duration = 0.3) %>%
    add_text(rlang::as_name(y_col), position = c(-6, 0, 0), size = 24) %>%
    fade_in(duration = 0.3)

  # Add line segments progressively
  n_segments <- length(x_norm) - 1
  delay_per_segment <- (duration - 0.6) / n_segments

  for (i in seq_len(n_segments)) {
    s %>%
      add_line(
        start = c(x_norm[i], y_norm[i], 0),
        end = c(x_norm[i + 1], y_norm[i + 1], 0),
        color = color,
        stroke_width = 3
      ) %>%
      draw_border(duration = delay_per_segment)
  }

  # Add points at data locations
  for (i in seq_along(x_norm)) {
    s %>%
      add_circle(
        radius = 0.08,
        position = c(x_norm[i], y_norm[i], 0),
        fill_color = color,
        color = color
      ) %>%
      grow_from_center(duration = 0.1)
  }

  s
}

#' Animate ggplot data transition
#'
#' @description
#' Animates a transition between two states of ggplot data (e.g., before/after filtering).
#' Note: The primary `animate_data_transition()` is defined in data_animation.R
#' with a richer Mobject-based implementation. This version is a simpler ggplot-oriented stub.
#'
#' @param scene A Scene object
#' @param from Starting data state
#' @param to Ending data state
#' @param duration Duration of transition
#' @param transition_type Type of transition ("morph", "fade", "slide")
#'
#' @return The Scene object
#'
#' @export
animate_ggplot_transition <- function(scene,
                                    from,
                                    to,
                                    duration = 1,
                                    transition_type = "morph") {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  # Store transition info for rendering
  scene$add_animation(list(
    type = "data_transition",
    start_time = scene$current_time,
    duration = duration,
    from_data = from,
    to_data = to,
    transition_type = transition_type
  ))

  invisible(scene)
}

#' Pipe data into animation
#'
#' @description
#' Helper to pipe data frames directly into animation creation.
#'
#' @param .data A data frame
#' @param .f Animation function to apply
#' @param ... Additional arguments passed to .f
#'
#' @return A Scene object
#'
#' @export
as_manim <- function(.data, .f, ...) {
  .f(.data, ...)
}
