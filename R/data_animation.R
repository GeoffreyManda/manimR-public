#' @title Data-Driven Animation
#' @description
#' Functions for creating animations directly from data. Supports
#' animated scatter plots, bar chart races, time series traces,
#' and smooth data transitions.
#' @name data_animation
#' @keywords internal
NULL

#' Animate a scatter plot from data
#'
#' @description
#' Creates an animated scatter plot where points appear one by one
#' or in groups. Supports color mapping, size mapping, and trajectories.
#'
#' @param data A data.frame with columns for x, y, and optional aesthetics
#' @param x Column name or index for x coordinates
#' @param y Column name or index for y coordinates
#' @param color Column name for color mapping (or single color)
#' @param size Column name for size mapping (or single size)
#' @param label Column name for labels
#' @param group Column name for grouping
#' @param axes Axes object (NULL = auto-create)
#' @param transition Animation style: "appear", "grow", "fade", "trail"
#' @param duration Total animation duration
#' @param stagger Time delay between points (NULL = auto)
#' @return A Scene object with the animation
#' @export
animate_data_scatter <- function(data,
                                  x, y,
                                  color = BLUE,
                                  size = NULL,
                                  label = NULL,
                                  group = NULL,
                                  axes = NULL,
                                  transition = "grow",
                                  duration = 3,
                                  stagger = NULL) {
  x_vals <- data[[x]]
  y_vals <- data[[y]]
  n <- nrow(data)

  # Auto-create axes
  if (is.null(axes)) {
    x_range <- range(x_vals, na.rm = TRUE)
    y_range <- range(y_vals, na.rm = TRUE)
    x_pad <- diff(x_range) * 0.1
    y_pad <- diff(y_range) * 0.1
    x_step <- pretty(x_range, n = 5)[2] - pretty(x_range, n = 5)[1]
    y_step <- pretty(y_range, n = 5)[2] - pretty(y_range, n = 5)[1]

    axes <- Axes$new(
      x_range = c(x_range[1] - x_pad, x_range[2] + x_pad, x_step),
      y_range = c(y_range[1] - y_pad, y_range[2] + y_pad, y_step),
      include_numbers = TRUE,
      tips = TRUE
    )
  }

  sc <- Scene$new(name = "data_scatter")
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))

  # Resolve colors
  colors <- if (is.character(color) && length(color) == 1 && color %in% names(data)) {
    col_vals <- data[[color]]
    if (is.numeric(col_vals)) {
      grad <- color_gradient(c(BLUE, RED), 256)
      rng <- range(col_vals, na.rm = TRUE)
      vapply(col_vals, function(v) {
        t <- (v - rng[1]) / max(rng[2] - rng[1], 1e-8)
        grad[max(1, ceiling(t * 256))]
      }, character(1))
    } else {
      palette <- c(BLUE, RED, GREEN, YELLOW, PURPLE, ORANGE, TEAL, PINK)
      lvls <- unique(col_vals)
      vapply(col_vals, function(v) {
        palette[((match(v, lvls) - 1) %% length(palette)) + 1]
      }, character(1))
    }
  } else {
    rep(color[1], n)
  }

  # Resolve sizes
  radii <- if (!is.null(size) && is.character(size) && size %in% names(data)) {
    s_vals <- data[[size]]
    rng <- range(s_vals, na.rm = TRUE)
    min_r <- DEFAULT_DOT_RADIUS * 0.5
    max_r <- DEFAULT_DOT_RADIUS * 3
    vapply(s_vals, function(v) {
      t <- (v - rng[1]) / max(rng[2] - rng[1], 1e-8)
      min_r + t * (max_r - min_r)
    }, numeric(1))
  } else {
    rep(DEFAULT_DOT_RADIUS * 1.5, n)
  }

  # Create dots
  dots <- list()
  for (i in seq_len(n)) {
    pos <- axes$c2p(x_vals[i], y_vals[i])
    dot <- Dot$new(point = pos, radius = radii[i], color = colors[i])
    dot$fill_color <- colors[i]
    dot$fill_opacity <- 0.8
    dots[[i]] <- dot
  }

  # Animate dots
  stagger_time <- stagger %||% (duration / n * 0.5)

  if (transition == "grow") {
    grow_anims <- lapply(dots, function(d) GrowFromCenter$new(d, run_time = duration / n * 2))
    anim <- do.call(LaggedStart$new, c(grow_anims, list(
      lag_ratio = stagger_time / (duration / n * 2)
    )))
  } else if (transition == "fade") {
    fade_anims <- lapply(dots, function(d) FadeIn$new(d, run_time = duration / n * 2))
    anim <- do.call(LaggedStart$new, c(fade_anims, list(
      lag_ratio = stagger_time / (duration / n * 2)
    )))
  } else if (transition == "trail") {
    # Connect dots with lines, then show
    for (i in seq_len(n)) {
      sc$add(dots[[i]])
      if (i > 1) {
        pos_prev <- axes$c2p(x_vals[i-1], y_vals[i-1])
        pos_curr <- axes$c2p(x_vals[i], y_vals[i])
        line <- Line$new(start = pos_prev, end = pos_curr,
                         color = colors[i], stroke_width = 1)
        sc$add(line)
        sc$play(AnimationGroup$new(
          list(Create$new(line, run_time = stagger_time),
               GrowFromCenter$new(dots[[i]], run_time = stagger_time))
        ))
      } else {
        sc$play(GrowFromCenter$new(dots[[i]], run_time = stagger_time))
      }
    }
    return(sc)
  } else {
    # "appear" - instant
    for (d in dots) {
      d$visible <- TRUE
      d$opacity <- 1
      sc$add(d)
    }
    return(sc)
  }

  for (d in dots) sc$add(d)
  sc$play(anim)
  sc
}


#' Animate a bar chart race
#'
#' @description
#' Creates a bar chart race animation where bars grow, shrink, and
#' reorder over time based on changing data.
#'
#' @param data A data.frame with columns: category, value, time_step
#' @param category Column name for bar labels
#' @param value Column name for bar values
#' @param time_step Column name for time steps
#' @param n_bars Number of bars to show at once
#' @param bar_color Bar color (or column name)
#' @param duration Total animation duration
#' @param axes_range Optional y-axis range
#' @return A Scene object
#' @export
animate_bar_race <- function(data,
                              category,
                              value,
                              time_step,
                              n_bars = 10,
                              bar_color = BLUE,
                              duration = 10,
                              axes_range = NULL) {
  sc <- Scene$new(name = "bar_race")

  time_steps <- sort(unique(data[[time_step]]))
  n_steps <- length(time_steps)
  duration_per_step <- duration / n_steps

  for (si in seq_along(time_steps)) {
    ts <- time_steps[si]
    step_data <- data[data[[time_step]] == ts, ]
    step_data <- step_data[order(-step_data[[value]]), ]
    step_data <- head(step_data, n_bars)

    # Create bars for this step
    max_val <- max(step_data[[value]], na.rm = TRUE)
    bar_height <- 0.5
    bar_spacing <- 0.7

    for (bi in seq_len(nrow(step_data))) {
      cat_name <- as.character(step_data[[category]][bi])
      val <- step_data[[value]][bi]
      bar_width <- (val / max_val) * 5  # Scale to 5 scene units

      y_pos <- (n_bars / 2 - bi + 0.5) * bar_spacing

      bar <- Rectangle$new(
        width = bar_width,
        height = bar_height,
        fill_color = bar_color,
        fill_opacity = 0.8,
        position = c(-2.5 + bar_width / 2, y_pos, 0)
      )

      label <- Text$new(
        text = cat_name,
        font_size = 18,
        color = WHITE,
        position = c(-2.8, y_pos, 0),
        alignment = "right"
      )

      val_label <- Text$new(
        text = format(round(val, 1)),
        font_size = 16,
        color = WHITE,
        position = c(-2.5 + bar_width + 0.3, y_pos, 0)
      )

      sc$add(bar)
      sc$add(label)
      sc$add(val_label)

      if (si == 1) {
        sc$play(GrowFromEdge$new(bar, edge = LEFT, run_time = duration_per_step * 0.8))
      }
    }

    # Time step label
    ts_label <- Text$new(
      text = as.character(ts),
      font_size = 48,
      color = GREY_A,
      position = c(3, -3, 0)
    )
    sc$add(ts_label)

    if (si < n_steps) {
      sc$wait(duration_per_step * 0.2)
    }
  }

  sc
}


#' Animate a time series
#'
#' @description
#' Progressive drawing of a time series line, optionally with
#' a trailing dot and value label.
#'
#' @param data Data.frame with time and value columns
#' @param time Column name for time/x values
#' @param value Column name for values/y values
#' @param color Line color
#' @param show_dot Show a trailing dot
#' @param show_value Show current value label
#' @param axes Axes object (NULL = auto-create)
#' @param duration Animation duration
#' @return A Scene object
#' @export
animate_time_series <- function(data,
                                 time, value,
                                 color = YELLOW,
                                 show_dot = TRUE,
                                 show_value = FALSE,
                                 axes = NULL,
                                 duration = 5) {
  t_vals <- data[[time]]
  v_vals <- data[[value]]

  if (is.null(axes)) {
    t_range <- range(t_vals, na.rm = TRUE)
    v_range <- range(v_vals, na.rm = TRUE)
    t_step <- pretty(t_range, n = 5)[2] - pretty(t_range, n = 5)[1]
    v_step <- pretty(v_range, n = 5)[2] - pretty(v_range, n = 5)[1]

    axes <- Axes$new(
      x_range = c(t_range[1], t_range[2], t_step),
      y_range = c(v_range[1], v_range[2], v_step),
      include_numbers = TRUE
    )
  }

  graph <- axes$plot_line_graph(t_vals, v_vals, color = color)

  sc <- Scene$new(name = "time_series")
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))

  sc$add(graph)
  sc$play(Create$new(graph, run_time = duration))

  if (show_dot) {
    end_pos <- axes$c2p(t_vals[length(t_vals)], v_vals[length(v_vals)])
    dot <- Dot$new(point = end_pos, color = color, radius = DEFAULT_DOT_RADIUS * 1.5)
    sc$add(dot)
    sc$play(GrowFromCenter$new(dot, run_time = 0.3))
  }

  sc
}


#' Smooth data transition
#'
#' @description
#' Smoothly transitions between two data states. Points morph from
#' one set of positions to another, useful for showing data
#' before/after transformations.
#'
#' @param scene Existing scene (or NULL to create new)
#' @param from_data Initial data positions (data.frame with x, y)
#' @param to_data Final data positions (same structure)
#' @param x X column name
#' @param y Y column name
#' @param color Dot color
#' @param axes Axes object
#' @param duration Transition duration
#' @return Scene object
#' @export
animate_data_transition <- function(scene = NULL,
                                     from_data,
                                     to_data,
                                     x, y,
                                     color = BLUE,
                                     axes = NULL,
                                     duration = 2) {
  if (is.null(scene)) scene <- Scene$new(name = "data_transition")

  n <- min(nrow(from_data), nrow(to_data))

  for (i in seq_len(n)) {
    from_pos <- if (!is.null(axes)) axes$c2p(from_data[[x]][i], from_data[[y]][i])
                else c(from_data[[x]][i], from_data[[y]][i], 0)
    to_pos <- if (!is.null(axes)) axes$c2p(to_data[[x]][i], to_data[[y]][i])
              else c(to_data[[x]][i], to_data[[y]][i], 0)

    dot <- Dot$new(point = from_pos, color = color)
    dot$generate_target()
    dot$target$move_to(to_pos)

    scene$add(dot)
    scene$play(MoveToTarget$new(dot, run_time = duration))
  }

  scene
}
