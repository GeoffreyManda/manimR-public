#' @title Statistical Visualization
#' @description
#' Animated statistical graphics for manimR. Includes distribution
#' visualizations, regression animations, hypothesis tests, and
#' probability demonstrations.
#' @name stat_viz
#' @keywords internal
NULL

#' Animate a histogram
#'
#' @description
#' Creates an animated histogram where bars grow from the x-axis.
#' Supports custom bin widths, color mapping, and optional density overlay.
#'
#' @param data Numeric vector or data.frame column
#' @param bins Number of bins
#' @param color Bar fill color
#' @param axes Axes object (NULL = auto)
#' @param show_density Overlay a density curve
#' @param density_color Density curve color
#' @param duration Animation duration
#' @return A Scene object
#' @export
animate_histogram <- function(data,
                               bins = 30,
                               color = BLUE,
                               axes = NULL,
                               show_density = FALSE,
                               density_color = YELLOW,
                               duration = 3) {
  if (is.data.frame(data)) data <- data[[1]]

  h <- graphics::hist(data, breaks = bins, plot = FALSE)
  n_bars <- length(h$counts)

  max_count <- max(h$counts)
  x_range <- range(h$breaks)
  x_pad <- diff(x_range) * 0.05

  if (is.null(axes)) {
    axes <- Axes$new(
      x_range = c(x_range[1] - x_pad, x_range[2] + x_pad,
                  pretty(x_range, n = 5)[2] - pretty(x_range, n = 5)[1]),
      y_range = c(0, max_count * 1.1, pretty(c(0, max_count), n = 5)[2]),
      include_numbers = TRUE
    )
  }

  sc <- Scene$new(name = "histogram")
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))

  # Create bars
  bars <- list()
  for (i in seq_len(n_bars)) {
    x_left <- h$breaks[i]
    x_right <- h$breaks[i + 1]
    count <- h$counts[i]
    if (count == 0) next

    bl <- axes$c2p(x_left, 0)
    tr <- axes$c2p(x_right, count)
    w <- tr[1] - bl[1]
    ht <- tr[2] - bl[2]
    center <- c((bl[1] + tr[1]) / 2, (bl[2] + tr[2]) / 2, 0)

    bar <- Rectangle$new(
      width = w * 0.95,
      height = ht,
      fill_color = color,
      fill_opacity = 0.7,
      stroke_width = 1,
      color = WHITE,
      position = center
    )
    bars[[length(bars) + 1]] <- bar
    sc$add(bar)
  }

  # Animate bars growing
  bar_anims <- lapply(bars, function(b) {
    GrowFromEdge$new(b, edge = DOWN, run_time = duration / 2)
  })
  sc$play(do.call(LaggedStart$new, c(bar_anims, list(lag_ratio = 0.05))))

  # Optional density curve
  if (show_density) {
    d <- stats::density(data)
    # Scale density to match histogram counts
    scale_factor <- max_count / max(d$y)
    graph <- axes$plot(
      func = stats::approxfun(d$x, d$y * scale_factor, rule = 2),
      x_range = c(x_range[1], x_range[2]),
      color = density_color,
      stroke_width = 3
    )
    sc$add(graph)
    sc$play(Create$new(graph, run_time = 1))
  }

  sc
}


#' Animate a normal distribution
#'
#' @description
#' Shows a normal distribution curve being drawn, with optional
#' shading for standard deviations or probability regions.
#'
#' @param mean Distribution mean
#' @param sd Standard deviation
#' @param shade_region c(lower, upper) for shading (NULL = no shading)
#' @param shade_color Shading color
#' @param show_mean Show vertical mean line
#' @param show_sd Show standard deviation markers
#' @param axes Axes object (NULL = auto)
#' @param duration Animation duration
#' @return A Scene object
#' @export
animate_normal_distribution <- function(mean = 0,
                                         sd = 1,
                                         shade_region = NULL,
                                         shade_color = YELLOW,
                                         show_mean = TRUE,
                                         show_sd = TRUE,
                                         axes = NULL,
                                         duration = 3) {
  x_min <- mean - 4 * sd
  x_max <- mean + 4 * sd
  y_max <- stats::dnorm(mean, mean, sd) * 1.2

  if (is.null(axes)) {
    axes <- Axes$new(
      x_range = c(x_min, x_max, sd),
      y_range = c(0, y_max, y_max / 4),
      include_numbers = TRUE
    )
  }

  sc <- Scene$new(name = "normal_dist")
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))

  # Normal curve
  norm_func <- function(x) stats::dnorm(x, mean, sd)
  graph <- axes$plot(norm_func, x_range = c(x_min, x_max),
                     color = BLUE, stroke_width = 3)
  sc$add(graph)
  sc$play(Create$new(graph, run_time = duration * 0.5))

  # Shaded region
  if (!is.null(shade_region)) {
    area <- axes$get_area(graph, x_range = shade_region,
                          color = shade_color, fill_opacity = 0.4)
    sc$add(area)
    sc$play(FadeIn$new(area, run_time = 0.5))

    # Label with probability
    prob <- stats::pnorm(shade_region[2], mean, sd) -
            stats::pnorm(shade_region[1], mean, sd)
    prob_text <- Text$new(
      text = sprintf("P = %.3f", prob),
      font_size = 24,
      color = shade_color,
      position = axes$c2p(mean(shade_region), y_max * 0.5)
    )
    sc$add(prob_text)
    sc$play(FadeIn$new(prob_text, run_time = 0.3))
  }

  # Mean line
  if (show_mean) {
    mean_line <- DashedLine$new(
      start = axes$c2p(mean, 0),
      end = axes$c2p(mean, norm_func(mean)),
      color = RED,
      stroke_width = 2
    )
    sc$add(mean_line)
    sc$play(Create$new(mean_line, run_time = 0.3))

    mu_label <- MathTex$new(
      tex_string = "\\mu",
      color = RED,
      position = axes$c2p(mean, -y_max * 0.1)
    )
    sc$add(mu_label)
    sc$play(FadeIn$new(mu_label, run_time = 0.2))
  }

  # SD markers
  if (show_sd) {
    for (k in c(-1, 1)) {
      sd_x <- mean + k * sd
      sd_line <- DashedLine$new(
        start = axes$c2p(sd_x, 0),
        end = axes$c2p(sd_x, norm_func(sd_x)),
        color = GREEN,
        stroke_width = 1
      )
      sc$add(sd_line)
      sc$play(Create$new(sd_line, run_time = 0.2))
    }

    sigma_label <- MathTex$new(
      tex_string = "\\sigma",
      color = GREEN,
      position = axes$c2p(mean + sd, -y_max * 0.1)
    )
    sc$add(sigma_label)
    sc$play(FadeIn$new(sigma_label, run_time = 0.2))
  }

  sc
}


#' Animate linear regression
#'
#' @description
#' Shows data points appearing, then animates the regression line
#' being drawn with optional confidence bands and residuals.
#'
#' @param data Data.frame
#' @param x X column name
#' @param y Y column name
#' @param show_residuals Show vertical residual lines
#' @param show_ci Show confidence interval band
#' @param ci_level Confidence level (default 0.95)
#' @param point_color Data point color
#' @param line_color Regression line color
#' @param axes Axes object (NULL = auto)
#' @param duration Animation duration
#' @return A Scene object
#' @export
animate_regression <- function(data, x, y,
                                show_residuals = FALSE,
                                show_ci = FALSE,
                                ci_level = 0.95,
                                point_color = BLUE,
                                line_color = RED,
                                axes = NULL,
                                duration = 5) {
  x_vals <- data[[x]]
  y_vals <- data[[y]]
  n <- length(x_vals)

  # Fit regression
  fit <- stats::lm(y_vals ~ x_vals)
  b0 <- stats::coef(fit)[1]
  b1 <- stats::coef(fit)[2]

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
      include_numbers = TRUE
    )
  }

  sc <- Scene$new(name = "regression")
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))

  # Add data points
  dots <- list()
  for (i in seq_len(n)) {
    pos <- axes$c2p(x_vals[i], y_vals[i])
    dot <- Dot$new(point = pos, color = point_color, radius = DEFAULT_DOT_RADIUS * 1.5)
    dots[[i]] <- dot
    sc$add(dot)
  }
  dot_anims <- lapply(dots, function(d) GrowFromCenter$new(d, run_time = 0.3))
  sc$play(do.call(LaggedStart$new, c(dot_anims, list(lag_ratio = 0.05))))

  # Draw regression line
  reg_func <- function(xv) b0 + b1 * xv
  x_rng <- range(x_vals)
  reg_line <- axes$plot(reg_func, x_range = x_rng, color = line_color,
                        stroke_width = 3)
  sc$add(reg_line)
  sc$play(Create$new(reg_line, run_time = duration * 0.3))

  # Equation label
  sign <- if (b1 >= 0) "+" else "-"
  eq_text <- sprintf("y = %.2f %s %.2fx", b0, sign, abs(b1))
  eq_label <- Text$new(
    text = eq_text,
    font_size = 24,
    color = line_color,
    position = c(3, 3.5, 0)
  )
  sc$add(eq_label)
  sc$play(FadeIn$new(eq_label, run_time = 0.3))

  # Optional residuals
  if (show_residuals) {
    for (i in seq_len(n)) {
      y_pred <- reg_func(x_vals[i])
      data_pos <- axes$c2p(x_vals[i], y_vals[i])
      pred_pos <- axes$c2p(x_vals[i], y_pred)

      resid_line <- Line$new(
        start = data_pos,
        end = pred_pos,
        color = YELLOW,
        stroke_width = 1
      )
      sc$add(resid_line)
      sc$play(Create$new(resid_line, run_time = 0.1))
    }
  }

  # Optional confidence interval
  if (show_ci) {
    se <- summary(fit)$sigma
    t_val <- stats::qt((1 + ci_level) / 2, df = n - 2)
    x_bar <- mean(x_vals)
    ss_x <- sum((x_vals - x_bar)^2)

    ci_upper <- function(xv) {
      y_hat <- b0 + b1 * xv
      se_pred <- se * sqrt(1/n + (xv - x_bar)^2 / ss_x)
      y_hat + t_val * se_pred
    }
    ci_lower <- function(xv) {
      y_hat <- b0 + b1 * xv
      se_pred <- se * sqrt(1/n + (xv - x_bar)^2 / ss_x)
      y_hat - t_val * se_pred
    }

    upper_curve <- axes$plot(ci_upper, x_range = x_rng, color = line_color,
                             stroke_width = 1)
    lower_curve <- axes$plot(ci_lower, x_range = x_rng, color = line_color,
                             stroke_width = 1)
    upper_curve$stroke_opacity <- 0.5
    lower_curve$stroke_opacity <- 0.5

    sc$add(upper_curve)
    sc$add(lower_curve)
    sc$play(AnimationGroup$new(
      Create$new(upper_curve, run_time = 0.5),
      Create$new(lower_curve, run_time = 0.5)
    ))
  }

  # R-squared
  r_sq <- summary(fit)$r.squared
  r2_text <- Text$new(
    text = sprintf("R-squared = %.3f", r_sq),
    font_size = 20,
    color = GREY_A,
    position = c(3, 3, 0)
  )
  sc$add(r2_text)
  sc$play(FadeIn$new(r2_text, run_time = 0.3))

  sc
}


#' Animate a box plot
#'
#' @description
#' Creates an animated box plot showing quartiles, median, whiskers,
#' and outliers.
#'
#' @param data Numeric vector or named list of vectors
#' @param color Box color
#' @param show_points Show individual data points
#' @param duration Animation duration
#' @return A Scene object
#' @export
animate_boxplot <- function(data,
                             color = BLUE,
                             show_points = FALSE,
                             duration = 3) {
  if (is.numeric(data)) data <- list(Data = data)

  n_groups <- length(data)
  sc <- Scene$new(name = "boxplot")

  # Compute statistics
  all_vals <- unlist(data)
  y_range <- range(all_vals, na.rm = TRUE)
  y_pad <- diff(y_range) * 0.1

  axes <- Axes$new(
    x_range = c(0, n_groups + 1, 1),
    y_range = c(y_range[1] - y_pad, y_range[2] + y_pad,
                pretty(y_range, n = 5)[2] - pretty(y_range, n = 5)[1]),
    include_numbers = TRUE,
    include_ticks = TRUE
  )
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))

  for (gi in seq_along(data)) {
    vals <- data[[gi]]
    q1 <- stats::quantile(vals, 0.25)
    med <- stats::median(vals)
    q3 <- stats::quantile(vals, 0.75)
    iqr <- q3 - q1
    lower_fence <- max(min(vals), q1 - 1.5 * iqr)
    upper_fence <- min(max(vals), q3 + 1.5 * iqr)

    x_center <- gi
    box_width <- 0.6

    # Box (Q1 to Q3)
    bl <- axes$c2p(x_center - box_width/2, q1)
    tr <- axes$c2p(x_center + box_width/2, q3)
    box_h <- tr[2] - bl[2]
    box_w <- tr[1] - bl[1]
    center <- c((bl[1] + tr[1])/2, (bl[2] + tr[2])/2, 0)

    box <- Rectangle$new(
      width = box_w, height = box_h,
      fill_color = color, fill_opacity = 0.4,
      stroke_width = 2, color = WHITE,
      position = center
    )
    sc$add(box)
    sc$play(GrowFromCenter$new(box, run_time = duration / (n_groups * 4)))

    # Median line
    med_left <- axes$c2p(x_center - box_width/2, med)
    med_right <- axes$c2p(x_center + box_width/2, med)
    med_line <- Line$new(start = med_left, end = med_right,
                          color = YELLOW, stroke_width = 3)
    sc$add(med_line)
    sc$play(Create$new(med_line, run_time = 0.2))

    # Whiskers
    lower_pt <- axes$c2p(x_center, lower_fence)
    upper_pt <- axes$c2p(x_center, upper_fence)
    q1_pt <- axes$c2p(x_center, q1)
    q3_pt <- axes$c2p(x_center, q3)

    lower_whisker <- Line$new(start = q1_pt, end = lower_pt,
                               color = WHITE, stroke_width = 1)
    upper_whisker <- Line$new(start = q3_pt, end = upper_pt,
                               color = WHITE, stroke_width = 1)
    sc$add(lower_whisker)
    sc$add(upper_whisker)
    sc$play(AnimationGroup$new(
      Create$new(lower_whisker, run_time = 0.2),
      Create$new(upper_whisker, run_time = 0.2)
    ))

    # Outliers
    outliers <- vals[vals < lower_fence | vals > upper_fence]
    for (out in outliers) {
      pt <- axes$c2p(x_center, out)
      dot <- Dot$new(point = pt, radius = DEFAULT_DOT_RADIUS, color = RED)
      sc$add(dot)
      sc$play(GrowFromCenter$new(dot, run_time = 0.1))
    }
  }

  sc
}


#' Animate correlation between two variables
#'
#' @description
#' Shows how changing one variable affects another, with a
#' connecting line showing the relationship.
#'
#' @param func Function mapping x -> y for the relationship
#' @param x_range x range to demonstrate
#' @param color Dot color
#' @param trace_color Trace line color
#' @param duration Animation duration
#' @return A Scene object
#' @export
animate_function_trace <- function(func,
                                    x_range = c(-3, 3),
                                    color = BLUE,
                                    trace_color = YELLOW,
                                    duration = 4) {
  axes <- Axes$new(
    x_range = c(x_range[1] - 0.5, x_range[2] + 0.5, 1),
    y_range = c(-4, 4, 1),
    include_numbers = TRUE
  )

  # Draw the function
  graph <- axes$plot(func, x_range = x_range, color = trace_color, stroke_width = 2)

  sc <- Scene$new(name = "function_trace")
  sc$add(axes)
  sc$play(Create$new(axes, run_time = 0.5))
  sc$add(graph)
  sc$play(Create$new(graph, run_time = 1))

  # Moving dot along the curve
  tracker <- ValueTracker$new(value = x_range[1])
  dot <- Dot$new(
    point = axes$c2p(x_range[1], func(x_range[1])),
    color = color,
    radius = DEFAULT_DOT_RADIUS * 2
  )
  sc$add(dot)
  sc$play(GrowFromCenter$new(dot, run_time = 0.3))

  # Animate the dot moving along
  sc$play(tracker$animate_value(
    x_range[2],
    run_time = duration - 2,
    rate_func = smooth
  ))

  sc
}
