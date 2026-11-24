#' @title Statistics Tutorial Module
#' @description
#' Interactive tutorial visualizing statistical concepts including
#' distributions, hypothesis testing, and regression.
#'
#' @name tutorial_statistics
NULL

#' Tutorial: Statistics
#'
#' @keywords internal
#' @noRd
tutorial_statistics <- function() {
  cli::cli_h1("Tutorial: Statistics through Visualization")

  cli::cli_h2("What we'll explore:")
  cli::cli_ul(
    "Descriptive statistics",
    "Probability distributions",
    "Central Limit Theorem",
    "Hypothesis testing",
    "Linear regression"
  )
  cli::cli_text("")

  lessons <- c(
    "Descriptive Statistics",
    "Normal Distribution",
    "Central Limit Theorem",
    "Hypothesis Testing",
    "Linear Regression",
    "Return to Main Menu"
  )

  choice <- utils::menu(
    title = "Select a lesson:",
    choices = lessons
  )

  switch(choice,
    cli::cli_text("Descriptive Statistics - Coming soon!"),
    normal_distribution_tutorial(),
    cli::cli_text("Central Limit Theorem - Coming soon!"),
    cli::cli_text("Hypothesis Testing - Coming soon!"),
    cli::cli_text("Linear Regression - Coming soon!"),
    return(invisible(NULL))
  )

  cli::cli_text("Press Enter to return...")
  readline()
  invisible(TRUE)
}


# =========================================================================
# Normal Distribution Tutorial
# =========================================================================

#' Normal Distribution Animated Tutorial
#'
#' @description
#' A comprehensive animated tutorial that builds intuition for the
#' normal (Gaussian) distribution step by step:
#'
#' 1. **Title card** introducing the topic
#' 2. **Bell curve construction** -- axes appear, then the PDF is drawn
#' 3. **Mean and standard deviation** -- annotated on the curve
#' 4. **The 68-95-99.7 rule** -- successive shaded regions
#' 5. **Standard normal vs. general normal** -- side-by-side comparison
#' 6. **Cumulative distribution** -- CDF curve and probability lookup
#' 7. **Sampling demonstration** -- histogram of random samples converging
#' 8. **Summary card**
#'
#' @param output Output file path (default: "normal_distribution_tutorial.gif")
#' @param format Output format: "gif", "mp4", "webm", "png_sequence"
#' @param quality Render quality: "low", "medium", "high", "production"
#' @param fps Frames per second
#' @param open Open the file after rendering?
#'
#' @return Invisibly returns the output file path
#'
#' @examples
#' \dontrun{
#' # Render the full tutorial as a GIF
#' normal_distribution_tutorial()
#'
#' # High-quality MP4
#' normal_distribution_tutorial(format = "mp4", quality = "high")
#' }
#'
#' @export
normal_distribution_tutorial <- function(output = "normal_distribution_tutorial",
                                          format = "gif",
                                          quality = "low",
                                          fps = 10,
                                          open = FALSE) {
  sc <- Scene$new(name = "normal_distribution_tutorial")

  # ---------------------------------------------------------------
  # Colour palette (3Blue1Brown-inspired)
  # ---------------------------------------------------------------
  col_curve     <- BLUE_C
  col_fill_1sd  <- BLUE_D
  col_fill_2sd  <- GREEN_C
  col_fill_3sd  <- YELLOW_C
  col_mean      <- RED_C
  col_sd        <- GREEN_C
  col_cdf       <- TEAL_C
  col_accent    <- YELLOW_C
  col_text      <- WHITE
  col_dim       <- GREY_A

  # ---------------------------------------------------------------
  # SCENE 1 -- Title Card
  # ---------------------------------------------------------------
  title <- Text$new(
    text = "The Normal Distribution",
    font_size = 72,
    color = col_text,
    weight = "bold",
    position = c(0, 1.5, 0)
  )
  subtitle <- Text$new(
    text = "The bell curve that governs the world",
    font_size = 36,
    color = col_dim,
    position = c(0, 0.3, 0)
  )
  formula_txt <- Text$new(
    text = "f(x) = (1 / sqrt(2*pi*s^2)) * exp(-(x-u)^2 / (2*s^2))",
    font_size = 24,
    color = col_accent,
    position = c(0, -1.0, 0)
  )

  sc$add_object(title)
  sc$add_object(subtitle)
  sc$add_object(formula_txt)
  sc$play(FadeIn$new(title, run_time = 1))
  sc$play(FadeIn$new(subtitle, run_time = 0.8))
  sc$play(FadeIn$new(formula_txt, run_time = 0.8))
  sc$wait(1.5)

  # Fade out title
  sc$play(FadeOut$new(title, run_time = 0.5))
  sc$play(FadeOut$new(subtitle, run_time = 0.3))
  sc$play(FadeOut$new(formula_txt, run_time = 0.3))
  sc$wait(0.3)

  # ---------------------------------------------------------------
  # SCENE 2 -- Build the bell curve
  # ---------------------------------------------------------------
  mu <- 0; sigma <- 1
  x_min <- mu - 4 * sigma
  x_max <- mu + 4 * sigma
  y_peak <- stats::dnorm(mu, mu, sigma)

  axes <- Axes$new(
    x_range = c(x_min, x_max, 1),
    y_range = c(0, y_peak * 1.25, 0.1),
    x_length = 10,
    y_length = 5,
    include_numbers = TRUE,
    include_ticks = TRUE,
    include_tips = TRUE
  )
  axes$position <- c(0, -0.5, 0)

  # Axis labels
  x_label <- Text$new(text = "x", font_size = 28, color = col_dim,
                       position = c(5.5, -3.2, 0))
  y_label <- Text$new(text = "f(x)", font_size = 28, color = col_dim,
                       position = c(-5.8, 2.0, 0))

  sc$add_object(axes)
  sc$play(Create$new(axes, run_time = 1.0))
  sc$add_object(x_label)
  sc$add_object(y_label)
  sc$play(FadeIn$new(x_label, run_time = 0.3))
  sc$play(FadeIn$new(y_label, run_time = 0.3))

  # Draw the bell curve
  norm_pdf <- function(x) stats::dnorm(x, mu, sigma)
  bell_curve <- axes$plot(norm_pdf,
                          x_range = c(x_min, x_max),
                          color = col_curve,
                          stroke_width = 4)
  sc$add_object(bell_curve)
  sc$play(Create$new(bell_curve, run_time = 2.0))
  sc$wait(0.5)

  # Label: "The Bell Curve"
  curve_label <- Text$new(
    text = "The Bell Curve",
    font_size = 36,
    color = col_curve,
    position = c(0, 3.3, 0)
  )
  sc$add_object(curve_label)
  sc$play(FadeIn$new(curve_label, run_time = 0.5))
  sc$wait(1.0)
  sc$play(FadeOut$new(curve_label, run_time = 0.3))

  # ---------------------------------------------------------------
  # SCENE 3 -- Mean (mu) and standard deviation (sigma)
  # ---------------------------------------------------------------

  # Mean line
  mean_bottom <- axes$c2p(mu, 0)
  mean_top    <- axes$c2p(mu, norm_pdf(mu))
  mean_line <- DashedLine$new(
    start = mean_bottom,
    end   = mean_top,
    color = col_mean,
    stroke_width = 3
  )
  sc$add_object(mean_line)
  sc$play(Create$new(mean_line, run_time = 0.6))

  mu_label <- Text$new(
    text = "mean (mu) = 0",
    font_size = 24,
    color = col_mean,
    position = axes$c2p(mu, -0.04) + c(0, -0.3, 0)
  )
  sc$add_object(mu_label)
  sc$play(FadeIn$new(mu_label, run_time = 0.4))
  sc$wait(0.5)

  # SD markers at +/- 1 sigma
  sd_lines <- list()
  for (k in c(-1, 1)) {
    sd_x <- mu + k * sigma
    sd_bottom <- axes$c2p(sd_x, 0)
    sd_top    <- axes$c2p(sd_x, norm_pdf(sd_x))
    sd_line <- DashedLine$new(
      start = sd_bottom, end = sd_top,
      color = col_sd, stroke_width = 2
    )
    sd_lines[[length(sd_lines) + 1]] <- sd_line
    sc$add_object(sd_line)
    sc$play(Create$new(sd_line, run_time = 0.3))
  }

  # Double-arrow showing sigma span
  sigma_arrow_l <- axes$c2p(mu, norm_pdf(mu) * 0.6)
  sigma_arrow_r <- axes$c2p(mu + sigma, norm_pdf(mu) * 0.6)
  sigma_bracket <- Arrow$new(
    start = sigma_arrow_l,
    end   = sigma_arrow_r,
    color = col_sd,
    stroke_width = 2
  )
  sc$add_object(sigma_bracket)
  sc$play(Create$new(sigma_bracket, run_time = 0.4))

  sigma_label <- Text$new(
    text = "sigma = 1",
    font_size = 22,
    color = col_sd,
    position = (sigma_arrow_l + sigma_arrow_r) / 2 + c(0, 0.35, 0)
  )
  sc$add_object(sigma_label)
  sc$play(FadeIn$new(sigma_label, run_time = 0.3))
  sc$wait(1.0)

  # Clean up annotations before the rule
  sc$play(FadeOut$new(mu_label, run_time = 0.2))
  sc$play(FadeOut$new(sigma_label, run_time = 0.2))
  sc$play(FadeOut$new(sigma_bracket, run_time = 0.2))
  for (sl in sd_lines) {
    sc$play(FadeOut$new(sl, run_time = 0.1))
  }

  # ---------------------------------------------------------------
  # SCENE 4 -- The 68-95-99.7 Rule
  # ---------------------------------------------------------------

  rule_title <- Text$new(
    text = "The 68-95-99.7 Rule",
    font_size = 40,
    color = col_accent,
    position = c(0, 3.3, 0)
  )
  sc$add_object(rule_title)
  sc$play(FadeIn$new(rule_title, run_time = 0.5))

  # +/- 1 SD  (68.27%)
  area_1sd <- axes$get_area(bell_curve,
                             x_range = c(mu - sigma, mu + sigma),
                             color = col_fill_1sd,
                             fill_opacity = 0.45)
  sc$add_object(area_1sd)
  sc$play(FadeIn$new(area_1sd, run_time = 0.8))

  label_1sd <- Text$new(
    text = "68.27%",
    font_size = 28,
    color = WHITE,
    position = axes$c2p(mu, norm_pdf(mu) * 0.35)
  )
  sc$add_object(label_1sd)
  sc$play(FadeIn$new(label_1sd, run_time = 0.3))

  range_1sd <- Text$new(
    text = "(mu - sigma , mu + sigma)",
    font_size = 20,
    color = col_fill_1sd,
    position = axes$c2p(mu, 0) + c(0, -0.55, 0)
  )
  sc$add_object(range_1sd)
  sc$play(FadeIn$new(range_1sd, run_time = 0.3))
  sc$wait(1.0)

  # +/- 2 SD  (95.45%)
  sc$play(FadeOut$new(area_1sd, run_time = 0.3))
  sc$play(FadeOut$new(label_1sd, run_time = 0.2))
  sc$play(FadeOut$new(range_1sd, run_time = 0.2))

  area_2sd <- axes$get_area(bell_curve,
                             x_range = c(mu - 2 * sigma, mu + 2 * sigma),
                             color = col_fill_2sd,
                             fill_opacity = 0.4)
  sc$add_object(area_2sd)
  sc$play(FadeIn$new(area_2sd, run_time = 0.8))

  label_2sd <- Text$new(
    text = "95.45%",
    font_size = 28,
    color = WHITE,
    position = axes$c2p(mu, norm_pdf(mu) * 0.35)
  )
  sc$add_object(label_2sd)
  sc$play(FadeIn$new(label_2sd, run_time = 0.3))

  range_2sd <- Text$new(
    text = "(mu - 2*sigma , mu + 2*sigma)",
    font_size = 20,
    color = col_fill_2sd,
    position = axes$c2p(mu, 0) + c(0, -0.55, 0)
  )
  sc$add_object(range_2sd)
  sc$play(FadeIn$new(range_2sd, run_time = 0.3))
  sc$wait(1.0)

  # +/- 3 SD  (99.73%)
  sc$play(FadeOut$new(area_2sd, run_time = 0.3))
  sc$play(FadeOut$new(label_2sd, run_time = 0.2))
  sc$play(FadeOut$new(range_2sd, run_time = 0.2))

  area_3sd <- axes$get_area(bell_curve,
                             x_range = c(mu - 3 * sigma, mu + 3 * sigma),
                             color = col_fill_3sd,
                             fill_opacity = 0.35)
  sc$add_object(area_3sd)
  sc$play(FadeIn$new(area_3sd, run_time = 0.8))

  label_3sd <- Text$new(
    text = "99.73%",
    font_size = 28,
    color = WHITE,
    position = axes$c2p(mu, norm_pdf(mu) * 0.35)
  )
  sc$add_object(label_3sd)
  sc$play(FadeIn$new(label_3sd, run_time = 0.3))

  range_3sd <- Text$new(
    text = "(mu - 3*sigma , mu + 3*sigma)",
    font_size = 20,
    color = col_fill_3sd,
    position = axes$c2p(mu, 0) + c(0, -0.55, 0)
  )
  sc$add_object(range_3sd)
  sc$play(FadeIn$new(range_3sd, run_time = 0.3))
  sc$wait(1.5)

  # Clean up rule section
  sc$play(FadeOut$new(area_3sd, run_time = 0.3))
  sc$play(FadeOut$new(label_3sd, run_time = 0.2))
  sc$play(FadeOut$new(range_3sd, run_time = 0.2))
  sc$play(FadeOut$new(rule_title, run_time = 0.3))

  # ---------------------------------------------------------------
  # SCENE 5 -- Different means and SDs
  # ---------------------------------------------------------------

  vary_title <- Text$new(
    text = "Changing mean and sigma",
    font_size = 36,
    color = col_accent,
    position = c(0, 3.3, 0)
  )
  sc$add_object(vary_title)
  sc$play(FadeIn$new(vary_title, run_time = 0.5))

  # Curve for mu=0, sigma=0.5  (taller, narrower)
  norm_narrow <- function(x) stats::dnorm(x, 0, 0.5)
  curve_narrow <- axes$plot(norm_narrow,
                            x_range = c(-4, 4),
                            color = RED_C,
                            stroke_width = 3)
  sc$add_object(curve_narrow)
  sc$play(Create$new(curve_narrow, run_time = 1.0))

  label_narrow <- Text$new(
    text = "sigma = 0.5",
    font_size = 20, color = RED_C,
    position = axes$c2p(1.5, stats::dnorm(0, 0, 0.5) * 0.7)
  )
  sc$add_object(label_narrow)
  sc$play(FadeIn$new(label_narrow, run_time = 0.3))

  # Curve for mu=0, sigma=2  (shorter, wider)
  norm_wide <- function(x) stats::dnorm(x, 0, 2)
  curve_wide <- axes$plot(norm_wide,
                          x_range = c(-4, 4),
                          color = TEAL_C,
                          stroke_width = 3)
  sc$add_object(curve_wide)
  sc$play(Create$new(curve_wide, run_time = 1.0))

  label_wide <- Text$new(
    text = "sigma = 2",
    font_size = 20, color = TEAL_C,
    position = axes$c2p(-2.5, stats::dnorm(0, 0, 2) * 1.5)
  )
  sc$add_object(label_wide)
  sc$play(FadeIn$new(label_wide, run_time = 0.3))

  # Original curve label
  label_orig <- Text$new(
    text = "sigma = 1",
    font_size = 20, color = col_curve,
    position = axes$c2p(2.0, norm_pdf(1.0))
  )
  sc$add_object(label_orig)
  sc$play(FadeIn$new(label_orig, run_time = 0.3))
  sc$wait(1.5)

  # Clean up comparison
  sc$play(FadeOut$new(curve_narrow, run_time = 0.3))
  sc$play(FadeOut$new(curve_wide, run_time = 0.3))
  sc$play(FadeOut$new(label_narrow, run_time = 0.2))
  sc$play(FadeOut$new(label_wide, run_time = 0.2))
  sc$play(FadeOut$new(label_orig, run_time = 0.2))
  sc$play(FadeOut$new(vary_title, run_time = 0.3))

  # ---------------------------------------------------------------
  # SCENE 6 -- Cumulative Distribution Function (CDF)
  # ---------------------------------------------------------------

  cdf_title <- Text$new(
    text = "Cumulative Probability  P(X <= x)",
    font_size = 36,
    color = col_cdf,
    position = c(0, 3.3, 0)
  )
  sc$add_object(cdf_title)
  sc$play(FadeIn$new(cdf_title, run_time = 0.5))

  # Shade from -inf to x=1.0
  shade_x <- 1.0
  area_cdf <- axes$get_area(bell_curve,
                             x_range = c(x_min, shade_x),
                             color = col_cdf,
                             fill_opacity = 0.4)
  sc$add_object(area_cdf)
  sc$play(FadeIn$new(area_cdf, run_time = 1.0))

  # Vertical line at x = 1
  cdf_line <- DashedLine$new(
    start = axes$c2p(shade_x, 0),
    end   = axes$c2p(shade_x, norm_pdf(shade_x)),
    color = col_cdf,
    stroke_width = 2
  )
  sc$add_object(cdf_line)
  sc$play(Create$new(cdf_line, run_time = 0.3))

  # Probability label
  prob_val <- stats::pnorm(shade_x, mu, sigma)
  prob_label <- Text$new(
    text = sprintf("P(X <= %.0f) = %.4f", shade_x, prob_val),
    font_size = 26,
    color = col_cdf,
    position = axes$c2p(-2.0, norm_pdf(mu) * 0.6)
  )
  sc$add_object(prob_label)
  sc$play(FadeIn$new(prob_label, run_time = 0.5))
  sc$wait(1.5)

  # Clean up CDF
  sc$play(FadeOut$new(area_cdf, run_time = 0.3))
  sc$play(FadeOut$new(cdf_line, run_time = 0.2))
  sc$play(FadeOut$new(prob_label, run_time = 0.2))
  sc$play(FadeOut$new(cdf_title, run_time = 0.3))

  # ---------------------------------------------------------------
  # SCENE 7 -- Sampling: histogram converges to curve
  # ---------------------------------------------------------------

  sample_title <- Text$new(
    text = "Random Sampling",
    font_size = 36,
    color = col_accent,
    position = c(0, 3.3, 0)
  )
  sc$add_object(sample_title)
  sc$play(FadeIn$new(sample_title, run_time = 0.5))

  # Generate samples and build histogram progressively
  set.seed(42)
  n_samples <- 500
  all_samples <- stats::rnorm(n_samples, mu, sigma)

  # We'll show bars for n = 50 samples
  n_show <- 50
  batch <- all_samples[seq_len(n_show)]
  h <- graphics::hist(batch, breaks = seq(-4, 4, by = 0.5), plot = FALSE)

  # Scale histogram to match PDF
  bin_width <- 0.5
  max_density_est <- max(h$counts / (n_show * bin_width))
  y_scale <- y_peak / max(max_density_est, y_peak)  # scale bars

  bars <- list()
  for (i in seq_along(h$counts)) {
    if (h$counts[i] == 0) next
    x_left  <- h$breaks[i]
    x_right <- h$breaks[i + 1]
    density_val <- h$counts[i] / (n_show * bin_width)

    bl <- axes$c2p(x_left, 0)
    tr <- axes$c2p(x_right, density_val)
    w  <- abs(tr[1] - bl[1])
    ht <- abs(tr[2] - bl[2])
    center <- c((bl[1] + tr[1]) / 2, (bl[2] + tr[2]) / 2, 0)

    bar <- Rectangle$new(
      width = w * 0.92,
      height = ht,
      fill_color = col_fill_1sd,
      fill_opacity = 0.55,
      stroke_width = 1,
      color = WHITE,
      position = center
    )
    bar$z_index <- -1
    bars[[length(bars) + 1]] <- bar
    sc$add_object(bar)
  }

  # Animate all bars appearing
  for (b in bars) {
    sc$play(FadeIn$new(b, run_time = 0.05))
  }

  n_label <- Text$new(
    text = sprintf("n = %d samples", n_show),
    font_size = 22,
    color = col_dim,
    position = c(3.5, 2.5, 0)
  )
  sc$add_object(n_label)
  sc$play(FadeIn$new(n_label, run_time = 0.3))
  sc$wait(1.0)

  # Now show n = 500 samples
  h2 <- graphics::hist(all_samples, breaks = seq(-4, 4, by = 0.5), plot = FALSE)

  # Fade out old bars
  for (b in bars) {
    sc$play(FadeOut$new(b, run_time = 0.03))
  }
  sc$play(FadeOut$new(n_label, run_time = 0.2))

  bars2 <- list()
  for (i in seq_along(h2$counts)) {
    if (h2$counts[i] == 0) next
    x_left  <- h2$breaks[i]
    x_right <- h2$breaks[i + 1]
    density_val <- h2$counts[i] / (n_samples * bin_width)

    bl <- axes$c2p(x_left, 0)
    tr <- axes$c2p(x_right, density_val)
    w  <- abs(tr[1] - bl[1])
    ht <- abs(tr[2] - bl[2])
    center <- c((bl[1] + tr[1]) / 2, (bl[2] + tr[2]) / 2, 0)

    bar <- Rectangle$new(
      width = w * 0.92,
      height = ht,
      fill_color = col_fill_2sd,
      fill_opacity = 0.55,
      stroke_width = 1,
      color = WHITE,
      position = center
    )
    bar$z_index <- -1
    bars2[[length(bars2) + 1]] <- bar
    sc$add_object(bar)
  }

  for (b in bars2) {
    sc$play(FadeIn$new(b, run_time = 0.05))
  }

  n_label2 <- Text$new(
    text = sprintf("n = %d samples", n_samples),
    font_size = 22,
    color = col_dim,
    position = c(3.5, 2.5, 0)
  )
  sc$add_object(n_label2)
  sc$play(FadeIn$new(n_label2, run_time = 0.3))

  converge_label <- Text$new(
    text = "Histogram converges to the PDF!",
    font_size = 26,
    color = col_accent,
    position = c(0, -3.3, 0)
  )
  sc$add_object(converge_label)
  sc$play(FadeIn$new(converge_label, run_time = 0.5))
  sc$wait(2.0)

  # Fade everything for final card
  for (b in bars2) {
    sc$play(FadeOut$new(b, run_time = 0.02))
  }
  sc$play(FadeOut$new(n_label2, run_time = 0.2))
  sc$play(FadeOut$new(converge_label, run_time = 0.2))
  sc$play(FadeOut$new(sample_title, run_time = 0.2))
  sc$play(FadeOut$new(bell_curve, run_time = 0.3))
  sc$play(FadeOut$new(mean_line, run_time = 0.2))
  sc$play(FadeOut$new(axes, run_time = 0.3))
  sc$play(FadeOut$new(x_label, run_time = 0.1))
  sc$play(FadeOut$new(y_label, run_time = 0.1))
  sc$wait(0.3)

  # ---------------------------------------------------------------
  # SCENE 8 -- Summary Card
  # ---------------------------------------------------------------

  summary_title <- Text$new(
    text = "Key Takeaways",
    font_size = 56,
    color = col_accent,
    weight = "bold",
    position = c(0, 2.5, 0)
  )

  points_text <- list(
    Text$new(text = "1. Symmetric about the mean",
             font_size = 28, color = col_text, position = c(0, 1.2, 0)),
    Text$new(text = "2. Fully described by mean and SD",
             font_size = 28, color = col_text, position = c(0, 0.4, 0)),
    Text$new(text = "3. 68-95-99.7 rule for probability",
             font_size = 28, color = col_text, position = c(0, -0.4, 0)),
    Text$new(text = "4. Sample means converge to it (CLT)",
             font_size = 28, color = col_text, position = c(0, -1.2, 0)),
    Text$new(text = "5. Foundation of statistical inference",
             font_size = 28, color = col_text, position = c(0, -2.0, 0))
  )

  sc$add_object(summary_title)
  sc$play(FadeIn$new(summary_title, run_time = 0.6))

  for (pt in points_text) {
    sc$add_object(pt)
    sc$play(FadeIn$new(pt, run_time = 0.4))
  }
  sc$wait(2.0)

  # ---------------------------------------------------------------
  # Render
  # ---------------------------------------------------------------
  out_path <- render(
    sc,
    output  = output,
    format  = format,
    quality = quality,
    fps     = fps,
    open    = open
  )

  invisible(out_path)
}
