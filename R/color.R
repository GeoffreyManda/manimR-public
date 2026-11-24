#' @title Color System for manimR
#' @description
#' Comprehensive color management including gradients, interpolation,
#' and integration with R color palettes.
#' @name color_system
#' @keywords internal
NULL

#' Validate and normalize a color specification
#'
#' @description Converts any color input to a standardized hex string.
#'   Accepts hex strings, named R colors, RGB/RGBA numeric vectors,
#'   and manimR color constants.
#'
#' @param color A color specification (hex string, color name, or RGB vector)
#' @return A standardized uppercase hex color string (#RRGGBB or #RRGGBBAA)
#' @keywords internal
#' @noRd
validate_color <- function(color) {
  if (is.null(color)) return("#FFFFFF")

  if (is.character(color) && length(color) == 1) {
    # Already hex
    if (grepl("^#[0-9A-Fa-f]{6}$", color) || grepl("^#[0-9A-Fa-f]{8}$", color)) {
      return(toupper(color))
    }
    # Named R color
    if (color %in% grDevices::colors()) {
      rgb_vals <- grDevices::col2rgb(color)
      return(grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255))
    }
    # Try as-is (might be a constant that resolved to hex)
    if (nchar(color) > 0) {
      tryCatch({
        rgb_vals <- grDevices::col2rgb(color)
        return(grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255))
      }, error = function(e) {
        cli::cli_abort("Invalid color specification: {.val {color}}")
      })
    }
  }

  if (is.numeric(color)) {
    if (length(color) == 3) {
      # Values in [0,1]
      if (all(color >= 0 & color <= 1)) {
        return(grDevices::rgb(color[1], color[2], color[3]))
      }
      # Values in [0,255]
      return(grDevices::rgb(color[1], color[2], color[3], maxColorValue = 255))
    }
    if (length(color) == 4) {
      if (all(color >= 0 & color <= 1)) {
        return(grDevices::rgb(color[1], color[2], color[3], color[4]))
      }
      return(grDevices::rgb(color[1], color[2], color[3], color[4], maxColorValue = 255))
    }
  }

  cli::cli_abort("Invalid color specification: {.val {deparse(color)}}")
}

#' Convert hex color to RGB vector
#'
#' @param hex Hex color string
#' @return Numeric vector of length 3 (values 0-1)
#' @keywords internal
#' @noRd
hex_to_rgb <- function(hex) {
  rgb_mat <- grDevices::col2rgb(hex)
  as.numeric(rgb_mat[, 1]) / 255
}

#' Convert RGB vector to hex string
#'
#' @param rgb Numeric vector of length 3 (values 0-1)
#' @return Hex color string
#' @keywords internal
#' @noRd
rgb_to_hex <- function(rgb) {
  grDevices::rgb(rgb[1], rgb[2], rgb[3])
}

#' Interpolate between two colors
#'
#' @description Linearly interpolates between two colors in RGB space.
#'
#' @param color1 Start color (hex string or name)
#' @param color2 End color (hex string or name)
#' @param alpha Interpolation factor (0 = color1, 1 = color2)
#' @return Hex color string
#'
#' @export
interpolate_color <- function(color1, color2, alpha) {
  c1 <- hex_to_rgb(validate_color(color1))
  c2 <- hex_to_rgb(validate_color(color2))
  result <- c1 + (c2 - c1) * alpha
  result <- pmax(0, pmin(1, result))
  rgb_to_hex(result)
}

#' Create a color gradient
#'
#' @description Generates a vector of colors forming a smooth gradient.
#'
#' @param colors Vector of color specifications (at least 2)
#' @param n Number of colors to generate
#' @return Character vector of hex colors
#'
#' @export
color_gradient <- function(colors, n = 10) {
  if (length(colors) < 2) {
    cli::cli_abort("Need at least 2 colors for a gradient")
  }

  colors <- vapply(colors, validate_color, character(1))
  n_segments <- length(colors) - 1
  per_segment <- ceiling(n / n_segments)

  result <- character(0)
  for (i in seq_len(n_segments)) {
    alphas <- seq(0, 1, length.out = per_segment + 1)
    if (i < n_segments) alphas <- alphas[-length(alphas)]
    segment <- vapply(alphas, function(a) interpolate_color(colors[i], colors[i + 1], a), character(1))
    result <- c(result, segment)
  }

  result[seq_len(n)]
}

#' Get a predefined color palette
#'
#' @description Returns a named list of colors from a predefined palette.
#'
#' @param palette Name of the palette. Options: "manim", "3b1b", "academic",
#'   "pastel", "neon", "earth"
#' @return Named character vector of hex colors
#'
#' @export
manim_palette <- function(palette = "manim") {
  switch(palette,
    manim = c(
      blue = BLUE, teal = TEAL, green = GREEN, yellow = YELLOW,
      gold = GOLD, red = RED, maroon = MAROON, purple = PURPLE,
      pink = PINK, orange = ORANGE, white = WHITE, grey = GREY
    ),
    "3b1b" = c(
      blue = "#1C758A", brown = "#736357", cream = "#FFFFDD",
      grey = "#888888", pink = "#D147BD", green = "#83C167",
      yellow = "#FFFF00", red = "#FC6255"
    ),
    academic = c(
      primary = "#2C3E50", secondary = "#E74C3C", accent = "#3498DB",
      success = "#27AE60", warning = "#F39C12", info = "#1ABC9C",
      light = "#ECF0F1", dark = "#34495E"
    ),
    pastel = c(
      blue = "#A8D8EA", pink = "#FFB6C1", green = "#B5EAD7",
      yellow = "#FFEAA7", purple = "#C3AED6", orange = "#FFD3B6",
      mint = "#DCEDC1", lavender = "#E2D1F9"
    ),
    neon = c(
      cyan = "#00FFFF", magenta = "#FF00FF", green = "#39FF14",
      yellow = "#FFFF00", orange = "#FF6600", pink = "#FF1493",
      blue = "#0000FF", red = "#FF0000"
    ),
    earth = c(
      forest = "#228B22", ocean = "#006994", sand = "#C2B280",
      clay = "#B66A50", sky = "#87CEEB", stone = "#808588",
      moss = "#8A9A5B", bark = "#8B4513"
    ),
    cli::cli_abort("Unknown palette: {.val {palette}}")
  )
}

#' Apply color to submobjects by gradient
#'
#' @description Applies a gradient of colors across submobjects of a VGroup
#'   or VMobject with children.
#'
#' @param mobject A VMobject or VGroup
#' @param ... Colors to gradient between
#' @return The mobject (modified in place)
#'
#' @keywords internal
set_color_by_gradient <- function(mobject, ...) {
  colors <- list(...)
  if (length(colors) < 2) return(mobject)

  colors <- vapply(colors, validate_color, character(1))

  if (inherits(mobject, "VGroup") && length(mobject$submobjects) > 0) {
    n <- length(mobject$submobjects)
    gradient <- color_gradient(colors, n)
    for (i in seq_len(n)) {
      mobject$submobjects[[i]]$color <- gradient[i]
    }
  } else {
    mobject$color <- validate_color(colors[[1]])
  }

  invisible(mobject)
}
