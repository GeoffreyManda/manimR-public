#' @title Grammar of Animated Graphics
#' @description
#' A ggplot2-inspired declarative API for building mathematical animations.
#' Uses a layered grammar where you specify: data, aesthetic mappings,
#' geometric representations, animation transitions, and timing.
#'
#' @details
#' The grammar follows ggplot2 conventions but extends them for animation:
#'
#' ```r
#' manim_plot(data) +
#'   anim_point(aes(x, y, color = group)) +
#'   anim_transition(type = "fade") +
#'   anim_timing(duration = 3, easing = "smooth") +
#'   anim_camera(zoom = 1.5) +
#'   anim_theme_dark()
#' ```
#'
#' @name grammar
#' @keywords internal
NULL

#' Create a manim animation plot
#'
#' @description
#' The starting point for the grammar of animated graphics. Similar to
#' ggplot2's `ggplot()`, this creates an animation specification that
#' layers are added to with `+`.
#'
#' @param data A data.frame (optional, can be provided per layer)
#' @param mapping Default aesthetic mapping (via `aes()`)
#' @param scene_config List of scene configuration options
#' @return A ManimPlot object (list with class "ManimPlot")
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple point animation
#' df <- data.frame(x = 1:10, y = sin(1:10))
#' manim_plot(df) +
#'   anim_point(aes(x, y)) +
#'   anim_transition("grow") +
#'   anim_render()
#'
#' # Function animation
#' manim_plot() +
#'   anim_function(function(x) sin(x), x_range = c(-PI, PI)) +
#'   anim_transition("create") +
#'   anim_render()
#' }
manim_plot <- function(data = NULL, mapping = NULL, scene_config = NULL) {
  plot <- list(
    data = data,
    mapping = mapping,
    layers = list(),
    transitions = list(),
    timing = list(duration = 3, fps = 30, easing = "smooth"),
    camera_config = list(),
    theme = list(background = BLACK, foreground = WHITE),
    scene_config = scene_config %||% list()
  )
  class(plot) <- "ManimPlot"
  plot
}


#' Aesthetic mapping for manim grammar
#'
#' @description
#' Maps data columns to visual properties. Extends ggplot2's `aes()` with
#' animation-specific aesthetics (time, phase, group_anim).
#'
#' @param x X aesthetic
#' @param y Y aesthetic
#' @param color Color aesthetic
#' @param fill Fill aesthetic
#' @param size Size aesthetic
#' @param alpha Opacity aesthetic
#' @param label Label aesthetic
#' @param group Group aesthetic
#' @param time Time aesthetic (for temporal data)
#' @param phase Animation phase grouping
#' @param ... Additional aesthetics
#' @return A list of aesthetic mappings
#' @export
aes <- function(x = NULL, y = NULL, color = NULL, fill = NULL,
                size = NULL, alpha = NULL, label = NULL,
                group = NULL, time = NULL, phase = NULL, ...) {
  mapping <- list()

  # Capture expressions as quosures for later evaluation
  mc <- match.call(expand.dots = TRUE)
  for (nm in names(mc)[-1]) {
    mapping[[nm]] <- mc[[nm]]
  }

  class(mapping) <- "ManimAes"
  mapping
}


#' Add operator for ManimPlot layers
#'
#' @param plot A ManimPlot object
#' @param layer A layer, transition, or theme to add
#' @return Modified ManimPlot
#' @export
`+.ManimPlot` <- function(plot, layer) {
  if (inherits(layer, "ManimLayer")) {
    plot$layers <- c(plot$layers, list(layer))
  } else if (inherits(layer, "ManimTransition")) {
    plot$transitions <- c(plot$transitions, list(layer))
  } else if (inherits(layer, "ManimTiming")) {
    plot$timing <- modifyList(plot$timing, layer)
  } else if (inherits(layer, "ManimCamera")) {
    plot$camera_config <- modifyList(plot$camera_config, layer)
  } else if (inherits(layer, "ManimTheme")) {
    plot$theme <- modifyList(plot$theme, layer)
  } else if (inherits(layer, "ManimRender")) {
    return(render_manim_plot(plot, layer))
  }
  plot
}

#' Print method for ManimPlot
#' @param x A ManimPlot object
#' @param ... Ignored
#' @return `x`, invisibly
#' @export
print.ManimPlot <- function(x, ...) {
  cat("<ManimPlot>\n")
  cat("  Layers:", length(x$layers), "\n")
  cat("  Transitions:", length(x$transitions), "\n")
  cat("  Duration:", x$timing$duration, "s @", x$timing$fps, "fps\n")
  if (!is.null(x$data)) {
    cat("  Data:", nrow(x$data), "rows x", ncol(x$data), "cols\n")
  }
  invisible(x)
}


# ---- Layer functions ----

#' Add animated points
#'
#' @param mapping Aesthetic mapping
#' @param data Optional data override
#' @param dot_radius Radius of dots
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_point <- function(mapping = NULL, data = NULL,
                       dot_radius = DEFAULT_DOT_RADIUS, ...) {
  layer <- list(
    type = "point",
    mapping = mapping,
    data = data,
    params = list(dot_radius = dot_radius, ...)
  )
  class(layer) <- "ManimLayer"
  layer
}


#' Add animated lines/curves
#'
#' @param mapping Aesthetic mapping
#' @param data Optional data override
#' @param stroke_width Stroke width
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_line <- function(mapping = NULL, data = NULL,
                      stroke_width = DEFAULT_STROKE_WIDTH, ...) {
  layer <- list(
    type = "line",
    mapping = mapping,
    data = data,
    params = list(stroke_width = stroke_width, ...)
  )
  class(layer) <- "ManimLayer"
  layer
}


#' Add animated bars
#'
#' @param mapping Aesthetic mapping
#' @param data Optional data override
#' @param bar_width Width of bars
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_bar <- function(mapping = NULL, data = NULL,
                     bar_width = 0.8, ...) {
  layer <- list(
    type = "bar",
    mapping = mapping,
    data = data,
    params = list(bar_width = bar_width, ...)
  )
  class(layer) <- "ManimLayer"
  layer
}


#' Add animated text labels
#'
#' @param mapping Aesthetic mapping
#' @param data Optional data override
#' @param font_size Font size
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_text <- function(mapping = NULL, data = NULL,
                      font_size = DEFAULT_FONT_SIZE, ...) {
  layer <- list(
    type = "text",
    mapping = mapping,
    data = data,
    params = list(font_size = font_size, ...)
  )
  class(layer) <- "ManimLayer"
  layer
}


#' Add an animated mathematical function
#'
#' @param func Function(x) -> y
#' @param x_range c(min, max) for x values
#' @param color Curve color
#' @param stroke_width Stroke width
#' @param num_points Sampling resolution
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_function <- function(func, x_range = c(-5, 5), color = YELLOW,
                          stroke_width = DEFAULT_STROKE_WIDTH,
                          num_points = 200, ...) {
  layer <- list(
    type = "function",
    mapping = NULL,
    data = NULL,
    params = list(
      func = func, x_range = x_range, color = color,
      stroke_width = stroke_width, num_points = num_points, ...
    )
  )
  class(layer) <- "ManimLayer"
  layer
}


#' Add animated axes
#'
#' @param x_range X axis range c(min, max, step)
#' @param y_range Y axis range c(min, max, step)
#' @param include_numbers Show number labels
#' @param tips Show arrow tips
#' @param color Axis color
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_axes <- function(x_range = c(-5, 5, 1), y_range = c(-3, 3, 1),
                      include_numbers = TRUE, tips = TRUE,
                      color = WHITE, ...) {
  layer <- list(
    type = "axes",
    mapping = NULL,
    data = NULL,
    params = list(
      x_range = x_range, y_range = y_range,
      include_numbers = include_numbers, tips = tips,
      color = color, ...
    )
  )
  class(layer) <- "ManimLayer"
  layer
}


#' Add a LaTeX equation
#'
#' @param tex LaTeX string
#' @param color Color
#' @param font_size Font size
#' @param position Position c(x, y, z)
#' @param ... Additional args
#' @return A ManimLayer
#' @export
anim_latex <- function(tex, color = WHITE, font_size = DEFAULT_FONT_SIZE,
                       position = ORIGIN, ...) {
  layer <- list(
    type = "latex",
    mapping = NULL,
    data = NULL,
    params = list(
      tex = tex, color = color, font_size = font_size,
      position = position, ...
    )
  )
  class(layer) <- "ManimLayer"
  layer
}


# ---- Transition functions ----

#' Specify animation transition type
#'
#' @param type Transition type: "create", "fade", "grow", "transform",
#'   "write", "draw_border", "none"
#' @param ... Override parameters (e.g., shift direction for fade)
#' @return A ManimTransition
#' @export
anim_transition <- function(type = "create", ...) {
  trans <- list(type = type, params = list(...))
  class(trans) <- "ManimTransition"
  trans
}


# ---- Timing functions ----

#' Set animation timing
#'
#' @param duration Total duration in seconds
#' @param fps Frames per second
#' @param easing Easing function name
#' @param lag_ratio Lag ratio for staggered animations (0-1)
#' @return A ManimTiming
#' @export
anim_timing <- function(duration = 3, fps = 30, easing = "smooth",
                        lag_ratio = 0.05) {
  timing <- list(duration = duration, fps = fps, easing = easing,
                 lag_ratio = lag_ratio)
  class(timing) <- "ManimTiming"
  timing
}


# ---- Camera functions ----

#' Set camera configuration
#'
#' @param zoom Zoom factor
#' @param center Camera center position
#' @param rotation Camera rotation (radians)
#' @param phi 3D azimuth angle
#' @param theta 3D elevation angle
#' @return A ManimCamera
#' @export
anim_camera <- function(zoom = 1, center = ORIGIN, rotation = 0,
                        phi = NULL, theta = NULL) {
  cam <- list(zoom = zoom, center = center, rotation = rotation,
              phi = phi, theta = theta)
  class(cam) <- "ManimCamera"
  cam
}


# ---- Theme functions ----

#' Dark theme (default manim look)
#' @return A ManimTheme
#' @export
anim_theme_dark <- function() {
  theme <- list(background = BLACK, foreground = WHITE,
                accent = BLUE, grid_color = GREY_D)
  class(theme) <- "ManimTheme"
  theme
}


#' Light theme
#' @return A ManimTheme
#' @export
anim_theme_light <- function() {
  theme <- list(background = WHITE, foreground = BLACK,
                accent = BLUE_D, grid_color = GREY_B)
  class(theme) <- "ManimTheme"
  theme
}


#' 3Blue1Brown theme
#' @return A ManimTheme
#' @export
anim_theme_3b1b <- function() {
  theme <- list(background = "#1C1C1C", foreground = WHITE,
                accent = BLUE_D, grid_color = "#333333")
  class(theme) <- "ManimTheme"
  theme
}


#' Academic theme (LaTeX-friendly)
#' @return A ManimTheme
#' @export
anim_theme_academic <- function() {
  theme <- list(background = "#FFFFF8", foreground = "#333333",
                accent = "#2B4C7E", grid_color = "#DDDDDD")
  class(theme) <- "ManimTheme"
  theme
}


# ---- Render trigger ----

#' Trigger rendering of the ManimPlot
#'
#' @param output Output filename
#' @param format Output format ("mp4", "gif", "svg")
#' @param quality Quality preset
#' @param preview Open preview after render
#' @return A ManimRender specification
#' @export
anim_render <- function(output = NULL, format = "mp4",
                        quality = "medium", preview = FALSE) {
  spec <- list(output = output, format = format,
               quality = quality, preview = preview)
  class(spec) <- "ManimRender"
  spec
}


# ---- Internal: build and render ----

#' Build a ManimPlot into a Scene
#'
#' @param plot A ManimPlot object
#' @return A Scene object
#' @keywords internal
build_manim_plot <- function(plot) {
  # Create scene
  sc <- Scene$new(
    name = "grammar_plot",
    background_color = plot$theme$background %||% BLACK,
    fps = plot$timing$fps %||% 30
  )

  # Resolve default transition
  default_transition <- if (length(plot$transitions) > 0) {
    plot$transitions[[1]]
  } else {
    list(type = "create", params = list())
  }

  axes_obj <- NULL  # For coordinate mapping

  # Build each layer
  for (li in seq_along(plot$layers)) {
    layer <- plot$layers[[li]]
    transition <- if (li <= length(plot$transitions)) {
      plot$transitions[[li]]
    } else {
      default_transition
    }

    # Merge mapping
    mapping <- layer$mapping %||% plot$mapping

    # Merge data
    data <- layer$data %||% plot$data

    # Build mobjects for this layer
    mobjects <- private_build_layer(layer, data, mapping, axes_obj, plot$theme)

    # If this layer created axes, store for later layers
    if (layer$type == "axes" && length(mobjects) > 0) {
      axes_obj <- mobjects[[1]]
    }

    # Add to scene with animation
    duration_per_layer <- (plot$timing$duration %||% 3) / max(length(plot$layers), 1)

    for (mob in mobjects) {
      # Create animation based on transition type
      anim <- private_create_transition(mob, transition, plot$timing)
      sc$add(mob)
      sc$play(anim)
    }
  }

  sc
}


#' Build a layer into mobjects
#' @keywords internal
private_build_layer <- function(layer, data, mapping, axes, theme) {
  switch(layer$type,
    "point" = private_build_points(data, mapping, layer$params, axes, theme),
    "line" = private_build_lines(data, mapping, layer$params, axes, theme),
    "bar" = private_build_bars(data, mapping, layer$params, axes, theme),
    "text" = private_build_texts(data, mapping, layer$params, axes, theme),
    "function" = private_build_function(layer$params, axes, theme),
    "axes" = private_build_axes(layer$params, theme),
    "latex" = private_build_latex(layer$params, theme),
    list()
  )
}


#' Build point layer
#' @keywords internal
private_build_points <- function(data, mapping, params, axes, theme) {
  if (is.null(data) || is.null(mapping)) return(list())

  x_col <- as.character(mapping$x)
  y_col <- as.character(mapping$y)
  color_col <- if (!is.null(mapping$color)) as.character(mapping$color) else NULL

  dots <- list()
  radius <- params$dot_radius %||% DEFAULT_DOT_RADIUS

  for (i in seq_len(nrow(data))) {
    x_val <- data[[x_col]][i]
    y_val <- data[[y_col]][i]

    # Map through axes if available
    pos <- if (!is.null(axes) && inherits(axes, "Axes")) {
      axes$c2p(x_val, y_val)
    } else {
      c(x_val, y_val, 0)
    }

    # Color
    col <- if (!is.null(color_col) && color_col %in% names(data)) {
      # Map factor/numeric to color
      val <- data[[color_col]][i]
      if (is.numeric(val)) {
        grad <- color_gradient(c(BLUE, RED), 256)
        rng <- range(data[[color_col]], na.rm = TRUE)
        t <- (val - rng[1]) / max(rng[2] - rng[1], 1e-8)
        grad[max(1, ceiling(t * 256))]
      } else {
        # Use default palette for categorical
        palette <- c(BLUE, RED, GREEN, YELLOW, PURPLE, ORANGE, TEAL, PINK)
        levels <- unique(data[[color_col]])
        idx <- match(val, levels)
        palette[((idx - 1) %% length(palette)) + 1]
      }
    } else {
      theme$accent %||% BLUE
    }

    dot <- Dot$new(point = pos, radius = radius, color = col)
    dots[[length(dots) + 1]] <- dot
  }

  dots
}


#' Build line layer
#' @keywords internal
private_build_lines <- function(data, mapping, params, axes, theme) {
  if (is.null(data) || is.null(mapping)) return(list())

  x_col <- as.character(mapping$x)
  y_col <- as.character(mapping$y)

  x_vals <- data[[x_col]]
  y_vals <- data[[y_col]]

  if (!is.null(axes) && inherits(axes, "Axes")) {
    line <- axes$plot_line_graph(x_vals, y_vals,
                                 color = params$color %||% theme$accent %||% YELLOW,
                                 stroke_width = params$stroke_width %||% DEFAULT_STROKE_WIDTH)
  } else {
    line <- VMobject$new(
      color = params$color %||% YELLOW,
      stroke_width = params$stroke_width %||% DEFAULT_STROKE_WIDTH
    )
    pts <- matrix(0, nrow = length(x_vals), ncol = 3)
    for (i in seq_along(x_vals)) pts[i, ] <- c(x_vals[i], y_vals[i], 0)
    line$points <- pts
  }

  list(line)
}


#' Build bar layer
#' @keywords internal
private_build_bars <- function(data, mapping, params, axes, theme) {
  if (is.null(data) || is.null(mapping)) return(list())

  x_col <- as.character(mapping$x)
  y_col <- as.character(mapping$y)
  bar_width <- params$bar_width %||% 0.8

  bars <- list()
  for (i in seq_len(nrow(data))) {
    x_val <- data[[x_col]][i]
    y_val <- data[[y_col]][i]

    if (!is.null(axes) && inherits(axes, "Axes")) {
      bottom <- axes$c2p(x_val, 0)
      top <- axes$c2p(x_val, y_val)
    } else {
      bottom <- c(x_val, 0, 0)
      top <- c(x_val, y_val, 0)
    }

    height <- abs(top[2] - bottom[2])
    center <- (bottom + top) / 2

    bar <- Rectangle$new(
      width = bar_width,
      height = height,
      fill_color = theme$accent %||% BLUE,
      fill_opacity = 0.8,
      stroke_width = 1,
      color = WHITE,
      position = center
    )
    bars[[length(bars) + 1]] <- bar
  }

  bars
}


#' Build text layer
#' @keywords internal
private_build_texts <- function(data, mapping, params, axes, theme) {
  if (is.null(data) || is.null(mapping)) return(list())

  x_col <- as.character(mapping$x)
  y_col <- as.character(mapping$y)
  label_col <- if (!is.null(mapping$label)) as.character(mapping$label) else NULL

  texts <- list()
  for (i in seq_len(nrow(data))) {
    x_val <- data[[x_col]][i]
    y_val <- data[[y_col]][i]
    lbl <- if (!is.null(label_col)) as.character(data[[label_col]][i])
           else as.character(i)

    pos <- if (!is.null(axes)) axes$c2p(x_val, y_val) else c(x_val, y_val, 0)

    txt <- Text$new(
      text = lbl,
      font_size = params$font_size %||% DEFAULT_FONT_SIZE,
      color = theme$foreground %||% WHITE,
      position = pos
    )
    texts[[length(texts) + 1]] <- txt
  }

  texts
}


#' Build function layer
#' @keywords internal
private_build_function <- function(params, axes, theme) {
  func <- params$func
  x_range <- params$x_range %||% c(-5, 5)
  color <- params$color %||% YELLOW
  sw <- params$stroke_width %||% DEFAULT_STROKE_WIDTH

  if (!is.null(axes) && inherits(axes, "Axes")) {
    graph <- axes$plot(func, x_range = x_range, color = color,
                       stroke_width = sw, num_points = params$num_points %||% 200)
    list(graph)
  } else {
    # Create standalone curve
    x_vals <- seq(x_range[1], x_range[2], length.out = params$num_points %||% 200)
    y_vals <- vapply(x_vals, func, numeric(1))

    vm <- VMobject$new(color = color, stroke_width = sw)
    pts <- matrix(0, nrow = length(x_vals), ncol = 3)
    for (i in seq_along(x_vals)) pts[i, ] <- c(x_vals[i], y_vals[i], 0)

    # Fit Bezier
    bezier_pts <- matrix(numeric(0), ncol = 3)
    j <- 1
    while (j + 3 <= nrow(pts)) {
      bezier_pts <- rbind(bezier_pts, pts[j,], pts[j+1,], pts[j+2,], pts[j+3,])
      j <- j + 3
    }
    vm$points <- bezier_pts
    list(vm)
  }
}


#' Build axes layer
#' @keywords internal
private_build_axes <- function(params, theme) {
  ax <- Axes$new(
    x_range = params$x_range %||% c(-5, 5, 1),
    y_range = params$y_range %||% c(-3, 3, 1),
    include_numbers = params$include_numbers %||% FALSE,
    tips = params$tips %||% TRUE,
    color = params$color %||% theme$foreground %||% WHITE
  )
  list(ax)
}


#' Build LaTeX layer
#' @keywords internal
private_build_latex <- function(params, theme) {
  tex <- MathTex$new(
    tex_string = params$tex,
    color = params$color %||% theme$foreground %||% WHITE,
    font_size = params$font_size %||% DEFAULT_FONT_SIZE,
    position = params$position %||% ORIGIN
  )
  list(tex)
}


#' Create transition animation for a mobject
#' @keywords internal
private_create_transition <- function(mob, transition, timing) {
  type <- transition$type %||% "create"
  dur <- timing$duration %||% 1
  easing <- timing$easing %||% "smooth"

  switch(type,
    "create" = Create$new(mob, run_time = dur, rate_func = get_easing(easing)),
    "fade" = FadeIn$new(mob, run_time = dur, rate_func = get_easing(easing),
                        shift = transition$params$shift),
    "grow" = GrowFromCenter$new(mob, run_time = dur, rate_func = get_easing(easing)),
    "write" = Write$new(mob, run_time = dur, rate_func = get_easing(easing)),
    "draw_border" = DrawBorderThenFill$new(mob, run_time = dur, rate_func = get_easing(easing)),
    "transform" = Create$new(mob, run_time = dur, rate_func = get_easing(easing)),
    "none" = {
      mob$visible <- TRUE
      mob$opacity <- 1
      Wait$new(mob, run_time = 0.01)
    },
    Create$new(mob, run_time = dur, rate_func = get_easing(easing))
  )
}


#' Render a ManimPlot
#' @keywords internal
render_manim_plot <- function(plot, render_spec) {
  scene <- build_manim_plot(plot)

  render(
    scene = scene,
    output = render_spec$output,
    format = render_spec$format %||% "mp4",
    quality = render_spec$quality %||% "medium",
    preview = render_spec$preview %||% FALSE
  )
}
