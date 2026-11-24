#' @title Mathematical Objects (MObjects) - Legacy Helpers
#' @description
#' Legacy MObject subtypes and pipe API functions for adding objects to scenes.
#' The main MObject class is defined in mobject.R.
#' @name objects
NULL

#' TextObject R6 Class
#'
#' @description
#' Represents text in a scene.
#'
#' @keywords internal
TextObject <- R6::R6Class(
  "TextObject",
  inherit = MObject,

  public = list(
    #' @field text The text content
    text = "",

    #' @field font Font family
    font = "sans",

    #' @field size Font size
    size = 48,

    #' @field weight Font weight
    weight = "normal",

    #' @description Create a new TextObject
    #' @param text Text content
    #' @param position Position
    #' @param size Font size
    #' @param color Text color
    #' @param font Font family
    #' @param weight Font weight
    #' @param opacity Opacity
    initialize = function(text,
                         position = c(0, 0, 0),
                         size = 48,
                         color = "#FFFFFF",
                         font = "sans",
                         weight = "normal",
                         opacity = 1) {
      super$initialize(
        type = "text",
        position = position,
        color = color,
        opacity = opacity
      )
      self$text <- text
      self$font <- font
      self$size <- size
      self$weight <- weight
    },

    #' @description Get the text content
    get_text = function() {
      self$text
    }
  )
)

#' LaTeXObject R6 Class
#'
#' @description
#' Represents LaTeX mathematical notation in a scene.
#'
#' @keywords internal
#' LaTeXObject R6 Class
#'
#' @description
#' Represents LaTeX mathematical notation in a scene.
#' This object stores the vector data (Bezier curves) of the rendered LaTeX.
#'
#' @keywords internal
LaTeXObject <- R6::R6Class(
  "LaTeXObject",
  inherit = VMobject,

  public = list(
    #' @field latex The LaTeX string
    latex = "",

    #' @field size Font size scale
    size = 1,

    #' @field image_path Path to compiled SVG (kept for ref)
    image_path = NULL,

    #' @field terms Parsed terms for highlighting
    terms = NULL,

    #' @description Create a new LaTeXObject
    #' @param latex LaTeX string
    #' @param color Text color
    #' @param position Position
    #' @param size Size scale
    #' @param opacity Opacity
    #' @param ... Additional parameters
    initialize = function(latex,
                          color = "#FFFFFF",
                          position = c(0, 0, 0),
                          size = 1,
                          opacity = 1,
                          ...) {
      super$initialize(
        position = position,
        color = color,
        opacity = opacity,
        ...
      )
      self$type <- "latex"
      self$latex <- latex
      self$size <- size
      
      # Compile to vector graphics
      self$compile_vector()
      
      # Parse terms (placeholder)
      self$terms <- private$parse_terms(latex)
    },

    #' @description Compile the LaTeX string into vector paths
    compile_vector = function() {
      # Compile to SVG
      svg_path <- compile_latex_svg(self$latex, self$color)
      
      if (!is.null(svg_path)) {
        self$image_path <- svg_path
        self$data$path <- svg_path # Compatibility
        
        # Parse SVG points
        shapes <- import_svg_file(svg_path)
        
        # Combine all shapes into one point matrix for now
        # Ideally we keep them separate for sub-mobject transformations (e.g. TransformMatchingTex)
        # But for MObject parity, we flatten
        if (length(shapes) > 0) {
          # Only take first shape? No, we need all.
          # VMobject supports one list of points currently (Single Continuous Path assumption vs Multi path)
          # We need to handle breaks. My bezier_to_points doesn't handle breaks explicitly yet, 
          # but we can just append them.
          
          # Fix: We need to center the points!
          # dvisvgm coordinates are probably page coordinates or bounding box.
          
          combined_points <- do.call(rbind, shapes)
          
          # Center the points
          # Calculate bounding box
          if (nrow(combined_points) > 0) {
             # dvisvgm usually puts 0,0 at top left? Or bottom left?
             # We need to center around 0,0
             
             min_x <- min(combined_points[, 1])
             max_x <- max(combined_points[, 1])
             min_y <- min(combined_points[, 2])
             max_y <- max(combined_points[, 2])
             
             center_x <- (min_x + max_x) / 2
             center_y <- (min_y + max_y) / 2
             
             combined_points[, 1] <- combined_points[, 1] - center_x
             combined_points[, 2] <- combined_points[, 2] - center_y
             
             # Flip Y? SVG Y is down. Manim Y is up.
             combined_points[, 2] <- -combined_points[, 2]
             
             # Apply size scale
             combined_points <- combined_points * self$size
          }
          
          self$set_points(combined_points)
        }
      }
    },

    #' @description Get the LaTeX string
    get_latex = function() {
      self$latex
    },

    #' @description Get a specific term
    #' @param term_string The term to find
    get_term = function(term_string) {
      if (!is.null(self$terms)) {
        for (term in self$terms) {
          if (term$text == term_string) {
            return(term)
          }
        }
      }
      NULL
    }
  ),

  private = list(
    parse_terms = function(latex) {
      terms <- list()
      terms[[1]] <- list(
        text = latex,
        start = 1,
        end = nchar(latex)
      )
      terms
    }
  )
)

#' ShapeObject R6 Class
#'
#' @description
#' Represents geometric shapes in a scene.
#'
#' @keywords internal
ShapeObject <- R6::R6Class(
  "ShapeObject",
  inherit = MObject,

  public = list(
    #' @field shape_type Type of shape
    shape_type = "rectangle",

    #' @field width Shape width
    width = 1,

    #' @field height Shape height
    height = 1,

    #' @field radius Radius (for circles)
    radius = 0.5,

    #' @field fill_color Fill color
    fill_color = NULL,

    #' @field fill_opacity Fill opacity
    fill_opacity = 0.5,

    #' @field stroke_width Stroke width
    stroke_width = 2,

    #' @description Create a new ShapeObject
    #' @param shape_type Type of shape
    #' @param position Position
    #' @param color Stroke color
    #' @param fill_color Fill color
    #' @param width Width
    #' @param height Height
    #' @param radius Radius
    #' @param stroke_width Stroke width
    #' @param fill_opacity Fill opacity
    #' @param opacity Opacity
    initialize = function(shape_type = "rectangle",
                         position = c(0, 0, 0),
                         color = "#FFFFFF",
                         fill_color = NULL,
                         width = 1,
                         height = 1,
                         radius = 0.5,
                         stroke_width = 2,
                         fill_opacity = 0.5,
                         opacity = 1) {
      super$initialize(
        type = "shape",
        position = position,
        color = color,
        opacity = opacity
      )
      self$shape_type <- shape_type
      self$width <- width
      self$height <- height
      self$radius <- radius
      self$fill_color <- if (!is.null(fill_color)) validate_color(fill_color) else NULL
      self$fill_opacity <- fill_opacity
      self$stroke_width <- stroke_width
    }
  )
)

#' Add text to a scene
#'
#' @description
#' Adds a text element to the scene. Text is rendered using the system font.
#'
#' @param scene A Scene object
#' @param text The text to display
#' @param position Position as c(x, y) or c(x, y, z)
#' @param size Font size in points
#' @param color Text color
#' @param font Font family
#' @param weight Font weight ("normal", "bold")
#' @param opacity Opacity (0 to 1)
#'
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("text_demo") %>%
#'   add_text("Hello, World!", size = 72, color = "yellow") %>%
#'   write_in() %>%
#'   render()
#' }
#'
#' @export
add_text <- function(scene,
                     text,
                     position = c(0, 0, 0),
                     size = 48,
                     color = "#FFFFFF",
                     font = "sans",
                     weight = "normal",
                     opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  obj <- TextObject$new(
    text = text,
    position = position,
    size = size,
    color = color,
    font = font,
    weight = weight,
    opacity = opacity
  )

  scene$add_object(obj)
  invisible(scene)
}

#' Add LaTeX to a scene
#'
#' @description
#' Adds a LaTeX mathematical expression to the scene. Requires LaTeX to be
#' installed on the system.
#'
#' @param scene A Scene object
#' @param latex LaTeX string (without $ delimiters)
#' @param position Position as c(x, y) or c(x, y, z)
#' @param size Size multiplier
#' @param color Text color
#' @param opacity Opacity (0 to 1)
#'
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("latex_demo") %>%
#'   add_latex("E = mc^2") %>%
#'   write_in() %>%
#'   pause(1) %>%
#'   add_latex("\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}",
#'             position = c(0, -2, 0)) %>%
#'   write_in() %>%
#'   render()
#' }
#'
#' @export
add_latex <- function(scene,
                      latex,
                      position = c(0, 0, 0),
                      size = 1,
                      color = "#FFFFFF",
                      opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  obj <- LaTeXObject$new(
    latex = latex,
    position = position,
    size = size,
    color = color,
    opacity = opacity
  )

  scene$add_object(obj)
  invisible(scene)
}

#' Add a shape to a scene
#'
#' @description
#' Adds a geometric shape to the scene.
#'
#' @param scene A Scene object
#' @param shape_type Type of shape ("rectangle", "circle", "triangle", "polygon")
#' @param position Center position
#' @param color Stroke color
#' @param fill_color Fill color (NULL for no fill)
#' @param width Width (for rectangles)
#' @param height Height (for rectangles)
#' @param radius Radius (for circles)
#' @param stroke_width Stroke width
#' @param fill_opacity Fill opacity
#' @param opacity Overall opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("shapes") %>%
#'   add_shape("circle", radius = 1, color = "blue", fill_color = "lightblue") %>%
#'   grow_from_center() %>%
#'   render()
#' }
#'
#' @export
add_shape <- function(scene,
                      shape_type = "rectangle",
                      position = c(0, 0, 0),
                      color = "#FFFFFF",
                      fill_color = NULL,
                      width = 1,
                      height = 1,
                      radius = 0.5,
                      stroke_width = 2,
                      fill_opacity = 0.5,
                      opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  obj <- ShapeObject$new(
    shape_type = shape_type,
    position = position,
    color = color,
    fill_color = fill_color,
    width = width,
    height = height,
    radius = radius,
    stroke_width = stroke_width,
    fill_opacity = fill_opacity,
    opacity = opacity
  )

  scene$add_object(obj)
  invisible(scene)
}

#' Add a circle to a scene
#'
#' @description
#' Convenience function to add a circle to the scene.
#'
#' @param scene A Scene object
#' @param radius Circle radius
#' @param position Center position
#' @param color Stroke color
#' @param fill_color Fill color
#' @param stroke_width Stroke width
#' @param fill_opacity Fill opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("circle_demo") %>%
#'   add_circle(radius = 2, color = "red") %>%
#'   draw_border() %>%
#'   render()
#' }
#'
#' @export
add_circle <- function(scene,
                       radius = 1,
                       position = c(0, 0, 0),
                       color = "#FFFFFF",
                       fill_color = NULL,
                       stroke_width = 2,
                       fill_opacity = 0.5) {

  add_shape(
    scene,
    shape_type = "circle",
    position = position,
    color = color,
    fill_color = fill_color,
    radius = radius,
    stroke_width = stroke_width,
    fill_opacity = fill_opacity
  )
}

#' Add a rectangle to a scene
#'
#' @description
#' Convenience function to add a rectangle to the scene.
#'
#' @param scene A Scene object
#' @param width Rectangle width
#' @param height Rectangle height
#' @param position Center position
#' @param color Stroke color
#' @param fill_color Fill color
#' @param stroke_width Stroke width
#' @param fill_opacity Fill opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @examples
#' \dontrun{
#' scene("rect_demo") %>%
#'   add_rectangle(width = 4, height = 2, fill_color = "green") %>%
#'   grow_from_center() %>%
#'   render()
#' }
#'
#' @export
add_rectangle <- function(scene,
                          width = 2,
                          height = 1,
                          position = c(0, 0, 0),
                          color = "#FFFFFF",
                          fill_color = NULL,
                          stroke_width = 2,
                          fill_opacity = 0.5) {

  add_shape(
    scene,
    shape_type = "rectangle",
    position = position,
    color = color,
    fill_color = fill_color,
    width = width,
    height = height,
    stroke_width = stroke_width,
    fill_opacity = fill_opacity
  )
}

#' Add a function plot to a scene
#'
#' @description
#' Plots a mathematical function as a path.
#'
#' @param scene A Scene object
#' @param fun Function to plot (R function)
#' @param xlim X-axis limits as c(min, max)
#' @param n_points Number of points to sample
#' @param color Line color
#' @param stroke_width Line width
#' @param opacity Opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
add_function_plot <- function(scene,
                               fun,
                               xlim = c(-5, 5),
                               n_points = 100,
                               color = "#FFFFFF",
                               stroke_width = 2,
                               opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  if (!is.function(fun)) {
    cli::cli_abort("{.arg fun} must be a function")
  }

  # Generate points
  x_vals <- seq(xlim[1], xlim[2], length.out = n_points)
  y_vals <- vapply(x_vals, fun, numeric(1))

  # Create path object
  obj <- ShapeObject$new(
    shape_type = "path",
    position = c(0, 0, 0),
    color = color,
    stroke_width = stroke_width,
    opacity = opacity
  )
  obj$data$x <- x_vals
  obj$data$y <- y_vals

  scene$add_object(obj)
  invisible(scene)
}

#' Add a line to a scene
#'
#' @description
#' Adds a line between two points.
#'
#' @param scene A Scene object
#' @param start Start point
#' @param end End point
#' @param color Line color
#' @param stroke_width Line width
#' @param opacity Opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
add_line <- function(scene,
                     start = c(-1, 0, 0),
                     end = c(1, 0, 0),
                     color = "#FFFFFF",
                     stroke_width = 2,
                     opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  obj <- ShapeObject$new(
    shape_type = "line",
    position = start,  # Start point
    color = color,
    stroke_width = stroke_width,
    opacity = opacity
  )
  obj$data$end <- validate_position(end)

  scene$add_object(obj)
  invisible(scene)
}


#' Add a path (polyline) to a scene
#'
#' @description
#' Adds a smooth path through a series of (x, y) coordinates. Useful for
#' plotting function curves and data traces.
#'
#' @param scene A Scene object
#' @param x Numeric vector of x coordinates
#' @param y Numeric vector of y coordinates
#' @param color Path color
#' @param stroke_width Line width
#' @param opacity Opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
add_path <- function(scene,
                     x, y,
                     color = "#FFFFFF",
                     stroke_width = 2,
                     opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  n <- length(x)
  if (length(y) != n) {
    cli::cli_abort("{.arg x} and {.arg y} must have the same length")
  }
  if (n < 2) {
    cli::cli_abort("Path needs at least 2 points")
  }

  # Build a ShapeObject of type "path" that stores the polyline coordinates
  obj <- ShapeObject$new(
    shape_type = "path",
    position = c(0, 0, 0),
    color = color,
    stroke_width = stroke_width,
    opacity = opacity
  )
  obj$data$x <- x
  obj$data$y <- y

  scene$add_object(obj)
  invisible(scene)
}


#' Add an arrow to a scene
#'
#' @description
#' Adds an arrow between two points.
#'
#' @param scene A Scene object
#' @param start Start point
#' @param end End point (where arrow head points)
#' @param color Arrow color
#' @param stroke_width Line width
#' @param tip_size Size of the arrow head
#' @param opacity Opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
add_arrow <- function(scene,
                      start = c(-1, 0, 0),
                      end = c(1, 0, 0),
                      color = "#FFFFFF",
                      stroke_width = 2,
                      tip_size = 0.2,
                      opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  obj <- ShapeObject$new(
    shape_type = "arrow",
    position = start,
    color = color,
    stroke_width = stroke_width,
    opacity = opacity
  )
  obj$data$end <- validate_position(end)
  obj$data$tip_size <- tip_size

  scene$add_object(obj)
  invisible(scene)
}

#' Add an image to a scene
#'
#' @description
#' Adds an image file to the scene.
#'
#' @param scene A Scene object
#' @param path Path to the image file
#' @param position Center position
#' @param width Desired width (height scales proportionally)
#' @param height Desired height (width scales proportionally)
#' @param opacity Opacity
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
add_image <- function(scene,
                      path,
                      position = c(0, 0, 0),
                      width = NULL,
                      height = NULL,
                      opacity = 1) {

  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Image file not found: {.path {path}}")
  }

  obj <- MObject$new(
    type = "image",
    position = position,
    opacity = opacity
  )
  obj$data$path <- normalizePath(path)
  obj$data$width <- width
  obj$data$height <- height

  scene$add_object(obj)
  invisible(scene)
}

#' Add a generic mobject to a scene
#'
#' @description
#' Adds a pre-created MObject to the scene.
#'
#' @param scene A Scene object
#' @param mobject An MObject
#'
#' @return The Scene object (invisibly) for piping
#'
#' @export
add_mobject <- function(scene, mobject) {
  if (!inherits(scene, "Scene")) {
    cli::cli_abort("{.arg scene} must be a Scene object")
  }

  if (!inherits(mobject, "MObject")) {
    cli::cli_abort("{.arg mobject} must be an MObject")
  }

  scene$add_object(mobject)
  invisible(scene)
}
