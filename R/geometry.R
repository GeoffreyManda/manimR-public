# Note: Line, Arrow, and Vector are defined in geometry_line.R with richer APIs
# (get_length, get_angle, put_start_and_end_on, ArrowTip submobjects, etc.)
# Do NOT redefine them here -- geometry.R loads after geometry_line.R per Collate
# order, so any definitions here would silently override the richer versions.

#' @title Circle
#' @description A circular VMobject.
#' @export
Circle <- R6::R6Class("Circle",
  inherit = VMobject,
  public = list(
    radius = 1,
    initialize = function(radius = 1, color = "#FFFFFF", ...) {
      super$initialize(...)
      self$radius <- radius
      self$color <- color
      self$generate_points()
    },
    
    generate_points = function() {
      # Bezier approximation: 4 segments
      # Handle = radius * 0.55191502449
      k <- 0.55191502449 * self$radius
      r <- self$radius
      
      # Points (Right, Top, Left, Bottom)
      p0 <- c(r, 0, 0)
      p1 <- c(0, r, 0)
      p2 <- c(-r, 0, 0)
      p3 <- c(0, -r, 0)
      
      # Handles
      # Q1: p0+up*k, p1+right*k (Wait, handles are tangent)
      # Tangent at p0 (1,0) is vertical (0,1).
      h0a <- c(r, k, 0)
      h1b <- c(k, r, 0)
      
      # Q2
      h1a <- c(-k, r, 0)
      h2b <- c(-r, k, 0)
      
      # Q3
      h2a <- c(-r, -k, 0)
      h3b <- c(-k, -r, 0)
      
      # Q4
      h3a <- c(k, -r, 0)
      h0b <- c(r, -k, 0)
      
      self$add_cubic_bezier(p0, h0a, h1b, p1)
      self$add_cubic_bezier(p1, h1a, h2b, p2)
      self$add_cubic_bezier(p2, h2a, h3b, p3)
      self$add_cubic_bezier(p3, h3a, h0b, p0)
    }
  )
)

#' @title Dot
#' @description A solid dot (small circle).
#' @export
Dot <- R6::R6Class("Dot",
  inherit = Circle,
  public = list(
    initialize = function(point = c(0, 0, 0), radius = 0.08, color = "#FFFFFF", ...) {
      super$initialize(radius = radius, color = color, ...)
      self$position <- point
      self$fill_opacity <- 1.0 # Dots are filled
      self$fill_color <- color
      self$stroke_width <- 0 # Usually no stroke
    }
  )
)

#' @title Rectangle
#' @description A simple rectangle.
#' @export
Rectangle <- R6::R6Class("Rectangle",
  inherit = VMobject,
  public = list(
    width = 1,
    height = 1,
    initialize = function(width = 4, height = 2, color = "#FFFFFF", ...) {
      super$initialize(...)
      self$width <- width
      self$height <- height
      self$color <- color
      self$generate_points()
    },
    
    generate_points = function() {
      w <- self$width / 2
      h <- self$height / 2
      
      colors <- c(w, h, 0)
      ul <- c(-w, h, 0)
      dl <- c(-w, -h, 0)
      dr <- c(w, -h, 0)
      ur <- c(w, h, 0)
      
      self$add_line(ul, dl)
      self$add_line(dl, dr)
      self$add_line(dr, ur)
      self$add_line(ur, ul)
    }
  )
)

#' @title Square
#' @description A square.
#' @export
Square <- R6::R6Class("Square",
  inherit = Rectangle,
  public = list(
    side_length = 2,
    initialize = function(side_length = 2, ...) {
      super$initialize(width = side_length, height = side_length, ...)
      self$side_length <- side_length
    }
  )
)
