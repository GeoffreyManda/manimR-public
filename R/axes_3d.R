#' @title ThreeDAxes
#' @description 3D Axes.
#' @export
ThreeDAxes <- R6::R6Class("ThreeDAxes",
  inherit = Axes,
  public = list(
    z_range = NULL,
    z_length = NULL,
    
    initialize = function(x_range = c(-5, 5, 1), 
                          y_range = c(-5, 5, 1),
                          z_range = c(-3, 3, 1),
                          x_length = 10,
                          y_length = 10,
                          z_length = 6,
                          ...) {
      self$z_range <- z_range
      self$z_length <- z_length
      
      super$initialize(x_range, y_range, x_length, y_length, ...)
    },
    
    create_axes = function() {
      super$create_axes() # Create X and Y
      
      # Create Z Axis
      # Direction usually standard +Z is UP in 3D plots, but in Manim +Z is usually towards camera?
      # In ManimR backend, Z is Z.
      # We draw Z axis from z_min to z_max
      
      p_z_min <- self$c2p(0, 0, self$z_range[1])
      p_z_max <- self$c2p(0, 0, self$z_range[2])
      
      self$add_line(p_z_min, p_z_max)
    },
    
    c2p = function(x, y, z=0) {
      # Use 2D mapping for X/Y
      xy <- super$c2p(x, y, 0) # Returns (x_s, y_s, 0)
      
      # Z mapping
      z_min <- self$z_range[1]
      z_max <- self$z_range[2]
      z_norm <- (z - z_min) / (z_max - z_min) 
      z_scene <- (z_norm - 0.5) * self$z_length
      
      c(xy[1], xy[2], z_scene)
    }
  )
)
