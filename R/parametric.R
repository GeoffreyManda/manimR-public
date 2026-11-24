#' @title ParametricFunction
#' @description A function mapping t to (x,y,z).
#' @export
ParametricFunction <- R6::R6Class("ParametricFunction",
  inherit = VMobject,
  public = list(
    t_func = NULL,
    t_range = NULL,
    
    initialize = function(t_func, t_range = c(0, 1, 0.01), color = "#FFFFFF", ...) {
      super$initialize(...)
      self$t_func <- t_func
      self$t_range <- t_range
      self$color <- color
      self$generate_points()
    },
    
    generate_points = function() {
      ts <- seq(self$t_range[1], self$t_range[2], by = self$t_range[3])
      
      # Evaluate function
      # t_func should return a numeric vector of length 3 or list
      pts <- matrix(numeric(0), ncol=3)
      for (t in ts) {
        pt <- self$t_func(t)
        if (length(pt) == 2) pt <- c(pt, 0)
        pts <- rbind(pts, pt)
      }
      
      # Set points directly as a polyline
      self$set_points(pts)
    }
  )
)

#' @title FunctionGraph
#' @description Graph of y = f(x).
#' @export
FunctionGraph <- R6::R6Class("FunctionGraph",
  inherit = ParametricFunction,
  public = list(
    initialize = function(f, x_range = c(-5, 5, 0.1), ...) {
      
      # Wrapper to convert x to (x, f(x), 0)
      para_func <- function(t) {
        c(t, f(t), 0)
      }
      
      super$initialize(t_func = para_func, t_range = x_range, ...)
    }
  )
)
