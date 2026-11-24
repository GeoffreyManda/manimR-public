#' @title Movement Animations
#' @description Animations that move mobjects along paths or through homotopies.
#' @name animation_movement
NULL

#' MoveAlongPath R6 Class
#'
#' @description Moves a mobject along the path of another VMobject.
#' @export
MoveAlongPath <- R6::R6Class(
  "MoveAlongPath",
  inherit = Animation,
  public = list(
    #' @field path The VMobject whose Bezier path to follow
    path = NULL,

    #' @description Create a MoveAlongPath animation
    #' @param mobject Mobject to move
    #' @param path VMobject defining the path
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, path, run_time = 2,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$path <- path
    },

    #' @description Move along the path
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      if (is.null(self$path) || !inherits(self$path, "VMobject")) {
        return(invisible(self))
      }

      # Get point at proportion along the path
      pt <- self$path$point_from_proportion(alpha)
      self$mobject$position <- pt

      invisible(self)
    }
  )
)

#' Homotopy R6 Class
#'
#' @description Applies a continuous deformation to a VMobject's points.
#'   The homotopy function takes (x, y, z, t) and returns c(x', y', z').
#' @export
Homotopy <- R6::R6Class(
  "Homotopy",
  inherit = Animation,
  public = list(
    #' @field homotopy_func Function(x, y, z, t) -> c(x', y', z')
    homotopy_func = NULL,

    #' @description Create a Homotopy animation
    #' @param mobject VMobject to deform
    #' @param homotopy Function(x, y, z, t) -> c(x', y', z')
    #' @param run_time Duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, homotopy, run_time = 3,
                          rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$homotopy_func <- homotopy
    },

    #' @description Apply homotopy to all points at current t
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      mob <- self$mobject
      if (!inherits(mob, "VMobject") || is.null(self$starting_mobject)) {
        return(invisible(self))
      }

      if (!self$starting_mobject$has_points()) return(invisible(self))

      # Apply homotopy to each point from the starting state
      start_pts <- self$starting_mobject$points
      new_pts <- start_pts

      for (i in seq_len(nrow(start_pts))) {
        pt <- start_pts[i, ]
        new_pts[i, ] <- self$homotopy_func(pt[1], pt[2], pt[3], alpha)
      }

      mob$points <- new_pts
      invisible(self)
    }
  )
)

#' PhaseFlow R6 Class
#'
#' @description Moves a mobject according to a velocity field over time.
#' @export
PhaseFlow <- R6::R6Class(
  "PhaseFlow",
  inherit = Animation,
  public = list(
    #' @field velocity_func Function(point) -> velocity vector
    velocity_func = NULL,
    #' @field virtual_time Total virtual time to simulate
    virtual_time = 1,

    #' @description Create a PhaseFlow animation
    #' @param mobject Mobject to move
    #' @param velocity_func Function(point) -> c(vx, vy, vz) velocity
    #' @param virtual_time How much "physics time" to simulate
    #' @param run_time Animation duration
    #' @param rate_func Easing
    #' @param ... Additional args
    initialize = function(mobject, velocity_func, virtual_time = 1,
                          run_time = 3, rate_func = "linear", ...) {
      super$initialize(mobject = mobject, run_time = run_time,
                       rate_func = rate_func, ...)
      self$velocity_func <- velocity_func
      self$virtual_time <- virtual_time
    },

    #' @description Begin: save starting state
    begin = function() {
      private$start_position <- self$mobject$position
      super$begin()
    },

    #' @description Euler integration of velocity field
    #' @param alpha Eased progress
    interpolate_mobject = function(alpha) {
      # Simple Euler integration from start
      dt <- 0.01
      pos <- private$start_position
      target_time <- alpha * self$virtual_time
      t <- 0

      while (t < target_time) {
        step <- min(dt, target_time - t)
        vel <- self$velocity_func(pos)
        if (length(vel) == 2) vel <- c(vel, 0)
        pos <- pos + vel * step
        t <- t + step
      }

      self$mobject$position <- pos
      invisible(self)
    }
  ),

  private = list(
    start_position = NULL
  )
)
