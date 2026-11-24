#' @title VMobject - Vectorized Mathematical Object
#' @description
#' The base class for all vector-graphics-based objects. VMobjects store
#' their shape as cubic Bezier curves (4 control points per curve segment)
#' and support fill, stroke, submobjects, and smooth morphing.
#' @name vmobject
#' @keywords internal
NULL

#' VMobject R6 Class
#'
#' @description
#' Base class for all vector mobjects. Stores geometry as cubic Bezier
#' control points in a (N x 3) matrix where every 4 rows define one curve.
#' Supports hierarchical submobjects for grouping.
#'
#' @export
#' @importFrom R6 R6Class
VMobject <- R6::R6Class(
  "VMobject",
  inherit = MObject,

  public = list(
    #' @field points Matrix of Bezier control points (N x 3, 4 per curve)
    points = NULL,

    #' @field stroke_width Stroke width in pixels
    stroke_width = DEFAULT_STROKE_WIDTH,

    #' @field stroke_color Stroke color (NULL uses self$color)
    stroke_color = NULL,

    #' @field stroke_opacity Stroke opacity (0-1)
    stroke_opacity = 1,

    #' @field fill_color Fill color (NULL = no fill)
    fill_color = NULL,

    #' @field fill_opacity Fill opacity (0-1)
    fill_opacity = 0,

    #' @field submobjects List of child VMobjects
    submobjects = NULL,

    #' @description Create a new VMobject
    #' @param position Position
    #' @param color Color
    #' @param opacity Opacity
    #' @param stroke_width Stroke width
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param ... Additional args passed to MObject
    initialize = function(position = c(0, 0, 0),
                          color = "#FFFFFF",
                          opacity = 1,
                          stroke_width = DEFAULT_STROKE_WIDTH,
                          fill_color = NULL,
                          fill_opacity = 0,
                          ...) {
      super$initialize(type = "vmobject", position = position,
                       color = color, opacity = opacity)
      self$stroke_width <- stroke_width
      self$fill_color <- if (!is.null(fill_color)) validate_color(fill_color) else NULL
      self$fill_opacity <- fill_opacity
      self$points <- matrix(numeric(0), ncol = 3)
      self$submobjects <- list()
    },

    # ---- Point Management ----

    #' @description Set points from a matrix
    #' @param points Matrix (N x 2 or N x 3)
    set_points = function(points) {
      if (is.null(points) || length(points) == 0) {
        self$points <- matrix(numeric(0), ncol = 3)
        return(invisible(self))
      }
      if (is.vector(points)) points <- matrix(points, ncol = length(points))
      if (ncol(points) == 2) points <- cbind(points, 0)
      if (ncol(points) != 3) stop("Points must have 2 or 3 columns")
      self$points <- points
      invisible(self)
    },

    #' @description Get points matrix
    get_points = function() self$points,

    #' @description Check if this VMobject has any points
    has_points = function() {
      !is.null(self$points) && is.matrix(self$points) && nrow(self$points) >= 4
    },

    #' @description Clear all points
    clear_points = function() {
      self$points <- matrix(numeric(0), ncol = 3)
      invisible(self)
    },

    #' @description Get number of curves (4 points each)
    get_num_curves = function() {
      if (!self$has_points()) return(0)
      nrow(self$points) %/% 4
    },

    #' @description Add a straight line as a cubic Bezier
    #' @param start Start point (x, y, z)
    #' @param end End point (x, y, z)
    add_line = function(start, end) {
      p0 <- as.numeric(start)
      p3 <- as.numeric(end)
      if (length(p0) == 2) p0 <- c(p0, 0)
      if (length(p3) == 2) p3 <- c(p3, 0)
      p1 <- p0 + (p3 - p0) / 3
      p2 <- p0 + 2 * (p3 - p0) / 3
      self$points <- rbind(self$points, rbind(p0, p1, p2, p3))
      invisible(self)
    },

    #' @description Add a cubic Bezier curve
    #' @param p0,p1,p2,p3 Control points
    add_cubic_bezier = function(p0, p1, p2, p3) {
      p0 <- as.numeric(p0); if (length(p0) == 2) p0 <- c(p0, 0)
      p1 <- as.numeric(p1); if (length(p1) == 2) p1 <- c(p1, 0)
      p2 <- as.numeric(p2); if (length(p2) == 2) p2 <- c(p2, 0)
      p3 <- as.numeric(p3); if (length(p3) == 2) p3 <- c(p3, 0)
      self$points <- rbind(self$points, rbind(p0, p1, p2, p3))
      invisible(self)
    },

    #' @description Append another VMobject's points
    #' @param other A VMobject
    append_points = function(other) {
      if (other$has_points()) {
        self$points <- rbind(self$points, other$get_points())
      }
      invisible(self)
    },

    # ---- Spatial Overrides ----

    #' @description Get bounding box computed from actual points
    get_bounding_box = function() {
      all_pts <- self$get_all_points()
      if (is.null(all_pts) || nrow(all_pts) == 0) {
        return(super$get_bounding_box())
      }
      valid <- !is.na(all_pts[, 1])
      all_pts <- all_pts[valid, , drop = FALSE]
      if (nrow(all_pts) == 0) return(super$get_bounding_box())

      list(
        min = apply(all_pts, 2, min) + self$position,
        max = apply(all_pts, 2, max) + self$position
      )
    },

    #' @description Get center computed from bounding box
    get_center = function() {
      bb <- self$get_bounding_box()
      (bb$min + bb$max) / 2
    },

    #' @description Get all points including from submobjects
    get_all_points = function() {
      pts <- self$points
      for (sub in self$submobjects) {
        if (inherits(sub, "VMobject") && sub$has_points()) {
          sub_pts <- sub$get_all_points()
          if (!is.null(sub_pts) && nrow(sub_pts) > 0) {
            sub_pts <- sub_pts + matrix(rep(sub$position, nrow(sub_pts)),
                                         ncol = 3, byrow = TRUE)
            pts <- rbind(pts, sub_pts)
          }
        }
      }
      pts
    },

    # ---- Path Queries ----

    #' @description Get the start point of the first curve
    get_start = function() {
      if (!self$has_points()) return(self$position)
      self$points[1, ] + self$position
    },

    #' @description Get the end point of the last curve
    get_end = function() {
      if (!self$has_points()) return(self$position)
      self$points[nrow(self$points), ] + self$position
    },

    #' @description Get approximate arc length
    get_arc_length = function() {
      path_length(self$points)
    },

    #' @description Get point at proportion along the path
    #' @param proportion Value in \[0,1\]
    point_from_proportion = function(proportion) {
      pt <- point_from_proportion(self$points, proportion)
      pt + self$position
    },

    #' @description Get interpolated points for drawing
    #' @param precision Samples per curve
    get_draw_points = function(precision = 10) {
      if (!self$has_points()) return(matrix(numeric(0), ncol = 3))
      sample_bezier(self$points, precision)
    },

    # ---- Style Methods ----

    #' @description Set fill color and opacity
    #' @param color Fill color (NULL to clear)
    #' @param opacity Fill opacity
    set_fill = function(color = NULL, opacity = NULL) {
      if (!is.null(color)) self$fill_color <- validate_color(color)
      if (!is.null(opacity)) self$fill_opacity <- clamp(opacity)
      for (sub in self$submobjects) {
        if (inherits(sub, "VMobject")) sub$set_fill(color, opacity)
      }
      invisible(self)
    },

    #' @description Set stroke color, width, and opacity
    #' @param color Stroke color
    #' @param width Stroke width
    #' @param opacity Stroke opacity
    set_stroke = function(color = NULL, width = NULL, opacity = NULL) {
      if (!is.null(color)) {
        self$stroke_color <- validate_color(color)
        self$color <- self$stroke_color
      }
      if (!is.null(width)) self$stroke_width <- width
      if (!is.null(opacity)) self$stroke_opacity <- clamp(opacity)
      for (sub in self$submobjects) {
        if (inherits(sub, "VMobject")) sub$set_stroke(color, width, opacity)
      }
      invisible(self)
    },

    #' @description Set both fill and stroke style
    #' @param fill_color Fill color
    #' @param fill_opacity Fill opacity
    #' @param stroke_color Stroke color
    #' @param stroke_width Stroke width
    #' @param stroke_opacity Stroke opacity
    set_style = function(fill_color = NULL, fill_opacity = NULL,
                         stroke_color = NULL, stroke_width = NULL,
                         stroke_opacity = NULL) {
      self$set_fill(fill_color, fill_opacity)
      self$set_stroke(stroke_color, stroke_width, stroke_opacity)
      invisible(self)
    },

    #' @description Override set_color to also set stroke
    #' @param color New color
    set_color = function(color) {
      color <- validate_color(color)
      self$color <- color
      self$stroke_color <- color
      for (sub in self$submobjects) {
        if (inherits(sub, "MObject")) sub$set_color(color)
      }
      invisible(self)
    },

    # ---- Submobject Management ----

    #' @description Add child submobjects
    #' @param ... VMobjects to add as children
    add = function(...) {
      objs <- list(...)
      for (obj in objs) {
        if (inherits(obj, "MObject")) {
          self$submobjects <- c(self$submobjects, list(obj))
        }
      }
      invisible(self)
    },

    #' @description Add a single submobject (alias for add)
    #' @param obj MObject to add as child
    add_submobject = function(obj) {
      self$add(obj)
    },

    #' @description Remove a child submobject
    #' @param obj The VMobject to remove
    remove = function(obj) {
      self$submobjects <- Filter(function(s) !identical(s$id, obj$id),
                                  self$submobjects)
      invisible(self)
    },

    #' @description Get a submobject by index
    #' @param i Index (1-based)
    get_submobject = function(i) {
      if (i < 1 || i > length(self$submobjects)) return(NULL)
      self$submobjects[[i]]
    },

    # ---- Transform Overrides ----

    #' @description Move to position
    #' @param point Target position
    move_to = function(point) {
      self$position <- validate_position(point)
      invisible(self)
    },

    #' @description Shift by direction
    #' @param direction Shift vector
    shift = function(direction) {
      direction <- validate_position(direction)
      self$position <- self$position + direction
      invisible(self)
    },

    #' @description Rotate (applies to points directly)
    #' @param angle Angle in radians
    #' @param about_point Center of rotation
    #' @param axis Rotation axis for 3D
    rotate = function(angle, about_point = NULL, axis = c(0, 0, 1)) {
      self$rotation <- self$rotation + angle
      if (self$has_points()) {
        self$points <- rotate_points_3d(self$points, angle, axis)
      }
      for (sub in self$submobjects) {
        if (inherits(sub, "VMobject")) sub$rotate(angle, about_point, axis)
      }
      invisible(self)
    },

    #' @description Scale (applies to points directly)
    #' @param factor Scale factor
    #' @param about_point Center of scaling
    scale = function(factor, about_point = NULL) {
      self$scale_factor <- self$scale_factor * factor
      if (self$has_points()) {
        self$points <- self$points * factor
      }
      for (sub in self$submobjects) {
        if (inherits(sub, "VMobject")) sub$scale(factor, about_point)
      }
      invisible(self)
    },

    # ---- Print ----

    #' @description Print VMobject summary
    print = function() {
      cli::cli_text("{.cls {class(self)[1]}} [{self$id}]")
      cli::cli_ul()
      cli::cli_li("Position: ({paste(round(self$position, 2), collapse = ', ')})")
      cli::cli_li("Color: {self$color}")
      n_curves <- self$get_num_curves()
      cli::cli_li("Curves: {n_curves}")
      cli::cli_li("Submobjects: {length(self$submobjects)}")
      if (!is.null(self$fill_color)) {
        cli::cli_li("Fill: {self$fill_color} ({self$fill_opacity})")
      }
      cli::cli_end()
      invisible(self)
    }
  )
)
