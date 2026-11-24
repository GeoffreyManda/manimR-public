#' @title Group and VGroup Classes
#' @description
#' Containers for composing multiple MObjects. Group holds any MObjects,
#' VGroup holds VMobjects and inherits VMobject methods.
#' @name group
NULL

#' Group R6 Class
#'
#' @description Container for mixed MObjects (not necessarily VMobjects).
#' @export
Group <- R6::R6Class(
  "Group",
  inherit = MObject,

  public = list(
    #' @field submobjects List of child MObjects
    submobjects = NULL,

    #' @description Create a new Group
    #' @param ... MObjects to include
    initialize = function(...) {
      super$initialize(type = "group")
      self$submobjects <- list(...)
    },

    #' @description Add objects to the group
    #' @param ... MObjects to add
    add = function(...) {
      objs <- list(...)
      for (obj in objs) {
        if (inherits(obj, "MObject")) {
          self$submobjects <- c(self$submobjects, list(obj))
        }
      }
      invisible(self)
    },

    #' @description Remove an object from the group
    #' @param obj MObject to remove
    remove = function(obj) {
      self$submobjects <- Filter(function(s) !identical(s$id, obj$id),
                                  self$submobjects)
      invisible(self)
    },

    #' @description Get number of objects
    length = function() length(self$submobjects),

    #' @description Access submobject by index
    #' @param i Index (1-based)
    `[[` = function(i) self$submobjects[[i]],

    #' @description Set color on all children
    #' @param color New color
    set_color = function(color) {
      color <- validate_color(color)
      self$color <- color
      for (sub in self$submobjects) sub$set_color(color)
      invisible(self)
    },

    #' @description Set opacity on all children
    #' @param opacity New opacity
    set_opacity = function(opacity) {
      self$opacity <- clamp(opacity)
      for (sub in self$submobjects) sub$set_opacity(opacity)
      invisible(self)
    },

    #' @description Move all children
    #' @param point Target position
    move_to = function(point) {
      point <- validate_position(point)
      shift_vec <- point - self$get_center()
      self$shift(shift_vec)
      invisible(self)
    },

    #' @description Shift all children
    #' @param direction Shift vector
    shift = function(direction) {
      direction <- validate_position(direction)
      self$position <- self$position + direction
      for (sub in self$submobjects) sub$shift(direction)
      invisible(self)
    },

    #' @description Get center of all children's bounding boxes
    get_center = function() {
      if (length(self$submobjects) == 0) return(self$position)
      centers <- do.call(rbind, lapply(self$submobjects, function(s) s$get_center()))
      colMeans(centers)
    },

    #' @description Get bounding box enclosing all children
    get_bounding_box = function() {
      if (length(self$submobjects) == 0) return(super$get_bounding_box())
      bbs <- lapply(self$submobjects, function(s) s$get_bounding_box())
      mins <- do.call(rbind, lapply(bbs, `[[`, "min"))
      maxs <- do.call(rbind, lapply(bbs, `[[`, "max"))
      list(min = apply(mins, 2, min), max = apply(maxs, 2, max))
    },

    #' @description Arrange children in a row or column
    #' @param direction Arrangement direction (RIGHT for row, DOWN for column)
    #' @param buff Space between objects
    #' @param center Whether to center the arrangement at the group's position
    arrange = function(direction = RIGHT, buff = MED_SMALL_BUFF, center = TRUE) {
      if (length(self$submobjects) < 2) return(invisible(self))

      direction <- validate_position(direction)
      dir_norm <- normalize(direction)

      # Place objects sequentially along direction
      current_pos <- self$submobjects[[1]]$get_center()
      for (i in 2:length(self$submobjects)) {
        prev <- self$submobjects[[i - 1]]
        curr <- self$submobjects[[i]]

        prev_bb <- prev$get_bounding_box()
        curr_bb <- curr$get_bounding_box()

        # Distance from prev's far edge to curr's near edge
        if (abs(dir_norm[1]) > 0.5) {
          edge_dist <- if (dir_norm[1] > 0) {
            prev_bb$max[1] - curr_bb$min[1]
          } else {
            prev_bb$min[1] - curr_bb$max[1]
          }
        } else {
          edge_dist <- if (dir_norm[2] > 0) {
            prev_bb$max[2] - curr_bb$min[2]
          } else {
            prev_bb$min[2] - curr_bb$max[2]
          }
        }

        shift_amount <- edge_dist + buff
        curr$shift(dir_norm * shift_amount)
      }

      if (center) {
        overall_center <- self$get_center()
        shift_to_origin <- self$position - overall_center
        for (sub in self$submobjects) sub$shift(shift_to_origin)
      }

      invisible(self)
    },

    #' @description Arrange children in a grid
    #' @param n_cols Number of columns
    #' @param buff Space between objects
    arrange_in_grid = function(n_cols = 3, buff = MED_SMALL_BUFF) {
      n <- length(self$submobjects)
      if (n == 0) return(invisible(self))

      n_rows <- ceiling(n / n_cols)

      for (i in seq_len(n)) {
        row <- (i - 1) %/% n_cols
        col <- (i - 1) %% n_cols
        x <- (col - (n_cols - 1) / 2) * (1 + buff)
        y <- ((n_rows - 1) / 2 - row) * (1 + buff)
        self$submobjects[[i]]$move_to(c(x, y, 0) + self$position)
      }

      invisible(self)
    }
  )
)

#' VGroup R6 Class
#'
#' @description Container specifically for VMobjects. Inherits from VMobject
#'   so it can be treated as a single drawable entity.
#' @export
VGroup <- R6::R6Class(
  "VGroup",
  inherit = VMobject,

  public = list(
    #' @description Create a new VGroup
    #' @param ... VMobjects to include
    initialize = function(...) {
      super$initialize()
      self$type <- "vgroup"
      objs <- list(...)
      for (obj in objs) {
        if (inherits(obj, "MObject")) {
          self$submobjects <- c(self$submobjects, list(obj))
        }
      }
    },

    #' @description Add VMobjects
    #' @param ... VMobjects to add
    add = function(...) {
      objs <- list(...)
      for (obj in objs) {
        if (inherits(obj, "MObject")) {
          self$submobjects <- c(self$submobjects, list(obj))
        }
      }
      invisible(self)
    },

    #' @description Remove a VMobject
    #' @param obj VMobject to remove
    remove = function(obj) {
      self$submobjects <- Filter(function(s) !identical(s$id, obj$id),
                                  self$submobjects)
      invisible(self)
    },

    #' @description Get number of objects
    length = function() length(self$submobjects),

    #' @description Arrange children in a line
    #' @param direction Direction
    #' @param buff Buffer between objects
    #' @param center Center the arrangement
    arrange = function(direction = RIGHT, buff = MED_SMALL_BUFF, center = TRUE) {
      if (length(self$submobjects) < 2) return(invisible(self))

      direction <- validate_position(direction)
      dir_norm <- normalize(direction)

      for (i in 2:length(self$submobjects)) {
        prev <- self$submobjects[[i - 1]]
        curr <- self$submobjects[[i]]

        prev_bb <- prev$get_bounding_box()
        curr_bb <- curr$get_bounding_box()

        if (abs(dir_norm[1]) > 0.5) {
          edge_dist <- if (dir_norm[1] > 0) {
            prev_bb$max[1] - curr_bb$min[1]
          } else {
            prev_bb$min[1] - curr_bb$max[1]
          }
        } else {
          edge_dist <- if (dir_norm[2] > 0) {
            prev_bb$max[2] - curr_bb$min[2]
          } else {
            prev_bb$min[2] - curr_bb$max[2]
          }
        }

        curr$shift(dir_norm * (edge_dist + buff))
      }

      if (center) {
        overall_center <- self$get_center()
        shift_to_origin <- self$position - overall_center
        for (sub in self$submobjects) sub$shift(shift_to_origin)
      }

      invisible(self)
    },

    #' @description Arrange in grid
    #' @param n_cols Number of columns
    #' @param buff Buffer
    arrange_in_grid = function(n_cols = 3, buff = MED_SMALL_BUFF) {
      n <- length(self$submobjects)
      if (n == 0) return(invisible(self))
      n_rows <- ceiling(n / n_cols)
      for (i in seq_len(n)) {
        row <- (i - 1) %/% n_cols
        col <- (i - 1) %% n_cols
        x <- (col - (n_cols - 1) / 2) * (1 + buff)
        y <- ((n_rows - 1) / 2 - row) * (1 + buff)
        self$submobjects[[i]]$move_to(c(x, y, 0) + self$position)
      }
      invisible(self)
    }
  )
)
