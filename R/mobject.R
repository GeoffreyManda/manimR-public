#' @title MObject Base Class
#' @description
#' The fundamental base class for all mathematical objects in manimR.
#' Every visual element in a scene inherits from MObject.
#' @name mobject
#' @keywords internal
NULL

#' MObject R6 Class
#'
#' @description
#' Base class for all visual objects in manimR. Provides position,
#' color, opacity, visibility, and spatial query methods that all
#' objects inherit.
#'
#' @export
#' @importFrom R6 R6Class
MObject <- R6::R6Class(
  "MObject",

  public = list(
    #' @field id Unique identifier
    id = NULL,

    #' @field type Object type string
    type = "mobject",

    #' @field position Position vector (x, y, z)
    position = c(0, 0, 0),

    #' @field rotation Rotation in radians
    rotation = 0,

    #' @field scale_factor Scale factor
    scale_factor = 1,

    #' @field color Primary color (hex string)
    color = "#FFFFFF",

    #' @field opacity Opacity (0 to 1)
    opacity = 1,

    #' @field visible Whether the object is visible
    visible = FALSE,

    #' @field z_index Drawing order (higher = on top)
    z_index = 0,

    #' @field data Additional object-specific data
    data = NULL,

    #' @field updaters List of updater functions called each frame
    updaters = NULL,

    #' @field target Target state for MoveToTarget animation
    target = NULL,

    #' @description Create a new MObject
    #' @param type Object type
    #' @param position Position vector
    #' @param color Color
    #' @param opacity Opacity
    initialize = function(type = "mobject",
                          position = c(0, 0, 0),
                          color = "#FFFFFF",
                          opacity = 1) {
      self$id <- generate_id("mob")
      self$type <- type
      self$position <- validate_position(position)
      self$color <- validate_color(color)
      self$opacity <- clamp(opacity)
      self$data <- list()
      self$updaters <- list()
    },

    # ---- Copying ----

    #' @description Create a deep copy with a new ID
    copy = function() {
      obj <- self$clone(deep = TRUE)
      obj$id <- generate_id("mob")
      obj
    },

    # ---- Spatial Queries ----

    #' @description Get center position
    get_center = function() {
      self$position
    },

    #' @description Get bounding box as list(min, max)
    get_bounding_box = function() {
      list(
        min = self$position - c(0.5, 0.5, 0) * self$scale_factor,
        max = self$position + c(0.5, 0.5, 0) * self$scale_factor
      )
    },

    #' @description Get left edge point
    get_left = function() {
      bb <- self$get_bounding_box()
      c(bb$min[1], self$position[2], self$position[3])
    },

    #' @description Get right edge point
    get_right = function() {
      bb <- self$get_bounding_box()
      c(bb$max[1], self$position[2], self$position[3])
    },

    #' @description Get top edge point
    get_top = function() {
      bb <- self$get_bounding_box()
      c(self$position[1], bb$max[2], self$position[3])
    },

    #' @description Get bottom edge point
    get_bottom = function() {
      bb <- self$get_bounding_box()
      c(self$position[1], bb$min[2], self$position[3])
    },

    #' @description Get corner point
    #' @param direction Direction vector (e.g., UR, DL)
    get_corner = function(direction) {
      bb <- self$get_bounding_box()
      x <- if (direction[1] > 0) bb$max[1] else if (direction[1] < 0) bb$min[1] else self$position[1]
      y <- if (direction[2] > 0) bb$max[2] else if (direction[2] < 0) bb$min[2] else self$position[2]
      c(x, y, self$position[3])
    },

    #' @description Get width of object
    get_width = function() {
      bb <- self$get_bounding_box()
      bb$max[1] - bb$min[1]
    },

    #' @description Get height of object
    get_height = function() {
      bb <- self$get_bounding_box()
      bb$max[2] - bb$min[2]
    },

    # ---- Immediate Transforms (Modify in-place, return self) ----

    #' @description Move to an absolute position
    #' @param point Target position (2D or 3D)
    move_to = function(point) {
      self$position <- validate_position(point)
      invisible(self)
    },

    #' @description Shift by a relative vector
    #' @param direction Direction vector to shift by
    shift = function(direction) {
      direction <- validate_position(direction)
      self$position <- self$position + direction
      invisible(self)
    },

    #' @description Rotate by an angle
    #' @param angle Angle in radians
    #' @param about_point Point to rotate around (default: center)
    rotate = function(angle, about_point = NULL) {
      self$rotation <- self$rotation + angle
      invisible(self)
    },

    #' @description Scale by a factor
    #' @param factor Scale multiplier
    #' @param about_point Point to scale around (default: center)
    scale = function(factor, about_point = NULL) {
      self$scale_factor <- self$scale_factor * factor
      invisible(self)
    },

    #' @description Position next to another object
    #' @param other Another MObject or a point
    #' @param direction Direction (e.g., RIGHT, UP)
    #' @param buff Buffer distance
    next_to = function(other, direction = RIGHT, buff = MED_SMALL_BUFF) {
      if (inherits(other, "MObject")) {
        target_point <- other$get_center()
      } else {
        target_point <- validate_position(other)
      }

      direction <- validate_position(direction)
      dir_norm <- normalize(direction)

      # Position self so its edge touches other's edge + buff
      self$position <- target_point + dir_norm * (0.5 + buff)
      invisible(self)
    },

    #' @description Move to the edge of the frame
    #' @param direction Direction to the edge (e.g., LEFT, UP)
    #' @param buff Buffer from edge
    to_edge = function(direction = LEFT, buff = MED_LARGE_BUFF) {
      direction <- validate_position(direction)
      edge_point <- c(
        if (abs(direction[1]) > 0.5) sign(direction[1]) * (FRAME_X_RADIUS - buff) else self$position[1],
        if (abs(direction[2]) > 0.5) sign(direction[2]) * (FRAME_Y_RADIUS - buff) else self$position[2],
        self$position[3]
      )
      self$position <- edge_point
      invisible(self)
    },

    #' @description Move to a corner of the frame
    #' @param direction Corner direction (e.g., UL, DR)
    #' @param buff Buffer from corner
    to_corner = function(direction = UL, buff = MED_LARGE_BUFF) {
      direction <- validate_position(direction)
      self$position <- c(
        sign(direction[1]) * (FRAME_X_RADIUS - buff),
        sign(direction[2]) * (FRAME_Y_RADIUS - buff),
        self$position[3]
      )
      invisible(self)
    },

    #' @description Align to another object along an edge
    #' @param other Another MObject
    #' @param direction Edge direction (e.g., UP aligns tops)
    align_to = function(other, direction) {
      direction <- validate_position(direction)
      target <- if (inherits(other, "MObject")) other$get_center() else validate_position(other)

      if (abs(direction[1]) > 0.5) self$position[1] <- target[1]
      if (abs(direction[2]) > 0.5) self$position[2] <- target[2]
      if (abs(direction[3]) > 0.5) self$position[3] <- target[3]
      invisible(self)
    },

    # ---- Color & Style ----

    #' @description Set the primary color
    #' @param color New color
    set_color = function(color) {
      self$color <- validate_color(color)
      invisible(self)
    },

    #' @description Set opacity
    #' @param opacity New opacity (0-1)
    set_opacity = function(opacity) {
      self$opacity <- clamp(opacity)
      invisible(self)
    },

    #' @description Match the color of another object
    #' @param other Another MObject
    match_color = function(other) {
      self$color <- other$color
      invisible(self)
    },

    # ---- Updaters ----

    #' @description Add an updater function
    #' @param func Function taking (self, dt) called each frame
    add_updater = function(func) {
      self$updaters <- c(self$updaters, list(func))
      invisible(self)
    },

    #' @description Remove all updaters
    clear_updaters = function() {
      self$updaters <- list()
      invisible(self)
    },

    # ---- Target (for MoveToTarget animation) ----

    #' @description Generate a target copy for animation
    generate_target = function() {
      self$target <- self$copy()
      self$target
    },

    # ---- Print ----

    #' @description Print object summary
    print = function() {
      cli::cli_text("{.cls {class(self)[1]}} [{self$id}]")
      cli::cli_ul()
      cli::cli_li("Position: ({paste(round(self$position, 2), collapse = ', ')})")
      cli::cli_li("Color: {self$color}")
      cli::cli_li("Visible: {self$visible}")
      cli::cli_li("Scale: {self$scale_factor}")
      cli::cli_end()
      invisible(self)
    }
  )
)

#' TextObject R6 Class
#'
#' @description Represents plain text in a scene.
#' @export
TextObject <- R6::R6Class(
  "TextObject",
  inherit = MObject,

  public = list(
    #' @field text The text content
    text = "",
    #' @field font Font family
    font = "sans",
    #' @field size Font size in points
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
      super$initialize(type = "text", position = position,
                       color = color, opacity = opacity)
      self$text <- text
      self$font <- font
      self$size <- size
      self$weight <- weight
    },

    #' @description Get the text content
    get_text = function() self$text
  )
)

#' ImageMobject R6 Class
#'
#' @description Represents a raster image in a scene.
#' @export
ImageMobject <- R6::R6Class(
  "ImageMobject",
  inherit = MObject,

  public = list(
    #' @field image_path Path to the image file
    image_path = NULL,
    #' @field image_width Display width
    image_width = NULL,
    #' @field image_height Display height
    image_height = NULL,

    #' @description Create a new ImageMobject
    #' @param path Path to image file
    #' @param position Position
    #' @param width Display width
    #' @param height Display height
    #' @param opacity Opacity
    initialize = function(path,
                          position = c(0, 0, 0),
                          width = NULL,
                          height = NULL,
                          opacity = 1) {
      super$initialize(type = "image", position = position, opacity = opacity)
      if (!file.exists(path)) cli::cli_abort("Image file not found: {.path {path}}")
      self$image_path <- normalizePath(path)
      self$image_width <- width
      self$image_height <- height
      self$data$path <- self$image_path
      self$data$width <- width
      self$data$height <- height
    }
  )
)
