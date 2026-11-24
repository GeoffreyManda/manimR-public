#' @title Camera System
#' @description
#' Camera classes that control the viewport: position, zoom, rotation.
#' Used by Scene to determine which portion of the mathematical coordinate
#' space is visible and how it maps to pixels.
#' @name camera
NULL

#' Camera R6 Class
#'
#' @description
#' A 2D camera with position, zoom (frame width/height), and rotation.
#' The camera defines the coordinate-to-pixel mapping used by the renderer.
#'
#' @export
Camera <- R6::R6Class(
  "Camera",
  public = list(
    #' @field frame_center Center of the camera frame in world coordinates
    frame_center = NULL,
    #' @field frame_width Width of the visible frame in world units
    frame_width = FRAME_WIDTH,
    #' @field frame_height Height of the visible frame in world units
    frame_height = FRAME_HEIGHT,
    #' @field rotation Camera rotation in radians
    rotation = 0,
    #' @field background_color Background color
    background_color = "#000000",
    #' @field pixel_width Output width in pixels
    pixel_width = 1920,
    #' @field pixel_height Output height in pixels
    pixel_height = 1080,

    #' @description Create a new Camera
    #' @param frame_center Center position c(x, y, z)
    #' @param frame_width Visible width in world units
    #' @param frame_height Visible height in world units
    #' @param background_color Background color
    #' @param pixel_width Output pixel width
    #' @param pixel_height Output pixel height
    initialize = function(frame_center = ORIGIN,
                          frame_width = FRAME_WIDTH,
                          frame_height = FRAME_HEIGHT,
                          background_color = "#000000",
                          pixel_width = 1920,
                          pixel_height = 1080) {
      self$frame_center <- validate_position(frame_center)
      self$frame_width <- frame_width
      self$frame_height <- frame_height
      self$background_color <- validate_color(background_color)
      self$pixel_width <- pixel_width
      self$pixel_height <- pixel_height
    },

    #' @description Get the zoom level (1.0 = default frame width)
    get_zoom = function() {
      FRAME_WIDTH / self$frame_width
    },

    #' @description Set zoom level
    #' @param zoom Zoom factor (> 1 = zoomed in, < 1 = zoomed out)
    set_zoom = function(zoom) {
      self$frame_width <- FRAME_WIDTH / zoom
      self$frame_height <- FRAME_HEIGHT / zoom
      invisible(self)
    },

    #' @description Move the camera to a position
    #' @param point Target position
    move_to = function(point) {
      self$frame_center <- validate_position(point)
      invisible(self)
    },

    #' @description Shift the camera by a vector
    #' @param direction Shift vector
    shift = function(direction) {
      direction <- validate_position(direction)
      self$frame_center <- self$frame_center + direction
      invisible(self)
    },

    #' @description Convert world coordinates to pixel coordinates
    #' @param point World position c(x, y, z)
    #' @return Pixel position c(px, py)
    world_to_pixel = function(point) {
      point <- validate_position(point)
      # Offset by camera center
      rel <- point - self$frame_center

      # Apply camera rotation
      if (abs(self$rotation) > 1e-10) {
        ca <- cos(-self$rotation)
        sa <- sin(-self$rotation)
        rx <- rel[1] * ca - rel[2] * sa
        ry <- rel[1] * sa + rel[2] * ca
        rel[1] <- rx
        rel[2] <- ry
      }

      # Map to pixel space
      px <- (rel[1] / self$frame_width + 0.5) * self$pixel_width
      py <- (0.5 - rel[2] / self$frame_height) * self$pixel_height
      c(px, py)
    },

    #' @description Convert pixel coordinates to world coordinates
    #' @param px,py Pixel position
    #' @return World position c(x, y, z)
    pixel_to_world = function(px, py) {
      # Map from pixel to normalised
      nx <- (px / self$pixel_width - 0.5) * self$frame_width
      ny <- (0.5 - py / self$pixel_height) * self$frame_height

      # Undo camera rotation
      if (abs(self$rotation) > 1e-10) {
        ca <- cos(self$rotation)
        sa <- sin(self$rotation)
        rx <- nx * ca - ny * sa
        ry <- nx * sa + ny * ca
        nx <- rx
        ny <- ry
      }

      # Offset by camera center
      c(nx + self$frame_center[1],
        ny + self$frame_center[2],
        self$frame_center[3])
    },

    #' @description Get the visible bounding box in world coordinates
    #' @return List with min and max
    get_visible_bounds = function() {
      hw <- self$frame_width / 2
      hh <- self$frame_height / 2
      list(
        min = self$frame_center - c(hw, hh, 0),
        max = self$frame_center + c(hw, hh, 0)
      )
    },

    #' @description Check if a point is within the visible frame
    #' @param point World position
    #' @param buff Optional buffer around the frame
    #' @return Logical
    is_in_frame = function(point, buff = 0) {
      point <- validate_position(point)
      bounds <- self$get_visible_bounds()
      all(point[1:2] >= bounds$min[1:2] - buff) &&
        all(point[1:2] <= bounds$max[1:2] + buff)
    },

    #' @description Print camera summary
    print = function() {
      cli::cli_text("{.cls Camera}")
      cli::cli_ul()
      cli::cli_li("Center: ({paste(round(self$frame_center, 2), collapse = ', ')})")
      cli::cli_li("Frame: {round(self$frame_width, 2)} x {round(self$frame_height, 2)}")
      cli::cli_li("Zoom: {round(self$get_zoom(), 2)}x")
      cli::cli_li("Rotation: {round(self$rotation * 180 / pi, 1)} deg")
      cli::cli_li("Pixels: {self$pixel_width}x{self$pixel_height}")
      cli::cli_end()
      invisible(self)
    }
  )
)

#' MovingCamera R6 Class
#'
#' @description
#' A Camera that can be animated. Provides the `frame` object (a VMobject
#' rectangle representing the camera's field of view) which can be animated
#' with standard Transform/ApplyMethod animations.
#'
#' @export
MovingCamera <- R6::R6Class(
  "MovingCamera",
  inherit = Camera,
  public = list(
    #' @field frame A Rectangle VMobject representing the camera view
    frame = NULL,

    #' @description Create a MovingCamera
    #' @param ... Passed to Camera$new()
    initialize = function(...) {
      super$initialize(...)
      # Create a frame mobject that represents the camera view
      self$frame <- Rectangle$new(
        width = self$frame_width,
        height = self$frame_height
      )
      self$frame$position <- self$frame_center
      self$frame$stroke_width <- 0
    },

    #' @description Sync camera state from the frame mobject
    #'   Call this after animating the frame to update the camera.
    sync_from_frame = function() {
      self$frame_center <- self$frame$position
      # Derive frame dimensions from the frame mobject's current scale
      self$frame_width <- FRAME_WIDTH * self$frame$scale_factor
      self$frame_height <- FRAME_HEIGHT * self$frame$scale_factor
      self$rotation <- self$frame$rotation
      invisible(self)
    },

    #' @description Override move_to to also update frame
    #' @param point Target position
    move_to = function(point) {
      super$move_to(point)
      self$frame$position <- self$frame_center
      invisible(self)
    },

    #' @description Override shift to also update frame
    #' @param direction Shift vector
    shift = function(direction) {
      super$shift(direction)
      self$frame$position <- self$frame_center
      invisible(self)
    },

    #' @description Override set_zoom to also scale frame
    #' @param zoom Zoom factor
    set_zoom = function(zoom) {
      super$set_zoom(zoom)
      self$frame$scale_factor <- 1 / zoom
      invisible(self)
    }
  )
)

#' ThreeDCamera R6 Class
#'
#' @description
#' A 3D camera with spherical coordinates (phi, theta, gamma) and
#' perspective projection.
#'
#' @export
ThreeDCamera <- R6::R6Class(
  "ThreeDCamera",
  inherit = Camera,
  public = list(
    #' @field phi Polar angle from Z-axis (0 = top-down, PI/2 = horizontal)
    phi = 0,
    #' @field theta Azimuthal angle around Z-axis
    theta = -PI / 2,
    #' @field gamma Roll angle
    gamma = 0,
    #' @field focal_distance Distance from camera to focal point
    focal_distance = 5,
    #' @field zoom 3D zoom factor
    zoom = 1,
    #' @field light_source Position of the light source
    light_source = NULL,

    #' @description Create a ThreeDCamera
    #' @param phi Polar angle
    #' @param theta Azimuthal angle
    #' @param gamma Roll angle
    #' @param focal_distance Focal distance
    #' @param zoom Zoom factor
    #' @param ... Passed to Camera
    initialize = function(phi = 0, theta = -PI / 2, gamma = 0,
                          focal_distance = 5, zoom = 1, ...) {
      super$initialize(...)
      self$phi <- phi
      self$theta <- theta
      self$gamma <- gamma
      self$focal_distance <- focal_distance
      self$zoom <- zoom
      self$light_source <- c(-7, -9, 10)
    },

    #' @description Set spherical camera angles
    #' @param phi Polar angle
    #' @param theta Azimuthal angle
    #' @param gamma Roll angle
    set_euler_angles = function(phi = NULL, theta = NULL, gamma = NULL) {
      if (!is.null(phi)) self$phi <- phi
      if (!is.null(theta)) self$theta <- theta
      if (!is.null(gamma)) self$gamma <- gamma
      invisible(self)
    },

    #' @description Get the camera position in Cartesian coordinates
    get_position = function() {
      r <- self$focal_distance
      c(
        r * sin(self$phi) * cos(self$theta),
        r * sin(self$phi) * sin(self$theta),
        r * cos(self$phi)
      )
    },

    #' @description Get the 3x3 rotation matrix for the camera orientation
    get_rotation_matrix = function() {
      # Rotation: first theta around Z, then phi around Y, then gamma around Z
      ct <- cos(self$theta); st <- sin(self$theta)
      cp <- cos(self$phi);   sp <- sin(self$phi)
      cg <- cos(self$gamma); sg <- sin(self$gamma)

      # Combined rotation matrix
      Rz_theta <- matrix(c(ct, -st, 0, st, ct, 0, 0, 0, 1), 3, 3, byrow = TRUE)
      Ry_phi <- matrix(c(cp, 0, sp, 0, 1, 0, -sp, 0, cp), 3, 3, byrow = TRUE)
      Rz_gamma <- matrix(c(cg, -sg, 0, sg, cg, 0, 0, 0, 1), 3, 3, byrow = TRUE)

      Rz_gamma %*% Ry_phi %*% Rz_theta
    },

    #' @description Project a 3D point to 2D screen coordinates
    #' @param point 3D point c(x, y, z)
    #' @return 2D point c(x, y) in screen coordinates
    project_point = function(point) {
      point <- validate_position(point)
      # Translate relative to camera center
      rel <- point - self$frame_center

      # Apply camera rotation
      R <- self$get_rotation_matrix()
      rotated <- as.numeric(R %*% rel)

      # Perspective projection
      z_offset <- self$focal_distance - rotated[3]
      if (z_offset < 0.1) z_offset <- 0.1

      scale <- self$focal_distance / z_offset * self$zoom
      c(rotated[1] * scale, rotated[2] * scale)
    },

    #' @description Project a matrix of 3D points to 2D
    #' @param points Matrix (N x 3)
    #' @return Matrix (N x 2)
    project_points = function(points) {
      if (is.null(points) || nrow(points) == 0) {
        return(matrix(numeric(0), ncol = 2))
      }

      # Translate
      center <- matrix(rep(self$frame_center, nrow(points)),
                        ncol = 3, byrow = TRUE)
      rel <- points - center

      # Rotate
      R <- self$get_rotation_matrix()
      rotated <- rel %*% t(R)

      # Perspective
      z_offset <- self$focal_distance - rotated[, 3]
      z_offset[z_offset < 0.1] <- 0.1
      scale <- self$focal_distance / z_offset * self$zoom

      cbind(rotated[, 1] * scale, rotated[, 2] * scale)
    },

    #' @description Print 3D camera summary
    print = function() {
      cli::cli_text("{.cls ThreeDCamera}")
      cli::cli_ul()
      cli::cli_li("Phi: {round(self$phi * 180 / pi, 1)} deg")
      cli::cli_li("Theta: {round(self$theta * 180 / pi, 1)} deg")
      cli::cli_li("Gamma: {round(self$gamma * 180 / pi, 1)} deg")
      cli::cli_li("Focal distance: {self$focal_distance}")
      cli::cli_li("Zoom: {self$zoom}x")
      cli::cli_end()
      invisible(self)
    }
  )
)
