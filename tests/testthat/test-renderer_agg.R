
library(testthat)
library(manimR)

# Mock scene object
Scene <- R6::R6Class("Scene",
  public = list(
    objects = list(),
    timeline = list(),
    get_duration = function() 1,
    add_object = function(obj) self$objects[[obj$id]] <- obj
  )
)

test_that("Renderer produces a PNG file", {
  # Skip if ragg/magick not installed (local dev check)
  skip_if_not_installed("ragg")

  # Create a simple renderer using new API
  renderer <- manimR:::Renderer$new(width = 100, height = 100, background_color = "#000000")

  # Create a temp file
  tmp <- tempfile(fileext = ".png")

  # Begin frame (opens device)
  renderer$begin_frame(tmp)

  # Draw something directly using base graphics (simulating draw_object)
  graphics::rect(0, 0, 1, 1, col = "red")

  # End frame (closes device)
  renderer$end_frame()

  # Check if file exists and has size
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)

  # Cleanup
  unlink(tmp)
})

test_that("Renderer works via render_single_frame", {
  skip_if_not_installed("ragg")
  
  # Create dummy scene
  s <- manimR::scene("test_render")
  manimR::add_circle(s, radius = 1, color = "red")
  manimR::add_text(s, "Hello", position = c(0, 0, 0))
  
  # Add animation to make objects visible
  manimR::grow_from_center(s)
  
  tmp <- tempfile(fileext = ".png")
  
  # Try to render frame 1 (time 1.0)
  manimR:::render_single_frame(s, 1.0, tmp, 800, 600, "#000000", FALSE)
  
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 1000) # Should be substantial size
})
