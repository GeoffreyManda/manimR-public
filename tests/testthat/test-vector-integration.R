
library(testthat)
library(manimR)

test_that("Vector LaTeX integration works", {
  skip_if(Sys.which("dvisvgm") == "", "dvisvgm not available")
  
  # Create scene
  s <- manimR::scene("vector_test")
  manimR::add_latex(s, "A") # Use simple char
  
  # Get object
  obj_id <- names(s$objects)[length(s$objects)]
  tex_obj <- s$objects[[obj_id]]
  
  # Check it is a VMobject/LaTeXObject
  expect_true(inherits(tex_obj, "VMobject"))
  
  # Check it has points
  # Should have some points from 'A'
  pts <- tex_obj$get_points()
  expect_gt(nrow(pts), 0)
  expect_equal(ncol(pts), 3)
  
  # Render frame
  tmp <- tempfile(fileext = ".png")
  manimR:::render_single_frame(s, 0.0, tmp, 400, 400, "white", FALSE)
  
  expect_true(file.exists(tmp))
  # Check content not blank
  expect_gt(magick::image_info(magick::image_read(tmp))$width, 0)
})
