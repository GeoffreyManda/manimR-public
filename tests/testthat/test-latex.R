
library(testthat)
library(manimR)

test_that("latex compiler works", {
  skip_if_not(manimR:::check_latex_deps(), "LaTeX or magick not available")
  
  # Simple formula
  img_path <- manimR:::compile_latex_svg("E = mc^2")
  
  expect_false(is.null(img_path))
  expect_true(file.exists(img_path))
  
  # Check if it's a valid SVG (compile_latex_svg now returns SVG)
  img <- magick::image_read(img_path)
  info <- magick::image_info(img)
  expect_equal(info$format, "SVG")
  expect_gt(info$width, 0)
})
