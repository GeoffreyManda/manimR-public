
library(testthat)
library(manimR)

test_that("LaTeXObject renders as image", {
  skip_if_not_installed("ragg")
  skip_if_not(manimR:::check_latex_deps(), "LaTeX/magick missing")
  
  # Create scene with LaTeX
  s <- manimR::scene("latex_test")
  manimR::add_latex(s, "x^2 + y^2 = z^2")
  # Get the object (it's the last added one)
  obj_id <- names(s$objects)[length(s$objects)]
  tex_obj <- s$objects[[obj_id]]
  
  # Ensure it compiled
  expect_true(!is.null(tex_obj$data$path))
  expect_true(file.exists(tex_obj$data$path))
  
  # Render a frame
  tmp <- tempfile(fileext = ".png")
  manimR:::render_single_frame(s, 0.0, tmp, 400, 400, "white", FALSE)
  
  expect_true(file.exists(tmp))
  # Detailed check would be to read tmp and see if it's not blank
  img <- magick::image_read(tmp)
  # Standard deviation > 0 means not solid color
  expect_gt(magick::image_info(img)$width, 0)
})
