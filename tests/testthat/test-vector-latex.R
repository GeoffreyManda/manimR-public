
library(testthat)
library(manimR)

test_that("Vector LaTeX compilation works", {
  skip_if(Sys.which("dvisvgm") == "", "dvisvgm not available")
  skip_if(Sys.which("latex") == "", "latex not available")
  
  # Compile simple formula
  svg_path <- manimR:::compile_latex_svg("x^2")
  
  expect_false(is.null(svg_path))
  expect_true(file.exists(svg_path))
  
  # Check content is SVG
  content <- readLines(svg_path, warn = FALSE)
  expect_true(any(grepl("<svg", content)))
  expect_true(any(grepl("<path", content))) # Should have paths
})
