
library(testthat)
library(manimR)

test_that("SVG path parsing works", {
  # Rectangle path: M 0 0 L 10 0 L 10 10 L 0 10 Z
  path <- "M 0 0 L 10 0 L 10 10 L 0 10 Z"
  points <- manimR:::parse_svg_path_data(path)
  
  # Should have 4 segments -> 16 points (or optimized if we merge start/end)
  # My parser blindly appends 4 points per segment for now.
  # 4 lines * 4 points = 16 points.
  expect_equal(nrow(points), 16)
  
  # Check first segment (Line 0,0 -> 10,0)
  # P0=0,0 P3=10,0
  expect_equal(points[1, 1:2], c(0, 0))
  expect_equal(points[4, 1:2], c(10, 0))
})

test_that("Cubic bezier parsing works", {
  # Cubic: M 0 0 C 1 1 2 2 3 3
  path <- "M 0 0 C 1 1 2 2 3 3"
  points <- manimR:::parse_svg_path_data(path)
  
  expect_equal(nrow(points), 4)
  expect_equal(points[2, 1:2], c(1, 1)) # P1
  expect_equal(points[3, 1:2], c(2, 2)) # P2
  expect_equal(points[4, 1:2], c(3, 3)) # P3
})
