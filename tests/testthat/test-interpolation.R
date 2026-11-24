
library(testthat)
library(manimR)

test_that("Curve matching works", {
  # 1 curve (4 points)
  c1 <- matrix(runif(4*3), ncol=3)
  # 2 curves (8 points)
  c2 <- matrix(runif(8*3), ncol=3)
  
  matched <- manimR:::match_curves(c1, c2)
  
  # Both should be 8 points (2 curves)
  expect_equal(nrow(matched[[1]]), 8)
  expect_equal(nrow(matched[[2]]), 8)
  
  # Check duplication logic (last point of c1 should be repeated for the new null curve)
  last_curve_start <- matched[[1]][5, ]
  original_end <- c1[4, ]
  
  expect_equal(last_curve_start, original_end)
})

test_that("Matrix interpolation works", {
  m1 <- matrix(0, nrow=4, ncol=3)
  m2 <- matrix(10, nrow=4, ncol=3)
  
  # Halfway
  res <- manimR:::interpolate_matrices(m1, m2, 0.5)
  expect_equal(res[1,1], 5)
  
  # Start
  res <- manimR:::interpolate_matrices(m1, m2, 0)
  expect_equal(res[1,1], 0)
  
  # End
  res <- manimR:::interpolate_matrices(m1, m2, 1)
  expect_equal(res[1,1], 10)
})
