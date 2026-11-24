# Tests for utility functions

test_that("validate_color handles named colors", {
  result <- manimR:::validate_color("red")
  expect_match(result, "^#[0-9A-F]{6}$")
})

test_that("validate_color handles hex colors", {
  result <- manimR:::validate_color("#FF0000")
  expect_equal(result, "#FF0000")

  result_lower <- manimR:::validate_color("#ff0000")
  expect_equal(result_lower, "#FF0000")
})

test_that("validate_color handles NULL", {
  result <- manimR:::validate_color(NULL)
  expect_equal(result, "#FFFFFF")
})

test_that("validate_color rejects invalid input", {
  expect_error(manimR:::validate_color("not_a_color"), "Invalid color")
})

test_that("validate_position normalizes 2D to 3D", {
  result <- manimR:::validate_position(c(1, 2))
  expect_equal(result, c(1, 2, 0))
})

test_that("validate_position accepts 3D", {
  result <- manimR:::validate_position(c(1, 2, 3))
  expect_equal(result, c(1, 2, 3))
})

test_that("validate_position handles NULL", {
  result <- manimR:::validate_position(NULL)
  expect_equal(result, c(0, 0, 0))
})

test_that("validate_duration rejects non-numeric", {
  expect_error(manimR:::validate_duration("one"), "numeric")
})

test_that("validate_duration rejects negative", {
  expect_error(manimR:::validate_duration(-1), "non-negative")
})

test_that("generate_id creates unique IDs", {
  id1 <- manimR:::generate_id("test")
  Sys.sleep(0.01)  # Ensure different timestamp
  id2 <- manimR:::generate_id("test")

  expect_false(id1 == id2)
  expect_match(id1, "^test_")
})

test_that("lerp interpolates correctly", {
  expect_equal(manimR:::lerp(0, 10, 0), 0)
  expect_equal(manimR:::lerp(0, 10, 1), 10)
  expect_equal(manimR:::lerp(0, 10, 0.5), 5)
})

test_that("easing functions return values in [0,1] range", {
  easings <- c("linear", "smooth", "in_quad", "out_quad",
               "in_out_quad", "in_cubic", "out_cubic", "in_out_cubic")

  for (name in easings) {
    fn <- manimR:::get_easing(name)

    # Check endpoints
    expect_equal(fn(0), 0, label = paste(name, "at 0"))
    expect_equal(fn(1), 1, label = paste(name, "at 1"))

    # Check midpoint is reasonable
    mid <- fn(0.5)
    expect_true(mid >= 0 && mid <= 1, label = paste(name, "midpoint"))
  }
})

test_that("get_easing returns smooth for unknown", {
  expect_warning(fn <- manimR:::get_easing("unknown"))
  expect_equal(fn(0.5), manimR:::ease_smooth(0.5))
})

test_that("manimR_options gets default values", {
  opts <- manimR_options()
  expect_type(opts, "list")
  expect_true("fps" %in% names(opts))
  expect_true("verbose" %in% names(opts))
})

test_that("manimR_options gets single value", {
  fps <- manimR_options("fps")
  expect_type(fps, "double")
})

test_that("manimR_options sets values", {
  old <- manimR_options("verbose")
  manimR_options(verbose = FALSE)
  expect_false(manimR_options("verbose"))
  manimR_options(verbose = old)  # Restore
})

test_that("pipe operator works", {
  result <- 5 %>% `+`(3) %>% `*`(2)
  expect_equal(result, 16)
})
