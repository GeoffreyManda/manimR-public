# Tests for rendering functions

test_that("get_quality_settings returns correct dimensions", {
  low <- manimR:::get_quality_settings("low")
  expect_equal(low$width, 854)
  expect_equal(low$height, 480)

  medium <- manimR:::get_quality_settings("medium")
  expect_equal(medium$width, 1280)
  expect_equal(medium$height, 720)

  high <- manimR:::get_quality_settings("high")
  expect_equal(high$width, 1920)
  expect_equal(high$height, 1080)

  ultra <- manimR:::get_quality_settings("4K")
  expect_equal(ultra$width, 3840)
  expect_equal(ultra$height, 2160)
})

test_that("render() requires a Scene object", {
  expect_error(render("not a scene"), "Scene object")
})

test_that("render() warns for empty scene", {
  s <- scene("empty")
  expect_warning(render(s), "no duration")
})

test_that("preview() creates temp file", {
  skip_on_cran()
  skip_if_not(requireNamespace("magick", quietly = TRUE))

  s <- scene("test") %>%
    add_text("Hello") %>%
    fade_in(duration = 0.5)

  # Just check it doesn't error
  expect_no_error({
    suppressMessages(preview(s))
  })
})

test_that("render validates format parameter", {
  s <- scene("test") %>%
    add_text("Test") %>%
    write_in()

  expect_error(
    render(s, format = "invalid_format")
  )
})
