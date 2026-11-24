# Tests for object creation

test_that("add_text() adds a TextObject", {
  s <- scene("test") %>%
    add_text("Hello, World!")

  expect_equal(length(s$objects), 1)

  obj <- s$get_last_object()
  expect_s3_class(obj, "TextObject")
  expect_equal(obj$text, "Hello, World!")
  expect_equal(obj$type, "text")
})

test_that("add_text() accepts position and style parameters", {
  s <- scene("test") %>%
    add_text("Test",
             position = c(1, 2, 0),
             size = 72,
             color = "red",
             font = "serif")

  obj <- s$get_last_object()

  expect_equal(obj$position, c(1, 2, 0))
  expect_equal(obj$size, 72)
  expect_equal(obj$font, "serif")
})

test_that("add_latex() adds a LaTeXObject", {
  s <- scene("test") %>%
    add_latex("E = mc^2")

  expect_equal(length(s$objects), 1)

  obj <- s$get_last_object()
  expect_s3_class(obj, "LaTeXObject")
  expect_equal(obj$latex, "E = mc^2")
  expect_equal(obj$type, "latex")
})

test_that("add_shape() adds a ShapeObject", {
  s <- scene("test") %>%
    add_shape("rectangle", width = 2, height = 1)

  obj <- s$get_last_object()
  expect_s3_class(obj, "ShapeObject")
  expect_equal(obj$shape_type, "rectangle")
  expect_equal(obj$width, 2)
  expect_equal(obj$height, 1)
})

test_that("add_circle() is a shortcut for circle shapes", {
  s <- scene("test") %>%
    add_circle(radius = 2, color = "blue")

  obj <- s$get_last_object()
  expect_equal(obj$shape_type, "circle")
  expect_equal(obj$radius, 2)
})

test_that("add_rectangle() is a shortcut for rectangle shapes", {
  s <- scene("test") %>%
    add_rectangle(width = 4, height = 2)

  obj <- s$get_last_object()
  expect_equal(obj$shape_type, "rectangle")
  expect_equal(obj$width, 4)
  expect_equal(obj$height, 2)
})

test_that("add_line() creates a line object", {
  s <- scene("test") %>%
    add_line(start = c(-1, 0, 0), end = c(1, 0, 0))

  obj <- s$get_last_object()
  expect_equal(obj$shape_type, "line")
  expect_equal(obj$data$end, c(1, 0, 0))
})

test_that("add_arrow() creates an arrow object", {
  s <- scene("test") %>%
    add_arrow(start = c(0, 0, 0), end = c(2, 0, 0), tip_size = 0.3)

  obj <- s$get_last_object()
  expect_equal(obj$shape_type, "arrow")
  expect_equal(obj$data$tip_size, 0.3)
})

test_that("position validation works", {
  # 2D position should be extended to 3D
  s <- scene("test") %>%
    add_text("Test", position = c(1, 2))

  obj <- s$get_last_object()
  expect_equal(length(obj$position), 3)
  expect_equal(obj$position, c(1, 2, 0))
})

test_that("color validation works for named colors", {
  s <- scene("test") %>%
    add_text("Test", color = "red")

  obj <- s$get_last_object()
  expect_match(obj$color, "^#[0-9A-F]{6}$")
})

test_that("objects get unique IDs", {
  s <- scene("test") %>%
    add_text("One") %>%
    add_text("Two") %>%
    add_text("Three")

  ids <- sapply(s$objects, function(obj) obj$id)
  expect_equal(length(unique(ids)), 3)
})

test_that("get_last_object() returns the most recent object", {
  s <- scene("test") %>%
    add_text("First") %>%
    add_text("Second") %>%
    add_text("Third")

  obj <- s$get_last_object()
  expect_equal(obj$text, "Third")
})
