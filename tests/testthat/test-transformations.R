# Tests for transformation functions

test_that("transform_to() transforms LaTeX expressions", {
  s <- scene("test") %>%
    add_latex("a^2") %>%
    write_in() %>%
    transform_to("a^2 + b^2", duration = 2)

  # Should have 2 objects (source and target)
  expect_equal(length(s$objects), 2)

  # Should have a transform animation
  transform_anim <- s$timeline[[2]]
  expect_equal(transform_anim$type, "transform")
})

test_that("transform_to() creates target object", {
  s <- scene("test") %>%
    add_latex("x") %>%
    write_in() %>%
    transform_to("x^2")

  # Check target object exists
  target <- s$objects[[2]]
  expect_s3_class(target, "LaTeXObject")
  expect_equal(target$latex, "x^2")
})

test_that("morph() transforms objects", {
  s <- scene("test") %>%
    add_latex("f(x)") %>%
    write_in() %>%
    morph("g(x)")

  expect_equal(length(s$objects), 2)
})

test_that("move_to() adds movement animation", {

  s <- scene("test") %>%
    add_circle(position = c(0, 0, 0)) %>%
    grow_from_center() %>%
    move_to(c(3, 2, 0), duration = 1)

  anim <- s$timeline[[2]]
  # move_to() now uses ApplyMethod internally

  expect_equal(anim$type, "applymethod")
  expect_true(inherits(anim$animation_object, "Animation"))
})

test_that("shift() adds relative movement", {
  s <- scene("test") %>%
    add_text("Test") %>%
    write_in() %>%
    shift(c(2, 0, 0))

  anim <- s$timeline[[2]]
  # shift() now uses ApplyMethod internally
  expect_equal(anim$type, "applymethod")
  expect_true(inherits(anim$animation_object, "Animation"))
})

test_that("rotate() adds rotation animation", {
  s <- scene("test") %>%
    add_rectangle() %>%
    grow_from_center() %>%
    rotate(pi / 2, duration = 1)

  anim <- s$timeline[[2]]
  # rotate() now uses RotateAnim R6 class
  expect_equal(anim$type, "rotateanim")
  expect_equal(anim$animation_object$angle, pi / 2)
})

test_that("rotate() converts degrees when specified", {
  s <- scene("test") %>%
    add_rectangle() %>%
    grow_from_center() %>%
    rotate(90, degrees = TRUE)

  anim <- s$timeline[[2]]
  # rotate() now uses RotateAnim R6 class
  expect_equal(anim$animation_object$angle, pi / 2, tolerance = 1e-10)
})

test_that("scale_object() adds scaling animation", {
  s <- scene("test") %>%
    add_circle() %>%
    grow_from_center() %>%
    scale_object(2, duration = 1)

  anim <- s$timeline[[2]]
  # scale_object() now uses ApplyMethod internally
  expect_equal(anim$type, "applymethod")
  expect_true(inherits(anim$animation_object, "Animation"))
})

test_that("scale_object() rejects invalid factors", {
  s <- scene("test") %>%
    add_circle() %>%
    grow_from_center()

  expect_error(scale_object(s, -1), "positive")
  expect_error(scale_object(s, 0), "positive")
})

test_that("highlight_term() adds highlight animation", {
  s <- scene("test") %>%
    add_latex("E = mc^2") %>%
    write_in() %>%
    highlight_term("c^2", color = "yellow")

  anim <- s$timeline[[2]]
  expect_equal(anim$type, "highlight_term")
  expect_equal(anim$term, "c^2")
})

test_that("change_color() adds color animation", {
  s <- scene("test") %>%
    add_text("Test") %>%
    write_in() %>%
    change_color("red")

  anim <- s$timeline[[2]]
  # change_color() now uses ApplyMethod internally
  expect_equal(anim$type, "applymethod")
  expect_true(inherits(anim$animation_object, "Animation"))
})

test_that("circumscribe() adds circumscribe animation", {
  s <- scene("test") %>%
    add_latex("Important") %>%
    write_in() %>%
    circumscribe(shape = "rectangle", color = "yellow")

  anim <- s$timeline[[2]]
  expect_equal(anim$type, "circumscribe")
  # shape is stored on the R6 animation object
  expect_equal(anim$animation_object$shape, "rectangle")
})

test_that("transformations chain correctly", {
  s <- scene("test") %>%
    add_circle(position = c(-3, 0, 0)) %>%
    grow_from_center() %>%
    move_to(c(0, 0, 0)) %>%
    scale_object(2) %>%
    rotate(pi) %>%
    move_to(c(3, 0, 0))

  expect_equal(length(s$timeline), 5)
})
