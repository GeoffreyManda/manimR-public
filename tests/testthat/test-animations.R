# Tests for animation functions

test_that("write_in() adds animation to timeline", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    write_in(duration = 2)

  expect_equal(length(s$timeline), 1)
  expect_equal(s$timeline[[1]]$type, "write_in")
  expect_equal(s$timeline[[1]]$duration, 2)
})

test_that("fade_in() adds fade animation", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    fade_in(duration = 1)

  expect_equal(s$timeline[[1]]$type, "fadein")
})

test_that("fade_in() with direction creates fadein animation", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    fade_in(direction = "up", shift = 1)

  anim <- s$timeline[[1]]
  expect_equal(anim$type, "fadein")
})

test_that("fade_out() adds fade out animation", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    fade_in() %>%
    fade_out(duration = 0.5)

  expect_equal(length(s$timeline), 2)
  expect_equal(s$timeline[[2]]$type, "fadeout")
})

test_that("grow_from_center() adds grow animation", {
  s <- scene("test") %>%
    add_circle() %>%
    grow_from_center(duration = 1.5)

  anim <- s$timeline[[1]]
  expect_equal(anim$type, "growfromcenter")
  expect_equal(anim$duration, 1.5)
})

test_that("shrink_to_center() adds shrink animation", {
  s <- scene("test") %>%
    add_circle() %>%
    grow_from_center() %>%
    shrink_to_center()

  expect_equal(s$timeline[[2]]$type, "fadeout")
})

test_that("draw_border() adds border animation", {
  s <- scene("test") %>%
    add_rectangle() %>%
    draw_border(duration = 2)

  anim <- s$timeline[[1]]
  expect_equal(anim$type, "drawborderthenfill")
})

test_that("flash() adds flash animation", {
  s <- scene("test") %>%
    add_text("Attention!") %>%
    write_in() %>%
    flash(color = "yellow")

  expect_equal(s$timeline[[2]]$type, "flash")
})

test_that("indicate() adds indicate animation", {
  s <- scene("test") %>%
    add_text("Important") %>%
    write_in() %>%
    indicate(scale_factor = 1.3)

  anim <- s$timeline[[2]]
  expect_equal(anim$type, "indicate")
})

test_that("pulse() adds wiggle animation", {
  s <- scene("test") %>%
    add_circle() %>%
    grow_from_center() %>%
    pulse(n_pulses = 3)

  anim <- s$timeline[[2]]
  expect_equal(anim$type, "wiggle")
})

test_that("animations chain correctly", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    write_in(duration = 1) %>%
    pause(0.5) %>%
    fade_out(duration = 0.5)

  expect_equal(length(s$timeline), 3)
  expect_equal(s$get_duration(), 2)
})

test_that("animation without object throws error", {
  s <- scene("test")

  expect_error(write_in(s), "No object")
})

test_that("duration validation rejects negative values", {
  s <- scene("test") %>%
    add_text("Test")

  expect_error(write_in(s, duration = -1), "non-negative")
})

test_that("easing parameter is stored", {
  s <- scene("test") %>%
    add_text("Test") %>%
    fade_in(easing = "in_out_cubic")

  expect_equal(s$timeline[[1]]$easing, "in_out_cubic")
})
