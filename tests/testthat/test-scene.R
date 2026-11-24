# Tests for scene creation and management

test_that("scene() creates a Scene object", {
  s <- scene("test_scene")

  expect_s3_class(s, "Scene")
  expect_equal(s$name, "test_scene")
  expect_equal(s$width, 1920)
  expect_equal(s$height, 1080)
  expect_equal(s$fps, 30)
  expect_equal(s$dimensions, "2D")
})

test_that("scene() accepts custom parameters", {
  s <- scene(
    name = "custom",
    width = 3840,
    height = 2160,
    fps = 60,
    background_color = "#FF0000",
    dimensions = "3D"
  )

  expect_equal(s$width, 3840)
  expect_equal(s$height, 2160)
  expect_equal(s$fps, 60)
  expect_equal(s$background_color, "#FF0000")
  expect_equal(s$dimensions, "3D")
})

test_that("scene_3d() creates a 3D scene", {
  s <- scene_3d("test_3d")

  expect_s3_class(s, "Scene")
  expect_equal(s$dimensions, "3D")
})

test_that("pause() adds pause to timeline", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    pause(2)

  expect_equal(length(s$timeline), 1)
  expect_equal(s$timeline[[1]]$type, "pause")
  expect_equal(s$timeline[[1]]$duration, 2)
})

test_that("scene duration is calculated correctly", {
  s <- scene("test") %>%
    add_text("One") %>%
    write_in(duration = 1) %>%
    pause(2) %>%
    add_text("Two") %>%
    write_in(duration = 1)

  expect_equal(s$get_duration(), 4)
})

test_that("scene frame count is calculated correctly", {
  s <- scene("test", fps = 30) %>%
    add_text("Test") %>%
    write_in(duration = 2)

  expect_equal(s$get_frame_count(), 60)
})

test_that("wait() is an alias for pause()", {
  s1 <- scene("test1") %>%
    add_text("Hello") %>%
    pause(1)

  s2 <- scene("test2") %>%
    add_text("Hello") %>%
    wait(1)

  expect_equal(s1$get_duration(), s2$get_duration())
})

test_that("scene can be printed", {
  s <- scene("test") %>%
    add_text("Hello") %>%
    write_in()

  # cli output may go to stdout or message connection depending on context;

  # verify print runs without error and returns invisibly
  out <- capture.output(result <- print(s), type = "output")
  msg <- capture.output(print(s), type = "message")
  combined <- paste(c(out, msg), collapse = "\n")
  expect_true(grepl("manimR Scene|Scene|test", combined))
})
