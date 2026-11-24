# Tests for template functions

test_that("list_templates returns a tibble", {
  templates <- list_templates()

  expect_s3_class(templates, "tbl_df")
  expect_true("name" %in% names(templates))
  expect_true("description" %in% names(templates))
  expect_true("category" %in% names(templates))
})

test_that("list_templates includes expected templates", {
  templates <- list_templates()

  expect_true("theorem_proof" %in% templates$name)
  expect_true("definition_display" %in% templates$name)
  expect_true("algorithm_visualization" %in% templates$name)
})

test_that("use_template creates a Template object", {
  t <- use_template("theorem_proof")

  expect_s3_class(t, "Template")
  expect_equal(t$name, "theorem_proof")
})

test_that("use_template errors for unknown template", {
  expect_error(use_template("nonexistent"), "not found")
})

test_that("customize sets configuration", {
  t <- use_template("theorem_proof") %>%
    customize(color_scheme = "dark", font = "serif")

  expect_equal(t$get("color_scheme"), "dark")
  expect_equal(t$get("font"), "serif")
})

test_that("set_theorem sets theorem configuration", {
  t <- use_template("theorem_proof") %>%
    set_theorem("My Theorem", "Statement here")

  expect_equal(t$get("theorem_name"), "My Theorem")
  expect_equal(t$get("theorem_statement"), "Statement here")
})

test_that("set_proof_steps sets proof steps", {
  steps <- list("Step 1", "Step 2", "Step 3")

  t <- use_template("theorem_proof") %>%
    set_proof_steps(steps)

  expect_equal(t$get("proof_steps"), steps)
})

test_that("Template can be built into a scene", {
  t <- use_template("theorem_proof") %>%
    set_theorem("Test", "a = b")

  s <- t$build()

  expect_s3_class(s, "Scene")
  expect_true(length(s$objects) > 0)
})

test_that("Template can be printed", {
  t <- use_template("theorem_proof") %>%
    set_theorem("Test")

  # cli output may go to stdout or message connection depending on context;
  # verify print runs without error and captures expected text
  out <- capture.output(print(t), type = "output")
  msg <- capture.output(print(t), type = "message")
  combined <- paste(c(out, msg), collapse = "\n")
  expect_true(grepl("manimR Template|Template|theorem_proof", combined))
})
