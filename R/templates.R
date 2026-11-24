#' @title Animation Templates
#' @description
#' Pre-built templates for common animation scenarios like theorem proofs,
#' algorithm visualizations, and data presentations.
#' @name templates
NULL

#' Use a pre-built template
#'
#' @description
#' Loads a pre-built animation template that can be customized and rendered.
#'
#' @param template_name Name of the template
#' @param ... Additional arguments passed to the template
#'
#' @return A Template object that can be customized
#'
#' @examples
#' \dontrun{
#' use_template("theorem_proof") %>%
#'   set_theorem("Pythagorean Theorem", "a^2 + b^2 = c^2") %>%
#'   set_proof_steps(list(
#'     "Consider a right triangle...",
#'     "By similar triangles..."
#'   )) %>%
#'   render()
#' }
#'
#' @export
use_template <- function(template_name, ...) {
  available <- list_templates()

  if (!template_name %in% available$name) {
    cli::cli_abort(
      c("Template not found: {.val {template_name}}",
        "i" = "Available templates: {.val {available$name}}")
    )
  }

  # Create template object
  template <- Template$new(template_name)

  if (manimR_options("verbose")) {
    cli::cli_alert_info("Loaded template: {.val {template_name}}")
  }

  template
}

#' List available templates
#'
#' @description
#' Returns a data frame of available animation templates with their descriptions.
#'
#' @return A tibble with template names, descriptions, and categories
#'
#' @examples
#' \dontrun{
#' list_templates()
#' }
#'
#' @export
list_templates <- function() {
  tibble::tibble(
    name = c(
      "theorem_proof",
      "definition_display",
      "algorithm_visualization",
      "data_story",
      "equation_derivation",
      "comparison",
      "timeline",
      "process_flow"
    ),
    description = c(
      "Display a theorem statement and animate its proof",
      "Introduce a mathematical definition with examples",
      "Visualize an algorithm step by step",
      "Create a data-driven narrative animation",
      "Show step-by-step equation derivation",
      "Compare two or more concepts side by side",
      "Animate events along a timeline",
      "Show a process with connected steps"
    ),
    category = c(
      "academic",
      "academic",
      "data_science",
      "data_science",
      "academic",
      "educational",
      "educational",
      "educational"
    )
  )
}

#' Template R6 Class
#'
#' @description
#' Internal class representing an animation template.
#'
#' @keywords internal
#' @importFrom R6 R6Class
Template <- R6::R6Class(
  "Template",

  public = list(
    #' @field name Template name
    name = NULL,

    #' @field config Template configuration
    config = NULL,

    #' @field scene Internal scene object
    scene = NULL,

    #' @description Create a new Template
    #' @param name Template name
    initialize = function(name) {
      self$name <- name
      self$config <- list()
      self$scene <- scene(name)
    },

    #' @description Set a configuration value
    #' @param key Configuration key
    #' @param value Configuration value
    set = function(key, value) {
      self$config[[key]] <- value
      invisible(self)
    },

    #' @description Get a configuration value
    #' @param key Configuration key
    get = function(key) {
      self$config[[key]]
    },

    #' @description Build the scene from template
    build = function() {
      # Build scene based on template type
      switch(self$name,
        "theorem_proof" = private$build_theorem_proof(),
        "definition_display" = private$build_definition(),
        "equation_derivation" = private$build_derivation(),
        # Default
        private$build_generic()
      )
      self$scene
    },

    #' @description Print template info
    print = function() {
      cli::cli_h2("manimR Template: {self$name}")
      cli::cli_ul()
      for (key in names(self$config)) {
        cli::cli_li("{key}: {.val {self$config[[key]]}}")
      }
      cli::cli_end()
      invisible(self)
    }
  ),

  private = list(
    build_theorem_proof = function() {
      theorem_name <- self$config$theorem_name %||% "Theorem"
      theorem_statement <- self$config$theorem_statement %||% "Statement"
      proof_steps <- self$config$proof_steps %||% list()

      # Build the scene
      self$scene %>%
        add_text(theorem_name, position = c(0, 3, 0), size = 72) %>%
        write_in(duration = 1)

      if (!is.null(theorem_statement)) {
        self$scene %>%
          add_latex(theorem_statement, position = c(0, 1.5, 0)) %>%
          write_in(duration = 1.5) %>%
          pause(1)
      }

      # Add proof steps
      for (i in seq_along(proof_steps)) {
        y_pos <- 0 - (i - 1) * 1.2
        self$scene %>%
          add_latex(proof_steps[[i]], position = c(0, y_pos, 0), size = 0.8) %>%
          write_in(duration = 1) %>%
          pause(0.5)
      }
    },

    build_definition = function() {
      term <- self$config$term %||% "Term"
      definition <- self$config$definition %||% "Definition"

      self$scene %>%
        add_text(term, position = c(0, 2, 0), size = 72, color = "#4CAF50") %>%
        write_in(duration = 1) %>%
        pause(0.5) %>%
        add_text(definition, position = c(0, 0, 0), size = 36) %>%
        fade_in(duration = 1)
    },

    build_derivation = function() {
      steps <- self$config$steps %||% list()
      if (length(steps) == 0) return()

      # Add first step
      self$scene %>%
        add_latex(steps[[1]], position = c(0, 2, 0)) %>%
        write_in(duration = 1.5)

      # Transform through remaining steps
      for (i in seq_along(steps)[-1]) {
        self$scene %>%
          pause(1) %>%
          transform_to(steps[[i]], duration = 1.5)
      }
    },

    build_generic = function() {
      title <- self$config$title %||% "Animation"
      self$scene %>%
        add_text(title, size = 72) %>%
        write_in(duration = 1)
    }
  )
)

#' Customize a template
#'
#' @description
#' Apply customizations to a template.
#'
#' @param template A Template object
#' @param ... Named customization options (color_scheme, font, style, etc.)
#'
#' @return The Template object (invisibly) for piping
#'
#' @export
customize <- function(template, ...) {
  if (!inherits(template, "Template")) {
    cli::cli_abort("{.arg template} must be a Template object")
  }

  opts <- list(...)
  for (key in names(opts)) {
    template$set(key, opts[[key]])
  }

  invisible(template)
}

# Template setter functions

#' Set theorem for template
#'
#' @param template A Template object
#' @param name Theorem name
#' @param statement Theorem statement (LaTeX)
#' @return The Template object
#' @export
set_theorem <- function(template, name, statement = NULL) {
  template$set("theorem_name", name)
  if (!is.null(statement)) {
    template$set("theorem_statement", statement)
  }
  invisible(template)
}

#' Set proof steps for template
#'
#' @param template A Template object
#' @param steps List of proof step strings (LaTeX)
#' @return The Template object
#' @export
set_proof_steps <- function(template, steps) {
  template$set("proof_steps", steps)
  invisible(template)
}

#' Set derivation steps
#'
#' @param template A Template object
#' @param steps List of equation strings (LaTeX)
#' @return The Template object
#' @export
set_derivation_steps <- function(template, steps) {
  template$set("steps", steps)
  invisible(template)
}

#' Render a template
#'
#' @param x A Template object
#' @param ... Additional arguments passed to render
#' @return Output file path (invisibly)
#' @export
render.Template <- function(x, ...) {
  scene <- x$build()
  render(scene, ...)
}
