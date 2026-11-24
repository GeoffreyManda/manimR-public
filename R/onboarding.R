#' @title Onboarding and Tutorial System
#' @description
#' Interactive onboarding wizard, tutorial mode, and user preference system
#' for manimR. Provides an educational introduction to the package and
#' foundational mathematics through visualization.
#'
#' @name onboarding
NULL

# Global onboarding state
.onboarding_state <- new.env(parent = emptyenv())

#' User modes
USER_MODE_TUTORIAL <- "tutorial"
USER_MODE_DEVELOPER <- "developer"
USER_MODE_EXPERT <- "expert"

#' Initialize onboarding system
#'
#' @description
#' Called on package load to check if user needs onboarding.
#'
#' @keywords internal
#' @noRd
initialize_onboarding <- function() {
  # Check if user has completed onboarding
  onboarding_done <- getOption("manimR.onboarding_complete", FALSE)
  user_prefs <- manimR_user_prefs()

  if (!onboarding_done && interactive()) {
    # Delay onboarding to allow package to fully load
    later::later(function() {
      tryCatch({
        run_onboarding_wizard()
      }, error = function(e) {
        cli::cli_alert_warning("Could not start onboarding: {e$message}")
      })
    }, delay = 1)
  }

  invisible()
}

#' Run interactive onboarding wizard
#'
#' @description
#' Guides new users through package setup, theme selection,
#' and mode selection with interactive examples.
#'
#' @export
run_onboarding_wizard <- function() {
  if (!interactive()) {
    return(invisible(NULL))
  }

  cli::cli_h1("Welcome to manimR!")
  cli::cli_text("Let's set up your mathematical animation environment.")
  cli::cli_text("")

  # Step 1: User mode selection
  cli::cli_h2("Step 1: Choose Your Experience Level")

  mode <- utils::menu(
    title = "Which mode suits you best?",
    choices = c(
      "Tutorial Mode - I'm learning math and want guided visualizations",
      "Developer Mode - I want to build animations and need debugging tools",
      "Expert Mode - I know what I'm doing, give me full control"
    )
  )

  selected_mode <- switch(mode,
    USER_MODE_TUTORIAL,
    USER_MODE_DEVELOPER,
    USER_MODE_EXPERT,
    USER_MODE_EXPERT
  )

  manimR_user_prefs(mode = selected_mode)

  # Step 2: Theme selection
  cli::cli_h2("Step 2: Choose Your Visual Theme")

  theme <- utils::menu(
    title = "Select a theme:",
    choices = c(
      "Academic - Clean, professional look for papers and presentations",
      "Dark - Modern dark theme for videos and dark backgrounds",
      "Light - Bright theme for documents and light backgrounds",
      "3Blue1Brown - Grant Sanderson's signature style",
      "Minimal - Simple, distraction-free visualization"
    )
  )

  theme_name <- switch(theme,
    "academic",
    "dark",
    "light",
    "3b1b",
    "minimal",
    "dark"
  )

  apply_theme(theme_name)
  manimR_user_prefs(theme = theme_name)

  # Step 3: Subject interests
  cli::cli_h2("Step 3: What Math Topics Interest You?")
  cli::cli_text("Select topics for personalized tutorials:")

  topics <- list(
    algebra = readline("Algebra and Functions? (y/n): ") == "y",
    linear_algebra = readline("Matrices and Vectors? (y/n): ") == "y",
    calculus = readline("Calculus? (y/n): ") == "y",
    graph_theory = readline("Graph Theory? (y/n): ") == "y",
    geometry = readline("Geometry? (y/n): ") == "y",
    statistics = readline("Statistics? (y/n): ") == "y",
    number_theory = readline("Number Theory? (y/n): ") == "y"
  )

  manimR_user_prefs(topics = topics)

  # Step 4: Backend verification
  cli::cli_h2("Step 4: Performance Setup")
  cli::cli_text("Checking available backends...")

  backends <- list_backends()
  if (backends["cpp"]) {
    cli::cli_alert_success("C++ backend available - fast rendering enabled!")
  } else if (backends["rust"]) {
    cli::cli_alert_success("Rust backend available - fast rendering enabled!")
  } else {
    cli::cli_alert_warning("No compiled backend. Install Rcpp for better performance.")
  }

  # Step 5: Quick demo (optional)
  cli::cli_h2("Step 5: Quick Demo")
  demo_choice <- utils::menu(
    title = "Would you like to see a quick demonstration?",
    choices = c("Yes - Show me what manimR can do!", "No - I'll explore on my own")
  )

  if (demo_choice == 1) {
    show_quick_demo()
  }

  # Mark onboarding complete
  options(manimR.onboarding_complete = TRUE)
  save_user_prefs()

  cli::cli_h2("Setup Complete!")
  cli::cli_text("You're ready to create mathematical animations.")
  cli::cli_text("")
  cli::cli_alert_info("Quick start:")
  cli::cli_code(
    "scene(\"my_first\") %>%",
    "  add_latex(\"E = mc^2\") %>%",
    "  write_in() %>%",
    "  render()"
  )
  cli::cli_text("")
  cli::cli_alert_info("Get help: vignette(\"getting-started\") or run_tutorial()")

  invisible(TRUE)
}

#' Show quick demonstration
#'
#' @keywords internal
#' @noRd
show_quick_demo <- function() {
  cli::cli_alert_info("Creating a quick demo animation...")

  # Create a simple demo that showcases key features
  demo_scene <- scene("demo", width = 854, height = 480) %>%
    add_text("manimR Demo", position = c(0, 2), size = 64, color = "#4FC1FF") %>%
    fade_in(duration = 0.5) %>%
    pause(0.5) %>%
    add_latex("f(x) = x^2", position = c(-3, 0)) %>%
    write_in(duration = 1) %>%
    add_circle(radius = 0.3, position = c(2, 0), color = "#FF6B6B") %>%
    grow_from_center() %>%
    move_to(c(3, 0), duration = 0.5) %>%
    pause(0.5)

  # Render preview
  preview_file <- preview(demo_scene)

  cli::cli_alert_success("Demo rendered!")
  cli::cli_text("Preview saved to: {.path {preview_file}}")
}

#' User preferences management
#'
#' @description
#' Get or set user preferences for manimR. Preferences persist
#' across R sessions.
#'
#' @param ... Named preferences to set, or names to get
#' @return If getting, returns preference values. If setting, invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Get all preferences
#' manimR_user_prefs()
#'
#' # Get specific preference
#' manimR_user_prefs("theme")
#'
#' # Set preferences
#' manimR_user_prefs(theme = "dark", mode = "developer")
#' }
#'
#' @export
manimR_user_prefs <- function(...) {
  dots <- list(...)

  # Default preferences
  defaults <- list(
    mode = USER_MODE_TUTORIAL,
    theme = "dark",
    font = "sans",
    font_size = 48,
    background_color = "#1a1a1a",
    primary_color = "#4FC1FF",
    secondary_color = "#FF6B6B",
    accent_color = "#50FA7B",
    topics = list(
      algebra = TRUE,
      linear_algebra = TRUE,
      calculus = TRUE,
      graph_theory = FALSE,
      geometry = TRUE,
      statistics = FALSE,
      number_theory = FALSE
    ),
    auto_preview = FALSE,
    default_quality = "medium",
    default_fps = 30,
    show_hints = TRUE,
    developer_mode = FALSE
  )

  # Load saved preferences
  prefs_file <- get_prefs_file_path()
  if (file.exists(prefs_file)) {
    tryCatch({
      saved_prefs <- jsonlite::read_json(prefs_file)
      defaults <- modifyList(defaults, saved_prefs)
    }, error = function(e) NULL)
  }

  if (length(dots) == 0) {
    # Return all preferences
    return(defaults)
  }

  if (is.null(names(dots))) {
    # Get specific preferences
    return(defaults[unlist(dots)])
  }

  # Set preferences
  for (nm in names(dots)) {
    defaults[[nm]] <- dots[[nm]]
    options(setNames(list(dots[[nm]]), paste0("manimR.", nm)))
  }

  # Save preferences
  save_user_prefs(defaults)

  invisible(NULL)
}

#' Get preferences file path
#'
#' @keywords internal
#' @noRd
get_prefs_file_path <- function() {
  config_dir <- tools::R_user_dir("manimR", "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  file.path(config_dir, "user_prefs.json")
}

#' Save user preferences
#'
#' @keywords internal
#' @noRd
save_user_prefs <- function(prefs = NULL) {
  if (is.null(prefs)) {
    prefs <- manimR_user_prefs()
  }

  prefs_file <- get_prefs_file_path()
  jsonlite::write_json(prefs, prefs_file, pretty = TRUE, auto_unbox = TRUE)
}

#' Apply visual theme
#'
#' @description
#' Applies a predefined visual theme to manimR.
#'
#' @param theme_name Name of theme: "academic", "dark", "light", "3b1b", "minimal"
#'
#' @examples
#' \dontrun{
#' apply_theme("3b1b")  # 3Blue1Brown style
#' apply_theme("dark")  # Modern dark theme
#' }
#'
#' @export
apply_theme <- function(theme_name = c("dark", "academic", "light", "3b1b", "minimal")) {
  theme_name <- match.arg(theme_name)

  themes <- list(
    academic = list(
      background_color = "#FFFFFF",
      primary_color = "#2E5C8A",
      secondary_color = "#8B4513",
      accent_color = "#228B22",
      font = "serif",
      font_size = 48
    ),
    dark = list(
      background_color = "#1a1a1a",
      primary_color = "#4FC1FF",
      secondary_color = "#FF6B6B",
      accent_color = "#50FA7B",
      font = "sans",
      font_size = 48
    ),
    light = list(
      background_color = "#F5F5F5",
      primary_color = "#0066CC",
      secondary_color = "#CC3300",
      accent_color = "#009933",
      font = "sans",
      font_size = 48
    ),
    `3b1b` = list(
      background_color = "#0C0C0C",
      primary_color = "#4FC1FF",
      secondary_color = "#FF6B6B",
      accent_color = "#FF9F43",
      font = "sans",
      font_size = 48
    ),
    minimal = list(
      background_color = "#FFFFFF",
      primary_color = "#000000",
      secondary_color = "#666666",
      accent_color = "#999999",
      font = "sans",
      font_size = 48
    )
  )

  theme <- themes[[theme_name]]

  manimR_user_prefs(
    theme = theme_name,
    background_color = theme$background_color,
    primary_color = theme$primary_color,
    secondary_color = theme$secondary_color,
    accent_color = theme$accent_color,
    font = theme$font,
    font_size = theme$font_size
  )

  cli::cli_alert_success("Applied theme: {.val {theme_name}}")
  invisible(theme)
}

#' Run interactive tutorial
#'
#' @description
#' Launches an interactive tutorial covering manimR basics
#' and selected math topics.
#'
#' @param topic Specific topic to start with (optional)
#'
#' @examples
#' \dontrun{
#' run_tutorial()  # Full tutorial menu
#' run_tutorial("algebra")  # Start with algebra
#' }
#'
#' @export
run_tutorial <- function(topic = NULL) {
  if (is.null(topic)) {
    # Show tutorial menu
    cli::cli_h1("manimR Interactive Tutorials")
    cli::cli_text("Choose a topic to explore through visualization:")
    cli::cli_text("")

    choices <- c(
      "Package Basics - Creating your first animation",
      "Algebra - Visualizing equations and functions",
      "Linear Algebra - Matrices, vectors, and transformations",
      "Calculus - Limits, derivatives, and integrals",
      "Graph Theory - Networks and algorithms",
      "Geometry - Shapes and spatial reasoning",
      "Statistics - Distributions and inference",
      "Exit Tutorial"
    )

    selection <- utils::menu(title = "Select a tutorial:", choices = choices)

    topic <- switch(selection,
      "basics",
      "algebra",
      "linear_algebra",
      "calculus",
      "graph_theory",
      "geometry",
      "statistics",
      return(invisible(NULL))
    )
  }

  # Launch appropriate tutorial
  switch(topic,
    basics = tutorial_basics(),
    algebra = tutorial_algebra(),
    linear_algebra = tutorial_linear_algebra(),
    calculus = tutorial_calculus(),
    graph_theory = tutorial_graph_theory(),
    geometry = tutorial_geometry(),
    statistics = tutorial_statistics(),
    cli::cli_alert_warning("Unknown tutorial topic: {.val {topic}}")
  )

  invisible(TRUE)
}

#' Tutorial: Package basics
#'
#' @keywords internal
#' @noRd
tutorial_basics <- function() {
  cli::cli_h1("Tutorial: Package Basics")

  cli::cli_h2("1. Creating a Scene")
  cli::cli_text("A scene is the canvas for your animation.")
  cli::cli_code(
    "scene(\"my_animation\")"
  )
  cli::cli_text("")
  cli::cli_text("This creates a 1920x1080 scene at 30fps.")
  cli::cli_text("Press Enter to see it in action...")
  readline()

  # Show example
  demo <- scene("tutorial_basics", width = 854, height = 480) %>%
    add_text("Hello, World!", color = manimR_user_prefs("primary_color"))

  preview(demo)
  cli::cli_alert_success("Scene created and previewed!")
  cli::cli_text("")

  cli::cli_h2("2. Adding Animations")
  cli::cli_text("Chain animations using the pipe operator (%>%)")
  cli::cli_code(
    "scene(\"demo\") %>%",
    "  add_text(\"Watch me appear!\") %>%",
    "  fade_in(duration = 1) %>%",
    "  pause(0.5) %>%",
    "  shift(c(2, 0), duration = 1)"
  )
  cli::cli_text("")
  cli::cli_text("Press Enter to continue...")
  readline()

  cli::cli_h2("3. Rendering")
  cli::cli_text("Export your animation to video:")
  cli::cli_code(
    "scene(\"demo\") %>%",
    "  add_text(\"Done!\") %>%",
    "  write_in() %>%",
    "  render(format = \"mp4\", quality = \"high\")"
  )

  cli::cli_h2("4. Next Steps")
  cli::cli_text("Try these tutorials next:")
  cli::cli_ul(
    "run_tutorial(\"algebra\") - Visualize equations",
    "run_tutorial(\"linear_algebra\") - See matrix operations",
    "vignette(\"getting-started\") - Read the full guide"
  )

  invisible(TRUE)
}

#' Get current user mode
#'
#' @description
#' Returns the current user mode (tutorial, developer, or expert).
#'
#' @return Character string indicating mode
#'
#' @export
get_user_mode <- function() {
  manimR_user_prefs("mode")[[1]]
}

#' Set user mode
#'
#' @description
#' Switch between tutorial, developer, and expert modes.
#'
#' @param mode One of "tutorial", "developer", "expert"
#'
#' @export
set_user_mode <- function(mode = c("tutorial", "developer", "expert")) {
  mode <- match.arg(mode)
  manimR_user_prefs(mode = mode)

  if (mode == USER_MODE_DEVELOPER) {
    manimR_user_prefs(developer_mode = TRUE, show_hints = TRUE)
    cli::cli_alert_success("Developer mode activated")
    cli::cli_alert_info("Debug tools enabled. Use debug_scene() for diagnostics.")
  } else if (mode == USER_MODE_TUTORIAL) {
    manimR_user_prefs(developer_mode = FALSE, show_hints = TRUE)
    cli::cli_alert_success("Tutorial mode activated")
    cli::cli_alert_info("Hints and educational content enabled.")
  } else {
    manimR_user_prefs(developer_mode = FALSE, show_hints = FALSE)
    cli::cli_alert_success("Expert mode activated")
  }

  invisible(mode)
}

#' Show contextual hint
#'
#' @description
#' Displays a helpful hint based on current user mode.
#' Only shows in tutorial or developer mode.
#'
#' @param message Hint message to display
#' @param code Optional code example
#'
#' @keywords internal
#' @noRd
show_hint <- function(message, code = NULL) {
  prefs <- manimR_user_prefs()

  if (!prefs$show_hints) {
    return(invisible(NULL))
  }

  cli::cli_alert_info(message)

  if (!is.null(code)) {
    cli::cli_code(code)
  }

  invisible(TRUE)
}

#' Reset onboarding
#'
#' @description
#' Resets onboarding state so the wizard runs again on next load.
#' Useful for starting fresh or after updates.
#'
#' @export
reset_onboarding <- function() {
  options(manimR.onboarding_complete = FALSE)
  prefs_file <- get_prefs_file_path()
  if (file.exists(prefs_file)) {
    file.remove(prefs_file)
  }
  cli::cli_alert_success("Onboarding reset. Restart R to run wizard again.")
  invisible(TRUE)
}
