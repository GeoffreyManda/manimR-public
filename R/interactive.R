#' @title Interactive Output
#' @description
#' Integration with Shiny and htmlwidgets for interactive animation
#' viewing and parameter exploration. Provides a Shiny module for
#' live animation playback and an htmlwidget for embedding.
#' @name interactive
#' @keywords internal
NULL

#' Create an htmlwidget for a manimR animation
#'
#' @description
#' Converts a rendered animation (GIF or frames) into an htmlwidget
#' that can be embedded in Shiny apps, R Markdown documents, and
#' Jupyter notebooks.
#'
#' @param scene A Scene object (will be rendered to GIF)
#' @param width Widget width
#' @param height Widget height
#' @param fps Playback FPS
#' @param controls Show play/pause controls
#' @param loop Loop the animation
#' @param autoplay Start playing automatically
#' @return An htmlwidget object
#' @export
manim_widget <- function(scene,
                          width = "100%",
                          height = "400px",
                          fps = 30,
                          controls = TRUE,
                          loop = TRUE,
                          autoplay = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("htmlwidgets package required for manim_widget()")
  }

  # Render to GIF in temp
  gif_path <- tempfile(fileext = ".gif")
  render(scene, output = gif_path, format = "gif",
         quality = "medium", fps = fps, progress = FALSE)

  # Read GIF and encode as base64
  gif_raw <- readBin(gif_path, "raw", file.info(gif_path)$size)
  gif_b64 <- base64enc::base64encode(gif_raw)

  # Create widget data
  widget_data <- list(
    gif_data = gif_b64,
    controls = controls,
    loop = loop,
    autoplay = autoplay,
    fps = fps
  )

  htmlwidgets::createWidget(
    name = "manimR",
    x = widget_data,
    width = width,
    height = height,
    package = "manimR"
  )
}


#' Shiny output for manimR animation
#'
#' @description
#' Creates a Shiny output element for displaying manimR animations.
#' Use with `renderManimR()` on the server side.
#'
#' @param outputId Shiny output ID
#' @param width Width of the output
#' @param height Height of the output
#' @return A Shiny UI element
#' @export
manimROutput <- function(outputId, width = "100%", height = "400px") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package required for manimROutput()")
  }

  shiny::tags$div(
    id = outputId,
    class = "manimr-output",
    style = sprintf("width: %s; height: %s; background: #000;", width, height),
    shiny::tags$img(
      id = paste0(outputId, "-img"),
      style = "max-width: 100%; max-height: 100%; display: block; margin: auto;"
    )
  )
}


#' Render manimR animation for Shiny
#'
#' @description
#' Server-side function for rendering manimR scenes in Shiny.
#' Automatically re-renders when reactive inputs change.
#'
#' @param expr An expression that returns a Scene object
#' @param env Environment for evaluation
#' @param quoted Whether expr is quoted
#' @param fps Render FPS
#' @param quality Render quality
#' @return A Shiny render function
#' @export
renderManimR <- function(expr, env = parent.frame(), quoted = FALSE, fps = 15,
                          quality = "low") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package required for renderManimR()")
  }

  if (!quoted) expr <- substitute(expr)

  shiny::renderImage({
    scene <- eval(expr, envir = env)

    gif_path <- tempfile(fileext = ".gif")
    render(scene, output = gif_path, format = "gif",
           quality = quality, fps = fps, progress = FALSE)

    list(
      src = gif_path,
      contentType = "image/gif",
      alt = "manimR animation"
    )
  }, deleteFile = TRUE)
}


#' Shiny app for interactive animation exploration
#'
#' @description
#' Launches a Shiny application for interactively exploring and
#' tweaking animation parameters. Provides sliders for common
#' parameters and live preview.
#'
#' @param scene_func A function that takes parameters and returns a Scene
#' @param params Named list of parameter specifications:
#'   each element is a list with 'min', 'max', 'value', 'step', 'label'
#' @param title App title
#' @return Launches a Shiny app (does not return)
#' @export
manim_explorer <- function(scene_func,
                            params = list(),
                            title = "manimR Explorer") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package required for manim_explorer()")
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        lapply(names(params), function(nm) {
          p <- params[[nm]]
          shiny::sliderInput(
            inputId = nm,
            label = p$label %||% nm,
            min = p$min %||% 0,
            max = p$max %||% 10,
            value = p$value %||% 1,
            step = p$step %||% 0.1
          )
        }),
        shiny::actionButton("render_btn", "Render", class = "btn-primary"),
        shiny::hr(),
        shiny::selectInput("quality", "Quality",
                           choices = c("preview", "low", "medium"),
                           selected = "preview"),
        shiny::numericInput("fps", "FPS", value = 15, min = 5, max = 60)
      ),
      shiny::mainPanel(
        width = 9,
        manimROutput("animation", height = "500px"),
        shiny::verbatimTextOutput("info")
      )
    )
  )

  server <- function(input, output, session) {
    scene_reactive <- shiny::eventReactive(input$render_btn, {
      # Collect parameter values
      param_values <- list()
      for (nm in names(params)) {
        param_values[[nm]] <- input[[nm]]
      }

      # Call scene function with parameters
      do.call(scene_func, param_values)
    })

    output$animation <- renderManimR({
      scene_reactive()
    }, fps = shiny::reactive(input$fps)(),
       quality = shiny::reactive(input$quality)())

    output$info <- shiny::renderText({
      sc <- scene_reactive()
      sprintf("Duration: %.2fs | Objects: %d | Animations: %d",
              sc$get_duration(),
              length(sc$objects),
              length(sc$timeline))
    })
  }

  shiny::shinyApp(ui, server)
}


#' Export animation as HTML page
#'
#' @description
#' Renders a scene and creates a standalone HTML page with the
#' animation embedded as a GIF. Useful for sharing without
#' requiring R or video players.
#'
#' @param scene A Scene object
#' @param output_path Output HTML file path
#' @param title Page title
#' @param fps Animation FPS
#' @param quality Render quality
#' @param background Page background color
#' @return Path to the HTML file (invisibly)
#' @export
export_html <- function(scene,
                         output_path = "animation.html",
                         title = "manimR Animation",
                         fps = 30,
                         quality = "medium",
                         background = "#000000") {
  # Render to GIF
  gif_path <- tempfile(fileext = ".gif")
  render(scene, output = gif_path, format = "gif",
         quality = quality, fps = fps, progress = TRUE)

  # Read and encode
  gif_raw <- readBin(gif_path, "raw", file.info(gif_path)$size)
  if (requireNamespace("base64enc", quietly = TRUE)) {
    gif_b64 <- base64enc::base64encode(gif_raw)
  } else {
    gif_b64 <- base64encode_simple(gif_raw)
  }

  # Create HTML
  html <- sprintf('<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>%s</title>
  <style>
    body { margin: 0; padding: 20px; background: %s; display: flex;
           justify-content: center; align-items: center; min-height: 100vh; }
    img { max-width: 100%%; border-radius: 8px; box-shadow: 0 4px 20px rgba(0,0,0,0.5); }
    .container { text-align: center; }
    h1 { color: #fff; font-family: sans-serif; margin-bottom: 20px; }
    .footer { color: #666; font-size: 12px; margin-top: 20px; font-family: sans-serif; }
  </style>
</head>
<body>
  <div class="container">
    <h1>%s</h1>
    <img src="data:image/gif;base64,%s" alt="manimR animation" />
    <p class="footer">Created with manimR</p>
  </div>
</body>
</html>', title, background, title, gif_b64)

  writeLines(html, output_path)

  if (manimR_options("verbose")) {
    cli::cli_alert_success("Exported to {.path {output_path}}")
  }

  invisible(output_path)
}


#' Simple base64 encoder (fallback when base64enc not available)
#' @keywords internal
base64encode_simple <- function(raw_data) {
  # Use built-in base64 encoding via jsonlite if available
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::base64_enc(raw_data)
  } else {
    # Very basic implementation
    warning("base64enc or jsonlite needed for base64 encoding")
    ""
  }
}
