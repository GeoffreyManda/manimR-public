#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default options if not already set
  op <- options()
  op.manimR <- list(
    manimR.render_quality = "medium",
    manimR.fps = 30,
    manimR.cache = TRUE,
    manimR.verbose = TRUE,
    manimR.background_color = "#000000",
    manimR.default_font = "sans",
    manimR.latex_compiler = "pdflatex",
    manimR.backend = "auto"
  )

  toset <- !(names(op.manimR) %in% names(op))
  if (any(toset)) options(op.manimR[toset])

  # Initialize backend system
  tryCatch({
    initialize_backends()
  }, error = function(e) {
    # Fallback if backend initialization fails
    if (exists(".backend_state", inherits = FALSE)) {
      assign("current_backend", "r", envir = .backend_state)
      assign("cpp_available", FALSE, envir = .backend_state)
      assign("rust_available", FALSE, envir = .backend_state)
      assign("initialized", TRUE, envir = .backend_state)
    }
  })

  invisible()
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  backend <- tryCatch(get_backend(), error = function(e) "r")
  has_compiled <- tryCatch(has_compiled_backend(), error = function(e) FALSE)

  pkg_version <- utils::packageVersion("manimR")

  msg <- paste0(
    "manimR: Mathematical Animation for R\n",
    "Version ", pkg_version, " | Backend: ", backend
  )

  if (!has_compiled) {
    msg <- paste0(
      msg,
      "\n\u2139 Install Rcpp for C++ acceleration"
    )
  }

  packageStartupMessage(msg)

  if (!has_latex()) {
    packageStartupMessage(
      "Note: LaTeX not found. Install LaTeX for full math rendering support."
    )
  }

  if (!has_ffmpeg()) {
    packageStartupMessage(
      "Note: ffmpeg not found. Video export may be limited."
    )
  }
}
