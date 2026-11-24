#' @title Backend Selection and Dispatch
#' @description
#' Functions for selecting and configuring the computational backend (C++, Rust,
#' or pure R) for manimR operations. The package automatically selects the best
#' available backend, but users can override this choice.
#'
#' @name backend
NULL

# Global backend state
.backend_state <- new.env(parent = emptyenv())

# Backend preference constants
BACKEND_CPP <- "cpp"
BACKEND_RUST <- "rust"
BACKEND_R <- "r"

#' Initialize backend system
#'
#' @description
#' Called on package load to detect available backends and set defaults.
#'
#' @keywords internal
#' @noRd
initialize_backends <- function() {
  # Detect available backends
  cpp_available <- .detect_cpp_backend()
  rust_available <- .detect_rust_backend()

  .backend_state$cpp_available <- cpp_available
  .backend_state$rust_available <- rust_available
  .backend_state$initialized <- TRUE

  # Determine default backend priority: cpp > rust > r
  user_pref <- getOption("manimR.backend", NULL)

  if (!is.null(user_pref)) {
    # User has specified a preference
    selected <- switch(user_pref,
      "cpp" = if (cpp_available) BACKEND_CPP else .find_fallback(),
      "rust" = if (rust_available) BACKEND_RUST else .find_fallback(),
      "r" = BACKEND_R,
      "auto" = .select_best_backend(),
      .select_best_backend()
    )
  } else {
    selected <- .select_best_backend()
  }

  .backend_state$current_backend <- selected

  if (manimR_options("verbose")) {
    cli::cli_alert_info("Using backend: {.val {selected}}")
    if (selected == BACKEND_R) {
      cli::cli_alert_warning("No compiled backend available. Install Rcpp for C++ support.")
    }
  }

  invisible(selected)
}

#' Detect C++ backend availability
#'
#' @return TRUE if C++ backend is available
#' @keywords internal
#' @noRd
.detect_cpp_backend <- function() {
  # Check if Rcpp module is loadable
  if (!requireNamespace("Rcpp", quietly = TRUE)) {
    return(FALSE)
  }

  # Check if the manimR shared library is actually loaded
  tryCatch({
    # getLoadedDLLs() returns DLLs that are currently loaded in the R session
    loaded_dlls <- getLoadedDLLs()
    if ("manimR" %in% names(loaded_dlls)) {
      return(TRUE)
    }

    # If not yet loaded, check if the shared library exists on disk
    # (it may be loaded later when useDynLib kicks in)
    pkg_path <- system.file("libs", package = "manimR")
    if (dir.exists(pkg_path)) {
      libs <- list.files(pkg_path, pattern = "\\.(so|dll|dylib)$", full.names = TRUE)
      if (length(libs) > 0) {
        # Verify a .Call symbol is actually available
        result <- tryCatch({
          .Call("_manimR_cpp_smoothstep", 0.0, 1.0, 0.5, PACKAGE = "manimR")
          TRUE
        }, error = function(e) FALSE)
        return(result)
      }
    }

    FALSE
  }, error = function(e) FALSE)
}

#' Detect Rust backend availability
#'
#' @return TRUE if Rust backend is available
#' @keywords internal
#' @noRd
.detect_rust_backend <- function() {
  # Check if extendr module is available
  if (!requireNamespace("rextendr", quietly = TRUE)) {
    return(FALSE)
  }

  tryCatch({
    # Try to source the Rust-generated wrapper
    rust_wrapper <- system.file("rust", "manimR_rust.R", package = "manimR")
    if (file.exists(rust_wrapper)) {
      # Verify an actual Rust function is callable
      if (exists("rust_easing_vectorized", mode = "function")) {
        rust_easing_vectorized(0.5, "smooth")
        return(TRUE)
      }
    }

    # Check for Rust shared library
    pkg_path <- system.file("libs", package = "manimR")
    if (dir.exists(pkg_path)) {
      rust_libs <- list.files(pkg_path, pattern = "rust", full.names = TRUE)
      if (length(rust_libs) > 0 && exists("rust_easing_vectorized", mode = "function")) {
        rust_easing_vectorized(0.5, "smooth")
        return(TRUE)
      }
    }

    FALSE
  }, error = function(e) FALSE)
}

#' Select best available backend
#'
#' @return Backend identifier string
#' @keywords internal
#' @noRd
.select_best_backend <- function() {
  if (.backend_state$cpp_available) {
    return(BACKEND_CPP)
  } else if (.backend_state$rust_available) {
    return(BACKEND_RUST)
  } else {
    return(BACKEND_R)
  }
}

#' Find fallback backend
#'
#' @return Fallback backend identifier
#' @keywords internal
#' @noRd
.find_fallback <- function() {
  if (.backend_state$cpp_available) {
    return(BACKEND_CPP)
  } else if (.backend_state$rust_available) {
    return(BACKEND_RUST)
  } else {
    return(BACKEND_R)
  }
}

#' Get current backend
#'
#' @description
#' Returns the currently active computational backend.
#'
#' @return Character string: "cpp", "rust", or "r"
#'
#' @examples
#' \dontrun{
#' # Check which backend is currently in use
#' get_backend()
#' }
#'
#' @export
get_backend <- function() {
  if (!.backend_state$initialized) {
    initialize_backends()
  }
  .backend_state$current_backend
}

#' List available backends
#'
#' @description
#' Returns a list of all available backends on the current system.
#'
#' @return Named logical vector indicating availability of each backend
#'
#' @examples
#' \dontrun{
#' list_backends()
#' }
#'
#' @export
list_backends <- function() {
  if (!.backend_state$initialized) {
    initialize_backends()
  }

  c(
    cpp = .backend_state$cpp_available,
    rust = .backend_state$rust_available,
    r = TRUE  # Pure R is always available
  )
}

#' Set computational backend
#'
#' @description
#' Sets the computational backend to use for subsequent operations.
#' If the requested backend is not available, falls back to the best
#' available alternative with a warning.
#'
#' @param backend One of "cpp", "rust", "r", or "auto"
#'
#' @return Invisibly returns the selected backend name
#'
#' @examples
#' \dontrun{
#' # Force C++ backend
#' set_backend("cpp")
#'
#' # Use Rust backend
#' set_backend("rust")
#'
#' # Use pure R (guaranteed to work)
#' set_backend("r")
#'
#' # Auto-select best available
#' set_backend("auto")
#' }
#'
#' @export
set_backend <- function(backend = c("auto", "cpp", "rust", "r")) {
  backend <- match.arg(backend)

  if (!.backend_state$initialized) {
    initialize_backends()
  }

  if (backend == "auto") {
    selected <- .select_best_backend()
  } else if (backend == "cpp") {
    if (.backend_state$cpp_available) {
      selected <- BACKEND_CPP
    } else {
      cli::cli_warn("C++ backend not available. Using fallback.")
      selected <- .find_fallback()
    }
  } else if (backend == "rust") {
    if (.backend_state$rust_available) {
      selected <- BACKEND_RUST
    } else {
      cli::cli_warn("Rust backend not available. Using fallback.")
      selected <- .find_fallback()
    }
  } else {
    selected <- BACKEND_R
  }

  old_backend <- .backend_state$current_backend
  .backend_state$current_backend <- selected

  if (manimR_options("verbose") && old_backend != selected) {
    cli::cli_alert_info("Switched backend: {.val {old_backend}} -> {.val {selected}}")
  }

  invisible(selected)
}

#' Check if compiled backend is available
#'
#' @description
#' Returns TRUE if either C++ or Rust backend is available,
#' meaning operations will be accelerated.
#'
#' @return Logical
#'
#' @examples
#' \dontrun{
#' if (has_compiled_backend()) {
#'   message("Using accelerated rendering!")
#' }
#' }
#'
#' @export
has_compiled_backend <- function() {
  if (!.backend_state$initialized) {
    initialize_backends()
  }
  .backend_state$cpp_available || .backend_state$rust_available
}

#===============================================================================
# Backend Dispatch Functions
#===============================================================================

#' Dispatch to appropriate easing function backend
#'
#' @param t Numeric vector of time values
#' @param easing_name Name of easing function
#' @return Numeric vector of eased values
#' @keywords internal
#' @noRd
dispatch_easing <- function(t, easing_name) {
  backend <- get_backend()

  if (backend == BACKEND_CPP) {
    return(cpp_easing_vectorized(t, easing_name))
  } else if (backend == BACKEND_RUST) {
    return(tryCatch(
      rust_easing_vectorized(t, easing_name),
      error = function(e) {
        cli::cli_warn("Rust backend failed for easing, falling back to R: {e$message}")
        r_easing_vectorized(t, easing_name)
      }
    ))
  } else {
    return(r_easing_vectorized(t, easing_name))
  }
}

#' Dispatch frame batch rendering
#'
#' @param scene Scene object
#' @param frame_times Vector of frame times
#' @param width Frame width
#' @param height Frame height
#' @param bg_color Background color
#' @return List of rendered frame data
#' @keywords internal
#' @noRd
dispatch_render_frames <- function(object_data, animation_data, frame_times, width, height, background_color) {
  backend <- get_backend()

  if (backend == BACKEND_CPP) {
    return(cpp_render_frame_batch(object_data, animation_data, frame_times, width, height))
  } else if (backend == BACKEND_RUST) {
    return(tryCatch(
      rust_render_frame_batch(object_data, animation_data, frame_times, width, height),
      error = function(e) {
        cli::cli_warn("Rust backend failed for frame rendering, falling back to R: {e$message}")
        r_render_frame_batch(object_data, animation_data, frame_times, width, height)
      }
    ))
  } else {
    return(r_render_frame_batch(object_data, animation_data, frame_times, width, height))
  }
}

#' Dispatch interpolation
#'
#' @param start Start values
#' @param end End values
#' @param t Interpolation parameter (0-1)
#' @return Interpolated values
#' @keywords internal
#' @noRd
dispatch_lerp <- function(start, end, t) {
  backend <- get_backend()

  if (backend == BACKEND_CPP) {
    return(cpp_lerp_vectorized(start, end, t))
  } else if (backend == BACKEND_RUST) {
    return(tryCatch(
      rust_lerp_vectorized(start, end, t),
      error = function(e) {
        cli::cli_warn("Rust backend failed for lerp, falling back to R: {e$message}")
        start + (end - start) * t
      }
    ))
  } else {
    return(start + (end - start) * t)
  }
}

#' Dispatch color interpolation
#'
#' @param color1 First color (hex)
#' @param color2 Second color (hex)
#' @param t Interpolation parameter (0-1)
#' @return Interpolated color
#' @keywords internal
#' @noRd
dispatch_color_lerp <- function(color1, color2, t) {
  backend <- get_backend()

  if (backend == BACKEND_CPP) {
    return(cpp_color_interpolate(color1, color2, t))
  } else if (backend == BACKEND_RUST) {
    return(tryCatch(
      rust_color_interpolate(color1, color2, t),
      error = function(e) {
        cli::cli_warn("Rust backend failed for color interpolation, falling back to R: {e$message}")
        r_color_interpolate(color1, color2, t)
      }
    ))
  } else {
    return(r_color_interpolate(color1, color2, t))
  }
}

#' Dispatch geometry generation
#'
#' @param shape_type Shape type ("circle", "rectangle", etc.)
#' @param params Shape parameters
#' @return Matrix of vertex coordinates
#' @keywords internal
#' @noRd
dispatch_generate_geometry <- function(shape_type, params) {
  backend <- get_backend()

  if (backend == BACKEND_CPP) {
    return(cpp_generate_geometry(shape_type, params))
  } else if (backend == BACKEND_RUST) {
    return(tryCatch(
      rust_generate_geometry(shape_type, params),
      error = function(e) {
        cli::cli_warn("Rust backend failed for geometry generation, falling back to R: {e$message}")
        r_generate_geometry(shape_type, params)
      }
    ))
  } else {
    return(r_generate_geometry(shape_type, params))
  }
}

#' Dispatch matrix transformation
#'
#' @param vertices Matrix of vertices (2xN or 3xN)
#' @param transform Transformation matrix
#' @return Transformed vertices
#' @keywords internal
#' @noRd
dispatch_transform_vertices <- function(vertices, transform) {
  backend <- get_backend()

  if (backend == BACKEND_CPP) {
    return(cpp_transform_vertices(vertices, transform))
  } else if (backend == BACKEND_RUST) {
    return(tryCatch(
      rust_transform_vertices(vertices, transform),
      error = function(e) {
        cli::cli_warn("Rust backend failed for vertex transform, falling back to R: {e$message}")
        r_transform_vertices(vertices, transform)
      }
    ))
  } else {
    return(r_transform_vertices(vertices, transform))
  }
}

#' Benchmark backends
#'
#' @description
#' Runs a simple benchmark comparing the performance of available backends.
#'
#' @param n_iterations Number of iterations for the benchmark
#'
#' @return A data frame with benchmark results
#'
#' @examples
#' \dontrun{
#' results <- benchmark_backends()
#' print(results)
#' }
#'
#' @export
benchmark_backends <- function(n_iterations = 10000) {
  backends <- list()

  if (.backend_state$cpp_available || .detect_cpp_backend()) {
    backends$cpp <- function() {
      cpp_easing_vectorized(seq(0, 1, length.out = n_iterations), "smooth")
    }
  }

  if (.backend_state$rust_available || .detect_rust_backend()) {
    backends$rust <- function() {
      tryCatch(
        rust_easing_vectorized(seq(0, 1, length.out = n_iterations), "smooth"),
        error = function(e) {
          cli::cli_warn("Rust benchmark failed: {e$message}")
          NA_real_
        }
      )
    }
  }

  backends$r <- function() {
    r_easing_vectorized(seq(0, 1, length.out = n_iterations), "smooth")
  }

  results <- list()

  for (name in names(backends)) {
    # Warmup
    backends[[name]]()

    # Timed run
    start <- Sys.time()
    backends[[name]]()
    elapsed <- as.numeric(Sys.time() - start, units = "secs")

    results[[name]] <- data.frame(
      backend = name,
      iterations = n_iterations,
      time_seconds = elapsed,
      ops_per_second = n_iterations / elapsed
    )
  }

  do.call(rbind, results)
}

#===============================================================================
# Pure R Fallback Implementations
#===============================================================================

#' Vectorized easing in pure R
#'
#' @keywords internal
#' @noRd
r_easing_vectorized <- function(t, easing_name) {
  get_easing(easing_name)(t)
}

#' Pure R frame batch rendering
#'
#' @keywords internal
#' @noRd
r_render_frame_batch <- function(scene, frame_times, width, height, bg_color) {
  # Fall back to the original sequential rendering
  lapply(frame_times, function(time) {
    get_object_states_at_time(scene, time)
  })
}

#' Pure R color interpolation
#'
#' @keywords internal
#' @noRd
r_color_interpolate <- function(color1, color2, t) {
  # Convert hex to RGB
  rgb1 <- grDevices::col2rgb(color1) / 255
  rgb2 <- grDevices::col2rgb(color2) / 255

  # Interpolate
  rgb_interp <- rgb1 + (rgb2 - rgb1) * t

  # Convert back to hex
  grDevices::rgb(rgb_interp[1], rgb_interp[2], rgb_interp[3])
}

#' Pure R geometry generation
#'
#' @keywords internal
#' @noRd
r_generate_geometry <- function(shape_type, params) {
  switch(shape_type,
    circle = {
      n_points <- params$n_points %||% 100
      theta <- seq(0, 2 * pi, length.out = n_points)
      rbind(
        x = params$radius * cos(theta) + params$center[1],
        y = params$radius * sin(theta) + params$center[2]
      )
    },
    rectangle = {
      w <- params$width / 2
      h <- params$height / 2
      cx <- params$center[1]
      cy <- params$center[2]
      rbind(
        x = c(cx - w, cx + w, cx + w, cx - w),
        y = c(cy - h, cy - h, cy + h, cy + h)
      )
    },
    # Default: return empty
    rbind(x = numeric(0), y = numeric(0))
  )
}

#' Pure R vertex transformation
#'
#' @keywords internal
#' @noRd
r_transform_vertices <- function(vertices, transform) {
  # Simple matrix multiplication
  transform %*% rbind(vertices, 1)
}
