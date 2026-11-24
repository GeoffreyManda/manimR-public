#' @title Easing Functions (Rate Functions)
#' @description
#' 49 easing functions for controlling animation timing, matching and
#' surpassing Python Manim's rate function library. Each function maps
#' t in \[0,1\] to an output in \[0,1\] (approximately).
#' @name easing
#' @keywords internal
NULL

# ---- Core Easing Functions ----

#' @keywords internal
#' @noRd
ease_linear <- function(t) t

#' @keywords internal
#' @noRd
ease_smooth <- function(t) {
  # Hermite smoothstep: t^2 * (3 - 2t)
  t * t * (3 - 2 * t)
}

#' @keywords internal
#' @noRd
ease_double_smooth <- function(t) {
  if (t < 0.5) {
    ease_smooth(2 * t) / 2
  } else {
    (ease_smooth(2 * t - 1) + 1) / 2
  }
}

#' @keywords internal
#' @noRd
ease_smoothstep <- function(t) ease_smooth(t)

#' @keywords internal
#' @noRd
ease_smootherstep <- function(t) {
  # Ken Perlin's smoother step: 6t^5 - 15t^4 + 10t^3
  t * t * t * (t * (t * 6 - 15) + 10)
}

#' @keywords internal
#' @noRd
ease_smoothererstep <- function(t) {
  # Even smoother: -20t^7 + 70t^6 - 84t^5 + 35t^4
  t * t * t * t * (35 + t * (-84 + t * (70 - 20 * t)))
}

# ---- Quadratic ----

#' @keywords internal
#' @noRd
ease_in_quad <- function(t) t * t

#' @keywords internal
#' @noRd
ease_out_quad <- function(t) 1 - (1 - t) * (1 - t)

#' @keywords internal
#' @noRd
ease_in_out_quad <- function(t) {
  if (t < 0.5) 2 * t * t else 1 - (-2 * t + 2)^2 / 2
}

# ---- Cubic ----

#' @keywords internal
#' @noRd
ease_in_cubic <- function(t) t * t * t

#' @keywords internal
#' @noRd
ease_out_cubic <- function(t) 1 - (1 - t)^3

#' @keywords internal
#' @noRd
ease_in_out_cubic <- function(t) {
  if (t < 0.5) 4 * t^3 else 1 - (-2 * t + 2)^3 / 2
}

# ---- Quartic ----

#' @keywords internal
#' @noRd
ease_in_quart <- function(t) t^4

#' @keywords internal
#' @noRd
ease_out_quart <- function(t) 1 - (1 - t)^4

#' @keywords internal
#' @noRd
ease_in_out_quart <- function(t) {
  if (t < 0.5) 8 * t^4 else 1 - (-2 * t + 2)^4 / 2
}

# ---- Quintic ----

#' @keywords internal
#' @noRd
ease_in_quint <- function(t) t^5

#' @keywords internal
#' @noRd
ease_out_quint <- function(t) 1 - (1 - t)^5

#' @keywords internal
#' @noRd
ease_in_out_quint <- function(t) {
  if (t < 0.5) 16 * t^5 else 1 - (-2 * t + 2)^5 / 2
}

# ---- Sinusoidal ----

#' @keywords internal
#' @noRd
ease_in_sine <- function(t) 1 - cos(t * pi / 2)

#' @keywords internal
#' @noRd
ease_out_sine <- function(t) sin(t * pi / 2)

#' @keywords internal
#' @noRd
ease_in_out_sine <- function(t) -(cos(pi * t) - 1) / 2

# ---- Exponential ----

#' @keywords internal
#' @noRd
ease_in_expo <- function(t) if (t == 0) 0 else 2^(10 * t - 10)

#' @keywords internal
#' @noRd
ease_out_expo <- function(t) if (t == 1) 1 else 1 - 2^(-10 * t)

#' @keywords internal
#' @noRd
ease_in_out_expo <- function(t) {
  if (t == 0) return(0)
  if (t == 1) return(1)
  if (t < 0.5) 2^(20 * t - 10) / 2 else (2 - 2^(-20 * t + 10)) / 2
}

# ---- Circular ----

#' @keywords internal
#' @noRd
ease_in_circ <- function(t) 1 - sqrt(1 - t * t)

#' @keywords internal
#' @noRd
ease_out_circ <- function(t) sqrt(1 - (t - 1)^2)

#' @keywords internal
#' @noRd
ease_in_out_circ <- function(t) {
  if (t < 0.5) (1 - sqrt(1 - (2 * t)^2)) / 2
  else (sqrt(1 - (-2 * t + 2)^2) + 1) / 2
}

# ---- Back ----

#' @keywords internal
#' @noRd
ease_in_back <- function(t) {
  c1 <- 1.70158
  c3 <- c1 + 1
  c3 * t^3 - c1 * t^2
}

#' @keywords internal
#' @noRd
ease_out_back <- function(t) {
  c1 <- 1.70158
  c3 <- c1 + 1
  1 + c3 * (t - 1)^3 + c1 * (t - 1)^2
}

#' @keywords internal
#' @noRd
ease_in_out_back <- function(t) {
  c1 <- 1.70158
  c2 <- c1 * 1.525
  if (t < 0.5) {
    ((2 * t)^2 * ((c2 + 1) * 2 * t - c2)) / 2
  } else {
    ((2 * t - 2)^2 * ((c2 + 1) * (t * 2 - 2) + c2) + 2) / 2
  }
}

# ---- Elastic ----

#' @keywords internal
#' @noRd
ease_in_elastic <- function(t) {
  if (t == 0) return(0)
  if (t == 1) return(1)
  c4 <- (2 * pi) / 3
  -(2^(10 * t - 10)) * sin((t * 10 - 10.75) * c4)
}

#' @keywords internal
#' @noRd
ease_out_elastic <- function(t) {
  if (t == 0) return(0)
  if (t == 1) return(1)
  c4 <- (2 * pi) / 3
  2^(-10 * t) * sin((t * 10 - 0.75) * c4) + 1
}

#' @keywords internal
#' @noRd
ease_in_out_elastic <- function(t) {
  if (t == 0) return(0)
  if (t == 1) return(1)
  c5 <- (2 * pi) / 4.5
  if (t < 0.5) {
    -(2^(20 * t - 10) * sin((20 * t - 11.125) * c5)) / 2
  } else {
    (2^(-20 * t + 10) * sin((20 * t - 11.125) * c5)) / 2 + 1
  }
}

# ---- Bounce ----

#' @keywords internal
#' @noRd
ease_out_bounce <- function(t) {
  n1 <- 7.5625
  d1 <- 2.75
  if (t < 1 / d1) {
    n1 * t * t
  } else if (t < 2 / d1) {
    t <- t - 1.5 / d1
    n1 * t * t + 0.75
  } else if (t < 2.5 / d1) {
    t <- t - 2.25 / d1
    n1 * t * t + 0.9375
  } else {
    t <- t - 2.625 / d1
    n1 * t * t + 0.984375
  }
}

#' @keywords internal
#' @noRd
ease_in_bounce <- function(t) 1 - ease_out_bounce(1 - t)

#' @keywords internal
#' @noRd
ease_in_out_bounce <- function(t) {
  if (t < 0.5) (1 - ease_out_bounce(1 - 2 * t)) / 2
  else (1 + ease_out_bounce(2 * t - 1)) / 2
}

# ---- Manim-specific Rate Functions ----

#' @keywords internal
#' @noRd
ease_there_and_back <- function(t) {
  # Goes to 1 at t=0.5, back to 0 at t=1
  if (t < 0.5) ease_smooth(2 * t) else ease_smooth(2 * (1 - t))
}

#' @keywords internal
#' @noRd
ease_there_and_back_with_pause <- function(t) {
  # Goes to 1 at t=1/3, stays at 1, back to 0 at t=1
  if (t < 1/3) ease_smooth(3 * t)
  else if (t < 2/3) 1
  else ease_smooth(3 * (1 - t))
}

#' @keywords internal
#' @noRd
ease_rush_into <- function(t) {
  # Fast start, gentle stop at t=1
  2 * ease_smooth(t / 2)
}

#' @keywords internal
#' @noRd
ease_rush_from <- function(t) {
  # Gentle start, fast end
  2 * ease_smooth(t / 2 + 0.5) - 1
}

#' @keywords internal
#' @noRd
ease_slow_into <- function(t) {
  # Fast start, slow approach to 1
  sqrt(1 - (1 - t)^2)
}

#' @keywords internal
#' @noRd
ease_running_start <- function(t) {
  # Overshoots slightly before returning
  pullback <- 0.1
  if (t < pullback) {
    -ease_smooth(t / pullback) * 0.1
  } else {
    ease_smooth((t - pullback) / (1 - pullback)) * 1.1
  }
}

#' @keywords internal
#' @noRd
ease_wiggle <- function(t) {
  # Wiggles around 0 and returns to 0
  sin(4 * pi * t) * (1 - t)
}

#' @keywords internal
#' @noRd
ease_lingering <- function(t) {
  # Quickly reaches near 1, then slowly approaches 1
  if (t == 0) return(0)
  ease_smooth(t)^(1/3)
}

#' @keywords internal
#' @noRd
ease_not_quite_there <- function(t) {
  # Approaches but never quite reaches 1 (reaches ~0.95)
  ease_smooth(t) * 0.95
}

#' @keywords internal
#' @noRd
ease_exponential_decay <- function(t) {
  1 - exp(-5 * t)
}

# ---- Easing Registry ----

#' Get an easing function by name
#'
#' @description Returns an easing function by its name. Supports 49 named
#'   easing functions plus custom functions. If a C++ backend is available,
#'   uses the optimized vectorized version.
#'
#' @param name Name of the easing function, or a function
#' @return An easing function that maps \[0,1\] -> \[0,1\]
#'
#' @keywords internal
#' @noRd
get_easing <- function(name = "smooth") {
  if (is.function(name)) return(name)

  easings <- list(
    # Standard
    linear           = ease_linear,
    smooth           = ease_smooth,
    double_smooth    = ease_double_smooth,
    smoothstep       = ease_smoothstep,
    smootherstep     = ease_smootherstep,
    smoothererstep   = ease_smoothererstep,

    # Quadratic
    in_quad          = ease_in_quad,
    out_quad         = ease_out_quad,
    in_out_quad      = ease_in_out_quad,
    ease_in_quad     = ease_in_quad,
    ease_out_quad    = ease_out_quad,
    ease_in_out_quad = ease_in_out_quad,

    # Cubic
    in_cubic          = ease_in_cubic,
    out_cubic         = ease_out_cubic,
    in_out_cubic      = ease_in_out_cubic,
    ease_in_cubic     = ease_in_cubic,
    ease_out_cubic    = ease_out_cubic,
    ease_in_out_cubic = ease_in_out_cubic,

    # Quartic
    in_quart          = ease_in_quart,
    out_quart         = ease_out_quart,
    in_out_quart      = ease_in_out_quart,
    ease_in_quart     = ease_in_quart,
    ease_out_quart    = ease_out_quart,
    ease_in_out_quart = ease_in_out_quart,

    # Quintic
    in_quint          = ease_in_quint,
    out_quint         = ease_out_quint,
    in_out_quint      = ease_in_out_quint,
    ease_in_quint     = ease_in_quint,
    ease_out_quint    = ease_out_quint,
    ease_in_out_quint = ease_in_out_quint,

    # Sinusoidal
    in_sine          = ease_in_sine,
    out_sine         = ease_out_sine,
    in_out_sine      = ease_in_out_sine,
    ease_in_sine     = ease_in_sine,
    ease_out_sine    = ease_out_sine,
    ease_in_out_sine = ease_in_out_sine,

    # Exponential
    in_expo          = ease_in_expo,
    out_expo         = ease_out_expo,
    in_out_expo      = ease_in_out_expo,
    ease_in_expo     = ease_in_expo,
    ease_out_expo    = ease_out_expo,
    ease_in_out_expo = ease_in_out_expo,

    # Circular
    in_circ          = ease_in_circ,
    out_circ         = ease_out_circ,
    in_out_circ      = ease_in_out_circ,
    ease_in_circ     = ease_in_circ,
    ease_out_circ    = ease_out_circ,
    ease_in_out_circ = ease_in_out_circ,

    # Back
    in_back          = ease_in_back,
    out_back         = ease_out_back,
    in_out_back      = ease_in_out_back,
    ease_in_back     = ease_in_back,
    ease_out_back    = ease_out_back,
    ease_in_out_back = ease_in_out_back,

    # Elastic
    in_elastic          = ease_in_elastic,
    out_elastic         = ease_out_elastic,
    in_out_elastic      = ease_in_out_elastic,
    ease_in_elastic     = ease_in_elastic,
    ease_out_elastic    = ease_out_elastic,
    ease_in_out_elastic = ease_in_out_elastic,

    # Bounce
    in_bounce          = ease_in_bounce,
    out_bounce         = ease_out_bounce,
    in_out_bounce      = ease_in_out_bounce,
    ease_in_bounce     = ease_in_bounce,
    ease_out_bounce    = ease_out_bounce,
    ease_in_out_bounce = ease_in_out_bounce,

    # Manim-specific
    there_and_back             = ease_there_and_back,
    there_and_back_with_pause  = ease_there_and_back_with_pause,
    rush_into                  = ease_rush_into,
    rush_from                  = ease_rush_from,
    slow_into                  = ease_slow_into,
    running_start              = ease_running_start,
    wiggle                     = ease_wiggle,
    lingering                  = ease_lingering,
    not_quite_there            = ease_not_quite_there,
    exponential_decay          = ease_exponential_decay,

    # Legacy aliases
    ease_in   = ease_in_quad,
    ease_out  = ease_out_quad
  )

  if (!name %in% names(easings)) {
    cli::cli_warn("Unknown easing {.val {name}}, using {.val smooth}")
    name <- "smooth"
  }

  easings[[name]]
}

#' Apply easing to a vector of time values
#'
#' @description Vectorized application of an easing function. Uses C++
#'   backend when available for performance.
#'
#' @param t Numeric vector of values in \[0,1\]
#' @param easing Name of easing function or a function
#' @return Numeric vector of eased values
#'
#' @keywords internal
#' @noRd
apply_easing <- function(t, easing = "smooth") {
  # Try C++ backend first for known functions
  if (is.character(easing) && has_compiled_backend()) {
    tryCatch(
      return(cpp_easing_vectorized(t, easing)),
      error = function(e) NULL
    )
  }

  # Fallback to R implementation
  ease_fn <- get_easing(easing)
  vapply(t, ease_fn, numeric(1))
}

#' List all available easing functions
#'
#' @description Returns a character vector of all available easing function
#'   names.
#'
#' @return Character vector of easing names
#' @export
list_easings <- function() {
  c(
    "linear", "smooth", "double_smooth", "smoothstep", "smootherstep",
    "smoothererstep",
    paste0("ease_in_", c("quad", "cubic", "quart", "quint", "sine",
                          "expo", "circ", "back", "elastic", "bounce")),
    paste0("ease_out_", c("quad", "cubic", "quart", "quint", "sine",
                           "expo", "circ", "back", "elastic", "bounce")),
    paste0("ease_in_out_", c("quad", "cubic", "quart", "quint", "sine",
                              "expo", "circ", "back", "elastic", "bounce")),
    "there_and_back", "there_and_back_with_pause", "rush_into", "rush_from",
    "slow_into", "running_start", "wiggle", "lingering", "not_quite_there",
    "exponential_decay"
  )
}
