#' @title Text and Markup Classes
#' @description
#' Text rendering classes for manimR. Supports plain text, bold/italic,
#' multi-line, font selection via systemfonts, and LaTeX math via the
#' existing latex compiler pipeline.
#' @name text
#' @keywords internal
NULL

#' Text R6 Class
#'
#' @description
#' A VMobject that renders text as Bezier outlines. Uses systemfonts for
#' font discovery and textshaping for glyph layout. Each glyph becomes a
#' submobject (VMobject) so individual letters can be animated.
#'
#' @details
#' Unlike Python Manim which uses Cairo/Pango, manimR uses R-native
#' systemfonts + textshaping for cross-platform font support. When these
#' packages are unavailable, falls back to a simple bounding-box placeholder.
#'
#' @export
Text <- R6::R6Class(
  "Text",
  inherit = VMobject,

  public = list(
    #' @field text The text string
    text = NULL,

    #' @field font Font family name
    font = "sans",

    #' @field font_size Font size in manimR units (default 48 ~ DEFAULT_FONT_SIZE)
    font_size = DEFAULT_FONT_SIZE,

    #' @field weight Font weight ("normal", "bold")
    weight = "normal",

    #' @field slant Font slant ("normal", "italic", "oblique")
    slant = "normal",

    #' @field line_spacing Multiplier for line spacing
    line_spacing = 1.2,

    #' @field alignment Text alignment ("left", "center", "right")
    alignment = "center",

    #' @field gradient Optional color gradient across glyphs
    gradient = NULL,

    #' @field glyphs List of per-glyph VMobjects (submobjects)
    glyphs = NULL,

    #' @description Create a new Text object
    #' @param text The text string to render
    #' @param font Font family (default "sans")
    #' @param font_size Size in manimR units (default DEFAULT_FONT_SIZE)
    #' @param color Text color
    #' @param weight Font weight
    #' @param slant Font slant
    #' @param line_spacing Line spacing multiplier
    #' @param alignment Text alignment
    #' @param gradient Optional color gradient (vector of colors)
    #' @param position Position in scene
    #' @param ... Additional args passed to VMobject
    initialize = function(text,
                          font = "sans",
                          font_size = DEFAULT_FONT_SIZE,
                          color = WHITE,
                          weight = "normal",
                          slant = "normal",
                          line_spacing = 1.2,
                          alignment = "center",
                          gradient = NULL,
                          position = ORIGIN,
                          ...) {
      super$initialize(
        position = position,
        color = color,
        stroke_width = 0,
        fill_color = color,
        fill_opacity = 1,
        ...
      )
      self$text <- text
      self$font <- font
      self$font_size <- font_size
      self$weight <- weight
      self$slant <- slant
      self$line_spacing <- line_spacing
      self$alignment <- alignment
      self$gradient <- gradient
      self$type <- "text"

      private$build_text()
    },

    #' @description Get text as string
    get_text = function() {
      self$text
    },

    #' @description Set new text (rebuilds geometry)
    #' @param new_text New text string
    set_text = function(new_text) {
      self$text <- new_text
      self$submobjects <- list()
      self$points <- matrix(numeric(0), ncol = 3)
      private$build_text()
      invisible(self)
    },

    #' @description Get the i-th glyph (1-indexed) as a VMobject
    #' @param i Index of the glyph
    get_glyph = function(i) {
      if (i < 1 || i > length(self$submobjects)) {
        stop("Glyph index out of range")
      }
      self$submobjects[[i]]
    },

    #' @description Slice a range of glyphs (returns a VGroup)
    #' @param from Start index (1-indexed)
    #' @param to End index (1-indexed, inclusive)
    slice = function(from, to) {
      to <- min(to, length(self$submobjects))
      from <- max(from, 1L)
      grp <- VGroup$new()
      for (i in seq(from, to)) {
        grp$add(self$submobjects[[i]]$copy())
      }
      grp
    },

    #' @description Apply a color gradient across all glyphs
    #' @param colors Vector of color hex strings
    set_color_by_gradient = function(colors) {
      n <- length(self$submobjects)
      if (n == 0) return(invisible(self))
      grad <- color_gradient(colors, n)
      for (i in seq_len(n)) {
        self$submobjects[[i]]$set_fill(grad[i], opacity = self$fill_opacity)
        self$submobjects[[i]]$color <- grad[i]
      }
      invisible(self)
    },

    #' @description Get number of visible glyphs (excluding spaces)
    num_glyphs = function() {
      length(self$submobjects)
    }
  ),

  private = list(
    # Build text geometry from the string
    build_text = function() {
      if (is.null(self$text) || nchar(self$text) == 0) return()

      # Scale: font_size 48 ≈ 0.5 scene units height for a capital letter
      scale_factor <- self$font_size / 48.0 * 0.5

      has_systemfonts <- requireNamespace("systemfonts", quietly = TRUE)
      has_textshaping <- requireNamespace("textshaping", quietly = TRUE)

      if (has_systemfonts && has_textshaping) {
        private$build_with_shaping(scale_factor)
      } else {
        private$build_fallback(scale_factor)
      }

      # Apply gradient if specified
      if (!is.null(self$gradient) && length(self$gradient) >= 2) {
        self$set_color_by_gradient(self$gradient)
      }
    },

    # Build text using systemfonts + textshaping for accurate glyph metrics
    build_with_shaping = function(scale_factor) {
      lines <- strsplit(self$text, "\n", fixed = TRUE)[[1]]
      if (length(lines) == 0) return()

      # Resolve font
      font_spec <- systemfonts::match_font(
        family = self$font,
        bold = (self$weight == "bold"),
        italic = (self$slant != "normal")
      )

      # Get font metrics for line height
      fi <- systemfonts::font_info(
        family = self$font,
        weight = self$weight,
        italic = (self$slant != "normal"),
        size = 48  # reference size
      )
      ascent <- fi$max_ascend[1]
      descent <- abs(fi$max_descend[1])
      line_height_pts <- (ascent + descent) * self$line_spacing

      # Process each line
      all_glyphs <- list()
      line_widths <- numeric(length(lines))
      y_offset <- 0

      for (li in seq_along(lines)) {
        line <- lines[li]
        if (nchar(line) == 0) {
          y_offset <- y_offset - line_height_pts * scale_factor / 48
          next
        }

        # Shape the text to get glyph positions
        shaped <- textshaping::shape_text(
          strings = line,
          family = self$font,
          italic = (self$slant != "normal"),
          weight = if (self$weight == "bold") "bold" else "normal",
          size = 48
        )

        # shaped returns a data frame with columns: glyph, x_offset, y_offset, etc.
        glyph_info <- shaped$shape
        if (is.null(glyph_info) || nrow(glyph_info) == 0) {
          y_offset <- y_offset - line_height_pts * scale_factor / 48
          next
        }

        line_width <- max(glyph_info$x_offset + glyph_info$advance, na.rm = TRUE)
        line_widths[li] <- line_width * scale_factor / 48

        # Create a glyph VMobject for each character (excluding spaces)
        chars <- strsplit(line, "")[[1]]
        for (gi in seq_len(nrow(glyph_info))) {
          # glyph rows correspond 1:1 to characters in the string
          char_idx <- gi
          if (char_idx > length(chars)) next
          ch <- chars[char_idx]
          if (ch == " ") next  # Skip space glyphs

          glyph_x <- glyph_info$x_offset[gi] * scale_factor / 48
          glyph_y <- y_offset

          # Create a simple rectangle placeholder per glyph
          # (Full outline extraction would require freetype2 bindings)
          glyph_w <- glyph_info$advance[gi] * scale_factor / 48
          glyph_h <- ascent * scale_factor / 48

          glyph_mob <- private$make_glyph_rect(
            ch, glyph_x, glyph_y, glyph_w, glyph_h
          )
          all_glyphs[[length(all_glyphs) + 1]] <- glyph_mob
        }

        y_offset <- y_offset - line_height_pts * scale_factor / 48
      }

      # Center alignment
      max_width <- if (length(line_widths) > 0) max(line_widths) else 0

      # Add all glyphs as submobjects
      for (g in all_glyphs) {
        g$color <- self$color
        g$fill_color <- self$fill_color
        g$fill_opacity <- self$fill_opacity
        g$stroke_width <- self$stroke_width
        self$add_submobject(g)
      }

      # Store composite bounding box as self$points
      private$update_bounding_points()
    },

    # Fallback when systemfonts/textshaping not available:
    # Approximate each character as a small rectangle
    build_fallback = function(scale_factor) {
      lines <- strsplit(self$text, "\n", fixed = TRUE)[[1]]
      if (length(lines) == 0) return()

      char_width <- 0.3 * scale_factor
      char_height <- 0.5 * scale_factor
      line_height <- char_height * self$line_spacing
      y_offset <- 0

      for (li in seq_along(lines)) {
        line <- lines[li]
        chars <- strsplit(line, "")[[1]]

        # Calculate line width for alignment
        line_width <- length(chars) * char_width

        x_start <- switch(self$alignment,
          "left"   = 0,
          "center" = -line_width / 2,
          "right"  = -line_width,
          -line_width / 2
        )

        for (ci in seq_along(chars)) {
          ch <- chars[ci]
          if (ch == " ") next

          x_pos <- x_start + (ci - 1) * char_width
          glyph_mob <- private$make_glyph_rect(
            ch, x_pos, y_offset, char_width * 0.8, char_height
          )
          glyph_mob$color <- self$color
          glyph_mob$fill_color <- self$fill_color
          glyph_mob$fill_opacity <- self$fill_opacity
          glyph_mob$stroke_width <- self$stroke_width
          self$add_submobject(glyph_mob)
        }

        y_offset <- y_offset - line_height
      }

      private$update_bounding_points()
    },

    # Create a small rectangle VMobject representing one glyph
    make_glyph_rect = function(char, x, y, w, h) {
      vm <- VMobject$new(
        fill_color = self$fill_color %||% self$color,
        fill_opacity = self$fill_opacity,
        stroke_width = 0
      )
      vm$data <- list(char = char)

      # Rectangle as 4 cubic Bezier curves (degenerate lines)
      x1 <- x
      x2 <- x + w
      y1 <- y
      y2 <- y + h

      corners <- list(
        c(x1, y1, 0), c(x2, y1, 0),
        c(x2, y2, 0), c(x1, y2, 0)
      )

      pts <- matrix(numeric(0), ncol = 3)
      for (i in 1:4) {
        p0 <- corners[[i]]
        p3 <- corners[[(i %% 4) + 1]]
        p1 <- p0 + (p3 - p0) / 3
        p2 <- p0 + 2 * (p3 - p0) / 3
        pts <- rbind(pts, p0, p1, p2, p3)
      }
      vm$points <- pts
      vm
    },

    # Update self$points to be the bounding box of all submobjects
    update_bounding_points = function() {
      if (length(self$submobjects) == 0) return()

      all_pts <- do.call(rbind, lapply(self$submobjects, function(s) {
        if (!is.null(s$points) && nrow(s$points) > 0) s$points
        else NULL
      }))

      if (is.null(all_pts) || nrow(all_pts) == 0) return()

      mins <- apply(all_pts, 2, min)
      maxs <- apply(all_pts, 2, max)

      # Store bounding box as a single curve (4 points)
      self$points <- rbind(mins, mins, maxs, maxs)
    }
  )
)


#' MarkupText R6 Class
#'
#' @description
#' Text with Pango-style markup support. Allows inline bold, italic, color,
#' and size changes using a simple tag syntax.
#'
#' @details
#' Supported tags:
#' - `<b>bold</b>`
#' - `<i>italic</i>`
#' - `<color hex="#FF0000">red text</color>`
#' - `<size val="72">big text</size>`
#'
#' @export
MarkupText <- R6::R6Class(
  "MarkupText",
  inherit = Text,

  public = list(
    #' @field original_text The raw markup string
    original_text = NULL,

    #' @description Create MarkupText
    #' @param text Markup text string
    #' @param ... Arguments passed to Text
    initialize = function(text, ...) {
      self$original_text <- text
      # Strip tags for plain text rendering, preserve for metadata
      plain <- gsub("<[^>]+>", "", text)
      super$initialize(text = plain, ...)
    }
  )
)


#' Title R6 Class
#'
#' @description
#' A large, centered title text. Convenience wrapper around Text with
#' increased font_size and bold weight positioned at the top of the screen.
#'
#' @export
Title <- R6::R6Class(
  "Title",
  inherit = Text,

  public = list(
    #' @description Create a Title
    #' @param text Title text
    #' @param font_size Font size (default 72)
    #' @param color Color
    #' @param ... Additional args
    initialize = function(text,
                          font_size = 72,
                          color = WHITE,
                          ...) {
      super$initialize(
        text = text,
        font_size = font_size,
        color = color,
        weight = "bold",
        position = c(0, 3, 0),  # Near top of frame
        ...
      )
    }
  )
)


#' BulletedList R6 Class
#'
#' @description
#' A vertically arranged list of Text items with bullet markers.
#'
#' @export
BulletedList <- R6::R6Class(
  "BulletedList",
  inherit = VMobject,

  public = list(
    #' @field items Character vector of list items
    items = NULL,

    #' @field bullet Character used as bullet
    bullet = "\u2022",

    #' @description Create a BulletedList
    #' @param ... Text strings for each bullet item
    #' @param font_size Font size
    #' @param color Color
    #' @param buff Vertical spacing between items
    #' @param bullet Bullet character
    initialize = function(...,
                          font_size = DEFAULT_FONT_SIZE,
                          color = WHITE,
                          buff = MED_LARGE_BUFF,
                          bullet = "\u2022") {
      super$initialize(color = color)
      self$items <- list(...)
      self$bullet <- bullet
      self$type <- "bulleted_list"

      y_pos <- 0
      for (item in self$items) {
        txt <- Text$new(
          text = paste(bullet, item),
          font_size = font_size,
          color = color,
          alignment = "left",
          position = c(-3, y_pos, 0)
        )
        self$add_submobject(txt)
        y_pos <- y_pos - buff
      }
    },

    #' @description Fade in items one by one (returns list of FadeIn animations)
    fade_in_sequentially = function() {
      lapply(self$submobjects, function(sub) {
        FadeIn$new(sub)
      })
    }
  )
)


#' Paragraph R6 Class
#'
#' @description
#' Multi-line text that wraps at a specified width. Similar to Text but
#' with automatic word-wrapping.
#'
#' @export
Paragraph <- R6::R6Class(
  "Paragraph",
  inherit = Text,

  public = list(
    #' @field line_width Maximum line width in scene units
    line_width = 6,

    #' @description Create a Paragraph

    #' @param text The paragraph text (will be wrapped)
    #' @param line_width Maximum width before wrapping (scene units)
    #' @param ... Arguments passed to Text
    initialize = function(text, line_width = 6, ...) {
      self$line_width <- line_width

      # Estimate characters per line based on font size
      args <- list(...)
      fs <- if (!is.null(args$font_size)) args$font_size else DEFAULT_FONT_SIZE
      scale <- fs / 48.0 * 0.5
      chars_per_line <- max(floor(line_width / (0.3 * scale)), 10)

      # Word wrap
      wrapped <- private$word_wrap(text, chars_per_line)
      super$initialize(text = wrapped, ...)
    }
  ),

  private = list(
    word_wrap = function(text, max_chars) {
      words <- strsplit(text, "\\s+")[[1]]
      lines <- character(0)
      current_line <- ""

      for (word in words) {
        test <- if (nchar(current_line) == 0) word
                else paste(current_line, word)
        if (nchar(test) > max_chars && nchar(current_line) > 0) {
          lines <- c(lines, current_line)
          current_line <- word
        } else {
          current_line <- test
        }
      }
      if (nchar(current_line) > 0) {
        lines <- c(lines, current_line)
      }

      paste(lines, collapse = "\n")
    }
  )
)


#' MathTex R6 Class
#'
#' @description
#' Renders LaTeX math expressions as VMobject Bezier curves. Uses the
#' existing compile_latex_svg + import_svg_file pipeline to convert LaTeX
#' to SVG paths, then parses those into submobjects.
#'
#' @export
MathTex <- R6::R6Class(
  "MathTex",
  inherit = VMobject,

  public = list(
    #' @field tex_string The LaTeX expression
    tex_string = NULL,

    #' @field tex_template LaTeX preamble template name
    tex_template = "default",

    #' @description Create a MathTex object
    #' @param tex_string LaTeX math string (without $ delimiters)
    #' @param color Color
    #' @param font_size Font size
    #' @param position Position
    #' @param ... Additional args
    initialize = function(tex_string,
                          color = WHITE,
                          font_size = DEFAULT_FONT_SIZE,
                          position = ORIGIN,
                          ...) {
      super$initialize(
        position = position,
        color = color,
        stroke_width = 0,
        fill_color = color,
        fill_opacity = 1,
        ...
      )
      self$tex_string <- tex_string
      self$type <- "mathtex"

      private$build_from_latex(font_size)
    },

    #' @description Get the i-th submobject (e.g. individual glyph path)
    #' @param i Index
    get_part = function(i) {
      if (i < 1 || i > length(self$submobjects)) {
        stop("Part index out of range")
      }
      self$submobjects[[i]]
    },

    #' @description Get parts matching specific indices (for TransformMatchingShapes)
    #' @param indices Integer vector of part indices
    get_parts_by_indices = function(indices) {
      grp <- VGroup$new()
      for (i in indices) {
        if (i >= 1 && i <= length(self$submobjects)) {
          grp$add(self$submobjects[[i]]$copy())
        }
      }
      grp
    }
  ),

  private = list(
    build_from_latex = function(font_size) {
      # Try to compile LaTeX to SVG
      svg_path <- tryCatch(
        compile_latex_svg(self$tex_string, self$color),
        error = function(e) NULL
      )

      if (!is.null(svg_path) && file.exists(svg_path)) {
        # Parse SVG into point matrices
        shapes <- tryCatch(
          import_svg_file(svg_path),
          error = function(e) NULL
        )

        if (!is.null(shapes) && length(shapes) > 0) {
          private$build_from_svg_shapes(shapes, font_size)
          return()
        }
      }

      # Fallback: create a placeholder rectangle with the TeX string stored
      private$build_fallback(font_size)
    },

    build_from_svg_shapes = function(shapes, font_size) {
      # Scale from SVG coordinates to manimR scene units
      # dvisvgm typically outputs at 72dpi with bp units
      # We want font_size 48 ≈ 0.5 scene-unit height
      scale <- font_size / 48.0 * 0.5

      # Collect all points to find bounding box for centering
      all_pts <- do.call(rbind, shapes)
      if (nrow(all_pts) == 0) return()

      center_x <- mean(range(all_pts[, 1]))
      center_y <- mean(range(all_pts[, 2]))

      # SVG y-axis is inverted relative to scene
      for (shape_pts in shapes) {
        vm <- VMobject$new(
          fill_color = self$fill_color %||% self$color,
          fill_opacity = self$fill_opacity,
          stroke_width = 0
        )

        # Center and scale the points, flip Y
        centered <- shape_pts
        centered[, 1] <- (centered[, 1] - center_x) * scale / 10
        centered[, 2] <- -(centered[, 2] - center_y) * scale / 10  # flip Y

        vm$points <- centered
        vm$color <- self$color
        self$add_submobject(vm)
      }

      # Update bounding points
      all_sub_pts <- do.call(rbind, lapply(self$submobjects, function(s) s$points))
      if (!is.null(all_sub_pts) && nrow(all_sub_pts) > 0) {
        mins <- apply(all_sub_pts, 2, min)
        maxs <- apply(all_sub_pts, 2, max)
        self$points <- rbind(mins, mins, maxs, maxs)
      }
    },

    build_fallback = function(font_size) {
      # Placeholder: a rectangle with stored tex_string
      scale <- font_size / 48.0 * 0.5
      w <- nchar(self$tex_string) * 0.2 * scale
      h <- 0.5 * scale

      pts <- rbind(
        c(-w/2, -h/2, 0), c(w/6 - w/2, -h/2, 0),
        c(w/3 - w/2, -h/2, 0), c(w/2, -h/2, 0),
        c(w/2, -h/6, 0), c(w/2, h/6, 0), c(w/2, h/2, 0),
        c(w/3, h/2, 0), c(-w/6, h/2, 0), c(-w/2, h/2, 0),
        c(-w/2, h/6, 0), c(-w/2, -h/6, 0), c(-w/2, -h/2, 0),
        c(-w/2, -h/2, 0), c(-w/2, -h/2, 0), c(-w/2, -h/2, 0)
      )

      self$points <- pts
      self$data <- list(tex_string = self$tex_string, is_fallback = TRUE)
    }
  )
)


#' Tex R6 Class
#'
#' @description
#' Full LaTeX document rendering (not just math mode). Wraps the
#' expression in a full LaTeX document environment.
#'
#' @export
Tex <- R6::R6Class(
  "Tex",
  inherit = MathTex,

  public = list(
    #' @description Create a Tex object (full LaTeX mode)
    #' @param tex_string LaTeX string (can include text-mode commands)
    #' @param ... Arguments passed to MathTex
    initialize = function(tex_string, ...) {
      # Wrap in text mode if not already math
      if (!grepl("\\$", tex_string) && !grepl("\\\\begin", tex_string)) {
        tex_string <- paste0("\\text{", tex_string, "}")
      }
      super$initialize(tex_string = tex_string, ...)
    }
  )
)


#' `%||%` Null coalesce operator (internal)
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
