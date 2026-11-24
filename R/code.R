#' @title Code Display Class
#' @description
#' Syntax-highlighted code block as a VMobject. Supports line numbers,
#' background panel, and per-token color theming.
#' @name code
#' @keywords internal
NULL

#' Code R6 Class
#'
#' @description
#' Displays source code with syntax highlighting. Each line is a Text
#' submobject. Tokens are colored according to a theme (default: monokai).
#' Includes an optional background rectangle and line numbers.
#'
#' @details
#' Syntax highlighting uses simple regex-based token classification.
#' For R, Python, and JavaScript, common patterns (keywords, strings,
#' comments, numbers) are recognized and colored.
#'
#' @export
Code <- R6::R6Class(

  "Code",
  inherit = VMobject,

  public = list(
    #' @field code_string The source code string
    code_string = NULL,

    #' @field language Programming language ("r", "python", "javascript", "plain")
    language = "r",

    #' @field theme Color theme name
    theme = "monokai",

    #' @field tab_width Number of spaces per tab
    tab_width = 4,

    #' @field font Monospace font family
    font = "mono",

    #' @field font_size Font size
    font_size = 24,

    #' @field line_numbers Whether to show line numbers
    line_numbers = TRUE,

    #' @field background_color Background panel color
    background_color = "#272822",

    #' @field background_opacity Background panel opacity
    background_opacity = 0.9,

    #' @field corner_radius Background corner radius
    corner_radius = 0.2,

    #' @field line_objects List of Text objects, one per line
    line_objects = NULL,

    #' @field background Background rectangle VMobject
    background = NULL,

    #' @description Create a Code block
    #' @param code_string Source code string
    #' @param language Language for highlighting
    #' @param theme Color theme
    #' @param font Monospace font
    #' @param font_size Font size
    #' @param line_numbers Show line numbers
    #' @param tab_width Tab width in spaces
    #' @param position Position
    #' @param ... Additional args
    initialize = function(code_string,
                          language = "r",
                          theme = "monokai",
                          font = "mono",
                          font_size = 24,
                          line_numbers = TRUE,
                          tab_width = 4,
                          position = ORIGIN,
                          ...) {
      super$initialize(position = position, ...)
      self$code_string <- code_string
      self$language <- tolower(language)
      self$theme <- theme
      self$font <- font
      self$font_size <- font_size
      self$line_numbers <- line_numbers
      self$tab_width <- tab_width
      self$type <- "code"

      private$build_code()
    },

    #' @description Highlight specific lines (set their opacity to 1, dim others)
    #' @param line_indices Integer vector of line numbers to highlight (1-indexed)
    #' @param dim_opacity Opacity for non-highlighted lines
    highlight_lines = function(line_indices, dim_opacity = 0.3) {
      for (i in seq_along(self$line_objects)) {
        if (i %in% line_indices) {
          self$line_objects[[i]]$set_opacity(1)
        } else {
          self$line_objects[[i]]$set_opacity(dim_opacity)
        }
      }
      invisible(self)
    },

    #' @description Get a specific line object
    #' @param n Line number (1-indexed)
    get_line = function(n) {
      if (n < 1 || n > length(self$line_objects)) stop("Line index out of range")
      self$line_objects[[n]]
    },

    #' @description Get the number of lines
    num_lines = function() {
      length(self$line_objects)
    }
  ),

  private = list(
    build_code = function() {
      # Replace tabs with spaces
      code <- gsub("\t", strrep(" ", self$tab_width), self$code_string)
      lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
      if (length(lines) == 0) return()

      theme_colors <- private$get_theme(self$theme)
      scale <- self$font_size / 48.0 * 0.5
      char_w <- 0.26 * scale
      line_h <- 0.5 * scale * 1.3
      padding <- 0.3

      # Calculate dimensions
      max_chars <- max(nchar(lines))
      num_prefix_chars <- if (self$line_numbers) nchar(as.character(length(lines))) + 2 else 0
      total_width <- (max_chars + num_prefix_chars) * char_w + 2 * padding
      total_height <- length(lines) * line_h + 2 * padding

      # Background rectangle
      bg <- RoundedRectangle$new(
        width = total_width,
        height = total_height,
        corner_radius = self$corner_radius,
        fill_color = self$background_color,
        fill_opacity = self$background_opacity,
        stroke_width = 0
      )
      bg$z_index <- -1
      self$add_submobject(bg)
      self$background <- bg

      # Build each line
      self$line_objects <- list()
      x_start <- -total_width / 2 + padding
      y_start <- total_height / 2 - padding - line_h / 2

      for (i in seq_along(lines)) {
        # Line number prefix
        prefix <- ""
        if (self$line_numbers) {
          num_str <- formatC(i, width = num_prefix_chars - 1, flag = " ")
          prefix <- paste0(num_str, " ")
        }

        line_text <- paste0(prefix, lines[i])
        y_pos <- y_start - (i - 1) * line_h

        # Determine line color (simple token coloring)
        line_color <- private$colorize_line(lines[i], theme_colors)

        txt <- Text$new(
          text = line_text,
          font = self$font,
          font_size = self$font_size,
          color = line_color,
          alignment = "left",
          position = c(x_start, y_pos, 0)
        )
        self$line_objects[[i]] <- txt
        self$add_submobject(txt)
      }
    },

    # Simple line-level colorization (returns the dominant color for the line)
    colorize_line = function(line, theme) {
      trimmed <- trimws(line)
      if (nchar(trimmed) == 0) return(theme$text)

      # Comment detection
      comment_patterns <- list(
        r = "^#",
        python = "^#",
        javascript = "^//|^/\\*",
        c = "^//|^/\\*",
        cpp = "^//|^/\\*",
        plain = NULL
      )
      pat <- comment_patterns[[self$language]]
      if (!is.null(pat) && grepl(pat, trimmed)) {
        return(theme$comment)
      }

      # String detection
      if (grepl('^["\']', trimmed) || grepl('["\']\\s*$', trimmed)) {
        return(theme$string)
      }

      # Keyword detection
      kw_patterns <- list(
        r = "^(if|else|for|while|repeat|function|return|library|require|TRUE|FALSE|NULL|NA|in|next|break)\\b",
        python = paste0(
          "^(def|class|if|elif|else|for|while|return|import|from|True|False|None|with|as|",
          "try|except|raise|yield|lambda|in|not|and|or|is|pass|break|continue)\\b"
        ),
        javascript = paste0(
          "^(function|const|let|var|if|else|for|while|return|class|new|this|true|false|",
          "null|undefined|import|export|async|await|try|catch|throw)\\b"
        ),
        plain = NULL
      )

      kw_pat <- kw_patterns[[self$language]]
      if (!is.null(kw_pat)) {
        # Check first word of trimmed line
        first_word <- sub("^(\\w+).*", "\\1", trimmed)
        if (grepl(kw_pat, first_word)) {
          return(theme$keyword)
        }
      }

      # Function call detection
      if (grepl("\\w+\\s*\\(", trimmed)) {
        return(theme$func)
      }

      # Number detection
      if (grepl("^[0-9]", trimmed)) {
        return(theme$number)
      }

      theme$text
    },

    get_theme = function(name) {
      themes <- list(
        monokai = list(
          text     = "#F8F8F2",
          keyword  = "#F92672",
          string   = "#E6DB74",
          comment  = "#75715E",
          func     = "#A6E22E",
          number   = "#AE81FF",
          operator = "#F92672"
        ),
        dracula = list(
          text     = "#F8F8F2",
          keyword  = "#FF79C6",
          string   = "#F1FA8C",
          comment  = "#6272A4",
          func     = "#50FA7B",
          number   = "#BD93F9",
          operator = "#FF79C6"
        ),
        solarized = list(
          text     = "#839496",
          keyword  = "#859900",
          string   = "#2AA198",
          comment  = "#586E75",
          func     = "#268BD2",
          number   = "#D33682",
          operator = "#859900"
        ),
        github = list(
          text     = "#24292E",
          keyword  = "#D73A49",
          string   = "#032F62",
          comment  = "#6A737D",
          func     = "#6F42C1",
          number   = "#005CC5",
          operator = "#D73A49"
        )
      )
      if (name %in% names(themes)) themes[[name]]
      else themes[["monokai"]]
    }
  )
)
