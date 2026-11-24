#' @title Table Classes
#' @description
#' Table visualization for manimR. Displays data in a formatted grid
#' with cell highlighting, row/column selection, and animation support.
#' @name table
#' @keywords internal
NULL

#' Table R6 Class
#'
#' @description
#' A table composed of cells arranged in rows and columns. Each cell
#' contains a Text submobject. The table supports highlighting individual
#' cells, rows, or columns with color, and individual cell animation.
#'
#' @export
Table <- R6::R6Class(
  "Table",
  inherit = VMobject,

  public = list(
    #' @field data Matrix or data.frame of table contents
    data = NULL,

    #' @field col_names Column header names
    col_names = NULL,

    #' @field row_names Row header names
    row_names = NULL,

    #' @field n_rows Number of data rows
    n_rows = 0,

    #' @field n_cols Number of columns
    n_cols = 0,

    #' @field cell_width Width of each cell in scene units
    cell_width = 1.5,

    #' @field cell_height Height of each cell
    cell_height = 0.6,

    #' @field font_size Font size for cell text
    font_size = 24,

    #' @field header_font_size Font size for headers
    header_font_size = 28,

    #' @field line_color Color of grid lines
    line_color = NULL,

    #' @field header_color Header text color
    header_color = NULL,

    #' @field cell_entries Matrix-like list of Text mobjects (row x col)
    cell_entries = NULL,

    #' @field h_lines VGroup of horizontal lines
    h_lines = NULL,

    #' @field v_lines VGroup of vertical lines
    v_lines = NULL,

    #' @field header_entries List of header Text mobjects
    header_entries = NULL,

    #' @description Create a Table
    #' @param data Matrix, data.frame, or list of row vectors
    #' @param col_names Character vector of column names (NULL = auto)
    #' @param row_names Character vector of row names (NULL = none)
    #' @param cell_width Cell width
    #' @param cell_height Cell height
    #' @param font_size Font size for cells
    #' @param header_font_size Font size for headers
    #' @param line_color Grid line color
    #' @param header_color Header text color
    #' @param color Default text color
    #' @param include_outer_lines Include border lines
    #' @param ... Additional args
    initialize = function(data,
                          col_names = NULL,
                          row_names = NULL,
                          cell_width = 1.5,
                          cell_height = 0.6,
                          font_size = 24,
                          header_font_size = 28,
                          line_color = WHITE,
                          header_color = YELLOW,
                          color = WHITE,
                          include_outer_lines = TRUE,
                          ...) {
      super$initialize(color = color, ...)
      self$cell_width <- cell_width
      self$cell_height <- cell_height
      self$font_size <- font_size
      self$header_font_size <- header_font_size
      self$line_color <- line_color
      self$header_color <- header_color
      self$type <- "table"

      # Convert data to matrix of strings
      if (is.data.frame(data)) {
        if (is.null(col_names)) col_names <- names(data)
        self$data <- as.matrix(data)
      } else if (is.list(data) && !is.matrix(data)) {
        self$data <- do.call(rbind, lapply(data, function(row) {
          as.character(row)
        }))
      } else {
        self$data <- as.matrix(data)
      }

      # Convert all entries to character
      self$data <- matrix(as.character(self$data),
                          nrow = nrow(self$data),
                          ncol = ncol(self$data))

      self$n_rows <- nrow(self$data)
      self$n_cols <- ncol(self$data)
      self$col_names <- col_names
      self$row_names <- row_names

      private$build_table(include_outer_lines)
    },

    #' @description Get a cell entry (Text mobject) at row, col (1-indexed)
    #' @param row Row index
    #' @param col Column index
    get_cell = function(row, col) {
      self$cell_entries[[(row - 1) * self$n_cols + col]]
    },

    #' @description Get an entire row of cell entries as a VGroup
    #' @param row Row index (1-indexed)
    get_row = function(row) {
      grp <- VGroup$new()
      for (col in seq_len(self$n_cols)) {
        grp$add(self$get_cell(row, col))
      }
      grp
    },

    #' @description Get an entire column of cell entries as a VGroup
    #' @param col Column index (1-indexed)
    get_column = function(col) {
      grp <- VGroup$new()
      for (row in seq_len(self$n_rows)) {
        grp$add(self$get_cell(row, col))
      }
      grp
    },

    #' @description Highlight a specific cell
    #' @param row Row index
    #' @param col Column index
    #' @param color Highlight color
    highlight_cell = function(row, col, color = YELLOW) {
      cell <- self$get_cell(row, col)
      if (!is.null(cell)) {
        # Add a highlight rectangle behind the cell
        x <- private$col_x(col)
        y <- private$row_y(row)
        rect <- Rectangle$new(
          width = self$cell_width,
          height = self$cell_height,
          fill_color = color,
          fill_opacity = 0.3,
          stroke_width = 0,
          position = c(x, y, 0)
        )
        rect$z_index <- -1
        self$add_submobject(rect)
      }
      invisible(self)
    },

    #' @description Highlight an entire row
    #' @param row Row index
    #' @param color Highlight color
    highlight_row = function(row, color = YELLOW) {
      for (col in seq_len(self$n_cols)) {
        self$highlight_cell(row, col, color)
      }
      invisible(self)
    },

    #' @description Highlight an entire column
    #' @param col Column index
    #' @param color Highlight color
    highlight_column = function(col, color = YELLOW) {
      for (row in seq_len(self$n_rows)) {
        self$highlight_cell(row, col, color)
      }
      invisible(self)
    },

    #' @description Add a horizontal line after a specific row
    #' @param row Row index (line appears below this row)
    #' @param color Line color
    #' @param stroke_width Line width
    add_horizontal_line = function(row, color = NULL, stroke_width = 2) {
      col <- color %||% self$line_color
      y <- private$row_y(row) - self$cell_height / 2
      n_col_offset <- if (!is.null(self$row_names)) 1 else 0
      total_cols <- self$n_cols + n_col_offset
      x_start <- private$col_x(1) - self$cell_width / 2 -
                 n_col_offset * self$cell_width
      x_end <- private$col_x(self$n_cols) + self$cell_width / 2

      line <- Line$new(
        start = c(x_start, y, 0),
        end = c(x_end, y, 0),
        color = col,
        stroke_width = stroke_width
      )
      self$add_submobject(line)
      invisible(self)
    }
  ),

  private = list(
    build_table = function(include_outer_lines) {
      self$cell_entries <- list()
      self$header_entries <- list()

      has_header <- !is.null(self$col_names)
      has_row_names <- !is.null(self$row_names)

      total_rows <- self$n_rows + if (has_header) 1 else 0
      total_cols <- self$n_cols + if (has_row_names) 1 else 0

      # Column headers
      if (has_header) {
        for (ci in seq_len(self$n_cols)) {
          x <- private$col_x(ci)
          y <- private$header_y()
          txt <- Text$new(
            text = self$col_names[ci],
            font_size = self$header_font_size,
            color = self$header_color,
            weight = "bold",
            position = c(x, y, 0)
          )
          self$header_entries[[ci]] <- txt
          self$add_submobject(txt)
        }
      }

      # Row names and data cells
      for (ri in seq_len(self$n_rows)) {
        y <- private$row_y(ri)

        # Row name
        if (has_row_names && ri <= length(self$row_names)) {
          rn_x <- private$col_x(1) - self$cell_width
          txt <- Text$new(
            text = self$row_names[ri],
            font_size = self$font_size,
            color = self$header_color,
            weight = "bold",
            position = c(rn_x, y, 0)
          )
          self$add_submobject(txt)
        }

        # Data cells
        for (ci in seq_len(self$n_cols)) {
          x <- private$col_x(ci)
          txt <- Text$new(
            text = self$data[ri, ci],
            font_size = self$font_size,
            color = self$color,
            position = c(x, y, 0)
          )
          self$cell_entries[[(ri - 1) * self$n_cols + ci]] <- txt
          self$add_submobject(txt)
        }
      }

      # Grid lines
      private$add_grid_lines(include_outer_lines, has_header, has_row_names)
    },

    col_x = function(col) {
      (col - (self$n_cols + 1) / 2) * self$cell_width + self$position[1]
    },

    row_y = function(row) {
      has_header <- !is.null(self$col_names)
      top <- if (has_header) (self$n_rows + 1) / 2 else self$n_rows / 2
      offset <- if (has_header) row else row - 0.5
      (top - offset) * self$cell_height + self$position[2]
    },

    header_y = function() {
      (self$n_rows / 2 + 0.5) * self$cell_height + self$position[2]
    },

    add_grid_lines = function(include_outer, has_header, has_row_names) {
      self$h_lines <- VGroup$new()
      self$v_lines <- VGroup$new()

      n_col_extra <- if (has_row_names) 1 else 0
      total_width <- (self$n_cols + n_col_extra) * self$cell_width
      total_data_rows <- self$n_rows + if (has_header) 1 else 0
      total_height <- total_data_rows * self$cell_height

      x_left <- private$col_x(1) - self$cell_width / 2 -
                n_col_extra * self$cell_width
      x_right <- private$col_x(self$n_cols) + self$cell_width / 2
      y_top <- private$header_y() + self$cell_height / 2
      if (!has_header) y_top <- private$row_y(1) + self$cell_height / 2
      y_bottom <- private$row_y(self$n_rows) - self$cell_height / 2

      # Horizontal lines
      # Line below header
      if (has_header) {
        y <- private$header_y() - self$cell_height / 2
        line <- Line$new(
          start = c(x_left, y, 0), end = c(x_right, y, 0),
          color = self$line_color, stroke_width = 2
        )
        self$h_lines$add(line)
        self$add_submobject(line)
      }

      # Lines between rows
      for (ri in seq_len(self$n_rows - 1)) {
        y <- private$row_y(ri) - self$cell_height / 2
        line <- Line$new(
          start = c(x_left, y, 0), end = c(x_right, y, 0),
          color = self$line_color, stroke_width = 1
        )
        self$h_lines$add(line)
        self$add_submobject(line)
      }

      # Lines between columns
      for (ci in seq_len(self$n_cols - 1)) {
        x <- private$col_x(ci) + self$cell_width / 2
        line <- Line$new(
          start = c(x, y_top, 0), end = c(x, y_bottom, 0),
          color = self$line_color, stroke_width = 1
        )
        self$v_lines$add(line)
        self$add_submobject(line)
      }

      # Outer border
      if (include_outer) {
        # Top
        self$add_submobject(Line$new(
          start = c(x_left, y_top, 0), end = c(x_right, y_top, 0),
          color = self$line_color, stroke_width = 2
        ))
        # Bottom
        self$add_submobject(Line$new(
          start = c(x_left, y_bottom, 0), end = c(x_right, y_bottom, 0),
          color = self$line_color, stroke_width = 2
        ))
        # Left
        self$add_submobject(Line$new(
          start = c(x_left, y_top, 0), end = c(x_left, y_bottom, 0),
          color = self$line_color, stroke_width = 2
        ))
        # Right
        self$add_submobject(Line$new(
          start = c(x_right, y_top, 0), end = c(x_right, y_bottom, 0),
          color = self$line_color, stroke_width = 2
        ))
      }
    }
  )
)


#' MathTable R6 Class
#'
#' @description
#' A table optimized for displaying mathematical content. Cell entries
#' can be MathTex objects instead of plain Text.
#'
#' @export
MathTable <- R6::R6Class(
  "MathTable",
  inherit = Table,

  public = list(
    #' @field use_latex Whether cells should be rendered as LaTeX
    use_latex = TRUE,

    #' @description Create a MathTable
    #' @param data Matrix of LaTeX strings
    #' @param use_latex Render cells as LaTeX (TRUE) or plain text (FALSE)
    #' @param ... Arguments passed to Table
    initialize = function(data, use_latex = TRUE, ...) {
      self$use_latex <- use_latex
      super$initialize(data = data, ...)
    }
  )
)


#' IntegerTable R6 Class
#'
#' @description
#' A convenience table for displaying integer matrices, useful for
#' demonstrating matrix operations and linear algebra.
#'
#' @export
IntegerTable <- R6::R6Class(
  "IntegerTable",
  inherit = Table,

  public = list(
    #' @description Create an IntegerTable from a numeric matrix
    #' @param mat Numeric matrix
    #' @param ... Arguments passed to Table
    initialize = function(mat, ...) {
      str_mat <- matrix(as.character(as.integer(mat)),
                        nrow = nrow(mat), ncol = ncol(mat))
      super$initialize(data = str_mat, ...)
    }
  )
)


#' Matrix display R6 Class
#'
#' @description
#' A matrix displayed with brackets, as commonly seen in linear algebra.
#' Wraps a Table with bracket decorations.
#'
#' @export
MatrixMobject <- R6::R6Class(
  "MatrixMobject",
  inherit = VMobject,

  public = list(
    #' @field mat The numeric matrix
    mat = NULL,

    #' @field table_mob The inner Table
    table_mob = NULL,

    #' @field left_bracket Left bracket VMobject
    left_bracket = NULL,

    #' @field right_bracket Right bracket VMobject
    right_bracket = NULL,

    #' @field bracket_type Bracket style ("square", "round", "curly")
    bracket_type = "square",

    #' @description Create a MatrixMobject
    #' @param mat Numeric matrix
    #' @param bracket_type "square", "round", or "curly"
    #' @param font_size Cell font size
    #' @param color Text color
    #' @param ... Additional args
    initialize = function(mat,
                          bracket_type = "square",
                          font_size = 30,
                          color = WHITE,
                          ...) {
      super$initialize(color = color, ...)
      self$mat <- mat
      self$bracket_type <- bracket_type
      self$type <- "matrix"

      # Create inner table (no grid lines, no headers)
      str_mat <- matrix(as.character(round(mat, 4)),
                        nrow = nrow(mat), ncol = ncol(mat))
      self$table_mob <- Table$new(
        data = str_mat,
        cell_width = 1.0,
        cell_height = 0.5,
        font_size = font_size,
        color = color,
        line_color = "transparent",
        include_outer_lines = FALSE
      )
      self$add_submobject(self$table_mob)

      # Add brackets
      private$add_brackets()
    },

    #' @description Get entry at (i, j) as a mobject
    #' @param i Row index
    #' @param j Column index
    get_entry = function(i, j) {
      self$table_mob$get_cell(i, j)
    },

    #' @description Get row as VGroup
    #' @param i Row index
    get_row = function(i) {
      self$table_mob$get_row(i)
    },

    #' @description Get column as VGroup
    #' @param j Column index
    get_col = function(j) {
      self$table_mob$get_column(j)
    }
  ),

  private = list(
    add_brackets = function() {
      nr <- nrow(self$mat)
      nc <- ncol(self$mat)
      table_h <- nr * self$table_mob$cell_height
      table_w <- nc * self$table_mob$cell_width

      bracket_h <- table_h + 0.3
      x_offset <- table_w / 2 + 0.15

      # Left bracket
      self$left_bracket <- private$make_bracket(
        -x_offset, bracket_h, "left"
      )
      self$add_submobject(self$left_bracket)

      # Right bracket
      self$right_bracket <- private$make_bracket(
        x_offset, bracket_h, "right"
      )
      self$add_submobject(self$right_bracket)
    },

    make_bracket = function(x, height, side) {
      vm <- VMobject$new(color = self$color, stroke_width = 2)
      h2 <- height / 2

      if (self$bracket_type == "square") {
        # Square bracket: three line segments
        inset <- if (side == "left") 0.15 else -0.15
        pts <- rbind(
          c(x + inset, h2, 0), c(x + inset, h2, 0),
          c(x, h2, 0), c(x, h2, 0),
          c(x, h2, 0), c(x, h2, 0),
          c(x, -h2, 0), c(x, -h2, 0),
          c(x, -h2, 0), c(x, -h2, 0),
          c(x + inset, -h2, 0), c(x + inset, -h2, 0)
        )
      } else if (self$bracket_type == "round") {
        # Parenthesis as Bezier curve
        curve_x <- if (side == "left") -0.3 else 0.3
        pts <- rbind(
          c(x, h2, 0),
          c(x + curve_x, h2 * 0.5, 0),
          c(x + curve_x, -h2 * 0.5, 0),
          c(x, -h2, 0)
        )
      } else {
        # Curly brace
        mid_x <- if (side == "left") -0.2 else 0.2
        pts <- rbind(
          c(x, h2, 0), c(x + mid_x/2, h2*0.7, 0),
          c(x + mid_x, h2*0.3, 0), c(x + mid_x, 0, 0),
          c(x + mid_x, 0, 0), c(x + mid_x, -h2*0.3, 0),
          c(x + mid_x/2, -h2*0.7, 0), c(x, -h2, 0)
        )
      }

      vm$points <- pts
      vm
    }
  )
)
