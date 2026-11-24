#' SVG Parser for ManimR
#'
#' @description
#' Parses SVG files (specifically from dvisvgm) into Bezier curve points.
#' @name svg_parser
NULL

#' Parse SVG Path Data
#'
#' @description
#' Converts an SVG path string ('d' attribute) into a matrix of cubic Bezier points.
#'
#' @param path_str The SVG path string (e.g., "M 10 10 L 20 20 ...")
#' @return Matrix of points (N x 3)
#' @keywords internal
parse_svg_path_data <- function(path_str) {
  # This uses a simplified parsing strategy adapted for simple paths
  # For full SVG compliance, this needs a more robust state machine
  
  # Normalize spacing
  d <- gsub(",", " ", path_str)
  
  # Handle negative numbers (insert space before - unless part of scientific notation)
  # Lookbehind needs perl=TRUE
  d <- gsub("(?<![eE])\\-", " -", d, perl = TRUE)
  
  d <- gsub("\\s+", " ", d)
  d <- trimws(d)
  
  # Split into tokens
  # We need to preserve command letters
  # Insert space before commands to ensure splitting works
  d <- gsub("([a-zA-Z])", " \\1 ", d)
  
  # Cleanup double spaces again
  d <- gsub("\\s+", " ", d)
  
  tokens <- strsplit(d, "\\s+")[[1]]
  tokens <- tokens[tokens != ""]
  
  points <- matrix(numeric(0), ncol = 3)
  cursor <- c(0, 0)
  current_path_start <- c(0, 0)
  last_control_point <- NULL
  
  i <- 1
  while (i <= length(tokens)) {
    cmd <- tokens[i]
    i <- i + 1
    
    if (!grepl("^[a-zA-Z]$", cmd)) {
      # Should be a command, if number ignore or error?
      # Implicit command repetition not supported in this simple parser yet
      next 
    }
    
    ucmd <- toupper(cmd)
    is_rel <- cmd == tolower(cmd)
    
    if (ucmd == "M") {
      # Move to
      x <- as.numeric(tokens[i])
      y <- as.numeric(tokens[i+1])
      i <- i + 2
      
      pt <- c(x, y)
      if (is_rel) pt <- cursor + pt
      
      cursor <- pt
      current_path_start <- pt
      
      # M command itself doesn't generate points for the curve list
      # but establishes the start for subsequent commands
      
    } else if (ucmd == "L") {
      # Line to (convert to cubic)
      x <- as.numeric(tokens[i])
      y <- as.numeric(tokens[i+1])
      i <- i + 2
      
      pt <- c(x, y)
      if (is_rel) pt <- cursor + pt
      
      # Line from cursor to pt
      # Cubic: P0=cursor, P1=cursor+(pt-cursor)/3, P2=cursor+2*(pt-cursor)/3, P3=pt
      p0 <- cursor
      p3 <- pt
      p1 <- p0 + (p3 - p0) / 3
      p2 <- p0 + 2 * (p3 - p0) / 3
      
      # Append points: P0, P1, P2, P3
      # Note: VMobject optimization handles duplicate P0 if sequence
      new_pts <- rbind(
        c(p0, 0), c(p1, 0), c(p2, 0), c(p3, 0)
      )
      points <- rbind(points, new_pts)
      
      cursor <- pt
      
    } else if (ucmd == "C") {
      # Cubic Bezier
      # Needs 6 args: x1 y1 x2 y2 x y
      
      # Handle repetition?
      # dvisvgm might output multiple sets of coords for one C?
      # Let's assume explicit for now or implement loop
      
      # Keep consuming while next tokens are numbers
      while (i + 5 <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
         x1 <- as.numeric(tokens[i]); y1 <- as.numeric(tokens[i+1])
         x2 <- as.numeric(tokens[i+2]); y2 <- as.numeric(tokens[i+3])
         x  <- as.numeric(tokens[i+4]); y  <- as.numeric(tokens[i+5])
         i <- i + 6
         
         p1 <- c(x1, y1)
         p2 <- c(x2, y2)
         p3 <- c(x, y)
         
         if (is_rel) {
           p1 <- cursor + p1
           p2 <- cursor + p2
           p3 <- cursor + p3
         }
         

         new_pts <- rbind(
           c(cursor, 0), c(p1, 0), c(p2, 0), c(p3, 0)
         )

    # Deduplication Logic
    # If redundant start point (matches last point), skip it.
    
    if (nrow(points) > 0) {
      last_pt <- points[nrow(points), 1:2]
      start_new <- new_pts[1, 1:2]
      
      # Tolerance 1e-6
      if (sum(abs(last_pt - start_new)) < 1e-6) {
        # Skip first point of new_pts
        new_pts <- new_pts[-1, , drop=FALSE]
      }
    }
    
    points <- rbind(points, new_pts)
    cursor <- p3 # Update cursor (was p3)
    }

      

    } else if (ucmd == "S") {
      # Smooth Cubic Bezier
      # Args: x2 y2 x y
      # CP1 is reflection of previous CP2
      
      while (i + 3 <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
        x2 <- as.numeric(tokens[i]); y2 <- as.numeric(tokens[i+1])
        x  <- as.numeric(tokens[i+2]); y  <- as.numeric(tokens[i+3])
        i <- i + 4
        
        p2 <- c(x2, y2)
        p3 <- c(x, y)
        if (is_rel) {
           p2 <- cursor + p2
           p3 <- cursor + p3
        }
        
        # Calculate CP1
        # Need previous command's last control point.
        # We need to track prev_cp2 in the loop state.
        # If prev command was not C or S, CP1 = Cursor
        
        # We need to add state tracking for 'last_control_point'
        # For now, simplistic approximation: If we don't track it, assumes smooth?
        # Actually reflection is critical for font smoothing.
        # Let's verify if we can access last point row.
        
        p0 <- cursor
        
        # Reflection Logic: CP1 = P0 + (P0 - Prev_CP2) = 2*P0 - Prev_CP2
        # We need a 'last_control_point' variable outside the loop.
        # If not set (or prev cmd wasn't bezier), use P0.
        
        if (exists("last_control_point") && !is.null(last_control_point)) {
          p1 <- 2 * p0 - last_control_point
        } else {
          p1 <- p0
        }
        
        new_pts <- rbind(
           c(p0, 0), c(p1, 0), c(p2, 0), c(p3, 0)
        )
        points <- rbind(points, new_pts)
        
        cursor <- p3
        last_control_point <- p2 # Set for next S
        last_cmd_was_bezier <- TRUE
      }
      
    } else if (ucmd == "Q") {
      # Quadratic Bezier
      # x1 y1 x y
      # Convert to cubic
      # CP1 = P0 + 2/3 (Q1-P0)
      # CP2 = P3 + 2/3 (Q1-P3)
      
      while (i + 3 <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
         x1 <- as.numeric(tokens[i]); y1 <- as.numeric(tokens[i+1])
         x  <- as.numeric(tokens[i+2]); y  <- as.numeric(tokens[i+3])
         i <- i + 4
         
         qp1 <- c(x1, y1) # Control point
         p3 <- c(x, y)    # End point
         
         if (is_rel) {
           qp1 <- cursor + qp1
           p3 <- cursor + p3
         }
         
         p0 <- cursor
         
         # Convert quadratic control point to cubic control points
         cp1 <- p0 + (2/3) * (qp1 - p0)
         cp2 <- p3 + (2/3) * (qp1 - p3)
         
         new_pts <- rbind(
           c(p0, 0), c(cp1, 0), c(cp2, 0), c(p3, 0)
         )
         points <- rbind(points, new_pts)
         cursor <- p3
         last_control_point <- qp1 # For Q continuation T command (not impl yet)
      }
      
    } else if (ucmd == "H") {
       # Horizontal Line To
       # argument: x
       x <- as.numeric(tokens[i])
       i <- i + 1
       
       pt <- c(x, cursor[2])
       if (is_rel) pt <- c(cursor[1] + x, cursor[2])
       
       # Convert to Cubic (as Line)
       p0 <- cursor
       p3 <- pt
       p1 <- p0 + (p3 - p0) / 3
       p2 <- p0 + 2 * (p3 - p0) / 3
       
       new_pts <- rbind(
         c(p0, 0), c(p1, 0), c(p2, 0), c(p3, 0)
       )
       points <- rbind(points, new_pts)
       cursor <- pt

    } else if (ucmd == "V") {
       # Vertical Line To
       # argument: y
       y <- as.numeric(tokens[i])
       i <- i + 1
       
       pt <- c(cursor[1], y)
       if (is_rel) pt <- c(cursor[1], cursor[2] + y)
       
       # Convert to Cubic (as Line)
       p0 <- cursor
       p3 <- pt
       p1 <- p0 + (p3 - p0) / 3
       p2 <- p0 + 2 * (p3 - p0) / 3
       
       new_pts <- rbind(
         c(p0, 0), c(p1, 0), c(p2, 0), c(p3, 0)
       )
       points <- rbind(points, new_pts)
       cursor <- pt
       
    } else if (ucmd == "Z") {
      # Close path
      # Line to current_path_start
      if (any(cursor != current_path_start)) {
        p0 <- cursor
        p3 <- current_path_start
        p1 <- p0 + (p3 - p0) / 3
        p2 <- p0 + 2 * (p3 - p0) / 3
        
        new_pts <- rbind(
          c(p0, 0), c(p1, 0), c(p2, 0), c(p3, 0)
        )
        points <- rbind(points, new_pts)
        cursor <- p3
      }
    } else {
      # Unknown command
      cli::cli_warn("Unknown SVG command: {cmd}")
    }
  }
  
  points
}

#' Import SVG file to VMobject points
#'
#' @param filepath Path to SVG file
#' @return List of point matrices (one for each path)
#' @keywords internal
import_svg_file <- function(filepath) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    cli::cli_abort("Resize 'xml2' package is required for SVG import.")
  }
  
  doc <- xml2::read_xml(filepath)
  
  # dvisvgm specific: usually inside <g> tags or top level <path>
  # Let's find all paths
  # SVG namespace usually requires stripping or explicit handling. 
  # xml2 handles default namespace well usually with d1 prefix or stripping.
  
  paths <- xml2::xml_find_all(doc, ".//*[local-name()='path']")
  
  all_shapes <- list()
  
  for (p in paths) {
    d <- xml2::xml_attr(p, "d")
    if (!is.na(d)) {
      pts <- parse_svg_path_data(d)
      if (nrow(pts) > 0) {
        # Transform?
        # dvisvgm uses transform attributes on <use> or <g>. This is complex.
        # Minimal viable product: assume baked transforms or simple paths
        all_shapes[[length(all_shapes) + 1]] <- pts
      }
    }
  }
  
  all_shapes
}
