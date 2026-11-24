#' @title LaTeX Compiler
#' @description 
#' Functions to compile LaTeX strings into bitmap images for rendering.
#' @name latex_compiler
NULL

#' Check for LaTeX tools
#' @keywords internal
check_latex_deps <- function() {
  has_pdflatex <- Sys.which("pdflatex") != ""
  has_magick <- requireNamespace("magick", quietly = TRUE)
  
  if (!has_pdflatex) {
    cli::cli_warn("pdflatex not found. LaTeX rendering will fall back to text.")
    return(FALSE)
  }
  if (!has_magick) {
    cli::cli_warn("magick package not installed. LaTeX rendering requires magick.")
    return(FALSE)
  }
  TRUE
}

#' Compile LaTeX to SVG (Vector)
#' 
#' @param latex_str LaTeX string (without delimiters)
#' @param color Color hex string
#' 
#' @return Path to generated SVG file, or NULL if failed
#' @keywords internal
compile_latex_svg <- function(latex_str, color = "#FFFFFF") {
  if (!check_latex_deps()) return(NULL)
  
  # Check for dvisvgm
  if (Sys.which("dvisvgm") == "") {
    cli::cli_warn("dvisvgm not found. Cannot compile to vector SVG.")
    return(NULL)
  }
  
  hash <- digest::digest(list(latex_str, color, "svg"))
  cache_dir <- file.path(tempdir(), "manimr_latex_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir)
  
  out_svg <- file.path(cache_dir, paste0(hash, ".svg"))
  if (file.exists(out_svg)) return(out_svg)
  
  # Create tex file
  tex_file <- tempfile(fileext = ".tex")
  
  # Manim-like preamble for DVI output
  # dvisvgm needs DVI or XDV. pdflatex produces PDF. 
  # latex produces DVI.
  # We should use `latex` (not pdflatex) to get DVI, then `dvisvgm`.
  # Or `dvisvgm --pdf` if supported (often requires ghostscript).
  # Standard route: latex -> dvi -> dvisvgm.
  
  template <- r"(
\documentclass[preview]{standalone}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{dsfont}
\usepackage{xcolor}
\begin{document}
\color[HTML]{%s}
$%s$
\end{document}
  )"
  
  hex_color <- sub("#", "", color)
  cat(sprintf(template, hex_color, latex_str), file = tex_file)
  
  dvi_file <- replace_ext_with(tex_file, ".dvi")
  
  # Run latex (to get DVI)
  # If 'latex' command missing, try 'dvilualatex' or 'xelatex -no-pdf'? 
  # Usually 'latex' is available if 'pdflatex' is.
  res <- system2("latex", 
                 args = c("-interaction=nonstopmode", 
                          "-halt-on-error",
                          paste0("-output-directory=", dirname(tex_file)),
                          tex_file),
                 stdout = FALSE, 
                 stderr = FALSE)
                 
  if (res != 0 || !file.exists(dvi_file)) {
    # Fallback: maybe they only have pdflatex? 
    # Try pdflatex -> pdf -> dvisvgm --pdf (needs generic PDF support)
    cli::cli_warn("LaTeX (DVI) compilation failed. Trying PDF route...")
    
    # Try compile_latex (PDF) then convert? 
    # Let's stick to DVI logic first. 
    return(NULL)
  }
  
  # Run dvisvgm
  # -n: no fonts (convert text to paths) -> CRITICAL for vector portability
  expected_svg <- replace_ext_with(dvi_file, ".svg")
  
  res_svg <- system2("dvisvgm",
                     args = c("--no-fonts", # Convert text to paths
                              paste0("--output=", expected_svg), # Explicit absolute path
                              dvi_file),    # Input file
                     stdout = TRUE,
                     stderr = TRUE)
                     
  if (!file.exists(expected_svg)) {
    cli::cli_warn("dvisvgm failed. Output: {paste(res_svg, collapse = '\n')}")
    return(NULL)
  }
  
  file.copy(expected_svg, out_svg)
  unlink(c(tex_file, dvi_file, replace_ext_with(tex_file, ".log"), replace_ext_with(tex_file, ".aux"), expected_svg))
  
  return(out_svg)
}


#' Helper to replace extension
#' @keywords internal
replace_ext_with <- function(path, ext) {
  tools::file_path_sans_ext(path) %>% paste0(ext)
}
