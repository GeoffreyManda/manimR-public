#' @title Showcase Tutorial Suite
#' @description
#' Ten animated tutorials that each stress-test a unique dimension of
#' the manimR feature set.  Run them individually or call
#' \code{run_all_showcase_tutorials()} for the full battery.
#' @name tutorial_showcase
#' @keywords internal
NULL

# =================================================================
#  Helper: safely render a scene and return the path
# =================================================================
.render_tutorial <- function(sc, name, format = "gif", quality = "low",
                             fps = 10) {
  out_dir <- file.path(tempdir(), "manimR_showcase")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, name)

  render(sc,
         output  = out_path,
         format  = format,
         quality = quality,
         fps     = fps)

  final <- paste0(out_path, ".", format)
  if (file.exists(final)) {
    cli::cli_alert_success("{name}: {round(file.info(final)$size/1024,1)} KB")
  }
  invisible(final)
}


# =================================================================
#  1. GEOMETRY SHOWCASE
#  Stress-tests: Circle, Square, Rectangle, RoundedRectangle, Triangle,
#  RegularPolygon, Star, Arc, Sector, Annulus, Ellipse, Line, Arrow,
#  DashedLine, DoubleArrow, Dot, CubicBezier, Angle, RightAngle,
#  VGroup, and Transform between shapes.
# =================================================================

#' @export
tutorial_geometry <- function(format = "gif", quality = "low", fps = 10) {
  sc <- Scene$new(name = "geometry_showcase")

  # Title
  title <- Text$new("Geometry Showcase", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))

  # Row 1: basic shapes
  shapes <- list(
    Circle$new(radius = 0.6, color = BLUE_C),
    Square$new(side_length = 1.1, color = RED_C),
    Triangle$new(radius = 0.7, color = GREEN_C),
    Rectangle$new(width = 1.4, height = 0.8, color = TEAL_C),
    RoundedRectangle$new(width = 1.2, height = 0.8, corner_radius = 0.2,
                         color = ORANGE)
  )
  x_pos <- seq(-4.5, 4.5, length.out = length(shapes))
  for (i in seq_along(shapes)) {
    s <- shapes[[i]]
    s$position <- c(x_pos[i], 1.2, 0)
    s$fill_color <- s$color
    s$fill_opacity <- 0.4
    sc$add_object(s)
    sc$play(GrowFromCenter$new(s, run_time = 0.25))
  }

  # Row 2: arcs, sectors, star, polygon
  row2 <- list(
    Arc$new(radius = 0.6, start_angle = 0, angle = PI * 1.3, color = PURPLE_A),
    Sector$new(radius = 0.6, start_angle = 0, angle = PI / 2, color = GOLD),
    Annulus$new(inner_radius = 0.3, outer_radius = 0.6, color = PINK),
    RegularPolygon$new(n = 6, radius = 0.6, color = TEAL_C),
    Star$new(outer_radius = 0.6, inner_radius = 0.3, n = 5, color = YELLOW_C)
  )
  for (i in seq_along(row2)) {
    s <- row2[[i]]
    s$position <- c(x_pos[i], -0.6, 0)
    s$fill_color <- s$color
    s$fill_opacity <- 0.5
    sc$add_object(s)
    sc$play(Create$new(s, run_time = 0.25))
  }

  # Row 3: line types
  lines <- list(
    Line$new(start = c(-5.5, -2.2, 0), end = c(-4, -2.2, 0), color = WHITE),
    Arrow$new(start = c(-3.5, -2.2, 0), end = c(-2, -2.2, 0), color = RED_C),
    DoubleArrow$new(start = c(-1.5, -2.2, 0), end = c(0, -2.2, 0), color = GREEN_C),
    DashedLine$new(start = c(0.5, -2.2, 0), end = c(2, -2.2, 0), color = BLUE_C)
  )
  for (l in lines) {
    sc$add_object(l)
    sc$play(Create$new(l, run_time = 0.2))
  }

  # Dot
  dot <- Dot$new(point = c(3.5, -2.2, 0), radius = 0.12, color = YELLOW_C)
  sc$add_object(dot)
  sc$play(GrowFromCenter$new(dot, run_time = 0.2))

  # Bezier curve
  bez <- CubicBezier$new(
    p0 = c(4, -2.5, 0), p1 = c(4.3, -1.5, 0),
    p2 = c(5.0, -1.5, 0), p3 = c(5.3, -2.5, 0),
    color = PURPLE_A
  )
  sc$add_object(bez)
  sc$play(Create$new(bez, run_time = 0.3))

  sc$wait(1.0)

  # Transform circle -> square
  sc$play(Transform$new(shapes[[1]], shapes[[2]], run_time = 1.0))
  sc$wait(1.5)

  .render_tutorial(sc, "01_geometry", format, quality, fps)
}


# =================================================================
#  2. ANIMATION TYPES
#  Stress-tests: FadeIn, FadeOut, Create, Write, Uncreate,
#  DrawBorderThenFill, GrowFromCenter, GrowFromEdge, SpinInFromNothing,
#  RotateAnim, Flash, Indicate, Wiggle, Circumscribe,
#  AnimationGroup, Succession, LaggedStart, Transform, Wait.
# =================================================================

#' Animation Types Tutorial Module
#'
#' Showcase of the animation classes: fades, creation/writing effects,
#' growth, rotation, indication, and composition (groups, succession,
#' lagged starts, transforms).
#'
#' @param format Output format, e.g. "gif" or "mp4"
#' @param quality Render quality preset
#' @param fps Frames per second
#' @return Path to the rendered output, invisibly
#' @export
tutorial_animations <- function(format = "gif", quality = "low", fps = 10) {
  sc <- Scene$new(name = "animation_showcase")

  title <- Text$new("Animation Types", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.2, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.5))

  make_obj <- function(label, pos, col = BLUE_C) {
    c1 <- Circle$new(radius = 0.5, color = col)
    c1$fill_color <- col; c1$fill_opacity <- 0.5
    c1$position <- pos
    t1 <- Text$new(label, font_size = 16, color = WHITE, position = pos)
    list(shape = c1, label = t1)
  }

  # FadeIn
  o <- make_obj("FadeIn", c(-4, 1.5, 0))
  sc$add_object(o$shape); sc$add_object(o$label)
  sc$play(FadeIn$new(o$shape, run_time = 0.5))
  sc$play(FadeIn$new(o$label, run_time = 0.2))

  # Create
  o <- make_obj("Create", c(-2, 1.5, 0), RED_C)
  sc$add_object(o$shape); sc$add_object(o$label)
  sc$play(Create$new(o$shape, run_time = 0.5))
  sc$play(FadeIn$new(o$label, run_time = 0.2))

  # GrowFromCenter
  o <- make_obj("Grow", c(0, 1.5, 0), GREEN_C)
  sc$add_object(o$shape); sc$add_object(o$label)
  sc$play(GrowFromCenter$new(o$shape, run_time = 0.5))
  sc$play(FadeIn$new(o$label, run_time = 0.2))

  # DrawBorderThenFill
  o <- make_obj("Border", c(2, 1.5, 0), TEAL_C)
  sc$add_object(o$shape); sc$add_object(o$label)
  sc$play(DrawBorderThenFill$new(o$shape, run_time = 0.6))
  sc$play(FadeIn$new(o$label, run_time = 0.2))

  # SpinIn
  o <- make_obj("SpinIn", c(4, 1.5, 0), PURPLE_A)
  sc$add_object(o$shape); sc$add_object(o$label)
  sc$play(SpinInFromNothing$new(o$shape, run_time = 0.6))
  sc$play(FadeIn$new(o$label, run_time = 0.2))

  # Indication animations (row 2)
  target <- Circle$new(radius = 0.6, color = YELLOW_C)
  target$fill_color <- YELLOW_C; target$fill_opacity <- 0.5
  target$position <- c(-3, -0.5, 0)
  sc$add_object(target)
  sc$play(GrowFromCenter$new(target, run_time = 0.3))

  fl_label <- Text$new("Flash", font_size = 16, color = WHITE,
                        position = c(-3, -0.5, 0))
  sc$add_object(fl_label)
  sc$play(Flash$new(target, run_time = 0.5))
  sc$play(FadeIn$new(fl_label, run_time = 0.2))

  ind_obj <- Circle$new(radius = 0.6, color = ORANGE)
  ind_obj$fill_color <- ORANGE; ind_obj$fill_opacity <- 0.5
  ind_obj$position <- c(-1, -0.5, 0)
  sc$add_object(ind_obj)
  sc$play(GrowFromCenter$new(ind_obj, run_time = 0.3))
  sc$play(Indicate$new(ind_obj, run_time = 0.6))
  il <- Text$new("Indicate", font_size = 16, color = WHITE,
                  position = c(-1, -0.5, 0))
  sc$add_object(il); sc$play(FadeIn$new(il, run_time = 0.2))

  wig_obj <- Square$new(side_length = 0.8, color = PINK)
  wig_obj$fill_color <- PINK; wig_obj$fill_opacity <- 0.5
  wig_obj$position <- c(1, -0.5, 0)
  sc$add_object(wig_obj)
  sc$play(GrowFromCenter$new(wig_obj, run_time = 0.3))
  sc$play(Wiggle$new(wig_obj, run_time = 0.6))
  wl <- Text$new("Wiggle", font_size = 16, color = WHITE,
                  position = c(1, -0.5, 0))
  sc$add_object(wl); sc$play(FadeIn$new(wl, run_time = 0.2))

  # Rotation
  rot_obj <- RegularPolygon$new(n = 5, radius = 0.5, color = TEAL_C)
  rot_obj$fill_color <- TEAL_C; rot_obj$fill_opacity <- 0.5
  rot_obj$position <- c(3, -0.5, 0)
  sc$add_object(rot_obj)
  sc$play(GrowFromCenter$new(rot_obj, run_time = 0.3))
  sc$play(RotateAnim$new(rot_obj, angle = TAU, run_time = 1.0))
  rl <- Text$new("Rotate", font_size = 16, color = WHITE,
                  position = c(3, -0.5, 0))
  sc$add_object(rl); sc$play(FadeIn$new(rl, run_time = 0.2))

  # Composition: LaggedStart
  comp_label <- Text$new("LaggedStart", font_size = 22, color = GREEN_C,
                          position = c(0, -2.2, 0))
  sc$add_object(comp_label); sc$play(FadeIn$new(comp_label, run_time = 0.3))

  dots <- list()
  for (i in 1:8) {
    d <- Dot$new(point = c(-4 + i, -3, 0), radius = 0.1, color = BLUE_C)
    d$fill_color <- BLUE_C; d$fill_opacity <- 1
    dots[[i]] <- d
    sc$add_object(d)
  }
  dot_anims <- lapply(dots, function(d) GrowFromCenter$new(d, run_time = 0.3))
  sc$play(do.call(LaggedStart$new, c(dot_anims, list(lag_ratio = 0.15))))

  sc$wait(1.0)

  .render_tutorial(sc, "02_animations", format, quality, fps)
}


# =================================================================
#  3. CALCULUS
#  Stress-tests: Axes, plot(), get_area(), TangentLine, Transform
#  (function morphing), multiple function curves, area shading.
# =================================================================

#' @export
tutorial_calculus <- function(format = "gif", quality = "low", fps = 10) {
  sc <- Scene$new(name = "calculus_showcase")

  title <- Text$new("Calculus Visualization", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  axes <- Axes$new(
    x_range = c(-4, 4, 1), y_range = c(-2, 4, 1),
    x_length = 10, y_length = 6,
    include_numbers = TRUE, include_ticks = TRUE, include_tips = TRUE
  )
  axes$position <- c(0, -0.3, 0)
  sc$add_object(axes)
  sc$play(Create$new(axes, run_time = 0.8))

  # Plot f(x) = x^2 / 4
  f <- function(x) x^2 / 4
  curve1 <- axes$plot(f, x_range = c(-3.5, 3.5), color = BLUE_C,
                      stroke_width = 4)
  sc$add_object(curve1)
  sc$play(Create$new(curve1, run_time = 1.5))

  f_label <- Text$new("f(x) = x^2 / 4", font_size = 24, color = BLUE_C,
                       position = c(3.5, 2.5, 0))
  sc$add_object(f_label); sc$play(FadeIn$new(f_label, run_time = 0.3))
  sc$wait(0.5)

  # Shade area under curve [0, 2]
  area <- axes$get_area(curve1, x_range = c(0, 2), color = BLUE_D,
                        fill_opacity = 0.4)
  sc$add_object(area)
  sc$play(FadeIn$new(area, run_time = 0.8))

  area_label <- Text$new("Area = integral 0..2 x^2/4 dx = 2/3",
                          font_size = 22, color = BLUE_D,
                          position = c(3, -1.5, 0))
  sc$add_object(area_label); sc$play(FadeIn$new(area_label, run_time = 0.4))
  sc$wait(1.0)

  # Clear area
  sc$play(FadeOut$new(area, run_time = 0.3))
  sc$play(FadeOut$new(area_label, run_time = 0.2))

  # Tangent line at x = 2
  x0 <- 2
  slope <- x0 / 2  # f'(x) = x/2
  y0 <- f(x0)
  p0 <- axes$c2p(x0, y0)
  t_start <- axes$c2p(x0 - 1.5, y0 + slope * (-1.5))
  t_end   <- axes$c2p(x0 + 1.5, y0 + slope * (1.5))
  tangent <- Line$new(start = t_start, end = t_end, color = RED_C,
                       stroke_width = 3)
  sc$add_object(tangent)
  sc$play(Create$new(tangent, run_time = 0.5))

  dot_pt <- Dot$new(point = p0, radius = 0.08, color = YELLOW_C)
  sc$add_object(dot_pt)
  sc$play(GrowFromCenter$new(dot_pt, run_time = 0.2))

  tan_label <- Text$new("slope = f'(2) = 1", font_size = 22, color = RED_C,
                         position = c(4, 1.5, 0))
  sc$add_object(tan_label); sc$play(FadeIn$new(tan_label, run_time = 0.3))
  sc$wait(1.0)

  # Transform f(x) = x^2/4 into g(x) = sin(x)
  sc$play(FadeOut$new(tangent, run_time = 0.2))
  sc$play(FadeOut$new(dot_pt, run_time = 0.1))
  sc$play(FadeOut$new(tan_label, run_time = 0.2))
  sc$play(FadeOut$new(f_label, run_time = 0.2))

  g <- function(x) sin(x)
  curve2 <- axes$plot(g, x_range = c(-3.5, 3.5), color = GREEN_C,
                      stroke_width = 4)
  sc$add_object(curve2)
  sc$play(Transform$new(curve1, curve2, run_time = 1.5))

  g_label <- Text$new("g(x) = sin(x)", font_size = 24, color = GREEN_C,
                       position = c(3.5, 2.5, 0))
  sc$add_object(g_label); sc$play(FadeIn$new(g_label, run_time = 0.3))
  sc$wait(1.5)

  .render_tutorial(sc, "03_calculus", format, quality, fps)
}


# =================================================================
#  4. LINEAR ALGEBRA
#  Stress-tests: NumberPlane, Vector, Arrow, VGroup, Matrix transform
#  visualization, basis vectors, grid deformation concept.
# =================================================================

#' @export
tutorial_linear_algebra <- function(format = "gif", quality = "low",
                                     fps = 10) {
  sc <- Scene$new(name = "linear_algebra_showcase")

  title <- Text$new("Linear Algebra", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # NumberPlane (grid)
  plane <- NumberPlane$new(
    x_range = c(-5, 5, 1), y_range = c(-4, 4, 1),
    x_length = 10, y_length = 8
  )
  sc$add_object(plane)
  sc$play(Create$new(plane, run_time = 0.8))

  # Basis vectors i-hat and j-hat
  i_hat <- Arrow$new(start = c(0, 0, 0), end = c(1, 0, 0),
                      color = GREEN_C, stroke_width = 5)
  j_hat <- Arrow$new(start = c(0, 0, 0), end = c(0, 1, 0),
                      color = RED_C, stroke_width = 5)
  sc$add_object(i_hat); sc$add_object(j_hat)
  sc$play(GrowFromCenter$new(i_hat, run_time = 0.4))
  sc$play(GrowFromCenter$new(j_hat, run_time = 0.4))

  i_label <- Text$new("i-hat", font_size = 22, color = GREEN_C,
                       position = c(1.2, -0.3, 0))
  j_label <- Text$new("j-hat", font_size = 22, color = RED_C,
                       position = c(-0.5, 1.2, 0))
  sc$add_object(i_label); sc$add_object(j_label)
  sc$play(FadeIn$new(i_label, run_time = 0.2))
  sc$play(FadeIn$new(j_label, run_time = 0.2))

  # A vector v = (2, 1)
  v <- Arrow$new(start = c(0, 0, 0), end = c(2, 1, 0),
                  color = YELLOW_C, stroke_width = 4)
  sc$add_object(v)
  sc$play(GrowFromCenter$new(v, run_time = 0.5))

  v_label <- Text$new("v = (2,1)", font_size = 22, color = YELLOW_C,
                       position = c(2.3, 1.3, 0))
  sc$add_object(v_label)
  sc$play(FadeIn$new(v_label, run_time = 0.3))
  sc$wait(1.0)

  # Show linear combination
  lc_label <- Text$new("v = 2*i + 1*j", font_size = 28, color = WHITE,
                        position = c(-3, 3, 0))
  sc$add_object(lc_label)
  sc$play(FadeIn$new(lc_label, run_time = 0.4))

  # Dashed component lines
  h_comp <- DashedLine$new(start = c(0, 0, 0), end = c(2, 0, 0),
                            color = GREEN_C, stroke_width = 2)
  v_comp <- DashedLine$new(start = c(2, 0, 0), end = c(2, 1, 0),
                            color = RED_C, stroke_width = 2)
  sc$add_object(h_comp); sc$add_object(v_comp)
  sc$play(Create$new(h_comp, run_time = 0.3))
  sc$play(Create$new(v_comp, run_time = 0.3))
  sc$wait(1.0)

  # Matrix transformation label
  sc$play(FadeOut$new(lc_label, run_time = 0.2))
  sc$play(FadeOut$new(h_comp, run_time = 0.2))
  sc$play(FadeOut$new(v_comp, run_time = 0.2))

  mat_label <- Text$new("Apply [[0,-1],[1,0]]  (90-deg rotation)",
                         font_size = 24, color = TEAL_C,
                         position = c(0, 3.3, 0))
  sc$add_object(mat_label)
  sc$play(FadeIn$new(mat_label, run_time = 0.4))

  # Show transformed basis vectors
  i_hat2 <- Arrow$new(start = c(0, 0, 0), end = c(0, 1, 0),
                       color = GREEN_D, stroke_width = 5)
  j_hat2 <- Arrow$new(start = c(0, 0, 0), end = c(-1, 0, 0),
                       color = RED_D, stroke_width = 5)
  v2 <- Arrow$new(start = c(0, 0, 0), end = c(-1, 2, 0),
                   color = YELLOW_D, stroke_width = 4)

  sc$add_object(i_hat2); sc$add_object(j_hat2); sc$add_object(v2)
  sc$play(Transform$new(i_hat, i_hat2, run_time = 1.0))
  sc$play(Transform$new(j_hat, j_hat2, run_time = 1.0))
  sc$play(Transform$new(v, v2, run_time = 1.0))

  v2_label <- Text$new("v' = (-1,2)", font_size = 22, color = YELLOW_D,
                        position = c(-1.5, 2.3, 0))
  sc$add_object(v2_label)
  sc$play(FadeIn$new(v2_label, run_time = 0.3))
  sc$wait(1.5)

  .render_tutorial(sc, "04_linear_algebra", format, quality, fps)
}


# =================================================================
#  5. GRAPH THEORY
#  Stress-tests: Graph, DiGraph, multiple layouts (spring, circular,
#  tree), vertex/edge access, labels, highlight.
# =================================================================

#' @export
tutorial_graph_theory <- function(format = "gif", quality = "low", fps = 10) {
  sc <- Scene$new(name = "graph_theory_showcase")

  title <- Text$new("Graph Theory", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # Undirected graph -- spring layout
  vertices <- as.list(1:7)
  edges <- list(c(1,2), c(1,3), c(2,3), c(2,4), c(3,5), c(4,5),
                c(4,6), c(5,7), c(6,7))
  g1 <- Graph$new(vertices, edges, layout = "spring", labels = TRUE)
  # Shift left
  for (s in g1$submobjects) s$position <- s$position + c(-3, 0, 0)
  sc$add_object(g1)
  sc$play(FadeIn$new(g1, run_time = 1.0))

  g1_label <- Text$new("Spring layout", font_size = 20, color = GREY_A,
                        position = c(-3, -3.2, 0))
  sc$add_object(g1_label); sc$play(FadeIn$new(g1_label, run_time = 0.2))
  sc$wait(0.5)

  # Directed graph -- circular layout (right side)
  dg <- DiGraph$new(as.list(1:6),
                    list(c(1,2), c(2,3), c(3,4), c(4,5), c(5,6), c(6,1),
                         c(1,4), c(2,5)),
                    layout = "circular", labels = TRUE)
  for (s in dg$submobjects) s$position <- s$position + c(3, 0, 0)
  sc$add_object(dg)
  sc$play(FadeIn$new(dg, run_time = 1.0))

  dg_label <- Text$new("Directed (circular)", font_size = 20, color = GREY_A,
                        position = c(3, -3.2, 0))
  sc$add_object(dg_label); sc$play(FadeIn$new(dg_label, run_time = 0.2))
  sc$wait(1.5)

  .render_tutorial(sc, "05_graph_theory", format, quality, fps)
}


# =================================================================
#  6. 3D SHOWCASE
#  Stress-tests: ThreeDScene, ThreeDCamera, Sphere, Torus, Cube,
#  Cylinder, Cone, Surface, ThreeDAxes, depth sorting.
# =================================================================

#' 3D Showcase Tutorial Module
#'
#' Showcase of the 3D system: ThreeDScene, ThreeDCamera, solids
#' (sphere, torus, cube, cylinder, cone), surfaces, 3D axes, and
#' depth sorting.
#'
#' @param format Output format, e.g. "gif" or "mp4"
#' @param quality Render quality preset
#' @param fps Frames per second
#' @return Path to the rendered output, invisibly
#' @export
tutorial_3d <- function(format = "gif", quality = "low", fps = 10) {
  sc <- ThreeDScene$new(name = "3d_showcase")

  title <- Text$new("3D Objects", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # 3D Axes
  axes3d <- ThreeDAxes$new()
  sc$add_object(axes3d)
  sc$play(Create$new(axes3d, run_time = 0.8))

  # Sphere
  sp <- Sphere$new(radius = 0.8)
  sp$position <- c(-3, 0, 0)
  sp$color <- BLUE_C; sp$fill_color <- BLUE_C; sp$fill_opacity <- 0.6
  sc$add_object(sp)
  sc$play(GrowFromCenter$new(sp, run_time = 0.5))

  sp_label <- Text$new("Sphere", font_size = 18, color = BLUE_C,
                        position = c(-3, -1.5, 0))
  sc$add_object(sp_label); sc$play(FadeIn$new(sp_label, run_time = 0.2))

  # Cube
  cb <- Cube$new(side_length = 1.2)
  cb$position <- c(0, 0, 0)
  cb$color <- RED_C; cb$fill_color <- RED_C; cb$fill_opacity <- 0.5
  sc$add_object(cb)
  sc$play(GrowFromCenter$new(cb, run_time = 0.5))

  cb_label <- Text$new("Cube", font_size = 18, color = RED_C,
                        position = c(0, -1.5, 0))
  sc$add_object(cb_label); sc$play(FadeIn$new(cb_label, run_time = 0.2))

  # Torus
  tor <- Torus$new(major_radius = 0.6, minor_radius = 0.25)
  tor$position <- c(3, 0, 0)
  tor$color <- GREEN_C; tor$fill_color <- GREEN_C; tor$fill_opacity <- 0.5
  sc$add_object(tor)
  sc$play(GrowFromCenter$new(tor, run_time = 0.5))

  tor_label <- Text$new("Torus", font_size = 18, color = GREEN_C,
                         position = c(3, -1.5, 0))
  sc$add_object(tor_label); sc$play(FadeIn$new(tor_label, run_time = 0.2))

  # Parametric surface: z = sin(x)*cos(y)
  surf <- Surface$new(
    func = function(u, v) c(u, v, 0.5 * sin(u) * cos(v)),
    u_range = c(-2, 2), v_range = c(-2, 2),
    u_resolution = 12, v_resolution = 12
  )
  surf$position <- c(0, 2.5, 0)
  surf$color <- TEAL_C; surf$fill_color <- TEAL_C; surf$fill_opacity <- 0.4
  sc$add_object(surf)
  sc$play(FadeIn$new(surf, run_time = 0.8))

  surf_label <- Text$new("Parametric Surface", font_size = 18,
                          color = TEAL_C, position = c(0, 3.5, 0))
  sc$add_object(surf_label); sc$play(FadeIn$new(surf_label, run_time = 0.2))

  sc$wait(1.5)

  .render_tutorial(sc, "06_3d", format, quality, fps)
}


# =================================================================
#  7. CODE & TABLES
#  Stress-tests: Code (syntax highlighting, themes), Table, MatrixMobject
# =================================================================

#' Code & Tables Tutorial Module
#'
#' Showcase of code blocks (syntax highlighting, themes), tables,
#' and matrix mobjects.
#'
#' @param format Output format, e.g. "gif" or "mp4"
#' @param quality Render quality preset
#' @param fps Frames per second
#' @return Path to the rendered output, invisibly
#' @export
tutorial_code_tables <- function(format = "gif", quality = "low", fps = 10) {
  sc <- Scene$new(name = "code_tables_showcase")

  title <- Text$new("Code & Tables", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.3)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # Code block
  code <- Code$new(
    code_string = paste0(
      'fibonacci <- function(n) {\n  if (n <= 1) return(n)\n  a <- 0; b <- 1\n',
      '  for (i in 2:n) {\n    tmp <- b\n    b <- a + b\n    a <- tmp\n  }\n  b\n}'
    ),
    language = "r",
    theme = "monokai",
    font_size = 20,
    line_numbers = TRUE,
    position = c(-3, 0.5, 0)
  )
  sc$add_object(code)
  sc$play(FadeIn$new(code, run_time = 1.0))
  sc$wait(0.5)

  code_label <- Text$new("R code (Monokai)", font_size = 18, color = GREY_A,
                          position = c(-3, -2.5, 0))
  sc$add_object(code_label); sc$play(FadeIn$new(code_label, run_time = 0.2))

  # Table
  data_mat <- matrix(
    c("Algorithm", "Time", "Space",
      "Bubble Sort", "O(n^2)", "O(1)",
      "Merge Sort", "O(n log n)", "O(n)",
      "Quick Sort", "O(n log n)", "O(log n)"),
    nrow = 4, ncol = 3, byrow = TRUE
  )
  tb <- Table$new(data_mat, font_size = 18,
                  cell_width = 1.6, cell_height = 0.5,
                  header_color = YELLOW_C)
  tb$position <- c(3, 0.8, 0)
  sc$add_object(tb)
  sc$play(FadeIn$new(tb, run_time = 1.0))

  tb_label <- Text$new("Table", font_size = 18, color = GREY_A,
                        position = c(3, -1.5, 0))
  sc$add_object(tb_label); sc$play(FadeIn$new(tb_label, run_time = 0.2))

  # MatrixMobject
  mat_data <- matrix(c(1, 0, 0, 1), nrow = 2)
  mat <- MatrixMobject$new(mat_data, bracket_style = "square")
  mat$position <- c(3, -2.5, 0)
  sc$add_object(mat)
  sc$play(FadeIn$new(mat, run_time = 0.8))

  mat_label <- Text$new("Matrix (Identity)", font_size = 18, color = GREY_A,
                         position = c(3, -3.5, 0))
  sc$add_object(mat_label); sc$play(FadeIn$new(mat_label, run_time = 0.2))

  sc$wait(1.5)

  .render_tutorial(sc, "07_code_tables", format, quality, fps)
}


# =================================================================
#  8. VECTOR FIELDS
#  Stress-tests: ArrowVectorField, StreamLines, NumberPlane integration.
# =================================================================

#' Vector Fields Tutorial Module
#'
#' Showcase of arrow vector fields, stream lines, and number-plane
#' integration.
#'
#' @param format Output format, e.g. "gif" or "mp4"
#' @param quality Render quality preset
#' @param fps Frames per second
#' @return Path to the rendered output, invisibly
#' @export
tutorial_vector_fields <- function(format = "gif", quality = "low", fps = 10) {
  sc <- Scene$new(name = "vector_fields_showcase")

  title <- Text$new("Vector Fields", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # Background plane
  plane <- NumberPlane$new(
    x_range = c(-5, 5, 1), y_range = c(-4, 4, 1),
    x_length = 10, y_length = 8
  )
  sc$add_object(plane)
  sc$play(Create$new(plane, run_time = 0.6))

  # ArrowVectorField: rotation field  F(x,y) = (-y, x)
  vf <- ArrowVectorField$new(
    func = function(x, y) c(-y, x),
    x_range = c(-4, 4, 1),
    y_range = c(-3, 3, 1),
    color_scheme = "magnitude"
  )
  sc$add_object(vf)
  sc$play(FadeIn$new(vf, run_time = 1.5))

  vf_label <- Text$new("F(x,y) = (-y, x)  [rotation field]",
                        font_size = 24, color = TEAL_C,
                        position = c(0, 3.5, 0))
  sc$add_object(vf_label); sc$play(FadeIn$new(vf_label, run_time = 0.3))
  sc$wait(1.5)

  # Fade out and show StreamLines
  sc$play(FadeOut$new(vf, run_time = 0.5))
  sc$play(FadeOut$new(vf_label, run_time = 0.2))

  sl <- StreamLines$new(
    func = function(x, y) c(-y, x),
    x_range = c(-4, 4),
    y_range = c(-3, 3),
    n_stream_lines = 25
  )
  sc$add_object(sl)
  sc$play(Create$new(sl, run_time = 2.0))

  sl_label <- Text$new("Stream Lines (same field)",
                        font_size = 24, color = GREEN_C,
                        position = c(0, 3.5, 0))
  sc$add_object(sl_label); sc$play(FadeIn$new(sl_label, run_time = 0.3))
  sc$wait(1.5)

  .render_tutorial(sc, "08_vector_fields", format, quality, fps)
}


# =================================================================
#  9. DATA ANIMATION
#  Stress-tests: animate_data_scatter, animate_time_series,
#  grammar (manim_plot + anim_point + anim_line).
# =================================================================

#' Data Animation Tutorial Module
#'
#' Showcase of data animation: scatter and time-series animations and
#' the grammar interface (manim_plot, anim_point, anim_line).
#'
#' @param format Output format, e.g. "gif" or "mp4"
#' @param quality Render quality preset
#' @param fps Frames per second
#' @return Path to the rendered output, invisibly
#' @export
tutorial_data_animation <- function(format = "gif", quality = "low",
                                     fps = 10) {
  sc <- Scene$new(name = "data_animation_showcase")

  title <- Text$new("Data Animation", font_size = 56, color = YELLOW_C,
                     weight = "bold", position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # Scatter plot built manually
  set.seed(123)
  n <- 30
  x_data <- sort(stats::runif(n, 0, 10))
  y_data <- 2 + 0.8 * x_data + stats::rnorm(n, sd = 1.5)

  axes <- Axes$new(
    x_range = c(0, 11, 2), y_range = c(0, 12, 2),
    x_length = 9, y_length = 6,
    include_numbers = TRUE, include_ticks = TRUE
  )
  axes$position <- c(0, -0.5, 0)
  sc$add_object(axes)
  sc$play(Create$new(axes, run_time = 0.6))

  ax_label <- Text$new("Scatter + Regression", font_size = 28,
                        color = TEAL_C, position = c(0, 3.3, 0))
  sc$add_object(ax_label); sc$play(FadeIn$new(ax_label, run_time = 0.3))

  # Add dots with LaggedStart
  dots <- list()
  for (i in seq_len(n)) {
    pos <- axes$c2p(x_data[i], y_data[i])
    d <- Dot$new(point = pos, radius = 0.06, color = BLUE_C)
    d$fill_color <- BLUE_C; d$fill_opacity <- 0.8
    dots[[i]] <- d
    sc$add_object(d)
  }
  dot_anims <- lapply(dots, function(d) GrowFromCenter$new(d, run_time = 0.15))
  sc$play(do.call(LaggedStart$new, c(dot_anims, list(lag_ratio = 0.08))))

  # Regression line
  fit <- stats::lm(y_data ~ x_data)
  b0 <- stats::coef(fit)[1]; b1 <- stats::coef(fit)[2]
  reg_func <- function(x) b0 + b1 * x
  reg_line <- axes$plot(reg_func, x_range = c(0, 10),
                        color = RED_C, stroke_width = 3)
  sc$add_object(reg_line)
  sc$play(Create$new(reg_line, run_time = 1.0))

  eq_txt <- sprintf("y = %.1f + %.2fx  (R2 = %.2f)", b0, b1,
                     summary(fit)$r.squared)
  eq_label <- Text$new(eq_txt, font_size = 20, color = RED_C,
                        position = c(3, -3, 0))
  sc$add_object(eq_label); sc$play(FadeIn$new(eq_label, run_time = 0.4))
  sc$wait(1.5)

  .render_tutorial(sc, "09_data_animation", format, quality, fps)
}


# =================================================================
#  10. COMPLEX & POLAR PLANES
#  Stress-tests: ComplexPlane, PolarPlane.
# =================================================================

#' Complex & Polar Planes Tutorial Module
#'
#' Showcase of the complex plane and polar plane coordinate systems.
#'
#' @param format Output format, e.g. "gif" or "mp4"
#' @param quality Render quality preset
#' @param fps Frames per second
#' @return Path to the rendered output, invisibly
#' @export
tutorial_complex_polar <- function(format = "gif", quality = "low",
                                    fps = 10) {
  sc <- Scene$new(name = "complex_polar_showcase")

  title <- Text$new("Complex & Polar Planes", font_size = 56,
                     color = YELLOW_C, weight = "bold",
                     position = c(0, 3.3, 0))
  sc$add_object(title)
  sc$play(FadeIn$new(title, run_time = 0.6))
  sc$wait(0.5)
  sc$play(FadeOut$new(title, run_time = 0.3))

  # Complex Plane
  cp <- ComplexPlane$new(
    x_range = c(-3, 3, 1), y_range = c(-3, 3, 1),
    x_length = 6, y_length = 6
  )
  cp$position <- c(-3, 0, 0)
  sc$add_object(cp)
  sc$play(Create$new(cp, run_time = 0.8))

  cp_label <- Text$new("Complex Plane", font_size = 22, color = BLUE_C,
                        position = c(-3, 3.3, 0))
  sc$add_object(cp_label); sc$play(FadeIn$new(cp_label, run_time = 0.2))

  # Plot unit circle on complex plane
  unit_circle <- Circle$new(radius = 1, color = YELLOW_C)
  unit_circle$position <- c(-3, 0, 0)
  sc$add_object(unit_circle)
  sc$play(Create$new(unit_circle, run_time = 0.5))

  # Points e^(i*theta) for several angles
  angles <- c(0, PI/4, PI/2, 3*PI/4, PI)
  for (a in angles) {
    x <- cos(a); y <- sin(a)
    pt <- Dot$new(point = c(-3 + x, y, 0), radius = 0.06, color = RED_C)
    sc$add_object(pt)
    sc$play(GrowFromCenter$new(pt, run_time = 0.15))
  }
  euler_label <- Text$new("e^(i*theta) = cos + i*sin",
                           font_size = 18, color = YELLOW_C,
                           position = c(-3, -3.3, 0))
  sc$add_object(euler_label); sc$play(FadeIn$new(euler_label, run_time = 0.3))

  # Polar Plane (right side)
  pp <- PolarPlane$new(
    radius_range = c(0, 3, 1),
    azimuth_step = PI / 6
  )
  pp$position <- c(3, 0, 0)
  sc$add_object(pp)
  sc$play(Create$new(pp, run_time = 0.8))

  pp_label <- Text$new("Polar Plane", font_size = 22, color = GREEN_C,
                        position = c(3, 3.3, 0))
  sc$add_object(pp_label); sc$play(FadeIn$new(pp_label, run_time = 0.2))

  # Plot a polar rose: r = 2*cos(3*theta)
  rose_pts <- matrix(0, ncol = 3, nrow = 200)
  for (i in 1:200) {
    theta <- (i - 1) / 200 * 2 * PI
    r <- 2 * cos(3 * theta)
    if (r < 0) r <- 0
    rose_pts[i, ] <- c(3 + r * cos(theta), r * sin(theta), 0)
  }
  rose <- VMobject$new(color = PINK, stroke_width = 3)
  # Build Bezier
  pts <- matrix(numeric(0), ncol = 3)
  idx <- 1
  while (idx + 3 <= 200) {
    pts <- rbind(pts, rose_pts[idx,], rose_pts[idx+1,],
                 rose_pts[idx+2,], rose_pts[idx+3,])
    idx <- idx + 3
  }
  rose$points <- pts
  sc$add_object(rose)
  sc$play(Create$new(rose, run_time = 1.5))

  rose_label <- Text$new("r = 2*cos(3*theta)", font_size = 18, color = PINK,
                          position = c(3, -3.3, 0))
  sc$add_object(rose_label); sc$play(FadeIn$new(rose_label, run_time = 0.3))

  sc$wait(1.5)

  .render_tutorial(sc, "10_complex_polar", format, quality, fps)
}


# =================================================================
#  RUNNER: execute all 10 tutorials
# =================================================================

#' Run All Showcase Tutorials
#'
#' @description
#' Runs all 10 stress-test tutorials, each exercising a different
#' dimension of manimR's capabilities.  Reports pass/fail and timing.
#'
#' @param format Output format ("gif" or "mp4")
#' @param quality Render quality ("low", "medium", "high")
#' @param fps Frames per second
#'
#' @return Data frame with name, status, time, file_size for each tutorial
#' @export
run_all_showcase_tutorials <- function(format = "gif", quality = "low",
                                       fps = 10) {
  tutorials <- list(
    list(name = "01 Geometry",       fn = tutorial_geometry),
    list(name = "02 Animations",     fn = tutorial_animations),
    list(name = "03 Calculus",       fn = tutorial_calculus),
    list(name = "04 Linear Algebra", fn = tutorial_linear_algebra),
    list(name = "05 Graph Theory",   fn = tutorial_graph_theory),
    list(name = "06 3D Showcase",    fn = tutorial_3d),
    list(name = "07 Code & Tables",  fn = tutorial_code_tables),
    list(name = "08 Vector Fields",  fn = tutorial_vector_fields),
    list(name = "09 Data Animation", fn = tutorial_data_animation),
    list(name = "10 Complex/Polar",  fn = tutorial_complex_polar)
  )

  results <- data.frame(
    name = character(), status = character(),
    seconds = numeric(), size_kb = numeric(),
    error = character(),
    stringsAsFactors = FALSE
  )

  cli::cli_h1("manimR Showcase Tutorial Suite")
  cli::cli_alert_info("Format: {format} | Quality: {quality} | FPS: {fps}")
  cli::cli_text("")

  for (tut in tutorials) {
    cli::cli_alert("Running: {tut$name} ...")
    t0 <- proc.time()["elapsed"]
    status <- "PASS"
    err_msg <- ""
    fpath <- ""

    tryCatch({
      fpath <- tut$fn(format = format, quality = quality, fps = fps)
    }, error = function(e) {
      status <<- "FAIL"
      err_msg <<- conditionMessage(e)
      cli::cli_alert_danger("{tut$name}: {err_msg}")
    })

    elapsed <- as.numeric(proc.time()["elapsed"] - t0)
    fsize <- if (file.exists(fpath)) round(file.info(fpath)$size / 1024, 1)
             else NA_real_

    results <- rbind(results, data.frame(
      name = tut$name, status = status,
      seconds = round(elapsed, 1), size_kb = fsize,
      error = err_msg,
      stringsAsFactors = FALSE
    ))
  }

  cli::cli_text("")
  cli::cli_h2("Results")
  n_pass <- sum(results$status == "PASS")
  n_fail <- sum(results$status == "FAIL")
  cli::cli_alert_success("Passed: {n_pass}/10")
  if (n_fail > 0) cli::cli_alert_danger("Failed: {n_fail}/10")
  print(results[, c("name", "status", "seconds", "size_kb")])

  if (n_fail > 0) {
    cli::cli_h2("Errors")
    for (i in which(results$status == "FAIL")) {
      cli::cli_alert_danger("{results$name[i]}: {results$error[i]}")
    }
  }

  invisible(results)
}
