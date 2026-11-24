#' @title ManimR Constants
#' @description
#' Direction vectors, mathematical constants, and standard color definitions
#' used throughout the manimR package.
#' @name constants
#' @keywords internal
NULL

# ---- Direction Vectors ----

#' Direction Constants
#'
#' @description Standard direction vectors for positioning and movement.
#' @name directions
#' @keywords internal
NULL

#' @rdname directions
#' @export
ORIGIN <- c(0, 0, 0)

#' @rdname directions
#' @export
UP <- c(0, 1, 0)

#' @rdname directions
#' @export
DOWN <- c(0, -1, 0)

#' @rdname directions
#' @export
LEFT <- c(-1, 0, 0)

#' @rdname directions
#' @export
RIGHT <- c(1, 0, 0)

#' @rdname directions
#' @export
OUT <- c(0, 0, 1)

#' @rdname directions
#' @export
IN <- c(0, 0, -1)

#' @rdname directions
#' @export
UL <- c(-1, 1, 0)

#' @rdname directions
#' @export
UR <- c(1, 1, 0)

#' @rdname directions
#' @export
DL <- c(-1, -1, 0)

#' @rdname directions
#' @export
DR <- c(1, -1, 0)

# ---- Mathematical Constants ----

#' Mathematical Constants
#'
#' @description Standard mathematical constants.
#' @name math_constants
#' @keywords internal
NULL

#' @rdname math_constants
#' @export
PI <- pi

#' @rdname math_constants
#' @export
TAU <- 2 * pi

#' @rdname math_constants
#' @export
DEGREES <- pi / 180

# ---- Frame Dimensions ----

#' Frame Constants
#'
#' @description Default frame dimensions matching Manim's coordinate system.
#'   The frame spans from -FRAME_X_RADIUS to +FRAME_X_RADIUS horizontally
#'   and -FRAME_Y_RADIUS to +FRAME_Y_RADIUS vertically.
#' @name frame_constants
#' @keywords internal
NULL

#' @rdname frame_constants
#' @export
FRAME_WIDTH <- 14.222

#' @rdname frame_constants
#' @export
FRAME_HEIGHT <- 8

#' @rdname frame_constants
#' @export
FRAME_X_RADIUS <- FRAME_WIDTH / 2

#' @rdname frame_constants
#' @export
FRAME_Y_RADIUS <- FRAME_HEIGHT / 2

# ---- Default Sizes ----

#' Size Constants
#'
#' @description Default sizes for common objects.
#' @name size_constants
#' @keywords internal
NULL

#' @rdname size_constants
#' @export
DEFAULT_DOT_RADIUS <- 0.08

#' @rdname size_constants
#' @export
DEFAULT_SMALL_DOT_RADIUS <- 0.04

#' @rdname size_constants
#' @export
DEFAULT_DASH_LENGTH <- 0.12

#' @rdname size_constants
#' @export
DEFAULT_ARROW_TIP_LENGTH <- 0.35

#' @rdname size_constants
#' @export
DEFAULT_ARROW_TIP_WIDTH <- 0.35

#' @rdname size_constants
#' @export
DEFAULT_STROKE_WIDTH <- 4

#' @rdname size_constants
#' @export
DEFAULT_FONT_SIZE <- 48

#' @rdname size_constants
#' @export
SMALL_BUFF <- 0.1

#' @rdname size_constants
#' @export
MED_SMALL_BUFF <- 0.25

#' @rdname size_constants
#' @export
MED_LARGE_BUFF <- 0.5

#' @rdname size_constants
#' @export
LARGE_BUFF <- 1.0

# ---- Standard Colors ----
# Matching Manim Community's color palette

#' @rdname manim_colors
#' @export
WHITE <- "#FFFFFF"

#' @rdname manim_colors
#' @export
GREY_A <- "#DDDDDD"

#' @rdname manim_colors
#' @export
GREY_B <- "#BBBBBB"

#' @rdname manim_colors
#' @export
GREY_C <- "#888888"

#' @rdname manim_colors
#' @export
GREY_D <- "#444444"

#' @rdname manim_colors
#' @export
GREY_E <- "#222222"

#' @rdname manim_colors
#' @export
BLACK <- "#000000"

#' Standard Manim Colors
#'
#' @description Color constants matching Manim Community's palette.
#' @name manim_colors
#' @keywords internal
NULL

#' @rdname manim_colors
#' @export
BLUE_A <- "#C7E9F1"

#' @rdname manim_colors
#' @export
BLUE_B <- "#9CDCEB"

#' @rdname manim_colors
#' @export
BLUE_C <- "#58C4DD"

#' @rdname manim_colors
#' @export
BLUE_D <- "#29ABCA"

#' @rdname manim_colors
#' @export
BLUE_E <- "#236B8E"

#' @rdname manim_colors
#' @export
BLUE <- BLUE_C

#' @rdname manim_colors
#' @export
TEAL_A <- "#ACEAD7"

#' @rdname manim_colors
#' @export
TEAL_B <- "#76DDC0"

#' @rdname manim_colors
#' @export
TEAL_C <- "#5CD0B3"

#' @rdname manim_colors
#' @export
TEAL_D <- "#55C1A7"

#' @rdname manim_colors
#' @export
TEAL_E <- "#49A88F"

#' @rdname manim_colors
#' @export
TEAL <- TEAL_C

#' @rdname manim_colors
#' @export
GREEN_A <- "#C9E2AE"

#' @rdname manim_colors
#' @export
GREEN_B <- "#A6CF8C"

#' @rdname manim_colors
#' @export
GREEN_C <- "#83C167"

#' @rdname manim_colors
#' @export
GREEN_D <- "#77B05D"

#' @rdname manim_colors
#' @export
GREEN_E <- "#699C52"

#' @rdname manim_colors
#' @export
GREEN <- GREEN_C

#' @rdname manim_colors
#' @export
YELLOW_A <- "#FFF1B6"

#' @rdname manim_colors
#' @export
YELLOW_B <- "#FFEA94"

#' @rdname manim_colors
#' @export
YELLOW_C <- "#FFFF00"

#' @rdname manim_colors
#' @export
YELLOW_D <- "#F4D345"

#' @rdname manim_colors
#' @export
YELLOW_E <- "#E8C11C"

#' @rdname manim_colors
#' @export
YELLOW <- YELLOW_C

#' @rdname manim_colors
#' @export
GOLD_A <- "#F7C797"

#' @rdname manim_colors
#' @export
GOLD_B <- "#F9B775"

#' @rdname manim_colors
#' @export
GOLD_C <- "#F0AC5F"

#' @rdname manim_colors
#' @export
GOLD_D <- "#E1A158"

#' @rdname manim_colors
#' @export
GOLD_E <- "#C78D46"

#' @rdname manim_colors
#' @export
GOLD <- GOLD_C

#' @rdname manim_colors
#' @export
RED_A <- "#F7A1A3"

#' @rdname manim_colors
#' @export
RED_B <- "#FF8080"

#' @rdname manim_colors
#' @export
RED_C <- "#FC6255"

#' @rdname manim_colors
#' @export
RED_D <- "#E65A4C"

#' @rdname manim_colors
#' @export
RED_E <- "#CF5044"

#' @rdname manim_colors
#' @export
RED <- RED_C

#' @rdname manim_colors
#' @export
MAROON_A <- "#ECABC1"

#' @rdname manim_colors
#' @export
MAROON_B <- "#EC92AB"

#' @rdname manim_colors
#' @export
MAROON_C <- "#C55F73"

#' @rdname manim_colors
#' @export
MAROON_D <- "#A24D61"

#' @rdname manim_colors
#' @export
MAROON_E <- "#94424F"

#' @rdname manim_colors
#' @export
MAROON <- MAROON_C

#' @rdname manim_colors
#' @export
PURPLE_A <- "#CAA3E8"

#' @rdname manim_colors
#' @export
PURPLE_B <- "#B189C6"

#' @rdname manim_colors
#' @export
PURPLE_C <- "#9A72AC"

#' @rdname manim_colors
#' @export
PURPLE_D <- "#715582"

#' @rdname manim_colors
#' @export
PURPLE_E <- "#644172"

#' @rdname manim_colors
#' @export
PURPLE <- PURPLE_C

#' @rdname manim_colors
#' @export
PINK <- "#D147BD"

#' @rdname manim_colors
#' @export
LIGHT_PINK <- "#DC75CD"

#' @rdname manim_colors
#' @export
ORANGE <- "#FF862F"

#' @rdname manim_colors
#' @export
LIGHT_BROWN <- "#CD853F"

#' @rdname manim_colors
#' @export
DARK_BROWN <- "#8B4513"

#' @rdname manim_colors
#' @export
GREY <- GREY_C

#' @rdname manim_colors
#' @export
GRAY <- GREY

#' @rdname manim_colors
#' @export
LIGHT_GREY <- GREY_B

#' @rdname manim_colors
#' @export
LIGHT_GRAY <- LIGHT_GREY

#' @rdname manim_colors
#' @export
DARK_GREY <- GREY_D

#' @rdname manim_colors
#' @export
DARK_GRAY <- DARK_GREY
