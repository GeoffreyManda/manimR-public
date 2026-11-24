#' @title ManimR Theme and Colors
#' @description A collection of colors and theme settings for cinematic aesthetics.
#' @export
ManimColor <- list(
  # Apple-inspired Dark Mode Colors
  Background = "#000000",             # Pure Black (OLED style)
  GreyLight  = "#E5E5EA",             # Text Light
  GreyDark   = "#1C1C1E",             # Surface Dark
  GreyMedium = "#3A3A3C",             # Secondary
  
  # Vibrant Accents
  Blue       = "#0A84FF",             # iOS Blue
  Green      = "#30D158",             # iOS Green
  Indigo     = "#5E5CE6",             # iOS Indigo
  Orange     = "#FF9F0A",             # iOS Orange
  Pink       = "#FF375F",             # iOS Pink
  Purple     = "#BF5AF2",             # iOS Purple
  Red        = "#FF453A",             # iOS Red
  Teal       = "#64D2FF",             # iOS Teal
  Yellow     = "#FFD60A",             # iOS Yellow
  
  # Professional
  Gold       = "#FFD700",
  White      = "#FFFFFF"
)

#' @title Apply Theme
#' @description Applies global defaults for a cinematic look.
#' @param scene A Scene object
#' @export
apply_cinematic_theme <- function(scene) {
  scene$background_color <- ManimColor$Background
  # TODO: We could iterate existing objects and update stroke widths?
  # Usually this is called before adding objects.
  invisible(scene)
}
