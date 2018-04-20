##########################################################################################################################################################################################################
## Global Constants
##########################################################################################################################################################################################################


default_colors <- c(
  `Tahiti Gold`        = "#EF7B05",
  `Amber`      = "#FCBE0E",
  `Atlantis`       = "#8ABD24",
  `Bermuda Gray`     = "#7296AF",
  `Downy`     = "#68C3CD",
  `Congress Blue` = "#004992",
  `Mexican Red` = "#B22229",
  `Bronco`  = "#8c8c8c")

bb_themes <- c(
  `main`        = "main",
  `dark`      = "dark",
  `light`       = "light")


##########################################################################################################################################################################################################
##Funktion für das erzeugen einer angepassten Farbpalette
##########################################################################################################################################################################################################
#' R Function für generating a custom colorpallette of length x
#'
#' @param x :numeric vector - size of colorpalette
#'
#' @return one character vector of length x containig the custom colors
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' cols<-getBBColors(7)
#'
#'@export
getBBColors<-function(x) {

  cols2<-c("#EF7B05","#FCBE0E","#8ABD24","#7296AF","#68C3CD","#004992","#B22229","#8c8c8c")
  if(x<9){mypalette<-cols2[1:x]
  }else{
    mypalette<-RColorBrewer::brewer.pal(9,"Set1")
    mypalette<-grDevices::colorRampPalette(mypalette, space = "Lab")
    mypalette<-mypalette(x)}
  return(mypalette)

}



##########################################################################################################################################################################################################
##Funktion to extract colors as hex codes
##########################################################################################################################################################################################################
#' Function to extract bb colors as hex codes
#'
#' @param ... Character names of default_colors

#' @return vector of colors (Hex-format)
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' colors<-bb_cols("Bermuda Gray","Downy")

#'
#'@export
bb_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (default_colors)

  default_colors[cols]
}


default_palettes <- list(
  `main`  = bb_cols("Tahiti Gold", "Amber","Congress Blue"),# "Atlantis"),

  `cool`  = bb_cols("Atlantis", "Bermuda Gray"),

  `hot`   = bb_cols("Mexican Red","Tahiti Gold",  "Amber"),

  `mixed` = bb_cols("Congress Blue", "Tahiti Gold", "Mexican Red", "Atlantis"),

  `grey`  = bb_cols("Bermuda Gray", "Bronco"),

  `complete`  = bb_cols("Tahiti Gold", "Amber", "Atlantis","Bermuda Gray","Downy","Congress Blue","Mexican Red","Bronco")
)

##########################################################################################################################################################################################################
##Funktion to extract colors as hex codes
##########################################################################################################################################################################################################
#' Return function to interpolate a bb color palette
#'
#' @param palette Character name of palette in bb_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'

#' @return vector of colors (Hex-format)
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' colors<-bb_cols("Bermuda Gray","Downy")

#'
#'@export
bb_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- default_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

##########################################################################################################################################################################################################
##Function to construct ggplot color scales for bb colors
##########################################################################################################################################################################################################
#' Color scale constructor for bb colors
#'
#' @param palette Character name of palette in bb_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_bb <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bb_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("bb_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

##########################################################################################################################################################################################################
##Function to construct ggplot fill
##########################################################################################################################################################################################################
#' Fill scale constructor for bb colors
#'
#' @param palette Character name of palette in bb_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_bb <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bb_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("bb_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
##########################################################################################################################################################################################################
##Function to streamline default  ggplot themeing
##########################################################################################################################################################################################################
#' Fill scale constructor for bb colors
#'
#' @param theme Character name of theme in bb_palettes
#' @param ... Additional arguments passed to theme()
#'
#'
theme_bb <- function(theme = "main", ...) {


  if (theme == "main") {
    theme_minimal(base_size = 11, base_family = "sans")+
      theme(axis.title = element_text(vjust=0.1),axis.title.y=element_text(vjust=0.5),legend.position="bottom",legend.key.size = unit(0.3,"cm"),...)
  } else {
    # To Do: Add more custom themes
    theme_minimal(base_size = 11, base_family = "sans")+
      theme(axis.title = element_text(vjust=0.1),axis.title.y=element_text(vjust=0.5),legend.position="bottom",legend.key.size = unit(0.3,"cm"),...)
  }

}












