#' KTH color palette with 5 qualitative colors
#' 
#' This palette is taken from https://intra.kth.se/en/administration/kommunikation/grafiskprofil/profilfarger-1.845077
#' 
#' This function aligns its signature with RColorBrewer::color.pal()
#' 
#' @param n number of colors to use (1..5), default is to return all five
#' @param name name of palette
#' @return named vector with colors and hex codes
#' @importFrom grDevices rgb
#' @export
#' @examples 
#' palette_kth(1)  # return primary color in the KTH palette
palette_kth <- function(n = 5, name = "KTH") {
  
  if (name != "KTH")
    stop("Please use RColorBrewer::brewer.pal() for non-KTH palettes")
  
  if (!n %in% 1:5)
    stop("Please provide n colors (1..5) to return from palette")
  
  palette <- c(
    blue = rgb(25, 84, 166, maxColorValue = 256),
    lightblue = rgb(36, 160, 216, maxColorValue = 256),
    cerise = rgb(216, 84, 151, maxColorValue = 256),
    olive = rgb(176, 201, 43, maxColorValue = 256),
    gray = rgb(101, 101, 108, maxColorValue = 256)
  )
  
  palette[1:n]
  
}

#' KTH color palette information
#' 
#' This function aligns its signature with RColorBrewer::color.pal.info()
#' 
#' @return data frame with information about the available palette(s)
#' @export
palette_kth_info <- function() {
  data.frame(row.names = "KTH",
    maxcolors = 5, 
    category = "qual", 
    colorblind = FALSE
  )
}