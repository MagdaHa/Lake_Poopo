#' calc_water_area
#'
#' This function calculates the area in sqkm per pixel values of a binary raster imge (eg. water areas).
#' output: area in sqkm
#' @param raster the raster you want to use for the area calculation
#' @param res_x x resolution of the raster layer, default value is 30m
#' @param res_y y resolution of the raster layer, default value is 30m
#' 
#' @details this function does xxx
#' @keywords threshold, index calculation, remote sensing
#' @export
#' @examples

calc_water_area <- function(raster, res_x, res_y){
  #define default value for res_x and res_y
  if (missing(res_x) & missing(res_y)){
    res_x <- 0.03
    res_y <- 0.03
  }
  vals <- getValues(x)
  water_cells <- sum(vals ==1, na.rm = T)
  water_area <-  sum(vals ==1, na.rm = T) * res_x * res_y
  return(water_area)
}
