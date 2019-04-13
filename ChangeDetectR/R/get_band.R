#' get_band
#'
#' This function returns back the NIR and GREEN bands for the satelites Landsat 4, 5 and 8.
#' output: raster layer
#' @param band the band you want to return
#' @param image_folder folder where your images are stored
#' @param sensor_type sentor type of your images
#' 
#' @details this function does xxx
#' @keywords Landsat, bands, remote sensing
#' @export
#' @examples




get_band <- function(band, image_folder, sensor_type) {
  
  if (band == "NIR"){
    if (sensor_type == "LC08") {
      nir_file <- list.files(image_folder, pattern = "B5.TIF", full.names = TRUE)
      NIR_band <- brick(nir_file)
    } else if (sensor_type == "LT05" | sensor_type == "LT04") {     # google wie man or eleganter schreibt
      nir_file <- list.files(image_folder, pattern = "B4.TIF", full.names = TRUE)
      NIR_band <- brick(nir_file)
    } else {
      message("Sensor type not supported.")
      return(NA)
    }
    return(NIR_band)
  } else if (band == "GREEN"){
    if (sensor_type == "LC08") {
      green_file <- list.files(image_folder, pattern = "B3.TIF", full.names = TRUE)
      GREEN_band <- brick(green_file)
    } else if (sensor_type == "LT05" | sensor_type == "LT04") {
      green_file <- list.files(image_folder, pattern = "B2.TIF", full.names = TRUE)
      GREEN_band <- brick(green_file)
    } else {
      message("Sensor type not supported.")
      return(NA)
    }
    return(GREEN_band)
  }
}