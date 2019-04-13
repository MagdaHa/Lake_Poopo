#' get_sensor_type
#'
#' This function returns the satellite name based on the file name (only for Landsat 4, 5 and 8).
#' output: sensor type name
#' @param image_path uses the path where the image is located
#' @details this function does xxx
#' 
#' @keywords Landsat, bands, sensor,remote sensing
#' @export
#' @examples


get_sensor_type <- function(image_path) {
  
  if (grepl("LC08", image_path)){
    return("LC08")
  } else if (grepl("LT05", image_path)) {
    return("LT05")
  } else if (grepl("LT04", image_path)) {
    return("LT04")  
  } else {
    
    message("Sensor not found.")
  }
}