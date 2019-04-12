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