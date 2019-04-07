# calculation in km²
calc_water_area <- function(x, res_x, res_y){
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
