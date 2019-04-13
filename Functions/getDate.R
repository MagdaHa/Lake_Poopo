getDate <- function(file) {
  date <- as.character(c(195004:202007))
  for (i in 1:length(date)){
    if (grepl(date[i], file)) {
      return(date[i])
    }
  }
}