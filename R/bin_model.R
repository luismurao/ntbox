#' bin_model: Binarize a model using a threshold
#'
#' @description Function to binarize a continuos model given a threshold
#'
#' @param model A continuos raster model of suitability values
#' @param occs Ocurrence data with two colomuns (longitude and latitude).
#' @param percent Type of thresholding method. Values go from 0-100 percent
#' of the data. 0 is equivalent to the minimum training presence.
#' @return A binary map of the prediction.
#' @examples
#' \dontrun{
#'
#' model_p <- system.file("extdata/ambystoma_model.tif",
#'                        package = "ntbox")
#' model <- raster::raster(model_p)

#' data_p <- system.file("extdata/ambystoma_validation.csv",
#'                       package = "ntbox")
#' data <- read.csv(data_p)

#' occs <- data[which(data$presence_absence==1),]

#' binary <- bin_model(model,occs,percent = 5)

#' raster::plot(binary)
#' }
#' @export

bin_model <- function(model,occs,percent){
  suits <- sort(stats::na.omit(raster::extract(model,occs)))
  prop <- percent/100
  npoints <- length(suits)
  nsuits <- npoints *prop
  int_npoints <- round(nsuits)
  if(int_npoints<npoints && npoints>10){
    thpos <- ceiling(nsuits)
  }
  else{
    thpos <- int_npoints
  }
  th <- suits[thpos]
  mod_bin <- model >= th
  return(mod_bin)

}

