#' biaslayer: Function to create a bias layer
#'
#' @description biaslayer estimates a raster that represents the sampling bias; it can be used in the \code{\link{maxent_call}{ntbox}} function.
#' @param occs_df A data.frame with the occurrence data and coordinates.
#' @param longitude A character vector of the column name of longitude.
#' @param latitude A character vector of the column name of latitude.
#' @param raster_mold A raster of the calibration area. This will serve just as a mold to make the bias layer.
#' @export
#' @details The biaslayer uses the 2 dimensional kernel desity estimator \code{\link{kde2d}{MASS}}.
#' @examples
#' # Birds of North America
#' birds <- read.csv(system.file("extdata",
#'                               "birdsN_America.csv",
#'                               package = "ntbox"))
#' bio01 <- raster::raster(system.file("extdata",
#'                                     "bio01_bias.tif",
#'                                     package = "ntbox"))
#'
#' biasBirds <- biaslayer(occs_df = birds,
#'                        longitude = "Longitude",
#'                        latitude = "Latitude",
#'                        raster_mold = bio01)
#'
#' raster::plot(biasBirds)
#'
biaslayer <- function(occs_df,longitude, latitude, raster_mold){
  rbias <- raster_mold
  ll_ras <- raster::rasterize(occs_df[,c(longitude,latitude)],
                              raster_mold, 1)
  no_na <- which(!is.na(raster_mold[]))

  ll_rasIDs <- which(raster::getValues(ll_ras) %in% 1)
  occ_T <- sp::coordinates(ll_ras)[ll_rasIDs,]

  dens <- MASS::kde2d(occ_T[,1], occ_T[,2],
                      n = c(ncol(ll_ras),
                            nrow(ll_ras)))
  biasLayer <- raster::raster(dens)
  if(!all(dim(biasLayer)[1:2] == dim(raster_mold)[1:2]))
    rbias <- raster::resample(biasLayer,raster_mold)
  else{
    rbias[no_na] <- biasLayer[no_na]
  }
  names(rbias) <- "BiasLayer"

  return(rbias)
}
