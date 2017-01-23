#' Function to clean duplicated longitude and latitude data
#' @description Clean duplicated longitude and latitude data using threshold distance
#'              which is a distance (in grades) between points to be considered
#'              duplicates.
#' @param data A data.frame with longitude and latitude data
#' @param longitude A character vector of the column name of longitude.
#' @param latitude A character vector of the column name of latitude.
#' @param threshold A numerc value representig the distance (in grades) between coordinates
#'            to be considered as a duplicate.
#' @return Returns a data.frame with coordinate data from species
#' @export
#' @examples
#' # Species genus
#' genus <- "ambystoma"
#' # Species name
#' species <- "tigrinum"
#' # GBIF search
#' # ambystoma_tigrinum <- searh_gbif_data(genus,species,
#' #                                        occlim=100,
#' #                                        writeFile=FALSE)
#' # ambystoma_tigrinum_clean <- clean_dup(ambystoma_tigrinum,
#' #                                     longitude = "longitude",
#' #                                     latitude = "latitude",
#' #                                     threshold=0.0)
#' # Check the dimensions of  data
#' # dim(ambystoma_tigrinum)
#' # dim(ambystoma_tigrinum_clean)
clean_dup <- function(data,longitude,latitude,threshold=0.0){
  data <- data[!is.na(data[,longitude]),]
  dat_sp <- SpatialPointsDataFrame(data[,c(longitude ,latitude)],data)
  dat_sp1 <- remove.duplicates(dat_sp, zero = threshold)
  return(dat_sp1@data)
}
