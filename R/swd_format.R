#' swd_format: Prepare data in Maxents swd format.
#' @param env_layers A raster stack or brick with the environmental information.
#' @param nbg Number of points for the background data.
#' @param occs_points A data frame with longitude lantitude data.
#' @param sp_name Species name. Default sp_name="sp".
#' @param longitude Column name containing longitude data.
#' @param latitude Column name containing latitude data.
#' @description swd_format It reshape your occurrence and background information
#' using the format samples with data (swd) to run maxent with an ordinary samples file.
#' @details The difference between the tipical way of running MaxEnt models and it
#' is  that the program doesn’t need to look in the environmental layers
#' to obtain values for the variables at the sample points or the background points.
#' each time that you call maxent (see \code{\link[ntbox]{maxent_call}}).
#' One of the main advantages is that MaxEnt runs much faster see section SWD Format here
#'  \url{https://biodiversityinformatics.amnh.org/open_source/maxent/Maxent_tutorial2017.pdf}
#' for details.
#' @return A list with occurrence or/and background points in swd format.
#' Note that coordinates correspond to pixel centroids
#' @export
#' @examples
#' \dontrun{
#' # Bioclimatic layers path
#' wcpath <- list.files(system.file("extdata/bios",
#'                                 package = "ntbox"),
#'                     pattern = ".tif$",full.names = TRUE)
#' # Bioclimatic layers
#' wc <- raster::stack(wcpath)
#' # Occurrence data for the giant hummingbird (Patagona gigas)
#' pg <- utils::read.csv(system.file("extdata/p_gigas.csv",
#'                                   package = "ntbox"))
#'
#' pg_swd <- ntbox::swd_format(env_layers=wc,
#'                             nbg=10000,
#'                             occs_points =pg,
#'                             sp_name="p_gigas",
#'                             longitude = "longitude",
#'                             latitude  = "latitude")
#'
#' head(pg_swd$occs_swd)
#'
#' head(pg_swd$bg_swd)
#'
#'}

swd_format <- function(env_layers,nbg=NULL,occs_points,sp_name="sp",longitude, latitude){
  if(is.numeric(nbg)){
    bg_swd <- ntbox::sample_envbg(env_layers,
                                  nbg = nbg,
                                  coordinates = T,
                                  cellIDs = FALSE)
    bg_swd <- data.frame(background="background",bg_swd)
  }


  occs_env <- raster::extract(env_layers,
                              occs_points[,c(longitude,latitude)],
                              cellnumbers=TRUE)
  xy_occs <- raster::xyFromCell(env_layers,
                                occs_env[,1])
  occs_swd <- data.frame(sp_name = sp_name,
                         xy_occs,occs_env[,-1])


  r1 <- list(occs_swd=occs_swd)
  if(exists("bg_swd")){
    r1[["bg_swd"]] <- bg_swd
  }
  message("Note that coordinates correspond to pixel centroids")
  return(r1)

}
