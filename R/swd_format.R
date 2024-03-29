#' swd_format: Prepare data in Maxents swd format.
#' @param env_layers A raster stack or brick with the environmental information.
#' @param nbg Number of points for the background data.
#' @param occs_points A data frame with longitude and latitude data.
#' @param sp_name Species name. Default sp_name="sp".
#' @param longitude Column name containing longitude data.
#' @param latitude Column name containing latitude data.
#' @param random_seed A numeric value for random seed
#' @param parallel Run the process in parallel
#' @param ncores Number of cores to run the parallel process
#' @description swd_format It reshapes your occurrence and background information
#' using the format samples with data (swd) to run maxent with an ordinary samples file.
#' @details The difference between the typical way of running MaxEnt models and it
#' is  that the program doesn’t need to look in the environmental layers
#' to obtain values for the variables at the sample points or the background points.
#' each time that you call maxent (see \code{\link[ntbox]{maxent_call}}).
#' One of the main advantages is that MaxEnt runs much faster see section SWD Format here
#'  \url{https://biodiversityinformatics.amnh.org/open_source/maxent/Maxent_tutorial2017.pdf}
#' for details.
#' @return A list with the occurrence or/and background points in swd format.
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

swd_format <- function(env_layers,nbg=NULL,occs_points,sp_name="sp",longitude,
                       latitude,random_seed=NULL,parallel=TRUE,ncores=4){
  if(is.numeric(nbg)){
    bg_swd <- ntbox::sample_envbg(env_layers,
                                  nbg = nbg,
                                  coordinates = T,
                                  cellIDs = FALSE,
                                  rseed = random_seed,
                                  parallel = parallel,
                                  ncores = ncores)
    bg_swd <- data.frame(sp_name="background",bg_swd)
  }


  occs_cellID <- raster::cellFromXY(env_layers[[1]],
                                    occs_points[,c(longitude,latitude)])

  xy_occs <- raster::xyFromCell(env_layers[[1]],
                                occs_cellID)


  canP <- raster::canProcessInMemory(env_layers[[1]],
                                     n = raster::nlayers(env_layers))
  if (canP) {
    occs_env <-env_layers[occs_cellID]
  } else{
    future::plan(future::multisession)
    fnames <- sapply(env_layers@layers, function(x) x@file@name)
    fnames <- unique(fnames)
    indexL <- 1:raster::nlayers(env_layers)

    occs_env <- furrr::future_map_dfc(indexL, function(x) {
      if (length(fnames) == 1)
        r1 <- raster::raster(fnames, band = x)
      else r1 <- raster::raster(fnames[x])
      d1 <- data.frame(r1[occs_cellID])
      names(d1) <- names(r1)
      return(d1)
    },.options = furrr::furrr_options(seed = NULL))
    future::plan(future::sequential)
  }



  occs_swd <- data.frame(sp_name = sp_name,
                         xy_occs,occs_env)


  r1 <- list(occs_swd=occs_swd)
  if(exists("bg_swd")){
    r1[["bg_swd"]] <- bg_swd
  }
  message("Note that coordinates correspond to pixel centroids")
  return(r1)

}
