#' Generate random environmental background data
#'
#' @description Generate environmental background data is a function similar
#' to sampleRandom function of the raster package but optimized for Ecological
#' niche modeling.
#' @param envlayers A raster stack or brick.
#' @param nbg Number of points for the background data
#' @param nprop Proportion of environmental data to be sampled. Default NULL
#'
#' @examples
#' \dontrun{
#' wcpath <- list.files(system.file("extdata/bios",
#'                      package = "ntbox"),
#'                      pattern = ".tif$",
#'                      full.names = TRUE)
#'
#' envlayers <- raster::stack(wcpath)
#' vals <- envbg(envlayers,nbg = 3583)
#' # Using a proportion of data
#' vals <- envbg(envlayers,nprop = 0.20)
#' }

sample_envbg <- function(envlayers,nbg,nprop=NULL){
  if(class(envlayers) == "RasterStack" ||
     class(envlayers) == "RasterBrick"){
    l1 <- envlayers[[1]]
    #nona <- raster::Which(!is.na(l1),cells=TRUE)
    nona <- which(!is.na(as.vector(l1)))
    n_nona <- length(nona)
    if(!is.null(nprop)){
      npoints <- ceiling(nprop*n_nona)
    }
    else{
      npoints <- nbg
    }
    #cat("Number of points to be sampled:",npoints)
    toSamp <- sample(nona,size = npoints,replace = FALSE)
    canP <- raster::canProcessInMemory(l1,
                                       n=raster::nlayers(envlayers))
    if(canP){
      env_bg <- envlayers[toSamp]
    }
    else{
      future::plan(future::multiprocess)
      fnames <- sapply(envlayers@layers, function(x) x@file@name)
      env_bg <- furrr::future_map_dfc(fnames, function(x){
        r1 <- raster::raster(x)
        d1 <- data.frame(r1[toSamp])
        names(d1) <- names(r1)
        return(d1)
      })
      future::plan(future::sequential)
    }

  }
  else
    stop("envlayers should be of class RasterStack or RasterBrick")
  return(env_bg)
}


