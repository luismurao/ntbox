#' Generate random environmental background data
#'
#' @description Generate environmental background data is a function similar
#' to sampleRandom function of the raster package but optimized for Ecological
#' niche modeling.
#' @param envlayers A raster stack or brick.
#' @param nbg Number of points for the background data
#' @param nprop Proportion of environmental data to be sampled. Default NULL
#' @param coordinates Logical. If TRUE cell coordinates will be returned
#' @param cellIDs Logical. If TRUE cell IDs will be returned
#' @param rseed Random seed number. Default NULL
#' @param ncores Number of workers to run the parallel process.
#' @import future
#' @examples
#' \dontrun{
#' wcpath <- list.files(system.file("extdata/bios",
#'                                  package = "ntbox"),
#'                      pattern = ".tif$",
#'                      full.names = TRUE)
#'
#' envlayers <- raster::stack(wcpath)
#' vals <- sample_envbg(envlayers,nbg = 3583)
#' # Using a proportion of data
#' vals <- sample_envbg(envlayers,nprop = 0.20)
#' }
#' @export
sample_envbg <- function(envlayers,nbg,nprop=NULL,coordinates=FALSE,
                         cellIDs=FALSE,rseed=NULL,ncores=4){
  if(class(envlayers) == "RasterStack" ||
     class(envlayers) == "RasterBrick"){
    envlayers <- raster::stack(envlayers)
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
    if(!is.numeric(rseed))
      set.seed(rseed)
    #cat("Number of points to be sampled:",npoints)
    toSamp <- sample(nona,size = npoints,replace = FALSE)
    canP <- raster::canProcessInMemory(l1,
                                       n=raster::nlayers(envlayers))
    if(canP){
      env_bg <- envlayers[toSamp]
    }
    else {
      n_cores <- future::availableCores() -1
      if(ncores>n_cores || is.null(ncores)){
        n_cores <- n_cores
      } else{
        n_cores <- ncores
      }
      fnames <- sapply(envlayers@layers, function(x) x@file@name)
      fnames <- unique(fnames)
      indexL <- 1:raster::nlayers(envlayers)
      furrr::furrr_options(globals = c("fnames",
                                       "toSamp",
                                       "indexL"))
      plan(multisession,workers=n_cores)
      options(future.globals.maxSize= 8500*1024^2)
      env_bg <- furrr::future_map_dfc(indexL, function(x){
        if(length(fnames) == 1)
          r1 <- raster::raster(fnames,band=x)
        else
          r1 <- raster::raster(fnames[x])
        r2 <- r1[]
        d1 <- data.frame(r2[toSamp])
        names(d1) <- names(r1)
        return(d1)
      },.progress = TRUE)
      future::plan(future::sequential)
    }
    if(coordinates){
      coords <- raster::xyFromCell(l1,toSamp)
      env_bg <- data.frame(coords,env_bg)
    }
    if(cellIDs){
      env_bg <- data.frame(cellID=toSamp,env_bg)
    }

  }
  else
    stop("envlayers should be of class RasterStack or RasterBrick")
  return(env_bg)
}


