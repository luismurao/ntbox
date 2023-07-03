#' Function to read raster layers with the same extent and resolution in all `raster` acepted formats
#' @description Read raster layers that that have the same extent and resolution,
#' all  \code{\link[raster]{raster-package}} formats are accepted (see \code{\link[raster]{writeFormats}})
#' @param layers_path Path to the folder conteining raster layers. All layers in the folder have to have the same extent and resoltion
#' @return Returns a raster stack of raster layers with the same extent and resolution
#' @export
#' @examples
#' \dontrun{
#' ## RasterStack with the niche variables
#' dir_bios <- system.file("extdata/bios",package = "ntbox")
#' nicheStack <- ntbox::rlayers_ntb(dir_bios)
#' raster::plot(nicheStack[[1]])
#' }

rlayers_ntb <- function(layers_path){
  # Regular expression to cheack for rasters layer with different formats
  ras_formats <- "(.asc$)|(.bil$)|(.sdat$)|(.rst$)|(.nc$)|(.tif$)|(.envi$)|(.img$)"
  layers_paths <- list.files(layers_path,
                             pattern = ras_formats,
                             full.names = T)
  sysinf <- Sys.info()
  os <- sysinf['sysname']
  if(os == "Windows") patt <- "\\\\" else patt <- "/"
  # Read raster layers
  layers_list <- lapply(seq_along(layers_paths), function(x){
    r <- raster::raster(layers_paths[x])
    np <- normalizePath(r@file@name)

    vs <- unlist(strsplit(np,split = patt))
    vs <- unlist(strsplit(vs[length(vs)],
                          split="[.]"))
    lname <- vs[1]
    names(r) <- lname
    return(r)
    })
  # Check resolution and extent
  resol <- unlist(lapply(layers_list,function(x) {
    ras_extent <- raster::extent(x)

    resol <- paste0(raster::res(x),
                    c(ras_extent[1],
                      ras_extent[2],
                      ras_extent[3],
                      ras_extent[4]),
                    collapse = "_")
  }))
  # Read layers only if they have the same extent and resolution
  if(length(unique(resol))==1){
    raster_stack <- raster::stack(layers_list)
    return(raster_stack)
  }

  emess <- try({
    raster_stack <- raster::stack(c(layers_paths))
  },silent = TRUE)

  if(!methods::is(emess, "try-error")){
  warning(paste("Raster Layers in\n",
                layers_path,"\n Some layer(s) have a resolution that is",
                  "not significantly different\n"))
    print(resol)
    return(raster_stack)
  }
  else{
    warning(paste("Raster Layers in\n",
                  layers_path,"\nhave to have the same extent and resolution"))
    return(NULL)
  }

}
