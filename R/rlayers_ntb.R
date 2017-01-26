#' Function to read raster layers with the same extent and resolution in all `raster` acepted formats
#' @description Read raster layers that that have the same extent and resolution,
#'              all  \link{raster} formats are accepted
#' @param layers_path Path to the folder conteining raster layers. All layers in the folder have to have the same extent and resoltion
#' @return Returns a raster stack of raster layers with the same extent and resolution
#' @export
#' @examples
#' ## RasterStack with the niche variables
#' # nicheStack <- rlayers_ntb(list.files(system.file("extdata",
#' #                   package = "nichetoolbox"),
#' #                   pattern = ".asc$",full.names = TRUE))

rlayers_ntb <- function(layers_path){
  # Regular expression to cheack for rasters layer with different formats
  ras_formats <- "(*.asc$)|(*.bil$)|(*.sdat$)|(*.rst$)|(*.nc$)|(*.tif$)|(*.envi$)|(*.img$)"
  layers_paths <- list.files(layers_path, pattern = ras_formats,full.names = TRUE)
  # Read raster layers
  layers_list <- lapply(layers_paths, raster)
  # Check resolution and extent
  resol <- unlist(lapply(layers_list,function(x) {
    ras_extent <- raster::extent(x)

    resol <- paste0(res(x),
                    c(ras_extent[1],ras_extent[2],ras_extent[3],ras_extent[4]),
                    collapse = "_")
  }))
  # Read layers only if they have the same extent and resolution
  if(length(unique(resol))==1){
    raster_stack <- stack(layers_paths)
    return(raster_stack)
  }

  else{
    warning(paste("Raster Layers in\n", layers_path,"\nhave to have the same extent and resolution"))
    return(NULL)
  }

}
