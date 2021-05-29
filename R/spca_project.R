#' spca_project: Project Principal Component Analysis in various spatio-temporal
#' scenarios in bash mode
#'
#' @description A simple function to project Principal Component Analysis for Spatial Data
#' in various spatial-temporal scenarios.
#' @details proj_dir_names and layers_to_proj_dirs must have the same length.
#' If user does not provide names for projection directories, ntbox will create
#' directories for each projection scenario.
#' @details It uses the function \code{\link[ntbox]{spca}} to compute PCs
#' @param layers_stack A RasterStack of environmental variables.
#' @param pca_obj An object of class \code{\link[stats]{prcomp}} (default NULL). Usefull when the user already
#' has done the PCa for `layers_stack`.
#' @param layers_to_proj_dirs Paths to directories of projection layers
#' @param sv_dir A directory where the PCs will be saved. If NULL the PCs will not be written.
#' @param pca_proj_dir_names A vector with directory names where PCs will be saved. For example, pca_ccsm4_2050_6
#' @param layers_format A raster format for writing PCA results (see \code{\link[raster]{writeFormats}}). Default = ".asc"
#' @export

spca_project <- function(layers_stack,layers_to_proj_dirs,
                         sv_dir,pca_proj_dir_names=NULL,
                         pca_obj=NULL,layers_format=".tif"){

  layers_stack <- raster::stack(layers_stack)

  proj_layers_stackL <- lapply(layers_to_proj_dirs, function(pth){
    ntbox::rlayers_ntb(pth)
  })

  if(!dir.exists(sv_dir)) dir.create(sv_dir)
  sv_dir <- normalizePath(sv_dir)
  sv_dir_reference <- file.path(sv_dir,"pca_reference_layers")
  slength <- length(layers_to_proj_dirs) == length(pca_proj_dir_names)
  if(!slength){
    cat(paste("layers_to_proj_dirs and pca_proj_dir_names do not have",
                  "the same length.\n Creating directory names for pca projection scenarios\n\n"))
    pca_proj_dir_names <- stringr::str_split(layers_to_proj_dirs,"/") %>%
      sapply(function(x){
      return(paste0("pca_",x[length(x)]))
    })
  }

  if(class(pca_obj) != "prcomp" && class(layers_stack)=="RasterStack"){
    pcs <- spca(layers_stack =layers_stack,
                pca_obj = NULL,
                sv_dir=sv_dir_reference,
                layers_format=layers_format)
    pca_obj <- pcs$pc_results
  }
  pca_dir_paths <- file.path(sv_dir,pca_proj_dir_names)
  sdirs <- sapply(pca_dir_paths,
                  function(dir_name)
                    if(!dir.exists(dir_name)) dir.create(dir_name))

  res_proj <- seq_along(pca_dir_paths) %>% purrr::map(function(x){
    layers_to_proj <- proj_layers_stackL[[x]]

    if(class(layers_to_proj)=="RasterStack" && class(pca_obj) == "prcomp"){

      nombres_pcs <- colnames(pca_obj$x)
      if(length(nombres_pcs)>9)
        nombres_pcs[1:9] <- paste0("PC0",1:9)

      if(!all(names(layers_to_proj) == names(layers_stack)))
        cat(paste("Assuming that the layers that have the same position in the",
                  "stack represent the same kind of variables\n\n"))

      layers_to_proj <- layers_to_proj[[1:length(names(pca_obj$center))]]

      names(layers_to_proj) <- names(pca_obj$center)
      proj_data <- raster::getValues(layers_to_proj)
      id_vals <- which(stats::complete.cases(proj_data))
      proj_noNA <-  proj_data [id_vals,]

      pc_projDF <- stats::predict(pca_obj,newdata=proj_noNA)

      if(length(colnames( pc_projDF ))>9)
        colnames(pc_projDF)[1:9] <- paste0("PC0",1:9)
      pca_layer <- layers_to_proj[[1]]
      pca_values <- rep(NA,raster::ncell(pca_layer))

      layers_to_proj <- raster::stack(seq_along(nombres_pcs) %>% purrr::map(function(x){
        pca_values[id_vals] <- pc_projDF[,x]
        pca_layer[] <- pca_values
        return(pca_layer)
      }))

      names(layers_to_proj) <- colnames(pc_projDF)

      layers_path_proj <- file.path(pca_dir_paths[x],
                                    paste0(names(layers_to_proj),
                                           layers_format))
      1:length(layers_path_proj) %>%
        purrr::map(function(x){
          raster::writeRaster(layers_to_proj[[x]],
                              layers_path_proj[x],
                              overwrite=TRUE)
        })
      return(layers_to_proj)
    }
  })
  return()
}
