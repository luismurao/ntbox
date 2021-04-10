#' spca: Principal Component Analysis for Spatial Data
#'
#' @description Performs PCA for a stack of raster layers.
#' @param layers_stack A RasterStack of environmental variables
#' @param layers_to_proj A RasterStack of the environmental variables that will be projected (default NULL). If provided
#' the function will project the PCA for those layers by using the PCA object computed for the `layers_stack`. It can also use the PCA object
#' stored in `pca_obj`.
#' @param pca_obj An object of class \code{\link[stats]{prcomp}} (default NULL). Usefull when the user already
#' has done the PCa for `layers_stack`
#' and just wants to project the PCA on the `layers_to_proj` object.
#' @param sv_dir A directory where the PCs will be saved. If NULL the PCs will not be written.
#' @param sv_proj_dir  A directory where the PCs projection will be saved. If NULL the PCs will be written inside sv_dir.
#' @param layers_format A raster format for writing PCA results (see \code{\link[raster]{writeFormats}}). Default = ".asc"
#' @return A list containing either the raster stack with Principal Components of `layers_stack` or `layers_to_proj`,
#' a barplot of the cumulative and explained variance of each compoent of `layers_stack` and a \code{\link[stats]{prcomp}} object(`pca_obj`).
#' @details spca uses the function \code{\link[stats]{prcomp}} of the `stats` package. If `sv_dir` is provided  the PCs
#' and `pca_obj` will be stored on it. The names of the layers in `layers_to_proj` need to be named
#' with exactly the same names of those either in`layers_stack` or the `pca_obj`.
#' @importFrom stats prcomp
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' # -------------------------------------------------------------------
#' # A PCA on layers_stack  without saving.
#' layers_stack <- raster::stack(list.files(system.file("extdata",
#'                                     package = "ntbox"),
#'                                     pattern = "M_layers.tif$",
#'                                     full.names = TRUE))
#' pcs <- spca(layers_stack, sv_dir=NULL,layers_format=NULL)
#' raster::plot(pcs$pc_layers)
#'
#' \dontrun{
#' # -------------------------------------------------------------------
#' # PCA projection without saving.
#'
#' layers_to_proj <- raster::stack(list.files(system.file("extdata",
#'                                                       package = "ntbox"),
#'                                           pattern = "G_layers.tif$",
#'                                           full.names = TRUE))
#' pcs_with_proj <- spca(layers_stack =layers_stack,
#'                       layers_to_proj = layers_to_proj,
#'                       pca_obj = NULL,
#'                       sv_dir=NULL,layers_format=NULL)
#' # Barplot of the Cumulative and explained variance in each component,
#' print(pcs_with_proj$pca_plot)
#' }
#' # -------------------------------------------------------------------
#' # PCA projection and saving.
#' \dontrun{
#' pcs_with_proj_sv <- spca(layers_stack =layers_stack,
#'                          layers_to_proj = layers_to_proj,
#'                          pca_obj = NULL,
#'                          sv_dir=".",layers_format=".asc")
#'
#' }
#' # -------------------------------------------------------------------
#' # PCA projection, saving and using the pca_obj.
#' \dontrun{
#'
#' pca_obj <- base::readRDS(list.files(system.file("extdata",
#'                                                 package = "ntbox"),
#'                                                 pattern = ".rds$",
#'                                                 full.names = TRUE))
#'
#' pcs_with_proj_sv <- spca(layers_stack =NULL,
#'                          layers_to_proj = layers_to_proj,
#'                          pca_obj = pca_obj,
#'                          sv_dir=".",layers_format=".tif")
#'
#' raster::plot(pcs_with_proj_sv$pcs_layers_projection)
#' }

spca <- function(layers_stack,layers_to_proj=NULL,pca_obj=NULL,sv_dir=NULL,layers_format=".asc",sv_proj_dir=NULL){

  results <- list()
  layers_stack <- raster::stack(layers_stack)
  if(class(layers_stack)=="RasterStack"){

    layers_vals <- names(layers_stack) %>% purrr::map_dfc(function(x){
      df1 <- data.frame(val = layers_stack[[x]][])
      names(df1) <- x
      return(df1)
    })
    layers_vals <- as.matrix(layers_vals)
    id_vals <- which(stats::complete.cases(layers_vals))
    #id_nas <- 1:dim(layers_vals)[1]
    #id_nas <- id_nas[-id_vals]
    layers_noNA <- layers_vals[id_vals,]

    if(class(pca_obj) != "prcomp"){
      pca_obj <- stats::prcomp(x = layers_noNA,center = TRUE,
                               scale. = TRUE)
    }

    pca_summary <- base::summary(pca_obj)
    pca_plot <- .plot_pca(pca_summary = pca_summary)

    pca_layer <- layers_stack[[1]]
    nombres_pcs <- colnames(pca_obj$x)
    if(ncol(layers_vals)>9)
      nombres_pcs[1:9] <- paste0("PC0",1:9)
    pca_estimate <- pca_obj$x
    pca_values <- rep(NA,raster::ncell(layers_stack[[1]]))

    colnames(pca_estimate) <- nombres_pcs
    layers_pca <- seq_along(nombres_pcs) %>% purrr::map(function(x){

      pca_values[id_vals] <- pca_estimate[,x]
      pca_layer[] <- pca_values
      return(pca_layer)
    }) %>% raster::stack(.)
    names(layers_pca) <- nombres_pcs

    results <- list(pc_layers=layers_pca,
                    pc_results=pca_obj,
                    pca_plot=pca_plot)

    if(!is.null(sv_dir) && dir.exists(sv_dir) && class(results$pc_layers) == "RasterStack"){
      pca_obj_file <- file.path(sv_dir,
                                paste0("pca_object",
                                       format(Sys.time(),
                                              "%y_%m_%d_%H_%M"),".rds"))

      saveRDS(pca_obj, pca_obj_file)

      n_layers <- seq_along(names(layers_pca))
      layers_path <- file.path(sv_dir,paste0(names(layers_pca),
                                             layers_format))

      pca_plot_path <- file.path(sv_dir,"pca_variance_exp.pdf")

      ggplot2::ggsave(pca_plot_path,plot = pca_plot,
                      width = 8,height = 8)


      n_layers %>% purrr::map(~raster::writeRaster(layers_pca[[.x]],
                                                   layers_path[.x],
                                                   overwrite=TRUE))

    }
  }

  if(class(layers_to_proj)=="RasterStack" && class(pca_obj) == "prcomp"){

    if(!all(names(layers_to_proj) == names(layers_pca)))
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

    layers_to_proj <- seq_along(nombres_pcs) %>% purrr::map(function(x){
      pca_values[id_vals] <- pc_projDF[,x]
      pca_layer[] <- pca_values
      return(pca_layer)
    }) %>% raster::stack(.)


    names(layers_to_proj) <- colnames(pc_projDF)
    results$pcs_layers_projection <- layers_to_proj

    if((!is.null(sv_dir) && dir.exists(sv_dir)) && is.null(sv_proj_dir))
      sv_proj_dir <- sv_dir

    if(!dir.exists(sv_proj_dir)) dir.create(sv_proj_dir)

    if(!is.null(results$pcs_layers_projection) && dir.exists(sv_proj_dir)){

      if(sv_proj_dir == sv_dir){
        sv_proj_dir <- file.path(sv_dir,"pca_projection")
        dir.create(sv_proj_dir)
      }

      layers_path_proj <- file.path(sv_proj_dir,
                                    paste0(names(layers_to_proj),
                                           layers_format))

      if(exists("layers_path_proj")){
        1:length(layers_path_proj) %>%
          purrr::map(~raster::writeRaster(layers_to_proj[[.x]],
                                          layers_path_proj[.x],
                                          overwrite=TRUE))
      }
    }
    return(results)
  }
  if(length(results)>0L) return(results)
  else
    stop("layers: must be of class RasterStack")
}


# This code is entirely adapted from
# https://www.r-graph-gallery.com/297-circular-barplot-with-groups/

.plot_pca <- function(pca_summary){
  data_pca <- data.frame(
    pca=rep(colnames(pca_summary$importance),2) ,
    group= rep(c("Cumulative Proportion","Proportion of Variance"),
               each=length(colnames(pca_summary$importance))),
    value=c(pca_summary$importance[3,],pca_summary$importance[2,])*100
  )
  group <- id <-  stat <- end <- NULL
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data_pca$group), ncol(data_pca)) )
  colnames(to_add) <- colnames(data_pca)
  to_add$group <- rep(levels(data_pca$group), each=empty_bar)
  data_pca <- rbind(data_pca, to_add)
  data_pca <- data_pca %>% dplyr::arrange(group)
  data_pca$id <- seq(1, nrow(data_pca))

  label_data <- data_pca
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     #
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  base_data <- data_pca %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title=mean(c(start,end)))

  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data),
                                     1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]

  p <- ggplot(data_pca, aes_(x=~as.factor(id), y=~value, fill=~group)) +

    geom_bar(aes_(x=~as.factor(id), y=~value, fill=~group),
             stat="identity", alpha=0.5) +

    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data,
                 aes_(x = ~end, y = 80, xend = ~start, yend = 80),
                 colour = "grey", alpha=1, size=0.3 ,
                 inherit.aes = FALSE ) +
    geom_segment(data=grid_data,
                 aes_(x = ~end, y = 60, xend = ~start, yend = 60),
                 colour = "grey", alpha=1, size=0.3 ,
                 inherit.aes = FALSE ) +
    geom_segment(data=grid_data,
                 aes_(x = ~end, y = 40, xend = ~start, yend = 40),
                 colour = "grey", alpha=1, size=0.3 ,
                 inherit.aes = FALSE ) +
    geom_segment(data=grid_data,
                 aes_(x = ~end, y = 20, xend = ~start, yend = 20),
                 colour = "grey", alpha=1, size=0.3 ,
                 inherit.aes = FALSE ) +

    # Add text showing the value of each 100/75/50/25 lines
    #annotate("text", x = rep(max(data_pca$id),4),
    #         y = c(20, 40, 60, 80),
    #         label = c("20", "40", "60", "80") ,
    #         color="#A8A8A8", size=3.5 , angle=0,
    #         fontface="bold", hjust=1) +

    geom_bar(aes_(x=~as.factor(id), y=~value,
                  fill=~group), stat="identity",
             alpha=0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm")
    ) +
    coord_polar() +
    geom_text(data=label_data,
              aes_(x=~id, y=~value+10,
                   label=~paste(pca,"\n",value),
                   hjust=~hjust), color="black",
              fontface="bold",alpha=0.6,
              size=2.85, angle= label_data$angle,
              inherit.aes = FALSE ) +

    # Add base line information
    geom_segment(data=base_data,
                 aes_(x = ~start, y = -5, xend = ~end, yend = -5),
                 colour = "black", alpha=0.8, size=0.6 ,
                 inherit.aes = FALSE )  +
    geom_text(data=base_data,
              aes_(x = ~title, y = -18, label=~group),
              hjust=c(1,0), colour = "black",
              alpha=0.8, size=4, fontface="bold",
              inherit.aes = FALSE)

  return(p)
}
