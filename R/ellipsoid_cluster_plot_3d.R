#' Function to plot clusters in 3-dimsensions.
#' @description Plot cluster data in 3D by modelling them as an Ellipsoid.
#' @param niche_data A data.frame or matrix containing niche variables
#' @param cluster_ids Cluster ids
#' @param x variable on x-axes
#' @param y variable on y-axes
#' @param z variable on z-axes
#' @param mve A logical value. If TRUE a minimum volume ellipsoid will be computed using
#' the function \code{\link[MASS]{cov.rob}} of the \pkg{MASS} package. If False the covariance matrix of the input data will be used.
#' @param level proportion of points inside the ellipsoid.
#' @param ellips Ellipsoid plot of each cluster
#' @param alpha Transparency level of ellipsoid plots
#' @param grupos Logical. Show ids of a geographic grouping variable
#' @param vgrupo A vector containing the levels of the  geographic grouping variable
#' @param cex1 Point size
#' @return Returns a list of the ellipsoid metada for each cluster.
#' @details The output of this function is the same of the \code{\link[ntbox]{cov_center}} function.
#' @export
#' @examples
#' \dontrun{
#' cluster_data <- read.csv(system.file("extdata",
#'                                      "nichekmeansCluster.csv",
#'                                       package = "ntbox"))
#'
#' ellipsoid_clusters <- ellipsoid_cluster_plot_3d(niche_data =environ_data[,c("bio5","bio6","bio12")],
#'                                                 cluster_ids = cluster_data$cluster,
#'                                                 x = "bio5",y="bio6",z="bio12",mve = T,
#'                                                 ellips = T,alpha = 0.25,
#'                                                 grupos = T,vgrupo =cluster_data$cluster,
#'                                                 cex1 = 1,level = 0.975)
#' # Print metadata for the Minimum Volume Ellipsoid that belongs to cluster 1
#' print(ellipsoid_clusters$cluster_n_1)
#' }

ellipsoid_cluster_plot_3d <- function(niche_data,cluster_ids,x,y,z,mve=FALSE,ellips=TRUE,level=0.975,alpha,grupos,vgrupo,cex1=0.25){

  dat_clus <- data.frame(cluster_ids=cluster_ids, niche_data[,c(x,y,z)])

  colores <- c("black","brown4","blue","cyan",
               "darkgoldenrod","darkmagenta",
               "darkgreen","chocolate4","azure3",
               "chartreuse4","aquamarine","brown1",
               "chocolate1","coral1")

  cols <- sapply(vgrupo,function(x) return(colores[x]))

  ifelse(grupos ==FALSE,cols <- "black",cols <- cols)
  rgl::par3d(cex=.8)
  if(grupos)
    rgl::plot3d(dat_clus[,c(x,y,z)],type="n",col=cols)
  else
    rgl::plot3d(dat_clus[,c(x,y,z)],col=cols)

  if(ellips){
    plot_clusters <- split(x = dat_clus,
                           f = dat_clus[,"cluster_ids"]) %>%
      purrr::map(~.plot_ellipsoid3d(.x[,c(x,y,z)],
                                    alpha = alpha,
                                    level = level,
                                    color_index = .x$cluster_ids,
                                    colores=colores))
    names(plot_clusters) <- paste0("cluster_n_",names(plot_clusters))
  }

  if(grupos){

    with(dat_clus,
         rgl::text3d(dat_clus[,x],
                dat_clus[,y],
                dat_clus[,z],
                vgrupo,
                adj = c(0.5,0.5),
                #useFreeType=F,
                #family="mono",
                #font=1,
                cex=cex1))
  }
  return(invisible(plot_clusters))

}

.plot_ellipsoid3d <- function(data,mve=F,color_index,alpha,level,colores){

  if(mve){
    centroid_shape <- ntbox::cov_center(data,mve=T,
                                        level=level,
                                        vars = 1:3)
  }
  else{
    centroid_shape <- ntbox::cov_center(data,mve=F,
                                        level=level,
                                        vars = 1:3)
  }


  ellips <- rgl::ellipse3d(centroid_shape$covariance,
                           centre=centroid_shape$centroid,
                           level = 0.99)
  rgl::shade3d(ellips,
               col=colores[color_index],
               alpha=alpha,lit=FALSE)
  return(centroid_shape)
}
