#' Function to plot clusters
#' @description 3D plot of clusters
#' @param niche_data A data.frame or matrix containing niche variables
#' @param cluster_ids Cluster ids
#' @param x variable on x-axes
#' @param y variable on y-axes
#' @param z variable on z-axes
#' @param level proportion of points inside the ellipsoid
#' @param ellips Ellipsoid plot of each cluster
#' @param alpha Transparency level of ellipsoid plots
#' @param grupos Logical. Show ids of a geographic grouping variable
#' @param vgrupo A vector containing the levels of the  geographic grouping variable
#' @param cex1 Point size
#' @return Returns a data.frame with coordinate data from species
#' @export
#' @examples
#' \dontrun{
#' cluster_data <- read.csv(system.file("extdata",
#'                                      "nichekmeansCluster.csv",
#'                                       package = "ntbox"))
#'
#' ellipsoid_cluster_plot_3d(niche_data = cluster_data,
#'                           cluster_ids = cluster_data$cluster,
#'                           x = "bio1",y = "bio3",z = "bio12",
#'                           ellips = TRUE,alpha = 0.25,
#'                           grupos = F,vgrupo = NULL,
#'                           cex1 = 1,level = 0.975)
#' }

ellipsoid_cluster_plot_3d <- function(niche_data,cluster_ids,x,y,z,ellips,alpha,grupos,vgrupo,cex1=0.25,level){

  km <- cluster_ids
  nclus <- length(unique(km))
  dat_clus <- niche_data[,c(x,y,z)]

  colores <- c("black","brown4","blue","cyan",
               "darkgoldenrod","darkmagenta",
               "darkgreen","chocolate4","azure3",
               "chartreuse4","aquamarine","brown1",
               "chocolate1","coral1")

  cols <- sapply(vgrupo,function(x) return(colores[x]))

  ifelse(grupos ==FALSE,cols <- "black",cols <- cols)
  rgl::par3d(cex=.8)
  if(grupos)
    rgl::plot3d(dat_clus,type="n",col=cols)
  else
    rgl::plot3d(dat_clus,col=cols)

  if(ellips){
    for(i in 1:nclus){
      assign("x", dat_clus[km==i,1])
      assign("y", dat_clus[km==i,2])
      assign("z", dat_clus[km==i,3])
      dfn <- 3
      dfd <- length(x) - 1
      #ell.radius <- sqrt(dfn * qf(level, dfn, dfd))
      ellips <- rgl::ellipse3d(stats::cov(cbind(x,y,z)),
                               centre=c(mean(x), mean(y), mean(z)),
                               level = 0.99)
      rgl::shade3d(ellips, col=colores[i], alpha=alpha,lit=FALSE)
      #wire3d(ellips, col=i,alpha=alpha)

    }
  }

  if(grupos){

    with(dat_clus,
         rgl::text3d(dat_clus[,1],
                dat_clus[,2],
                dat_clus[,3],
                vgrupo,
                adj = c(0.5,0.5),
                useFreeType=T,
                #family="mono",
                #font=1,
                cex=cex1))
  }

}
