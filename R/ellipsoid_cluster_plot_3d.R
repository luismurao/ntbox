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
  par3d(cex=.8)
  if(grupos)
    plot3d(dat_clus,type="n",col=cols)
  else
    plot3d(dat_clus,col=cols)

  if(ellips){
    for(i in 1:nclus){
      assign("x", dat_clus[km==i,1])
      assign("y", dat_clus[km==i,2])
      assign("z", dat_clus[km==i,3])
      dfn <- 3
      dfd <- length(x) - 1
      ell.radius <- sqrt(dfn * qf(level, dfn, dfd))
      ellips <- ellipsoid(center=c(mean(x), mean(y), mean(z)),
                          shape=cov(cbind(x,y,z)), radius=ell.radius)
      shade3d(ellips, col=colores[i], alpha=alpha,lit=FALSE)
      #wire3d(ellips, col=i,alpha=alpha)

    }
  }

  if(grupos){

    with(dat_clus,
         text3d(dat_clus[,1],
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
