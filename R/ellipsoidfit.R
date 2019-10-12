#' Function fit an ellipsoid model
#' @description Function fit an ellipsoid model using the shape matrix (covariance matrix)
#' of the niche variables.
#' @param envlayers A RasterStack or RasterBrick objet of the niche varibles.
#' @param centroid A vector with the values of the centers of the ellipsoid (see \code{\link[ntbox]{cov_center}}).
#' @param covar The shape matrix (covariance) of the ellipoid (see \code{\link[ntbox]{cov_center}}).
#' @param level The proportion of points  to be included inside the ellipsoid
#' @param plot Logical If True a plot of niche will be shown.
#' @param size The size of the points of the niche plot.
#' @param xlab1 For x label for 2-dimensional histogram
#' @param ylab1 For y label for 2-dimensional histogram
#' @param zlab1 For z label for 2-dimensional histogram
#' @param ... Arguments passed to \code{\link[rgl]{plot3d}} function from rgl
#' @return Returns a list containing a data.frame with the suitability values; a suitability raster;
#' a data.frame with the mahalanobis and euclidean distances to the centroid.
#' @export
#' @examples
#' ## Load niche data
#' \dontrun{
#' d_cardon <-  read.csv(system.file("extdata", "cardon_virtual.csv", package = "ntbox"))
#' ## Compute the centroid and shape (covariance matrix) of the ellipsoid model.
#' covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=c(3,4,5))
#' ## RasterStack with the niche variables
#' nicheStack <- stack(list.files(system.file("extdata",
#'                    package = "ntbox"),
#'                    pattern = ".asc$",full.names = TRUE))
#' # Fitting the ellipsoid model
#'  ellipsoidMod <- ellipsoidfit(nicheStack,
#'                           covar_centroid$centroid,
#'                           covar_centroid$covariance,
#'                           level=0.99,plot=TRUE,size=3)
#'  plot(ellipsoidMod$suitRaster)
#' }


ellipsoidfit <- function(envlayers,centroid,covar,level=0.95,
                         plot=T,size,
                         xlab1="niche var 1",ylab1= "niche var 2",zlab1="S",...){

  if(class(envlayers)=="RasterStack" || class(envlayers)=="RasterBrick"){
    resolution <- raster::res(envlayers)
    extention <- raster::extent(envlayers)
    env_vars <- raster::getValues(envlayers)
    coordinates <- sp::coordinates(envlayers)
    suitRaster <- envlayers[[1]]
    #toDF<- data.frame(raster::rasterToPoints(data))
    #coordinates <- toDF[,c(1,2)]
    #data <- toDF[,-c(1,2)]

  }
  else{
    env_vars <- data.frame(envlayers)
  }

  # Calculating distance to the centroid
  mahalanobisD <- stats::mahalanobis(env_vars,
                                    center = centroid,
                                    cov = covar)


  ecucliedean <- sqrt(rowSums(centroid-env_vars)^2)


  suit <- function( mahalanobisD){
    a <- 1
    expo <- exp(-0.5* mahalanobisD)
    return(a*expo)
  }
  # Computing the suitabilities
  suits <- suit( mahalanobisD)
  #suits[suits<threshold] <- 0

  if(dim(env_vars)[2]==2 && plot==TRUE){

    x <- seq(from = centroid[1]/2,to =centroid[1]*2 ,length=100)
    x <- sort(x)
    y <- seq(from = centroid[2]/2,to =centroid[2]*2 ,length=100)
    y <- sort(y)
    #maha1 <- stats::mahalanobis(cbind(x,y),
    #                            center = centroid,
    #                            cov = covar)
    suit1 <- function(x,y) {
      maha1 <- stats::mahalanobis(cbind(x,y),
                                  center = centroid,
                                  cov = covar)
      expo <- exp(-0.5* maha1)
      return(expo)
    }
    #z <- x %o% y
    z <- outer(x,y,FUN = suit1)

    p1 <- graphics::persp(x,y,z, box=T,xlab=xlab1,
                          ylab=ylab1,zlab=zlab1, col="blue",
                          theta = 55, phi = 30,r = 40,
                          d = 0.1, expand = 0.5,
                          ticktype = "detailed", nticks=5,
                          cex.lab=1.5, cex.axis=1.3,
                          cex.main=1.5, cex.sub=1.5)


    ranges <- t(sapply(list(x,y,z),range))
    means <- rowMeans(ranges)

    ## label offset distance, as a fraction of the plot width
    labelspace <- 0.12  ## tweak this until you like the result

    xpos <- min(x)-(diff(range(x)))*labelspace
    ypos <- min(y)-(diff(range(y)))*labelspace
    labelbot3d <- c(xpos,ypos,min(z))
    labeltop3d <- c(xpos,ypos,max(z))
    labelmid3d <- c(xpos,ypos,mean(range(z)))

    trans3dfun <- function(v) { grDevices::trans3d(v[1],v[2],v[3],p1) }
    labelbot2d <- trans3dfun(labelbot3d)
    labelmid2d <- trans3dfun(labelmid3d)
    labeltop2d <- trans3dfun(labeltop3d)
    labelang <- 180/pi*atan2(labeltop2d$y-labelbot2d$y,labeltop2d$x-labelbot2d$x)
    graphics::par(xpd=NA,srt=labelang)  ## disable clipping and set string rotation
    graphics::text(labelmid2d[1]$x,labelmid2d$y,zlab1,cex=1.5)


  }

  if(dim(env_vars)[2]==3 && plot==TRUE){

    data1 <- env_vars[!is.na(suits),]
    dfd <- dim(data1)[1] - 1
    dfn <- dim(data1)[2] - 1
    # Ellipsoid radius
    #ell.radius_E <- sqrt(dfn * qf(level, dfn, dfd))
    suits2 <- suits[!is.na(suits)]

    ellips_E  <- rgl::ellipse3d(covar,centre = centroid,level = 0.99)


    if(dfd > 50000)
      np <- 50000
    else
      np <- dim(data1)[1]

    toSam <- sample(1:length(data1[,1]),np)
    data1 <- data1[toSam,]

    rgl::plot3d(data1,size = size,col=grDevices::hsv(suits2[toSam]*.71,.95,.9),...)
    rgl::wire3d(ellips_E, col=4, lit=FALSE,alpha=.1)
  }

  distances <- data.frame(mahalanobisD,ecucliedean)

  data <- data.frame(env_vars,ncel=1:dim(env_vars)[1])
  #data <- na.omit(data)

  if(exists('coordinates')){
    distances <- data.frame(coordinates,
                            env_vars,
                            distances,
                            ncel=1:dim(env_vars)[1])
    distances <- stats::na.omit(distances)
    suitsDF <- stats::na.omit(data.frame(coordinates,suitability=suits,env_vars))
    suitRaster[] <- suits
    names(suitRaster) <- "suitability"
    return(list(suits=suitsDF,suitRaster=suitRaster,ncentedist=distances))
  }

  return(data.frame(suitability=suits,env_vars,ncentedist=distances))
}
