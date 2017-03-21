#' Function fit an ellipsoid model
#' @description Function fit an ellipsoid model using the shape matrix (covariance matrix)
#' of the niche variables.
#' @param data A RasterStack or RasterBrick objet of the niche varibles.
#' @param centroid A vector with the values of the centers of the ellipsoid (see \code{\link[ntb]{cov_center}}).
#' @param covar The shape matrix (covariance) of the ellipoid (see \code{\link[ntb]{cov_center}}).
#' @param level The proportion of points  to be included inside the ellipsoid
#' @param threshold Threshold value for the suitabilities to be cosidered as 0,
#' by default all suiabilities <0.05 are cosidered as zeros
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
#' # d_cardon <-  read.csv(system.file("extdata", "cardon_virtual.csv", package = "nichetoolbox"))
#' ## Compute the centroid and shape (covariance matrix) of the ellipsoid model.
#' # covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=c(3,4,5))
#' ## RasterStack with the niche variables
#' # nicheStack <- stack(list.files(system.file("extdata",
#' #                   package = "nichetoolbox"),
#' #                   pattern = ".asc$",full.names = TRUE))
#' # Fitting the ellipsoid model
#' # ellipsoidMod <- ellipsoidfit(nicheStack,
#' #                          covar_centroid$centroid,
#' #                          covar_centroid$covariance,
#' #                          level=0.95,threshold=0.05,plot=TRUE,size=3)
#' # plot(ellipsoidMod$suitRaster)

ellipsoidfit <- function(data,centroid,covar,level=0.95,
                         threshold=0.05,plot=T,size,
                         xlab1="niche var 1",ylab1= "niche var 2",zlab1="Suitability",...){

  if(class(data)=="RasterStack" || class(data)=="RasterBrick"){
    resolution <- res(data)
    extention <- extent(data)
    toDF<- data.frame(rasterToPoints(data))
    coordinates <- toDF[,c(1,2)]
    data <- toDF[,-c(1,2)]

  }
  else{
    data <- data.frame(data)
  }

  # Computing the suitabilities
  suits <- suit(data,medias = centroid,covMatrix = covar)
  suits[suits<threshold] <- 0

  if(dim(data)[2]==2 && plot==TRUE){

    x <- seq(from = centroid[1]/1.5,to =centroid[1]*1.25 ,length=60)
    y <- seq(from = centroid[2]/1.5,to =centroid[2]*1.25 ,length=60)

    suit1 <- function(x,y) suit(cbind(x,y),medias = centroid,covMatrix = covar)

    z <- outer(x,y,suit1)

    p1 <- persp(x,y,z, box=T,xlab=xlab1,ylab=ylab1,zlab="",
                col="blue", theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
                ticktype = "detailed", nticks=5,
                cex.lab=1.5, cex.axis=1.3, cex.main=1.5, cex.sub=1.5)


    ranges <- t(sapply(list(x,y,z),range))
    means <- rowMeans(ranges)

    ## label offset distance, as a fraction of the plot width
    labelspace <- 0.12  ## tweak this until you like the result

    xpos <- min(x)-(diff(range(x)))*labelspace
    ypos <- min(y)-(diff(range(y)))*labelspace
    labelbot3d <- c(xpos,ypos,min(z))
    labeltop3d <- c(xpos,ypos,max(z))
    labelmid3d <- c(xpos,ypos,mean(range(z)))

    trans3dfun <- function(v) { trans3d(v[1],v[2],v[3],p1) }
    labelbot2d <- trans3dfun(labelbot3d)
    labelmid2d <- trans3dfun(labelmid3d)
    labeltop2d <- trans3dfun(labeltop3d)
    labelang <- 180/pi*atan2(labeltop2d$y-labelbot2d$y,labeltop2d$x-labelbot2d$x)
    par(xpd=NA,srt=labelang)  ## disable clipping and set string rotation
    text(labelmid2d[1]$x,labelmid2d$y,zlab1,cex=1.5)


  }

  if(dim(data)[2]==3 && plot==TRUE){

    data1 <- data[!is.na(suits),]
    dfd <- dim(data1)[1] - 1
    dfn <- dim(data1)[2] - 1
    # Ellipsoid radius
    #ell.radius_E <- sqrt(dfn * qf(level, dfn, dfd))
    suits2 <- suits[!is.na(suits)]

    #ellips_E <- ellipsoid(center=centroid,
    #                      shape=covar, radius=ell.radius_E)

    ellips_E  <- ellipse3d(covar,centre = centroid,level = 0.99)


    if(dfd > 50000)
      np <- 50000
    else
      np <- dim(data1)[1]

    toSam <- sample(1:length(data1[,1]),np)
    data1 <- data1[toSam,]

    plot3d(data1,size = size,col=hsv(suits2[toSam]*.71,.95,.9),...)
    wire3d(ellips_E, col=4, lit=FALSE,alpha=.1)
  }
  # Calculating distance to the centroid
  mahalanobis <- mahalanobis(data,
                             center = centroid,
                             cov = covar)


  ecucliedean <- sqrt(rowSums(centroid-data)^2)

  distances <- data.frame(mahalanobis,ecucliedean)

  data <- data.frame(data,ncel=1:dim(data)[1])
  if(exists('coordinates')){
    # Data Frame with coordinates and suitability values
    sDataFrame <- data.frame(coordinates,suitability=suits)
    rasterDF <- raster(extention)
    res(rasterDF) <- resolution
    cels <- cellFromXY(rasterDF,sDataFrame[,1:2])
    rasterDF[cels] <- sDataFrame[,3]
    #crs(rasterDF) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
    return(list(suits=cbind(sDataFrame,data),suitRaster=rasterDF,ncentedist=distances))
  }

  return(data.frame(suitability=suits,data,ncentedist=distances))
}


suit <- function(data,medias,covMatrix){
  a <- 1
  expo <- exp(-0.5*mahalanobis(data,medias,cov = covMatrix))
  return(a*expo)
}


# This is a helper function for drawing ellipsoids
ellipsoid <- function(center=c(0, 0, 0), radius=1, shape=diag(3), n=30){
  if (!requireNamespace("rgl")) "rgl package is missing"
  # adapted from the shapes3d demo in the rgl package
  degvec <- seq(0, 2*pi, length.out=n)
  ecoord2 <- function(p) c(cos(p[1])*sin(p[2]), sin(p[1])*sin(p[2]), cos(p[2]))
  v <- t(apply(expand.grid(degvec,degvec), 1, ecoord2))
  v <- center + radius * t(v %*% chol(shape))
  v <- rbind(v, rep(1,ncol(v)))
  e <- expand.grid(1:(n-1), 1:n)
  i1 <- apply(e, 1, function(z) z[1] + n*(z[2] - 1))
  i2 <- i1 + 1
  i3 <- (i1 + n - 1) %% n^2 + 1
  i4 <- (i2 + n - 1) %% n^2 + 1
  i <- rbind(i1, i2, i4, i3)
  rgl::qmesh3d(v, i)
}
