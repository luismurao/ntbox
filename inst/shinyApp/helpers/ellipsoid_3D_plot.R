
ellipsoid_plot_3d <- function(suits,data,covar,centroid,level=0.95,xlab1="",ylab1="",zlab1="Suitability"){
  dim_data <- dim(data)[2]
  if(dim_data == 3){
    data1 <- data[!is.na(suits), ]
    dfd <- dim(data1)[1] - 1
    dfn <- dim(data1)[2] - 1
    ell.radius_E <- sqrt(dfn * qf(level, dfn, dfd))
    suits2 <- suits[!is.na(suits)]
    ellips_E <- ellipsoid(center = centroid, shape = covar,
                          radius = ell.radius_E)
    if (dfd > 50000)
      np <- 50000
    else np <- dim(data1)[1]
    toSam <- sample(1:length(data1[, 1]), np)
    data1 <- data1[toSam, ]
    plot3d(data1, size = 3, col = hsv(suits2[toSam] *
                                        0.71, 0.95, 0.9))
    wire3d(ellips_E, col = 4, lit = FALSE, alpha = 0.1)
  }
  if(dim_data == 2){

    x <- seq(from = centroid[1]/1.5, to = centroid[1] * 1.25,
             length = 60)
    y <- seq(from = centroid[2]/1.5, to = centroid[2] * 1.25,
             length = 60)
    suit <- function(data,medias,covMatrix){
      a <- 1
      expo <- exp(-0.5*mahalanobis(data,medias,cov = covMatrix))
      return(a*expo)
    }
    suit1 <- function(x, y) suit(cbind(x, y), medias = centroid,
                                 covMatrix = covar)
    z <- outer(x, y, suit1)

    p1 <- persp(x, y, z, box = T, xlab = xlab1, ylab = ylab1,
                zlab = "", col = "blue", theta = 55, phi = 30, r = 40,
                d = 0.1, expand = 0.5, ticktype = "detailed", nticks = 5,
                cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.5, cex.sub = 1.5)
    ranges <- t(sapply(list(x, y, z), range))
    means <- rowMeans(ranges)
    labelspace <- 0.12
    xpos <- min(x) - (diff(range(x))) * labelspace
    ypos <- min(y) - (diff(range(y))) * labelspace
    labelbot3d <- c(xpos, ypos, min(z))
    labeltop3d <- c(xpos, ypos, max(z))
    labelmid3d <- c(xpos, ypos, mean(range(z)))
    trans3dfun <- function(v) {
      trans3d(v[1], v[2], v[3], p1)
    }
    labelbot2d <- trans3dfun(labelbot3d)
    labelmid2d <- trans3dfun(labelmid3d)
    labeltop2d <- trans3dfun(labeltop3d)
    labelang <- 180/pi * atan2(labeltop2d$y - labelbot2d$y,
                               labeltop2d$x - labelbot2d$x)
    par(xpd = NA, srt = labelang)
    text(labelmid2d[1]$x, labelmid2d$y, zlab1, cex = 1.5)
  }
}
