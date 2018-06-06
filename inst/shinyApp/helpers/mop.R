mop <- function(m_stack,g_stack,percentil_prop=0.10,
                compute_each=1000,normalized=FALSE){
  mPoints <- rasterToPoints(m_stack)
  gPoints <- rasterToPoints(g_stack)
  m1 <- mPoints[, -(1:2)]
  m2 <- gPoints[,- (1:2)]

  if(dim(m1)[2] != dim(m2)[2])
    stop("Stacks must have the same dimensions")

  steps <- seq(1,dim(m2)[1],compute_each)
  kkk <-c(steps,  dim(m2)[1]+1)
  out_index <- PlotOut(m1,m2)
  long_k <- length(kkk)

  mop1 <- lapply( 1:(length(kkk)-1), function(x){
    seq_rdist <- kkk[x]:(kkk[x+1]-1)
    eudist <- rdist(m2[seq_rdist,],m1)
    mean_quantile <- parallel::mclapply(1:dim(eudist)[1],function(y){
      di <- eudist[y,]
      qdi <- quantile(di,probs=percentil_prop,na.rm=T)
      ii <-  which(di<=qdi)
      return(mean(di[ii]))
    })
    avance <- (x/long_k)*100
    cat("Computation progress: ", avance,"%" ,"\n")
    return(unlist(mean_quantile))
  })
  mop2 <- unlist(mop1)
  MOP_ALL <- data.frame(gPoints[,1:2],mop2)
  mop_max <- max(mop2)
  MOP_ALL[out_index,3] <- mop_max*1.1
  coordinates(MOP_ALL) <- ~x+y
  gridded(MOP_ALL) <- TRUE
  mop_raster <- raster(MOP_ALL)

  colramp <- colorRampPalette(c("#2cd81c","#385caa",
                                "#1825df","black"))(226)

  if(normalized){
    mop_raster <- 1 - (mop_raster/mop_max)
    colramp <- rev(colramp)
  }

  return(mop_raster)
}




## This function collects all the rows outside the range of any of the environmental variable and will be plotted in black color later.
PlotOut <- function (M1, G1)
{
  d1 = dim(M1)
  AllVec = matrix(0,0,0)
  for (i in 3:d1[2])
  {
    MRange = range(M1[,i])
    l1 = which(G1[,i] < range(M1[,i])[1] | G1[,4] > range(M1[,4])[2])
    AllVec = c(l1,AllVec)
  }
  AllVec = unique(AllVec)

  return(AllVec)
}
