#' MOP: Extrapolation risk analysis for model transfer
#'
#' @description mop calculates a Mobility-Oriented Parity
#' @param M_stack a RasterStack of variables representing the calibration area (M area in ENM context).
#' @param G_stack a RasterStack of variables representing areas or scenarios to which models will be transferred.
#' @param percent (numeric) percent of values sampled from te calibration region to calculate the MOP.
#' @param comp_each (numeric) compute distance matrix for a each fixed number of rows (default = 2000).
#' @param parallel (logical) if TRUE, calculations will be performed in parallel using the available
#' cores of the computer. This will demand more RAM and almost full use of the CPU; hence, its use
#' is more recommended in high-performance computers. Using this option will speed up the analyses.
#' Default = FALSE
#' @param normalized (logical) if TRUE mop output will be normalized to 1.
#' @return A mobility-oriented parity RasterLayer where values of 0 represent strict extrapolation,
#' which means complete dissimilarity of environments between the calibration (M) and projection area (G).
#'
#' @details The MOP is calculated following Owens et al.
#' (2013; \url{https://doi.org/10.1016/j.ecolmodel.2013.04.011}). This function is a modification
#' of the \code{\link[ENMGadgets]{MOP}} funcion, available at \url{https://github.com/narayanibarve/ENMGadgets}.The value of the comp_each parameter dependes on the RAM memory aviable for the process; the computation can be faster if the user choose a bigger value for this parameter but you have to becarefull on memory use.
#' @importFrom future %<-%
#' @export
#'
#' @examples
#' m_stack <- raster::stack(list.files(system.file("extdata",
#'                                     package = "ntbox"),
#'                                     pattern = "M_layers.tif$",
#'                                     full.names = TRUE))
#' g_stack <- raster::stack(list.files(system.file("extdata",
#'                                     package = "ntbox"),
#'                                     pattern = "G_layers.tif$",
#'                                     full.names = TRUE))
#'
#' mop_res <- mop(M_stack = m_stack,
#'                G_stack = g_stack, percent = 10,
#'                comp_each=2000)
#' raster::plot(mop_res)

mop <- function(M_stack, G_stack, percent = 10, comp_each = 2000, parallel = FALSE,normalized=TRUE) {
  mPoints <- raster::rasterToPoints(M_stack)
  m_nona <- stats::na.omit(mPoints)
  m_naID <- attr(m_nona,"na.action")
  gPoints <- raster::rasterToPoints(G_stack)
  g_nona <- stats::na.omit(gPoints)
  g_naID <- attr(g_nona,"na.action")

  m1 <- m_nona[, -(1:2)]
  m2 <- g_nona[, -(1:2)]

  if(dim(m1)[2] != dim(m2)[2]) {
    stop("Stacks must have the same dimensions.")
  }

  steps <- seq(1, dim(m2)[1], comp_each)
  kkk <- c(steps,  dim(m2)[1] + 1)
  out_index <- plot_out(m1, m2)
  long_k <- length(kkk)

  if (!parallel) {
    mop1 <- lapply(1:(length(kkk) - 1), function(x) {
      seq_rdist <- kkk[x]:(kkk[x + 1] - 1)
      eudist <- fields::rdist(m2[seq_rdist, ], m1)
      mean_quantile <- lapply(1:dim(eudist)[1], function(y) {
        di <- eudist[y, ]
        qdi <- stats::quantile(di, probs = percent / 100,
                               na.rm = TRUE)
        ii <-  which(di <= qdi)
        return(mean(di[ii]))
      })

      avance <- (x / long_k) * 100
      cat("Computation progress: ", avance,"%" ,"\n")

      return(unlist(mean_quantile))
    })

    mop_vals <- unlist(mop1)

  }else {


    future::plan(future::multiprocess)
    mop_env <- new.env()

    pasos <- 1:(length(kkk) - 1)
    pasosChar <- paste0(pasos)

    for (paso in pasosChar) {
      x <- as.numeric(paso)
      mop_env[[paso]] %<-% {
        seq_rdist <- kkk[x]:(kkk[x + 1] - 1)
        eudist <- fields::rdist(m2[seq_rdist, ], m1)
        mop_dist <- lapply(1:dim(eudist)[1], function(y){
          di <- eudist[y, ]
          qdi <- stats::quantile(di, probs = percent / 100,
                          na.rm = TRUE)
          ii <-  which(di <= qdi)
          pond_mean <- mean(di,na.rm = TRUE)
          return(pond_mean)
        })
        mop <-unlist(mop_dist)
        return(mop)
      }
      avance <- (x / long_k) * 100
      cat("Computation progress: ", avance,"%" ,"\n")
    }
    ClusterRegistry1 <- NULL
    # ClusterRegistry is taken from the future package, for details visit:
    # https://github.com/HenrikBengtsson/future

    source(system.file("shinyApp/helpers/ClusterRegistry1.R",package = "ntbox"))

    ClusterRegistry1(action = "stop")
    mop_list <- as.list(mop_env)
    mop_names <- sort(as.numeric(names(mop_list)))
    mop_names <- as.character(mop_names)
    mop_vals <- unlist(mop_list[mop_names])
  }

  if(!is.null(g_naID)){
    mop_all <- data.frame(gPoints[,1:2])
    mop_all$mop <- NA
    mop_all$mop[-g_naID] <- mop_vals

  }else{
    mop_all <- data.frame(gPoints[, 1:2], mop = mop_vals)
  }

  mop_max <- max(stats::na.omit(mop_vals)) * 1.05
  mop_all[out_index, 3] <- mop_max
  suppressWarnings({
    sp::coordinates(mop_all) <- ~ x + y
    sp::gridded(mop_all) <- TRUE
    mop_raster <- raster::raster(mop_all)
    if(normalized)
      mop_raster <- 1 - (mop_raster / mop_max)
  })



  return(mop_raster)
}


#' Detection of environmental values ouside the calibration area of a model
#'
#' @description plot.out for calculating a mobility-oriented parity layer.
#' This function is designed to be used specifically in the \code{\link{mop}} function.
#'
#' @param M1 a numeric matrix or raster object containing values of all environmental variables in the calibration area.
#' @param G1 a numeric matrix or raster object containing values of all environmental variables in the full area of interest.
#' @export

plot_out <- function (M1, G1) {
  if(class(M1) == "RasterBrick" | class(M1) == "RasterStack" |class(M1) == "raster"){
    M1 <- raster::rasterToPoints(M1)

  }

  if(class(G1) == "RasterBrick" | class(G1) == "RasterStack" |class(G1) == "raster"){
    G1 <- raster::rasterToPoints(G1)

  }

  d1 <- dim(M1)
  AllVec <- matrix(0, 0, 0)

  for (i in 3:d1[2]) {
    MRange <- range(M1[, i])
    l1 <- which(G1[, i] < range(M1[, i])[1] | G1[,i] > range(M1[, i])[2])
    AllVec <- c(l1, AllVec)
  }

  AllVec <- unique(AllVec)

  return(AllVec)
}
