#' Exdet univariate: NT1 metric, univariate extrapolation risk analysis for model transfer
#'
#' @description exdet_univa calculates NT1 metric.
#' @param M_stack a RasterStack or a Matrix of variables representing the calibration area (M area in ENM context). If M_stack is matrix it should contain the values of environmental variables as get it from \code{\link[raster]{getValues}} function.
#' @param G_stack a RasterStack or a Matrix of variables representing areas or scenarios to which models will be transferred. If G_stack is matrix it should contain the values of environmental variables as get it from \code{\link[raster]{getValues}} function.
#' @param G_mold a Raster representing the extent of the projection area. This is only necessary when G_stack is of class matrix; G_mold will we use as a mold to save the NT1 values computed by exdet_univa function.
#' @details The exdet univariate (\code{\link[ntbox]{exdet_univar}}) and multivariate (\code{\link[ntbox]{exdet_multvar}}) is calculated following:
#' Mesgaran, M.B., Cousens, R.D. & Webber, B.L. (2014) Here be dragons: a tool for quantifying novelty due to covariate range and correlation change when projecting species distribution models. Diversity & Distributions, 20: 1147–1159, DOI: 10.1111/ddi.12209.
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
#' NT1 <- exdet_univar(M_stack = m_stack,
#'                     G_stack = g_stack,G_mold=NULL)
#' raster::plot(NT1)

exdet_univar <- function(M_stack, G_stack,G_mold=NULL) {

  if(class(M_stack) == "matrix")
    m_Values <- M_stack
  if(class(M_stack) == "RasterStack")
    m_Values <- raster::getValues(M_stack)
  if(class(G_stack) == "RasterStack"){
    extD <- G_stack[[1]]
    g_Values <- raster::getValues(G_stack)
  }
  if(class(G_stack) == "matrix" && class(G_mold) == "Raster"){
    g_Values <- G_stack
    extD <- G_mold
  }

  max_min_mvector <- sapply(1:dim(g_Values)[2],
                            function(x) range(m_Values[,x],na.rm = T))
  min_mvector <- matrix(rep(max_min_mvector[1,],
                            each=nrow(g_Values)),
                        nrow =nrow(g_Values) )
  max_mvector <- matrix(rep(max_min_mvector[2,],
                            each=nrow(g_Values)),
                        nrow =nrow(g_Values) )
  gMin <- g_Values - min_mvector
  gMax <- max_mvector - g_Values
  max_min <-  max_mvector[1,] - min_mvector[1,]

  udij <- sapply(1:dim(gMin)[2], function(x) {
    comp_vec <- data.frame(gMin[,x],gMax[,x],0)
    min_1 <- do.call(base::pmin, comp_vec)
    return( min_1 /  max_min[x])
  })

  nt1 <- base::rowSums(udij,na.rm = F)
  extD[] <- nt1
  names(extD) <- "NT1"
  return(extD)
}
