#' Exdet multivariate: NT2 metric, multivariate extrapolation risk analysis for model transfer
#'
#' @description exdet_multvar calculates NT2 metric.
#' @param M_stack a RasterStack or a Matrix of variables representing the calibration area (M area in ENM context). If M_stack is matrix it should contain the values of environmental variables as get it from \code{\link[raster]{getValues}} function.
#' @param G_stack a RasterStack or a Matrix of variables representing areas or scenarios to which models will be transferred. If G_stack is matrix it should contain the values of environmental variables as get it from \code{\link[raster]{getValues}} function.
#' @param G_mold a Raster representing the extent of the projection area. This is only necessary when G_stack is of class matrix; G_mold will we use as a mold to save the NT1 values computed by exdet_univa function.
#' @return NT2 metric (multivariate extrapolation) as a raster object.
#' @details The exdet univariate (\code{\link[ntbox]{exdet_univar}}) and multivariate (\code{\link[ntbox]{exdet_multvar}}) is calculated following:
#' Mesgaran, M.B., Cousens, R.D. & Webber, B.L. (2014) Here be dragons: a tool for quantifying novelty due to covariate range and correlation change when projecting species distribution models. Diversity & Distributions, 20: 1147–1159, DOI: 10.1111/ddi.12209.
#' @export
#' @examples
#' m_stack <- raster::stack(list.files(system.file("extdata",
#'                                                 package = "ntbox"),
#'                                     pattern = "M_layers.tif$",
#'                                     full.names = TRUE))
#' g_stack <- raster::stack(list.files(system.file("extdata",
#'                                                 package = "ntbox"),
#'                                     pattern = "G_layers.tif$",
#'                                     full.names = TRUE))
#'
#' NT2 <- exdet_multvar(M_stack = m_stack,
#'                      G_stack = g_stack)
#' raster::plot(NT2)

exdet_multvar <- function(M_stack, G_stack,G_mold=NULL) {

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

  mu_m  <- base::colMeans(m_Values,na.rm = T)
  var_m<-  stats::var(m_Values,na.rm = T)
  maha_m <- stats::mahalanobis(x = m_Values,
                               center = mu_m,
                               cov = var_m,na.rm = T)
  maha_g <- stats::mahalanobis(x = g_Values,
                               center = mu_m,
                               cov = var_m,na.rm = T)
  maha_max <- max(maha_m,na.rm = T)
  NT2 <- maha_g / maha_max
  extD[] <- NT2
  names(extD) <- "NT2"
  return(extD)
}
