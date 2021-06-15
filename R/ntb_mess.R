#' ntb_mess: Multivariate Environmental Similarity Surfaces (MESS)
#'
#' @description ntb_mess calculates multivariate environmental similarity surfaces as described by Elith et al., (2010) and optimized from the \code{\link[dismo]{mess}} function of the \code{\link[dismo]{dismo-package}}.
#' @param M_stack a RasterStack or a Matrix of variables representing the calibration area (M area in ENM context). If M_stack is matrix it should contain the values of environmental variables as get it from \code{\link[raster]{getValues}} function.
#' @param G_stack a RasterStack or a Matrix of variables representing areas or scenarios to which models will be transferred. If G_stack is matrix it should contain the values of environmental variables as get it from \code{\link[raster]{getValues}} function.
#' @references Elith J., M. Kearney M., and S. Phillips, 2010. The art of modelling range-shifting species. Methods in Ecology and Evolution 1:330-342.
#' @return A rasterLayer with MESS values
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
#' messVals <- ntbox::ntb_mess(M_stack = m_stack,
#'                             G_stack = g_stack)
#' raster::plot(messVals)
ntb_mess <- function(M_stack, G_stack){

  if(class(M_stack) == "matrix")
    mMat <- M_stack
  if(class(M_stack) == "RasterStack")
    mMat <- raster::getValues(M_stack)
  if(class(G_stack) == "RasterStack"){
    gMat <- raster::getValues(G_stack)
    mess_res <- G_stack[[1]]
  }

  mMat <- stats::na.omit(mMat)
  mMat_sorted <- apply(mMat,2,sort,decreasing=F)

  if(dim(gMat)[2] == dim(mMat)[2]){

    c1 <- 1:dim(mMat)[2] %>%
      purrr::map(~.dismo_mess2(gVar =gMat[,.x],
                               mVar = mMat_sorted[,.x] ))
    min_1 <- do.call(base::pmin, c1 )
    mess_res[] <- min_1
    names(mess_res) <- "MESS"
    return(mess_res)
  }
  warning("M_stack and G_stack must have the same number of variables")
  return()
}


.dismo_mess <- function(gVar,mVar){
  nrowsM <-  length(mVar)
  comp_list <- list(gVar,mVar)
  intI <- do.call(base::findInterval, comp_list)
  f<-100*intI/nrowsM
  maxv <- max(comp_list[[2]])
  minv <- min(comp_list[[2]])
  opt1 <- 100*(comp_list[[1]]-minv)/(maxv-minv)
  opt2 <- 2*f
  opt3 <- 2 * (100-f)
  opt4 <- 100*(maxv-comp_list[[1]])/(maxv-minv)
  simi <- ifelse(f==0, opt1,
                 ifelse(f<=50, opt2,
                        ifelse(f<100, opt3,opt4)))
  return(simi)
}


.dismo_mess2 <- function(gVar,mVar){
  nrowsM <-  length(mVar)
  comp_list <- list(gVar,mVar)
  intI <- do.call(base::findInterval, comp_list)
  f<-100*intI/nrowsM
  maxv <- max(comp_list[[2]])
  minv <- min(comp_list[[2]])
  #opt1 <- 100*(comp_list[[1]]-minv)/(maxv-minv)
  #opt2 <- 2*f
  #opt3 <- 2 * (100-f)
  #opt4 <- 100*(maxv-comp_list[[1]])/(maxv-minv)
  simi <- ifelse(f==0, 100*(comp_list[[1]]-minv)/(maxv-minv),
                 ifelse(f<=50, 2*f,
                        ifelse(f<100, 2 * (100-f),
                               100*(maxv-comp_list[[1]])/(maxv-minv))))
  return(simi)
}
