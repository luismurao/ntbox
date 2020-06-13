#' nc: Function to check the number of available cores
#'
#' @description nc is a helper function to check if the number of cores required to run a parallel process is less or equal to the total number of cores of the system.
#' @param ncores Number of cores for the parallel process.
#' @return  Returns an integer representing the number of cores that will be used to run a parallel process.
#' @details If ncores is bigger than the system's number of cores the function will return the system's number of cores. The functions that use this helper are \code{\link{mop}} and \code{\link{pROC}}.
#' @export
#' @examples
#' # Print the number of cores
#' print(nc(ncores=8))

nc<- function(ncores){
  nc_max <- future::availableCores()
  if(ncores > nc_max){
    warning(paste("The specified ncores are more than the system's ncores.",
                  "Runing with the system's ncores"))
    ncores <- nc_max
  }
  return(ncores)
}
