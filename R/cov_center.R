#' Function to compute the shape (covarince) and center of an ellipsoid model
#' @description Function to compute covariance and the center of an
#' ellipsoid model using the values of the niche variables of the ocurrences points
#' @param data A data.frame or a matrix with the numeric values of the variables
#' that will be used to model the niche.
#' @param mve A logical value. If TRUE a minimum volume ellipsoid will be computed using
#' the function \code{\link[MASS]{cov.mve}} of the \pkg{MASS} package. If False the covariance matrix of the input data will be used.
#' @param level A numerical value specifying the proportion of the data to be
#' used to compute the ellipsoid.
#' @param vars A numeric vector specifying the columns indexes of the variables of the input data
#' which will be used to fit the ellipsoid model. If NULL the user will be asked to enter the indexes
#' interactively
#' @return Returns a list containing the centroid of the ellipsoid, the covariance matrix based on
#' the input data and the input data.
#' @export
#' @examples
#' ## Load niche data
#' # d_cardon <-  read.csv(system.file("extdata", "cardon_virtual.csv", package = "nichetoolbox"))
#' ## Look the data
#' # head(d_cardon)
#' ## Compute the centroid and shape (covariance matrix) of the ellipsoid model.
#' ## The user will be asked to enter the variables indexes
#' #covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=NULL)
#' ## Alternatively you can specify the columns indexes of the variables
#' # covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=c(3,4,5))
#'
#' #+++++++++++++++++++++++++++++ Check the output +++++++++++++++++++++++++++++#
#' ## ellipsoid center
#' # covar_centroid$centroid
#' ## ellipsoid shape
#' # covar_centroid$covariance
#' ## Data input
#' # head(covar_centroid$data)
cov_center <- function(data,mve=TRUE,level,vars=NULL){

  if(is.null(vars)){

    nvars <- readline(prompt="Number of variables to fit the ellipsoid model:\n\n")
    data <- data.frame(data)

    allvars <- names(data)
    print(nvars)
    vars <- numeric(nvars)
    cat('Select a variable form the list:\n\n')
    for (i in 1:dim(data)[2]){
      cat(i,'.-',allvars[i],'\n')

    }
    cont <- 1
    while(0 %in% vars){
      n <- readline(prompt="Enter an option from the above list: ")

      if(n %in% 1:dim(data)[2]){
        vars[cont] <- as.numeric(n)
        cont <- cont+1
      }
      else{
        cat('Option not in the list:\n')
      }

    }
  }
  data <- data[,vars]

  if(mve){

    # Compute the number of points of our data that represent the proportion of the data
    NDquntil <- function(nD,level){
      n <- round(nD*level)
      if(n > nD)
        n <- nD
      return(n)
    }

    n <-NDquntil(dim(data)[1],level)
    # Centroid and covarianve for the Minimum volume Ellipsoid
    cent_var <- cov.mve(data,quantile.used =n)
    centroid <- cent_var$center
    vari <- cent_var$cov
  }
  else{
    centroid <- colMeans(data)
    vari <- cov(data)
  }

  # Axis length computations
  sigmaI <- solve(vari)/qchisq(level,df = dim(data)[2])
  sigIEigenSys <- eigen(sigmaI)
  sigIEval <- sigIEigenSys$values
  sigIEvec <- sigIEigenSys$vectors

  stds <- 1/sqrt(sigIEval)

  axis_length <- list()
  for(i in 1:dim(sigmaI)[1]){
    assign(paste0(letters[i]), stds[i]*2)
    axis_length[[i]] <- eval(parse(text=letters[i]))
  }

  names(axis_length) <- letters[1:dim(vari)[1]]
  axis_length <- unlist(axis_length)

  n <- dim(vari)[1]

  # Axis volume

  ellip_vol <- function(n,axis_length){
    term1 <- 2* pi^(n/2)
    term2 <- n*gamma(n/2)
    term3 <- prod(axis_length)
    term4 <- (term1/term2)*term3
    return(term4)
  }
  vol2 <- ellip_vol(n,axis_length/2)


  # Semi-axis orientation

  axis_coordinates <- list()

  for(i in 1:dim(vari)[1]){
    assign(paste0("l",i,"_inf"),
           centroid - sigIEvec[,i]*stds[i])
    assign(paste0("l",i,"_sup"),
           centroid + sigIEvec[,i]*stds[i])

    coord_matrix <- matrix(c(eval(parse(text=paste0("l",i,"_sup"))),
                             eval(parse(text=paste0("l",i,"_inf")))
    ),byrow = T,nrow = 2)
    colnames(coord_matrix) <- names(centroid)
    rownames(coord_matrix) <- paste0("vec_",1:2)
    axis_coordinates[[i]] <- coord_matrix

  }



  return(list(centroid=centroid,covariance=vari,
              niche_volume =vol2,
              SemiAxis_length=axis_length/2,
              axis_coordinates=axis_coordinates))

}
