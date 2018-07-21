#' Function to compute the Minimum Volume covariance Matrix of an ellipsoid niche model.
#' @description Function to compute the covariance matrix, the niche centroid and volume of an
#' ellipsoid model. It uses the values of the niche variables of the ocurrences points.
#' @param data A data.frame or a matrix with the numeric values of the variables
#' that will be used to model the niche.
#' @param mve A logical value. If TRUE a minimum volume ellipsoid will be computed using
#' the function \code{\link[MASS]{cov.mve}} of the \pkg{MASS} package. If False the covariance matrix of the input data will be used.
#' @param level A numerical value specifying the proportion of the data to be
#' used to compute the ellipsoid.
#' @param vars A numeric or a string vector specifying the columns indexes/names of the variables of the input data which will be used to fit the ellipsoid model. If NULL the user will be asked to enter the indexes.
#' interactively
#' @return Returns a list containing the centroid of the ellipsoid, the covariance matrix based on
#' the input data, ellipsoid volume, semi-axis length and axis coordinates.
#' @export

cov_center <- function (data, mve = TRUE, level, vars = NULL)
{
  if (is.null(vars)) {
    nvars <- readline(prompt = "Number of variables to fit the ellipsoid model:\\n\\n")
    data <- data.frame(data)
    allvars <- names(data)
    print(nvars)
    vars <- numeric(nvars)
    cat("Select a variable form the list:\\n\\n")
    for (i in 1:dim(data)[2]) {
      cat(i, ".-", allvars[i], "\\n")
    }
    cont <- 1
    while (0 %in% vars) {
      n <- readline(prompt = "Enter an option from the above list: ")
      if (n %in% 1:dim(data)[2]) {
        vars[cont] <- as.numeric(n)
        cont <- cont + 1
      }
      else {
        cat("Option not in the list:\\n")
      }
    }
  }
  data <- data[, vars]
  if (mve) {

    NDquntil <- function(nD, level) {
      n <- floor(nD * level)
      if (n > nD)
        n <- nD
      return(n)
    }
    n <- NDquntil(dim(data)[1], level)
    cent_var <- MASS::cov.mve(data, quantile.used = n)
    centroid <- cent_var$center
    vari <- cent_var$cov
  }
  else {
    centroid <- colMeans(data)
    vari <- stats::cov(data)
  }
  # Ellipsoid volume
  ellip_vol <- function(n, axis_length) {
    term1 <- 2 * pi^(n/2)
    term2 <- n * gamma(n/2)
    term3 <- prod(axis_length)
    term4 <- (term1/term2) * term3
    return(term4)
  }

  sigmaI <- solve(vari)/stats::qchisq(level, df = dim(data)[2])
  sigIEigenSys <- eigen(sigmaI)
  sigIEval <- sigIEigenSys$values
  sigIEvec <- sigIEigenSys$vectors
  stds <- 1/sqrt(sigIEval)
  axis_length <- NULL
  for (i in 1:dim(sigmaI)[1]) {
    axis_length[i] <- stds[i] * 2
  }
  names(axis_length) <- letters[1:dim(vari)[1]]
  n <- dim(vari)[1]

  vol2 <- ellip_vol(n, axis_length/2)
  axis_coordinates <- list()
  for (i in 1:dim(vari)[1]) {
    assign(paste0("l", i, "_inf"),
           centroid - sigIEvec[,i] * stds[i])
    assign(paste0("l", i, "_sup"),
           centroid + sigIEvec[,i] * stds[i])
    coord_matrix <- matrix(c(eval(parse(text = paste0("l",
                                                      i, "_sup"))),
                             eval(parse(text = paste0("l", i, "_inf")))),
                           byrow = T, nrow = 2)
    colnames(coord_matrix) <- names(centroid)
    rownames(coord_matrix) <- paste0("vec_", 1:2)
    axis_coordinates[[i]] <- coord_matrix
  }
  return(list(centroid = centroid,
              covariance = vari,
              niche_volume = vol2,
              SemiAxis_length = axis_length/2,
              axis_coordinates = axis_coordinates))
}
