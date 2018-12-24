#' Function to find out strong correlations in a correlation matrix
#' @description The function finds out which variables have strong
#' correlations according to a correlation threshold. The output
#' returns a list of variables names that can summarize the information
#' and removes the variables that are redundant.
#' @param cor_mat A correlation matrix.
#' @param threshold Threshold valuefrom which it is considered that the correlation is high.
#' @param verbose Verbose output.
#' @return Returns a vector with variable names that can summarize the information.
#' @export
#' @examples
#' \dontrun{
#' environ_data <- read.csv(system.file("extdata",
#'                                      "nichekmeansCluster.csv",
#'                                       package = "ntbox"))[,-(1:3)]
#' environ_cors <- cor(environ_data)
#' correlation_finder(cor_mat = environ_cors ,threshold = 0.7,verbose = TRUE)
#' }

correlation_finder <- function(cor_mat,threshold,verbose=TRUE){
  if(is.matrix(cor_mat) || is.data.frame(cor_mat)){
    cor_mat <- data.frame(cor_mat)
    variables_cor <- function(x,threshold){
      x <- as.numeric(x)
      vars_pos <- which(x > threshold)
      vars_neg <- which(x < (-1)*threshold)
      vars <- c(vars_pos,vars_neg)
      cors <- x[vars]
      names(cors) <- names(cor_mat)[vars]
      return(cors)
    }

    list_cor <- mapply(variables_cor,x=cor_mat,threshold=threshold,SIMPLIFY = FALSE)

    nomvar <- names(cor_mat)
    list_cor2 <- sapply(nomvar, function(x) list_cor[[x]][which(list_cor[[x]]!=1)])


    nomvar2 <- nomvar
    descriptors <- NULL
    for(i in 1:length(list_cor2)){
      descriptors <- c(descriptors,names(list_cor2[i]))
      if(names(list_cor2[i]) %in% nomvar2){
        trash <- sort(unlist(sapply(c(descriptors,
                                      names(list_cor2[[i]])),
                                    function(x) which(x==nomvar2))))
        nomvar2 <- nomvar2[-trash]
      }
      else{
        descriptors <- descriptors[-length(descriptors)]
      }
    }


    if(verbose){
      cat('*****************************************************************\n\n')
      cat(' Here is a list of variables that can summarize your niche\n')
      cat(' information, according to the threshold of',threshold,":\n\n")
      cat(' ',descriptors,'\n\n')
      cat('*****************************************************************\n\n')

      cat('----------------------------------------------------------------\n\n')
      cat('Correlation list:\n\n')

      for(i in 1:dim(cor_mat)[1]){
        cat("Variable",names(list_cor)[i],"is strongly correlated with:\n\n")
        print(list_cor[[i]])
        cat('----------------------------------------------------------------\n\n')
      }
      return()
    }
    return(list(descriptors=descriptors,list_cor=list_cor))
  }
  else
    stop("cor_mat must be a matrix or a data.frame")
}
