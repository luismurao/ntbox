#' Partial ROC calculation for Niche Models
#'
#' @description pROC applies partial ROC tests to continues niche models.
#'
#' @param test_data a numerical matrix containing coordinates of the occurrences used to test
#' the ecological niche model to be evaluated; columns must be: longitude and latitude.
#' @param continuos_mod a RasterLayer of the ecological niche model to be evaluated.
#' @param E_percent (numeric) value from 0 to 100 that will be used as threshold (E);
#' default = 5.
#' @param boost_percent (numeric) value from 0 to 100 representing the percent of testing data
#' to be used for performing the bootstrap process for calculating the partial ROC;
#' default = 50.
#' @param n_iter (numeric) number of bootstrap iterations to be performed;
#' default = 1000.
#' @param parallel Logical to specify if the computation will be done in parallel. default=TRUE.
#' @return A data.frame containing the AUC values and AUC ratios calculated for each iteration.
#' @details Partial ROC is calculated following Peterson et al.
#' (2008; \url{http://dx.doi.org/10.1016/j.ecolmodel.2007.11.008}). This function is a modification
#' of the \code{\link[ENMGadgets]{PartialROC}} funcion, available at \url{https://github.com/narayanibarve/ENMGadgets}.
#' @importFrom purrr map_df
#' @useDynLib ntbox
#' @export

pROC <- function(continuos_mod,test_data,n_iter=1000,E_percent=5,boost_percent=50,parallel=TRUE){

  if(continuos_mod@data@min == continuos_mod@data@max){
    stop("\nModel with no variability.\n")
  }

  continuos_mod <-round((continuos_mod/raster::cellStats(continuos_mod,
                                                         max)) * 1000)
  test_value <- raster::extract(continuos_mod,test_data)
  #test_value <- unique(test_value)
  classpixels <- data.frame(raster::freq(continuos_mod))
  classpixels <- data.frame(stats::na.omit(classpixels))

  classpixels <- classpixels  %>% dplyr::mutate_(value= ~rev(value),
                                                 count= ~rev(count),
                                                 totpixperclass = ~cumsum(count),
                                                 percentpixels= ~ totpixperclass/sum(count)) %>%
    dplyr::arrange(value)


  error_sens <- 1-(E_percent/100)
  models_thresholds <- classpixels[,"value"]
  fractional_area <- classpixels[,"percentpixels"]
  n_data <- length(test_value)
  n_samp <- ceiling((boost_percent/100)*(n_data))

  big_classpixels <- matrix(rep(models_thresholds,each=n_samp),
                            ncol=length(models_thresholds))


  calc_aucDF <- function(big_classpixels,fractional_area,
                         test_value,n_data,n_samp,error_sens){

    rowsID <- sample(x = n_data,
                     size = n_samp,
                     replace=TRUE)
    test_value1 <- test_value[rowsID]
    omssion_matrix <-   big_classpixels >  test_value1
    sensibility <- 1 - colSums(omssion_matrix)/n_samp
    xyTable <- data.frame(fractional_area,sensibility)
    less_ID <- which(xyTable$sensibility<=error_sens)
    xyTable <- xyTable[-less_ID,]

    xyTable <- xyTable[order(xyTable$fractional_area,
                             decreasing = F),]

    auc_pmodel <- trap_roc(xyTable$fractional_area,
                           xyTable$sensibility)

    auc_prand <- trap_roc(xyTable$fractional_area,
                          xyTable$fractional_area)
    auc_ratio <- auc_pmodel/auc_prand

    auc_table <- data.frame(auc_pmodel,
                            auc_prand,
                            auc_ratio =auc_ratio )
    return(auc_table)

  }


  if(parallel){

    future::plan(future::multiprocess)
    roc_env <- new.env()
    n_cores <- future::availableCores()
    niter_big <- floor(n_iter/n_cores)
    n_runs <- rep(niter_big,n_cores)
    sum_n_runs <- sum(n_runs)
    n_runs[1] <- n_runs[1] + (n_iter - sum_n_runs)

    for(i in 1:length(n_runs)){
      x <- as.character(i)
      roc_env[[x]] %<-% {
        x1 <- 1:n_runs[i]
        auc_matrix1 <- x1 %>%
          purrr::map_df(~calc_aucDF(big_classpixels,
                                    fractional_area,
                                    test_value,n_data,n_samp,
                                    error_sens))
      }
    }
    partial_AUC <- as.list(roc_env)
    rm(roc_env)
    partial_AUC <- do.call(rbind.data.frame,partial_AUC)
    rownames(partial_AUC) <- NULL
    future::plan(future::sequential)

  }
  else{

    partial_AUC <- 1:n_iter %>%
      purrr::map_df(~calc_aucDF(big_classpixels,
                                fractional_area,
                                test_value,n_data,n_samp,
                                error_sens))

  }
  #return(partial_AUC)
  mauc <- mean(partial_AUC$auc_ratio,na.rm = TRUE)
  proc <- sum(partial_AUC$auc_ratio <= 1) / length(partial_AUC$auc_ratio)
  p_roc <- c(mauc, proc)
  names(p_roc) <- c(paste("Mean_AUC_ratio_at_", E_percent, "%", sep = ""), "Partial_ROC")

  p_roc_res <- list(pROC_summary=p_roc, pROC_results=partial_AUC)
  return(p_roc_res)

}
