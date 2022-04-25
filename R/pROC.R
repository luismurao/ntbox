#' Partial ROC calculation for Niche Models
#'
#' @description pROC applies partial ROC tests to continuous niche models.
#'
#' @param continuous_mod a RasterLayer or a numeric vector of the ecological niche model to be evaluated. If a numeric vector is provided it should contain the values of the predicted suitability.
#' @param test_data A numerical matrix, data.frame, or a numeric vector. If it is data.frame or matrix it should contain coordinates of the occurrences used to test the ecological niche model to be evaluated; columns must be: longitude and latitude. If numeric vector it should contain the values of the predicted suitability.
#' @param E_percent (numeric) value from 0 to 100 that will be used as a threshold (E);
#' default = 5.
#' @param boost_percent (numeric) value from 0 to 100 representing the percent of testing data
#' to be used for performing the bootstrap process for calculating the partial ROC;
#' default = 50.
#' @param n_iter (numeric) number of bootstrap iterations to be performed;
#' default = 1000.
#' @param rseed Logical. Whether or not to set a random seed. Default FALSE.
#' @param sub_sample Logical. Indicates whether the test should run using a subsample of size sub_sample_size. It is recommended for big rasters
#' @param sub_sample_size Numeric. Size of the sample to be used for computing pROC values.
#' @param parallel Logical to specify if the computation will be done in parallel. default=TRUE.
#' @param ncores Numeric; the number of cores to be used for parallelization.
#' @return A data.frame containing the AUC values and AUC ratios calculated for each iteration.
#' @details Partial ROC is calculated following Peterson et al.
#' (2008; \url{http://dx.doi.org/10.1016/j.ecolmodel.2007.11.008}). This function is a modification
#' of the PartialROC funcion, available at \url{https://github.com/narayanibarve/ENMGadgets}.
#' @import Rcpp
#' @references Peterson, A.T. et al. (2008) Rethinking receiver operating characteristic analysis applications in ecological niche modeling. Ecol. Modell., 213, 63–72.
#' @examples
#' # Load a continuous model
#' conti_model <- raster::raster(system.file("extdata",
#'                                           "ambystoma_model.tif",
#'                                            package="ntbox"))
#' # Read validation (test) data
#' test_data <- read.csv(system.file("extdata",
#'                                   "ambystoma_validation.csv",
#'                                   package = "ntbox"))
#'
#' # Filter only presences as the Partial ROC only needs occurrence data
#' test_data <- dplyr::filter(test_data, presence_absence==1)
#' test_data <- test_data[,c("longitude","latitude")]
#'
#' partial_roc <- pROC(continuous_mod=conti_model,
#'                     test_data = test_data,
#'                     n_iter=1000,E_percent=5,
#'                     boost_percent=50,
#'                     parallel=FALSE)
#'
#' @importFrom purrr map_df
#' @import future
#' @useDynLib ntbox
#' @export


pROC <- function(continuous_mod,test_data,
                 n_iter=1000,E_percent=5,
                 boost_percent=50,
                 parallel=FALSE,ncores=4,rseed=FALSE,
                 sub_sample=FALSE,sub_sample_size=10000){

  if (class(continuous_mod) == "RasterLayer") {
    if (continuous_mod@data@min == continuous_mod@data@max) {
      stop("\nModel with no variability.\n")
    }
    if (is.data.frame(test_data) || is.matrix(test_data)) {
      test_data <- stats::na.omit(raster::extract(continuous_mod,
                                                  test_data))

    }
    vals <- continuous_mod[!is.na(continuous_mod[])]
  }
  if(is.numeric(continuous_mod)){
    vals <- continuous_mod
    if (!is.numeric(test_data))
      stop("If continuous_mod is of class numeric,
           test_data must be numeric...")
  }
  if(sub_sample){
    nvals <- length(vals)
    if(sub_sample_size> nvals) sub_sample_size <- nvals
    vals <- base::sample(vals,size = sub_sample_size)
  }
  ndigits <- proc_precision(mod_vals = vals,
                            test_data = test_data)

  tomult <- as.numeric(paste0("1e+",ndigits))
  test_value <- test_data*tomult
  test_value <- round(as.vector(test_value))

  vals2 <- round(vals*tomult)
  classpixels <- as.data.frame(base::table(vals2),
                               stringsAsFactors = F)
  names(classpixels) <- c("value",
                          "count")
  classpixels$value <- as.numeric(classpixels$value)
  classpixels <- data.frame(stats::na.omit(classpixels))
  value <- count <- totpixperclass <- NULL
  classpixels <- classpixels %>%
    dplyr::mutate(value  = rev(value),
                   count = rev(count),
                   totpixperclass = cumsum(count),
                   percentpixels = totpixperclass/sum(count)) %>%
    dplyr::arrange(value)

  #if(nrow(classpixels)>1500){
  #  classpixels <- classpixels %>%
  #    dplyr::sample_n(1500) %>% dplyr::arrange(value)
  #}

  error_sens <- 1 - (E_percent/100)
  models_thresholds <- classpixels[, "value"]
  fractional_area <- classpixels[, "percentpixels"]
  n_data <- length(test_value)
  n_samp <- ceiling((boost_percent/100) * (n_data))

  big_classpixels <- matrix(rep(models_thresholds,
                                each = n_samp),
                            ncol = length(models_thresholds))


  calc_aucDF <- function(big_classpixels,
                         fractional_area,
                         test_value,
                         n_data, n_samp,
                         error_sens,rseed=NULL) {
    if(is.numeric(rseed)) set.seed(rseed)
    trapz_roc <- function(x,y){
      size_x <- length(x)
      xp <- c(x, x[size_x:1])
      yp <- c(numeric(size_x), y[size_x:1])
      nda <- 2 * size_x
      p1 <- sum(xp[1:(nda - 1)] * yp[2:nda]) + xp[nda] * yp[1]
      p2 <- sum(xp[2:nda] * yp[1:(nda - 1)]) + xp[1] * yp[nda]
      return(0.5 * (p1 - p2))
    }

    rowsID <- sample(x = n_data,
                     size = n_samp,
                     replace = TRUE)

    test_value1 <- test_value[rowsID]
    omssion_matrix <- big_classpixels > test_value1
    sensibility <- 1 - colSums(omssion_matrix)/n_samp
    xyTable <- data.frame(fractional_area, sensibility)
    xyTable <- rbind(xyTable,c(0,0))
    xyTable <- xyTable[order(xyTable$fractional_area,
                             decreasing = F),]
    auc_model <- trapz_roc(xyTable$fractional_area,
                               xyTable$sensibility)


    if(error_sens>0){
      less_ID <- which(xyTable$sensibility <= error_sens)
      xyTable <- xyTable[-less_ID, ]
      auc_pmodel <- trapz_roc(xyTable$fractional_area,
                                  xyTable$sensibility)

      auc_prand <- trapz_roc(xyTable$fractional_area,
                                 xyTable$fractional_area)

    }
    else{
      auc_pmodel <- auc_model
      auc_prand <- 0.5

    }



    auc_ratio <- auc_pmodel/auc_prand
    auc_table <- data.frame(auc_model,
                            auc_pmodel,
                            auc_prand,
                            auc_ratio)
    return(auc_table)
  }

  if (parallel) {
    n_cores <- ntbox::nc(ncores)

    furrr::furrr_options(packages = c("Rcpp","ntbox"))
    plan(multisession,workers=n_cores)
    options(future.globals.maxSize= 8500*1024^2)

    roc_env <- new.env()
    niter_big <- floor(n_iter/n_cores)
    n_runs <- rep(niter_big, n_cores)
    sum_n_runs <- sum(n_runs)
    n_runs[1] <- n_runs[1] + (n_iter - sum_n_runs)

    for (i in 1:length(n_runs)) {
      x <- as.character(i)
      roc_env[[x]] %<-% {
        library(Rcpp)
        x1 <- 1:n_runs[i]
        auc_matrix1 <- x1 %>%
          purrr::map_df(~calc_aucDF(big_classpixels,
                                    fractional_area,
                                    test_value,
                                    n_data, n_samp,
                                    error_sens,rseed=NULL))
      }
    }
    partial_AUC <- as.list(roc_env)
    rm(roc_env)
    partial_AUC <- do.call(rbind.data.frame,
                           partial_AUC)
    rownames(partial_AUC) <- NULL
    future::plan(future::sequential)
  }
  else {
    partial_AUC <- 1:n_iter %>%
      purrr::map_df(function(i){
        proc <- calc_aucDF(big_classpixels,
                           fractional_area,
                           test_value,
                           n_data,
                           n_samp,
                           error_sens,rseed = i)
      })
  }
  mauc <-  mean(partial_AUC$auc_model, na.rm = TRUE)
  maucp <- mean(partial_AUC$auc_ratio, na.rm = TRUE)
  proc <- sum(partial_AUC$auc_ratio <= 1, na.rm = TRUE)/
    length(partial_AUC$auc_ratio[!is.na(partial_AUC$auc_ratio)])

  p_roc <- c(mauc,maucp, proc)
  names(p_roc) <- c("Mean_AUC",
                    paste("Mean_pAUC_ratio_at_",
                          E_percent,
                          "%", sep = ""),
                    "P_value")
  p_roc_res <- list(pROC_summary = p_roc,
                    pROC_results = partial_AUC)
  return(p_roc_res)
}


proc_precision <- function(mod_vals,test_data){

  min_vals <- min(mod_vals,na.rm = TRUE)

  percentil_test <- unique(sort(stats::na.omit(test_data)))[2]


  #percentil_test <- stats::quantile(test_data,
  #                                  probs=0.1)
  partition_flag <- mean(c(min_vals,
                           percentil_test))
  fflag <- stringr::str_detect(partition_flag, "e")
  if (length(fflag)>0L && fflag) {
    ndigits <- stringr::str_split(partition_flag, "e-")[[1]]
    ndigits <- as.numeric(ndigits)[2] #- 1
  }
  else {
    med <- stringr::str_extract_all(partition_flag, pattern = "[0-9]|[.]")
    med <- unlist(med)
    med <- med[-(1:which(med == "."))]
    med1 <- which(med != 0)
    ndigits <- ifelse(med1[1] <= 2, 3, 4)
  }
  return(ndigits)
}

