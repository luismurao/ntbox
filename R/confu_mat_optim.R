#' Function to find the cut-off threshold that optimizes the confusion matrix
#' @description The function finds out which is the best cut-off threshold that
#' optimizes the confusion matrix based on any of the entries of the confussion matrix (a,b,c,d) or by Kappa, TSS, Prevalence, Specificity, Sensibility,Correct Classification Rate , Miss Classification Rate.
#' @param sdm_raster  Continuos predicition raster.
#' @param valData A data.frame or a matrix with validation data. The data must contain
#' longitude, latitude and a column of presences (1) and absences (0).
#' @param longitude Column name of the longitude data
#' @param latitude Column name of the latitude data
#' @param pres_abs Column name of presences and absences. The presences have to represented by
#' 1 and the absences with 0.
#' @param optim_by The optimation should be by any of the following options: kappa, tss, a, b, c, d, correct_class_rate, miss_cla_rate, posit_pre_pow, nega_pre_pow, comission_error,or by omission_error.
#' @param th_range A vector with the minimum and max range of threshold values used in the searching process.
#' @param step A numeric value representing the step size to be used in the searching process.
#' @return Returns a data.frame with the values of: confusion matrix, kappa, tss, prevalence,
#' specificity, sensibility,correct classification rate (correct_class_rate), miss classification rate (miss_cla_rate), positive predictive power (posit_pre_pow), negative predictive power (nega_pre_pow), comission error (comission_error) and omission error (omission_error).
#' @references Fielding,A.H. and Bell,J. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environ. Conserv., 24, 38–49.
#' @export
#' @examples
#' \dontrun{
#' # Read a Ambystoma tigrinum model
#' sdm_model <- raster::raster(file.path(system.file("extdata",
#'                                                      package = "ntbox"),
#'                                                      "ambystoma_model.tif"))
#' # Validation data with presences and absences
#' validation_data <- utils::read.csv(file.path(system.file("extdata",
#'                                                      package = "ntbox"),
#'                                                      "ambystoma_validation.csv"))
#' # ----------------------------------------------------------------------------------
#' # Optimization by Kappa metric
#' # ----------------------------------------------------------------------------------
#' kappa_threshold <- confu_mat_optim(sdm_raster = sdm_model,
#'                                       valData = validation_data,
#'                                       longitude = "longitude",
#'                                       latitude = "latitude",
#'                                       pres_abs = "presence_absence",
#'                                       optim_by = "kappa",
#'                                       th_range = c(0.005,0.5),step = 0.005)
#'
#' # Convert continuos model to binary by using Kappa optimization threshold
#' sdm_bin_kappa <- sdm_model >= kappa_threshold$threshold[1]
#' # Plot binary model
#' raster::plot(sdm_bin_kappa)
#' # ----------------------------------------------------------------------------------
#' # Optimization by Sensibility.
#' # ----------------------------------------------------------------------------------
#' sensibility_threshold <- confu_mat_optim(sdm_raster = sdm_model,
#'                                          valData = validation_data,
#'                                          longitude = "longitude",
#'                                          latitude = "latitude",
#'                                          pres_abs = "presence_absence",
#'                                          optim_by = "sensibility",
#'                                          th_range = c(0.005,0.5),step = 0.005)
#'
#' # Convert continuos model to binary by using Sensibility optimization threshold
#' sdm_bin_sensibility <- sdm_model >= sensibility_threshold$threshold[1]
#' # Plot binary model
#' raster::plot(sdm_bin_sensibility)
#' }
confu_mat_optim <- function(sdm_raster,valData,longitude,latitude,pres_abs,optim_by="kappa",th_range=c(0,1),step=0.005){

  if (is.data.frame(valData) || is.matrix(valData)) {
    valData <- data.frame(valData)
  }
  else stop("valData must be of class data.frame or matrix")
  if (class(sdm_raster) == "raster")
    stop("sdm_raster must be of class raster")
  values <- raster::extract(sdm_raster, valData[, c(longitude,
                                                    latitude)])
  obs <- valData[, pres_abs]

  na <- which(is.na(values))
  if (length(na) > 0L) {
    values <- values[-na]
    obs <- valData[-na,pres_abs]
  }
  th_range <- seq(from = th_range[1], to = th_range[2], by = step)
  df_list <- list(numeric(length(th_range)))
  for (k in 1:length(th_range)) {
    reclass <- (values >= th_range[k]) * 1
    comb <- sapply(1:length(reclass), function(x) {
      paste0(reclass[x], obs[x])
    })
    nobs <- length(comb)
    a1 <- numeric(nobs)
    b1 <- numeric(nobs)
    c1 <- numeric(nobs)
    d1 <- numeric(nobs)
    for (i in 1:nobs) {
      if (comb[i] == "11")
        a1[i] <- 1
      if (comb[i] == "10")
        b1[i] <- 1
      if (comb[i] == "01")
        c1[i] <- 1
      if (comb[i] == "00")
        d1[i] <- 1
    }
    a <- sum(a1,na.rm = T)
    b <- sum(b1,na.rm = T)
    c <- sum(c1,na.rm = T)
    d <- sum(d1,na.rm = T)

    kappa1 <- kappa(a = a,b = b,c = c,d = d)
    tss1 <- tss(a = a,b = b,c = c,d = d)
    correct_class_rate1 <- correct_class_rate(a = a,b = b,c = c,d = d)
    prevalence <- prevalencia(a = a,b = b,c = c,d = d)

    df_list[[k]] <- data.frame(threshold=th_range[k],
                               a=a,b=b,c=c,d=d,
                               kappa=round(kappa1,4),
                               tss=round(tss1,4),
                               prevalence=round(prevalence,4),
                               specificity =round(especificidad(b,d),4),
                               sensibility = round(sensibilidad(a,c),4),
                               correct_class_rate= round(correct_class_rate1,4),
                               miss_cla_rate = round(miss_cla_rate(a = a,b = b,c = c,d = d),4),
                               posit_pre_pow = round(posit_pre_pow(a,b),4),
                               nega_pre_pow = round(nega_pre_pow (c,d),4),
                               comission_error = round(tas_fals_pos(b,d),4),
                               omission_error = round(tas_fals_neg(a,c),4))

  }

  df_sim <- do.call(rbind.data.frame,df_list)
  if(optim_by %in% c("omission_error",
                     "comission_error",
                     "miss_cla_rate")){
    df_sim  <- df_sim[ order(df_sim[,optim_by]), ]
  }
  else
    df_sim  <- df_sim[ order(-df_sim[,optim_by]), ]
  return(df_sim)
}


f_error_com <- function(b,d)    return(b/(b+d))
f_error_om <- function(a,c)  return( c/(a+c))
sensibilidad <- function(a,c)  return(a/(a+c))
especificidad <- function(b,d)  return(d/(b+d))
tas_fals_pos <- function(b,d) return(b/(b+d))
tas_fals_neg <- function(a,c) return(c/(a+c))
posit_pre_pow <- function(a,b) return(a/(a+b))
nega_pre_pow <- function(c,d) return(d/(c+d))
miss_cla_rate <- function(a,b,c,d) return((b+c)/(a+b+c+d))
prevalencia <- function(a,b,c,d) return((b + d)/(a+b+c+d))
correct_class_rate <- function(a,b,c,d) return((a + d)/(a+b+c+d))
tss <- function(a,b,c,d) return(sensibilidad(a,c)+especificidad(b,d)-1)
kappa <- function(a,b,c,d){
  N <- a+b+c+d
  term1 <- ((a+d)-(((a+c)*(a+b)+(b+d)*(c+d))/N))
  term2 <- (N-(((a+c)*(a+b)+(b+d)*(c+d))/N))
  return(term1/term2)
}

