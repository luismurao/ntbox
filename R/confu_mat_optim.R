#' Function to find the cut-off threshold that optimizes the confusion matrix
#' @description The function finds out which is the best cut-off threshold that
#' optimizes the confusion matrix based on Kappa or TSS
#' @param sdm_raster  SDM predicition raster
#' @param valData A data.frame or a matrix with validation data. The data must contain
#' ongitude, latitude and a column of presences (1) and absences (0).
#' @param longitude Column name of the longitude data
#' @param latitude Column name of the latitude data
#' @param pres_aus Column name of presences and absences. The presences have to represented by
#' 1 and the absences with 0.
#' @param optim_by The optimation should be by kappa (Kappa)  metric or by tss (TSS) metric.
#' @param th_range A vector with the minimum and max range of threshold values used in the searching process.
#' @param step A numeric value representing the step size to be used in the searching process.
#' @return Returns a data.frame with the values of: confusion matrix, kappa, tss, prevalence,
#' specificity, sensibility,correct classification rate (correct_class_rate), miss classification rate (miss_cla_rate),
#' positive predictive power (posit_pre_pow), negative predictive power (nega_pre_pow), comission error (comission_error)
#' omission error (omission_error).
#' @export

confu_mat_optim <- function(sdm_raster,valData,longitude,latitude,pres_aus,optim_by="kappa",th_range=c(0,1),step=0.005){

  if(is.data.frame(valData) || is.matrix(valData)){
    valData <- data.frame(valData)
  }
  else
    stop("valData must be of class data.frame or matrix")
  if(class(sdm_raster) == "raster")
    stop("sdm_raster must be of class raster")

  values <- raster::extract(sdm_raster,
                            valData[,c(longitude,
                                       latitude)])

  obs <- valData[,pres_aus]
  na <- which(is.na(values))

  if(length(na)>0L){
    values <- values[-na]
    obs <- valData[-na]

  }


  th_range <- seq(from = th_range[1],to = th_range[2],by=step)
  df_list <- list(numeric(length(th_range)))


  for(k in 1:length(th_range)){

    #reclass <- sapply(values, function(x){
    #if(x >= th_range[k]) return(1)
    #else return(0)
    #})

    reclass <-  (values >= th_range[k])*1
    comb <- sapply(1:length(reclass), function(x){
      paste0(reclass[x],pres_aus[x])
    })

    nobs <- length(comb)
    a1 <- numeric(nobs)
    b1 <- numeric(nobs)
    c1 <- numeric(nobs)
    d1 <- numeric(nobs)

    for(i in 1:nobs){
      if(comb[i] == "11") a1[i] <- 1
      if(comb[i] == "10") b1[i] <- 1
      if(comb[i] == "01") c1[i] <- 1
      if(comb[i] == "00") d1[i] <- 1
    }

    a <- sum(a1)
    b <- sum(b1)
    c <- sum(c1)
    d <- sum(d1)
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
                               omission_error = round(tas_fals_neg(a,c),4)
    )
  }

  df_sim <- do.call(rbind.data.frame,df_list)

  if(!optim_by=="kappa"){
    order2 <- "kappa"
  }
  if(optim_by=="kappa"){
    order2 <- "tss"
  }
  df_sim  <- df_sim[ order(-df_sim[,optim_by],-df_sim [,order2]), ]
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

