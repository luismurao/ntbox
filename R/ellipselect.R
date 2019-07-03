#' ellipsoid_selection: Performs variable selection for ellipsoid models
#'
#' @description Performs variable selection for ellipsoid models according to omission rates in the environmental space.
#' @param env_train A data frame with the environmental training data.
#' @param env_vars A vector with the names of environmental variables to be used in the selection process
#' @param nvarstest A vector indicating the number of variables to fit the ellipsoids during model selection. It is allowed to test models with different number of variables (i.e. nvarstest=c(3,6)).
#' @param level Proportion of points to be included in the ellipsoids. This paramter is equivalent to the error (E) proposed by Peterson et al. (2008).
#' @param env_bg Environmental data to compute the aproximated prevalence of the model. The data should be a sample of the environmental layers of the calibrarion area.
#' @return A data.frame with 3 columns: i) "fitted_vars" the names of variables that were fitted; ii) "om_rate" omission rates of the model; iii) "bg_prevalence" approximated prevalence of the model see details section.
#' @details Model selection occurs in environmental space (E-space). For each variable combination the omission rate (omr) in E-space is computed using the function \code{\link[ntbox]{inEllipsoid}}. The results will be ordered by omr and if the user specified the environmental backgroud "env_bg" an estimated prevalence will be computed and the results will be ordered also by "bg_prevalence".
#'
#' The number of variables to construct candidate models can be specified by the user in the parameter "nvarstest". Model selection will be run in parallel if the user specified more than one set of combinations and the total number of model to be tested is greater than 500.
#' @export
#' @import future
#' @author Luis Osorio-Olvera <luismurao@gmail.com>
#' @references Peterson,A.T. et al. (2008) Rethinking receiver operating characteristic analysis applications in ecological niche modeling. Ecol. Modell., 213, 63–72.
#' @examples
#' \dontrun{
#' # Bioclimatic layers path
#' wcpath <- list.files(system.file("extdata/bios",
#'                                 package = "ntbox"),
#'                     pattern = ".tif$",full.names = TRUE)
#' # Bioclimatic layers
#' wc <- raster::stack(wcpath)
#' # Occurrence data for the giant hummingbird (Patagona gigas)
#' pg <- utils::read.csv(system.file("extdata/p_gigas.csv",
#'                                   package = "ntbox"))
#' # Split occs in train and test
#' pgL <- base::split(pg,pg$type)
#' pg_train <- pgL$train
#' pg_test <- pgL$test
# Environmental data for training and testing
#' pg_etrain <- raster::extract(wc,pg_train[,c("longitude",
#'                                             "latitude")],
#'                              df=TRUE)
#' pg_etrain <- pg_etrain[,-1]
#' pg_etest <- raster::extract(wc,pg_test[,c("longitude",
#'                                           "latitude")],
#'                             df=TRUE)
#' pg_etest <- pg_etest[,-1]
#'
#' # Non-correlated variables
#' env_varsL <- ntbox::correlation_finder(cor(pg_etrain),
#'                                        threshold = 0.8,
#'                                        verbose = F)
#' env_vars <- env_varsL$descriptors
#' # Number of variables to fit ellipsoids (3,5,6 )
#' nvarstest <- c(3,5,6)
#' # Level
#' level <- 0.95
#' # Environmental bagraund to compute the appoximated
#' # prevalence in the prediction
#' env_bg <- raster::sampleRandom(wc,10000)
#'
#' # Selection process
#'
#' e_selct <- ntbox::ellipsoid_selection(env_train = pg_etrain,
#'                                       env_vars = env_vars,
#'                                       level = level,
#'                                       nvarstest = nvarstest,
#'                                       env_bg = env_bg)
#'
#'
#' bestvarcomb <- stringr::str_split(e_selct$fitted_vars,",")[[1]]
#'
#' # Best ellipsoid model
#'
#' best_mod <- ntbox::cov_center(pg_etrain[,bestvarcomb],
#'                               mve = T,
#'                               level = 0.99,
#'                               vars = 1:length(bestvarcomb))
#'
#'
#' # Projection model in geographic space
#'
#' mProj <- ntbox::ellipsoidfit(wc[[bestvarcomb]],
#'                              centroid = best_mod$centroid,
#'                              covar = best_mod$covariance,
#'                              level = 0.99,size = 3)
#'
#' raster::plot(mProj$suitRaster)
#' points(pg[,c("longitude","latitude")],pch=20,cex=0.5)
#'
# Evaluating the model with partial roc of independent data
#' pg_proc <- ntbox::pROC(continuous_mod = mProj$suitRaster,
#'                        test_data = pg_test[,c("longitude","latitude")],
#'                        n_iter = 1000,
#'                        E_percent = 5,
#'                        boost_percent = 50,parallel = F)

#' print(pg_proc$pROC_summary)
#' }

ellipsoid_selection <- function(env_train,env_vars,nvarstest,level,env_bg=NULL){
  n_vars <- length(env_vars)
  ntest <- sapply(nvarstest, function(x) choose(n_vars,x))
  nmodels <- sum(ntest)
  cat("-----------------------------------------------------------------------------------------\n")

  cat("\t\t**** Starting model selection process ****\n")
  cat("-----------------------------------------------------------------------------------------\n\n")
  for(i in 1:length(ntest)){
    cat("A total number of",ntest[i] ,"models will be created for combinations",
        "of",n_vars, "variables taken by",nvarstest[i],"\n\n")
  }
  cat("-----------------------------------------------------------------------------------------\n")
  cat("\t **A total number of",nmodels ,"models will be tested **\n\n")
  cat("-----------------------------------------------------------------------------------------\n")

  cvars <- lapply(nvarstest, function(x) utils::combn(env_vars,x))


  if(length(cvars) >1L && nmodels >500){
    dir1 <- file.path(getwd(),"ellipsoid_select")
    dir.create(dir1)
    future::plan(multiprocess)
    model_select <- new.env()
    pasosChar <- paste(1:length(nvarstest))
    for (paso in pasosChar) {
      x <- as.numeric(paso)
      model_select[[paso]] %<-% {
        combs_v <- cvars[[x]]
        results_L <- lapply(1:ncol(combs_v), function(x_comb,env_bg1=env_bg){
          emd <- try(ntbox::cov_center(env_train[,combs_v [,x_comb]],
                                       level = 0.95,
                                       vars = 1:nrow(combs_v)),silent = TRUE)
          message1 <- attr(emd,"class")== "try-error"
          if(length(message1)>0L)
            return()

          in_e <-  inEllipsoid(centroid = emd$centroid,
                               eShape = emd$covariance,
                               env_data = env_train[,combs_v [,x_comb]],
                               level = level)
          a <- length(which(in_e$in_Ellipsoid==0))
          omrate <- a /nrow( in_e)

          d_results <- data.frame(fitted_vars =paste(combs_v [,x_comb],
                                                     collapse =  ","),
                                  nvars=nvarstest[x],
                                  om_rate=omrate)

          if(!is.null(env_bg1)){
            if(!is.data.frame(env_bg1))
              env_bg1 <- data.frame(env_bg1)
            in_ebg <-  inEllipsoid(centroid = emd$centroid,
                                   eShape = emd$covariance,
                                   env_data = env_bg1[,combs_v [,x_comb]],
                                   level = level)
            prevBG <- length(which(in_ebg$in_Ellipsoid==1))/nrow(in_ebg)
            d_results <-data.frame( d_results,bg_prevalence= prevBG)
          }
          return(d_results)

        })
        results_df <- do.call("rbind.data.frame",results_L)
        fname <- file.path(dir1,paste0("eselection_",x,".csv"))
        utils::write.csv(results_df,fname,row.names = F)
        return()
      }
    }

    cat("Finishing...\n\n")
    cat("-----------------------------------------------------------------------------------------\n")
    rfinal_p <- list.files(dir1,
                           pattern = "*.csv$",
                           full.names = TRUE)
    while (length(rfinal_p) != length(pasosChar)) {
      rfinal_p <- list.files(dir1,
                             pattern = "*.csv$",
                             full.names = TRUE)
    }

    rfinal <- rfinal_p %>%
      purrr::map_df(~read.csv(.x,stringsAsFactors = F))
    unlink(dir1,recursive = T)
    #rfinal <- do.call("rbind.data.frame",as.list(model_select))
    if("bg_prevalence" %in% names(rfinal))
      rfinal <- rfinal[order(rfinal$om_rate,
                             rfinal$bg_prevalence,
                             decreasing = F),]
    else
      rfinal <- rfinal[order(rfinal$om_rate,decreasing = F),]
    rownames(rfinal) <- NULL
    future::plan(sequential)
    return(rfinal)
  } else{
    results_L <- lapply(1:length(cvars), function(x) {
      combs_v <- cvars[[x]]
      results_L <- lapply(1:ncol(combs_v), function(x_comb){
        emd <- try(ntbox::cov_center(env_train[,combs_v [,x_comb]],
                                     level = 0.95,
                                     vars = 1:nrow(combs_v)),silent = TRUE)
        message1 <- attr(emd,"class")== "try-error"
        if(length(message1)>0L)
          return()

        in_e <-  inEllipsoid(centroid = emd$centroid,
                             eShape = emd$covariance,
                             env_data = env_train[,combs_v [,x_comb]],
                             level = level)
        a <- length(which(in_e$in_Ellipsoid==0))
        omrate <- a /nrow( in_e)

        d_results <- data.frame(fitted_vars =paste(combs_v [,x_comb],
                                                   collapse =  ","),
                                nvars=nvarstest[x],
                                om_rate=omrate)

        if(!is.null(env_bg)){
          if(!is.data.frame(env_bg))
            env_bg <- data.frame(env_bg)
          in_ebg <-  inEllipsoid(centroid = emd$centroid,
                                 eShape = emd$covariance,
                                 env_data = env_bg[,combs_v [,x_comb]],
                                 level = level)
          prevBG <- length(which(in_ebg$in_Ellipsoid==1))/nrow(in_ebg)
          d_results <-data.frame( d_results,bg_prevalence= prevBG)
        }
        return(d_results)

      })
      results_df <- do.call("rbind.data.frame",results_L)
      return(results_df)
    })
    rfinal <- do.call("rbind.data.frame",results_L)
    if("bg_prevalence" %in% names(rfinal))
      rfinal <- rfinal[order(rfinal$om_rate,
                             rfinal$bg_prevalence,
                             decreasing = F),]
    else
      rfinal <- rfinal[order(rfinal$om_rate,decreasing = F),]

    rownames(rfinal) <- NULL
    return(rfinal)
  }

}


#' ellipsoid_selection: Performs variable selection for ellipsoid models
#'
#' @description Performs variable selection for ellipsoid models according to omission rates in the environmental space.
#' @param centroid A numeric vector of centroids for each environmental variable
#' @param eShape Shape matrix of the ellipsoid (can be a covariance matrix or a minimum volume ellipsoid).
#' @param env_data A data frame with the environmental training data.
#' @param level Proportion of points to be included in the ellipsoids. This paramter is equivalent to the error (E) proposed by Peterson et al. (2008).
#' @export

inEllipsoid <- function(centroid,eShape,env_data,level){

  mh_dist <- stats::mahalanobis(env_data,
                                center = centroid,
                                cov =eShape)
  in_Ellipsoid <- mh_dist <= stats::qchisq(level,
                                           length(centroid))
  in_Ellipsoid <- in_Ellipsoid*1
  in_Ellipsoid_mh <- data.frame(in_Ellipsoid,mh_dist )

  return(in_Ellipsoid_mh)
}
