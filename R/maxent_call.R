#' maxent_call: Call maxent from R
#'
#' @description maxent_call Allows the user to introduce all the arguments that can be passed to MaxEnt. It also allows running MaxEnt from R.
#' @param maxentjar_path Path to maxent.jar
#' @param run_fromR Logical If TRUE, maxent will be executed from the current R session.
#' @param wait Logical If TRUE, R will wait until maxent fishes to run the model. Default TRUE.
#' @param features A vector with features classes to fit the maxent model. Use "l" for "linear","q" for "quadratic", "p" for "product", "h" for "hinge"and "t" for "threshold".
#' @param memory_assigned A numeric value representing the RAM memory assigned to the process.
#' @param environmentallayers Path to the directory containing environmental layers. Environmental variables can be in a directory containing one file per variable,
#' or all together in a .csv file in SWD format. Please enter a directory name or file name.
#' @param samplesfile Path to the file containing presence locations for one or more species.
#' @param testsamplesfile Path to the file containing test locations.
#' @param outputdirectory Path to the directory where model results will be written. if it is NULL the results will be written in the working directory.
#' @param projectionlayers Path to the directory containing the projection layer.
#' @param responsecurves Logical if TRUE creates graphs showing how predicted relative probability of occurrence depends on the value of each environmental variable.
#' @param pictures Logical if TRUE creates a .png image for each output grid.
#' @param jackknife  Logical if TRUE measures the importance of each environmental variable by training with each environmental variable first omitted, then used in isolation.
#' @param outputformat A character vector describing the type of output format to be written in model results. Representation of probabilities used in writing output grids. Possible output formats are cloglog, logistic, cumulative, and raw.
#' @param randomseed Logical if TRUE, a different random seed will be used for each run, so a different random test/train partition will be made and a different random subset of the background will be used, if applicable.
#' @param logscale Logical If TRUE, all pictures of models will use a logarithmic scale for color-coding.
#' @param warnings Logical If TRUE, pops up windows to warn about potential problems with input data. Regardless of this setting, warnings are always printed to the log file.
#' @param askoverwrite Logical if TRUE, the output files that already exist will be overwritten.
#' @param skipifexists Logical if TRUE, skips the species without remaking the model.
#' @param removeduplicates Logical if TRUE, removes duplicate presence records.
#' @param writeclampgrid Logical if TRUE, writes clamp grid when projecting.
#' @param writemess Logical if TRUE, does MESS analysis when projecting
#' @param randomtestpoints Numeric. Percentage of presence localities to be randomly set aside as test points used to compute AUC, omission, etc.
#' @param betamultiplier Numeric. Regularization multiplier.  A higher number gives a more spread-out distribution.
#' @param maximumbackground Numeric. Max number of background points.
#' @param biasfile Path to the bias file. Sampling is assumed to be biased according to the sampling distribution given in this grid file. Values in this file must not be zero or negative.
#' @param biastype Default 3. See \url{https://groups.google.com/forum/#!topic/maxent/bZYdlYmDG4s} for details.
#' @param replicates Numeric. The number of replicate runs to do when cross-validating, bootstrapping, or doing sampling with replacement runs.
#' @param replicatetype Character vector. Possible values are crossvalidate, bootstrap, and subsample. If replicates > 1, do multiple runs of this type: Crossvalidate: samples divided into replicates folds; each fold in turn used for test data. Bootstrap: replicate sample sets chosen by sampling with replacement. Subsample: replicate sample sets chosen by removing random test percentage without replacement to be used for evaluation.
#' @param perspeciesresults Logical, if TRUE write separate maxentResults file for each species.
#' @param writebackgroundpredictions Logical if TRUE, will write .csv file with predictions at background points.
#' @param responsecurvesexponent Logical if TRUE, shows exponent in response curves.
#' @param addsamplestobackground Logical if TRUE adds samples to background.
#' @param addallsamplestobackground Logical if TRUE adds all samples to background.
#' @param autorun Logical if TRUE starts running as soon as the program starts up.
#' @param writeplotdata Logical if TRUE writes plot data.
#' @param fadebyclamping Logical if TRUE reduces prediction at each point in projections by the difference between clamped and non-clamped output at that point.
#' @param extrapolate Logical, If TRUE predicts to regions of environmental space outside the limits encountered during training
#' @param visible Logical If TRUE makes the Maxent user interface visible.
#' @param autofeature Logical If TRUE Automatically selects which feature classes to use, based on the number of training samples
#' @param doclamp Logical If TRUE applies clamping when projecting
#' @param outputgrids Logical If TRUE writes output grids.  Turning this off when doing replicate runs causes only the summary grids (average, std deviation, etc.) to be written, not those for the individual runs.
#' @param plots Logical If TRUE writes various plots for inclusion in .html output.
#' @param appendtoresultsfile Logical If FALSE, maxentResults.csv file is reinitialized before each run.
#' @param maximumiterations Numeric. Stop training after this many iterations of the optimization algorithm.
#' @param convergencethreshold Numeric. Stop training when they drop in log loss per iteration drops below this number.
#' @param adjustsampleradius Numeric. Add this number of pixels to the radius of white/purple dots for samples on pictures of predictions. Negative values reduce the size of dots.
#' @param threads Numeric. The number of processor threads to use.  Matching this number to the number of cores on your computer speeds up some operations, especially variable jackknifing.
#' @param lq2lqptthreshold Numeric. The number of samples at which product and threshold features start being used.
#' @param l2lqthreshold Numeric. The number of samples at which quadratic features start being used.
#' @param hingethreshold Numeric. The number of samples at which hinge features start being used.
#' @param beta_threshold Numeric. Regularization parameter to be applied to all threshold features; negative value enables automatic setting.
#' @param beta_categorical Numeric. Regularization parameter to be applied to all categorical features; negative value enables automatic setting.
#' @param beta_lqp Numeric. Regularization parameter to be applied to all linear, quadratic, and product features; negative value enables automatic setting.
#' @param beta_hinge Numeric. Regularization parameter to be applied to all hinge features; negative value enables automatic setting.
#' @param logfile Filename to be used for writing debugging information about a run in the output directory.
#' @param cache Logical If TRUE makes a .mxe cached version of ascii files, for faster access.
#' @param defaultprevalence Numeric. Default prevalence of the species: the probability of presence at ordinary occurrence points. See Elith et al., Diversity and Distributions, 2011 for details.
#' @param applythresholdrule Apply a threshold rule, generating a binary output grid in addition to the regular prediction grid.  Use the full name of the threshold rule in Maxent's html output as the argument.  For example, 'applyThresholdRule=Fixed cumulative value 1'.
#' @param togglelayertype Toggle selection of environmental layers whose names begin with this prefix (default: all selected).
#' @param togglespeciesselected String Toggle selection of species whose names begin with this prefix (default: all selected)
#' @param togglelayerselected String. Toggle selection of environmental layers whose names begin with this prefix (default: all selected)
#' @param verbose Logical If TRUE gives a detailed diagnostics for debugging.
#' @param allowpartialdata Logical If TRUE During model training, allow the use of samples that have nodata values for one or more environmental variables..
#' @param prefixes Logical If TRUE during model training, allow the use of samples that have nodata values for one or more environmental variables.
#' @param nodata Numeric. Default nodata value.
#' @details The documentation of the parameters of this function are based on maxent´s help.
#' For detailed documentation of the parameter that can be passed to maxent go to \url{https://github.com/mrmaxent/Maxent/blob/master/density/parameters.csv}.
#' @examples
#' \dontrun{
#' environmentallayers <- system.file("extdata",
#'                                    package = "ntbox")
#'maxent_path <- "~/Downloads/maxent"
#'outputdirectory <- "~/Downloads/"
#'occ_data_path <- system.file("extdata",
#'                             "cardon_occs.csv",
#'                             package = "ntbox")
#'maxent_parm <- maxent_call(maxentjar_path = maxent_path,run_fromR=TRUE,
#'                           environmentallayers = environmentallayers,
#'                           samplesfile=occ_data_path,
#'                           betamultiplier = .1,
#'                           features=c("l","q","p"),
#'                           outputdirectory = outputdirectory)
#'print(maxent_parm)

#' }
#' @return A character vector with maxent`s parametrization.
#' @export
maxent_call <- function(maxentjar_path,
                        run_fromR=TRUE,
                        wait =TRUE,
                        features,
                        memory_assigned = 2000,
                        environmentallayers,
                        samplesfile,
                        testsamplesfile=NULL,
                        outputdirectory=NULL,
                        projectionlayers = NULL,
                        responsecurves = FALSE,
                        pictures =TRUE,
                        jackknife = FALSE,
                        outputformat= "cloglog",
                        randomseed=FALSE,
                        logscale = TRUE,
                        warnings = TRUE,
                        askoverwrite=FALSE,
                        skipifexists=FALSE,
                        removeduplicates=TRUE,
                        writeclampgrid=FALSE,
                        writemess =FALSE,
                        randomtestpoints =0,
                        betamultiplier=1,
                        maximumbackground=10000,
                        biasfile = "",
                        biastype = 3,
                        replicates=NULL,
                        replicatetype="crossvalidate",
                        perspeciesresults=FALSE,
                        writebackgroundpredictions=FALSE,
                        responsecurvesexponent=FALSE,
                        addsamplestobackground=TRUE,
                        addallsamplestobackground=FALSE,
                        autorun=TRUE,
                        writeplotdata=FALSE,
                        fadebyclamping=FALSE,
                        extrapolate=FALSE,
                        visible= FALSE,
                        autofeature =FALSE,
                        doclamp = FALSE,
                        outputgrids =TRUE,
                        plots =TRUE,
                        appendtoresultsfile =FALSE,
                        maximumiterations = 500,
                        convergencethreshold = 1.0E-5,
                        adjustsampleradius =0,
                        threads =1,
                        lq2lqptthreshold = 80,
                        l2lqthreshold = 10,
                        hingethreshold = 15,
                        beta_threshold = -1.0,
                        beta_categorical = -1.0,
                        beta_lqp = -1.0,
                        beta_hinge = -1.0,
                        logfile = "maxent.log",
                        cache = TRUE,
                        defaultprevalence = 0.5,
                        applythresholdrule=NULL ,
                        togglelayertype =NULL,
                        togglespeciesselected =NULL,
                        togglelayerselected = NULL,
                        verbose = FALSE,
                        allowpartialdata = FALSE,
                        prefixes =TRUE,
                        nodata = -9999){
  if(!is.list(features)) features <-list(features)
  selected_features <- paste(unlist(lapply(features, .maxent_features)),collapse = " ")
  #print(selected_features)
  maxentpath <- normalizePath(file.path(maxentjar_path,
                                        "maxent.jar"))

  maxentpath <- gsub(pattern = "[\\]","/",maxentpath)
  maxentpath <- paste0("'",maxentpath,"'")
  environmentallayers <- normalizePath(environmentallayers)
  environmentallayers <- gsub(pattern = "[\\]","/",environmentallayers)

  #env_name <- unlist(stringr::str_split(environmentallayers ,dl))
  env_name <- gsub(pattern = "[\\]","/",environmentallayers)
  env_name <- unlist(stringr::str_split(environmentallayers,pattern = "/"))
  environmentallayers <- paste0("'",environmentallayers,"'")


  ifelse(nchar(env_name[length(env_name)])>0L,
         env_name <- env_name[length(env_name)],
         env_name <- env_name[length(env_name)-1])
  samplesfile <- normalizePath(samplesfile)
  samplesfile <- gsub(pattern = "[\\]","/",samplesfile)
  samplesfile <- paste0("'",samplesfile,"'")
  if(!is.null(testsamplesfile)){
    testsamplesfile <- normalizePath(testsamplesfile)
    testsamplesfile <- gsub(pattern = "[\\]","/",testsamplesfile)
    testsamplesfile <- paste0("'",testsamplesfile,"'")
  }

  if(is.null(outputdirectory) || !dir.exists(outputdirectory)){
    outputdirectory <- getwd()
  } else{
    outputdirectory <- normalizePath(outputdirectory)
  }
  outputdirectory <- gsub(pattern = "[\\]","/",outputdirectory)


  f_act <- paste(unlist(features),collapse = "")

  outputdirectory <- file.path(outputdirectory,
                               paste0(env_name,
                                      "_bmult_",
                                      betamultiplier,"_",
                                      f_act))

  lapply(outputdirectory, function(x){
    if(!dir.exists(x))
      dir.create(x)
  })
  outputdirectory <- normalizePath(outputdirectory)
  outputdirectory <- gsub(pattern = "[\\]","/",outputdirectory)
  outputdirectory <- paste0("'",outputdirectory,"'")
  if(!is.null(projectionlayers)){
    projlayers <- unlist(
      lapply(projectionlayers, function(x){
        if(dir.exists(x)) return(normalizePath(x))
      })
    )
    projectionlayers <- paste0("'",paste0(projlayers,collapse = ","),"'")
  }

  maxent_call <-   paste0("java ","-mx",
                          memory_assigned,"m -jar ",
                          maxentpath," environmentallayers=",
                          environmentallayers,
                          " samplesfile=",samplesfile,
                          ifelse(!is.null(testsamplesfile),
                                 paste0(" testsamplesfile=",testsamplesfile),""),
                          " outputdirectory=",outputdirectory,
                          ifelse(is.null(projectionlayers),"",
                                 paste0(" projectionlayers=",
                                        projectionlayers)),
                          " ",selected_features,
                          " maximumbackground=", maximumbackground,
                          ifelse(responsecurves," responsecurves=true",
                                 " responsecurves=false"),
                          ifelse(pictures," pictures=true"," pictures=false"),
                          ifelse(jackknife," jackknife=true"," jackknife=false"),
                          ifelse(!outputformat %in% c("raw","cloglog","logistic",
                                                      "cumulative"),
                                 " outputformat=cloglog", paste0(" outputformat=",
                                                                 outputformat)),
                          ifelse(randomseed," randomseed=true"," randomseed=false"),
                          ifelse(logscale," logscale=true",
                                 " logscale=false"),
                          ifelse(warnings," warnings=true",
                                 " warnings=false"),
                          ifelse(askoverwrite," askoverwrite=true",
                                 " askoverwrite=false"),
                          ifelse(skipifexists," skipifexists=true",
                                 " skipifexists=false"),
                          ifelse(removeduplicates," removeduplicates=true",
                                 " removeduplicates=false"),
                          ifelse(writeclampgrid," writeclampgrid=true",
                                 " writeclampgrid=false"),
                          ifelse(writemess," writemess=true",
                                 " writemess=false"),
                          ifelse(is.numeric(randomtestpoints),
                                 paste0(" randomtestpoints=",
                                        randomtestpoints),""),
                          ifelse(is.numeric(betamultiplier),
                                 paste0(" betamultiplier=",
                                        betamultiplier),""),
                          ifelse(is.numeric(maximumbackground),
                                 paste0(" maximumbackground=",
                                        maximumbackground),""),
                          ifelse(!file.exists(biasfile),"",
                                 paste0(" biasfile=",{
                                   biasfile <- normalizePath(biasfile)
                                   biasfile <- gsub(pattern = "[\\]","/",biasfile)
                                   paste0("'",biasfile,"'")})),
                          ifelse(!file.exists(biasfile),"",
                                 paste0(" biastype=",biastype)),
                          ifelse(is.numeric(replicates),
                                 paste0(" replicates=",
                                        replicates),""),
                          ifelse(replicatetype %in% c("crossvalidate",
                                                      "bootstrap",
                                                      "subsample"),
                                 paste0(" replicatetype=",
                                        replicatetype),
                                 " replicatetype=crossvalidate"),
                          ifelse(is.logical(perspeciesresults) &&
                                   isTRUE(perspeciesresults),
                                 paste0(" perspeciesresults=","true"),
                                 " perspeciesresults=false"),
                          ifelse(is.logical(writebackgroundpredictions) &&
                                   isTRUE(writebackgroundpredictions),
                                 paste0(" writebackgroundpredictions=","true"),
                                 " writebackgroundpredictions=false"),
                          ifelse(is.logical(responsecurvesexponent) &&
                                   isTRUE(responsecurvesexponent),
                                 paste0(" responsecurvesexponent=","true"),
                                 " responsecurvesexponent=false"),
                          ifelse(is.logical(addsamplestobackground) &&
                                   isFALSE(addsamplestobackground),
                                 paste0(" addsamplestobackground=","false"),
                                 " addsamplestobackground=true"),
                          ifelse(is.logical(addallsamplestobackground) &&
                                   isTRUE(addsamplestobackground),
                                 paste0(" addallsamplestobackground=","true"),
                                 " addallsamplestobackground=false"),
                          ifelse(is.logical(writeplotdata) && isTRUE(writeplotdata),
                                 paste0(" writeplotdata=","true"),
                                 " writeplotdata=false"),
                          ifelse(is.logical(fadebyclamping) && isTRUE(fadebyclamping),
                                 paste0(" fadebyclamping=","true"),
                                 " fadebyclamping=false"),
                          ifelse(is.logical(extrapolate) && isTRUE(extrapolate),
                                 paste0(" extrapolate=","true"),
                                 " extrapolate=false"),
                          ifelse(is.logical(visible) && isTRUE(visible),
                                 paste0(" visible=","true")," visible=false"),
                          ifelse(is.logical(autofeature) && isTRUE(autofeature),
                                 paste0(" autofeature=","true"),
                                 " autofeature=false"),
                          ifelse(is.logical(doclamp) && isTRUE(doclamp),
                                 paste0(" doclamp=","true")," doclamp=false"),
                          ifelse(is.logical(outputgrids) && isTRUE(outputgrids),
                                 paste0(" outputgrids=","true")," outputgrids=false"),
                          ifelse(is.logical(plots) && isTRUE(plots),
                                 paste0(" plots=","true")," plots=false"),
                          ifelse(is.logical(appendtoresultsfile) &&
                                   isTRUE(appendtoresultsfile),
                                 paste0(" appendtoresultsfile=","true"),
                                 " appendtoresultsfile=false"),
                          ifelse(is.numeric(maximumiterations),
                                 paste0(" maximumiterations=",
                                        maximumiterations),
                                 " maximumiterations=500"),
                          ifelse(is.numeric(convergencethreshold),
                                 paste0(" convergencethreshold=",
                                        toupper(convergencethreshold)),
                                 " convergencethreshold=1.0E-5"),
                          ifelse(is.numeric(adjustsampleradius),
                                 paste0(" adjustsampleradius=",
                                        adjustsampleradius),
                                 " adjustsampleradius=0"),
                          ifelse(is.numeric(threads),
                                 paste0(" threads=",
                                        threads)," threads=1"),
                          ifelse(lq2lqptthreshold > 0 && lq2lqptthreshold<= 100,
                                 paste0(" lq2lqptthreshold=",
                                        lq2lqptthreshold)," lq2lqptthreshold=80"),
                          ifelse(lq2lqptthreshold >0 && lq2lqptthreshold<=100,
                                 paste0(" l2lqthreshold=",
                                        l2lqthreshold)," l2lqthreshold=10"),
                          ifelse(hingethreshold>0 && hingethreshold<=100,
                                 paste0(" hingethreshold=",
                                        hingethreshold)," hingethreshold=15"),
                          ifelse(beta_threshold < 0,
                                 paste0(" beta_threshold=",
                                        beta_threshold)," beta_threshold=-1.0"),
                          ifelse(beta_categorical < 0,
                                 paste0(" beta_categorical=",
                                        beta_categorical),
                                 " beta_categorical=-1.0"),
                          ifelse(beta_lqp < 0,
                                 paste0(" beta_lqp=",
                                        beta_lqp)," beta_lqp=-1.0"),
                          ifelse(beta_hinge < 0,
                                 paste0(" beta_hinge=",
                                        beta_hinge)," beta_hinge=-1.0"),
                          ifelse(is.character(logfile),
                                 paste0(" logfile=",
                                        logfile)," logfile=maxent.log"),
                          ifelse(is.logical(cache) && isTRUE(cache),
                                 paste0(" cache=","true")," cache=false"),
                          ifelse(defaultprevalence > 0 && defaultprevalence<=1,
                                 paste0(" defaultprevalence=",
                                        defaultprevalence)," defaultprevalence=0.5"),
                          ifelse(applythresholdrule %in% c("Fixed cumulative value 1",
                                                           "Fixed cumulative value 5",
                                                           "Fixed cumulative value 10",
                                                           "Minimum training presence",
                                                           "10 percentile training presence",
                                                           "Equal training sensitivity and specificity",
                                                           "Maximum training sensitivity plus specificity",
                                                           "Equal test sensitivity and specificity",
                                                           "Maximum test sensitivity plus specificity",
                                                           "Equate entropy of thresholded and original distributions"),
                                 paste0(" applythresholdrule=",
                                        paste0("'",applythresholdrule,"'")),""),
                          ifelse(is.character(togglelayertype),
                                 paste0(" togglelayertype=",togglelayertype),""),
                          ifelse(is.character(togglespeciesselected),
                                 paste0(" togglespeciesselected=",
                                        togglespeciesselected),""),
                          ifelse(is.character(togglelayerselected),
                                 paste0(" togglelayerselected=",
                                        togglelayerselected),""),
                          ifelse(is.logical(verbose) && isTRUE(verbose),
                                 paste0(" verbose=","true")," verbose=false"),
                          ifelse(is.logical(allowpartialdata) &&
                                   isTRUE(allowpartialdata),
                                 paste0(" allowpartialdata=","true"),
                                 " allowpartialdata=false"),
                          ifelse(is.logical(prefixes) && isTRUE(prefixes),
                                 paste0(" prefixes=","true")," prefixes=false"),
                          ifelse(!is.null(nodata),paste0(" nodata=",nodata),
                                 " nodata=-9999"),
                          ifelse(is.logical(autorun) && isTRUE(autorun),
                                 " autorun","")
  )
  if(run_fromR){
    #if(.Platform$OS.type == "unix")
    system(command = maxent_call,intern = wait,wait = wait)
    #else
    #  system2(command  = maxent_call, wait = T, invisible = FALSE)
  }
  #system(command = maxent_call)
  #cat(paste0("#!/bin/sh\n",maxent_call),file = "~/Dropbox/maxentcommand.sh")

  return(maxent_call)


}


.maxent_features <- function(x){

  test_0 <- c("l","linear",
              "q","quadratic",
              "p","product",
              "h","hinge",
              "t","threshold")
  #if(length(test_0  %in% x)==0L | "autofeature" %in% x)
  #  autofeature <- "autofeature=true"
  #else autofeature <- "autofeature=false"
  if("l"  %in% x | "linear" %in% x)
    l <- 'linear=true'
  else l <- 'linear=false'
  if("q" %in% x | "quadratic" %in% x)
    q <- 'quadratic=true'
  else q <- 'quadratic=false'
  if("p" %in% x | "product" %in% x)
    p <- 'product=true'
  else p <- 'product=false'
  if("t"  %in% x | "threshold" %in% x)
    t <- 'threshold=true'
  else t <- 'threshold=false'
  if("h"  %in% x | "hing" %in% x)
    h <- 'hinge=true'
  else h <- 'hinge=false'
  return(c(l,q,p,t,h))
}
