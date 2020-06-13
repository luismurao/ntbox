#' get_bio_oracle: Get environmental layers from Bio-Oracle.
#' @description Get bioclimatic layers from Bio-Oracle for current and future scenarios
#' @param period Time period. Posible values are: "current","2050","2100","2200".
#' @param var_type Type of variable.  Posible values are: 'salinity','sea_surface_temperature',
#' 'current_velocity','sea_water_temperature','sea_water_salinity','sea_ice_thickness',
#' 'chlorophyll_concentration','sea_surface_salinity'.
#' @param model Climate model. Possible values are: "UKMO-HadCM3" and "AOGCM"
#' @param scenario Climate change scenario. Posible values are "a1b","a2","b1","rcp26","rcp45","rcp60","rcp85".
#' @param sv_dir Path to the directory where the layers will be saved. The default is the working directory of the R session.
#' @param load2r Logical. Load layers into R?
#' @param parallel Download layers in parallel.
#' @seealso \code{\link[ntbox]{get_envirem_elev}}, \code{\link[ntbox]{get_envirem_clim}},  \code{\link[ntbox]{get_chelsa}}
#' @details For more details visit \url{http://www.bio-oracle.org/index.php}
#' @references Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H., Serrao, E. A., & De Clerck, O. (2017). Bio-ORACLE v2.0: Extending marine data layers for bioclimatic modelling. Global Ecology and Biogeography.
#' @export
#' @examples
#' \dontrun{
#' swater_temp <- get_bio_oracle(period = "current",
#'                               var_type =  'sea_water_temperature',
#'                               model = NULL,
#'                               scenario = NULL,
#'                               sv_dir="~/Desktop/",
#'                               load2r = TRUE,
#'                               parallel = TRUE)
#' swater_temp_2100_AOGCM_rcp85 <- get_bio_oracle(period = "2100",
#'                                                var_type ='sea_water_temperature',
#'                                                model = "AOGCM",
#'                                                scenario = "rcp85",
#'                                                sv_dir="C:/Users/l916o895/Desktop",
#'                                                load2r = TRUE,
#'                                                parallel = TRUE)
#' }
get_bio_oracle <- function(period,var_type,model=NULL,scenario=NULL,sv_dir=getwd(),load2r=TRUE,parallel=TRUE){
  if(!dir.exists(sv_dir))
    stop(paste("No such a file or directory,",
               sv_dir))
  bior_down <- NULL
  bio_oracle <- base::readRDS(file.path(system.file("extdata",
                                                    package = "ntbox"),
                                        "bio_oracle.rds"))
  bio_oracle_urls <- NULL

  if(period == "current"){
    bio_oracle <- bio_oracle[!duplicated(bio_oracle$current_layer_code),]

    bio_oracle <- bio_oracle %>% filter_(~type==var_type)
    layers_des <- paste("bio_oracle",
                        var_type,
                        period,sep = "_")
    bio_oracle_urls <- bio_oracle$current_layer_code
  }
  if(period %in% c("2050",
                   "2100",
                   "2200") &&
     scenario %in% c("rcp26",
                     "a1b",
                     "a2",
                     "b1",
                     "rcp26",
                     "rcp45",
                     "rcp60",
                     "rcp85") &&
     model %in% c("UKMO-HadCM3","AOGCM")){
    scenario <- base::toupper(scenario)
    period <- as.numeric(period)
    bio_oracle$split_code <- paste(bio_oracle$type,
                                   bio_oracle$year,
                                   bio_oracle$model,
                                   bio_oracle$scenario,
                                   sep="_")
    layers_des <- paste(var_type,
                        period,
                        model,
                        scenario,
                        sep = "_")


    bio_oracle <- bio_oracle %>% filter_(~split_code==layers_des)


    if(nrow(bio_oracle)>0L){
      layers_des <- paste("bio_oracle",
                          var_type,
                          period,
                          model,
                          scenario,
                          sep = "_")
      bio_oracle_urls <- bio_oracle$future_layer_code
    }

  }
  if(is.null(bio_oracle_urls) && exists("layers_des")){
    warning(paste("No spatial information for",
                  layers_des))
  }
  else{
    dir_name <- base::file.path(sv_dir, layers_des)
    if(!dir.exists(dir_name ))
      dir.create(dir_name )

    if(parallel){
      ncores <- parallel::detectCores() -1
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl,varlist = c("bio_oracle_urls",
                                             "dir_name"),
                              envir = environment())
      pardown <- function(x){
        r1 <- sdmpredictors::load_layers(bio_oracle_urls[x],
                                         rasterstack = FALSE,
                                         datadir = dir_name)
        return(r1)
      }

      bior_down <- parallel::clusterApply(cl, seq_along(bio_oracle_urls),
                                          function(x) pardown(x))
      parallel::stopCluster(cl)
    }
    else{
      bior_down <- sdmpredictors::load_layers(bio_oracle_urls,
                                              rasterstack = FALSE,
                                              datadir = dir_name)
    }

    if(load2r)
      bior_down <- raster::stack(unlist(bior_down))
    cite_bior <- paste("Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H.,",
                       "Serrao, E. A., & De Clerck, O. (2017). Bio-ORACLE v2.0:",
                       "Extending marine data layers for bioclimatic modelling.",
                       "Global Ecology and Biogeography.")
    base::message(paste("Please cite as",cite_bior))
  }
  return(bior_down)
}
