#' Compute omission rate of a model
#' @description Computes the omission rate value of a model given a threshold
#' @param model Ecological niche model in raster format.
#' @param threshold A numeric vector with the thresholds that will be used to compute the omission rates.
#' @param occ_train Train data with three columns: "sp_name", "longitude" and "latitude"
#' @param occ_test Test data with three columns: "sp_name", "longitude" and "latitude"
#' @return Returns a data.frame with the threshold value, the corresponding suiitability value at that thereshold and the omission rate.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Occurrence data for the giant hummingbird (Patagona gigas)
#' pg <- utils::read.csv(system.file("extdata/p_gigas.csv",
#'                                   package = "ntbox"))
#' # Split occs in train and test
#' pgL <- base::split(pg,pg$type)
#' pg_train <- pgL$train[,-4]
#' pg_test <- pgL$test[,-4]
#' mod_path <- file.path(system.file("extdata",
#'                             package = "ntbox"),
#'                      "p_gigas.tif")
#' model <- raster::raster(mod_path)
#' thresholds <- seq(0.01, 0.1,by = 0.03)
#' omrs <- omission_rate(model = model,
#'                       threshold = thresholds,
#'                       occ_train = pg_train,
#'                       occ_test = pg_test)
#'
#' print(omrs)

#' }

omission_rate <- function(model,threshold,occ_train,occ_test){
  suit_cal <- raster::extract(model,occ_train[,2:3])
  suit_evl <- raster::extract(model,occ_test[,2:3])
  n_vals <- length(suit_evl)

  omission_rates <- threshold %>% purrr::map_df(function(th){
    suit_at_th <- stats::quantile(suit_cal,th)
    omr <- 1 - length(which(suit_evl>=suit_at_th ))/n_vals
    df_omr <- data.frame(threshold = th,
                         suitability_val_at_th = suit_at_th,
                         om_rate=omr)
    return(df_omr)
  })
  return(omission_rates)
}

