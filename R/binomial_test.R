#' Binomial significance test for ecological niche models.
#'
#' @description binomial_test: significance estimation of a niche model by using the comulative binomial probability of success of predecting correctly an occurrence given the validation data and the proportional area predicted as present in the niche model.
#' @param binary_model A binary prediction map of the geographical distribution of a species.
#' @param validation_data A numerical matrix or data.frame containing coordinates of the occurrences used to test
#' the model to be evaluated; columns must be: longitude and latitude.
#' @export
#' @return A data.frame with 6 columns: (i) npixels_in_pred (numbre of pixeles in the prediction), (ii) npixels_pp (numbre of pixeles predicted as presences), (iii) prop_area_pp (proportional area predicted as presences), (iv) n_success (number of presences correctly predicted as presences), (v) n_fails (number of presences predicted as absences), and (vi) p_value (a value between 0-1 representing statistical significance of the niche model).
#' @details According to Anderson et al,(2003) The test is "employed to determine whether test points fall into regions of predicted presence more often than expected by chance, given the proportion of map pixels predicted present by the model". See
#' Anderson,R.P. et al. (2003) Evaluating predictive models of species’ distributions: Criteria for selecting optimal models. Ecol. Model., 162, 211–232.
#' @examples
#' binary_model <- raster::raster(file.path(system.file("extdata",
#'                                                      package = "ntbox"),
#'                                                      "binary_ixodes.tif"))
#' validation_data <- utils::read.csv(file.path(system.file("extdata",
#'                                                      package = "ntbox"),
#'                                                      "I_scapularis_eval.csv"))
#' binomial_test(binary_model, validation_data)


binomial_test <- function(binary_model,validation_data){

  if(class(binary_model) != "RasterLayer"){
    warning("Object must be a raster layer")
    return()
  }

  if(!(is.data.frame(validation_data) ||  is.matrix(validation_data))){
    warning("Object must be a data.frame or matrix")
    return()
  }
  else{
    clases <- raster::freq(binary_model)
    clases <- clases[-nrow(clases),]
    if(nrow(clases) == 2){
      npixels <- clases[1,2] + clases[2,2]
      proportional_area <- clases[2,2]/npixels
      pred_vals <- raster::extract(binary_model,validation_data)
      nasId <- attr(pred_vals,"na.action")

      if(length(nasId)>0L)
        cat(length(nasId),
            "validation data were omited because there are NA values (occurrences not in the prediction)")

      occs_table <- table(pred_vals)
      ocss_fail <-  occs_table[1]
      occs_succs <- occs_table[2]
      p_bin <- 1 - stats::pbinom(occs_succs,
                                 size=occs_succs+ocss_fail,
                                 prob = proportional_area)
      names(p_bin) <- "p_value"
      results_bin <- data.frame(npixels_in_pred=npixels,
                                npixels_pp =clases[2,2],
                                prop_area_pp=proportional_area,
                                n_success=occs_succs,
                                n_fails=ocss_fail,
                                p_value=p_bin)
      return( results_bin)
    }
    warning("binary_model has to have only 2 classes")
    return()
  }
}
