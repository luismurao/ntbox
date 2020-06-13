#' Function to visualize GIBIF data using googleVis
#' @description Visualize a time series of the history of GBIF records.
#' @param gbif_data A data.frame with GBIF data
#' @importFrom magrittr %>%
#' @return Returns a list with googleVis motion chart and a data.frame
#' @export
#' @details Creates a motionchart of the time series of the occurrence data. It uses the function \code{\link[googleVis]{gvisMotionChart}} from \code{\link[googleVis]{googleVis-package}}.
#' @examples
#' \dontrun{
#' ambystoma_tigrinum <- searh_gbif_data(genus = "Ambystoma",
#'                                       "tigrinum",occlim=1000,
#'                                       writeFile=FALSE)
#'
#' atigrinum_history <- occs_history(ambystoma_tigrinum)
#' plot(atigrinum_history$mot)
#' }
occs_history <- function(gbif_data){


  namesDB <- c("name","country","year","month","day")

  data <- gbif_data[,namesDB]
  s <-data %>% dplyr::group_by_(~name,~country,~year) %>%
    dplyr::summarise(count = dplyr::n())
  dat_byY <- dplyr::group_by_(dplyr::ungroup(s),~name) %>%
    dplyr::mutate_(prop = ~count/sum(count))
  dat_byY <- dat_byY %>% dplyr::mutate_(year1=~year)
  dat_byY <-  dplyr::ungroup(dat_byY)


  myStateSettings <-'{"xZoomedDataMin":1199145600000,"colorOption":"2",
  "duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
  "yAxisOption":"4","sizeOption":"_UNISIZE",
  "iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
  "xZoomedDataMax":1262304000000,"iconType":"LINE",
  "dimensions":{"iconDimensions":["dim0"]},
  "showTrails":false,"uniColorForNonSelected":false,
  "xAxisOption":"_TIME","orderedByX":false,"playDuration":25000,
  "xZoomedIn":false,"yZoomedDataMin":0,
  "yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
  '
  dat_byY <- dat_byY[ !duplicated(dat_byY[,c("country","year")]), ]

  mot <- googleVis::gvisMotionChart(dat_byY,
                                    idvar="country",
                                    timevar="year",
                                    xvar="year1",
                                    yvar="count",
                                    sizevar="prop",
                                    options=list(myStateSettings))



  return(list(mot=mot,data=data))
}
