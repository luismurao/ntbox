#' Function to visualize GIBIF data using googleVis
#' @description Visualize history of GBIF records
#' @param data A data.frame with GBIF data
#' @return Returns a list with googleVis motin chart and a data.frame
#' @export

occs_history <- function(data){

  # Data atributes
  name <- NULL
  country <- NULL
  year <- NULL
  month <- NULL
  day <- NULL
  count <- NULL
  namesDB <- c("name","country","year","month","day")

  data <- data[,namesDB]
  s <-data %>% dplyr::group_by(name,country,year) %>%
    dplyr::summarise(count = dplyr::n())
  dat_byY <- dplyr::group_by(dplyr::ungroup(s),name) %>%
    dplyr::mutate(prop = count/sum(count))
  dat_byY <- dat_byY %>% dplyr::mutate(year1=year)
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
