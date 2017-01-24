#' Function to run NicheToolBox shinyApp
#' @description This function initiates NicheToolBox GUI
#' @export
#' @examples
#' # Run nichetoolbox package
#' # run_nichetoobox()
run_ntbox <- function(){
  app_path <- system.file("shinyApp",package = "ntbox")
  return(shiny::runApp(app_path))
}
