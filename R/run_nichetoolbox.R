#' Function to run NicheToolBox shinyApp
#' @description This function initiates NicheToolBox GUI
#' @export
#' @examples
#' # Run nichetoolbox package
#' # run_nichetoobox()
run_nichetoolbox <- function(){
  app_path <- system.file("shinyApp",package = "nichetoolbox")
  return(shiny::runApp(app_path))
}
