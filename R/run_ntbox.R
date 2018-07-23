#' Function to run NicheToolBox shinyApp
#' @description This function initiates NicheToolBox GUI
#' @export
#' @examples
#' # Run nichetoolbox package
#' # run_nichetoobox()
run_ntbox <- function(){

  to_install <- system.file("shinyApp/load_install_pkgs.R",
                            package = "ntbox")

  cat("\n\n")
  cat("|----------------------------------------------------------------|\n")
  cat("|      **** Welcome to nichetoolbox version", paste0(utils::packageVersion("ntbox"),
                                                            " ****         |\n"))
  cat("|----------------------------------------------------------------|\n")
  cat("|           Looking for additional nichetoolbox dependencies     |\n")
  cat("|           Note that the first time can take few time           |\n")
  cat("|----------------------------------------------------------------|\n\n\n")

  if(.Platform$OS.type != "unix"){
    rtools_inst <- devtools::find_rtools()
    if(!rtools_inst){
      cat("If you have troubles while running ntbox please install Rtools:\n\n")
      cat("https://cran.r-project.org/bin/windows/Rtools/")

    }

  }

  source(to_install)

  app_path <- system.file("shinyApp",package = "ntbox")
  return(shiny::runApp(app_path))
}
