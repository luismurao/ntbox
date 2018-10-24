#' Function to run NicheToolBox shinyApp
#' @description This function initiates NicheToolBox GUI
#' @export
#' @examples
#' # Run nichetoolbox package
#' # run_nichetoobox()
run_ntbox <- function(){
  app_path <- .check_install()
  return(shiny::runApp(app_path))
}

.check_install <- function(){
  to_install <- system.file("shinyApp/load_install_pkgs.R",
                            package = "ntbox")

  cat("\n\n")
  cat("|----------------------------------------------------------------|\n")
  cat("|      **** Welcome to nichetoolbox version", paste0(utils::packageVersion("ntbox"),
                                                            " ****         |\n"))
  cat("|----------------------------------------------------------------|\n")
  cat("|      Looking for additional nichetoolbox dependencies          |\n")
  cat("|      Note that the first time it can take few seconds          |\n")
  cat("|----------------------------------------------------------------|\n\n\n")

  rtools_inst <- devtools::has_devel()
  if(!rtools_inst){
    if(Sys.info()["sysname"] == "Windows"){
      cat("If you have troubles while running ntbox please install Rtools:\n\n")
      cat("https://cran.r-project.org/bin/windows/Rtools/")
    }
    if(Sys.info()["sysname"] == "Darwin"){
      cat("If you have troubles while running ntbox please install Command Line Tools:\n\n")
      cat("Open terminal and type:\n")
      cat("xcode-select --install")
    }

  }

  source(to_install)

  app_path <- system.file("shinyApp",package = "ntbox")
  return(app_path)
}
