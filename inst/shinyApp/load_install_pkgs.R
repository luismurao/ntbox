# Github dependencies
devtools::install_github("luismurao/leaflet")

pkgs_ntb <- c("ENMGadgets","shinysky")

# Missing packages
pkgs_ntb_miss <- pkgs_ntb[!(pkgs_ntb %in% installed.packages())]
# Install missing packages
if(length(pkgs_ntb_miss)>0L){
  devtools::install_github("AnalytixWare/ShinySky")
  devtools::install_github("ENMGadgets", "narayanibarve")

}
library(ENMGadgets)
library(shinysky)
library(leaflet)
# Load packages
options(rgl.useNULL=TRUE)
#sapply(pkgs_ntb,function(x) library(x,character.only = TRUE))
#rgl.init()




