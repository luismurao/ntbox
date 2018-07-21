pkg_check <- c("shiny","rgeos","rgdal",
               "raster","maptools","dismo",
               "rgl","dygraphs","dygraphs",
               "png","XML","rmarkdown",
               "knitr","stringr","stringr",
               "MASS","animation","mgcv",
               "googleVis","rasterVis","plyr",
               "shinyBS","shinyjs","rglwidget",
               "car","maps","corrplot",
               "dplyr","cluster","sqldf",
               "fields","devtools","psych",
               "magrittr","shinythemes","grid",
               "RColorBrewer","ade4","V8","spocc")

pkgs_ntb_miss <- pkg_check[!(pkg_check %in% installed.packages())]
if(length(pkgs_ntb_miss)>=0L){
  install.packages(pkgs_ntb_miss,repos = "https://cloud.r-project.org/")
}

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
sapply(pkgs_ntb_miss,function(x) library(x,character.only = TRUE))

# Load packages
options(rgl.useNULL=TRUE)
#sapply(pkgs_ntb,function(x) library(x,character.only = TRUE))
#rgl.init()




