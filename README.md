# NicheToolBox

`ntbox`is the stable version of `nichetoolbox` which is an R package with a friendly Graphical User Interface (GUI) developed using shiny framework. The package aims to facilitate the process of building niche models and estimate the species distributions.

## Installation
```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox')
```
### Warning

There is a bug in `install_github` function of the `devtools` package. Because of this sometimes you will have troubles while installing `nichetoolbox` package (see [#issue 3](https://github.com/luismurao/nichetoolbox/issues/3)). In order to install it you will first have to install the dependencies of the package: 

##### Example 

If you see an error like this:

``` r
installing source package 'ntbox' ...
Warning in .write_description(db, file.path(outDir, "DESCRIPTION")) :
Unknown encoding with non-ASCII data: converting to ASCII
** R
** inst
** preparing package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :
there is no package called 'ridigbio'
Error : package 'spocc' could not be loaded
ERROR: lazy loading failed for package 'nichetoolbox'

```
*Note that the error is generated because **there is no package called 'ridigbio'**.* 

Just run the following code:

``` r
ntbox_pkgs <- c("devtools", "shiny", "rgeos", "rgdal", "sp", "raster", "maptools", "dismo", 
                 "rgl", "dygraphs", "png", "rmarkdown", "knitr", "stringr", "MASS",
                 "animation", "mgcv", "googleVis","rasterVis", "shinyBS","shinyjs",
                 "rglwidget", "car", "maps", "corrplot", "dplyr", "cluster", "sqldf",
                 "fields", "devtools", "psych", "shinythemes", "grid", "RColorBrewer",
                 "ade4", "spocc")

missing_pkgs <- ntbox_pkgs[which(!ntbox_pkgs %in% installed.packages())]

if(length(missing_pkgs))
  install.packages(missing_pkgs)
devtools::install_github("luismurao/leaflet")
devtools::install_github("AnalytixWare/ShinySky")
devtools::install_github("ENMGadgets", "narayanibarve")
```

and then try to install `ntbox` again 
```r
devtools::install_github('luismurao/ntbox')
```

## Usage 

```r
library(ntbox)
run_ntbox()

```

## Tutorial
[nichetoolbox](https://luismurao.github.io/GSoC/gsoc_final_eval.html)

## Acknowledgements

Posgrado en Ciencias Biológicas UNAM for academic training; GSoC 2016, PAIPIIT IN112175 (2015) and IN116018 (2018) for partial financial support.
