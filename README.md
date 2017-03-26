# NicheToolBox

`ntbox`is the stable version of `nichetoolbox` which is an R package with a friendly Graphical User Interface (GUI) developed using shiny framework. The package aims to facilitate the process of building niche models and estimate the species distributions.

## Installation
```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox')
```
### Warning

There is a bug in `install_github` function of the `devtools` package. Because of this sometimes you will have troubles while installing `nichetoolbox` package (see [#issue 3](https://github.com/luismurao/nichetoolbox/issues/3)). In order to install it you will first have to install some of the dependencies of the package: 

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
Note that the error is generated because **there is no package called 'ridigbio'** , Just try to install the `ridigbio` package

``` r
install.packages("ridigbio")
```

and then try to install `ntbox` again 
```r
devtools::install_github('luismurao/ntbox')
```

If the error **there is no package called 'package_name'** is shown again, do the above (`install.package("package_name")` and then `devtools::install_github('luismurao/ntbox')`) untill the error does not appear.


## Usage 

```r
library(ntbox)
run_ntbox()

```

## Tutorial
[nichetoolbox](https://luismurao.github.io/GSoC/gsoc_final_eval.html)
