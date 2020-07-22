# NicheToolBox

`ntbox`is the stable version of `nichetoolbox` which is an R package with a friendly Graphical User Interface (GUI) developed using shiny framework. The package aims to facilitate the process of building niche models and estimate the species distributions.

# Installation text

<span style="color:red">**Windows users:**</span> Before installation it is important to have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

<span style="color:red">**Mac users:**</span> Install Xcode command line tools 

<span style="color:red">**Linux users:**</span> please, follow the installation guide.

## Complete installation guide

Complete installation guide for Windows, Linux, and MacOS users https://luismurao.github.io/ntbox_installation_notes.html


```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox')
# If you want to build vignette, install pandoc before and then
devtools::install_github('luismurao/ntbox',build_vignettes=TRUE)
```

# Usage

The function that launches the GUI is 

```r
library(ntbox)
run_ntbox()

```

The package has three vignettes: a complete reference of the [GUI](https://luismurao.github.io/ntbox/articles/gui_reference.html), one for the [ellipsoid calibration and selection](https://luismurao.github.io/ntbox/articles/ellipsoid_selection.html) using R commands and one on how to obtain and clean [GBIF data](https://luismurao.github.io/ntbox/articles/GBIF_data_curation.html)


# GUI reference guide

Here you can find the reference guide for the Graphical user interface https://luismurao.github.io/ntbox_user_guide.html

# Help pages

Users can fin help for all `ntbox` functions at https://luismurao.github.io/ntbox/

# References

Please cite as

Osorio‐Olvera, L., Lira‐Noriega, A., Soberón, J., Townsend Peterson, A., Falconi, M., Contreras‐Díaz, R.G., Martínez‐Meyer, E., Barve, V. and Barve, N. (2020), ntbox: an R package with graphical user interface for modeling and evaluating multidimensional ecological niches. Methods Ecol Evol. Accepted Author Manuscript. doi:10.1111/2041-210X.13452

The paper of the application is accesible from

https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13452

# Acknowledgements

Posgrado en Ciencias Biológicas UNAM for academic training; GSoC 2016, PAIPIIT IN112175 (2015) and IN116018 (2018) for partial financial support.
