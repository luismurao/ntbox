# <span style="color:blue">NicheToolBox</span>

`ntbox`is the stable version of `nichetoolbox` which is an R package with a friendly Graphical User Interface (GUI) developed using shiny framework. The package aims to facilitate the process of building niche models and estimate the species distributions.

# <span style="color:red">Installation text</span>

**Windows users:** Before installation it is important to have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/).


## Complete installation guide

Complete installation guide for Windows, Linux, and MacOS users https://luismurao.github.io/ntbox_installation_notes.html

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox')
# If you want to build vignette, install pandoc before and then
devtools::install_github('luismurao/ntbox',build_vignettes=TRUE)
```

# <span style="color:blue">Usage</span>

```r
library(ntbox)
run_ntbox()

```

# <span style="color:blue">Reference guide</span>

Here you can find the reference guide for the Graphical user interface https://luismurao.github.io/ntbox_user_guide.html


# <span style="color:blue">Acknowledgements</span>

Posgrado en Ciencias Biológicas UNAM for academic training; GSoC 2016, PAIPIIT IN112175 (2015) and IN116018 (2018) for partial financial support.
