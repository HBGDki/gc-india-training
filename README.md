# HBGDki India Grand Challenges Training

Welcome to the repository for the HBGDki India Grand Challenges Training course material.

## Installation

During the training, we will provide cloud resources and "Portable R" on a USB stick for Windows users. However, if you woule like to maintain your own installation of all the material used for this training, please see below.

If you encounter any issues with installation, please file an issue detailing the problem in this repository including the output of running `sessionInfo()` in your R session. Any known issues and possible workarounds will be documented more prominently at the bottom of this README.

For more information about how to use the USB stick that you received at the training, please see the "README-USB.docx" document in this repository.

### System prerequisites:

- Latest version of R (3.3.2) (download and install from [here](https://cran.rstudio.com/))
- Latest version of RStudio (download and install from [here](https://www.rstudio.com/products/rstudio/download/))

### R packages

Install the following R packages with the following commands:

```r
install.packages(c("tidyverse", "rmarkdown", "formatR",
  "devtools", "brokenstick", "face", "plotly", "XML",
  "Hmisc", "reshape", "gamlss", "metrumrg", "nlme",
  "shiny", "ggplot2", "stringr", "data.table", "dplyr",
  "tidyr", "numDeriv", "shiny", "shinyjs", "ggrepel",
  "scales", "DT", "ggkm", "Hmisc", "quantreg", "markdown"
  repos = c(
    CRAN = "http://cran.rstudio.com/",
    deltarho = "http://packages.deltarho.org",
    rforge = "http://R-Forge.R-project.org"))
```

## Course material

This repository contains all of the resources needed for the course. You can either clone the repository or simply [download the zip file](https://github.com/HBGDki/gc-india-training/archive/master.zip) for the repository and unzip it on your computer.

Once you have the repository, you simply need to open up `material/training.Rmd` in RStudio and you are ready to go.

You can also look at some of the shiny applications inside the `shinyapps` directory by setting your working directory to tat of the desired application and running `shiny::runApp()`.

## Resources

The following resources can be useful to browse prior to the tutorial to help attendees have a better understanding of some concepts that will be built upon.

- [R for Data Science](http://r4ds.had.co.nz/)
- [dplyr vignette](https://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html)
- [ggplot2 cheatsheet](https://www.rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf)

