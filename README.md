# ColabNet - Visualising academic collborations using NCBI

## About
This project aims to create an interactive web application (R Shiny app) which
will have two tabs

### Collaburation Network Exploration

This tab allows users (aimed at students) to see what type of research and 
collaborations are going on in a department with the ability to filter on a 
specific topic, by researcher or collaborations between researchers.

### Collaburation Network Analysis

This tab is geared towards admins and will allows more detailed analysis and 
statistics on the level of researcher collaborations within a department.

## Running the app locally in R

### As a user

#### Install from GitHub

If you are a Windows user, you might need to install
[RTools](https://cran.r-project.org/bin/windows/Rtools/)

Install the app as an R package by running the following command
```r
library(devtools) # install if needed
install_github("pieterjanvc/colabNet")
```

Once the package has been installed, the app can be run as follows:
```r
library(colabNet)
colabNet()
```

#### Install from CRAN

_The package is currently not available on CRAN and needs to be complied from 
source using the GitHub repo_

### As a developer

1) Clone the [colabNet GitHub repo](https://github.com/pieterjanvc/colabNet.git) 
and open the R project in your IDE (organised an R package).
2) Load all functions devtools::load_all() 
(or R Studio: Build -> Load All or Ctrl/Cmd + Shift + L)
3) Run the app with `colabNet()`
