# ShipPowerModel

ShipPowerModel is an R package that contains functions for modeling the propulsive power of commercial marine vessels (CMV). It contains four different power models:

* Propeller law load factor model
* Admiralty formula load factor model
* Holtrop-Mennen resistance model
* Kristensen resistance model 

This package can be used in conjunction with vessel information and emission factors to estimate CMV air emissions.

## Getting Started

These instructions should get ShipPowerModel installed and running within your installation of R. See the vignettes and library documentation for how to use this package.

### Prerequisites

Building and installing this package and its documentation requires R version 3.5 or greater and the following packages:

* devtools
* testthat
* knitr
* rmarkdown
* ggplot2


### Installation

You can install this package directly from github using the following command:

```
devtools::install_github(repo = "USEPA/Marine_Emissions_Tools",
                         subdir = "ShipPowerModel",
                         build_vignettes = TRUE
                        )
```


## Authors

* **Isabela Brown**
* **Michael Aldridge**
* **Daniel Bizer-Cox**
