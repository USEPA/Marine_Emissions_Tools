# US EPA ShipEF

ShipEF is an R package that contains functions to assign marine vessel emission factors.

This package can be used in conjunction with [ship power estimates](../ShipPowerModel) and ship activity data to estimate CMV air emissions.

## Getting Started

These instructions should get ShipEF installed and running within your installation of R. See the vignettes and library documentation for how to use this package.

### Prerequisites

Building and installing this package and its documentation requires R version 3.5 or greater and the following packages:

* devtools
* data.table
* testthat
* knitr
* rmarkdown
* ggplot2

### Installation

You can install this package directly from github using the following command:

```
devtools::install_github(repo = "USEPA/Marine_Emissions_Tools",
                         subdir = "ShipEF",
                         build_vignettes = TRUE
                        )
```

## EPA Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
