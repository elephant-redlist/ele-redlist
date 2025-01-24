African Elephant Red List Assessment (data)  <img src='african-elephant.jpg' align="right" height="120" />
===========================================
[![Build Status](https://app.travis-ci.com/cttedwards/redData.svg?token=oxZdiRsNesp8jgJE5pF3&branch=master)](https://app.travis-ci.com/cttedwards/redData)

The `redData` R-package contains the data and cleaning scripts to accompany the `red` [package](https://github.com/cttedwards/red).

For permission to access the data please fill in the [Data Request Form](https://forms.gle/S1FBWAon51f4rZKE8). For enquiries, please contact the [AERL team](mailto:redlist.africanelephants@gmail.com).

## Installation
This package has been built using `R >= 4.1.0`. To install the package we recommend using the `remotes` package and executing the following command from within an active R session:

`remotes::install_github('cttedwards/redData', build_vignettes = TRUE, dependencies = TRUE)`

## Data and preparation scripts
Data preparation scripts are available from the package vignette:
`vignette("inputs_rla", package = "redData")`

Cleaned data from the [African Elephant Database](http://africanelephantdatabase.org/) are available using:
`data("AED", package = "redData")`

