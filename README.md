African Elephant Red List Assessment (model)  <img src='african-elephant.jpg' align="right" height="120" />
============================================

[![Build Status](https://app.travis-ci.com/cttedwards/red.svg?token=oxZdiRsNesp8jgJE5pF3&branch=master)](https://app.travis-ci.com/cttedwards/red)

The `red` R-package fits a regression based population dynamics model to elephant survey data and generates outputs suitable for the IUCN Redlist clasification scheme. These outputs have been published for the [savannah](https://www.iucnredlist.org/species/181008073/204401095) and [forest](https://www.iucnredlist.org/species/181007989/204404464) species.

## Installation
This package has been built using `R >= 4.1.0`. First install `rstan` and then the `redData` [package](https://github.com/cttedwards/redData). To install the `red` package we recommend using:

`R> remotes::install_github('cttedwards/red', dependencies = TRUE)`

Scripts used to run the model are available as package vignettes, which can be viewed using:

`R> browseVignettes(package = "red")`
