# growmodr: growth curve modelling and validation in R 

This README is for the R package growmodr

Copyright &copy; 2017, Jian Yen

*****

[![Build Status](https://travis-ci.org/jdyen/growmodr.svg?branch=master)](https://travis-ci.org/jdyen/growmodr)
[![Coverage Status](https://img.shields.io/codecov/c/github/jdyen/growmodr/master.svg)](https://codecov.io/github/jdyen/growmodr?branch=master)

## License details
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*****

## Overview
`growmodr` is a collection of R functions for fitting regression models to growth curves.
The emphasis is on easy model fitting and simple interfaces for extensive model comparison
and model validation. All functions in `growmodr` are written in R 3.4.0 and use Stan 2.12.0
and rstan 2.15.1.

Created 31 May 2017

Updated 11 August 2017

*****

## Installation
`growmodr` is distributed as an R package in source form, and is not currently available through the CRAN.

- installing `growmodr` requires an appropriate toolchain. The Stan developers provide excellent guides to installing RStan and an appropriate toolchain: [Windows](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows) and [Mac/Linux](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux). 
- the easiest (and recommended) method for installation is to use the devtools package to install directly from the GitHub source:
```
if (!require(devtools)) {
  install.packages("devtools")
}
# install the current version of growmodr
devtools::install_github("jdyen/growmodr", args = "--preclean")
```

*****

## Usage
Once `growmodr` has been installed there is one function to fit several different growth models: `growmod`. This function has a formula interface (`growmod.formula`) and information about its use can be found by typing `?growmod` in the R console. The formula interface has been set up to handle data clustered into blocks and predictor variables relevant to each block, with the syntax `size ~ (age | block / predictor)`. An example use-case for this setup is growth curves measured on species, with trait data for each species. The model then would be `size ~ (age | species / traits)`, and could be used to predict growth curves for any given species based on its trait values.

A fitted growth model is a `growmod` object and can be validated using the `validate` function. This function can used to cross-validate a fitted model or can be used to validate a fitted growth model against a holdout data set.

Models fitted using `growmod` are of class `growmod` and have several S3 methods available: `print`, `summary`, `plot`. Models validated using `validate` are of class `growmod_cv` and have `print`, `summary`, and `plot` methods available. 

A typical workflow for fitting a growth model with `growmod` would look something like:
```
# load growmodr package
library(growmodr)

# load or simulate data
data <- growmod_sim()

# fit growth model
mod <- growmod(size ~ (index | block / predictors),
               data = data,
               n_iter = 5000,
               n_chains = 4)

# summarise fitted model
summary(mod)
plot(mod)

# validate fitted model using leave-one-out cross validation
mod_val <- validate(mod, n_cv = 'loo')

# summarise cross-validation performance
summary(mod_val)
plot(mod_val)
```

## Further reading
Mathematical and statistical details of the models in growmod are in:

Thomas, F.M., Yen, J.D.L. and Vesk, P.A. (in prep.) Which nonlinear model? Using cross-validation to evaluate growth models in ecology.

Several applications of growth curve modelling to ecology are:

Thomas, F.M. and Vesk, P.A. (2017) Are trait-growth models transferable? Predicting multi-species growth trajectories between ecosystems using plant functional traits. PLoS ONE 12(5): e0176959.

Thomas, F.M. and Vesk, P.A. (in press) Height growth in woody plants examined with a trait-based model. Austral Ecology. DOI: 10.1111/aec.12501.

*****

## Feedback
Please report bugs or issues via the issues tracking system on this page.
Send comments, feedback or queries to: <jdl.yen@gmail.com>

