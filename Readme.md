# growmod: growth curve modelling and validation in R 

This README is for the R package growmod

Copyright &copy; 2017, Jian Yen

*****

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
`growmod` is a collection of R functions for fitting regression models to growth curves.
The emphasis is on easy model fitting and simple interfaces for extensive model comparison
and model validation. All functions in `growmod` are written in R 3.4.0 and use Stan 2.12.0
and rstan 2.15.1.

Created 31 May 2017

Updated 7 July 2017

*****

## Installation
growmod is distributed as an R package in source form, and is not currently available through the CRAN.

- the easiest (and recommended) method for installation is to use the devtools package to install directly from the GitHub source:
```
if (!require(devtools)) {
  install.packages("devtools")
}
# install the current version of growmod
devtools::install_github("jdyen/growmod")
```

*****

## Usage
Once `growmod` has been installed there is one function to fit several different growth models: `growmod`. This function has a formula interface (`growmod.formula`) and information about its use can be found by typing `?growmod` in the R console. The formula interface has been set up to handle data clustered into blocks and predictor variables relevant to each block, with the syntax `size ~ (age | block / predictor)`. An example use-case for this setup is growth curves measured on species, with trait data for each species. The model then would be `size ~ (age | species / traits)`, and could be used to predict growth curves for any given species based on its trait values.

A fitted growth model is a `growmod` object and can be validated using the `validate` function. This function can perform cross-validation on a fitted model or can be used to validate a fitted growth model against a holdout data set.

Mathematical and statistical details of the models in growmod are in:

Thomas F, et al. (in prep.)

Several applications of growth curve modelling to ecology are:

Thomas F and Vesk P (2017)

Thomas F and Vesk P (2017)

Models fitted using `growmod` are of class grow_mod and have several S3 methods available: `print`, `summary`, `plot`, `coef`, `predict`, `fitted` and `residuals`. 

*****

## Feedback
Please report bugs or issues via the issues tracking system on this page.
Send comments, feedback or queries to: <jdl.yen@gmail.com>

