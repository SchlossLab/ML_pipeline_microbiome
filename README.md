
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mikropml

> meek-ROPE em el

User-Friendly R Package for Supervised Machine Learning Pipelines

<!-- badges: start -->

[![check](https://github.com/SchlossLab/mikropml/workflows/check/badge.svg)](https://github.com/SchlossLab/mikropml/actions?query=workflow%3Acheck+branch%3Amaster)
[![codecov](https://codecov.io/gh/SchlossLab/mikropml/branch/master/graph/badge.svg)](https://codecov.io/gh/SchlossLab/mikropml)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/SchlossLab/mikropml/blob/master/LICENSE.md)
[![CRAN](https://img.shields.io/cran/v/mikropml?color=blue&label=CRAN&logo=R)](https://CRAN.R-project.org/package=mikropml)
[![Conda](https://img.shields.io/conda/vn/conda-forge/r-mikropml)](https://anaconda.org/conda-forge/r-mikropml)
<!-- badges: end -->

An interface to build machine learning models for classification and
regression problems. `mikropml` implements the ML pipeline described by
[Topçuoğlu *et al.* (2020)](https://doi.org/doi:10.1128/mBio.00434-20)
with reasonable default options for data preprocessing, hyperparameter
tuning, cross-validation, testing, model evaluation, and interpretation
steps. See the [website](http://www.schlosslab.org/mikropml/) for more
information, documentation, and examples.

## Installation

You can install the latest release from
[CRAN](https://cran.r-project.org/package=mikropml):

``` r
install.packages('mikropml')
```

or the development version from
[GitHub](https://github.com/SchlossLab/mikRopML):

``` r
# install.packages("devtools")
devtools::install_github("SchlossLab/mikropml")
```

or install from a terminal using
[conda](https://docs.conda.io/projects/conda/en/latest/index.html):

``` bash
conda install -c conda-forge r-mikropml
```

### Dependencies

  - Imports: caret, dplyr, e1071, glmnet, kernlab, MLmetrics,
    randomForest, rlang, stats, utils, xgboost
  - Suggests: doFuture, foreach, future, future.apply, ggplot2, knitr,
    purrr, rmarkdown, rpart, testthat, tidyr

## Usage

Check out the [introductory
vignette](http://www.schlosslab.org/mikropml/articles/introduction.html)
for a quick start tutorial. For a more in-depth discussion, read [all
the vignettes](http://www.schlosslab.org/mikropml/articles/index.html)
and/or take a look at the [reference
documentation](http://www.schlosslab.org/mikropml/reference/index.html).
We also provide an [example Snakemake
workflow](https://github.com/SchlossLab/mikropml-snakemake-workflow) for
running `mikropml` on an HPC.

## Help & Contributing

If you come across a bug, [open an
issue](https://github.com/SchlossLab/mikropml/issues) and include a
[minimal reproducible example](https://www.tidyverse.org/help/).

If you’d like to contribute, see our guidelines
[here](http://www.schlosslab.org/mikropml/CONTRIBUTING.html).

## Code of Conduct

Please note that the mikropml project is released with a [Contributor
Code of
Conduct](http://www.schlosslab.org/mikropml/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Why the name?

The word “mikrop” (pronounced “meek-ROPE”) is Turkish for “microbe”.
This package was originally implemented as a machine learning pipeline
for microbiome-based classification problems (see [Topçuoğlu *et al.*
2020](https://doi.org/10.1128/mBio.00434-20)). We realized that these
methods are applicable in many other fields too, but stuck with the name
because we like it\!
