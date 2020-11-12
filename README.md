
<!-- README.md is generated from README.Rmd. Please edit only README.Rmd! -->

<!-- # dlmextra -->

<!-- Extra functionality for the dlm package (for dynamic linear models) -->

# `dlmextra`: Extra functionality for the `dlm` package

<!-- badges: start -->

[![R build
status](https://github.com/r-cas/caracas/workflows/R-CMD-check/badge.svg)](https://github.com/r-cas/caracas/actions)
[![codecov.io](https://codecov.io/gh/r-cas/caracas/branch/master/graphs/badge.svg)](https://codecov.io/gh/r-cas/caracas?branch=master)
<!-- badges: end -->

## Installation

<!-- `caracas` is available on CRAN and can be installed as usual: -->

<!-- ``` -->

<!-- install.packages('caracas') -->

<!-- ``` -->

To build and install from Github with vignettes run this command from
within `R` (please install `remotes` first if not already installed):

    # install.packages('remotes')
    remotes::install_github("r-cas/caracas", build_vignettes = TRUE)

You can also install the package without vignettes if needed as follows:

    remotes::install_github("r-cas/caracas")

<!-- ## Configuring the Python environment -->

<!-- The `caracas` package uses the [`reticulate`](https://github.com/rstudio/reticulate) package (to run Python code). Thus, if you wish to configure your Python environment, you need to 1) load `reticulate`, 2) configure the Python environment, and 3) load `caracas`. The Python environment can be configured as described [here](https://rstudio.github.io/reticulate/articles/versions.html). Again, this need to be done *before* loading `caracas`. -->

## Development site

See <https://github.com/hojsgaard/dlmextra>.

## Online documentation

See <https://r-cas.github.io/caracas/>.

## Brief introduction

``` r
library(dlmextra)
```

Please find more examples in the other vignettes available at
<https://r-cas.github.io/caracas/>.
