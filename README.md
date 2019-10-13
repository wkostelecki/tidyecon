# tidyecon

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/wkostelecki/tidyecon.svg?branch=master)](https://travis-ci.org/wkostelecki/tidyecon)
<!-- badges: end -->

The goal of tidyecon is to provide high level functions for specifying cross-sectional models with decompositions.

## Installation

You can install from github with:

``` r
devtools::install_github("wkostelecki/tidyecon")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyecon)
model = estimate(mtcars, c("1", setdiff(names(mtcars), "mpg")), list(y = "mpg"))
decomp = decomp(model)
plot(decomp)
```

