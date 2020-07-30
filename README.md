# baffle
Waffle graphs in the Base graphics.

![example waffle](waffle.png)

## Installation

You can install the released version of baffle from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("baffle")
```

or directly from GitHub using the `devtools` package:
```r
devtools::install_github("j-moravec/baffle")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(baffle)
waffle(c(50,25,25))
waffle(c(25,75), c("darkorchid", "lightgray"))
waffle(c(14,8,4), nrow=3)
```

