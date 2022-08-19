# baffle
Waffle graphs in base R graphics.

## Installation

You can install the latest version of baffle from [CRAN](https://CRAN.R-project.org) with:

```r
install.packages("baffle")
```

or directly from GitHub using the `devtools` package:
```r
devtools::install_github("j-moravec/baffle")
```

## Example

See [online documentation](https://j-moravec.github.io/baffle/) for a more complex examples.

```r
library(baffle)
waffle(c(25,75), col=c("darkorchid", "lightgray"))
```

![](man/figures/waffle.svg)
