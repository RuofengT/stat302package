
# stat302package

<!-- badges: start -->
[![R-CMD-check](https://github.com/RuofengT/stat302package/workflows/R-CMD-check/badge.svg)](https://github.com/RuofengT/stat302package/actions)
<!-- badges: end -->

The goal of stat302package is to ...

## Installation

You can install the the package from GitHub using:

``` r
devtools::install_github("https://github.com/RuofengT/stat302package")
```
To view the vignettes:

```{r}
devtools::install_github("RuofengT/stat302package", build_vignette = TRUE, build_opts = c())
library(stat302package)
# Use this to view the vignette in the stat302package HTML help
help(package = "stat302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302package")
```
