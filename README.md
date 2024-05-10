
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![test-coverage](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml)
[![lint](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml)

<!-- badges: end -->

The goal of `pipeflow` is to provide a *lightweight* yet *powerful*
framework for composing and managing custom data analysis pipelines.
Dependencies among analysis steps are tracked automatically and are used
to re-run only steps that are affected by input or parameter changes,
similar to the `make` utility.

The package provides similar functionality to the
[targets](https://CRAN.R-project.org/package=targets) package, but aims
to be more *lightweight* and *easier* to use and understand. It can be
used in a wide range of data analysis projects, from small exploratory
analyses to large production pipelines while still providing a high
level of control over the analysis process.

### When to use `pipeflow`

- first reason
- second reason
- third reason

### Installation

``` r
# Install release version from CRAN
install.packages("pipeflow")

# Install development version from GitHub
devtools::install_github("rpahl/pipeflow")
```

### Usage

``` r
library(pipeflow)
```

### Getting Started
