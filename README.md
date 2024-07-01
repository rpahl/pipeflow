
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![test-coverage](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml)
[![lint](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml)

<!-- badges: end -->

# pipeflow <img src="man/figures/logo.png" align="right" width="163" height="121"/>

`pipeflow` provides an *easy* and yet *powerful* framework for composing
and running data analysis pipelines. It can be used in a wide range of
data analysis projects, from small exploratory analyses to large and
complex production pipelines.

Since a `pipeflow` pipeline basically conists of a sequence of R
functions and can be used interactively, it is very easy to learn for
typical R users. At the same time, it provides a lot of features that
are useful for more advanced users, such as the ability to dynamically
create branches or to let pipelines modify themselves at runtime.

### Why to use `pipeflow`

- easy to learn
- promotes standardized analysis workflows
- eases handling of complex analysis pipelines
- improves reproducibility and reusability of code
- eases error handling and debugging

### Features

- use interactively
- intuitive definition of dependencies
- validity of dependencies verified at definition time
- view and manage all pipeline parameters in one place
- automatically skip steps that are already up to date (similar to
  `make`)
- logging
- re-use, modify, extend and/or combine existing pipelines
- can be integrated with R packages, shiny applications or any other R
  code

### Advanced features

- dynamically create branches by applying same pipeline to multiple
  datasets
- let pipelines modify themselves at runtime

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

It is recommended to read the vignettes in the order they are listed
below:

- [Get started with
  pipeflow](https://rpahl.github.io/pipeflow/articles/get-started.html)
- [Modifying existing
  pipelines](https://rpahl.github.io/pipeflow/articles/modify-pipeline.html)
- [Combining
  pipelines](https://rpahl.github.io/pipeflow/articles/combine-pipelines.html)
- [Collecting
  output](https://rpahl.github.io/pipeflow/articles/collect-output.html)

### Advanced topics

- [split data and combine
  output](https://rpahl.github.io/pipeflow/articles/split-and-combine.html)
