
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![test-coverage](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml)
[![lint](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml)
[![codecov.io](https://codecov.io/github/rpahl/pipeflow/coverage.svg?branch=main)](https://codecov.io/github/rpahl/pipeflow?branch=main)
[![Lifecycle_Badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://cran.r-project.org/package=pipeflow)
<!-- badges: end -->

# pipeflow <img src="man/figures/logo.png" alt="logo" align="right" width="163" height="121"/>

`pipeflow` is an intuitive yet powerful framework for building and
running data processing pipelines. Whether you’re working in scientific
computing, machine learning (AI), or statistical reporting, `pipeflow`
adapts seamlessly to any scenario—from small exploratory analyses to
complex production workflows.

Since `pipeflow` pipelines are essentially sequences of R functions,
they’re easy to learn and can be used interactively by typical R users.
Advanced features like dynamic branching and self-modifying pipelines
provide additional flexibility for more complex workflows.

### Why use `pipeflow`

- Easy to learn yet powerful for complex workflows
- Automatically manages function dependencies
- Promotes standardized, reproducible analysis
- Simplifies error handling, debugging, and reusability

### Key features

- *Flexible Application:* Use interactively or programmatically in R
- *Dependency Management:* Dependencies checked at definition, ensuring
  reliable workflows
- *Comprehensive Logging:* Logs each step, with customizable logger
  options
- *Parameter Control:* Easily view and adjust all function parameters in
  one place
- *Modular Composition:* Modify, extend, and combine pipelines
  effortlessly
- *Intelligent Execution:* Skip steps already up-to-date, similar to
  `make`
- *Visualization:* View pipelines in both tabular and graphical formats

### Advanced features

- *Dynamic Branching:* Apply the same pipeline to multiple datasets
  seamlessly
- *Self-Modifying:* Pipelines can adapt and modify themselves at runtime

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

- [How to use pipeflow with split data
  sets](https://rpahl.github.io/pipeflow/articles/split-and-combine.html)
- [How pipelines can modify themselves at
  runtime](https://rpahl.github.io/pipeflow/articles/self-modify-pipeline.html)
