
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![test-coverage](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/check-standard.yaml)
[![lint](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/lint.yaml)
[![codecov.io](https://codecov.io/github/rpahl/pipeflow/coverage.svg?branch=main)](https://codecov.io/github/rpahl/pipeflow?branch=main)
[![Lifecycle_Badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://cran.r-project.org/package=pipeflow)
<!-- badges: end -->

# pipeflow <img src="man/figures/logo.png" alt="logo" align="right" width="163" height="121"/>

A lightweight yet powerful framework for building robust data analysis
pipelines. With `pipeflow`, you initialize a pipeline with your dataset
and construct your workflow step by step by seamlessly adding R
functions. Modify, remove, or insert steps at any stage while `pipeflow`
ensures the integrity and correctness of your pipeline.

Designed to help you focus on the *what* rather than the *how*, this
package simplifies the implementation of complex workflows, making even
large-scale data analysis projects manageable, adaptable, and reusable
over time.

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
