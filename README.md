
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
release](https://www.r-pkg.org/badges/version/pipeflow)](https://cran.r-project.org/package=pipeflow)
[![Dependencies](https://tinyverse.netlify.app/badge/pipeflow)](https://CRAN.R-project.org/package=pipeflow)
[![Code
coverage](https://codecov.io/gh/rpahl/pipeflow/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rpahl/pipeflow)
[![R-CMD-check
status](https://github.com/rpahl/pipeflow/workflows/R-CMD-check/badge.svg)](https://github.com/rpahl/pipeflow/actions)
[![Test
coverage](https://github.com/rpahl/pipeflow/workflows/test-coverage/badge.svg)](https://github.com/rpahl/pipeflow/actions)
[![Linter
status](https://github.com/rpahl/pipeflow/workflows/lint/badge.svg)](https://github.com/rpahl/pipeflow/actions)
[![CRAN
checks](https://badges.cranchecks.info/summary/pipeflow.svg)](https://cran.r-project.org/web/checks/check_results_pipeflow.html)
[![Downloads per
month](https://cranlogs.r-pkg.org/badges/last-month/pipeflow)](https://cran.r-project.org/package=pipeflow)
[![Downloads
total](https://cranlogs.r-pkg.org/badges/grand-total/pipeflow)](https://cran.r-project.org/package=pipeflow)
[![Last
commit](https://img.shields.io/github/last-commit/rpahl/pipeflow.svg)](https://github.com/rpahl/pipeflow/commits/main)
[![Lifecycle
status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# pipeflow <img src="man/figures/logo.png" alt="logo" align="right" width="163" height="121"/>

A lightweight yet powerful framework for building robust data analysis
pipelines. With `pipeflow`, you initialize a pipeline with your dataset
and construct workflows step by step by adding R functions. You can
modify, remove, or insert steps and parameters at any stage, while
`pipeflow` ensures the pipeline’s integrity.

Overall, `pipeflow` offers a beginner-friendly framework that simplifies
and streamlines the development of data analysis pipelines by making
them modular, intuitive, and adaptable. Thanks to its intuitive
interface, using `pipeflow` quickly pays off.

### Why use `pipeflow`

- Easy to learn yet suited for complex workflows
- Automatically manages function dependencies
- Promotes standardized, modular, and reproducible analyses
- Simplifies error handling, debugging, and reusability

### Key features

- *Flexible Application:* Use interactively or programmatically in R
- *Dependency Management:* Dependencies checked at definition time
  always ensure the pipeline’s integrity
- *Comprehensive Logging:* Logs each step, with customizable logger
  options
- *Parameter Control:* View and manage all analysis parameters in one
  place
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
  pipeflow](https://rpahl.github.io/pipeflow/articles/v01-get-started.html)
- [Modifying existing
  pipelines](https://rpahl.github.io/pipeflow/articles/v02-modify-pipeline.html)
- [Combining
  pipelines](https://rpahl.github.io/pipeflow/articles/v03-combine-pipelines.html)
- [Collecting
  output](https://rpahl.github.io/pipeflow/articles/v04-collect-output.html)

### Advanced topics

- [How to use pipeflow with split data
  sets](https://rpahl.github.io/pipeflow/articles/v05-split-and-combine.html)
- [How pipelines can modify themselves at
  runtime](https://rpahl.github.io/pipeflow/articles/v06-self-modify-pipeline.html)
