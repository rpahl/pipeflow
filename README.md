
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
[![CI
status](https://github.com/rpahl/pipeflow/actions/workflows/ci.yaml/badge.svg)](https://github.com/rpahl/pipeflow/actions/workflows/ci.yaml)
<!-- [![CRAN checks](https://badges.cranchecks.info/summary/pipeflow.svg)](https://cran.r-project.org/web/checks/check_results_pipeflow.html) -->
[![Downloads per
month](https://cranlogs.r-pkg.org/badges/last-month/pipeflow)](https://cran.r-project.org/package=pipeflow)
[![Downloads
total](https://cranlogs.r-pkg.org/badges/grand-total/pipeflow)](https://cran.r-project.org/package=pipeflow)
[![Last
commit](https://img.shields.io/github/last-commit/rpahl/pipeflow.svg)](https://github.com/rpahl/pipeflow/commits/main)
[![Lifecycle
status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# pipeflow <img src="man/figures/logo.png" alt="pipeflow hex logo" align="right" width="120" height="139"/>

{pipeflow} is a lightweight framework for building interactive data
analysis pipelines in R. You add R functions one by one as steps, tell
{pipeflow} which steps they take their inputs from, and it keeps the
pipeline consistent as you insert, replace or remove steps.

<img src="man/figures/cartoon.png" alt="cartoon" align="right" width="330"/>

### Why pipeflow?

- concise API: steps are plain R functions
- all parameters in one place
- checked as you build
- reruns only what changed
- fast: data.table and C++ DAG under the hood
- feature rich: views, split/map/reduce, nested pipelines

### Installation

``` r
# Install release version from CRAN
install.packages("pipeflow")

# Install development version from GitHub
devtools::install_github("rpahl/pipeflow")
```

### Cheat sheet

<a href="https://github.com/rpahl/pipeflow/blob/main/cheatsheet/pipeflow-cheatsheet.pdf"><img src="man/figures/cheatsheet-1.png" alt="pipeflow cheat sheet, page 1" width="330"/>
<img src="man/figures/cheatsheet-2.png" alt="pipeflow cheat sheet, page 2" width="330"/></a>

### Usage

``` r
library(pipeflow)

p <- pip_new("demo") |>
    pip_add("numbers", \(n = 5) seq_len(n)) |>
    pip_add("squared", \(x = ~numbers) x^2) |>
    pip_add("total", \(x = ~squared) sum(x))

p
# <pipeflow> demo (3 steps)
# -------------------------
#       step params depends state
# 1: numbers      n           new
# 2: squared      x numbers   new
# 3:   total      x squared   new
# -------------------------
# <ready> last run: never

pip_run(p)
# info [2026-10-15 10:21:00.423 UTC]: Starting run of pipeflow 'demo'
# info [2026-10-15 10:21:00.424 UTC]: Step 1/3 numbers
# info [2026-10-15 10:21:00.426 UTC]: Step 2/3 squared
# info [2026-10-15 10:21:00.428 UTC]: Step 3/3 total
# info [2026-10-15 10:21:00.429 UTC]: Finished run of pipeflow 'demo'

pip_collect(p)
# $numbers
# [1] 1 2 3 4 5
# 
# $squared
# [1]  1  4  9 16 25
# 
# $total
# [1] 55
```

### Getting Started

It is recommended to read the vignettes in the order they are listed
below:

- [Get started with
  pipeflow](https://rpahl.github.io/pipeflow/articles/v01-get-started.html)
- [Modify existing
  pipelines](https://rpahl.github.io/pipeflow/articles/v02-modify-pipeline.html)
- [Combine
  pipelines](https://rpahl.github.io/pipeflow/articles/v03a-combine-pipelines.html)
- [Pipeline
  views](https://rpahl.github.io/pipeflow/articles/v03b-pipeline-views.html)
- [Collect and group
  output](https://rpahl.github.io/pipeflow/articles/v04-collect-output.html)

### Advanced workflows

- [Split, map, and
  reduce](https://rpahl.github.io/pipeflow/articles/v05a-split-map-reduce.html)
- [Nested
  pipelines](https://rpahl.github.io/pipeflow/articles/v05b-nested-pipeline.html)
- [Handling
  failures](https://rpahl.github.io/pipeflow/articles/v05c-error-handling.html)
- [Annotated parameter
  values](https://rpahl.github.io/pipeflow/articles/v05d-param-value.html)
- [Self-modifying
  pipelines](https://rpahl.github.io/pipeflow/articles/v06-self-modify-pipeline.html)

### Benchmarks

- [pipeflow vs
  targets](https://rpahl.github.io/pipeflow/articles/v07-vs-targets.html)
