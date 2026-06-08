
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

# pipeflow <img src="man/figures/logo.png" alt="logo" align="right" width="163" height="121"/>

A lightweight yet powerful framework for building robust data analysis
pipelines. With {pipeflow}, you initialize a pipeline with your dataset
and construct workflows step by step simply by adding R functions. You
can modify, remove, or insert steps and parameters at any stage, while
{pipeflow} ensures the pipeline’s integrity.

Thanks to its intuitive interface, using {pipeflow} quickly pays off in
the beginning while in the long run will help you to keep a clear and
structured overview of your project.

<img src="man/figures/cartoon.png" alt="cartoon" align="right" width="330"/>

### Why use {pipeflow}

- Easy to learn yet suited for growingly complex workflows
- Automatically manages function and parameter dependencies
- Promotes structured and modular code
- Facilitates reusability and collaboration
- Simplifies error handling and debugging

### {pipeflow} vs {targets}

[{targets}](https://docs.ropensci.org/targets/) is the most widely used
pipeline toolkit in the R ecosystem and the de-facto standard for
heavy-duty reproducible workflows. The table below contrasts the two
packages to help you decide which one fits your project.

| Feature | **targets** | **pipeflow** |
|----|----|----|
| Paradigm | Declarative — define the full DAG upfront in a `_targets.R` script, then execute | Interactive — incrementally build the pipeline with `pip_add()` as you code |
| Execution | `tar_make()` runs in a **fresh R process** | `pip_run()` runs in the **current R session** |
| Dependencies | Heavy (~20+ packages: `tarchetypes`, `crew`, `qs`, etc.) | Minimal (`data.table`) |
| Learning curve | Steep — requires understanding `_targets.R` conventions, storage formats, cue rules | Shallow — R-native API with few concepts |
| Skip up-to-date steps | ✅ Hash-based invalidation of code and data | ✅ State-based (`done` / `outdated`) |
| Modify pipeline at runtime | ❌ Must edit `_targets.R` and re-run | ✅ `pip_remove()`, `pip_rename()`, `pip_replace()`, insert with `after =` |
| Parameter management | ❌ No unified parameter view across targets | ✅ `pip_get_params()` / `pip_set_params()` — one call updates all steps |
| Split / map / reduce | Requires target factories (`tarchetypes`) or branching | ✅ Built-in `exec = "split"` / `"auto"` / `"reduce"` |
| Dynamic branching | ✅ Comprehensive via `tarchetypes` | ✅ Auto-mapping over partition keys (`exec = "auto"`) |
| Views / tag filtering | ❌ | ✅ `pip_view()` — filter steps by tags, groups, or index |
| Pipeline composition | ❌ | ✅ `pip_bind()` two pipelines, `pip_add_from()` copy individual steps |
| Self-modifying pipelines | ❌ | ✅ `pip_run(recursive = TRUE)` — steps can return modified pipelines |
| Distributed computing | ✅ `crew` for HPC and cloud workers | ❌ (by design — stays lightweight and single-machine) |
| Cloud storage | ✅ AWS, GCS | ❌ |
| File tracking | ✅ File targets with `format = "file"` | ❌ |
| Step locking | ❌ | ✅ `pip_lock()` / `pip_unlock()` — protect steps from accidental modification |

In short, **{targets}** is the tool of choice for large-scale, formally
reproducible projects that may run on distributed infrastructure.
**{pipeflow}** is designed for interactive development, rapid parameter
exploration, and projects where you want to modify the pipeline
structure as your analysis evolves — all while keeping a shallow
learning curve and minimal dependencies.

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
- [Collecting and filtering
  output](https://rpahl.github.io/pipeflow/articles/v04-collect-output.html)

### Advanced topics

- [Split, map, and
  reduce](https://rpahl.github.io/pipeflow/articles/v05-split-map-reduce.html)
- [Recursive
  self-modification](https://rpahl.github.io/pipeflow/articles/v06-self-modify-pipeline.html)
