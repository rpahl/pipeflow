# pipeflow

A lightweight yet powerful framework for building robust data analysis
pipelines. With {pipeflow}, you initialize a pipeline with your dataset
and construct workflows step by step simply by adding R functions. You
can modify, remove, or insert steps and parameters at any stage, while
{pipeflow} ensures the pipeline’s integrity.

Thanks to its intuitive interface, using {pipeflow} quickly pays off in
the beginning while in the long run will help you to keep a clear and
structured overview of your project.

![cartoon](reference/figures/cartoon.png)

### Why use {pipeflow}

- Easy to learn yet suited for growingly complex workflows
- Automatically manages function and parameter dependencies
- Promotes structured and modular code
- Facilitates reusability and collaboration
- Simplifies error handling and debugging

### {pipeflow} vs {targets}

[{targets}](https://docs.ropensci.org/targets/) is the most widely used
pipeline toolkit in the R ecosystem. For a detailed comparison and
benchmark results, see the [vs targets
vignette](https://rpahl.github.io/pipeflow/articles/v07-vs-targets.html).

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
