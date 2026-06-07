---
name: 'R Markdown Conventions'
description: 'Conventions for R Markdown vignettes and documents'
applyTo: '**/*.Rmd'
---

# R Markdown / Vignette Conventions

- Keep lines wrapped at ≤80 characters where feasible.
- Use informative chunk labels (e.g., `data-prep`, `model-fit`) rather
  than auto-numbering.
- Set global knitr options at the top of each `.Rmd` file:

  ````markdown
  ```{r knitr-setup, include = FALSE}
  knitr::opts_chunk$set(
      comment = "#",
      prompt = FALSE,
      tidy = FALSE,
      cache = FALSE
  )
  ```
  ````

- Load the package with `require(pipeflow)` or `library(pipeflow)` in a
  setup chunk, not inline.
- Use `devtools::load_all(quiet = TRUE)` before rendering vignettes
  outside of `pkgdown::build_site()`.
- Vignette YAML headers should follow this template:

  ```yaml
  ---
  title: "Vignette title"
  output:
    rmarkdown::html_vignette:
      toc: true
      toc_depth: 4
  vignette: >
    %\VignetteIndexEntry{Vignette title}
    %\VignetteEngine{knitr::rmarkdown}
    %\VignetteEncoding{UTF-8}
  ---
  ```
