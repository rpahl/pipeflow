# Changelog

## pipeflow 0.3.0.9000

- Same as previous version.

## pipeflow 0.3.0

CRAN release: 2026-06-15

### New features

- **C++ DAG engine** (`src/dag.cpp`): All graph operations (node/edge
  add/remove, topological ordering, reachability queries) run at C++
  speed via Rcpp, eliminating R object overhead for dependency
  resolution.
- **New `pip_*` API**: Functional API replacing the R6 `Pipeline` class
  —
  [`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md),
  [`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md),
  [`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md),
  [`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md),
  [`pip_clone()`](https://github.com/rpahl/pipeflow/reference/pip_clone.md),
  [`pip_bind()`](https://github.com/rpahl/pipeflow/reference/pip_bind.md),
  [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md),
  [`pip_tag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md)/[`pip_untag()`](https://github.com/rpahl/pipeflow/reference/pip_untag.md),
  etc.
- **Execution modes** (`auto`/`split`/`reduce`/`plain`): Native support
  for map-reduce style workflows where steps split output into named
  partitions and downstream steps auto-map over them.
- **Dependency validation at definition time**:
  [`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md),
  [`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md),
  and
  [`pip_remove()`](https://github.com/rpahl/pipeflow/reference/pip_remove.md)
  fail fast on broken references.
- New pkgdown-only article *pipeflow vs targets*
  (`vignettes/articles/v07-vs-targets.Rmd`) with an 18-row feature
  comparison table and benchmarks.

### Breaking changes

- Legacy `pipe_*` functions and `Pipeline` R6 class are deprecated
  (preserved as aliases in `R/aliases.R`, `R/pipelineR6.R`).
- [`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect_out.md)
  no longer accepts `grouped` or `by` parameters. It returns a flat
  named list of step outputs. Use
  [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
  with tags and manual list composition for grouped output.
- The `group` column has been removed from the pipeline data.table.
  Steps previously using `group = "..."` in
  [`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
  should use `tags = "..."`.

### Documentation

- Comprehensive revision of all vignettes to match the new `pip_*` API.
- Revised `vignettes/v04-collect-output.Rmd` to demonstrate tag-based
  grouping via
  [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
  composition.
- Updated README with a “Why use {pipeflow}” feature list, a short usage
  example, and a revised vs-targets summary.

### Internal

- Extensive test suite covering C++ DAG, `pip_*` API, aliases, execution
  modes, and recursive runs.
- MIT license, updated CI workflows, dependabot config, kilo.json,
  lintr/styler config.

## pipeflow 0.2.3

CRAN release: 2025-07-26

- Add Depends R \>= 4.2.0 to DESCRIPTION
- Fix issue caused by update of lgr package
  ([\#571](https://github.com/rpahl/pipeflow/issues/571)e8260)
- Fix badge links in README

## pipeflow 0.2.2

CRAN release: 2024-12-22

- Add News and this Changelog
- Add unit tests and detailed documentation for [alias
  functions](https://rpahl.github.io/pipeflow/reference/index.html#alias-functions)
  ([\#24](https://github.com/rpahl/pipeflow/issues/24))
- Link to other packges via [my R
  universe](https://rpahl.r-universe.dev/packages)
  ([\#25](https://github.com/rpahl/pipeflow/issues/25))

## pipeflow 0.2.1

CRAN release: 2024-12-05

- CRAN release
