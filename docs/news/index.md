# Changelog

## pipeflow 0.2.3.9006

- **C++ DAG engine** (`src/dag.cpp`, 882 lines): All graph operations
  (node/edge add/remove, topological ordering, reachability queries) now
  run at C++ speed via Rcpp external pointers and `.Call` interface.
  This eliminates the R object overhead for dependency resolution.
- **New and improved `pip_*` API**: Functional API replacing the R6
  `Pipeline` class —
  [`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md),
  [`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md),
  [`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md),
  [`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md),
  [`pip_clone()`](https://github.com/rpahl/pipeflow/reference/pip_clone.md),
  [`pip_bind()`](https://github.com/rpahl/pipeflow/reference/pip_bind.md),
  etc.
- **Execution modes** (`auto`/`split`/`reduce`/`plain`): Native support
  for map-reduce style workflows where steps can split output into named
  partitions and downstream steps auto-map over them.
- **Backward compatibility**: Legacy `pipe_*` functions and `Pipeline`
  R6 class preserved as deprecated aliases (`R/aliases.R`,
  `R/pipelineR6.R`).
- [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
  — create filtered pipeline views (by dependencies, tags)
- [`pip_tag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md)
  /
  [`pip_untag()`](https://github.com/rpahl/pipeflow/reference/pip_untag.md)
- `[[` extract operator for pipelines
- Comprehensive revision of all vignettes to match new API
- MIT license, updated CI workflows, dependabot config, kilo.json,
  lintr/styler config
- Extensive test suite (~10K+ lines) covering C++ DAG, pip\_\* API,
  aliases, execution modes, and recursive runs

## pipeflow 0.2.3.9005

- deprecate legacy `pipe_*` API in favor of the new `pip_*` API
- deprecate legacy `Pipeline` R6 interface (`Pipeline$new()`)

## pipeflow 0.2.3.9005

- refactor: deprecate `keepOut` field and introduce `tags` field

## pipeflow 0.2.3.9004

- deprecate `all` parameter in
  [`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md)
- deprecate
  [`pipe_set_data_split()`](https://github.com/rpahl/pipeflow/reference/pipe_set_data_split.md)
  and
  [`pipe_set_keep_out()`](https://github.com/rpahl/pipeflow/reference/pipe_set_keep_out.md)
- add new functions to skip and unskip pipeline steps and groups:
  `pipe_skip_step()`, `pipe_unskip_step()`, `pipe_skip_group()`, and
  `pipe_unskip_group()`.
- add new function, `pipe_get_step_field()` to retrieve a specific field
  of a pipeline step
- remove advanced vignette on using pipeflow with split data sets

## pipeflow 0.2.3.9003

- Refactor: Remove Param object handling from pipeline and utility
  functions
- Refactor: Remove unused get_params_unique_json method and its
  documentation

## pipeflow 0.2.3.9002

- refactor: ensure unit tests for logging is always done with logging
  enabled
- refactor: add ‘locked’ column and improve indexing performance
- refactor: implement slightly faster step existence check
- feat: set locked status via boolean flag and add has_step_locked
  function

## pipeflow 0.2.3.9001

- feat: implement index map and BFS for dependency graphs (improved
  performance)
- chore: add benchmark for running long linear pipelines

## pipeflow 0.2.3.9000

- Same as previous version.

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
