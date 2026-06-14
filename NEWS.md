<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# pipeflow 0.2.3.9010

- Same as previous version.


# pipeflow 0.2.3.9009

- Same as previous version.


# pipeflow 0.2.3.9008

- Same as previous version.


# pipeflow 0.2.3.9007

- `pip_collect_out()` no longer accepts `grouped` or `by` parameters.
- The `group` column has been removed from the pipeline data.table.
- New pkgdown-only article *pipeflow vs targets*
- Revised `vignettes/v04-collect-output.Rmd` to demonstrate
- Updated README with a "Why use {pipeflow}" feature list, a
- Added "Dependency validation" row to the vs-targets comparison
- Fixed Ubuntu CI by adding `libglpk-dev` to system dependencies
- Extensive test updates for the `group` column removal and


# pipeflow 0.2.3.9006

- **C++ DAG engine** (`src/dag.cpp`, 882 lines): All graph operations (node/edge add/remove, topological ordering, reachability queries) now run at C++ speed via Rcpp external pointers and `.Call` interface. This eliminates the R object overhead for dependency resolution.
- **New and improved `pip_*` API**: Functional API replacing the R6 `Pipeline` class — `pip_new()`, `pip_add()`, `pip_run()`, `pip_replace()`, `pip_clone()`, `pip_bind()`, etc.
- **Execution modes** (`auto`/`split`/`reduce`/`plain`): Native support for map-reduce style workflows where steps can split output into named partitions and downstream steps auto-map over them.
- **Backward compatibility**: Legacy `pipe_*` functions and `Pipeline` R6 class preserved as deprecated aliases (`R/aliases.R`, `R/pipelineR6.R`).
- `pip_view()` — create filtered pipeline views (by dependencies, tags)
- `pip_tag()` / `pip_untag()`
- `[[` extract operator for pipelines
- Comprehensive revision of all vignettes to match new API
- MIT license, updated CI workflows, dependabot config, kilo.json, lintr/styler config
- Extensive test suite (~10K+ lines) covering C++ DAG, pip_* API, aliases, execution modes, and recursive runs


# pipeflow 0.2.3.9005

* deprecate legacy `pipe_*` API in favor of the new `pip_*` API
* deprecate legacy `Pipeline` R6 interface (`Pipeline$new()`)


# pipeflow 0.2.3.9005

* refactor: deprecate `keepOut` field and introduce `tags` field

# pipeflow 0.2.3.9004

* deprecate `all` parameter in `pipe_collect_out()`
* deprecate `pipe_set_data_split()` and `pipe_set_keep_out()`
* add new functions to skip and unskip pipeline steps and groups: `pipe_skip_step()`, `pipe_unskip_step()`, `pipe_skip_group()`, and `pipe_unskip_group()`.
* add new function, `pipe_get_step_field()` to retrieve a specific field of a pipeline step
* remove advanced vignette on using pipeflow with split data sets


# pipeflow 0.2.3.9003

* Refactor: Remove Param object handling from pipeline and utility functions
* Refactor: Remove unused get_params_unique_json method and its documentation


# pipeflow 0.2.3.9002

* refactor: ensure unit tests for logging is always done with logging enabled
* refactor:  add 'locked' column and improve indexing performance
* refactor: implement slightly faster step existence check
* feat: set locked status via boolean flag and add has_step_locked function


# pipeflow 0.2.3.9001

* feat: implement index map and BFS for dependency graphs (improved performance)
* chore: add benchmark for running long linear pipelines


# pipeflow 0.2.3.9000

- Same as previous version.


# pipeflow 0.2.3

* Add Depends R >= 4.2.0 to DESCRIPTION
* Fix issue caused by update of lgr package (#571e8260)
* Fix badge links in README

# pipeflow 0.2.2

* Add News and this Changelog
* Add unit tests and detailed documentation for [alias functions](https://rpahl.github.io/pipeflow/reference/index.html#alias-functions) (#24)
* Link to other packges via [my R universe](https://rpahl.r-universe.dev/packages) (#25)

# pipeflow 0.2.1

* CRAN release
