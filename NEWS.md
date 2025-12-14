<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

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


# pipeflow (development version)

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
