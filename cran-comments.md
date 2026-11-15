## Release summary

This is a minor release (0.5.0 -> 0.6.0). It adds pipelines as functions
(`as.function()`, `pip_inner()`), `pip_add()` of pipelines, `pip_clone(out =
)`, `pip_output_value()`, a `map` argument of `pip_run()` for parallel runs,
mermaid output of `pip_graph()` and more, and fixes several bugs. It also
changes some behaviour and removes two deprecated functions (see "Breaking
changes" in NEWS.md); the package has no reverse dependencies (see below).

## Test environments

* local Ubuntu 24.04 (WSL2), R 4.5.3
* GitHub Actions (R CMD check): ubuntu-latest, windows-latest, macos-latest,
  R 4.5.3
* GitHub Actions: ubuntu-latest, R 4.2 (oldest supported release)
* win-builder (Windows): R 4.6.1 (release), R 4.5.3 (oldrelease) and R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

This is also the result on win-builder for all three R versions above.

## Reverse dependencies

There are no reverse dependencies.
