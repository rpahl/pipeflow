## Release summary

This is a minor release (0.4.0 -> 0.5.0). It adds `pip_run(on_error =
"continue")`, `pip_param_value()`, `pip_run_state()`, the exported logger
`pipeflow_lgr()` and the `unknown` and `force` arguments of
`pip_set_params()`, lets pipelines survive `saveRDS()`, and fixes several
bugs. It also changes some behaviour (see "Breaking changes" in NEWS.md);
the package has no reverse dependencies (see below).

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
