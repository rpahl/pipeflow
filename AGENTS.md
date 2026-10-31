# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

`pipeflow` is an R package (CRAN) for building interactive data analysis pipelines.
Users add R functions as steps with `pip_add()`, and the dependencies between steps live
in a C++ DAG (Rcpp).

This is the single source of agent instructions. `CLAUDE.md` (for Claude Code) and
`.github/copilot-instructions.md` only point here, so add new instructions to this file.
Keep it to what an agent can't learn from the code or `dev/`: don't add overviews here.

The architecture (data model, call graphs, run flow, views, DAG) is in `dev/pipeflow.md`.
Pipelines as functions are in `dev/as_function.md`. Read the relevant section before you
change a flow, and update it when you change a flow it describes. "Where to change what"
in `dev/pipeflow.md` maps tasks to files.

## Environment

- Dependencies are pinned with `renv` (`renv.lock`, which requires R 4.5.3). `renv/` and
  `.Rprofile` are gitignored, so a fresh worktree may not have them. The R on PATH may
  be a different version. Check `R --version` before running tests or checks, and don't
  run them on a mismatched R. Several versions are installed under `/opt/R/<version>/`
  (managed by `rig`): if the default isn't 4.5.3, run with `/opt/R/4.5.3/bin/Rscript`
  and put `/opt/R/4.5.3/bin` first on `PATH`.
- Run R from the repo root. If `.Rprofile` exists, call `source(".Rprofile")` first so
  the renv library is active. If packages are missing, run
  `renv::restore(prompt = FALSE)`. Don't install into global or system libraries.

## Commands

```sh
Rscript -e "lintr::lint_package()"                # lint (CI fails on any lint)
Rscript -e "devtools::test()"                     # all tests
Rscript -e "devtools::test(filter = 'pipeflow')"  # tests/testthat/test_pipeflow.R
Rscript -e "devtools::test_active_file('tests/testthat/test_utils.R')"
Rscript -e "devtools::document()"                 # man/ and NAMESPACE from roxygen
Rscript -e "Rcpp::compileAttributes()"            # after changing [[Rcpp::export]]
Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual', error_on='warning')"
Rscript -e "devtools::build_readme()"             # README.md from README.Rmd
```

Cheat sheet (`cheatsheet/pipeflow-cheatsheet.typ`, exactly 2 pages). Run from the repo
root. `typst` is a snap that is not on PATH in the VS Code terminal, so use the full
path:

```sh
/snap/bin/typst compile --root . cheatsheet/pipeflow-cheatsheet.typ \
    cheatsheet/pipeflow-cheatsheet.pdf
# also checks the page count and fonts, and rebuilds man/figures/cheatsheet-*.png
cheatsheet/build.sh
```

Before opening a PR, run the full check in this order: lint, then tests, then
`rcmdcheck`. The target is 0 errors and 0 warnings, with 0 notes if possible. CI
(`.github/workflows/ci.yaml`) runs lint, then R CMD check on Ubuntu, Windows and macOS,
then covr. If the same failure is still there after 3 fix attempts (or after 6 attempts
in total), stop and summarize instead of weakening tests or masking errors. If CI is
still not green after about 45 minutes of work on an issue, commit the latest changes,
summarize the attempts and blockers, and don't open a PR.

## Rules that are easy to break

The full list of invariants, with the functions that keep them, is in "Invariants" in
`dev/pipeflow.md`. `tests/testthat/helper_invariants.R` checks them.

- **State is shared by reference.** A `pipeflow` is `list(name, view, pipenv)`, and all
  state lives in the `pipenv` environment. Edit it in place (`data.table::set`, `:=`)
  and return `x` invisibly. Only `pip_clone()`, `p[]` on a pipeline and
  `p[i, view = FALSE]` make real copies (including the DAG, via `dag_clone()`).
- **Row order is a topological order.** A step may only refer to steps in earlier rows;
  `pip_run()` just loops over the rows.
- **Three step maps must agree:** the `nodeId` column, `.steps_to_nodes` and the C++
  DAG. Anything that adds, removes or renames a step updates all three. Node ids are
  never reused, because views store node ids, not row numbers.
- **Derived columns** (`depends`, `unbound`, `nodeId`, see `.derived_cols`) are
  recomputed from `fun` and `params`, never assigned.
- **A one-sided formula in `params` is a step reference** (`~step1`, `~-1`). A
  `~try(step1)` wrapper marks an argument that may receive a failed input; rename,
  `rbind()` and `[<-` copying rebuild reference formulas and must keep the wrapper.
  Only the function made by `as.function()` sets formula arguments as values
  (`.pip_set_params(bind = FALSE)`).
- **Views** share `pipenv` and store their selector in the view object, so a view can
  shrink by reference but not grow. `pip_add()`, `pip_clone()` and `rbind()` reject
  views; `pip_remove()`, `pip_rename()` and `pip_replace()` edit through a view and gate
  the named step with `.pip_step_in_view()`.
- **Steps can change their own pipeline** (via `.self`). After each step,
  `.pip_run_row()` re-reads `pipenv$data` by step name, so never hold row indices
  across a step call.
- **A new `pipenv` field that edits change** must be added to `.pip_snapshot()` and
  `.pip_restore()`, so a failed `[<-` changes nothing.
- Don't edit the generated `R/RcppExports.R` and `src/RcppExports.cpp`; run
  `Rcpp::compileAttributes()`. The DAG is tested directly in `test_RcppExports.R`.

## Conventions

- Use the native pipe `|>`, a 4-space indent and lines of at most 88 characters
  (`.lintr`, `air.toml`). Local variables are camelCase. Internal helpers start with
  `.`, and exported functions start with `pip_`.
- Keep the API lean: one way to write each operation. Docs and examples use the `pip_*`
  functions. `$` methods are for steps that change their own pipeline
  (`.self$replace()`, `.self$restart()`), and `[[<-` is for setting one cell or a column
  that has no `pip_*` setter. Don't add convenience wrappers that only save a line.
- Every function has a roxygen header. Unexported functions end the header with `@noRd`.
  `NAMESPACE` and `man/` are generated by roxygen, so don't edit them by hand.
- Where code goes: the exported `pip_*` API is in `R/pipeflow.R` (except `pip_graph()`
  in `R/graph.R`), with internal `.pip_*` helpers at the top; S3 methods are in
  `R/generic.pipeflow.R`. The larger files are split into sections: a full-width
  `# ====` banner opens a block, a short `# ---` rule a section within it. Put a new
  function in the section it belongs to, and a helper in the file that uses it (a
  test-only helper goes into `tests/testthat/helper_*.R`). The test files follow the
  sections of their source file in the same order. See "Sections" in `dev/pipeflow.md`.
- Add tests together with new logic. Most source files `R/<x>.R` have tests in
  `tests/testthat/test_<x>.R` (testthat edition 3, run in parallel). Use `mockery` for
  external dependencies. `test_edit_chains.R` runs long chains of edits and checks the
  invariants after each edit. After changing how edits or runs work, run it with more
  random chains, e.g. `PIPEFLOW_RANDOM_CHAINS=100`.
- Vignettes (`vignettes/v0*.Rmd`, plus `vignettes/articles/` for pkgdown-only articles)
  start like `vignettes/v01-get-started.Rmd`: the same YAML header (with their own
  `title`, `description` and `VignetteIndexEntry`) and the same `knitr-setup` chunk,
  whose `options(width = 100L)` is undone by `options(old)` at the end. Use descriptive
  chunk labels, not auto-numbering. To render a vignette outside of
  `pkgdown::build_site()`, call `devtools::load_all(quiet = TRUE)` first.
- Commit messages use conventional commits: `feat:`, `fix:`, `test:`, `refactor:`,
  `docs:`. Note user-facing changes in `NEWS.md`.
- Versions are bumped by hand, no CI job does it. As the last commit of a PR, rebase on
  `main` first (so the version doesn't clash with other PRs), then increment the dev
  version in `DESCRIPTION` (`0.4.0.9014` -> `0.4.0.9015`), make sure the top heading of
  `NEWS.md` is `# pipeflow <new version>` with the PR's bullets under it, and commit as
  `chore: bump version to <version>`.
