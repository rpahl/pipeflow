# pipeflow — plan: a pipeline as a step function (`as.function()`)

Starting point: `main` at 5d68ca5 (0.5.0.9001). Branch `feat-as-function`,
worktree `as-function`. A working prototype (46 tests, exported API of
0.5.0 and base R only) is in `dev/prototype/` (scratch folder, see the
end). Line references are at 5d68ca5: `pf` = `R/pipeflow.R`.

## Idea

`as.function()` for a pipeline returns a function whose formals are the
unbound params of the pipeline (current values as defaults) and whose
result is the output of selected steps. It is a function like any other,
so `pip_add()` takes it, and everything pipeflow already does with
functions applies:

```r
inner <- pip_new("inner") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add("fit", \(data = ~data, degree = 1) lm(y ~ poly(x, degree), data)) |>
    pip_add("coefs", \(fit = ~fit, digits = 2) round(coef(fit), digits))

outer <- pip_new("outer") |>
    pip_add("data", \(data = my_data) data) |>
    pip_add("split_data", \(data = ~data, byVar = "batch") {
        split(data, data[[byVar]])
    }, exec = "split") |>
    pip_add("by_batch", as.function(
        inner, out = "coefs", bind = list(data = ~split_data)
    )) |>
    pip_add("table", \(coefs = ~by_batch) do.call(rbind, coefs),
            exec = "reduce")
```

- The inner params (`degree`, `digits`) are unbound params of the outer
  step `by_batch`: `pip_get_params(outer)` shows them, `pip_set_params()`
  sets them, a change outdates the step. This replaces the registration
  `params = pip_get_params(inner)` of the pattern in vignette v05b.
- The step is mapped over partitions by the existing exec modes (the
  function is called once per key); the output is partitioned, so
  downstream steps map per key too and `reduce` combines. This replaces the
  hand-written `Map()` loop and the `lapply()` over the returned pipelines.
- Per-key error context (`pipeflow_key_error`) comes for free; the open
  per-key failures work (F7) benefits directly.
- Annotated values: the defaults of the formals are the stored values of
  the inner pipeline (objects with a `pip_param_value()` method included);
  the outer run unwraps them before the call (0.5.0), so the inner pipeline
  receives plain values.

## Reuse of the inner pipelines (the point for interactive use)

A fresh clone per call reruns every inner step for every partition each
time the outer step is outdated. Keeping the run pipelines lets a changed
param rerun only the inner steps downstream of it, per partition. In the
prototype (3 partitions, a 0.1 s inner `fit`):

| change | no reuse | reuse |
|---|---|---|
| first run | 0.35 s | 0.32 s |
| `digits` (after the fit) | 0.32 s | 0.01 s |
| `degree` (before the fit) | 0.32 s | 0.32 s |
| data of one partition | 0.32 s | 0.11 s (only that partition) |

**Design.**
- The function keeps up to `cache` (default 16) run inner pipelines, least
  recently used dropped; `cache = 0` keeps none.
- A call is matched to a kept pipeline by its **bound inputs**: the
  formals bound to outer steps with `bind` (the partition slice).
  Identical inputs (`identical()`) mean the same pipeline. This needs no
  partition key: `.pip_execute_step_call()` (pf:403) calls the function
  with the per-key arguments only.
- **Correct whatever the match.** Every call sets all params and inputs
  with `pip_set_params()`, which skips values identical to the stored ones
  (0.5.0), and `pip_run()` reruns exactly the outdated steps. A "wrong"
  match costs speed, never correctness (pinned by a test).
- Why not the params as match key: a changed param is the common case and
  must hit; why not "most identical arguments": with two changed inputs
  the scores tie and the kept pipelines thrash.
- Limit: with `cache` smaller than the number of partitions, visited in
  the same order every time, least-recently-used eviction never hits. The
  size must cover the number of partitions; kept pipelines hold their
  inputs and outputs, so memory grows with it. Document it.

**Where the kept pipelines live.** Not in the closure: serialising the
function (`saveRDS()`, futures, F4) would copy them. The function holds an
id; the pipelines live in a registry (environment in the package) keyed by
that id; a copy in another process starts cold. A finalizer on the
function's environment removes the entry when the function is collected
(it must be created by a helper that only knows the id, otherwise the
finalizer keeps the environment alive). Pinned by tests: serialised size
does not change with the cache, round trip works cold, entry removed after
`gc()`. An Rcpp external pointer that owns the cache (like `.dag`) is the
alternative; decide in the implementation.

Consequences to document:
- A copy of the function in a worker process starts cold, so reuse helps
  runs in one process (an interactive session), not one-shot workers.
- Clones of the outer pipeline share the function, hence the cache. That is
  safe (same argument as above) and saves work.

## API

```r
as.function(x, out = NULL, bind = list(), cache = 16L, ...)  # x: pipeline
```

- `x`: a pipeline, not a view (`p[i, view = FALSE]` makes one). A snapshot
  is taken (`pip_clone()`): later changes of `x` do not change the
  function.
- `out`: `NULL` gives `pip_collect()` of the run pipeline (a flat named list
  of all outputs, by step); a step name gives that output; several names a
  named list.
- `bind`: named list of one-sided formulas, `list(data = ~split_data)`: the
  formal gets the default `~split_data` (an ordinary step reference, so the
  outer step depends on it) and becomes part of the match key. Names that
  are not unbound params of `x` are an error. Why an argument: the function
  cannot tell from its arguments which ones are step outputs. A bound
  formula that is not supplied in a direct call is an error (the default
  `~split_data` is no value).
- The inner pipeline runs with `lgr = NULL` and the default `on_error`;
  warnings and messages of inner steps reach the outer run (they are not
  muffled; pinned by a test). A failing inner step is an error of the outer
  step: `step 'by_batch': key 'D': step 'fit': ...`.

**Decided** (2026-10-17): the `as.function()` method (not a `pip_as_fun()`),
kept pipelines in an R registry (not an external pointer), `bind` as an
explicit argument. It was called `connect` for a while and is
`bind` again: the docs contrast bound and unbound params.

**Open / later.**
1. `pip_add(fun = <pipeline>)` as sugar later (`params` would then play the
   role of `bind`, and pipeflow knows which params are references, so no
   `bind` is needed there). Not in the first version.
2. **Do not forget:** whether the inner run should inherit `on_error` from
   the outer run, so that a run with `on_error = "continue"` gives failure
   objects for the failed inner steps. Not in the first version; track it
   as an issue when the PR is opened.
3. `bind` as a convenience of `rbind.pipeflow()`: binding two
   pipelines by pointing an unbound param of the second at a step of the
   first (`rbind(a, b, bind = list(data = "prep"))`). Separate PR; the
   same one-sided-formula rewrite as here, applied to a step of `b`.

## Dependencies on the other plans

`dev/PLAN-extension-hooks.md` (branch `docs-plan-hooks`: WP1 `.self`
release, WP2 output hook, WP3 setter hook): **none of them is needed**, and
the file layout avoids conflicts (new `R/as_function.R`, new test file; only
`NAMESPACE`, NEWS and `_pkgdown.yml` overlap). Interplay:
- WP3 (`` `pip_param_value<-` ``): without it a plain value set into the
  kept pipeline replaces an annotated object stored there (the stored
  default in the clone becomes plain). Nothing breaks: the step functions
  see plain values either way, and the annotated objects stay in the outer
  step's params, which is what a UI reads. With WP3 the kept pipelines also
  keep their annotations. No change to `as.function()` is needed.
- WP2 (output hook): the hook also runs for the steps of the inner
  pipeline; the output of the outer step (the extracted values) passes it
  again. A method that leaves already annotated values alone (e.g. a
  stamp that is set once) is idempotent. Nothing to do here.
- WP1 (`.self` release): inner steps that use `.self` see their own
  (cloned) inner pipeline; with WP1 their outputs no longer keep it alive.
  Independent.

## Status (2026-10-17)

Implemented in this worktree, not committed:
- `R/as_function.R` (`as.function.pipeflow()`, registry helpers named
  `.fun_*`), `S3method(as.function, pipeflow)`, `man/as.function.pipeflow.Rd`,
  `_pkgdown.yml` entry;
- `tests/testthat/test_as_function.R` (89 expectations; fixtures inside the
  `describe()` block, because closures from a helper file have the attached
  package environment of `load_all()` as ancestor, which `serialize()` warns
  about);
- vignette v05b rewritten around `as.function()` (the manual clone/run
  pattern stays as "Full control"), NEWS bullet;
- lint clean, `devtools::test()` all green, `rcmdcheck` 0/0/0.

Changes against the prototype:
- `pip_collect()` instead of `pip_collect(simplify = FALSE)` for
  `out = NULL` (the prototype returned each output nested one level deep);
- the template is reset (`pip_reset()`), so the result does not depend on
  whether `x` was run before; locked steps keep their outputs;
- a bound formal that is not supplied in a direct call is an error;
- the inner pipeline is kept before it is run, so after a failed inner step
  the steps that did run are reused;
- input checks for `out`, `bind` and `cache`.

Measured (3 ms per partition and call, 200 partitions, 10 or 10 000 rows):
the per-call overhead of `pip_clone()`/`pip_set_params()`/`pip_run()` is
about 3 ms, the `identical()` match is not visible even for 10 000-row
inputs, and equal-but-new inputs hit like pointer-equal ones. Reuse pays
when the inner steps cost more than that (documented). No `bench/` file was
added (the existing one is a rendered report of the pipeline run).

Still to do before the PR: rebase on `main` (other branches touch NEWS and
`NAMESPACE`), bump the version as the last commit, open an issue for the
`on_error` inheritance and one for `bind` in `rbind()`.

## Work (done, see Status)

## Not in scope

Namespaced params (names stay global; an inner `digits` is the outer
`digits`), per-key failures (F7), state reuse across processes,
`pip_add(fun = <pipeline>)` sugar, inheriting `on_error`, `bind` in
`rbind()`.

## Prototype

The prototype is superseded by `R/as_function.R`; it still runs and is kept
as a scratch reference. `dev/prototype/` (scratch, git-ignored like the rest
of `dev/`; add with `git add -f` if it should be kept):
- `as-function.R`: the implementation (about 130 lines) and the demo
  fixture; the S3 method is defined in the global environment there;
- `example-as-function.R`: the timed walkthrough above;
- `test-as-function.R`: 46 expectations.

Run (pipeflow 0.5.0 or this branch attached):

```sh
Rscript -e "library(pipeflow); source('dev/prototype/example-as-function.R')"
Rscript -e "library(pipeflow); library(testthat); \
            test_file('dev/prototype/test-as-function.R')"
```
