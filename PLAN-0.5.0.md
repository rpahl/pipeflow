# pipeflow 0.5.0 — plan

Starting point: `main` at 0.4.0.9004 (3457d4c). Since 0.4.0 (CRAN):
- steps that never ran stay "new", and runs outdate only what is
  downstream of executed steps (#84);
- `pip_run(on_error = "continue")` runs on after failed steps (#84);
- `~try(step)` references receive failure objects (#85; this replaced
  0.4.0.9002's `allow_failed`).

This plan covers the rest of 0.5.0: bug fixes, clearer errors, parameter
handling, one extension point (`pip_arg_value()`), `.self` only where it
is used, and the CRAN release.

Line references are at 3319065 (the code is the same at 3457d4c): `pf` =
`R/pipeflow.R`, `gen` = `R/generic.pipeflow.R`, `print` =
`R/print.pipeflow.R`.

## Conventions

- One PR per work package (WP), on branch `<type>-<topic>` in its own
  worktree. Before the PR run lint -> tests -> `rcmdcheck` (CLAUDE.md).
- Each PR adds a NEWS bullet under the development heading and its tests.
  The version bump ("Bump version to 0.4.0.900x") follows the merge, as
  for 0.4.0.9002/9003.
- Behaviour changes are marked in NEWS, so that WP8 can collect them
  into a "Breaking changes" block.
- Design-level items (WP5, WP6) start with a short proposal in the PR
  description; implement once it is agreed.

## WP1 — bug fixes (`fix-bugs`)

**B1. `x[[step, j]]` on a view reads the wrong row.** `.pip_subset2()`
(gen:252-266) takes the step's absolute row from `.pip_steps_to_rows()`
and indexes the view's data with it.

```r
p <- pip_new("p") |>
    pip_add("a", \(x = 1) x) |> pip_add("b", \(y = 2) y) |>
    pip_add("c", \(z = 3) z)
v <- pip_view(p, step = c("b", "c"))
v[["b", "params"]]   # list(z = 3): the params of 'c'
v[["c", "params"]]   # Error: selected step not part of view: c
```

Fix: `data.table::chmatch(i, data[["step"]])` on the view's data; error
if NA. Test with views that don't start at row 1 (the existing test uses
rows 1:2).

**B2. Clones share `.self` with the original.** `pip_clone()`
(pf:1273-1296) copies the step table, but the step functions keep their
closure environment, which holds `.self`. After a clone runs, the
original's steps see the clone as `.self`.

```r
r <- pip_new("self") |> pip_add("s", \(x = 1) {
    before <- .self[["name"]]
    if (x == 1) {
        cl <- pip_clone(.self, name = "inner-clone")
        pip_set_params(cl, list(x = 2)) |> pip_run(lgr = NULL)
    }
    c(before = before, after = .self[["name"]])
})
pip_run(r, lgr = NULL)
r[["s", "out"]]   # before = "self", after = "inner-clone"
```

Fix: re-wrap the `fun` column with `.wrap_self(fun, out)`, as
`.pip_bind()` does (pf:945). `.pip_bind()` clones before wrapping, so its
steps then get one more environment layer (harmless).

**B3. Names held only by locked steps are reported as unknown.**
`pip_set_params()` decides unknown names from the unlocked rows only
(pf:2631-2647), and the early return at pf:2633 happens before the check.
Setting `x` when only a locked step has it warns "Trying to set
parameters not defined in the target: x". Fix: decide on all rows of the
view, and set only the unlocked ones.

**B5. Messages, dead code, docs.**
- `pip_add()` with a `.self` formal says "cannot be used as a step name"
  (pf:1178). It should say "parameter name", as `pip_replace()` does.
- A primitive `fun` (`pip_add(p, "s", sum)`) fails inside `.wrap_self()`
  with "use of NULL environment is defunct". Check `is.primitive(fun)` in
  `pip_add()` and `pip_replace()` and say "wrap it, e.g. `\(x = 1)
  sum(x)`".
- `pip_replace()` pf:2042-2050 can never run: the dependency check just
  before it already stops. It also says "while adding". Delete it.
- NEWS 0.4.0 mentions `pip_derived_cols()` (doesn't exist) and step
  removal via `p[[step]] <- NULL` (errors; the working form is
  `p[[step, "step"]] <- NULL`). Correct the text.
- `?pip_view` (pf:2849) says it returns a `pipeflow_view` object; the
  class is `"pipeflow"`.

**B7. A manual halt is overwritten.** After the `break` on "halted"
(pf:2472), `pip_run()` sets `.run_state` to "ready" or "continued"
(pf:2479-2480), so "halted" is never seen. Fix: keep "halted".

## WP2 — step name and original condition in errors (`feat-step-errors`)

Today `.pip_step_error()` (pf:249-259) builds a `pipeflow_step_error` with
`step` and `parent`. On `on_error = "stop"`, `pip_run()`'s outer handler
(pf:2483-2487) re-raises only the message (`stop_no_call(e$message)`):
"Error: object 'x' not found" after a long run says nothing about the
step, and callers can't catch the step's own error class.

- Message: `"step '<step>': <conditionMessage(parent)>"`.
- The outer handler re-signals a `pipeflow_step_error` as it is
  (`stop(e)`), so `tryCatch(pip_run(p), pipeflow_step_error = \(e)
  e$parent)` works. Other errors keep today's path.
- Partitions (pf:397-402): raise a classed condition with fields `key`
  and `parent`; the message keeps "key '<k>': ".
- Failure objects and the `pipeflow_run_failed` warning (pf:275-337)
  build their messages from the original condition, so the step name
  can't appear twice ("step 'x' failed: step 'x': ...").
- Tests: the existing message regexes ("boom", "key 'b': boom") still
  match; add class, `step`, `key` and `parent` assertions for stop and
  continue, and for an rlang error with its own class.
- NEWS: error messages from `pip_run()` start with the step name.

## WP3 — parameter handling (`feat-set-params`)

**F1. Identical values don't outdate.** `pip_set_params()` outdates every
step a value is set on (loop pf:2655-2668), even if the value is
identical. Apps that push all inputs on every change re-run unchanged
steps. Skip (row, name) pairs whose value is `identical()` to the stored
one; outdate only the changed rows. This is the new default (NEWS);
`pip_run(force = TRUE)` still forces a run.

**F3. Control over unknown names.** `pip_set_params(x, params, unknown
= c("warn", "ignore", "error"))` replaces the fixed warning (pf:2645-2653).
The nested-pipeline pattern of vignette v05b triggers the warning by
design; `suppressWarnings()` also hides real warnings. Do it together
with B3.

**Q1. Warn when `params` overlap formals with defaults.**
`pip_add(p, "s", \(x = 99, ...) x, params = list(x = 1))` stores 99:
defaults of formals win silently (pf:753, 811, 2014). This silently
breaks the nested-pipeline pattern when a forwarded param is also a
formal. `pip_add()` and `pip_replace()` warn and name the overlapping
names.

## WP4 — failures reach reduce steps (`fix-reduce-failures`)

With `on_error = "continue"`, a failing partition fails the whole split
step, and steps downstream get one (not partitioned) failure object. A
`reduce` step that takes it by `~try()` then fails itself with "reduce
mode requires at least one partitioned input" (pf:363-365). That error
names the mode, not the cause.

- Fix: in reduce mode an argument holding a `pipeflow_failure` counts as
  a partitioned input, so the reduce step runs once and receives the
  failure.
- Tests: split -> per-key step fails -> a reduce step with `~try()` gets
  the failure; a reduce step without `try()` is skipped as usual.
- Not in 0.5.0: per-key failures (see Later).

## WP5 — `pip_arg_value()` (`feat-arg-value`)

Some packages store annotated values as step params (values with units
or labels, parameter objects that carry UI metadata). They must unwrap
them before the step function sees them. Today that means wrapping every
step function: copying formals, unwrapping arguments, forwarding `.self`.
An S3 hook makes that one method.

- Exported generic `pip_arg_value(x, ...)`; the default method returns
  `x`.
- One call site in `.pip_run_row()` (pf:410ff): applied to the
  **unbound** arguments (`dat$unbound[[i]]`, `...` names included). It is
  not applied to step outputs (`~step` inputs), partitions or failure
  objects.
- It runs inside the `withCallingHandlers` (pf:438), so an error in a
  method is a step error and `on_error = "continue"` applies to it.
- Docs: `?pip_arg_value` with an example (a value with a unit class), and
  a short section in the parameters vignette. Methods are meant for the
  package's or user's own classes, not base classes.
- Tests: called once per unbound argument in every exec mode (auto,
  split, reduce); never for `~step` inputs; an error in a method fails
  the step; the default method changes nothing (existing tests).
- Cost: one `lapply()` per step call, plus an API to keep stable.

## WP6 — `.self` only where it is used (`feat-self-opt-in`, proposal)

`.self` is injected into every step function (`.wrap_self()`, pf:134;
reset each run at pf:436). An output that keeps the step's frame alive
(a ggplot through its `plot_env`, a closure) then references the whole
pipeline. `saveRDS()` of such an output writes the pipeline with all its
outputs (~900 KB for a toy plot), and sending the output to another
process takes the pipeline along.

Proposal:
- `.wrap_self()` wraps only if `".self" %in% all.names(body(fun))`. No
  new argument or column. Document that `get(".self")` is not detected.
- The per-run assignment at pf:436 writes only into an environment that
  pipeflow created for `.self`, never into the function's own closure
  environment (often the global environment).
- Call sites: `.pip_append` (pf:757), `.pip_insert` (pf:815),
  `.pip_bind` (pf:945), `pip_replace` (pf:2018), `pip_clone` (WP1).
- Tests: the `saveRDS()` size of a closure output from a step without
  `.self`; `restart`/`halt` and the vignette examples (v05b, v06) still
  work.
- NEWS: `.self` is available in step functions that refer to it.
- Alternative if detection is too implicit: an argument `self = NULL`
  (NULL = detect, TRUE/FALSE = force), stored as a column. Decide in
  the PR.

## WP7 — small API and print items (`feat-run-state`)

- `pip_run_state(x)`: exported accessor for "ready", "running", "halted",
  "failed", "continued", next to `pip_reset()`. It needs B7.
- **View names:** a view is named `"<name> view"` (gen:456, pf:2941),
  so a view of a view is "p view view" and code holding a view can't get
  the pipeline name. Keep the pipeline name; `print()` already shows
  `<pipeflow_view>`.
- **Conditions in `print()`:** with `on_error = "continue"` a failed
  step's output is its condition, and `print()` puts the whole backtrace
  of an rlang error into the `out` column. Show conditions as
  `<error/class>` (print:68-81, as already done for params).
- Docs:
  - `pip_get_params()` includes locked rows but `pip_set_params()` skips
    them.
  - Views store row numbers, so they go stale when steps are added or
    removed through another handle.
  - `tags %like% "x"` in `[` also matches untagged steps; use
    `pip_view(tags = )`.

## WP8 — release 0.5.0

- Merge the dependabot PRs (#72, #81, #82).
- `devtools::build_readme()`; update the vignettes for `~try()`, F1, F3,
  `pip_arg_value()` and `.self`.
- NEWS: one `# pipeflow 0.5.0` section with a "Breaking changes" block:
  - `~try()` references (`allow_failed` was dev-only);
  - identical values no longer outdate;
  - step names in error messages;
  - view names;
  - `.self` only in step functions that refer to it.
- Checks: lint, tests, `rcmdcheck` on R 4.5.3 and on the oldest supported
  R (4.2); CI on Linux, Windows and macOS; win-builder (release, devel);
  `cran-comments.md`; reverse-dependency check.
- Submit; tag `v0.5.0` after acceptance.

## Order

WP1 -> WP2 -> WP3 -> WP4 -> WP5 -> WP6 -> WP7 -> WP8.
- WP2 comes before WP5: both change `.pip_run_row()`.
- WP6 comes after WP5: with `pip_arg_value()`, wrappers that only
  unwrap values disappear, so fewer step functions mention `.self`.
- WP1, WP3 and WP7 are independent and can swap places.

## Later (0.6+)

- **Per-key failures in split steps:** keep the outputs of the keys that
  succeeded and pass failure objects only for the failed keys. This
  changes `pipeflow_run_failed$failed`. Size L, about 150-250 lines: the
  per-key loop in `.pip_execute_step_call()`, the skip logic in
  `pip_run()` (pf:2371-2392), and `.pip_failure()` with a `key` field.
- **Pipelines that survive `saveRDS()`:** after `readRDS()` the DAG
  pointer is null ("null Dag pointer"). Rebuild it on demand from
  `depends` behind one `.pip_dag()` accessor used by all entry points
  (`pip_add`, `pip_run`, `pip_set_params`, `pip_remove`, `pip_replace`,
  `pip_clone`, `rbind`, `[`). Size M; needs a `saveRDS()`/`readRDS()`
  test per entry point.
- `pip_replace()` keeps tags and exec by default (`tags = NULL`, `exec =
  NULL`); changes a default.
- Export the default logger.
