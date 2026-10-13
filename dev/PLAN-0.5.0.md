# pipeflow 0.5.0 — plan

Starting point: `main` at 0.4.0.9004 (3457d4c). Since 0.4.0 (CRAN):
- steps that never ran stay "new", and runs outdate only what is
  downstream of executed steps (#84);
- `pip_run(on_error = "continue")` runs on after failed steps (#84);
- `~try(step)` references receive failure objects (#85; this replaced
  0.4.0.9002's `allow_failed`).

This plan covers the rest of 0.5.0: bug fixes, clearer errors, parameter
handling, one extension point (`pip_param_value()`), `.self` only where it
is used, pipelines that survive `saveRDS()`, `pip_replace()` defaults, the
exported logger, and the CRAN release. Only per-key failures in split
steps are left for later (see Later).

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

## WP5 — `pip_param_value()` (`feat-arg-value`)

Some packages store annotated values as step params (values with units
or labels, parameter objects that carry UI metadata). They must unwrap
them before the step function sees them. Today that means wrapping every
step function: copying formals, unwrapping arguments, forwarding `.self`.
An S3 hook makes that one method.

- Exported generic `pip_param_value(x, ...)`; the default method returns
  `x`.
- One call site in `.pip_run_row()` (pf:410ff): applied to the
  **unbound** arguments (`dat$unbound[[i]]`, `...` names included). It is
  not applied to step outputs (`~step` inputs), partitions or failure
  objects.
- It runs inside the `withCallingHandlers` (pf:438), so an error in a
  method is a step error and `on_error = "continue"` applies to it.
- Docs: `?pip_param_value` with an example (an annotated value of a custom
  class). The vignette follows later (see the end of this plan). Methods
  are meant for the package's or user's own classes, not base classes.
- Tests: called once per unbound argument in every exec mode (auto,
  split, reduce); never for `~step` inputs; an error in a method fails
  the step; the default method changes nothing (existing tests).
- Cost: one `lapply()` per step call, plus an API to keep stable.

## WP6 — `.self` only where it is used (`feat-self-opt-in`)

`.self` is injected into every step function (`.wrap_self()`, pf:134;
reset each run at pf:436). An output that keeps the step's frame alive
(a ggplot through its `plot_env`, a closure) then references the whole
pipeline. `saveRDS()` of such an output writes the pipeline with all its
outputs (~900 KB for a toy plot), and sending the output to another
process takes the pipeline along.

Agreed (2026-10-10):
- `.self` is provided per step call. `.pip_run_row()` calls the step
  function through a copy whose environment is a new child of the
  function's environment holding `.self`, and only if `".self" %in%
  all.names(body(fun))` (also covers functions defined in the body). No
  new argument or column. `get(".self")` is not detected (documented in
  `?pip_add`); a wrapper opts in by mentioning `.self` in its body.
- Step functions are stored as given: `.pip_rewrap_self()` and the
  add-time `.wrap_self()` calls (`.pip_append`, `.pip_insert`,
  `.pip_bind`, `pip_replace`, `pip_clone`, `.pip_compact`) go. Nothing is
  written into the function's own environment (often the global
  environment), and copies need no re-wrapping (B2).
- Tests: the serialized size of a closure output from a step without
  `.self`; `.self` is not visible to steps that don't refer to it; stored
  functions are identical to the given ones; `restart`/`halt` and the
  vignette examples (v05b, v06) still work.
- NEWS: `.self` is available in step functions that refer to it.
- Rejected: an argument `self = NULL` (NULL = detect, TRUE/FALSE =
  force), stored as a column; and wrapping at add time with a marker on
  pipeflow's environment.

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
    removed through another handle. Fixed in WP7e instead of documented.
  - `tags %like% "x"` in `[` also matches untagged steps; use
    `pip_view(tags = )`.

## WP7a — pipelines that survive `saveRDS()` (`feat-serialize`)

After `readRDS()` the DAG pointer is null ("null Dag pointer"), so a
saved pipeline can't be used. This blocks caching pipelines, sending them
to other processes (future, mirai, callr) and Shiny bookmarks. With WP6,
outputs no longer drag the pipeline along, so this is the other half.

- Rebuild the DAG on demand from `depends` behind one `.pip_dag()`
  accessor, and use it in all entry points: `pip_add`, `pip_run`,
  `pip_set_params`, `pip_remove`, `pip_replace`, `pip_clone`, `rbind`,
  `[`. `.steps_to_nodes` is a plain environment and survives, but check
  that it matches the rebuilt node ids (`nodeId` column). Views store node
  ids (WP7e), so the rebuilt DAG must keep the ids of the `nodeId` column
  rather than renumber them.
- Tests: a `saveRDS()`/`readRDS()` round trip per entry point, including a
  pipeline with views and one that was changed after a restart.
- Docs: a short note in `?pip_new` (or the get-started vignette) on saving
  pipelines. NEWS bullet.
- Size M.

## WP7b — `pip_replace()` keeps tags and exec (`feat-replace-keep`)

`pip_replace()` resets tags and exec mode to the defaults
(`tags = character(0)`, `exec = "auto"`), while `p[[step, "fun"]] <- f`
already keeps them. Make `pip_replace()` keep them by default:
`tags = NULL`, `exec = NULL` mean "keep", explicit values still set them.

- This changes a default: mark it as a behaviour change in NEWS (for the
  "Breaking changes" block of WP8).
- Update `[[<-` (the `"fun"` branch in `gen`), which passes the old values
  explicitly,
  and the examples and vignettes that rely on the reset.
- Tests: tags and exec are kept; explicit values (also `character(0)`)
  override.

## WP7c — export the default logger (`feat-export-logger`)

`pipeflow_lgr(level, msg)` (`R/log.R`) is the default for `pip_run(lgr =)`,
but users can't call or wrap it, e.g. to add a prefix or to filter levels
and delegate the rest.

- Export it with a roxygen header and an example (a logger that drops
  `"info"` and passes the rest on), and list it in `_pkgdown.yml`.
- Keep the signature `function(level, msg)`, which `.assert_logger()`
  requires; keep the output format.
- Tests: the exported function writes the documented format (extend
  `test_log.R`). NEWS bullet.

## WP7d - Vignette for `pip_param_value()`

`pip_param_value()` is not a topic for the "Get started" vignette. Create a
separate vignette for it, likely in the "Advanced workflows" section of
`_pkgdown.yml` (articles list), covering annotated parameter values, how to
write and register a method, what it is (not) applied to, and how errors in
a method behave. Do this after WP5 has merged.

## WP7e — views keep track of their steps (`feat-view-steps`)

A view stores the row numbers of its steps, so it covers the wrong steps
once rows shift: after a step is inserted before them or removed through
the pipeline or another view. `pip_remove()` re-maps only the view it is
given, and only if the result is assigned back.

- Store the DAG node ids of the covered steps instead (`nodeId` column).
  They don't change on insert, remove, rename or replace, and are never
  reused; only `.pip_compact()` renumbers them, on a new copy. Step names
  were rejected: a rename (also through the view itself, as in the
  `?pip_rename` example) would drop the step from the view.
- `.pip_view_rows()` resolves the ids to rows on each access (pipeline
  order; removed steps drop out); `.pip_rows_to_view()` builds the
  selector in `[` and `pip_view()`. Drop the re-mapping in `pip_remove()`.
- `v[["view"]]` returns the names of the covered steps (behaviour change;
  before: row indices). New steps are not added to a view, even if they
  match its filters.
- Tests: views after insert, remove, rename (through any handle) and
  replace; running a view after rows shifted. Docs: `?pip_view`,
  `?pip_remove`, the `view` meta field, vignette v03b. Branched from
  `feat-run-state` (WP7), since both touch the view code.

## WP8 — release 0.5.0

- Merge the dependabot PRs (#72, #81, #82).
- `devtools::build_readme()`; update the vignettes for `~try()`, F1, F3,
  `pip_param_value()`, `.self` and saving pipelines (no vignette for the
  exported logger).
- NEWS: one `# pipeflow 0.5.0` section with short bullets. "Breaking
  changes" only lists what requires different commands or usage:
  - `v[["view"]]` returns step names, not row indices;
  - `.self` only in step functions that refer to it;
  - `pip_replace()` keeps tags and exec mode.

  Everything else goes under "New features" (`on_error = "continue"`,
  `~try()`, ...), "Changes" (identical values no longer outdate, step names
  in error messages, view names, views keep track of their steps, ...) and
  "Bug fixes".
- CI: add an Ubuntu job on R 4.2, the oldest supported R.
- Checks: lint, tests, `rcmdcheck` on R 4.5.3; CI on Linux, Windows, macOS
  and R 4.2; win-builder (release, devel); `cran-comments.md`;
  reverse-dependency check.
- Set `DESCRIPTION` to `0.5.0` (three parts) before tagging: the pkgdown
  workflow sends four-part versions to `dev/` on `gh-pages` and three-part
  ones to the site root (`development: mode: auto`).
- Submit; tag `v0.5.0` after acceptance. The tag starts the first release
  build of the site.
- Site switch after the `v0.5.0` tag build:
  1. Set the Pages source to branch `gh-pages`, folder `/ (root)` (repo
     settings, done by the user), and check the site.
  2. Follow-up PR: remove `docs/`, add it to `.gitignore`, and drop the
     stale `docs/articles/v05c-param-value.html` (the old name of the
     `v05d` vignette).

## Order

WP1 -> WP2 -> WP3 -> WP4 -> WP5 -> WP6 -> WP7 -> WP7e -> WP7a -> WP7b ->
WP7c -> WP7d -> WP8.

## Later (0.6+)

- **Per-key failures in split steps:** keep the outputs of the keys that
  succeeded and pass failure objects only for the failed keys. This
  changes `pipeflow_run_failed$failed`. Size L, about 150-250 lines: the
  per-key loop in `.pip_execute_step_call()`, the skip logic in
  `pip_run()` (pf:2371-2392), and `.pip_failure()` with a `key` field.
