# pipeflow — plan: `on_error` for the inner pipeline of `as.function()`

Starting point: `main` at 8b321d6 (0.5.0.9005) plus the `as.function()` PR
(branch `feat-as-function`, 0.5.0.9006, plan in its `dev/PLAN-as-function.md`).
Worktree `plan-as-function-followups`. Line references: `pf` =
`R/pipeflow.R` at 8b321d6, `af` = `R/as_function.R` at the as.function()
PR. Decided and implemented: **B** (explicit `on_error` argument), with the
warning of a continued inner run passed on (open question 2: no muffling).

## Problem

The function made by `as.function()` runs its inner pipeline with the
defaults of `pip_run()`: `.fun_call()` (af:284) calls
`pip_run(pip, lgr = NULL)`, so `on_error = "stop"`. Two consequences:

- A failing inner step always fails the function, and with it the outer step.
  The outer run then reacts as usual: with `on_error = "continue"` the outer
  step is `"failed"`, its dependents are skipped, or receive the failure
  object if they refer to the step via `~try()`.
- A `~try(step)` reference *inside* the inner pipeline does nothing, because
  `try()` only has an effect in a continuing run (`?pip_run`, "Continuing
  after errors"). An inner pipeline that handles a failed input works when it
  is run on its own, and silently does not when it is nested.

```r
inner <- pip_new("inner") |>
    pip_add("input", \(x = NULL) x) |>
    pip_add("fit", \(x = ~input) if (anyNA(x)) stop("NA in x") else mean(x))

outer <- pip_new("outer") |>
    pip_add("parts", \(values = list(a = 1:3, b = c(1, NA))) values,
            exec = "split") |>
    pip_add("run", as.function(inner, bind = list(x = ~parts), out = "fit"))

pip_run(outer, lgr = NULL, on_error = "continue")
# Warning: Run of pipeline 'outer' continued after 1 failed step:
#   'run': key 'b': step 'fit': NA in x
outer[["run", "state"]]   # "failed"
```

## Facts

- `on_error` of `pip_run()` is a local variable (pf:2613). It is only handed
  on to the nested run after a restart. It is not stored in the pipeline, and
  `.self` (`.wrap_self()`, pf:173) is the pipeline, so a step function cannot
  see how the run that calls it was started.
- In a continuing run a failing step gets state `"failed"` and its condition
  as output (`pip_run()`, pf:2748 ff.). Steps with a failed input are skipped
  (they keep their state and have no output of their own) unless the input is
  referred to via `~try()`: then the step runs and receives a
  `pipeflow_failure` (`formula_try_args()`, `R/utils.R:70`; `.pip_failure()`,
  pf:415). The run state is `"continued"` and a warning lists the failures.
- A mapped outer step has no partial results: if one key fails, the whole
  step fails and the stored condition is the key error. That is a limit of
  pipeflow, not of this plan.
- The kept inner pipelines (`cache`) are kept before they are run, so after
  a failure the steps that did run are reused on the next call. A failed
  step is retried, as in any pipeline.

## What an inherited `on_error` would add

1. `~try()` inside the inner pipeline works as it does in a standalone run.
2. Inner steps that do not depend on the failed step still run, and their
   outputs are kept for the next call.

It does not change who fails: the outer run's `on_error` keeps deciding what
happens to the outer step. The benefit is modest; (1) is the reason to do it.

## Options

**A. Inherit from the outer run.** The function reads the `on_error` of the
run that calls it. Needs a pipeflow change first: the run would have to
expose its `on_error` while it runs (stored in the pipeline environment next
to `.run_state`, and reachable from a step function, e.g. through `.self`).
Automatic, but it adds a run context to pipeflow's API for one user, and the
function would behave differently depending on the run that calls it (a
direct call has no run: `"stop"`).

**B. Explicit argument.** `as.function(x, ..., on_error = c("stop",
"continue"))`, default `"stop"`, fixed at conversion like `out` and `cache`.
No pipeflow change, deterministic, easy to test. The user sets it twice (the
outer run and the function) if both should continue.

**C. Nothing.** Document that `~try()` in a nested pipeline is inert.

**Recommendation: B now, A only if users ask for the automatic variant.** B
is a small change in `af` and does not close the door for A: with A the
argument could later default to "inherit".

## Semantics (B)

- The inner run is `pip_run(pip, lgr = NULL, on_error = on_error)`.
- Return value: if all requested `out` steps are `"done"`, as today. If one
  of them is `"failed"` or was skipped, the function signals an error, so the
  outer run decides. The message stays the one of today (`step '<inner
  step>': <message>`, prefixed by the outer step and key). No condition
  objects are returned as values.
  With `out = NULL` all steps are requested, so any failed inner step is an
  error: name the steps you need in `out` to profit from a continuing run.
- The warning of a continued inner run ("Run of pipeline ... continued after
  ...") passes on like other warnings of inner steps. It is redundant when
  the function errors right after; decide whether to muffle it in that case
  (open question 2).
- The match key of the kept pipelines does not change; `on_error` is not an
  input.
- Validation: `match.arg()`-style, an error at conversion for other values.

## Work

- `af`: argument, `ctx$onError`, check of the requested steps after the run
  (the stored condition of the first `"failed"` step, in step order, is
  re-signalled with the usual `step '...'` prefix), roxygen (`@param`, a
  paragraph in the details).
- Vignette v05b: one short paragraph or bullet in "Caching of inner
  pipelines"/"A few things to know", linking `v05c-error-handling`.
- NEWS: bullet under the version of the PR.

## Tests

- default unchanged: inner failure is an error of the function, as before;
- `on_error = "continue"`: an inner step with `~try(step)` runs and gets the
  `pipeflow_failure`; the function returns the requested output;
- an inner step independent of the failed one runs and keeps its output in
  the kept pipeline (check via `.fun_pips()`);
- a requested `out` step that failed or was skipped is an error with the
  usual message, also inside an outer pipeline (state `"failed"`, dependents
  skipped, `~try()` in the outer pipeline receives the failure);
- retry of the same bound input after a failure reuses the steps that ran;
- `out = NULL` with a failed inner step is an error;
- invalid `on_error` is rejected at conversion.

## Acceptance

Tests green, lint clean, `R CMD check` 0/0/0; the example above, with
`on_error = "continue"` on the function, gives the failure for key `b` only
through the outer run's own mechanisms.

## Not in scope

Partial results of a mapped outer step (per-key failures in pipeflow),
inheriting other run options (`force`, `lgr`), a run context in pipeflow
(option A) unless decided.

## Open questions

1. A or B (recommended: B)? Is the benefit worth an argument at all (C)?
   -> **B**. `as.function(x, ..., on_error = c("stop", "continue"))`,
   validated with `match.arg()` as in `pip_run()`.
2. Muffle the warning of the continued inner run when the function errors
   anyway? -> **No**: it is passed on like any other warning of an inner
   step, and it lists all failures, while the error names only the first
   affected requested step.
3. For A: should `.self` offer the run's `on_error`, or should pipeflow
   provide a separate accessor? Both widen the API. -> Open, only if users
   ask for the automatic variant; the argument could then default to
   `"inherit"`.

## Implementation

`.fun_call()` runs the inner pipeline with `on_error = ctx$onError` and then
calls `.fun_check_run()`, which re-signals the condition of the failed step
with `.pip_step_error()`, so that the message is the same as in a run with
`on_error = "stop"`.

Which step failed for a requested step is worked out by
`.fun_failure_causes()`, which walks the steps in order and follows the
failures exactly as `pip_run()` follows them: a step that got a failure
through `~try()` has run, and a locked step is neither run nor skipped, so
neither hands the failure to its own dependents. Following all edges of the
DAG instead would name the wrong step when a second failure, handled or
shielded, lies upstream of the requested one.
