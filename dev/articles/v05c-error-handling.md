# Handling failures

In practice, steps fail: data is missing, a model does not converge, or
an input violates an assumption. This vignette shows how {pipeflow}
reports such failures, how a run can be continued for unaffected steps,
and how to pick up the pieces once the cause is fixed.

### A pipeline that fails

We use a small pipeline whose `check` step fails if the data exceeds a
limit.

``` r

library(pipeflow)

pip <- pip_new("analysis") |>
    pip_add("data", \(n = 5) seq_len(n)) |>
    pip_add("check", \(x = ~data, limit = 3) {
        if (max(x) > limit) stop("data exceeds limit")
        x
    }) |>
    pip_add("model", \(x = ~check) mean(x)) |>
    pip_add("total", \(x = ~data) sum(x))
```

With `n = 5` and `limit = 3`, the `check` step will fail, and the
`model` step downstream of it cannot run. The `total` step, however,
only needs the `data` step.

### Stop at the first error

By default,
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
stops at the first step that fails.

``` r

pip_run(pip)
# info [2026-10-20 18:52:10.474 UTC]: Starting run of pipeflow 'analysis'
# info [2026-10-20 18:52:10.474 UTC]: Step 1/4 data
# info [2026-10-20 18:52:10.476 UTC]: Step 2/4 check
# error [2026-10-20 18:52:10.478 UTC]: data exceeds limit
# Error:
# ! step 'check': data exceeds limit

pip
# <pipeflow> analysis (4 steps)
# -----------------------------
#     step  params depends  state       out
# 1:  data       n           done 1,2,3,4,5
# 2: check x,limit    data failed    [NULL]
# 3: model       x   check    new    [NULL]
# 4: total       x    data    new    [NULL]
# -----------------------------
# <failed> last run: 2026-10-20 18:52:10

pip_run_state(pip)
# [1] "failed"
```

The pipeline stays in a defined state. The failed step is marked as
`failed`, the steps that were not reached are still `new`, and the run
state indicates that the last run ended with an error.

Let’s inspect the error object returned by
[`tryCatch()`](https://rdrr.io/r/base/conditions.html).

``` r

err <- tryCatch(pip_run(pip, lgr = NULL), error = \(e) e)

class(err)
# [1] "pipeflow_step_error" "error"               "condition"
err$step
# [1] "check"

class(err$parent)
# [1] "simpleError" "error"       "condition"
err$parent$message
# [1] "data exceeds limit"
```

The error is of class `pipeflow_step_error`. Its field `step` names the
failed step and the field `parent` holds the original condition, which
keeps its own class, so that errors can be handled by class:

### Continue after errors

In the previous run, the `total` step was not run, although it does not
depend on the failed step, because the run stopped early. With
`on_error = "continue"` in
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md),
a failing step is still marked as failed but the run can continue for
all other steps that do not depend on it:

``` r

pip$reset()

suppressWarnings(pip_run(pip, on_error = "continue"))
# info [2026-10-20 18:52:10.761 UTC]: Starting run of pipeflow 'analysis'
# info [2026-10-20 18:52:10.761 UTC]: Step 1/4 data
# info [2026-10-20 18:52:10.763 UTC]: Step 2/4 check
# error [2026-10-20 18:52:10.764 UTC]: data exceeds limit
# info [2026-10-20 18:52:10.764 UTC]: Step 3/4 model - skipping step, input 'x' failed
# info [2026-10-20 18:52:10.764 UTC]: Step 4/4 total
# info [2026-10-20 18:52:10.765 UTC]: Finished run of pipeflow 'analysis'
# warn [2026-10-20 18:52:10.766 UTC]: Run of pipeline 'analysis' continued after 1 failed step:
#   'check': data exceeds limit
# Steps not run because an input failed: 'model'

pip
# <pipeflow> analysis (4 steps)
# -----------------------------
#     step  params depends  state                 out
# 1:  data       n           done           1,2,3,4,5
# 2: check x,limit    data failed <error/simpleError>
# 3: model       x   check    new              [NULL]
# 4: total       x    data   done                  15
# -----------------------------
# <continued> last run: 2026-10-20 18:52:10

pip_run_state(pip)
# [1] "continued"
```

Now `total` was calculated, while `model` was skipped, because its input
depends on the failed step. The `out` column of a failed step holds the
condition, which is printed in short form as `<kind/class>`.

To work with it programmatically, we can extract it as usual:

``` r

out <- pip[["check", "out"]]

class(out)
# [1] "simpleError" "error"       "condition"
out$message
# [1] "data exceeds limit"
```

Let’s inspect the warning object returned by
[`tryCatch()`](https://rdrr.io/r/base/conditions.html).

``` r

w <- tryCatch(
    pip_run(pip, lgr = NULL, on_error = "continue"),
    warning = \(w) w
)

names(w$failed)
# [1] "check"
w$skipped
# [1] "model"
```

The warning is signalled after all states and outputs are stored, so
catching it with [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
does not lose any results.

### Mitigate failed inputs with `try()`

Sometimes a step should run even if one of its inputs failed, for
example a report that mentions what is missing or if a fallback is
available. Such an argument is marked by wrapping the step reference in
[`try()`](https://rdrr.io/r/base/try.html):

``` r

pip_add(
    pip,
    "report",
    \(total = ~total, m = ~try(model)) {
        if (inherits(m, "pipeflow_failure")) {
            paste("total:", total, "- no model:", m$message)
        } else {
            paste("total:", total, "- mean:", m)
        }
    }
)

suppressWarnings(pip_run(pip, on_error = "continue"))
# info [2026-10-20 18:52:10.966 UTC]: Starting run of pipeflow 'analysis'
# info [2026-10-20 18:52:10.966 UTC]: Step 1/5 data - skipping done step
# info [2026-10-20 18:52:10.966 UTC]: Step 2/5 check
# error [2026-10-20 18:52:10.968 UTC]: data exceeds limit
# info [2026-10-20 18:52:10.968 UTC]: Step 3/5 model - skipping step, input 'x' failed
# info [2026-10-20 18:52:10.968 UTC]: Step 4/5 total - skipping done step
# info [2026-10-20 18:52:10.968 UTC]: Step 5/5 report
# info [2026-10-20 18:52:10.969 UTC]: Finished run of pipeflow 'analysis'
# warn [2026-10-20 18:52:10.970 UTC]: Run of pipeline 'analysis' continued after 1 failed step:
#   'check': data exceeds limit
# Steps not run because an input failed: 'model'

pip[["report", "out"]]
# [1] "total: 15 - no model: step 'model' was not run: step 'check' failed: data exceeds limit"
```

Instead of the output of `model`, the `m` argument received a *failure
object*, a condition of class `pipeflow_failure`, which stands in for
the missing output. The message tells why the input is not available. It
has the fields

- `step`: the step that provided the input (`model`),
- `failed_step`: the step that actually failed (`check`), and
- `parent`: the original error condition.

A step is only run despite a failed input if *all* its failed inputs are
marked with [`try()`](https://rdrr.io/r/base/try.html). Everything else
is skipped, as `model` above. Note that
[`try()`](https://rdrr.io/r/base/try.html) has an effect only in a
continuing run. With `on_error = "stop"`, the run ends at the failed
step anyway.

### Recover from failures

Both skipped and failed steps are retried in the next run, and so are
steps that got a failure object. Steps that were completed are not run
again. Let’s fix the cause by raising the limit and re-run:

``` r

pip_set_params(pip, list(limit = 10))
pip_run(pip)
# info [2026-10-20 18:52:11.037 UTC]: Starting run of pipeflow 'analysis'
# info [2026-10-20 18:52:11.038 UTC]: Step 1/5 data - skipping done step
# info [2026-10-20 18:52:11.038 UTC]: Step 2/5 check
# info [2026-10-20 18:52:11.039 UTC]: Step 3/5 model
# info [2026-10-20 18:52:11.040 UTC]: Step 4/5 total - skipping done step
# info [2026-10-20 18:52:11.040 UTC]: Step 5/5 report
# info [2026-10-20 18:52:11.048 UTC]: Finished run of pipeflow 'analysis'

pip
# <pipeflow> analysis (5 steps)
# -----------------------------
#      step  params     depends state                 out
# 1:   data       n              done           1,2,3,4,5
# 2:  check x,limit        data  done           1,2,3,4,5
# 3:  model       x       check  done                   3
# 4:  total       x        data  done                  15
# 5: report total,m total,model  done total: 15 - mean: 3
# -----------------------------
# <ready> last run: 2026-10-20 18:52:11
```

The run log shows that `data` and `total` were skipped, since they were
completed already. The failed step `check` and the skipped step `model`
were executed now, and so was `report`, because it had received a
failure object instead of the real output. The report now contains the
model result and the run state is `ready` again.

``` r

pip[["report", "out"]]
# [1] "total: 15 - mean: 3"

pip_run_state(pip)
# [1] "ready"
```

### Failures in split pipelines

Failures also work with the execution modes `split` and `reduce`, which
are described in the vignette on [split, map, and
reduce](https://rpahl.github.io/pipeflow/dev/articles/v05a-split-map-reduce.md).
Let’s split a small data set by group and let the `spread` step fail for
groups with a single row.

``` r

d <- data.frame(g = c("a", "a", "b", "b", "c"), y = c(1, 2, 3, 5, 8))

pip <- pip_new("groups") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add("by_group", \(data = ~data) split(data, data$g), exec = "split") |>
    pip_add("spread", \(d = ~by_group) {
        if (nrow(d) < 2) stop("need at least 2 rows")
        diff(range(d$y))
    }) |>
    pip_add(
        "combine",
        \(s = ~try(spread)) {
            if (inherits(s, "pipeflow_failure")) {
                return(s$message)
            }
            unlist(s)
        },
        exec = "reduce"
    )

pip_set_params(pip, list(data = d))
```

If one partition (group `c`) fails, the whole `spread` step fails. The
condition tells the failed key and keeps the original one as `parent`:

``` r

suppressWarnings(pip_run(pip, on_error = "continue"))
# info [2026-10-20 18:52:11.246 UTC]: Starting run of pipeflow 'groups'
# info [2026-10-20 18:52:11.246 UTC]: Step 1/4 data
# info [2026-10-20 18:52:11.247 UTC]: Step 2/4 by_group
# info [2026-10-20 18:52:11.248 UTC]: Step 3/4 spread
# error [2026-10-20 18:52:11.252 UTC]: key 'c': need at least 2 rows
# info [2026-10-20 18:52:11.252 UTC]: Step 4/4 combine
# info [2026-10-20 18:52:11.253 UTC]: Finished run of pipeflow 'groups'
# warn [2026-10-20 18:52:11.254 UTC]: Run of pipeline 'groups' continued after 1 failed step:
#   'spread': key 'c': need at least 2 rows

pip
# <pipeflow> groups (4 steps)
# ---------------------------
#        step params  depends  state                                         out   exec
# 1:     data   data            done                           <data.frame[5x2]>   auto
# 2: by_group   data     data   done                                   <list[3]>  split
# 3:   spread      d by_group failed                  <error/pipeflow_key_error>   auto
# 4:  combine      s   spread   done step 'spread' failed: key 'c': need at l... reduce
# ---------------------------
# <continued> last run: 2026-10-20 18:52:11

err <- pip[["spread", "out"]]
class(err)
# [1] "pipeflow_key_error" "error"              "condition"
err$key
# [1] "c"
err$parent$message
# [1] "need at least 2 rows"
```

The `combine` step reduces the outputs of all groups. Since the input
`spread` failed and is marked with
[`try()`](https://rdrr.io/r/base/try.html), `combine` ran once and
received the failure object, with the key in the message:

``` r

pip[["combine", "out"]]
# [1] "step 'spread' failed: key 'c': need at least 2 rows"
```

Likewise, if the pipeline is run with `on_error = "stop"`, the error is
a `pipeflow_step_error` as before, with the `pipeflow_key_error` as its
`parent`. Without [`try()`](https://rdrr.io/r/base/try.html) in the
`combine` step, it would just be skipped.

Failures are currently tracked per step, not per key, so the groups `a`
and `b` that were fine are not available as results of a run that failed
in group `c`. Once the cause is fixed, here by removing the group, the
pipeline completes:

``` r

pip_set_params(pip, list(data = d[d$g != "c", ]))
pip_run(pip)
# info [2026-10-20 18:52:11.388 UTC]: Starting run of pipeflow 'groups'
# info [2026-10-20 18:52:11.389 UTC]: Step 1/4 data
# info [2026-10-20 18:52:11.390 UTC]: Step 2/4 by_group
# info [2026-10-20 18:52:11.392 UTC]: Step 3/4 spread
# info [2026-10-20 18:52:11.393 UTC]: Step 4/4 combine
# info [2026-10-20 18:52:11.396 UTC]: Finished run of pipeflow 'groups'

pip[["combine", "out"]]
# a b 
# 1 2
```

### Failures in nested pipelines

A pipeline can be turned into a function with
[`as.function()`](https://rdrr.io/r/base/as.function.html) and added as
a step to another pipeline, as described in the vignette on [nested
pipelines](https://rpahl.github.io/pipeflow/dev/articles/v05b-nested-pipeline.md).
Such a step runs a pipeline of its own, which means it also has its own
error handling.

Let’s put the `spread` step of the previous section into an inner
pipeline that reports on it, and map that pipeline over the groups:

``` r

inner <- pip_new("inner") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add("spread", \(d = ~data) {
        if (nrow(d) < 2) stop("need at least 2 rows")
        diff(range(d$y))
    }) |>
    pip_add("report", \(s = ~try(spread)) {
        if (inherits(s, "pipeflow_failure")) "no spread" else s
    })

nested <- pip_new("nested") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add("by_group", \(data = ~data) split(data, data$g), exec = "split") |>
    pip_add(
        "per_group",
        as.function(inner, out = "report"),
        params = list(data = ~by_group)
    )

pip_set_params(nested, list(data = d))
```

By default, the inner pipeline is run with `on_error = "stop"`, so it
ends at the first failed inner step and the outer step fails with it.
The [`try()`](https://rdrr.io/r/base/try.html) reference of the `report`
step has no effect here, because
[`try()`](https://rdrr.io/r/base/try.html) only matters in a
*continuing* run:

``` r

pip_run(nested)
# info [2026-10-20 18:52:11.530 UTC]: Starting run of pipeflow 'nested'
# info [2026-10-20 18:52:11.531 UTC]: Step 1/3 data
# info [2026-10-20 18:52:11.531 UTC]: Step 2/3 by_group
# info [2026-10-20 18:52:11.533 UTC]: Step 3/3 per_group
# error [2026-10-20 18:52:11.549 UTC]: key 'c': step 'spread': need at least 2 rows
# Error:
# ! step 'per_group': key 'c': step 'spread': need at least 2 rows
```

With `as.function(on_error = "continue")`, the inner pipeline is run as
`pip_run(on_error = "continue")` runs it: inner steps that do not depend
on the failed step still run, and `~try(spread)` gives the `report` step
the failure object. Group `c` is handled inside the inner pipeline, and
the outer run completes:

``` r

pip_replace(
    nested,
    "per_group",
    as.function(inner, out = "report", on_error = "continue"),
    params = list(data = ~by_group)
)

pip_run(nested)
# info [2026-10-20 18:52:11.629 UTC]: Starting run of pipeflow 'nested'
# info [2026-10-20 18:52:11.629 UTC]: Step 1/3 data - skipping done step
# info [2026-10-20 18:52:11.630 UTC]: Step 2/3 by_group - skipping done step
# info [2026-10-20 18:52:11.630 UTC]: Step 3/3 per_group
# warn [2026-10-20 18:52:11.645 UTC]: Run of pipeline 'inner' continued after 1 failed step:
#   'spread': need at least 2 rows
# Warning: Run of pipeline 'inner' continued after 1 failed step:
#   'spread': need at least 2 rows
# info [2026-10-20 18:52:11.647 UTC]: Finished run of pipeflow 'nested'

nested[["per_group", "out"]]
# $a
# [1] 1
# 
# $b
# [1] 2
# 
# $c
# [1] "no spread"
# 
# attr(,"class")
# [1] "list"                 "pipeflow_partitioned"
```

The warning of the continued inner run is passed on like any other
warning of a step, so the outer run logs it and the result of every
group is available.

The function still fails if one of the steps named in `out` failed or
was not run because an input failed: the inner pipeline handles what it
can, and everything else is left to the outer run, as before.

``` r

pip_replace(
    nested,
    "per_group",
    as.function(
        inner,
        out = "spread",
        on_error = "continue"           # <--- this time we continue
    ),
    params = list(data = ~by_group)
)

pip_run(nested, lgr = NULL)
# Warning: Run of pipeline 'inner' continued after 1 failed step:
#   'spread': need at least 2 rows
# Error:
# ! step 'per_group': key 'c': step 'spread': need at least 2 rows
```

Since `out = NULL` requests the output of *all* inner steps, name the
steps you need in `out` to profit from a continuing inner run.

### Summary

- By default, a failing step stops the run with a `pipeflow_step_error`
  that names the step and holds the original condition in `parent`.
- `pip_run(on_error = "continue")` marks the step as `failed`, stores
  its condition as output, skips the steps that depend on it, and runs
  everything else. The run state becomes `continued`, and a
  `pipeflow_run_failed` warning lists both failed and skipped steps.
- A reference wrapped in [`try()`](https://rdrr.io/r/base/try.html),
  e.g. `x = ~try(step)`, makes a step run despite a failed input. The
  argument then receives a `pipeflow_failure` condition.
- The next run retries any failed and skipped steps (and those that
  received failure objects).
- For steps that map over partitions, the condition of the failed
  partition is a `pipeflow_key_error` with the field `key`. A `reduce`
  step with a [`try()`](https://rdrr.io/r/base/try.html) reference
  receives the failure.
- A step made with
  [`as.function()`](https://rdrr.io/r/base/as.function.html) runs a
  pipeline of its own. `as.function(on_error = "continue")` lets that
  inner pipeline continue after a failed inner step, which is also what
  makes [`try()`](https://rdrr.io/r/base/try.html) references inside it
  work. The step still fails if one of the steps named in `out` failed
  or was not run.
