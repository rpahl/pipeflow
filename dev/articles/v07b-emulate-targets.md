# Emulating targets features

The [comparison with
targets](https://rpahl.github.io/pipeflow/dev/articles/v07-vs-targets.md)
lists several features that {targets} has and {pipeflow} does not: file
tracking, metadata, runs in a fresh R process, and more. Many of them
can be emulated with existing {pipeflow} functions and a few lines of
your own code. This article shows one recipe per feature. Each section
names the {targets} feature, shows the recipe as running code, and says
where the emulation stops.

``` r

library(pipeflow)
```

## File targets

In {targets}, a target with `format = "file"` tracks a file: when the
file changes, the target and everything downstream of it run again.

{pipeflow} doesn’t look at files. But it does notice when a parameter
changes. So give the step that reads the file an extra parameter, here
`rev`, that it ignores, and set it to a fingerprint of the file before
each run.

``` r

path <- tempfile(fileext = ".csv")
write.csv(data.frame(x = 1:3), path, row.names = FALSE)

p <- pip_new("files") |>
    pip_add("read", \(path, rev = NULL) read.csv(path),
        params = list(path = path)
    ) |>
    pip_add("total", \(data = ~read) sum(data$x))

refresh <- function(p) {
    pip_set_params(p, list(rev = unname(tools::md5sum(path))))
}
```

The first run computes everything.

``` r

refresh(p)
p
# <pipeflow> files (2 steps)
# --------------------------
#     step   params depends state
# 1:  read path,rev           new
# 2: total     data    read   new
# --------------------------
# <ready> last run: never
# steps: 2 new
pip_run(p)
# info [2026-10-26 16:31:46.372 UTC]: Starting run of pipeflow 'files'
# info [2026-10-26 16:31:46.372 UTC]: Step 1/2 read
# info [2026-10-26 16:31:46.375 UTC]: Step 2/2 total
# info [2026-10-26 16:31:46.375 UTC]: Finished in 0.003 s (2 done, 0 skipped, 0 failed)
```

Setting a parameter to the value it already has is not a change (see
[Setting Identical
Values](https://rpahl.github.io/pipeflow/dev/articles/v01-get-started.html#setting-identical-values)).
So if the file is unchanged, `refresh()` leaves the states alone and
nothing runs.

``` r

refresh(p)
p
# <pipeflow> files (2 steps)
# --------------------------
#     step   params depends state               out
# 1:  read path,rev          done <data.frame[3x1]>
# 2: total     data    read  done                 6
# --------------------------
# <ready> last run: 2026-10-26 16:31:46 (0.003 s)
# steps: 2 done
pip_run(p)
# info [2026-10-26 16:31:46.459 UTC]: Starting run of pipeflow 'files'
# info [2026-10-26 16:31:46.459 UTC]: Step 1/2 read - skipping done step
# info [2026-10-26 16:31:46.459 UTC]: Step 2/2 total - skipping done step
# info [2026-10-26 16:31:46.459 UTC]: Finished in 0.001 s (0 done, 2 skipped, 0 failed)
```

Once the file is edited, its fingerprint changes. `refresh()` outdates
the step that reads it and all steps downstream of it.

``` r

write.csv(data.frame(x = 1:4), path, row.names = FALSE)

refresh(p)
p
# <pipeflow> files (2 steps)
# --------------------------
#     step   params depends    state               out
# 1:  read path,rev         outdated <data.frame[3x1]>
# 2: total     data    read outdated                 6
# --------------------------
# <ready> last run: 2026-10-26 16:31:46 (0.001 s)
# steps: 2 outdated
pip_run(p)
# info [2026-10-26 16:31:46.532 UTC]: Starting run of pipeflow 'files'
# info [2026-10-26 16:31:46.532 UTC]: Step 1/2 read
# info [2026-10-26 16:31:46.535 UTC]: Step 2/2 total
# info [2026-10-26 16:31:46.537 UTC]: Finished in 0.005 s (2 done, 0 skipped, 0 failed)
p[["total", "out"]]
# [1] 10
```

To refresh and run in one go, use `refresh(p) |> pip_run()`.

**Variants.** [`file.mtime()`](https://rdrr.io/r/base/file.info.html)
and [`file.size()`](https://rdrr.io/r/base/file.info.html) are cheaper
than [`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html), because
they don’t read the file. They are also less reliable: a copy or a
`git checkout` changes the modification time without changing the
content, and an edit can keep the size. For several files,
`tools::md5sum(c(path1, path2))` returns one hash per file, and any
change in the vector outdates the step. If different steps read
different files, give each its own parameter (`raw_rev`, `lookup_rev`,
…).

**Limits.**

- The fingerprint is only as fresh as the last `refresh()`. Forget to
  call it, and an edited file goes unnoticed.
- Only files that steps read are covered. {targets} also notices when a
  file that a target wrote was changed or deleted. Here you have to
  rerun that step yourself.

## Rebuild when code changes

{targets} reruns a target when its command or a function it calls
changes, including your own helper functions.

In {pipeflow}, replacing a step function with
[`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
already outdates the step and its downstream steps. What {pipeflow}
can’t see is a helper function that the step calls. Use the same trick
as for files, with a hash of the helper as fingerprint.

``` r

clean <- function(x) round(x, 1)

p <- pip_new("code") |>
    pip_add("data", \(n = 4) seq_len(n) / 3) |>
    pip_add("cleaned", \(x = ~data, code = NULL) clean(x))

refresh_code <- function(p) {
    pip_set_params(p, list(code = rlang::hash(utils::removeSource(clean))))
}

refresh_code(p) |> pip_run(lgr = NULL)
p
# <pipeflow> code (2 steps)
# -------------------------
#       step params depends state                                     out
# 1:    data      n          done 0.3333333,0.6666667,1.0000000,1.3333333
# 2: cleaned x,code    data  done                         0.3,0.7,1.0,1.3
# -------------------------
# <ready> last run: 2026-10-26 16:31:46 (0.002 s)
# steps: 2 done
```

[`removeSource()`](https://rdrr.io/r/utils/removeSource.html) matters:
in an interactive session, R keeps the source code of every function you
define, and the hash would cover it. Then running the same definition
again would change the hash. Without the source, defining the same
helper again changes nothing:

``` r

clean <- function(x) round(x, 1)

refresh_code(p)
p
# <pipeflow> code (2 steps)
# -------------------------
#       step params depends state                                     out
# 1:    data      n          done 0.3333333,0.6666667,1.0000000,1.3333333
# 2: cleaned x,code    data  done                         0.3,0.7,1.0,1.3
# -------------------------
# <ready> last run: 2026-10-26 16:31:46 (0.002 s)
# steps: 2 done
```

A new version of the helper outdates the step that calls it.

``` r

clean <- function(x) round(x, 2)

refresh_code(p)
p
# <pipeflow> code (2 steps)
# -------------------------
#       step params depends    state                                     out
# 1:    data      n             done 0.3333333,0.6666667,1.0000000,1.3333333
# 2: cleaned x,code    data outdated                         0.3,0.7,1.0,1.3
# -------------------------
# <ready> last run: 2026-10-26 16:31:46 (0.002 s)
# steps: 1 done, 1 outdated
pip_run(p)
# info [2026-10-26 16:31:46.757 UTC]: Starting run of pipeflow 'code'
# info [2026-10-26 16:31:46.757 UTC]: Step 1/2 data - skipping done step
# info [2026-10-26 16:31:46.757 UTC]: Step 2/2 cleaned
# info [2026-10-26 16:31:46.759 UTC]: Finished in 0.002 s (1 done, 1 skipped, 0 failed)
p[["cleaned", "out"]]
# [1] 0.33 0.67 1.00 1.33
```

**Limits.**

- You list the helpers yourself. {targets} finds the functions a target
  calls, and the functions those call, by static code analysis. Here, a
  helper of a helper is only covered if you add it to the hash, e.g.
  `rlang::hash(lapply(list(clean, other), utils::removeSource))`.
- Like {targets}, this does not track functions from packages. A package
  update does not outdate anything.

## A metadata table like `tar_meta()`

`tar_meta()` returns a table with one row per target: when it ran, how
long it took, the size of its output and the error if it failed.

The step table of a pipeline
([`pip_data()`](https://rpahl.github.io/pipeflow/dev/reference/pip_data.md))
has most of this. `time_start` is the start of the step’s last run.
`time` is the end of its last *successful* run, so a failed step keeps
the `time` of an earlier success (or `NA`). The duration is
`time - time_start`, but only for steps that did not fail. The output of
a failed step is the condition that it signalled.

``` r

meta <- function(p) {
    d <- as.data.frame(pip_data(p))
    ok <- d$state != "failed"
    seconds <- as.numeric(difftime(d$time, d$time_start, units = "secs"))
    bytes <- vapply(d$out, \(x) as.numeric(object.size(x)), numeric(1))
    error <- vapply(
        d$out,
        \(x) if (inherits(x, "error")) conditionMessage(x) else NA_character_,
        character(1)
    )
    data.frame(
        step = d$step,
        state = d$state,
        start = d$time_start,
        seconds = ifelse(ok, round(seconds, 2), NA),
        bytes = ifelse(ok, bytes, NA),
        error = error
    )
}
```

Let’s use it on a pipeline with a slow first step and a step that fails
for large inputs.

``` r

p <- pip_new("meta") |>
    pip_add("load", \(n = 3) {
        Sys.sleep(0.2)
        seq_len(n)
    }) |>
    pip_add("fit", \(x = ~load) {
        if (length(x) > 3) stop("too many values")
        sum(x)
    }) |>
    pip_add("report", \(x = ~fit) paste("total:", x))

pip_run(p, lgr = NULL)
meta(p)
#     step state               start seconds bytes error
# 1   load  done 2026-10-26 16:31:46     0.2    64  <NA>
# 2    fit  done 2026-10-26 16:31:47     0.0    56  <NA>
# 3 report  done 2026-10-26 16:31:47     0.0   120  <NA>
```

Now the input grows and `fit` fails. With `on_error = "continue"`, the
run goes on, and `report` is skipped because its input failed.

``` r

pip_set_params(p, list(n = 5))
pip_run(p, lgr = NULL, on_error = "continue")
# Warning: Run of pipeline 'meta' continued after 1 failed step:
#   'fit': too many values
# Steps not run because an input failed: 'report'
meta(p)
#     step    state               start seconds bytes           error
# 1   load     done 2026-10-26 16:31:47     0.2    80            <NA>
# 2    fit   failed 2026-10-26 16:31:47      NA    NA too many values
# 3 report outdated 2026-10-26 16:31:47     0.0   120            <NA>
```

The table shows the error of `fit`. `report` is `outdated` and still has
the start time and output of the first run.

**Limits.**

- Only the last run of each step is recorded. Keep a history by
  appending `meta(p)` to a table after each run.
- Warnings are not recorded, and there is no hash of the output.

## Metadata stored with each output

{targets} stores metadata such as size and format along with each
target.

In {pipeflow}, the generic
[`pip_output_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_output_value.md)
is called on every output right after its step returns. A method for
your own class can attach the metadata to the output itself. See
[Annotated step
outputs](https://rpahl.github.io/pipeflow/dev/articles/v05e-output-value.md)
for the details.

``` r

tracked <- function(x) structure(x, class = c("tracked", class(x)))

pip_output_value.tracked <- function(x, ctx = list(), ...) {
    attr(x, "meta") <- list(
        step = ctx[["step"]],
        time = ctx[["time"]],
        class = setdiff(class(x), "tracked"),
        bytes = as.numeric(object.size(x))
    )
    x
}
registerS3method("pip_output_value", "tracked", pip_output_value.tracked)

p <- pip_new("tracked") |>
    pip_add("cars", \(n = 5) tracked(head(mtcars, n)))

pip_run(p, lgr = NULL)
str(attr(p[["cars", "out"]], "meta"))
# List of 4
#  $ step : chr "cars"
#  $ time : POSIXct[1:1], format: "2026-10-26 16:31:47"
#  $ class: chr "data.frame"
#  $ bytes: num 2976
```

**Limits.** Each step has to mark its output with the class. Methods are
meant for your own classes, not for base classes such as `data.frame`.

## Reproducible random numbers

{targets} gives each target its own seed (see `tar_seed()`), so a target
draws the same random numbers no matter which other targets run.

In {pipeflow}, steps draw from the global random number stream. Steps
that are skipped don’t draw, so the numbers a step gets depend on which
steps ran before it. Here, `b` gets different numbers when it reruns
alone than in a full run, although the seed is the same.

``` r

p <- pip_new("rng") |>
    pip_add("a", \(n = 3) rnorm(n)) |>
    pip_add("b", \(n = 3, scale = 1) scale * rnorm(n))

set.seed(1)
pip_run(p, lgr = NULL)
b_full <- p[["b", "out"]]

pip_set_params(p, list(scale = 2))
set.seed(1)
pip_run(p, lgr = NULL)
p[["b", "out"]]
# [1] -1.2529076  0.3672866 -1.6712572
2 * b_full
# [1]  3.1905616  0.6590155 -1.6409368
```

The recipe: give each step a `seed` parameter and set it inside the
step. A small helper also restores the global random number state
afterwards, so running the pipeline doesn’t change the numbers that the
rest of your session draws.
[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
does the same, if you have {withr} installed.

``` r

with_seed <- function(seed, code) {
    old <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(
        if (is.null(old)) {
            rm(".Random.seed", envir = globalenv())
        } else {
            assign(".Random.seed", old, envir = globalenv())
        }
    )
    set.seed(seed)
    code
}
```

Now `b` gets the same numbers, whether it runs alone or with `a`.

``` r

p <- pip_new("rng") |>
    pip_add("a", \(n = 3, seed = 1) with_seed(seed, rnorm(n))) |>
    pip_add("b", \(n = 3, scale = 1, seed = 2) {
        with_seed(seed, scale * rnorm(n))
    })

pip_run(p, lgr = NULL)
b_full <- p[["b", "out"]]

pip_set_params(p, list(scale = 2))
pip_run(p, lgr = NULL)
p[["b", "out"]]
# [1] -1.7938291  0.3696984  3.1756907
2 * b_full
# [1] -1.7938291  0.3696984  3.1756907
```

**Limits.**

- You choose the seeds. {targets} derives them from the target names.
- `pip_set_params(p, list(seed = 42))` sets the seed of *all* steps with
  a `seed` parameter, so they would all draw the same numbers.
- Steps that run code in parallel need a parallel-safe generator, as
  with any R code.

## Results that survive an R restart

{targets} stores every target on disk, so the results survive when R is
restarted.

A {pipeflow} pipeline is an R object. Save it with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) after running it, and
read it back with [`readRDS()`](https://rdrr.io/r/base/readRDS.html) in
a new session. The functions, parameters, outputs and states are all
kept. The dependency graph lives in C++ and can’t be saved directly. It
is rebuilt automatically the first time the pipeline is used. Let’s save
the file pipeline from [above](#file-targets).

``` r

p <- pip_new("files") |>
    pip_add("read", \(path, rev = NULL) read.csv(path),
        params = list(path = path)
    ) |>
    pip_add("total", \(data = ~read) sum(data$x))

refresh(p) |> pip_run(lgr = NULL)

file <- tempfile(fileext = ".rds")
saveRDS(p, file)
rm(p)
```

Here, [`rm()`](https://rdrr.io/r/base/rm.html) stands in for the
restart. After [`readRDS()`](https://rdrr.io/r/base/readRDS.html), the
steps are still `done`. The fingerprint of the file was saved too, so
`refresh()` only reruns steps if the file changed in the meantime.

``` r

p <- readRDS(file)
p
# <pipeflow> files (2 steps)
# --------------------------
#     step   params depends state               out
# 1:  read path,rev          done <data.frame[4x1]>
# 2: total     data    read  done                10
# --------------------------
# <ready> last run: 2026-10-26 16:31:47 (0.002 s)
# steps: 2 done

refresh(p) |> pip_run()
# info [2026-10-26 16:31:47.803 UTC]: Starting run of pipeflow 'files'
# info [2026-10-26 16:31:47.803 UTC]: Step 1/2 read - skipping done step
# info [2026-10-26 16:31:47.803 UTC]: Step 2/2 total - skipping done step
# info [2026-10-26 16:31:47.804 UTC]: Finished in 0.001 s (0 done, 2 skipped, 0 failed)
```

Step functions keep their enclosing environment. If a step function was
created inside another function, that environment is saved with it,
including every object in it, which can make the file large. The global
environment is never saved: helpers such as `refresh()` or `with_seed()`
have to be defined again in the new session.

**Limits.**

- Nothing is saved automatically. If R crashes during a run, the results
  of that run are lost. {targets} saves each target as soon as it is
  built.
- The whole pipeline is one file, written and read as a whole, and it
  must fit in memory.

## Running in a fresh R process

`tar_make()` runs the pipeline in a new R process by default. The
results then can’t depend on objects that happen to be lying around in
your session.

With [{callr}](https://callr.r-lib.org), you can run a {pipeflow}
pipeline in a new R process as well. The pipeline is copied to the new
process, run there and copied back.

``` r

run_fresh <- function(p, ...) {
    callr::r(
        function(p, ...) {
            library(pipeflow)
            pip_run(p, ...)
            p
        },
        args = list(p, ...)
    )
}

p <- pip_new("fresh") |>
    pip_add("data", \(n = 5) seq_len(n)) |>
    pip_add("total", \(x = ~data) sum(x))

p <- run_fresh(p, lgr = NULL)
p
# <pipeflow> fresh (2 steps)
# --------------------------
#     step params depends state       out
# 1:  data      n          done 1,2,3,4,5
# 2: total      x    data  done        15
# --------------------------
# <ready> last run: 2026-10-26 16:31:48 (0.003 s)
# steps: 2 done
```

What you get back is a new pipeline object, so assign it (`p <-`). Views
and other references to the old object don’t see the results.

This is also where you find out what a step needs from your session. A
step that calls a function from a package you attached with
[`library()`](https://rdrr.io/r/base/library.html) fails in the new
process, where the package is not attached.

``` r

library(tools)

p <- pip_new("fresh") |>
    pip_add("ext", \(path = "data.csv") file_ext(path))

res <- tryCatch(run_fresh(p, lgr = NULL), error = identity)
conditionMessage(res$parent)
# [1] "step 'ext': could not find function \"file_ext\""
```

The same holds for objects and helper functions that a step finds in the
global environment of your session. Use
[`tools::file_ext()`](https://rdrr.io/r/tools/fileutils.html), or put
the [`library()`](https://rdrr.io/r/base/library.html) and
[`source()`](https://rdrr.io/r/base/source.html) calls that the steps
need into the function that `run_fresh()` runs in the new process.

**Limits.**

- {callr} is not a dependency of {pipeflow}, so you need to install it.
- The whole pipeline, with all outputs, is copied to the new process and
  back on every run. For large outputs, this takes time.
- Starting a new R process takes a moment too, which you pay on every
  run.

## Which steps are outdated?

`tar_outdated()` lists the targets that the next `tar_make()` would run.

In {pipeflow}, the state of a step tells whether it will run: `new`,
`outdated` and `failed` steps run, `done` steps are skipped. So a view
of these states lists what the next run does.

``` r

p <- pip_new("outdated") |>
    pip_add("a", \(x = 1) x) |>
    pip_add("b", \(a = ~a) a + 1) |>
    pip_add("c", \(b = ~b) b + 1) |>
    pip_add("d", \(y = 1) y)

pip_run(p, lgr = NULL)
pip_set_params(p, list(y = 2))
pip_view(p, state = c("new", "outdated", "failed"))
# <pipeflow_view> outdated (1 of 4 steps)
# ---------------------------------------
#    step params depends    state out
# 1:    d      y         outdated   1
# ---------------------------------------
# <ready> last run: 2026-10-26 16:31:48 (0.003 s)
# steps: 1 outdated
```

The view misses a `done` step that reruns because a step upstream of it
runs. {pipeflow} functions such as
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
or
[`pip_reset()`](https://rpahl.github.io/pipeflow/dev/reference/pip_reset.md)
outdate the downstream steps, so this only happens if you set a state by
hand, which outdates no other step (see
[`?Extract.pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)).
Here, `b` and `c` stay `done`, but they run again with the new output of
`a`.

``` r

p[["a", "state"]] <- "outdated"
pip_view(p, state = c("new", "outdated", "failed"))
# <pipeflow_view> outdated (2 of 4 steps)
# ---------------------------------------
#    step params depends    state out
# 1:    a      x         outdated   1
# 2:    d      y         outdated   1
# ---------------------------------------
# <ready> last run: 2026-10-26 16:31:48 (0.003 s)
# steps: 2 outdated
pip_run(p)
# info [2026-10-26 16:31:48.827 UTC]: Starting run of pipeflow 'outdated'
# info [2026-10-26 16:31:48.827 UTC]: Step 1/4 a
# info [2026-10-26 16:31:48.829 UTC]: Step 2/4 b
# info [2026-10-26 16:31:48.830 UTC]: Step 3/4 c
# info [2026-10-26 16:31:48.832 UTC]: Step 4/4 d
# info [2026-10-26 16:31:48.833 UTC]: Finished in 0.006 s (4 done, 0 skipped, 0 failed)
```

**Limits.**

- Locked steps are listed but skipped. Leave them out with
  `p[state %in% c("new", "outdated", "failed") & !locked]`.
- The states only know about changes made through {pipeflow}. Call
  `refresh()` from the recipes [above](#file-targets) first, so that
  changed files and helpers count.

## What can’t be emulated

Some features of {targets} need infrastructure that {pipeflow} doesn’t
have, and a few lines of code won’t change that.

- **Distributed workers.** {targets} sends targets to parallel workers,
  also on clusters and in the cloud, via {crew}. {pipeflow} runs the
  steps one after another in one R session. A step can parallelize its
  own work, but the pipeline does not schedule steps on workers.
- **Cloud storage.** {targets} can store targets in AWS S3 or Google
  Cloud Storage. You can upload the file from
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) yourself, but there
  is no storage per step.
- **Data larger than RAM.** {targets} keeps each target on disk and
  loads only what a target needs. A {pipeflow} pipeline keeps all
  outputs in memory. A step can return a file path instead of the data,
  but then you manage the files yourself.

If you need these, use {targets}. If you mainly work in an interactive
session or a Shiny app, the recipes above cover a good part of the rest.
