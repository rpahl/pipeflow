# Self-modifying pipelines

### Internal pipeline structure

{pipeflow} aims to offer a lean and intuitive interface that enables new
users to get started quickly without having to learn a lot of new
functions. At the same time, it was designed to provide easy access to
the underlying data structures to allow advanced users to modify the
pipeline basically in any way they want.

To see this, let’s briefly inspect the internal structure of a pipeline
object.

``` r
pip <- pip_new("my-pipeline") |>
    pip_add("init", \(xInit = 0) xInit) |>
    pip_add("f1", \(x = ~init) x + 1) |>
    pip_add("f2", \(x = ~f1) x + 2) |>
    pip_add("f3", \(x = ~f2) x + 3)

str(pip)
# List of 3
#  $ name  : chr "my-pipeline"
#  $ view  : NULL
#  $ pipenv:<environment: 0x5565409ca5a8>
```

There is the `name` of the pipeline, which is just a character string,
as well as a `view` entry, which is undefined initially.

The most “interesting” part is the `pipenv`: an environment that holds
the pipeline’s actual state. It is shared by the pipeline and all of its
views, which is why operations on a view write through to the underlying
pipeline.

``` r
ls(pip$pipenv)
# [1] "data"
```

Listing the `pipenv`[¹](#fn1) reveals the `data`entry, which contains
the pipeline’s step table as a `data.table` with one row per step:

``` r
pip$pipenv$data # or pip_data(pip)
#      step           fun    params    out  state   tags locked   exec                time depends unbound nodeId
#    <char>        <list>    <list> <list> <char> <list> <lgcl> <char>              <POSc>  <list>  <list>  <int>
# 1:   init <function[1]> <list[1]> [NULL]    new         FALSE   auto 2026-09-27 17:20:06           xInit      0
# 2:     f1 <function[1]> <list[1]> [NULL]    new         FALSE   auto 2026-09-27 17:20:06    init              1
# 3:     f2 <function[1]> <list[1]> [NULL]    new         FALSE   auto 2026-09-27 17:20:06      f1              2
# 4:     f3 <function[1]> <list[1]> [NULL]    new         FALSE   auto 2026-09-27 17:20:06      f2              3
```

Many of the columns should be already familar to you and most of them
can be manipulated safely via the `[<-` and `[[<-` operators, for
example:

``` r
pip[["f2", "step"]] <- "my_pretty_f2" # updates downstream 'depends'

pip
# <pipeflow> my-pipeline (4 steps)
# --------------------------------
#            step params      depends state
# 1:         init  xInit                new
# 2:           f1      x         init   new
# 3: my_pretty_f2      x           f1   new
# 4:           f3      x my_pretty_f2   new
# --------------------------------
# <ready> last run: never
```

``` r
pip[step %like% "f", "locked"] <- TRUE

pip
# <pipeflow> my-pipeline (4 steps)
# --------------------------------
#            step params      depends state locked
# 1:         init  xInit                new  FALSE
# 2:           f1      x         init   new   TRUE
# 3: my_pretty_f2      x           f1   new   TRUE
# 4:           f3      x my_pretty_f2   new   TRUE
# --------------------------------
# <ready> last run: never
```

An exception are the last three columns (`depends`, `unbound`,
`nodeId`): these are usually derived indirectly from the step definition
and therefore protected against direct assignment.

``` r
pip[["init", "nodeId"]] <- 99L
# Error in `[[<-.pipeflow`:
# ! direct assignment to column 'nodeId' is not supported.
```

Of course, you can even skip the `[<-` and `[[<-` operators and work
directly on the `data.table` object, but with an increased risk to
invalidate the internal consistency of the overall pipeline structure,
for example:

``` r
dat <- pip$pipenv$data
dat[2, "step"] <- "new f1 name" # fails to update downstream 'depends'
dat[3, "nodeId"] <- 99L # breaks link to internal DAG node
assign("data", dat, envir = pip$pipenv)

pip
# <pipeflow> my-pipeline (4 steps)
# --------------------------------
#            step params      depends state locked
# 1:         init  xInit                new  FALSE
# 2:  new f1 name      x         init   new   TRUE
# 3: my_pretty_f2      x           f1   new   TRUE
# 4:           f3      x my_pretty_f2   new   TRUE
# --------------------------------
# <ready> last run: never
```

For this reason, direct manipulation is useful during debugging and you
should mostly stick to the provided operators or functions unless you
really know what you are doing.

### Changing the pipeline structure at runtime

After this little excursion let’s next see how to safely modify our
pipeline structure at runtime.

``` r
pip <- pip_new("my-pipeline") |>
    pip_add("init", \(xInit = 0) xInit) |>
    pip_add("f1", \(x = ~init) x + 1) |>
    pip_add("f2", \(x = ~f1) x + 2) |>
    pip_add("f3", \(x = ~f2) x + 3)

(pip_run(pip))
# info [2026-09-27 15:20:07.182 UTC]: Starting run of pipeflow 'my-pipeline'
# info [2026-09-27 15:20:07.182 UTC]: Step 1/4 init
# info [2026-09-27 15:20:07.183 UTC]: Step 2/4 f1
# info [2026-09-27 15:20:07.185 UTC]: Step 3/4 f2
# info [2026-09-27 15:20:07.186 UTC]: Step 4/4 f3
# info [2026-09-27 15:20:07.187 UTC]: Finished run of pipeflow 'my-pipeline'
# <pipeflow> my-pipeline (4 steps)
# --------------------------------
#    step params depends state out
# 1: init  xInit          done   0
# 2:   f1      x    init  done   1
# 3:   f2      x      f1  done   3
# 4:   f3      x      f2  done   6
# --------------------------------
# <ready> last run: 2026-09-27 17:20:07
```

This pipeline just adds 1, 2, and 3 to the initial value, respectively.
Let’s modify step `f2` that in turn will modify `f3` at runtime based on
the interim result passed into `f2`.

#### Modify steps

``` r
pip |> pip_replace(
    "f2",
    \(x = ~f1) {
        if (x > 10) {
            .self$replace("f3", \(x = ~f1) x * 3)
            return(x / 2)
        }
        x + 2
    }
)
```

Basically, step `f2` now checks if the input is greater than 10, and if
so, it replaces step `f3` with a new step now referencing `f1` that
multiplies the input passed from `f1` by 3 and returns half of the
input.

To see this, let’s try it with an input of 15.

``` r
pip |>
    pip_set_params(list(xInit = 15)) |>
    pip_run()
# info [2026-09-27 15:20:07.298 UTC]: Starting run of pipeflow 'my-pipeline'
# info [2026-09-27 15:20:07.298 UTC]: Step 1/4 init
# info [2026-09-27 15:20:07.299 UTC]: Step 2/4 f1
# info [2026-09-27 15:20:07.300 UTC]: Step 3/4 f2
# info [2026-09-27 15:20:07.302 UTC]: Step 4/4 f3
# info [2026-09-27 15:20:07.303 UTC]: Finished run of pipeflow 'my-pipeline'

pip
# <pipeflow> my-pipeline (4 steps)
# --------------------------------
#    step params depends state out
# 1: init  xInit          done  15
# 2:   f1      x    init  done  16
# 3:   f2      x      f1  done   8
# 4:   f3      x      f1  done  48
# --------------------------------
# <ready> last run: 2026-09-27 17:20:07
```

We see that both the output of the pipeline and the dependencies of the
last step have changed. Let’s confirm by inspecting the function of the
last step.

``` r
pip[["f3", "fun"]]
# function (x = ~f1) 
# x * 3
# <environment: 0x556545b4f5f8>
```

#### Insert and remove steps

Next, we get even more hacky and instead of just replacing, we will go a
bit further to insert and remove steps. The pipeline definition is as
follows:

``` r
pip <- pip_new("hicky-hacky") |>
    pip_add("init", \(xInit = 0) xInit) |>
    pip_add("f1", \(x = ~init) x + 1) |>
    pip_add(
        "f2",
        \(x = ~f1) {
            if (x > 10) {
                .self |>
                    pip_add("f2a", \(x = ~f1) x + 21, after = "f1") |>
                    pip_add("f2b", \(x = ~f2a) x + 22, after = "f2a") |>
                    pip_replace("f3", \(x = ~f2b) x + 30) |>
                    pip_remove("f2")
            }
            x + 2
        }
    ) |>
    pip_add("f3", \(x = ~f2) x + 3)
```

If the input is greater than 10, we insert two new steps `f2a` and `f2b`
after `f1`, remove `f2`, and replace `f3` with a new step that adds 30
to the input. Let’s first run with the initial value of 0 to see the
original output.

``` r
pip_run(pip)
# info [2026-09-27 15:20:07.468 UTC]: Starting run of pipeflow 'hicky-hacky'
# info [2026-09-27 15:20:07.469 UTC]: Step 1/4 init
# info [2026-09-27 15:20:07.469 UTC]: Step 2/4 f1
# info [2026-09-27 15:20:07.470 UTC]: Step 3/4 f2
# info [2026-09-27 15:20:07.471 UTC]: Step 4/4 f3
# info [2026-09-27 15:20:07.472 UTC]: Finished run of pipeflow 'hicky-hacky'

pip
# <pipeflow> hicky-hacky (4 steps)
# --------------------------------
#    step params depends state out
# 1: init  xInit          done   0
# 2:   f1      x    init  done   1
# 3:   f2      x      f1  done   3
# 4:   f3      x      f2  done   6
# --------------------------------
# <ready> last run: 2026-09-27 17:20:07
```

Next, we set the initial value to 11 to trigger the changes.

``` r
pip |>
    pip_set_params(list(xInit = 11)) |>
    pip_run()
# info [2026-09-27 15:20:07.530 UTC]: Starting run of pipeflow 'hicky-hacky'
# info [2026-09-27 15:20:07.530 UTC]: Step 1/4 init
# info [2026-09-27 15:20:07.530 UTC]: Step 2/4 f1
# info [2026-09-27 15:20:07.538 UTC]: Step 3/4 f2
# info [2026-09-27 15:20:07.542 UTC]: Step 4/4 f3
# info [2026-09-27 15:20:07.543 UTC]: Finished run of pipeflow 'hicky-hacky'

pip
# <pipeflow> hicky-hacky (5 steps)
# --------------------------------
#    step params depends state    out
# 1: init  xInit          done     11
# 2:   f1      x    init  done     12
# 3:  f2a      x      f1   new [NULL]
# 4:  f2b      x     f2a  done       
# 5:   f3      x     f2b   new [NULL]
# --------------------------------
# <ready> last run: 2026-09-27 17:20:07
```

While the structure has changed as expected, some steps were not yet
run. In fact, since originally step `f3`came after `f2`, and in contrast
to what the log is showing, instead of step `f3`, actually the new step
`f2b` was run[²](#fn2) , albeit with x = NULL as input.

So to have the true results, we need to re-init the parameter and need
to re-run the pipeline.

``` r
pip |>
    pip_set_params(list(xInit = 11)) |>
    pip_run()
# info [2026-09-27 15:20:07.599 UTC]: Starting run of pipeflow 'hicky-hacky'
# info [2026-09-27 15:20:07.599 UTC]: Step 1/5 init
# info [2026-09-27 15:20:07.599 UTC]: Step 2/5 f1
# info [2026-09-27 15:20:07.600 UTC]: Step 3/5 f2a
# info [2026-09-27 15:20:07.601 UTC]: Step 4/5 f2b
# info [2026-09-27 15:20:07.602 UTC]: Step 5/5 f3
# info [2026-09-27 15:20:07.602 UTC]: Finished run of pipeflow 'hicky-hacky'

pip
# <pipeflow> hicky-hacky (5 steps)
# --------------------------------
#    step params depends state out
# 1: init  xInit          done  11
# 2:   f1      x    init  done  12
# 3:  f2a      x      f1  done  33
# 4:  f2b      x     f2a  done  55
# 5:   f3      x     f2b  done  85
# --------------------------------
# <ready> last run: 2026-09-27 17:20:07
```

Now the output of all steps is as expected. If we want to use {pipeflow}
in production, obviously, having to re-run the pipeline and temporarily
showing a wrong log is not ideal. That is, ideally, the pipeline run
would be aborted once away after all changes were made in `f2` and then
re-run right away from the beginning. Also, this process potentially
should be repeated recursively until the structure does not change
anymore.

### Invoke restart during run

Luckily, with some minimal changes, this behaviour can be achieved with
{pipeflow}. First, for any step where you want to restart the pipeline
run, you need to call `.self$restart()`, so we adapt the `f2` function
as follows:

``` r
pip <- pip_new("hacky-with-restart") |>
    pip_add("init", \(xInit = 0) xInit) |>
    pip_add("f1", \(x = ~init) x + 1) |>
    pip_add(
        "f2",
        \(x = ~f1) {
            if (x > 10) {
                .self |>
                    pip_add("f2a", \(x = ~f1) x + 21, after = "f1") |>
                    pip_add("f2b", \(x = ~f2a) x + 22, after = "f2a") |>
                    pip_replace("f3", \(x = ~f2b) x + 30) |>
                    pip_remove("f2")
                .self$restart() # <-- restart the run
            }
            x + 2
        }
    ) |>
    pip_add("f3", \(x = ~f2) x + 3)
```

Second, you just run the pipeline as usual.

``` r
pip |>
    pip_set_params(list(xInit = 11)) |>
    pip_run()
# info [2026-09-27 15:20:07.711 UTC]: Starting run of pipeflow 'hacky-with-restart'
# info [2026-09-27 15:20:07.711 UTC]: Step 1/4 init
# info [2026-09-27 15:20:07.711 UTC]: Step 2/4 f1
# info [2026-09-27 15:20:07.712 UTC]: Step 3/4 f2
# info [2026-09-27 15:20:07.716 UTC]: Restarting pipeline execution.
# info [2026-09-27 15:20:07.716 UTC]: Restarting run of pipeflow 'hacky-with-restart'
# info [2026-09-27 15:20:07.716 UTC]: Step 1/5 init
# info [2026-09-27 15:20:07.716 UTC]: Step 2/5 f1
# info [2026-09-27 15:20:07.717 UTC]: Step 3/5 f2a
# info [2026-09-27 15:20:07.718 UTC]: Step 4/5 f2b
# info [2026-09-27 15:20:07.719 UTC]: Step 5/5 f3
# info [2026-09-27 15:20:07.719 UTC]: Finished run of pipeflow 'hacky-with-restart'
```

As you can see, the run was aborted right after step `f2` and re-run
from the start based on the new structure. As a result, the log now is
fully aligned with the performed pipeline run.

Looking at the final pipeline overview, we see that the output matches
the expected output of the modified pipeline.

``` r
pip
# <pipeflow> hacky-with-restart (5 steps)
# ---------------------------------------
#    step params depends state out
# 1: init  xInit          done  11
# 2:   f1      x    init  done  12
# 3:  f2a      x      f1  done  33
# 4:  f2b      x     f2a  done  55
# 5:   f3      x     f2b  done  85
# ---------------------------------------
# <ready> last run: 2026-09-27 17:20:07
```

Of course, this was just a toy example to show some possibilities, but I
have made use of this feature already in various projects and may
present one of them in a more sophisticated example in the future.

Lastly note that since you have full access to the pipeline object, of
course, you can get even more hacky, but be aware that some additional
operations are done under the hood when steps are added or removed. It
is therefore not recommended to “manually” manipulate the internal
data.table object in terms of removing or adding rows, or changing
important columns such as `depends` or `nodeId` as this immediately
would invalidate the internal consistency of the dependency graph.

On the other hand, changing entries in columns such as `tags`, `time`,
`state` or `output` is generally not critical. If in doubt, just try and
see what works.

------------------------------------------------------------------------

1.  By default, [`ls()`](https://rdrr.io/r/base/ls.html) hides names
    starting with a dot. The internal entries, which become visible via
    `ls(pip$pipenv, all.names = TRUE)`, are not considered in this
    vignette.

2.  See the state of `f2b` in row 4: it is set to `done`.
