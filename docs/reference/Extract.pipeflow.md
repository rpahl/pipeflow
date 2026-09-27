# Extract or replace parts of a pipeline

A pipeline can be subset, read from and written to with the usual
extract and replace operators, treating it as a table of steps. The
extraction operators mirror base R, and the replacement operators update
steps (or their properties) in place.

## Usage

``` r
# S3 method for class 'pipeflow'
x[i, j, view = TRUE]

# S3 method for class 'pipeflow'
x[[i, j = NULL]]

# S3 method for class 'pipeflow'
x[[i, j]] <- value

# S3 method for class 'pipeflow'
x[i, j] <- value

# S3 method for class 'pipeflow'
x$i

# S3 method for class 'pipeflow'
x$i <- value
```

## Arguments

- x:

  A pipeflow pipeline or view object.

- i:

  The row or field selector. For `[` and `[<-`, the rows to select:
  integer row indices, character step names, or a boolean filter
  expression evaluated in the context of the step table. For `[[`, a
  step-table column name, a meta field, or — in the two-index form — a
  step name or integer row index. For `[[<-`, a step name, an integer
  row index, or one of the meta fields `"name"` and `"view"`. For a
  view, indices are relative to the covered steps and step names must be
  part of the view.

- j:

  For `[`, an optional character vector of step-table column names to
  extract. For `[<-`, the single step property to assign. For `[[` and
  `[[<-`, the column or step property to extract or assign.

- view:

  For `[`, if `TRUE` (default), a view referencing the selected steps is
  returned. If `FALSE`, a new pipeline is returned that includes the
  selected steps and all their upstream dependencies. Ignored when `j`
  is provided.

- value:

  The value to assign (`[[<-`, `[<-`).

## Value

For `[`, a pipeflow view (if `view = TRUE`) or a new pipeflow pipeline
(if `view = FALSE`); if `j` is provided, a `data.table` with the
selected rows and columns. For `[[`, the extracted step-table column,
single cell, or meta field. For the assignment forms, the updated
pipeline, invisibly.

## Details

### Extract or subset a pipeline (`x[i, j, view = TRUE]`)

`p[...]` selects steps from a pipeline. By default, a lightweight
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
is returned that references the selected steps without copying them. Set
`view = FALSE` to instead get a new, self-contained pipeline that
includes all required upstream dependencies.

Three forms of row selection are supported:

- **row indices** (relative to the current selection), e.g. `p[2:3]`,

- **step names**, e.g. `p[c("load", "fit")]`,

- a **boolean filter** that is evaluated in the context of the
  pipeline's step table, e.g. `p[state == "new"]` or
  `p[step %in% c("load", "fit") & tags %like% "io"]`.

Boolean filters may reference the columns of the step table (`step`,
`fun`, `params`, `depends`, `tags`, `state`, ...) directly as variables,
and the [data.table filter
operators](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
re-exported by pipeflow (e.g. `%like%`, `%chin%`, `%between%`) are
available. `p[]` returns a copy of the pipeline.

When `j` is provided, the selected steps are returned as a step-table
extraction rather than a pipeline. `j` must be a character vector of
column names: `p[, j]` keeps those columns for all steps and `p[i, j]`
first selects the rows and then keeps those columns. The result in this
case is a `data.table`.

If done on a view, row selection is relative to the covered steps, and
step names must be part of the view. For validated, programmatic filters
with dedicated arguments (such as matching *any* of a set of tags) use
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
instead.

### Extract values from a pipeline or view (`x[[i, j]]`)

A pipeline can be read like a data.frame of steps: `p[[column]]` returns
a column of the step table, and `p[[row, column]]` extracts a single
cell. In addition, a few meta fields are accessible by name.

#### Meta fields

The following meta fields are available via `p[["..."]]` (or `p$...`):

- `name` – the name of the pipeline.

- `view` – the absolute row indices of the steps covered by a view, or
  `NULL` for a full pipeline.

- `pipenv` – the shared inner environment holding the pipeline's state.
  All views and extracted subsets reference the same environment, so
  mutations are shared. The step table is available as
  `p[["pipenv"]][["data"]]` (see also
  [`pip_data()`](https://github.com/rpahl/pipeflow/reference/pip_data.md)).

#### Virtual methods

All pipeline functions are exposed as *virtual* methods via `p$...` (or
`p[["..."]]`). For example, `p$add(...)` is shorthand for
`pip_add(p, ...)` and returns the updated pipeline.

#### Step-table columns

`p[["column"]]` returns the raw values of a column of the step table,
named by the step names. For views, the column is restricted to the
steps covered by the view. The two-index form `p[[row, column]]`
extracts a single cell, where `row` is an integer row index or a step
name.

### Assign values to meta fields or step properties (`x[[i, j]] <- value`)

#### Assigning meta fields

- `p[["name"]] <- value` – sets the name of the pipeline

- `p[["view"]] <- steps` – allows to define the pipeline view explicitly
  via a character vector of step names, which is equivalent to
  `p[steps]` or `p[steps, ]`. Assigning `p[["view"]] <- NULL` clears the
  view and returns a full pipeline.

#### Assigning step properties

The two-index form `p[[step, property]] <- value` provides interactive
shortcuts for modifying a single step, where `step` is a step name or an
integer row index and `property` selects what to update:

- `p[[step, "step"]] <- newName` – rename the step; same as
  `pip_rename(p, step, newName)`).

  - `p[[step, "step"]] <- NULL` – remove the step, together with its
    downstream dependencies; same as `pip_remove(p, step, force = TRUE)`

- `p[[step, "fun"]] <- fun` – replace the step's function; same as
  [`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md))
  but tags and execution mode are kept

- `p[[step, "params"]] <- list(...)` – update the step's parameters;
  same as `pip_set_params(p, list(...))`

- `p[[step, "tags"]] <- tags` – set the step's tags explicitly to `tags`
  (a character vector); in contrast,
  [`pip_tag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md)
  will just add tags.

  - `p[[step, "tags"]] <- NULL` clears all tags

- `p[[step, "locked"]] <- TRUE|FALSE` – lock or unlock the step; same as
  [`pip_lock()`](https://github.com/rpahl/pipeflow/reference/pip_lock.md)
  /
  [`pip_unlock()`](https://github.com/rpahl/pipeflow/reference/pip_lock.md)

- `p[[step, "exec"]] <- mode` – set the step's execution mode.

- `p[[step, "state"]] <- state` – set the step's state.

- `p[[step, "time"]] <- time` – set the step's time stamp (a single
  `POSIXct` value).

- `p[[step, "out"]] <- value` – set the step's stored output (mostly
  useful for debugging)

Assigning to any other step-table column, or to a meta field other than
`name` and `view`, is not supported.

Views are supported as well. For a view, `i` is interpreted relative to
the steps covered by the view: an integer refers to the n-th visible
step, and a step name must be part of the view. Because views share the
pipeline environment, step properties are written through to the
originating pipeline, while `name` only renames the view itself.

### Bulk assignment of step properties (`x[i, j] <- value`)

`p[i, j] <- value` assigns the step property `j` to the selected steps
(see `[[<-.pipeflow` above for the supported properties), mirroring the
row and column selection of the extraction form `p[i, j]`. `i` selects
rows like in the extraction form (including negative row indices, which
select all rows but the excluded ones) and `j` must be a single property
name; `p[, j] <- value` selects all steps. With more than one selected
row, a value of length 1 is replicated to all selected rows and a value
whose length equals the number of selected rows is assigned element-wise
(`p[i, j] <- value` behaves like `p[[i[k], j]] <- value[[k]]`). As in
base R, longer values are recycled if the number of selected rows is a
multiple of the value length, otherwise an error is raised. To assign
the same list value (e.g. a set of tags) to all rows, wrap it in
`list(...)`. With a single selected row, `value` is stored as-is (like
`p[[i, j]] <- value`). For the `params` property, the value of each row
must itself be a list (e.g.
`p[1:2, "params"] <- list(list(a = 1), list(b = 2))`).

If `value` is a `pipeflow` pipeline or view, the writable step
properties of its steps are copied to the selected rows:
`p[i, ] <- q[i, ]` copies the steps of `q` selected by `i` into the
steps of `p` selected by `i` (the rows of both sides are aligned by
position and must have the same length). In this form, `i` is required
and `j` must be omitted. The properties are copied in the order `step`,
`fun`, `params`, `out`, `state`, `time`, `tags`, `locked`, `exec`: the
step is renamed to the source name (a no-op when it is unchanged), the
function is replaced with the source function (same as
[`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md);
its dependencies are recomputed against the steps of `p`), the unbound
parameters of the source are applied on top of the new function
defaults, and the runtime state and annotation columns are overwritten
(locked steps included). The structural columns `nodeId`, `depends` and
`unbound` are not copied. Instead of `p[i, j] <- q`, use
`p[i, j] <- q[i, j]` to copy the values of a single property; the
one-column table returned by the extraction form is unwrapped and
assigned element-wise.

### Extract and assign via \$ (`x$i / x$i <- value`)

`$` is a shortcut for the one-index form of `[[` / `[[<-`: `p$name` is
equivalent to `p[["name"]]`, and `p$name <- value` to
`p[["name"]] <- value`. This works for meta fields, step-table columns,
and the virtual methods (e.g. `p$add(...)` or `p$run()`).

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 5) seq_len(n), tags = c("io", "daily")) |>
  pip_add("square", \(x = ~load) x^2, tags = "model") |>
  pip_add("total", \(x = ~square) sum(x), tags = c("model", "report"))

# By default, `[` returns a view into the selected steps.
p["total"] # view with a single step
#> <pipeflow_view> pipe view (1 of 3 steps)
#> ----------------------------------------
#>     step params depends state         tags
#> 1: total      x  square   new model,report
#> ----------------------------------------
#> <ready> last run: never

# Select by name vector or integer row index
p[c("load", "square")][["step"]]   # view -> "load", "square"
#>     load   square 
#>   "load" "square" 
p[1:2, view = FALSE][["step"]]     # pipeline -> "load", "square"
#>     load   square 
#>   "load" "square" 

# Boolean filters are evaluated against the step table
p[tags %like% "model"][["step"]]  # "square", "total"
#>   square    total 
#> "square"  "total" 

# view = FALSE extracts a pipeline with all upstream dependencies
p[tags %like% "report", view = FALSE][["step"]] # "load", "square", "total"
#> pulled in 2 upstream dependencies
#>     load   square    total 
#>   "load" "square"  "total" 

# No arguments returns a copy of the pipeline
p2 <- p[]
pip_run(p2)
#> info [2026-09-27 17:53:04.693 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:04.693 UTC]: Step 1/3 load
#> info [2026-09-27 17:53:04.694 UTC]: Step 2/3 square
#> info [2026-09-27 17:53:04.699 UTC]: Step 3/3 total
#> info [2026-09-27 17:53:04.700 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (3 steps)
#> -------------------------
#>      step params depends state         tags
#> 1:   load      n           new     io,daily
#> 2: square      x    load   new        model
#> 3:  total      x  square   new model,report
#> -------------------------
#> <ready> last run: never
p2
#> <pipeflow> pipe (3 steps)
#> -------------------------
#>      step params depends state            out         tags
#> 1:   load      n          done      1,2,3,4,5     io,daily
#> 2: square      x    load  done  1, 4, 9,16,25        model
#> 3:  total      x  square  done             55 model,report
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:04

# Two-index extraction selects step-table columns by name
p[, "step"]                     # one-column data.table
#>      step
#>    <char>
#> 1:   load
#> 2: square
#> 3:  total
p[c("load", "square"), "out"]   # selected rows, one column
#>       out
#>    <list>
#> 1: [NULL]
#> 2: [NULL]

# Works with views - selection is relative to the covered steps
v <- pip_view(p, tags = "model")
v[1L, "out"]
#>       out
#>    <list>
#> 1: [NULL]
v[, "step"]
#>      step
#>    <char>
#> 1: square
#> 2:  total
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)
pip_run(p)
#> info [2026-09-27 17:53:04.716 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:04.716 UTC]: Step 1/2 load
#> info [2026-09-27 17:53:04.716 UTC]: Step 2/2 fit
#> info [2026-09-27 17:53:04.717 UTC]: Finished run of pipeflow 'pipe'

# Meta fields
p[["name"]]              # "pipe"
#> [1] "pipe"
p[["view"]]              # NULL - not a view
#> NULL
p[["pipenv"]]            # the inner pipeline environment
#> <environment: 0x559e078da340>
p[["pipenv"]][["data"]]  # the underlying step table
#> Indices: <step>, <nodeId>
#>      step           fun    params    out  state   tags locked   exec
#>    <char>        <list>    <list> <list> <char> <list> <lgcl> <char>
#> 1:   load <function[1]> <list[1]>      1   done         FALSE   auto
#> 2:    fit <function[1]> <list[1]>      2   done         FALSE   auto
#>                   time depends unbound nodeId
#>                 <POSc>  <list>  <list>  <int>
#> 1: 2026-09-27 19:53:04               x      0
#> 2: 2026-09-27 19:53:04    load              1

# Virtual methods
p$add("s3", \(x = ~fit) x * 10)
p$run()
#> info [2026-09-27 17:53:04.723 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:04.723 UTC]: Step 1/3 load - skipping done step
#> info [2026-09-27 17:53:04.723 UTC]: Step 2/3 fit - skipping done step
#> info [2026-09-27 17:53:04.723 UTC]: Step 3/3 s3
#> info [2026-09-27 17:53:04.724 UTC]: Finished run of pipeflow 'pipe'
p$restart   # a function; call p$restart() to request a restart
#> function (...) 
#> fn(x, ...)
#> <bytecode: 0x559e09bff290>
#> <environment: 0x559e06238500>
p$halt      # a function; call p$halt() to halt the current run
#> function (...) 
#> fn(x, ...)
#> <bytecode: 0x559e09bff290>
#> <environment: 0x559e06024b68>

# Column access, named by steps
p[["step"]]   # c(load = "load", fit = "fit")
#>   load    fit     s3 
#> "load"  "fit"   "s3" 
p[["out"]]    # c(load = 1, fit = 2)
#> $load
#> [1] 1
#> 
#> $fit
#> [1] 2
#> 
#> $s3
#> [1] 20
#> 

# Single cell: p[[row, column]]
p[["fit", "depends"]]    # "load"
#>      x 
#> "load" 
p[[2, "state"]]          # state of the second step
#> [1] "done"

# Views behave analogously:
v <- pip_view(p, step = c("load", "fit"))
v[["name"]]              # "pipe view"
#> [1] "pipe view"
v[["view"]]              # row indices of the covered steps
#> [1] 1 2

v[["step"]]              # c(load = "load", fit = "fit")
#>   load    fit 
#> "load"  "fit" 
v[["out"]]               # c(load = 1, fit = 2)
#> $load
#> [1] 1
#> 
#> $fit
#> [1] 2
#> 
v[["fit", "out"]]        # output of the "fit" step
#> [1] 2
p <- pip_new("pipe") |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load, k = 2) x * k) |>
  pip_add("report", \(x = ~fit) x)

# Assign the pipeline name via its meta field
p[["name"]] <- "demo"

# Replace a step's function (tags and exec mode are kept)
p[["fit", "fun"]] <- \(x = ~load, k = 2) x * k

# Update parameters, tags, locking and the execution mode
p[["fit", "params"]] <- list(k = 5)
p[["fit", "tags"]] <- c("model", "daily")
p[["fit", "locked"]] <- TRUE
p[["fit", "locked"]] <- FALSE
p[["fit", "exec"]] <- "plain"

# Rename a step; dependent steps are updated as well
p[["load", "step"]] <- "read"

# Assign by row index to set the state, time stamp or stored output
p[[2, "state"]] <- "outdated"
p[[2, "time"]] <- Sys.time() - 3600
p[[2, "out"]] <- 42
p
#> <pipeflow> demo (3 steps)
#> -------------------------
#>      step params depends    state    out        tags  exec
#> 1:   read      x              new [NULL]              auto
#> 2:    fit    x,k    read outdated     42 model,daily plain
#> 3: report      x     fit outdated [NULL]              auto
#> -------------------------
#> <ready> last run: never

# Restrict to a view covering selected steps, then clear it again
p[["view"]] <- c("read", "fit")
p[["step"]]                  # view -> "read", "fit"
#>   read    fit 
#> "read"  "fit" 
p[["view"]] <- NULL
p[["step"]]                  # full pipeline again
#>     read      fit   report 
#>   "read"    "fit" "report" 
p <- pip_new("pipe") |>
  pip_add("load", \(n = 5) seq_len(n)) |>
  pip_add("fit", \(x = ~load, k = 2) x * k) |>
  pip_add("report", \(x = ~fit) x)

# Assign a property element-wise or replicated to selected steps
p[c("load", "fit"), "state"] <- c("outdated", "done")
p[1:2, "tags"] <- c("io", "model")
p[1:2, "tags"] <- list(c("new", "tag"))  # same tags for both steps

# All steps at once; read-only columns are rejected
p[, "state"] <- "outdated"
try(p[c("load", "fit"), "depends"] <- "load")  # read-only
#> Error in `[[<-.pipeflow`(`*tmp*`, iSel[[k]], j, value = "load") : 
#>   direct assignment to column 'depends' is not supported.

# Copy the writable properties from another pipeline
q <- pip_clone(p)
q[c("load", "fit"), "out"] <- list(1:5, 6:10)
p[c("load", "fit"), ] <- q[c("load", "fit"), ]
p[c("load", "fit"), "tags"] <- q[c("load", "fit"), "tags"]
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)

p$name                # same as p[["name"]]
#> [1] "pipe"
p$step                # same as p[["step"]]
#>   load    fit 
#> "load"  "fit" 
p$name <- "renamed"   # same as p[["name"]] <- "renamed"

p$run                 # a virtual method; call p$run() to run the pipeline
#> function (...) 
#> fn(x, ...)
#> <bytecode: 0x559e09bff290>
#> <environment: 0x559e09d8afa0>
```
