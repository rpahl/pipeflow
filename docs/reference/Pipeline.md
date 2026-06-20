# Pipeline Class

This class implements an analysis pipeline. A pipeline consists of a
sequence of analysis steps, which can be added one by one. Each added
step may or may not depend on one or more previous steps. The pipeline
keeps track of the dependencies among these steps and will ensure that
all dependencies are met on creation of the pipeline, that is, before
the the pipeline is run. Once the pipeline is run, the output is stored
in the pipeline along with each step and can be accessed later.
Different pipelines can be bound together while preserving all
dependencies within each pipeline.

## Lifecycle

Deprecated. Legacy R6 interface. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and pip\_\* functions.

## Author

Roman Pahl

## Public fields

- `name`:

  `string` name of the pipeline

- `pipeline`:

  `data.table` the pipeline where each row represents one step.

## Methods

### Public methods

- [`Pipeline$new()`](#method-Pipeline-initialize)

- [`Pipeline$add()`](#method-Pipeline-add)

- [`Pipeline$append()`](#method-Pipeline-append)

- [`Pipeline$append_to_step_names()`](#method-Pipeline-append_to_step_names)

- [`Pipeline$collect_out()`](#method-Pipeline-collect_out)

- [`Pipeline$discard_steps()`](#method-Pipeline-discard_steps)

- [`Pipeline$get_data()`](#method-Pipeline-get_data)

- [`Pipeline$get_depends()`](#method-Pipeline-get_depends)

- [`Pipeline$get_depends_down()`](#method-Pipeline-get_depends_down)

- [`Pipeline$get_depends_up()`](#method-Pipeline-get_depends_up)

- [`Pipeline$get_graph()`](#method-Pipeline-get_graph)

- [`Pipeline$get_out()`](#method-Pipeline-get_out)

- [`Pipeline$get_params()`](#method-Pipeline-get_params)

- [`Pipeline$get_params_at_step()`](#method-Pipeline-get_params_at_step)

- [`Pipeline$get_params_unique()`](#method-Pipeline-get_params_unique)

- [`Pipeline$get_params_unique_json()`](#method-Pipeline-get_params_unique_json)

- [`Pipeline$get_step()`](#method-Pipeline-get_step)

- [`Pipeline$get_step_names()`](#method-Pipeline-get_step_names)

- [`Pipeline$get_step_number()`](#method-Pipeline-get_step_number)

- [`Pipeline$has_step()`](#method-Pipeline-has_step)

- [`Pipeline$insert_after()`](#method-Pipeline-insert_after)

- [`Pipeline$insert_before()`](#method-Pipeline-insert_before)

- [`Pipeline$length()`](#method-Pipeline-length)

- [`Pipeline$lock_step()`](#method-Pipeline-lock_step)

- [`Pipeline$pop_step()`](#method-Pipeline-pop_step)

- [`Pipeline$pop_steps_after()`](#method-Pipeline-pop_steps_after)

- [`Pipeline$pop_steps_from()`](#method-Pipeline-pop_steps_from)

- [`Pipeline$print()`](#method-Pipeline-print)

- [`Pipeline$remove_step()`](#method-Pipeline-remove_step)

- [`Pipeline$rename_step()`](#method-Pipeline-rename_step)

- [`Pipeline$replace_step()`](#method-Pipeline-replace_step)

- [`Pipeline$reset()`](#method-Pipeline-reset)

- [`Pipeline$run()`](#method-Pipeline-run)

- [`Pipeline$run_step()`](#method-Pipeline-run_step)

- [`Pipeline$set_data()`](#method-Pipeline-set_data)

- [`Pipeline$set_data_split()`](#method-Pipeline-set_data_split)

- [`Pipeline$set_keep_out()`](#method-Pipeline-set_keep_out)

- [`Pipeline$set_params()`](#method-Pipeline-set_params)

- [`Pipeline$set_params_at_step()`](#method-Pipeline-set_params_at_step)

- [`Pipeline$split()`](#method-Pipeline-split)

- [`Pipeline$unlock_step()`](#method-Pipeline-unlock_step)

- [`Pipeline$clone()`](#method-Pipeline-clone)

------------------------------------------------------------------------

### `Pipeline$new()`

constructor

#### Usage

    Pipeline$new(name, data = NULL, logger = NULL)

#### Arguments

- `name`:

  the name of the Pipeline

- `data`:

  optional data used at the start of the pipeline. The data also can be
  set later using the `set_data` function.

- `logger`:

  custom logger to be used for logging. If no logger is provided, the
  default logger is used, which should be sufficient for most use cases.
  If you do want to use your own custom log function, you need to
  provide a function that obeys the following form:

  `function(level, msg, ...) { your custom logging code here }`

  The `level` argument is a string and will be one of `info`, `warn`, or
  `error`. The `msg` argument is a string containing the message to be
  logged. The `...` argument is a list of named parameters, which can be
  used to add additional information to the log message. Currently, this
  is only used to add the context in case of a step giving a warning or
  error.

  Note that with the default logger, the log layout can be altered any
  time via
  [`set_log_layout()`](https://github.com/rpahl/pipeflow/reference/set_log_layout.md).

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("myPipe", data = data.frame(x = 1:8))
    p

    # Passing custom logger
    my_logger <- function(level, msg, ...) {
       cat(level, msg, "\n")
    }
    p <- Pipeline$new("myPipe", logger = my_logger)

------------------------------------------------------------------------

### `Pipeline$add()`

Add pipeline step

#### Usage

    Pipeline$add(
      step,
      fun,
      params = list(),
      description = "",
      group = step,
      keepOut = FALSE
    )

#### Arguments

- `step`:

  `string` the name of the step. Each step name must be unique.

- `fun`:

  `function` or name of the function to be applied at the step. Both
  existing and anonymous/lambda functions can be used. All function
  parameters must have default values. If a parameter is missing a
  default value in the function signature, alternatively, it can be set
  via the `params` argument (see Examples section with
  [`mean()`](https://rdrr.io/r/base/mean.html) function).

- `params`:

  `list` list of parameters to set or overwrite parameters of the passed
  function.

- `description`:

  `string` optional description of the step

- `group`:

  `string` output collected after pipeline execution (see function
  `collect_out`) is grouped by the defined group names. By default, this
  is the name of the step, which comes in handy when the pipeline is
  copy-appended multiple times to keep the results of the same
  function/step grouped at one place.

- `keepOut`:

  `logical` if `FALSE` (default) the output of the step is not collected
  when calling `collect_out` after the pipeline run. This option is used
  to only keep the results that matter and skip intermediate results
  that are not needed. See also function `collect_out` for more details.

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    # Add steps with lambda functions
    p <- Pipeline$new("myPipe", data = 1)
    p$add("s1", \(x = ~data) 2*x)  # use input data
    p$add("s2", \(x = ~data, y = ~s1) x * y)
    try(p$add("s2", \(z = 3) 3)) # error: step 's2' exists already
    try(p$add("s3", \(z = ~foo) 3)) # dependency 'foo' not found
    p

    # Add step with existing function
    p <- Pipeline$new("myPipe", data = c(1, 2, NA, 3, 4))
    p$add("calc_mean", mean, params = list(x = ~data, na.rm = TRUE))
    p$run()$get_out("calc_mean")

    # Step description
    p <- Pipeline$new("myPipe", data = 1:10)
    p$add("s1", \(x = ~data) 2*x, description = "multiply by 2")
    print(p)
    print(p, verbose = TRUE) # print all columns

    # Group output
    p <- Pipeline$new("myPipe", data = data.frame(x = 1:5, y = 1:5))
    p$add("prep_x", \(data = ~data) data$x, group = "prep")
    p$add("prep_y", \(data = ~data) (data$y)^2, group = "prep")
    p$add("sum", \(x = ~prep_x, y = ~prep_y) x + y)
    p$run()$collect_out(all = TRUE)

------------------------------------------------------------------------

### `Pipeline$append()`

Append another pipeline When appending, `pipeflow` takes care of
potential name clashes with respect to step names and dependencies, that
is, if needed, it will automatically adapt step names and dependencies
to make sure they are unique in the merged pipeline.

#### Usage

    Pipeline$append(p, outAsIn = FALSE, tryAutofixNames = TRUE, sep = ".")

#### Arguments

- `p`:

  `Pipeline` object to be appended.

- `outAsIn`:

  `logical` if `TRUE`, output of first pipeline is used as input for the
  second pipeline.

- `tryAutofixNames`:

  `logical` if `TRUE`, name clashes are tried to be automatically
  resolved by appending the 2nd pipeline's name. Only set to `FALSE`, if
  you know what you are doing.

- `sep`:

  `string` separator used when auto-resolving step names

#### Returns

returns new combined `Pipeline`.

#### Examples

    # Append pipeline
    p1 <- Pipeline$new("pipe1")
    p1$add("step1", \(x = 1) x)
    p2 <- Pipeline$new("pipe2")
    p2$add("step2", \(y = 1) y)
    p1$append(p2)

    # Append pipeline with potential name clashes
    p3 <- Pipeline$new("pipe3")
    p3$add("step1", \(z = 1) z)
    p1$append(p2)$append(p3)

    # Use output of first pipeline as input for second pipeline
    p1 <- Pipeline$new("pipe1", data = 8)
    p2 <- Pipeline$new("pipe2")
    p1$add("square", \(x = ~data) x^2)
    p2$add("log2", \(x = ~data) log2(x))

    p12 <- p1$append(p2, outAsIn = TRUE)
    p12$run()$get_out("log2")
    p12

    # Custom name separator
    p1$append(p2, sep = "___")

------------------------------------------------------------------------

### `Pipeline$append_to_step_names()`

Appends string to all step names and takes care of updating step
dependencies accordingly.

#### Usage

    Pipeline$append_to_step_names(postfix, sep = ".")

#### Arguments

- `postfix`:

  `string` to be appended to each step name.

- `sep`:

  `string` separator between step name and postfix.

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe")
    p$add("step1", \(x = 1) x)
    p$add("step2", \(y = 1) y)
    p$append_to_step_names("new")
    p
    p$append_to_step_names("foo", sep = "__")
    p

------------------------------------------------------------------------

### `Pipeline$collect_out()`

Collect output afer pipeline run, by default, from all steps for which
`keepOut` was set to `TRUE`. The output is grouped by the group names
(see `group` parameter in function `add`), which by default are set
identical to the step names.

#### Usage

    Pipeline$collect_out(groupBy = "group", all = FALSE)

#### Arguments

- `groupBy`:

  `string` column of pipeline by which to group the output.

- `all`:

  `logical` if `TRUE` all output is collected regardless of the
  `keepOut` flag. This can be useful for debugging.

#### Returns

`list` containing the output, named after the groups, which, by default,
are the steps.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("step1", \(x = ~data) x + 2)
    p$add("step2", \(x = ~step1) x + 2, keepOut = TRUE)
    p$run()
    p$collect_out()
    p$collect_out(all = TRUE) |> str()

    # Grouped output
    p <- Pipeline$new("pipe", data = 1:2)
    p$add("step1", \(x = ~data) x + 2, group = "add")
    p$add("step2", \(x = ~step1, y = 2) x + y, group = "add")
    p$add("step3", \(x = ~data) x * 3, group = "mult")
    p$add("step4", \(x = ~data, y = 2) x * y, group = "mult")
    p
    p$run()
    p$collect_out(all = TRUE) |> str()

    # Grouped by state
    p$set_params(list(y = 5))
    p
    p$collect_out(groupBy = "state", all = TRUE) |> str()

------------------------------------------------------------------------

### `Pipeline$discard_steps()`

Discard all steps that match a given `pattern`.

#### Usage

    Pipeline$discard_steps(pattern, recursive = FALSE, fixed = TRUE, ...)

#### Arguments

- `pattern`:

  `string` containing a regular expression (or character string for
  `fixed = TRUE`) to be matched.

- `recursive`:

  `logical` if `TRUE` the step is removed together with all its
  downstream dependencies.

- `fixed`:

  `logical` If `TRUE`, `pattern` is a string to be matched as is.
  Overrides all conflicting arguments.

- `...`:

  further arguments passed to
  [`grep()`](https://rdrr.io/r/base/grep.html).

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(x = ~data) x + 1)
    p$add("add2", \(x = ~add1) x + 2)
    p$add("mult3", \(x = ~add1) x * 3)
    p$add("mult4", \(x = ~add2) x * 4)
    p

    p$discard_steps("mult")
    p

    # Re-add steps
    p$add("mult3", \(x = ~add1) x * 3)
    p$add("mult4", \(x = ~add2) x * 4)
    p
    # Discarding 'add1' does not work ...
    try(p$discard_steps("add1"))

    # ... unless we enforce to remove its downstream dependencies as well
    p$discard_steps("add1", recursive = TRUE)   # this works
    p

    # Trying to discard non-existent steps is just ignored
    p$discard_steps("non-existent")

------------------------------------------------------------------------

### `Pipeline$get_data()`

Get data

#### Usage

    Pipeline$get_data()

#### Returns

the output defined in the `data` step, which by default is the first
step of the pipeline

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$get_data()
    p$set_data(3:4)
    p$get_data()

------------------------------------------------------------------------

### `Pipeline$get_depends()`

Get all dependencies defined in the pipeline

#### Usage

    Pipeline$get_depends()

#### Returns

named `list` of dependencies for each step

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(x = ~data) x + 1)
    p$add("add2", \(x = ~data, y = ~add1) x + y)
    p$get_depends()

------------------------------------------------------------------------

### `Pipeline$get_depends_down()`

Get all downstream dependencies of given step, by default descending
recursively.

#### Usage

    Pipeline$get_depends_down(step, recursive = TRUE)

#### Arguments

- `step`:

  `string` name of step

- `recursive`:

  `logical` if `TRUE`, dependencies of dependencies are also returned.

#### Returns

`list` of downstream dependencies

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(x = ~data) x + 1)
    p$add("add2", \(x = ~data, y = ~add1) x + y)
    p$add("mult3", \(x = ~add1) x * 3)
    p$add("mult4", \(x = ~add2) x * 4)
    p$get_depends_down("add1")
    p$get_depends_down("add1", recursive = FALSE)

------------------------------------------------------------------------

### `Pipeline$get_depends_up()`

Get all upstream dependencies of given step, by default descending
recursively.

#### Usage

    Pipeline$get_depends_up(step, recursive = TRUE)

#### Arguments

- `step`:

  `string` name of step

- `recursive`:

  `logical` if `TRUE`, dependencies of dependencies are also returned.

#### Returns

`list` of upstream dependencies

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(x = ~data) x + 1)
    p$add("add2", \(x = ~data, y = ~add1) x + y)
    p$add("mult3", \(x = ~add1) x * 3)
    p$add("mult4", \(x = ~add2) x * 4)
    p$get_depends_up("mult4")
    p$get_depends_up("mult4", recursive = FALSE)

------------------------------------------------------------------------

### `Pipeline$get_graph()`

Visualize the pipeline as a graph.

#### Usage

    Pipeline$get_graph(groups = NULL)

#### Arguments

- `groups`:

  `character` if not `NULL`, only steps belonging to the given groups
  are considered.

#### Returns

two data frames, one for nodes and one for edges ready to be used with
the `visNetwork` package.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = ~add1) x + y)
    p$add("mult1", \(x = ~add1, y = ~add2) x * y)
    graph <- pipe_get_graph(p)
    graph

    if (require("visNetwork", quietly = TRUE)) {
        do.call(visNetwork, args = p$get_graph())
    }

------------------------------------------------------------------------

### `Pipeline$get_out()`

Get output of given step

#### Usage

    Pipeline$get_out(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

the output at the given step.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(x = ~data) x + 1)
    p$add("add2", \(x = ~data, y = ~add1) x + y)
    p$run()
    p$get_out("add1")
    p$get_out("add2")

------------------------------------------------------------------------

### `Pipeline$get_params()`

Set unbound function parameters defined in the pipeline where 'unbound'
means parameters that are not linked to other steps. Trying \#' to set
parameters that don't exist in the pipeline is ignored, by default, with
a warning.

#### Usage

    Pipeline$get_params(ignoreHidden = TRUE)

#### Arguments

- `ignoreHidden`:

  `logical` if TRUE, hidden parameters (i.e. all names starting with a
  dot) are ignored and thus not returned.

#### Returns

`list` of parameters, sorted and named by step. Steps with no parameters
are filtered out.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
    p$add("add3", \() 1 + 2)
    p$get_params() |> str()
    p$get_params(ignoreHidden = FALSE) |> str()

------------------------------------------------------------------------

### `Pipeline$get_params_at_step()`

Get all unbound (i.e. not referring to other steps) at given step name.

#### Usage

    Pipeline$get_params_at_step(step, ignoreHidden = TRUE)

#### Arguments

- `step`:

  `string` name of step

- `ignoreHidden`:

  `logical` if TRUE, hidden parameters (i.e. all names starting with a
  dot) are ignored and thus not returned.

#### Returns

`list` of parameters defined at given step.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
    p$add("add3", \() 1 + 2)
    p$get_params_at_step("add2")
    p$get_params_at_step("add2", ignoreHidden = FALSE)
    p$get_params_at_step("add3")

------------------------------------------------------------------------

### `Pipeline$get_params_unique()`

Get all unbound (i.e. not referring to other steps) parameters defined
in the pipeline, but only list each parameter once. The values of the
parameters, will be the values of the first step where the parameter was
defined. This is particularly useful after the parameters where set
using the `set_params` function, which will set the same value for all
steps.

#### Usage

    Pipeline$get_params_unique(ignoreHidden = TRUE)

#### Arguments

- `ignoreHidden`:

  `logical` if TRUE, hidden parameters (i.e. all names starting with a
  dot) are ignored and thus not returned.

#### Returns

`list` of unique parameters

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
    p$add("mult1", \(x = 1, y = 2, .z = 3, b = ~add2) x * y * b)
    p$get_params_unique()
    p$get_params_unique(ignoreHidden = FALSE)

------------------------------------------------------------------------

### `Pipeline$get_params_unique_json()`

Get all unique function parameters in json format.

#### Usage

    Pipeline$get_params_unique_json(ignoreHidden = TRUE)

#### Arguments

- `ignoreHidden`:

  `logical` if TRUE, hidden parameters (i.e. all names starting with a
  dot) are ignored and thus not returned.

#### Returns

`list` flat unnamed json list of unique function parameters

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
    p$add("mult1", \(x = 1, y = 2, .z = 3, b = ~add2) x * y * b)
    p$get_params_unique_json()
    p$get_params_unique_json(ignoreHidden = FALSE)

------------------------------------------------------------------------

### `Pipeline$get_step()`

Get step of pipeline

#### Usage

    Pipeline$get_step(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

`data.table` row containing the step.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = 2, z = ~add1) x + y + z)
    p$run()
    add1 <- p$get_step("add1")
    print(add1)
    add1[["params"]]
    add1[["fun"]]
    try()
    try(p$get_step("foo")) # error: step 'foo' does not exist

------------------------------------------------------------------------

### `Pipeline$get_step_names()`

Get step names of pipeline

#### Usage

    Pipeline$get_step_names()

#### Returns

`character` vector of step names

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$get_step_names()

------------------------------------------------------------------------

### `Pipeline$get_step_number()`

Get step number

#### Usage

    Pipeline$get_step_number(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

the step number in the pipeline

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$get_step_number("f2")

------------------------------------------------------------------------

### `Pipeline$has_step()`

Check if pipeline has given step

#### Usage

    Pipeline$has_step(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

`logical` whether step exists

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$has_step("f2")
    p$has_step("foo")

------------------------------------------------------------------------

### `Pipeline$insert_after()`

Insert step after a certain step

#### Usage

    Pipeline$insert_after(afterStep, step, ...)

#### Arguments

- `afterStep`:

  `string` name of step after which to insert

- `step`:

  `string` name of step to insert

- `...`:

  further arguments passed to `add` method of the pipeline

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(x = ~f1) x)
    p$insert_after("f1", "f3", \(x = ~f1) x)
    p

------------------------------------------------------------------------

### `Pipeline$insert_before()`

Insert step before a certain step

#### Usage

    Pipeline$insert_before(beforeStep, step, ...)

#### Arguments

- `beforeStep`:

  `string` name of step before which to insert

- `step`:

  `string` name of step to insert

- `...`:

  further arguments passed to `add` method of the pipeline

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(x = ~f1) x)
    p$insert_before("f2", "f3", \(x = ~f1) x)
    p

------------------------------------------------------------------------

### `Pipeline$length()`

Length of the pipeline aka number of pipeline steps.

#### Usage

    Pipeline$length()

#### Returns

`numeric` length of pipeline.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$length()

------------------------------------------------------------------------

### `Pipeline$lock_step()`

Locking a step means that both its parameters and its output (given it
has output) are locked such that neither setting new pipeline parameters
nor future pipeline runs can change the current parameter and output
content.

#### Usage

    Pipeline$lock_step(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = 1, data = ~data) x + data)
    p$add("add2", \(x = 1, data = ~data) x + data)
    p$run()
    p$get_out("add1")
    p$get_out("add2")
    p$lock_step("add1")

    p$set_data(3)
    p$set_params(list(x = 3))
    p$run()
    p$get_out("add1")
    p$get_out("add2")

------------------------------------------------------------------------

### `Pipeline$pop_step()`

Drop last step from the pipeline.

#### Usage

    Pipeline$pop_step()

#### Returns

`string` the name of the step that was removed

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p
    p$pop_step() # "f2"
    p

------------------------------------------------------------------------

### `Pipeline$pop_steps_after()`

Drop all steps after the given step.

#### Usage

    Pipeline$pop_steps_after(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

`character` vector of steps that were removed.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$add("f3", \(z = 1) z)
    p$pop_steps_after("f1")  # "f2", "f3"
    p

------------------------------------------------------------------------

### `Pipeline$pop_steps_from()`

Drop all steps from and including the given step.

#### Usage

    Pipeline$pop_steps_from(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

`character` vector of steps that were removed.

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$add("f3", \(z = 1) z)
    p$pop_steps_from("f2")  # "f2", "f3"
    p

------------------------------------------------------------------------

### `Pipeline$print()`

Print the pipeline as a table.

#### Usage

    Pipeline$print(verbose = FALSE)

#### Arguments

- `verbose`:

  `logical` if `TRUE`, print all columns of the pipeline, otherwise only
  the most relevant columns are displayed.

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$print()

------------------------------------------------------------------------

### `Pipeline$remove_step()`

Remove certain step from the pipeline. If other steps depend on the step
to be removed, an error is given and the removal is blocked, unless
`recursive` was set to `TRUE`.

#### Usage

    Pipeline$remove_step(step, recursive = FALSE)

#### Arguments

- `step`:

  `string` the name of the step to be removed.

- `recursive`:

  `logical` if `TRUE` the step is removed together with all its
  downstream dependencies.

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = ~add1) x + y)
    p$add("mult1", \(x = 1, y = ~add2) x * y)
    p$remove_step("mult1")
    p
    try(p$remove_step("add1"))  # fails because "add2" depends on "add1"
    p$remove_step("add1", recursive = TRUE)  # removes "add1" and "add2"
    p

------------------------------------------------------------------------

### `Pipeline$rename_step()`

Safely rename a step in the pipeline. If new step name would result in a
name clash, an error is given.

#### Usage

    Pipeline$rename_step(from, to)

#### Arguments

- `from`:

  `string` the name of the step to be renamed.

- `to`:

  `string` the new name of the step.

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("add1", \(data = ~data, x = 1) x + data)
    p$add("add2", \(x = 1, y = ~add1) x + y)
    p
    try(p$rename_step("add1", "add2"))  # fails because "add2" exists
    p$rename_step("add1", "first_add")  # Ok
    p

------------------------------------------------------------------------

### `Pipeline$replace_step()`

Replaces an existing pipeline step.

#### Usage

    Pipeline$replace_step(
      step,
      fun,
      params = list(),
      description = "",
      group = step,
      keepOut = FALSE
    )

#### Arguments

- `step`:

  `string` the name of the step to be replaced. Step must exist.

- `fun`:

  `string` or `function` operation to be applied at the step. Both
  existing and lambda/anonymous functions can be used.

- `params`:

  `list` list of parameters to overwrite default parameters of existing
  functions.

- `description`:

  `string` optional description of the step

- `group`:

  `string` grouping information (by default the same as the name of the
  step. Any output collected later (see function `collect_out` by
  default is put together by these group names. This, for example, comes
  in handy when the pipeline is copy-appended multiple times to keep the
  results of the same function/step at one place.

- `keepOut`:

  `logical` if `FALSE` the output of the function will be cleaned at the
  end of the whole pipeline execution. This option is used to only keep
  the results that matter.

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 1) x + y)
    p$add("add2", \(x = ~data, y = 2) x + y)
    p$add("mult", \(x = 1, y = 2) x * y, keepOut = TRUE)
    p$run()$collect_out()
    p$replace_step("mult", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
    p$run()$collect_out()
    try(p$replace_step("foo", \(x = 1) x))   # step 'foo' does not exist

------------------------------------------------------------------------

### `Pipeline$reset()`

Resets the pipeline to the state before it was run. This means that all
output is removed and the state of all steps is reset to 'New'.

#### Usage

    Pipeline$reset()

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1:2)
    p$add("f1", \(x = 1) x)
    p$add("f2", \(y = 1) y)
    p$run()
    p
    p$reset()
    p

------------------------------------------------------------------------

### `Pipeline$run()`

Run all new and/or outdated pipeline steps.

#### Usage

    Pipeline$run(
      force = FALSE,
      recursive = TRUE,
      cleanUnkept = FALSE,
      progress = NULL,
      showLog = TRUE
    )

#### Arguments

- `force`:

  `logical` if `TRUE` all steps are run regardless of whether they are
  outdated or not.

- `recursive`:

  `logical` if `TRUE` and a step returns a new pipeline, the run of the
  current pipeline is aborted and the new pipeline is run recursively.

- `cleanUnkept`:

  `logical` if `TRUE` all output that was not marked to be kept is
  removed after the pipeline run. This option can be useful if temporary
  results require a lot of memory.

- `progress`:

  `function` this parameter can be used to provide a custom progress
  function of the form `function(value, detail)`, which will show the
  progress of the pipeline run for each step, where `value` is the
  current step number and `detail` is the name of the step.

- `showLog`:

  `logical` should the steps be logged during the pipeline run?

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    # Simple pipeline
    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 1) x + y)
    p$add("add2", \(x = ~add1, z = 2) x + z)
    p$add("final", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
    p$run()$collect_out()
    p$set_params(list(z = 4))  # outdates steps add2 and final
    p
    p$run()$collect_out()
    p$run(cleanUnkept = TRUE)  # clean up temporary results
    p

    # Recursive pipeline
    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 1) x + y)
    p$add("new_pipe", \(x = ~add1) {
        pp <- Pipeline$new("new_pipe", data = x)
        pp$add("add1", \(x = ~data) x + 1)
        pp$add("add2", \(x = ~add1) x + 2, keepOut = TRUE)
        }
    )
    p$run(recursive = TRUE)$collect_out()

    # Run pipeline with progress bar
    p <- Pipeline$new("pipe", data = 1)
    p$add("first step", \() Sys.sleep(1))
    p$add("second step", \() Sys.sleep(1))
    p$add("last step", \() Sys.sleep(1))
    pb <- txtProgressBar(min = 1, max = p$length(), style = 3)
    fprogress <- function(value, detail) {
       setTxtProgressBar(pb, value)
    }
    p$run(progress = fprogress, showLog = FALSE)

------------------------------------------------------------------------

### `Pipeline$run_step()`

Run given pipeline step possibly together with upstream and downstream
dependencies.

#### Usage

    Pipeline$run_step(
      step,
      upstream = TRUE,
      downstream = FALSE,
      cleanUnkept = FALSE
    )

#### Arguments

- `step`:

  `string` name of step

- `upstream`:

  `logical` if `TRUE`, run all dependent upstream steps first.

- `downstream`:

  `logical` if `TRUE`, run all depdendent downstream afterwards.

- `cleanUnkept`:

  `logical` if `TRUE` all output that was not marked to be kept is
  removed after the pipeline run. This option can be useful if temporary
  results require a lot of memory.

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 1) x + y)
    p$add("add2", \(x = ~add1, z = 2) x + z)
    p$add("mult", \(x = ~add1, y = ~add2) x * y)
    p$run_step("add2")
    p$run_step("add2", downstream = TRUE)
    p$run_step("mult", upstream = TRUE)

------------------------------------------------------------------------

### `Pipeline$set_data()`

Set data in first step of pipeline.

#### Usage

    Pipeline$set_data(data)

#### Arguments

- `data`:

  initial data set

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
    p$run()$collect_out()
    p$set_data(3)
    p$run()$collect_out()

------------------------------------------------------------------------

### `Pipeline$set_data_split()`

This function can be used to apply the pipeline repeatedly to various
data sets. For this, the pipeline split-copies itself by the list of
given data sets. Each sub-pipeline will have one of the data sets set as
input data. The step names of the sub-pipelines will be the original
step names plus the name of the data set.

#### Usage

    Pipeline$set_data_split(
      dataList,
      toStep = character(),
      groupBySplit = TRUE,
      sep = "."
    )

#### Arguments

- `dataList`:

  `list` of data sets

- `toStep`:

  `string` step name marking optional subset of the pipeline, at which
  the data split should be applied to.

- `groupBySplit`:

  `logical` whether to set step groups according to data split.

- `sep`:

  `string` separator to be used between step name and data set name when
  creating the new step names.

#### Returns

new combined `Pipeline` with each sub-pipeline having set one of the
data sets.

#### Examples

    # Split by three data sets
    dataList <- list(a = 1, b = 2, c = 3)
    p <- Pipeline$new("pipe")
    p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
    p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
    p$set_data_split(dataList)
    p
    p$run()$collect_out() |> str()

    # Don't group output by split
    p <- Pipeline$new("pipe")
    p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
    p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
    p$set_data_split(dataList, groupBySplit = FALSE)
    p
    p$run()$collect_out() |> str()

    # Split up to certain step
    p <- Pipeline$new("pipe")
    p$add("add1", \(x = ~data) x + 1)
    p$add("mult", \(x = ~data, y = ~add1) x * y)
    p$add("average_result", \(x = ~mult) mean(unlist(x)), keepOut = TRUE)
    p
    p$get_depends()[["average_result"]]

    p$set_data_split(dataList, toStep = "mult")
    p
    p$get_depends()[["average_result"]]

    p$run()$collect_out()

------------------------------------------------------------------------

### `Pipeline$set_keep_out()`

Change the `keepOut` flag at a given pipeline step, which determines
whether the output of that step is collected when calling
`collect_out()` after the pipeline was run.

#### Usage

    Pipeline$set_keep_out(step, keepOut = TRUE)

#### Arguments

- `step`:

  `string` name of step

- `keepOut`:

  `logical` whether to keep output of step

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
    p$add("add2", \(x = ~data, y = 2) x + y)
    p$add("mult", \(x = ~add1, y = ~add2) x * y)
    p$run()$collect_out()
    p$set_keep_out("add1", keepOut = FALSE)
    p$set_keep_out("mult", keepOut = TRUE)
    p$collect_out()

------------------------------------------------------------------------

### `Pipeline$set_params()`

Set parameters in the pipeline. If a parameter occurs in several steps,
the parameter is set commonly in all steps. Trying to set parameters
that don't exist in the pipeline is ignored, by default, with a warning.

#### Usage

    Pipeline$set_params(params, warnUndefined = TRUE)

#### Arguments

- `params`:

  `list` of parameters to be set

- `warnUndefined`:

  `logical` whether to give a warning when trying to set a parameter
  that is not defined in the pipeline.

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 2) x + y)
    p$add("add2", \(x = ~data, y = 3) x + y)
    p$add("mult", \(x = 4, z = 5) x * z)
    p$get_params()
    p$set_params(list(x = 3, y = 3))
    p$get_params()
    p$set_params(list(x = 5, z = 3))
    p$get_params()
    suppressWarnings(
        p$set_params(list(foo = 3)) # gives warning as 'foo' is undefined
    )
    p$set_params(list(foo = 3), warnUndefined = FALSE)

------------------------------------------------------------------------

### `Pipeline$set_params_at_step()`

Set unbound function parameters defined at given pipeline step where
'unbound' means parameters that are not linked to other steps.

#### Usage

    Pipeline$set_params_at_step(step, params)

#### Arguments

- `step`:

  `string` the name of the step

- `params`:

  `list` of parameters to be set

#### Returns

returns the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = ~data, y = 2, z = 3) x + y)
    p$set_params_at_step("add1", list(y = 5, z = 6))
    p$get_params()
    try(p$set_params_at_step("add1", list(foo = 3))) # foo not defined

------------------------------------------------------------------------

### `Pipeline$split()`

Splits pipeline into its independent parts.

#### Usage

    Pipeline$split()

#### Returns

list of `Pipeline` objects

#### Examples

    # Example for two independent calculation paths
    p <- Pipeline$new("pipe", data = 1)
    p$add("f1", \(x = ~data) x)
    p$add("f2", \(x = 1) x)
    p$add("f3", \(x = ~f1) x)
    p$add("f4", \(x = ~f2) x)
    p$split()

    # Example of split by three data sets
    dataList <- list(a = 1, b = 2, c = 3)
    p <- Pipeline$new("pipe")
    p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
    p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
    pipes <- p$set_data_split(dataList)$split()
    pipes

------------------------------------------------------------------------

### `Pipeline$unlock_step()`

Unlock previously locked step. If step was not locked, the command is
ignored.

#### Usage

    Pipeline$unlock_step(step)

#### Arguments

- `step`:

  `string` name of step

#### Returns

the `Pipeline` object invisibly

#### Examples

    p <- Pipeline$new("pipe", data = 1)
    p$add("add1", \(x = 1, data = ~data) x + data)
    p$add("add2", \(x = 1, data = ~data) x + data)
    p$lock_step("add1")
    p$set_params(list(x = 3))
    p$get_params()
    p$unlock_step("add1")
    p$set_params(list(x = 3))
    p$get_params()

------------------------------------------------------------------------

### `Pipeline$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Pipeline$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `Pipeline$new()`
## ------------------------------------------------

p <- Pipeline$new("myPipe", data = data.frame(x = 1:8))
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New

# Passing custom logger
my_logger <- function(level, msg, ...) {
   cat(level, msg, "\n")
}
p <- Pipeline$new("myPipe", logger = my_logger)

## ------------------------------------------------
## Method `Pipeline$add()`
## ------------------------------------------------

# Add steps with lambda functions
p <- Pipeline$new("myPipe", data = 1)
p$add("s1", \(x = ~data) 2*x)  # use input data
p$add("s2", \(x = ~data, y = ~s1) x * y)
try(p$add("s2", \(z = 3) 3)) # error: step 's2' exists already
#> Error : step 's2' already exists
try(p$add("s3", \(z = ~foo) 3)) # dependency 'foo' not found
#> Error : step 's3': dependency 'foo' not found
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     s1    data [NULL]   FALSE     s1    New
#> 3:     s2 data,s1 [NULL]   FALSE     s2    New

# Add step with existing function
p <- Pipeline$new("myPipe", data = c(1, 2, NA, 3, 4))
p$add("calc_mean", mean, params = list(x = ~data, na.rm = TRUE))
p$run()$get_out("calc_mean")
#> INFO  [2026-06-20 21:18:43.203] Start run of 'myPipe' pipeline:
#> INFO  [2026-06-20 21:18:43.241] Step 1/2 data
#> INFO  [2026-06-20 21:18:43.254] Step 2/2 calc_mean
#> INFO  [2026-06-20 21:18:43.280] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.281] Done.
#> [1] 2.5

# Step description
p <- Pipeline$new("myPipe", data = 1:10)
p$add("s1", \(x = ~data) 2*x, description = "multiply by 2")
print(p)
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     s1    data [NULL]   FALSE     s1    New
print(p, verbose = TRUE) # print all columns
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   data <function[1]> function <list[0]>         [NULL]   FALSE   data
#> 2:     s1 <function[1]> function <list[1]>    data [NULL]   FALSE     s1
#>      description                time  state
#>           <char>              <POSc> <char>
#> 1:               2026-06-20 21:18:43    New
#> 2: multiply by 2 2026-06-20 21:18:43    New

# Group output
p <- Pipeline$new("myPipe", data = data.frame(x = 1:5, y = 1:5))
p$add("prep_x", \(data = ~data) data$x, group = "prep")
p$add("prep_y", \(data = ~data) (data$y)^2, group = "prep")
p$add("sum", \(x = ~prep_x, y = ~prep_y) x + y)
p$run()$collect_out(all = TRUE)
#> INFO  [2026-06-20 21:18:43.299] Start run of 'myPipe' pipeline:
#> INFO  [2026-06-20 21:18:43.300] Step 1/4 data
#> INFO  [2026-06-20 21:18:43.303] Step 2/4 prep_x
#> INFO  [2026-06-20 21:18:43.306] Step 3/4 prep_y
#> INFO  [2026-06-20 21:18:43.308] Step 4/4 sum
#> INFO  [2026-06-20 21:18:43.310] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.311] Done.
#> $data
#>   x y
#> 1 1 1
#> 2 2 2
#> 3 3 3
#> 4 4 4
#> 5 5 5
#> 
#> $prep
#> $prep$prep_x
#> [1] 1 2 3 4 5
#> 
#> $prep$prep_y
#> [1]  1  4  9 16 25
#> 
#> 
#> $sum
#> [1]  2  6 12 20 30
#> 

## ------------------------------------------------
## Method `Pipeline$append()`
## ------------------------------------------------

# Append pipeline
p1 <- Pipeline$new("pipe1")
p1$add("step1", \(x = 1) x)
p2 <- Pipeline$new("pipe2")
p2$add("step2", \(y = 1) y)
p1$append(p2)
#>          step depends    out keepOut  group  state
#>        <char>  <list> <list>  <lgcl> <char> <char>
#> 1:       data         [NULL]   FALSE   data    New
#> 2:      step1         [NULL]   FALSE  step1    New
#> 3: data.pipe2         [NULL]   FALSE   data    New
#> 4:      step2         [NULL]   FALSE  step2    New

# Append pipeline with potential name clashes
p3 <- Pipeline$new("pipe3")
p3$add("step1", \(z = 1) z)
p1$append(p2)$append(p3)
#>           step depends    out keepOut  group  state
#>         <char>  <list> <list>  <lgcl> <char> <char>
#> 1:        data         [NULL]   FALSE   data    New
#> 2:       step1         [NULL]   FALSE  step1    New
#> 3:  data.pipe2         [NULL]   FALSE   data    New
#> 4:       step2         [NULL]   FALSE  step2    New
#> 5:  data.pipe3         [NULL]   FALSE   data    New
#> 6: step1.pipe3         [NULL]   FALSE  step1    New

# Use output of first pipeline as input for second pipeline
p1 <- Pipeline$new("pipe1", data = 8)
p2 <- Pipeline$new("pipe2")
p1$add("square", \(x = ~data) x^2)
p2$add("log2", \(x = ~data) log2(x))

p12 <- p1$append(p2, outAsIn = TRUE)
p12$run()$get_out("log2")
#> INFO  [2026-06-20 21:18:43.473] Start run of 'pipe1.pipe2' pipeline:
#> INFO  [2026-06-20 21:18:43.474] Step 1/4 data
#> INFO  [2026-06-20 21:18:43.477] Step 2/4 square
#> INFO  [2026-06-20 21:18:43.481] Step 3/4 data.pipe2
#> INFO  [2026-06-20 21:18:43.483] Step 4/4 log2
#> INFO  [2026-06-20 21:18:43.485] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.485] Done.
#> [1] 6
p12
#>          step    depends    out keepOut      group  state
#>        <char>     <list> <list>  <lgcl>     <char> <char>
#> 1:       data                 8   FALSE       data   Done
#> 2:     square       data     64   FALSE     square   Done
#> 3: data.pipe2     square     64   FALSE data.pipe2   Done
#> 4:       log2 data.pipe2      6   FALSE       log2   Done

# Custom name separator
p1$append(p2, sep = "___")
#>            step      depends    out keepOut  group  state
#>          <char>       <list> <list>  <lgcl> <char> <char>
#> 1:         data              [NULL]   FALSE   data    New
#> 2:       square         data [NULL]   FALSE square    New
#> 3: data___pipe2              [NULL]   FALSE   data    New
#> 4:         log2 data___pipe2 [NULL]   FALSE   log2    New

## ------------------------------------------------
## Method `Pipeline$append_to_step_names()`
## ------------------------------------------------

p <- Pipeline$new("pipe")
p$add("step1", \(x = 1) x)
p$add("step2", \(y = 1) y)
p$append_to_step_names("new")
p
#>         step depends    out keepOut  group  state
#>       <char>  <list> <list>  <lgcl> <char> <char>
#> 1:  data.new         [NULL]   FALSE   data    New
#> 2: step1.new         [NULL]   FALSE  step1    New
#> 3: step2.new         [NULL]   FALSE  step2    New
p$append_to_step_names("foo", sep = "__")
p
#>              step depends    out keepOut  group  state
#>            <char>  <list> <list>  <lgcl> <char> <char>
#> 1:  data.new__foo         [NULL]   FALSE   data    New
#> 2: step1.new__foo         [NULL]   FALSE  step1    New
#> 3: step2.new__foo         [NULL]   FALSE  step2    New

## ------------------------------------------------
## Method `Pipeline$collect_out()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("step1", \(x = ~data) x + 2)
p$add("step2", \(x = ~step1) x + 2, keepOut = TRUE)
p$run()
#> INFO  [2026-06-20 21:18:43.512] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:43.514] Step 1/3 data
#> INFO  [2026-06-20 21:18:43.516] Step 2/3 step1
#> INFO  [2026-06-20 21:18:43.519] Step 3/3 step2
#> INFO  [2026-06-20 21:18:43.520] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.521] Done.
p$collect_out()
#> $step2
#> [1] 5 6
#> 
p$collect_out(all = TRUE) |> str()
#> List of 3
#>  $ data : int [1:2] 1 2
#>  $ step1: num [1:2] 3 4
#>  $ step2: num [1:2] 5 6

# Grouped output
p <- Pipeline$new("pipe", data = 1:2)
p$add("step1", \(x = ~data) x + 2, group = "add")
p$add("step2", \(x = ~step1, y = 2) x + y, group = "add")
p$add("step3", \(x = ~data) x * 3, group = "mult")
p$add("step4", \(x = ~data, y = 2) x * y, group = "mult")
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:  step1    data [NULL]   FALSE    add    New
#> 3:  step2   step1 [NULL]   FALSE    add    New
#> 4:  step3    data [NULL]   FALSE   mult    New
#> 5:  step4    data [NULL]   FALSE   mult    New
p$run()
#> INFO  [2026-06-20 21:18:43.556] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:43.557] Step 1/5 data
#> INFO  [2026-06-20 21:18:43.563] Step 2/5 step1
#> INFO  [2026-06-20 21:18:43.565] Step 3/5 step2
#> INFO  [2026-06-20 21:18:43.567] Step 4/5 step3
#> INFO  [2026-06-20 21:18:43.569] Step 5/5 step4
#> INFO  [2026-06-20 21:18:43.571] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.571] Done.
p$collect_out(all = TRUE) |> str()
#> List of 3
#>  $ data: int [1:2] 1 2
#>  $ add :List of 2
#>   ..$ step1: num [1:2] 3 4
#>   ..$ step2: num [1:2] 5 6
#>  $ mult:List of 2
#>   ..$ step3: num [1:2] 3 6
#>   ..$ step4: num [1:2] 2 4

# Grouped by state
p$set_params(list(y = 5))
p
#>      step depends    out keepOut  group    state
#>    <char>  <list> <list>  <lgcl> <char>   <char>
#> 1:   data            1,2   FALSE   data     Done
#> 2:  step1    data    3,4   FALSE    add     Done
#> 3:  step2   step1    5,6   FALSE    add Outdated
#> 4:  step3    data    3,6   FALSE   mult     Done
#> 5:  step4    data    2,4   FALSE   mult Outdated
p$collect_out(groupBy = "state", all = TRUE) |> str()
#> List of 2
#>  $ Done    :List of 3
#>   ..$ data : int [1:2] 1 2
#>   ..$ step1: num [1:2] 3 4
#>   ..$ step3: num [1:2] 3 6
#>  $ Outdated:List of 2
#>   ..$ step2: num [1:2] 5 6
#>   ..$ step4: num [1:2] 2 4

## ------------------------------------------------
## Method `Pipeline$discard_steps()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(x = ~data) x + 1)
p$add("add2", \(x = ~add1) x + 2)
p$add("mult3", \(x = ~add1) x * 3)
p$add("mult4", \(x = ~add2) x * 4)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
#> 4:  mult3    add1 [NULL]   FALSE  mult3    New
#> 5:  mult4    add2 [NULL]   FALSE  mult4    New

p$discard_steps("mult")
#> step 'mult4' was removed
#> step 'mult3' was removed
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New

# Re-add steps
p$add("mult3", \(x = ~add1) x * 3)
p$add("mult4", \(x = ~add2) x * 4)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
#> 4:  mult3    add1 [NULL]   FALSE  mult3    New
#> 5:  mult4    add2 [NULL]   FALSE  mult4    New
# Discarding 'add1' does not work ...
try(p$discard_steps("add1"))
#> Error : cannot remove step 'add1' because the following steps depend on it: 'add2', 'mult3'

# ... unless we enforce to remove its downstream dependencies as well
p$discard_steps("add1", recursive = TRUE)   # this works
#> Removing step 'add1' and its downstream dependencies: 'add2', 'mult3', 'mult4'
#> step 'add1' was removed
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New

# Trying to discard non-existent steps is just ignored
p$discard_steps("non-existent")

## ------------------------------------------------
## Method `Pipeline$get_data()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$get_data()
#> [1] 1 2
p$set_data(3:4)
p$get_data()
#> [1] 3 4

## ------------------------------------------------
## Method `Pipeline$get_depends()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(x = ~data) x + 1)
p$add("add2", \(x = ~data, y = ~add1) x + y)
p$get_depends()
#> $data
#> character(0)
#> 
#> $add1
#>      x 
#> "data" 
#> 
#> $add2
#>      x      y 
#> "data" "add1" 
#> 

## ------------------------------------------------
## Method `Pipeline$get_depends_down()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(x = ~data) x + 1)
p$add("add2", \(x = ~data, y = ~add1) x + y)
p$add("mult3", \(x = ~add1) x * 3)
p$add("mult4", \(x = ~add2) x * 4)
p$get_depends_down("add1")
#> [1] "add2"  "mult3" "mult4"
p$get_depends_down("add1", recursive = FALSE)
#> [1] "add2"  "mult3"

## ------------------------------------------------
## Method `Pipeline$get_depends_up()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(x = ~data) x + 1)
p$add("add2", \(x = ~data, y = ~add1) x + y)
p$add("mult3", \(x = ~add1) x * 3)
p$add("mult4", \(x = ~add2) x * 4)
p$get_depends_up("mult4")
#> [1] "data" "add1" "add2"
p$get_depends_up("mult4", recursive = FALSE)
#> [1] "add2"

## ------------------------------------------------
## Method `Pipeline$get_graph()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = ~add1) x + y)
p$add("mult1", \(x = ~add1, y = ~add2) x * y)
graph <- pipe_get_graph(p)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
graph
#> $nodes
#>   id label group    shape     color   title
#> 1  1  data  data database lightblue <p></p>
#> 2  2  add1  add1      box lightblue <p></p>
#> 3  3  add2  add2      box lightblue <p></p>
#> 4  4 mult1 mult1      box lightblue <p></p>
#> 
#> $edges
#>         from to arrows
#> add1       1  2     to
#> add2       2  3     to
#> mult1.x    2  4     to
#> mult1.y    3  4     to
#> 

if (require("visNetwork", quietly = TRUE)) {
    do.call(visNetwork, args = p$get_graph())
}

{"x":{"nodes":{"id":[1,2,3,4],"label":["data","add1","add2","mult1"],"group":["data","add1","add2","mult1"],"shape":["database","box","box","box"],"color":["lightblue","lightblue","lightblue","lightblue"],"title":["<p><\/p>","<p><\/p>","<p><\/p>","<p><\/p>"]},"edges":{"from":[1,2,2,3],"to":[2,3,4,4],"arrows":["to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":["data","add1","add2","mult1"],"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}
## ------------------------------------------------
## Method `Pipeline$get_out()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(x = ~data) x + 1)
p$add("add2", \(x = ~data, y = ~add1) x + y)
p$run()
#> INFO  [2026-06-20 21:18:43.761] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:43.764] Step 1/3 data
#> INFO  [2026-06-20 21:18:43.768] Step 2/3 add1
#> INFO  [2026-06-20 21:18:43.772] Step 3/3 add2
#> INFO  [2026-06-20 21:18:43.777] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.778] Done.
p$get_out("add1")
#> [1] 2 3
p$get_out("add2")
#> [1] 3 5

## ------------------------------------------------
## Method `Pipeline$get_params()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
p$add("add3", \() 1 + 2)
p$get_params() |> str()
#> List of 2
#>  $ add1:List of 1
#>   ..$ x: num 1
#>  $ add2:List of 2
#>   ..$ x: num 1
#>   ..$ y: num 2
p$get_params(ignoreHidden = FALSE) |> str()
#> List of 2
#>  $ add1:List of 1
#>   ..$ x: num 1
#>  $ add2:List of 3
#>   ..$ x : num 1
#>   ..$ y : num 2
#>   ..$ .z: num 3

## ------------------------------------------------
## Method `Pipeline$get_params_at_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
p$add("add3", \() 1 + 2)
p$get_params_at_step("add2")
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
p$get_params_at_step("add2", ignoreHidden = FALSE)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
#> $.z
#> [1] 3
#> 
p$get_params_at_step("add3")
#> list()

## ------------------------------------------------
## Method `Pipeline$get_params_unique()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
p$add("mult1", \(x = 1, y = 2, .z = 3, b = ~add2) x * y * b)
p$get_params_unique()
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
p$get_params_unique(ignoreHidden = FALSE)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
#> $.z
#> [1] 3
#> 

## ------------------------------------------------
## Method `Pipeline$get_params_unique_json()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
p$add("mult1", \(x = 1, y = 2, .z = 3, b = ~add2) x * y * b)
p$get_params_unique_json()
#> [
#>   {
#>     "name": "x",
#>     "value": 1
#>   },
#>   {
#>     "name": "y",
#>     "value": 2
#>   }
#> ] 
p$get_params_unique_json(ignoreHidden = FALSE)
#> [
#>   {
#>     "name": "x",
#>     "value": 1
#>   },
#>   {
#>     "name": "y",
#>     "value": 2
#>   },
#>   {
#>     "name": ".z",
#>     "value": 3
#>   }
#> ] 

## ------------------------------------------------
## Method `Pipeline$get_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = 2, z = ~add1) x + y + z)
p$run()
#> INFO  [2026-06-20 21:18:43.843] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:43.845] Step 1/3 data
#> INFO  [2026-06-20 21:18:43.847] Step 2/3 add1
#> INFO  [2026-06-20 21:18:43.850] Step 3/3 add2
#> INFO  [2026-06-20 21:18:43.851] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.852] Done.
add1 <- p$get_step("add1")
print(add1)
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   add1 <function[1]> function <list[2]>    data    2,3   FALSE   add1
#>    description                time  state
#>         <char>              <POSc> <char>
#> 1:             2026-06-20 21:18:43   Done
add1[["params"]]
#> [[1]]
#> [[1]]$data
#> ~data
#> <environment: 0x0000022e8455d048>
#> 
#> [[1]]$x
#> [1] 1
#> 
#> 
add1[["fun"]]
#> [[1]]
#> function (data = ~data, x = 1) 
#> x + data
#> <environment: 0x0000022e7f153a58>
#> 
try()
#> Error in try() : argument "expr" is missing, with no default
try(p$get_step("foo")) # error: step 'foo' does not exist
#> Error : step 'foo' does not exist

## ------------------------------------------------
## Method `Pipeline$get_step_names()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$get_step_names()
#> [1] "data" "f1"   "f2"  

## ------------------------------------------------
## Method `Pipeline$get_step_number()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$get_step_number("f2")
#> [1] 3

## ------------------------------------------------
## Method `Pipeline$has_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$has_step("f2")
#> [1] TRUE
p$has_step("foo")
#> [1] FALSE

## ------------------------------------------------
## Method `Pipeline$insert_after()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("f1", \(x = 1) x)
p$add("f2", \(x = ~f1) x)
p$insert_after("f1", "f3", \(x = ~f1) x)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f3      f1 [NULL]   FALSE     f3    New
#> 4:     f2      f1 [NULL]   FALSE     f2    New

## ------------------------------------------------
## Method `Pipeline$insert_before()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("f1", \(x = 1) x)
p$add("f2", \(x = ~f1) x)
p$insert_before("f2", "f3", \(x = ~f1) x)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f3      f1 [NULL]   FALSE     f3    New
#> 4:     f2      f1 [NULL]   FALSE     f2    New

## ------------------------------------------------
## Method `Pipeline$length()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$length()
#> [1] 3

## ------------------------------------------------
## Method `Pipeline$lock_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = 1, data = ~data) x + data)
p$add("add2", \(x = 1, data = ~data) x + data)
p$run()
#> INFO  [2026-06-20 21:18:43.918] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:43.919] Step 1/3 data
#> INFO  [2026-06-20 21:18:43.922] Step 2/3 add1
#> INFO  [2026-06-20 21:18:43.925] Step 3/3 add2
#> INFO  [2026-06-20 21:18:43.928] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.929] Done.
p$get_out("add1")
#> [1] 2
p$get_out("add2")
#> [1] 2
p$lock_step("add1")

p$set_data(3)
p$set_params(list(x = 3))
#> skipping setting parameters x at locked step 'add1'
p$run()
#> INFO  [2026-06-20 21:18:43.956] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:43.958] Step 1/3 data
#> INFO  [2026-06-20 21:18:43.962] Step 2/3 add1 - skip 'locked' step
#> INFO  [2026-06-20 21:18:43.963] Step 3/3 add2
#> INFO  [2026-06-20 21:18:43.965] Finished execution of steps.
#> INFO  [2026-06-20 21:18:43.965] Done.
p$get_out("add1")
#> [1] 2
p$get_out("add2")
#> [1] 6

## ------------------------------------------------
## Method `Pipeline$pop_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
p$pop_step() # "f2"
#> [1] "f2"
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New

## ------------------------------------------------
## Method `Pipeline$pop_steps_after()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$add("f3", \(z = 1) z)
p$pop_steps_after("f1")  # "f2", "f3"
#> [1] "f2" "f3"
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New

## ------------------------------------------------
## Method `Pipeline$pop_steps_from()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$add("f3", \(z = 1) z)
p$pop_steps_from("f2")  # "f2", "f3"
#> [1] "f2" "f3"
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New

## ------------------------------------------------
## Method `Pipeline$print()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$print()
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New

## ------------------------------------------------
## Method `Pipeline$remove_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = ~add1) x + y)
p$add("mult1", \(x = 1, y = ~add2) x * y)
p$remove_step("mult1")
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
try(p$remove_step("add1"))  # fails because "add2" depends on "add1"
#> Error : cannot remove step 'add1' because the following steps depend on it: 'add2'
p$remove_step("add1", recursive = TRUE)  # removes "add1" and "add2"
#> Removing step 'add1' and its downstream dependencies: 'add2'
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New

## ------------------------------------------------
## Method `Pipeline$rename_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$add("add2", \(x = 1, y = ~add1) x + y)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
try(p$rename_step("add1", "add2"))  # fails because "add2" exists
#> Error : step 'add2' already exists
p$rename_step("add1", "first_add")  # Ok
p
#>         step   depends    out keepOut  group  state
#>       <char>    <list> <list>  <lgcl> <char> <char>
#> 1:      data           [NULL]   FALSE   data    New
#> 2: first_add      data [NULL]   FALSE   add1    New
#> 3:      add2 first_add [NULL]   FALSE   add2    New

## ------------------------------------------------
## Method `Pipeline$replace_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 1) x + y)
p$add("add2", \(x = ~data, y = 2) x + y)
p$add("mult", \(x = 1, y = 2) x * y, keepOut = TRUE)
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:44.031] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.033] Step 1/4 data
#> INFO  [2026-06-20 21:18:44.037] Step 2/4 add1
#> INFO  [2026-06-20 21:18:44.040] Step 3/4 add2
#> INFO  [2026-06-20 21:18:44.042] Step 4/4 mult
#> INFO  [2026-06-20 21:18:44.043] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.044] Done.
#> $mult
#> [1] 2
#> 
p$replace_step("mult", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:44.048] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.050] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-20 21:18:44.051] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-20 21:18:44.052] Step 3/4 add2 - skip 'done' step
#> INFO  [2026-06-20 21:18:44.053] Step 4/4 mult
#> INFO  [2026-06-20 21:18:44.055] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.056] Done.
#> $mult
#> [1] 6
#> 
try(p$replace_step("foo", \(x = 1) x))   # step 'foo' does not exist
#> Error : step 'foo' does not exist

## ------------------------------------------------
## Method `Pipeline$reset()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
p$run()
#> INFO  [2026-06-20 21:18:44.080] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.081] Step 1/3 data
#> INFO  [2026-06-20 21:18:44.083] Step 2/3 f1
#> INFO  [2026-06-20 21:18:44.085] Step 3/3 f2
#> INFO  [2026-06-20 21:18:44.087] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.087] Done.
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data            1,2   FALSE   data   Done
#> 2:     f1              1   FALSE     f1   Done
#> 3:     f2              1   FALSE     f2   Done
p$reset()
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New

## ------------------------------------------------
## Method `Pipeline$run()`
## ------------------------------------------------

# Simple pipeline
p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 1) x + y)
p$add("add2", \(x = ~add1, z = 2) x + z)
p$add("final", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:44.102] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.104] Step 1/4 data
#> INFO  [2026-06-20 21:18:44.107] Step 2/4 add1
#> INFO  [2026-06-20 21:18:44.110] Step 3/4 add2
#> INFO  [2026-06-20 21:18:44.112] Step 4/4 final
#> INFO  [2026-06-20 21:18:44.113] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.114] Done.
#> $final
#> [1] 8
#> 
p$set_params(list(z = 4))  # outdates steps add2 and final
p
#>      step   depends    out keepOut  group    state
#>    <char>    <list> <list>  <lgcl> <char>   <char>
#> 1:   data                1   FALSE   data     Done
#> 2:   add1      data      2   FALSE   add1     Done
#> 3:   add2      add1      4   FALSE   add2 Outdated
#> 4:  final add1,add2      8    TRUE  final Outdated
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:44.122] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.124] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-20 21:18:44.126] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-20 21:18:44.128] Step 3/4 add2
#> INFO  [2026-06-20 21:18:44.130] Step 4/4 final
#> INFO  [2026-06-20 21:18:44.132] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.132] Done.
#> $final
#> [1] 12
#> 
p$run(cleanUnkept = TRUE)  # clean up temporary results
#> INFO  [2026-06-20 21:18:44.134] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.139] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-20 21:18:44.140] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-20 21:18:44.142] Step 3/4 add2 - skip 'done' step
#> INFO  [2026-06-20 21:18:44.143] Step 4/4 final - skip 'done' step
#> INFO  [2026-06-20 21:18:44.143] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.144] Clean temporary results.
#> INFO  [2026-06-20 21:18:44.145] Done.
p
#>      step   depends    out keepOut  group    state
#>    <char>    <list> <list>  <lgcl> <char>   <char>
#> 1:   data           [NULL]   FALSE   data Outdated
#> 2:   add1      data [NULL]   FALSE   add1 Outdated
#> 3:   add2      add1 [NULL]   FALSE   add2 Outdated
#> 4:  final add1,add2     12    TRUE  final     Done

# Recursive pipeline
p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 1) x + y)
p$add("new_pipe", \(x = ~add1) {
    pp <- Pipeline$new("new_pipe", data = x)
    pp$add("add1", \(x = ~data) x + 1)
    pp$add("add2", \(x = ~add1) x + 2, keepOut = TRUE)
    }
)
p$run(recursive = TRUE)$collect_out()
#> INFO  [2026-06-20 21:18:44.153] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.155] Step 1/3 data
#> INFO  [2026-06-20 21:18:44.158] Step 2/3 add1
#> INFO  [2026-06-20 21:18:44.163] Step 3/3 new_pipe
#> INFO  [2026-06-20 21:18:44.173] Abort pipeline execution and restart on new.
#> INFO  [2026-06-20 21:18:44.175] Start run of 'new_pipe' pipeline:
#> INFO  [2026-06-20 21:18:44.177] Step 1/3 data
#> INFO  [2026-06-20 21:18:44.180] Step 2/3 add1
#> INFO  [2026-06-20 21:18:44.182] Step 3/3 add2
#> INFO  [2026-06-20 21:18:44.183] Finished execution of steps.
#> INFO  [2026-06-20 21:18:44.184] Done.
#> $add2
#> [1] 5
#> 

# Run pipeline with progress bar
p <- Pipeline$new("pipe", data = 1)
p$add("first step", \() Sys.sleep(1))
p$add("second step", \() Sys.sleep(1))
p$add("last step", \() Sys.sleep(1))
pb <- txtProgressBar(min = 1, max = p$length(), style = 3)
fprogress <- function(value, detail) {
   setTxtProgressBar(pb, value)
}
p$run(progress = fprogress, showLog = FALSE)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

## ------------------------------------------------
## Method `Pipeline$run_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 1) x + y)
p$add("add2", \(x = ~add1, z = 2) x + z)
p$add("mult", \(x = ~add1, y = ~add2) x * y)
p$run_step("add2")
#> INFO  [2026-06-20 21:18:47.274] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.275] Step 1/3 data (upstream)
#> INFO  [2026-06-20 21:18:47.278] Step 2/3 add1 (upstream)
#> INFO  [2026-06-20 21:18:47.282] Step 3/3 add2
#> INFO  [2026-06-20 21:18:47.285] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.287] Done.
p$run_step("add2", downstream = TRUE)
#> INFO  [2026-06-20 21:18:47.308] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.309] Step 1/4 data (upstream)
#> INFO  [2026-06-20 21:18:47.312] Step 2/4 add1 (upstream)
#> INFO  [2026-06-20 21:18:47.315] Step 3/4 add2
#> INFO  [2026-06-20 21:18:47.317] Step 4/4 mult (downstream)
#> INFO  [2026-06-20 21:18:47.319] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.319] Done.
p$run_step("mult", upstream = TRUE)
#> INFO  [2026-06-20 21:18:47.321] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.322] Step 1/4 data (upstream)
#> INFO  [2026-06-20 21:18:47.325] Step 2/4 add1 (upstream)
#> INFO  [2026-06-20 21:18:47.328] Step 3/4 add2 (upstream)
#> INFO  [2026-06-20 21:18:47.332] Step 4/4 mult
#> INFO  [2026-06-20 21:18:47.334] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.336] Done.

## ------------------------------------------------
## Method `Pipeline$set_data()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:47.352] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.353] Step 1/2 data
#> INFO  [2026-06-20 21:18:47.355] Step 2/2 add1
#> INFO  [2026-06-20 21:18:47.357] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.358] Done.
#> $add1
#> [1] 2
#> 
p$set_data(3)
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:47.363] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.365] Step 1/2 data
#> INFO  [2026-06-20 21:18:47.370] Step 2/2 add1
#> INFO  [2026-06-20 21:18:47.372] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.373] Done.
#> $add1
#> [1] 4
#> 

## ------------------------------------------------
## Method `Pipeline$set_data_split()`
## ------------------------------------------------

# Split by three data sets
dataList <- list(a = 1, b = 2, c = 3)
p <- Pipeline$new("pipe")
p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
p$set_data_split(dataList)
p
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.a               [NULL]   FALSE      a      New
#> 2: add1.a        data.a [NULL]    TRUE      a Outdated
#> 3: mult.a data.a,add1.a [NULL]    TRUE      a Outdated
#> 4: data.b               [NULL]   FALSE      b      New
#> 5: add1.b        data.b [NULL]    TRUE      b Outdated
#> 6: mult.b data.b,add1.b [NULL]    TRUE      b Outdated
#> 7: data.c               [NULL]   FALSE      c      New
#> 8: add1.c        data.c [NULL]    TRUE      c Outdated
#> 9: mult.c data.c,add1.c [NULL]    TRUE      c Outdated
p$run()$collect_out() |> str()
#> INFO  [2026-06-20 21:18:47.422] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.424] Step 1/9 data.a
#> INFO  [2026-06-20 21:18:47.427] Step 2/9 add1.a
#> INFO  [2026-06-20 21:18:47.432] Step 3/9 mult.a
#> INFO  [2026-06-20 21:18:47.435] Step 4/9 data.b
#> INFO  [2026-06-20 21:18:47.438] Step 5/9 add1.b
#> INFO  [2026-06-20 21:18:47.440] Step 6/9 mult.b
#> INFO  [2026-06-20 21:18:47.442] Step 7/9 data.c
#> INFO  [2026-06-20 21:18:47.445] Step 8/9 add1.c
#> INFO  [2026-06-20 21:18:47.447] Step 9/9 mult.c
#> INFO  [2026-06-20 21:18:47.449] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.449] Done.
#> List of 3
#>  $ a:List of 2
#>   ..$ add1.a: num 2
#>   ..$ mult.a: num 2
#>  $ b:List of 2
#>   ..$ add1.b: num 3
#>   ..$ mult.b: num 6
#>  $ c:List of 2
#>   ..$ add1.c: num 4
#>   ..$ mult.c: num 12

# Don't group output by split
p <- Pipeline$new("pipe")
p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
p$set_data_split(dataList, groupBySplit = FALSE)
p
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.a               [NULL]   FALSE data.a      New
#> 2: add1.a        data.a [NULL]    TRUE add1.a Outdated
#> 3: mult.a data.a,add1.a [NULL]    TRUE mult.a Outdated
#> 4: data.b               [NULL]   FALSE data.b      New
#> 5: add1.b        data.b [NULL]    TRUE add1.b Outdated
#> 6: mult.b data.b,add1.b [NULL]    TRUE mult.b Outdated
#> 7: data.c               [NULL]   FALSE data.c      New
#> 8: add1.c        data.c [NULL]    TRUE add1.c Outdated
#> 9: mult.c data.c,add1.c [NULL]    TRUE mult.c Outdated
p$run()$collect_out() |> str()
#> INFO  [2026-06-20 21:18:47.488] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.489] Step 1/9 data.a
#> INFO  [2026-06-20 21:18:47.492] Step 2/9 add1.a
#> INFO  [2026-06-20 21:18:47.495] Step 3/9 mult.a
#> INFO  [2026-06-20 21:18:47.501] Step 4/9 data.b
#> INFO  [2026-06-20 21:18:47.507] Step 5/9 add1.b
#> INFO  [2026-06-20 21:18:47.509] Step 6/9 mult.b
#> INFO  [2026-06-20 21:18:47.511] Step 7/9 data.c
#> INFO  [2026-06-20 21:18:47.514] Step 8/9 add1.c
#> INFO  [2026-06-20 21:18:47.516] Step 9/9 mult.c
#> INFO  [2026-06-20 21:18:47.518] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.519] Done.
#> List of 6
#>  $ add1.a: num 2
#>  $ mult.a: num 2
#>  $ add1.b: num 3
#>  $ mult.b: num 6
#>  $ add1.c: num 4
#>  $ mult.c: num 12

# Split up to certain step
p <- Pipeline$new("pipe")
p$add("add1", \(x = ~data) x + 1)
p$add("mult", \(x = ~data, y = ~add1) x * y)
p$add("average_result", \(x = ~mult) mean(unlist(x)), keepOut = TRUE)
p
#>              step   depends    out keepOut          group  state
#>            <char>    <list> <list>  <lgcl>         <char> <char>
#> 1:           data           [NULL]   FALSE           data    New
#> 2:           add1      data [NULL]   FALSE           add1    New
#> 3:           mult data,add1 [NULL]   FALSE           mult    New
#> 4: average_result      mult [NULL]    TRUE average_result    New
p$get_depends()[["average_result"]]
#>      x 
#> "mult" 

p$set_data_split(dataList, toStep = "mult")
p
#>               step       depends    out keepOut          group    state
#>             <char>        <list> <list>  <lgcl>         <char>   <char>
#>  1:         data.a               [NULL]   FALSE              a      New
#>  2:         add1.a        data.a [NULL]   FALSE              a Outdated
#>  3:         mult.a data.a,add1.a [NULL]   FALSE              a Outdated
#>  4:         data.b               [NULL]   FALSE              b      New
#>  5:         add1.b        data.b [NULL]   FALSE              b Outdated
#>  6:         mult.b data.b,add1.b [NULL]   FALSE              b Outdated
#>  7:         data.c               [NULL]   FALSE              c      New
#>  8:         add1.c        data.c [NULL]   FALSE              c Outdated
#>  9:         mult.c data.c,add1.c [NULL]   FALSE              c Outdated
#> 10: average_result     <list[1]> [NULL]    TRUE average_result      New
p$get_depends()[["average_result"]]
#> $x
#> [1] "mult.a" "mult.b" "mult.c"
#> 

p$run()$collect_out()
#> INFO  [2026-06-20 21:18:47.578] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.579] Step 1/10 data.a
#> INFO  [2026-06-20 21:18:47.582] Step 2/10 add1.a
#> INFO  [2026-06-20 21:18:47.585] Step 3/10 mult.a
#> INFO  [2026-06-20 21:18:47.587] Step 4/10 data.b
#> INFO  [2026-06-20 21:18:47.593] Step 5/10 add1.b
#> INFO  [2026-06-20 21:18:47.602] Step 6/10 mult.b
#> INFO  [2026-06-20 21:18:47.606] Step 7/10 data.c
#> INFO  [2026-06-20 21:18:47.609] Step 8/10 add1.c
#> INFO  [2026-06-20 21:18:47.616] Step 9/10 mult.c
#> INFO  [2026-06-20 21:18:47.619] Step 10/10 average_result
#> INFO  [2026-06-20 21:18:47.620] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.621] Done.
#> $average_result
#> [1] 6.666667
#> 

## ------------------------------------------------
## Method `Pipeline$set_keep_out()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
p$add("add2", \(x = ~data, y = 2) x + y)
p$add("mult", \(x = ~add1, y = ~add2) x * y)
p$run()$collect_out()
#> INFO  [2026-06-20 21:18:47.641] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:47.643] Step 1/4 data
#> INFO  [2026-06-20 21:18:47.645] Step 2/4 add1
#> INFO  [2026-06-20 21:18:47.648] Step 3/4 add2
#> INFO  [2026-06-20 21:18:47.650] Step 4/4 mult
#> INFO  [2026-06-20 21:18:47.651] Finished execution of steps.
#> INFO  [2026-06-20 21:18:47.652] Done.
#> $add1
#> [1] 2
#> 
p$set_keep_out("add1", keepOut = FALSE)
p$set_keep_out("mult", keepOut = TRUE)
p$collect_out()
#> $mult
#> [1] 6
#> 

## ------------------------------------------------
## Method `Pipeline$set_params()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 2) x + y)
p$add("add2", \(x = ~data, y = 3) x + y)
p$add("mult", \(x = 4, z = 5) x * z)
p$get_params()
#> $add1
#> $add1$y
#> [1] 2
#> 
#> 
#> $add2
#> $add2$y
#> [1] 3
#> 
#> 
#> $mult
#> $mult$x
#> [1] 4
#> 
#> $mult$z
#> [1] 5
#> 
#> 
p$set_params(list(x = 3, y = 3))
p$get_params()
#> $add1
#> $add1$y
#> [1] 3
#> 
#> 
#> $add2
#> $add2$y
#> [1] 3
#> 
#> 
#> $mult
#> $mult$x
#> [1] 3
#> 
#> $mult$z
#> [1] 5
#> 
#> 
p$set_params(list(x = 5, z = 3))
p$get_params()
#> $add1
#> $add1$y
#> [1] 3
#> 
#> 
#> $add2
#> $add2$y
#> [1] 3
#> 
#> 
#> $mult
#> $mult$x
#> [1] 5
#> 
#> $mult$z
#> [1] 3
#> 
#> 
suppressWarnings(
    p$set_params(list(foo = 3)) # gives warning as 'foo' is undefined
)
p$set_params(list(foo = 3), warnUndefined = FALSE)

## ------------------------------------------------
## Method `Pipeline$set_params_at_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = ~data, y = 2, z = 3) x + y)
p$set_params_at_step("add1", list(y = 5, z = 6))
p$get_params()
#> $add1
#> $add1$y
#> [1] 5
#> 
#> $add1$z
#> [1] 6
#> 
#> 
try(p$set_params_at_step("add1", list(foo = 3))) # foo not defined
#> Error : Unable to set parameter(s) foo at step add1 - candidates are y, z

## ------------------------------------------------
## Method `Pipeline$split()`
## ------------------------------------------------

# Example for two independent calculation paths
p <- Pipeline$new("pipe", data = 1)
p$add("f1", \(x = ~data) x)
p$add("f2", \(x = 1) x)
p$add("f3", \(x = ~f1) x)
p$add("f4", \(x = ~f2) x)
p$split()
#> [[1]]
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1    data [NULL]   FALSE     f1    New
#> 3:     f3      f1 [NULL]   FALSE     f3    New
#> 
#> [[2]]
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:     f2         [NULL]   FALSE     f2    New
#> 2:     f4      f2 [NULL]   FALSE     f4    New
#> 

# Example of split by three data sets
dataList <- list(a = 1, b = 2, c = 3)
p <- Pipeline$new("pipe")
p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
pipes <- p$set_data_split(dataList)$split()
pipes
#> [[1]]
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.a               [NULL]   FALSE      a      New
#> 2: add1.a        data.a [NULL]    TRUE      a Outdated
#> 3: mult.a data.a,add1.a [NULL]    TRUE      a Outdated
#> 
#> [[2]]
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.b               [NULL]   FALSE      b      New
#> 2: add1.b        data.b [NULL]    TRUE      b Outdated
#> 3: mult.b data.b,add1.b [NULL]    TRUE      b Outdated
#> 
#> [[3]]
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.c               [NULL]   FALSE      c      New
#> 2: add1.c        data.c [NULL]    TRUE      c Outdated
#> 3: mult.c data.c,add1.c [NULL]    TRUE      c Outdated
#> 

## ------------------------------------------------
## Method `Pipeline$unlock_step()`
## ------------------------------------------------

p <- Pipeline$new("pipe", data = 1)
p$add("add1", \(x = 1, data = ~data) x + data)
p$add("add2", \(x = 1, data = ~data) x + data)
p$lock_step("add1")
p$set_params(list(x = 3))
#> skipping setting parameters x at locked step 'add1'
p$get_params()
#> $add1
#> $add1$x
#> [1] 1
#> 
#> 
#> $add2
#> $add2$x
#> [1] 3
#> 
#> 
p$unlock_step("add1")
p$set_params(list(x = 3))
p$get_params()
#> $add1
#> $add1$x
#> [1] 3
#> 
#> 
#> $add2
#> $add2$x
#> [1] 3
#> 
#> 
```
