
#' @title Add pipeline step
#' @description A pipeline consists of a series of steps, which usually
#' are added one by one. Each step is made up of a function computing
#' something once the pipeline is run. This function can be an existing
#' R function (e.g. [mean()]) or an anonymous/lambda function specifically
#' defined for the pipeline. One useful feature is that function
#' parameters can refer to results of earlier pipeline steps using the
#' syntax `x = ~earlier_step_name` - see the Examples for more details.
#' @param pip `Pipeline` object
#' @param step `string` the name of the step. Each step name must
#' be unique.
#' @param fun `function` or name of the function to be applied at
#' the step. Both existing and anonymous/lambda functions can be used.
#' All function parameters must have default values. If a parameter
#' is missing a default value in the function signature, alternatively,
#' it can be set via the `params` argument (see Examples section with
#' [mean()] function).
#' @param params `list` list of parameters to set or overwrite
#' parameters of the passed function.
#' @param description `string` optional description of the step
#' @param group `string` output collected after pipeline execution
#' (see [pipe_collect_out()] is grouped by the defined group
#' names. By default, this is the name of the step, which comes in
#' handy when the pipeline is copy-appended multiple times to keep
#' the results of the same function/step grouped at one place.
#' @param keepOut `logical` if `FALSE` (default) the output of the
#' step is not collected when calling [pipe_collect_out()] after the pipeline
#' run. This option is used to only keep the results that matter
#' and skip intermediate results that are not needed. See also
#' function [pipe_collect_out()] for more details.
#' @return returns the `Pipeline` object invisibly
#' @examples
#' # Add steps with lambda functions
#' p <- pipe_new("myPipe", data = 1)
#' pipe_add(p, "s1", \(x = ~data) 2*x)  # use input data
#' pipe_add(p, "s2", \(x = ~data, y = ~s1) x * y)
#' try(pipe_add(p, "s2", \(z = 3) 3)) # error: step 's2' exists already
#' try(pipe_add(p, "s3", \(z = ~foo) 3)) # dependency 'foo' not found
#' p
#'
#' # Add step with existing function
#' p <- pipe_new("myPipe", data = c(1, 2, NA, 3, 4))
#' try(pipe_add(p, "calc_mean", mean))  # default value for x is missing
#' pipe_add(p, "calc_mean", mean, params = list(x = ~data, na.rm = TRUE))
#' p |> pipe_run() |> pipe_get_out("calc_mean")
#'
#' # Step description
#' p <- pipe_new("myPipe", data = 1:10)
#' pipe_add(p, "s1", \(x = ~data) 2*x, description = "multiply by 2")
#' print(p, verbose = TRUE) # print all columns including description
#'
#'
#' # Group output
#' p <- pipe_new("myPipe", data = data.frame(x = 1:2, y = 3:4))
#' pipe_add(p, "prep_x", \(data = ~data) data$x, group = "prep")
#' pipe_add(p, "prep_y", \(data = ~data) (data$y)^2, group = "prep")
#' pipe_add(p, "sum", \(x = ~prep_x, y = ~prep_y) x + y)
#' p |> pipe_run() |> pipe_collect_out()
#' @export
pipe_add <- function(
    pip,
    step,
    fun,
    params = list(),
    description = "",
    group = step,
    keepOut = FALSE
) {
    pip$add(
        step = step,
        fun = fun,
        params = params,
        description = description,
        group = group,
        keepOut = keepOut
    )

    if (is.function(fun)) {
        funcName <- as.character(substitute(fun))[[1]]
        index <- match(step, pip$get_step_names())
        pip$pipeline[index, "funcName"] <- funcName
    }

    invisible(pip)
}


#' @title Append two pipelines
#' @description When appending, `pipeflow` takes care of potential name
#' clashes with respect to step names and dependencies, that is, if
#' needed, it will automatically adapt step names and dependencies to
#' make sure they are unique in the merged pipeline.
#' @param pip `Pipeline` object to be appended to.
#' @param p `Pipeline` object to be appended.
#' @param outAsIn `logical` if `TRUE`, output of first pipeline is used
#' as input for the second pipeline.
#' @param tryAutofixNames `logical` if `TRUE`, name clashes are tried
#' to be automatically resolved by appending the 2nd pipeline's name.
#' Only set to `FALSE`, if you know what you are doing.
#' @param sep `string` separator used when auto-resolving step names
#' @return returns new combined `Pipeline` object.
#' @examples
#' # Append pipeline
#' p1 <- pipe_new("pipe1")
#' pipe_add(p1, "step1", \(x = 1) x)
#' p2 <- pipe_new("pipe2")
#' pipe_add(p2, "step2", \(y = 1) y)
#' p1 |> pipe_append(p2)
#'
#' # Append pipeline with potential name clashes
#' p3 <- pipe_new("pipe3")
#' pipe_add(p3, "step1", \(z = 1) z)
#' p1 |> pipe_append(p2) |> pipe_append(p3)
#'
#' # Use output of first pipeline as input for second pipeline
#' p1 <- pipe_new("pipe1", data = 8)
#' p2 <- pipe_new("pipe2")
#' pipe_add(p1, "square", \(x = ~data) x^2)
#' pipe_add(p2, "log2", \(x = ~data) log2(x))
#'
#' p12 <- p1 |> pipe_append(p2, outAsIn = TRUE)
#' p12 |> pipe_run() |> pipe_get_out("log2")
#' p12
#'
#' # Custom name separator for adapted step names
#' p1 |> pipe_append(p2, sep = "___")
#' @export
pipe_append <- function(
    pip,
    p,
    outAsIn = FALSE,
    tryAutofixNames = TRUE,
    sep = "."
) {
    pip$append(
        p = p,
        outAsIn = outAsIn,
        tryAutofixNames = tryAutofixNames,
        sep = sep
    )
}


#' @title Append string to all step names
#' @description Appends string to all step names and takes care
#' of updating step dependencies accordingly.
#' @param pip `Pipeline` object
#' @param postfix `string` to be appended to each step name.
#' @param sep `string` separator between step name and postfix.
#' @return returns the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe")
#' pipe_add(p, "step1", \(x = 1) x)
#' pipe_add(p, "step2", \(y = 1) y)
#' pipe_append_to_step_names(p, "new")
#' p
#' pipe_append_to_step_names(p, "foo", sep = "__")
#' p
#' @export
pipe_append_to_step_names <- function(
    pip,
    postfix,
    sep = "."
) {
    pip$append_to_step_names(postfix = postfix, sep = sep)
}


#' @title Clone pipeline
#' @description Creates a copy of a pipeline object.
#' @param pip `Pipeline` object
#' @param deep `logical` whether to perform a deep copy
#' @return returns the copied `Pipeline` object
#' @examples
#' p1 <- pipe_new("pipe")
#' pipe_add(p1, "step1", \(x = 1) x)
#' p2 <- pipe_clone(p1)
#' pipe_add(p2, "step2", \(y = 1) y)
#' p1
#' p2
#' @export
pipe_clone <- function(pip, deep = FALSE)
{
    pip$clone(deep = deep)
}


#' @title Collect structured output from entire pipeline
#' @description Collect outputs produced by the pipeline run.
#' Only steps that were not skipped contribute results.
#' The output is grouped by the user-defined group names
#' (see `group` parameter in function [pipe_add()]), which by default
#' are identical to the step names, that is, trivial groups of
#' size 1. Use `groupBy = "state"` to group results by the step's
#' state instead.
#' @param pip `Pipeline` object
#' @param groupBy `string` field of pipeline by which to group the
#' output.
#' @return `list` containing the output, named after the groups, which,
#' by default, are the steps.
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "step1", \(x = ~data) x + 2)
#' pipe_add(p, "step2", \(x = ~step1) x + 2)
#' pipe_run(p)
#' pipe_collect_out(p)
#'
#' # Grouped output
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "step1", \(x = ~data) x + 2, group = "add")
#' pipe_add(p, "step2", \(x = ~step1, y = 2) x + y, group = "add")
#' pipe_add(p, "step3", \(x = ~data) x * 3, group = "mult")
#' pipe_add(p, "step4", \(x = ~data, y = 2) x * y, group = "mult")
#' p
#'
#' pipe_run(p)
#' pipe_collect_out(p) |> str()
#'
#' # Grouped by state
#' pipe_set_params(p, list(y = 5))
#' p
#'
#' pipe_collect_out(p, groupBy = "state") |> str()
#' @export
pipe_collect_out <- function(pip, groupBy = c("group", "state"))
{
    pip$collect_out(groupBy = groupBy)
}


#' @title Discard steps from the pipeline
#' @description Discard all steps that match a given `pattern`.
#' @param pip `Pipeline` object
#' @param pattern `string` containing a regular expression (or
#' character string for `fixed = TRUE`) to be matched.
#' @param fixed `logical` If `TRUE`, `pattern` is a string to
#' be matched as is. Overrides all conflicting arguments.
#' @param recursive `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @param ... further arguments passed to [grep()].
#' @return the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(x = ~data) x + 1)
#' pipe_add(p, "add2", \(x = ~add1) x + 2)
#' pipe_add(p, "mult3", \(x = ~add1) x * 3)
#' pipe_add(p, "mult4", \(x = ~add2) x * 4)
#' p
#'
#' pipe_discard_steps(p, "mult")
#' p
#'
#' # Re-add steps
#' pipe_add(p, "mult3", \(x = ~add1) x * 3)
#' pipe_add(p, "mult4", \(x = ~add2) x * 4)
#' p
#'
#' # Discarding 'add1' does not work ...
#' try(pipe_discard_steps(p, "add1"))
#'
#' # ... unless we enforce to remove its downstream dependencies as well
#' pipe_discard_steps(p, "add1", recursive = TRUE)
#' p
#'
#' # Trying to discard non-existent steps is just ignored
#' pipe_discard_steps(p, "non-existent")
#' @export
pipe_discard_steps <- function(
    pip,
    pattern,
    recursive = FALSE,
    fixed = TRUE,
    ...
) {
    pip$discard_steps(
        pattern = pattern,
        recursive = recursive,
        fixed = fixed,
        ...
    )
}


#' @title Get data
#' @description Get the data set for the pipeline
#' @param pip `Pipeline` object
#' @return the output defined in the `data` step, which by default is
#' the first step of the pipeline
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_get_data(p)
#' pipe_set_data(p, 3:4)
#' pipe_get_data(p)
#' @export
pipe_get_data <- function(pip)
{
    pip$get_data()
}


#' @title Get step dependencies
#' @section Methods:
#' * `pipe_get_depends`: get all dependencies for all steps defined
#' in the pipeline
#' * `pipe_get_depends_down`: get all downstream dependencies of a
#' given step, by default descending recursively.
#' * `pipe_get_depends_up`: get all upstream dependencies of a
#' given step, by default descending recursively.
#' @param pip `Pipeline` object
#' @return
#' * `pipe_get_depends`: named list of dependencies for each step
#' * `pipe_get_depends_down`: list of downstream dependencies
#' * `pipe_get_depends_up`: list of downstream dependencies
#' @examples
#' # pipe_get_depends
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(x = ~data) x + 1)
#' pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
#' pipe_get_depends(p)
#'
#' # pipe_get_depends_down
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(x = ~data) x + 1)
#' pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
#' pipe_add(p, "mult3", \(x = ~add1) x * 3)
#' pipe_add(p, "mult4", \(x = ~add2) x * 4)
#' pipe_get_depends_down(p, "add1")
#' pipe_get_depends_down(p, "add1", recursive = FALSE)
#'
#' # pipe_get_depends_up
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(x = ~data) x + 1)
#' pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
#' pipe_add(p, "mult3", \(x = ~add1) x * 3)
#' pipe_add(p, "mult4", \(x = ~add2) x * 4)
#' pipe_get_depends_up(p, "mult4")
#' pipe_get_depends_up(p, "mult4", recursive = FALSE)
#' @rdname pipe_get_depends
#' @export
pipe_get_depends <- function(pip)
{
    pip$get_depends()
}


#' @param step `string` name of step
#' @param recursive `logical` if `TRUE`, dependencies of dependencies
#' are also returned.
#'
#' @rdname pipe_get_depends
#' @export
pipe_get_depends_down <- function(pip, step, recursive = TRUE)
{
    pip$get_depends_down(step = step, recursive = recursive)
}


#' @param step `string` name of step
#' @param recursive `logical` if `TRUE`, dependencies of dependencies
#' are also returned.
#' @rdname pipe_get_depends
#' @export
pipe_get_depends_up <- function(pip, step, recursive = TRUE)
{
    pip$get_depends_up(step = step, recursive = recursive)
}


#' @title Pipeline graph
#' @description Get the pipeline as a graph with nodes and edges.
#' @param pip `Pipeline` object
#' @param groups `character` if not `NULL`, only steps belonging to the
#' given groups are considered.
#' @return list with two data frames, one for nodes and one for edges
#' ready to be used with the [visNetwork::visNetwork()] function of the
#' \link[visNetwork]{visNetwork} package.
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
#' pipe_add(p, "add2", \(x = 1, y = ~add1) x + y)
#' pipe_add(p, "mult1", \(x = ~add1, y = ~add2) x * y)
#' graph <- pipe_get_graph(p)
#' graph
#'
#' if (require("visNetwork", quietly = TRUE)) {
#'     do.call(visNetwork, args = graph)
#' }
#' @export
pipe_get_graph <- function(pip, groups = NULL)
{
    pip$get_graph(groups = groups)
}


#' @title Get output of given step
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @return the output at the given step.
#' @seealso [pipe_collect_out()] to collect output of multiple steps.
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(x = ~data) x + 1)
#' pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
#' pipe_run(p)
#' pipe_get_out(p, "add1")
#' pipe_get_out(p, "add2")
#' @export
pipe_get_out <- function(pip, step)
{
    pip$get_out(step = step)
}


#' @title Get pipeline parameters
#' @description Retrieves unbound function parameters defined in
#' the pipeline where 'unbound' means parameters that are not linked
#' to other steps.
#'
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
#' paramater names starting with a dot) are ignored and thus not returned.
#' @return
#' * `pipe_get_params`: list of parameters, sorted and named by step -
#'    steps with no parameters are filtered out
#' * `pipe_get_params_at_step`: list of parameters at given step
#' * `pipe_get_params_unique`:  list of parameters where each parameter
#'   is only listed once. The values of the parameters will be the values
#'   of the first step where the parameters were defined, respectively.
#' @examples
#' # pipe_get_params
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
#' pipe_add(p, "add2", \(x = 1, y = 2, .z = 3) x + y + .z)
#' pipe_add(p, "add3", \() 1 + 2)
#' pipe_get_params(p, ) |> str()
#' pipe_get_params(p, ignoreHidden = FALSE) |> str()
#'
#' # pipe_get_params_at_step
#' pipe_get_params_at_step(p, "add2")
#' pipe_get_params_at_step(p, "add2", ignoreHidden = FALSE)
#' pipe_get_params_at_step(p, "add3")
#'
#' # pipe_get_params_unique
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
#' pipe_add(p, "add2", \(x = 1, y = 2, .z = 3) x + y + .z)
#' pipe_add(p, "mult1", \(x = 4, y = 5, .z = 6, b = ~add2) x * y * b)
#' pipe_get_params_unique(p)
#' pipe_get_params_unique(p, ignoreHidden = FALSE)
#'
#' @rdname pipe_get_params
#' @export
pipe_get_params <- function(pip, ignoreHidden = TRUE)
{
    pip$get_params(ignoreHidden = ignoreHidden)
}


#' @rdname pipe_get_params
#' @export
pipe_get_params_at_step <- function(pip, step, ignoreHidden = TRUE)
{
    pip$get_params_at_step(step = step, ignoreHidden = ignoreHidden)
}


#' @rdname pipe_get_params
#' @export
pipe_get_params_unique <- function(pip, ignoreHidden = TRUE)
{
    pip$get_params_unique(ignoreHidden = ignoreHidden)
}


#' @title Get step information
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @return
#' * `pipe_get_step`: `data.table` row containing the step
#' * `pipe_get_step_names`: `character` vector of step names
#' * `pipe_get_step_number`: the step number in the pipeline
#' * `pipe_has_step`: whether step exists
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
#' pipe_add(p, "add2", \(x = 1, y = 2, z = ~add1) x + y + z)
#' pipe_run(p)
#'
#' # pipe_get_step_names
#' pipe_get_step_names(p)
#'
#' # get_step_number
#' pipe_get_step_number(p, "add1")
#' pipe_get_step_number(p, "add2")
#'
#' # pipe_has_step
#' pipe_has_step(p, "add1")
#' pipe_has_step(p, "foo")
#'
#' # pipe_get_step
#' add1 <- pipe_get_step(p, "add1")
#' add1
#'
#' add1[["params"]]
#'
#' add1[["fun"]]
#'
#' try(p$get_step("foo")) # error: step 'foo' does not exist
#' @rdname step_info
#' @export
pipe_get_step <- function(pip, step)
{
    pip$get_step(step = step)
}


#' @title Get specific field of a step
#' @description Get a specific field/entry of a step
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @param what `string` name of the pipeline column to return
#' @return the requested entry at the given step
pipe_get_step_field <- function(pip, step, what)
{
    pip$get_step_field(step = step, what = what)
}


#' @rdname step_info
#' @export
pipe_get_step_names <- function(pip)
{
    pip$get_step_names()
}


#' @rdname step_info
#' @export
pipe_get_step_number <- function(pip, step)
{
    pip$get_step_number(step = step)
}


#' @rdname step_info
#' @export
pipe_has_step <- function(pip, step)
{
    pip$has_step(step = step)
}


#' @title Insert step
#' @param pip `Pipeline` object
#' @param afterStep `string` name of step after which to insert
#' @param step `string` name of step to insert
#' @param ... further arguments passed to [pipe_add()]
#'
#' @section Methods:
#' * `pipe_insert_after`: insert step after a certain step of the pipeline
#' * `pipe_insert_before`: insert step before a certain step of the pipeline
#'
#' @return returns the `Pipeline` object invisibly
#' @examples
#' # pipe_insert_after
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "f1", \(x = 1) x)
#' pipe_add(p, "f2", \(x = ~f1) x)
#' pipe_insert_after(p, "f1", step = "after_f1", \(x = ~f1) x)
#' p
#'
#' # insert_before
#' pipe_insert_before(p, "f2", step = "before_f2", \(x = ~f1) 2 * x)
#' p
#' @rdname pipe_insert
#' @export
pipe_insert_after <- function(pip, afterStep, step, ...)
{
    pip$insert_after(afterStep = afterStep, step = step, ...)
}


#' @param beforeStep `string` name of step before which to insert
#' @rdname pipe_insert
#' @export
pipe_insert_before <- function(pip, beforeStep, step, ...)
{
    pip$insert_before(beforeStep = beforeStep, step = step, ...)
}


#' @title Length of the pipeline
#' @param pip `Pipeline` object
#' @return `numeric` length of pipeline, that is, the total number of steps
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "f1", \(x = 1) x)
#' pipe_add(p, "f2", \(y = 1) y)
#' p
#' pipe_length(p)
#' @export
pipe_length <- function(pip)
{
    pip$length()
}


#' @title Lock/unlock steps
#' @description Locking a step means that both its parameters and its
#' output (given it has output) are locked such that neither
#' setting new pipeline parameters nor future pipeline runs can change
#' the current parameter and output content. To unlock a locked step,
#' use [pipe_unlock_step()].
#' @param pip `Pipeline` object
#' @param step `string` name of step to lock or unlock
#' @return the `Pipeline` object invisibly
#' @examples
#' # pipe_lock_step
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = 1, data = ~data) x + data)
#' pipe_add(p, "add2", \(x = 1, data = ~data) x + data)
#' pipe_run(p)
#' pipe_get_out(p, "add1")
#' pipe_get_out(p, "add2")
#' pipe_lock_step(p, "add1")
#'
#' pipe_set_data(p, 3)
#' pipe_set_params(p, list(x = 3))
#' pipe_run(p)
#' pipe_get_out(p, "add1")
#' pipe_get_out(p, "add2")
#'
#' # pipe_unlock_step
#' pipe_unlock_step(p, "add1")
#' pipe_set_params(p, list(x = 3))
#' pipe_run(p)
#' pipe_get_out(p, "add1")
#' @rdname pipe_lock_unlock
#' @export
pipe_lock_step <- function(pip, step)
{
    pip$lock_step(step = step)
}


#' @title Create new pipeline
#' @description A new pipeline is always initialized with one 'data' step,
#' which basically is a function returning the data.
#' @param name the name of the Pipeline
#' @param data optional data used at the start of the pipeline. The
#' data also can be set later using the [pipe_set_data()] function.
#' @param logger custom logger to be used for logging. If no logger
#' is provided, the default logger is used, which should be sufficient
#' for most use cases.
#' If you do want to use your own custom log function, you need to
#' provide a function that obeys the following form:
#'
#' `function(level, msg, ...) {
#'    your custom logging code here
#' }`
#'
#' The `level` argument is a string and will be one of `info`, `warn`,
#'  or `error`. The `msg` argument is a string containing the message
#' to be logged. The `...` argument is a list of named parameters,
#' which can be used to add additional information to the log message.
#' Currently, this is only used to add the context in case of a step
#' giving a warning or error.
#'
#' Note that with the default logger, the log layout can be altered
#' any time via [set_log_layout()].
#' @return returns the `Pipeline` object invisibly
#' @examples
#' data <- data.frame(x = 1:2, y = 3:4)
#' p <- pipe_new("myPipe", data = data)
#' p |> pipe_run() |> pipe_get_out("data")
#'
#' # Setting data later
#' p <- pipe_new("myPipe")
#' pipe_get_data(p)
#'
#' p <- pipe_set_data(p, data)
#' pipe_get_data(p)
#' p |> pipe_run() |> pipe_get_out("data")
#'
#' # Initialize with custom logger
#' my_logger <- function(level, msg, ...) {
#'    cat(level, msg, "\n")
#' }
#' p <- pipe_new("myPipe", data = data, logger = my_logger)
#' p |> pipe_run() |> pipe_get_out("data")
#' @export
pipe_new <- function(
    name,
    data = NULL,
    logger = NULL
) {
    Pipeline$new(name = name, data = data, logger = logger)
}


#' @title Pop steps from the pipeline
#' @description Use this function to drop steps from the end of the pipeline.
#' @section Methods:
#' * `pipe_pop_step`: drop last step from the pipeline
#' * `pipe_pop_steps_after`: drop all steps after given steps
#' * `pipe_pop_steps_from`: drop all steps from and including given steps
#' @return `string` the name of the step that was removed
#' @param pip `Pipeline` object
#' @examples
#' # pipe_pop_step
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "f1", \(x = 1) x)
#' pipe_add(p, "f2", \(y = 1) y)
#' p
#' pipe_pop_step(p)
#' p
#'
#' # pipe_pop_steps_after
#' pipe_add(p, "f2", \(y = 1) y)
#' pipe_add(p, "f3", \(z = 1) z)
#' p
#' pipe_pop_steps_after(p, "f1")
#' p
#'
#' # pipe_pop_steps_from
#' pipe_add(p, "f2", \(y = 1) y)
#' pipe_add(p, "f3", \(z = 1) z)
#' p
#' pipe_pop_steps_from(p, "f1")
#' p
#' @rdname pipe_pop_step
#' @export
pipe_pop_step <- function(pip)
{
    pip$pop_step()
}


#' @rdname pipe_pop_step
#' @param step `string` name of step
#' @export
pipe_pop_steps_after <- function(pip, step)
{
    pip$pop_steps_after(step = step)
}


#' @rdname pipe_pop_step
#' @param step `string` name of step
#' @export
pipe_pop_steps_from <- function(pip, step)
{
    pip$pop_steps_from(step = step)
}


#' @title Print the pipeline as a table
#' @param pip `Pipeline` object
#' @param verbose `logical` if `TRUE`, print all columns of the
#' pipeline, otherwise only the most relevant columns are displayed.
#' @return the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' p$add("f1", \(x = 1) x)
#' p$add("f2", \(y = 1) y)

#' pipe_print(p)
#' pipe_print(p, verbose = TRUE)
#'
#' # Also works with standard print function
#' print(p)
#' print(p, verbose = TRUE)
#' @export
pipe_print <- function(pip, verbose = FALSE)
{
    pip$print(verbose = verbose)
}


#' @title Remove certain step from the pipeline.
#' @description Can be used to remove any given step.
#' If other steps depend on the step to be removed, an error is
#' given and the removal is blocked, unless `recursive` was set to `TRUE`.
#' @param pip `Pipeline` object
#' @param step `string` the name of the step to be removed
#' @param recursive `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @return the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
#' pipe_add(p, "add2", \(x = 1, y = ~add1) x + y)
#' pipe_add(p, "mult1", \(x = 1, y = ~add2) x * y)
#' p
#'
#' pipe_remove_step(p, "mult1")
#' p
#'
#' try(pipe_remove_step(p, "add1"))
#' pipe_remove_step(p, "add1", recursive = TRUE)
#' p
#' @export
pipe_remove_step <- function(pip, step, recursive = FALSE)
{
    pip$remove_step(step = step, recursive = recursive)
}


#' @title Rename step
#' @description Safely rename a step in the pipeline. If new step
#' name would result in a name clash, an error is given.
#' @param pip `Pipeline` object
#' @param from `string` the name of the step to be renamed.
#' @param to `string` the new name of the step.
#' @return the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
#' pipe_add(p, "add2", \(x = 1, y = ~add1) x + y)
#' p
#'
#' try(pipe_rename_step(p, from = "add1", to = "add2"))
#'
#' pipe_rename_step(p, from = "add1", to = "first_add")
#' p
#' @export
pipe_rename_step <- function(pip, from, to)
{
    pip$rename_step(from = from, to = to)
}


#' @title Replace pipeline step
#' @description Replaces an existing pipeline step.
#' @param pip `Pipeline` object
#' @param step `string` the name of the step. Each step name must
#' be unique.
#' @param fun `function` or name of the function to be applied at
#' the step. Both existing and anonymous/lambda functions can be used.
#' All function parameters must have default values. If a parameter
#' is missing a default value in the function signature, alternatively,
#' it can be set via the `params` argument (see Examples section with
#' [mean()] function).
#' @param params `list` list of parameters to set or overwrite
#' parameters of the passed function.
#' @param description `string` optional description of the step
#' @param group `string` output collected after pipeline execution
#' (see function [pipe_collect_out()]) is grouped by the defined group
#' names. By default, this is the name of the step, which comes in
#' handy when the pipeline is copy-appended multiple times to keep
#' the results of the same function/step grouped at one place.
#' @param keepOut `logical` if `FALSE` (default) the output of the
#' step is not collected when calling [pipe_collect_out()] after the
#' pipeline run. This option is used to only keep the results that matter
#' and skip intermediate results that are not needed. See also
#' function [pipe_collect_out()] for more details.
#' @return returns the `Pipeline` object invisibly
#' @seealso [pipe_add()]
#' @examples
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
#' pipe_add(p, "add2", \(x = ~data, y = 2) x + y)
#' pipe_add(p, "mult", \(x = 1, y = 2) x * y, keepOut = TRUE)
#' pipe_run(p) |> pipe_collect_out()
#' pipe_replace_step(p, "mult", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
#' pipe_run(p) |> pipe_collect_out()
#' try(pipe_replace_step(p, "foo", \(x = 1) x))   # step 'foo' does not exist
#' @export
pipe_replace_step <- function(
    pip,
    step,
    fun,
    params = list(),
    description = "",
    group = step,
    keepOut = FALSE
) {
    pip$replace_step(
        step = step,
        fun = fun,
        params = params,
        description = description,
        group = group,
        keepOut = keepOut
    )

    if (is.function(fun)) {
        funcName <- as.character(substitute(fun))[[1]]
        index <- match(step, pip$get_step_names())
        pip$pipeline[index, "funcName"] <- funcName
    }


    invisible(pip)
}


#' @title  Reset pipeline
#' @description Resets the pipeline to the state before it was run.
#' This means that all output is removed and the state of all steps
#' is reset to 'New'.
#' @param pip `Pipeline` object
#' @return returns the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "f1", \(x = 1) x)
#' pipe_add(p, "f2", \(y = 1) y)
#' pipe_run(p, )
#' p
#'
#' pipe_reset(p)
#' p
#' @export
pipe_reset <- function(pip)
{
    pip$reset()
}


#' @title Run pipeline
#' @description Runs all new and/or outdated pipeline steps.
#' @param pip `Pipeline` object
#' @param force `logical` if `TRUE` all steps are run regardless of
#' whether they are outdated or not.
#' @param recursive `logical` if `TRUE` and a step returns a new
#' pipeline, the run of the current pipeline is aborted and the
#' new pipeline is run recursively.
#' @param progress `function` this parameter can be used to provide a
#' custom progress function of the form `function(value, detail)`,
#' which will show the progress of the pipeline run for each step,
#' where `value` is the current step number and `detail` is the name
#' of the step.
#' @param showLog `logical` should the steps be logged during the
#' pipeline run?
#' @return returns the `Pipeline` object invisibly
#' @examples
#' # Simple pipeline
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
#' pipe_add(p, "add2", \(x = ~add1, z = 2) x + z)
#' pipe_add(p, "final", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
#' p |> pipe_run() |> pipe_collect_out()

#' pipe_set_params(p, list(z = 4))  # outdates steps add2 and final
#' p
#'
#' p |> pipe_run() |> pipe_collect_out()
#'
#' # Recursive pipeline (for advanced users)
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
#' pipe_add(p, "new_pipe", \(x = ~add1) {
#'     p2 <- pipe_new("new_pipe", data = x)
#'     pipe_add(p2, "add1", \(x = ~data) x + 1)
#'     pipe_add(p2, "add2", \(x = ~add1) x + 2, keepOut = TRUE)
#'   }
#' )
#' p |> pipe_run() |> pipe_collect_out()
#'
#' # Run pipeline with progress bar
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "first step", \() Sys.sleep(0.5))
#' pipe_add(p, "second step", \() Sys.sleep(0.5))
#' pipe_add(p, "last step", \() Sys.sleep(0.5))
#' pb <- txtProgressBar(min = 1, max = pipe_length(p), style = 3)
#' fprogress <- function(value, detail) {
#'    setTxtProgressBar(pb, value)
#' }
#' pipe_run(p, progress = fprogress, showLog = FALSE)
#' @export
pipe_run <- function(
    pip,
    force = FALSE,
    recursive = TRUE,
    progress = NULL,
    showLog = TRUE
) {
    pip$run(
        force = force,
        recursive = recursive,
        progress = progress,
        showLog = showLog
    )
}


#' @title Run specific step
#' @description Run given pipeline step possibly together with
#' upstream and/or downstream dependencies.
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @param upstream `logical` if `TRUE`, run all dependent upstream
#' steps first.
#' @param downstream `logical` if `TRUE`, run all depdendent
#' downstream afterwards.
#' @return returns the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
#' pipe_add(p, "add2", \(x = ~add1, z = 2) x + z)
#' pipe_add(p, "mult", \(x = ~add1, y = ~add2) x * y)
#' pipe_run_step(p, "add2")
#'
#' pipe_run_step(p, "add2", downstream = TRUE)
#'
#' pipe_run_step(p, "mult", upstream = TRUE)
#' @export
pipe_run_step <- function(
    pip,
    step,
    upstream = TRUE,
    downstream = FALSE
) {
    pip$run_step(
        step = step,
        upstream = upstream,
        downstream = downstream
    )
}


#' @title Set data
#' @description Set data in first step of pipeline.
#' @param pip `Pipeline` object
#' @param data initial data set.
#' @return returns the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
#' p |> pipe_run() |> pipe_collect_out()
#'
#' pipe_set_data(p, 3)
#' p |> pipe_run() |> pipe_collect_out()
#' @export
pipe_set_data <- function(pip, data)
{
    pip$set_data(data = data)
}


#' @title Set pipeline parameters
#' @description Set unbound function parameters defined in
#' the pipeline where 'unbound' means parameters that are not linked
#' to other steps. Trying to set parameters that don't exist in
#' the pipeline is ignored, by default, with a warning.
#' @param pip `Pipeline` object
#' @param params `list` of parameters to be set
#' @param warnUndefined `logical` whether to give a warning when trying
#' to set a parameter that is not defined in the pipeline.
#' @return returns the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 2) x + y)
#' pipe_add(p, "add2", \(x = ~data, y = 3) x + y)
#' pipe_add(p, "mult", \(x = 4, z = 5) x * z)
#' pipe_get_params(p)
#' pipe_set_params(p, params = list(x = 3, y = 3))
#' pipe_get_params(p)
#' pipe_set_params(p, params = list(x = 5, z = 3))
#' pipe_get_params(p)
#'
#' suppressWarnings(
#'   pipe_set_params(p, list(foo = 3)) # gives warning as 'foo' is undefined
#' )
#' pipe_set_params(p, list(foo = 3), warnUndefined = FALSE)
#' @export
pipe_set_params <- function(pip, params, warnUndefined = TRUE)
{
    pip$set_params(params = params, warnUndefined = warnUndefined)
}


#' @title Set parameters at step
#' @description Set unbound function parameters defined at given pipeline
#' step where 'unbound' means parameters that are not linked to other
#' steps. If one or more parameters don't exist, an error is given.
#' @param pip `Pipeline` object
#' @param step `string` the name of the step
#' @param params `list` of parameters to be set
#' @return returns the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "add1", \(x = ~data, y = 2, z = 3) x + y)
#' pipe_set_params_at_step(p, step = "add1", params = list(y = 5, z = 6))
#' pipe_get_params(p)
#'
#' try(
#'   pipe_set_params_at_step(p, step = "add1", params = list(foo = 3))
#' )
#' @export
pipe_set_params_at_step <- function(pip, step, params)
{
    pip$set_params_at_step(step = step, params = params)
}


#' @title Split-up pipeline
#' @description Splits pipeline into its independent parts. This can be useful,
#' for example, to split-up the pipeline in order to run each part in parallel.
#' @param pip `Pipeline` object
#' @return list of `Pipeline` objects
#' @examples
#' # Example for two independent calculation paths
#' p <- pipe_new("pipe", data = 1)
#' pipe_add(p, "f1", \(x = ~data) x)
#' pipe_add(p, "f2", \(x = 1) x)
#' pipe_add(p, "f3", \(x = ~f1) x)
#' pipe_add(p, "f4", \(x = ~f2) x)
#' pipe_split(p)
#' @export
pipe_split <- function(pip)
{
    pip$split()
}


#' @title Skip/unskip pipeline group
#' @description Skips all steps that belong to the specified group.
#' Works like calling `skip_step` on every step in that group. Skipped
#' steps are not executed during `run()` and their outputs (if any)
#' are not considered for `collect_out()`.
#' @param pip `Pipeline` object
#' @param group `string` name of the group whose steps should be
#' skipped.
#' @return the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 15) |>
#'   pipe_add("f1", \(data = ~data, x = 1) data + x) |>
#'   pipe_add("log2", \(x = ~f1) log2(x), group = "prep") |>
#'   pipe_add("sqrt", \(x = ~log2) sqrt(x), group = "prep") |>
#'   pipe_add("final",  \(x = ~sqrt, y = ~f1) x + y)
#'
#' p |> pipe_run() |> pipe_collect_out()
#'
#' p |> pipe_set_params_at_step("f1", list(x = 5)) |> pipe_skip_group("prep")
#' p |> pipe_run() |> pipe_collect_out()
#'
#' p |> pipe_unskip_group("prep") |> pipe_run() |> pipe_collect_out()
#' @rdname pipe_skip_unskip_group
#' @export
pipe_skip_group <- function(pip, group)
{
    pip$skip_group(group = group)
}


#' @title Skip/unskip pipeline step
#' @description Skipping a step means that it is skipped during a
#' pipeline run and therefore it's output (if existing) remains
#' untouched. In addition, it is skipped when collecting output via
#' `collect_out`, that is, it's output will not be part of the
#' collected output list.
#' In contrast to `lock_step`, skipping a step does not "protect" the
#' step against changing the step's parameters.
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @return the `Pipeline` object invisibly
#' @examples
#' p <- pipe_new("pipe", data = 1) |>
#'   pipe_add("add1", \(x = 2, data = ~data) x + data) |>
#'   pipe_add("add2", \(x = 2, data = ~add1) x + data)
#'
#' p |> pipe_run() |> pipe_collect_out()
#'
#' p |> pipe_set_params(list(x = 3)) |> pipe_skip_step("add1")
#' p |> pipe_run() |> pipe_collect_out()
#'
#' p |> pipe_unskip_step("add1") |> pipe_run() |> pipe_collect_out()
#' @rdname pipe_skip_unskip_step
#' @export
pipe_skip_step <- function(pip, step)
{
    pip$skip_step(step = step)
}


#' @rdname pipe_lock_unlock
#' @export
pipe_unlock_step <- function(pip, step)
{
    pip$unlock_step(step = step)
}


#' @rdname pipe_skip_unskip_group
#' @export
pipe_unskip_group <- function(pip, group) {
    pip$unskip_group(group = group)
}


#' @rdname pipe_skip_unskip_step
#' @export
pipe_unskip_step <- function(pip, step) {
    pip$unskip_step(step = step)
}

# nocov end
