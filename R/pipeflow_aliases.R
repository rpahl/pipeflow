
# nocov start
#' @title Pipeline alias functions
#' @description Alias functions, one for each member function of a Pipeline
#'  object.
#' @param pip A pipeline object
#' @param ... Arguments passed to the respective pipeline method
#' @return The result of the respective pipeline method
#' @name pipelineAliases
NULL


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
#' p |> pipe_run() |> pipe_collect_out(all = TRUE)
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


#' @title Collect output from entire pipeline
#' @description Collects output afer pipeline run, by default, from all
#' steps for which `keepOut` was set to `TRUE` when steps were added
#' (see [pipe_add()]). The output is grouped by the group names (see
#' `group` parameter in [pipe_add()]),
#' which by default are set identical to the step names.
#' @param pip `Pipeline` object
#' @param groupBy `string` column of pipeline by which to group the
#' output.
#' @param all `logical` if `TRUE` all output is collected
#' regardless of the `keepOut` flag. This can be useful for debugging.
#' @return `list` containing the output, named after the groups, which,
#' by default, are the steps.
#' @examples
#' p <- pipe_new("pipe", data = 1:2)
#' pipe_add(p, "step1", \(x = ~data) x + 2)
#' pipe_add(p, "step2", \(x = ~step1) x + 2, keepOut = TRUE)
#' pipe_run(p)
#' pipe_collect_out(p)
#' pipe_collect_out(p, all = TRUE) |> str()
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
#' pipe_collect_out(p, all = TRUE) |> str()
#'
#' # Grouped by state
#' pipe_set_params(p, list(y = 5))
#' p
#'
#' pipe_collect_out(p, groupBy = "state", all = TRUE) |> str()
#' @export
pipe_collect_out <- function(pip, groupBy = "group", all = FALSE)
{
    pip$collect_out(groupBy = groupBy, all = all)
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
    pip$get_out(step)
}


#' @title Get pipeline parameters
#' @description Retrieves all unbound function parameters defined in
#' the pipeline where 'unbound' refers to parameters that are not pointing
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
#' * `get_params_unique_json`: flat unnamed json list of unique parameters
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
#' # get_params_unique_json
#' pipe_get_params_unique_json(p)
#' pipe_get_params_unique_json(p, ignoreHidden = FALSE)
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


#' @rdname pipe_get_params
#' @export
pipe_get_params_unique_json <- function(pip, ignoreHidden = TRUE)
{
    pip$get_params_unique_json(ignoreHidden = ignoreHidden)
}


#' @title Get step information
#' @param pip `Pipeline` object
#' @param step `string` name of step
#' @return
#' * `pipe_get_step`: `data.table` row containing the step
#' * `pipe_get_step_names`: `character` vector of step names
#' * `pipe_get_step_number`: the step number in the pipeline
#' * `pipe_get_step_number`: whether step exists
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
    pip$get_step(step)
}


#' @rdname step_info
#' @export
pipe_get_step_names <- function(pip, ...)
    pip$get_step_names(...)


#' @rdname step_info
#' @export
pipe_get_step_number <- function(pip, ...)
    pip$get_step_number(...)


#' @rdname step_info
#' @export
pipe_has_step <- function(pip, step)
{
    pip$has_step(step)
}


#' @rdname pipelineAliases
#' @export
pipe_insert_after <- function(pip, ...)
    pip$insert_after(...)


#' @rdname pipelineAliases
#' @export
pipe_insert_before <- function(pip, ...)
    pip$insert_before(...)


#' @rdname pipelineAliases
#' @export
pipe_length <- function(pip, ...)
    pip$length(...)


#' @rdname pipelineAliases
#' @export
pipe_lock_step <- function(pip, ...)
    pip$lock_step(...)

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
    Pipeline$new(name, data = data, logger = logger)
}


#' @rdname pipelineAliases
#' @export
pipe_print <- function(pip, ...)
    pip$print(...)


#' @rdname pipelineAliases
#' @export
pipe_pop_step <- function(pip, ...)
    pip$pop_step(...)


#' @rdname pipelineAliases
#' @export
pipe_pop_steps_after <- function(pip, ...)
    pip$pop_steps_after(...)


#' @rdname pipelineAliases
#' @export
pipe_pop_steps_from <- function(pip, ...)
    pip$pop_steps_from(...)


#' @rdname pipelineAliases
#' @export
pipe_remove_step <- function(pip, ...)
    pip$remove_step(...)

#' @rdname pipelineAliases
#' @export
pipe_rename_step <- function(pip, ...)
    pip$rename_step(...)


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


#' @rdname pipelineAliases
#' @export
pipe_reset <- function(pip, ...)
    pip$reset(...)


#' @rdname pipelineAliases
#' @export
pipe_run <- function(pip, ...)
    pip$run(...)


#' @rdname pipelineAliases
#' @export
pipe_run_step <- function(pip, ...)
    pip$run_step(...)


#' @rdname pipelineAliases
#' @export
pipe_set_data <- function(pip, ...)
    pip$set_data(...)


#' @rdname pipelineAliases
#' @export
pipe_set_data_split <- function(pip, ...)
    pip$set_data_split(...)


#' @rdname pipelineAliases
#' @export
pipe_set_keep_out <- function(pip, ...)
    pip$set_keep_out(...)


#' @rdname pipelineAliases
#' @export
pipe_set_params <- function(pip, ...)
    pip$set_params(...)


#' @rdname pipelineAliases
#' @export
pipe_set_params_at_step <- function(pip, ...)
    pip$set_params_at_step(...)


#' @rdname pipelineAliases
#' @export
pipe_split <- function(pip, ...)
    pip$split(...)


#' @rdname pipelineAliases
#' @export
pipe_unlock_step <- function(pip, ...)
    pip$unlock_step(...)

# nocov end
