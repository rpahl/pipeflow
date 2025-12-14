
#' @title Pipeline Class
#'
#' @description This class implements an analysis pipeline. A pipeline consists
#' of a sequence of analysis steps, which can be added one by one. Each added
#' step may or may not depend on one or more previous steps. The pipeline
#' keeps track of the dependencies among these steps and will ensure that
#' all dependencies are met on creation of the pipeline, that is, before the
#' the pipeline is run. Once the pipeline is run, the output is
#' stored in the pipeline along with each step and can be accessed later.
#' Different pipelines can be bound together while preserving all dependencies
#' within each pipeline.
#' @field name `string` name of the pipeline
#' @field pipeline `data.table` the pipeline where each row represents one step.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export
Pipeline = R6::R6Class(     # nolint cyclomatic complexity
    classname = "Pipeline",
    public = list(
        name = NULL,
        pipeline = NULL,

        #' @description constructor
        #' @param name the name of the Pipeline
        #' @param data optional data used at the start of the pipeline. The
        #' data also can be set later using the `set_data` function.
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
        #' p <- Pipeline$new("myPipe", data = data.frame(x = 1:8))
        #' p
        #'
        #' # Passing custom logger
        #' my_logger <- function(level, msg, ...) {
        #'    cat(level, msg, "\n")
        #' }
        #' p <- Pipeline$new("myPipe", logger = my_logger)
        initialize = function(
            name,
            data = NULL,
            logger = NULL
        ) {
            if (!is_string(name)) {
                stop_no_call("name must be a string")
            }

            if (!nzchar(name)) {
                stop_no_call("name must not be empty")
            }

            stopifnot(is.null(logger) || is.function(logger))
            if (is.null(logger)) {
                private$.lg <- lgr::get_logger(name = .this_package_name())$log
            }
            if (is.function(logger)) {
                expectedFormals <- c("level", "msg", "...")
                if (!setequal(names(formals(logger)), expectedFormals)) {
                    stop_no_call(
                        "logger function must have the following signature: ",
                        "function(", paste(expectedFormals, collapse = ", "),
                        ")"
                    )
                }
                private$.lg <- logger
            }

            self$name <- name
            self$pipeline <- private$._empty_pipeline()

            self$add("data", function() data, keepOut = FALSE)

            private$.reindex()

            invisible(self)
        },

        #' @description Add pipeline step
        #' @param step `string` the name of the step. Each step name must
        #' be unique.
        #' @param fun `function` or name of the function to be applied at
        #' the step. Both existing and anonymous/lambda functions can be used.
        #' All function parameters must have default values. If a parameter
        #' is missing a default value in the function signature, alternatively,
        #' it can be set via the `params` argument (see Examples section with
        #' [mean()] function).
        #'
        #' @param params `list` list of parameters to set or overwrite
        #' parameters of the passed function.
        #' @param description `string` optional description of the step
        #' @param group `string` output collected after pipeline execution
        #' (see function `collect_out`) is grouped by the defined group
        #' names. By default, this is the name of the step, which comes in
        #' handy when the pipeline is copy-appended multiple times to keep
        #' the results of the same function/step grouped at one place.
        #' @param keepOut `logical` if `FALSE` (default) the output of the
        #' step is not collected when calling `collect_out` after the pipeline
        #' run. This option is used to only keep the results that matter
        #' and skip intermediate results that are not needed. See also
        #' function `collect_out` for more details.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' # Add steps with lambda functions
        #' p <- Pipeline$new("myPipe", data = 1)
        #' p$add("s1", \(x = ~data) 2*x)  # use input data
        #' p$add("s2", \(x = ~data, y = ~s1) x * y)
        #' try(p$add("s2", \(z = 3) 3)) # error: step 's2' exists already
        #' try(p$add("s3", \(z = ~foo) 3)) # dependency 'foo' not found
        #' p
        #'
        #' # Add step with existing function
        #' p <- Pipeline$new("myPipe", data = c(1, 2, NA, 3, 4))
        #' p$add("calc_mean", mean, params = list(x = ~data, na.rm = TRUE))
        #' p$run()$get_out("calc_mean")
        #'
        #' # Step description
        #' p <- Pipeline$new("myPipe", data = 1:10)
        #' p$add("s1", \(x = ~data) 2*x, description = "multiply by 2")
        #' print(p)
        #' print(p, verbose = TRUE) # print all columns
        #'
        #' # Group output
        #' p <- Pipeline$new("myPipe", data = data.frame(x = 1:5, y = 1:5))
        #' p$add("prep_x", \(data = ~data) data$x, group = "prep")
        #' p$add("prep_y", \(data = ~data) (data$y)^2, group = "prep")
        #' p$add("sum", \(x = ~prep_x, y = ~prep_y) x + y)
        #' p$run()$collect_out()
        add = function(
            step,
            fun,
            params = list(),
            description = "",
            group = step,
            keepOut = FALSE
        ) {
            private$.verify_step_does_not_exist(step)
            stopifnot(
                is.function(fun) || is_string(fun),
                is.list(params),
                is_string(description),
                is_string(group) && nzchar(group),
                is.logical(keepOut)
            )

            if (is.function(fun)) {
                funcName <- as.character(substitute(fun))[[1]]
            }
            else {
                funcName <- fun
                fun <- get(fun, mode = "function")
            }

            params <- private$.prepare_and_verify_params(fun, funcName, params)

            # Derive and verify dependencies
            deps <- private$.derive_dependencies(
                params = params,
                step = step
            )
            sapply(deps, FUN = private$.verify_dependency, step = step)

            self$pipeline <- self$pipeline |>
                rbind(
                    list(
                        step = step,
                        fun = list(fun),
                        funcName = funcName,
                        params = list(params),
                        depends = list(deps),
                        out = list(NULL),
                        keepOut = keepOut,
                        group = group,
                        description = description,
                        time = Sys.time(),
                        state = "New",
                        locked = FALSE,
                        skip = FALSE
                    )
                )

            private$.reindex()

            invisible(self)
        },

        #' @description  Append another pipeline
        #' When appending, `pipeflow` takes care of potential name clashes with
        #' respect to step names and dependencies, that is, if needed, it will
        #' automatically adapt step names and dependencies to make sure they
        #' are unique in the merged pipeline.
        #' @param p `Pipeline` object to be appended.
        #' @param outAsIn `logical` if `TRUE`, output of first pipeline is used
        #' as input for the second pipeline.
        #' @param tryAutofixNames `logical` if `TRUE`, name clashes are tried
        #' to be automatically resolved by appending the 2nd pipeline's name.
        #' Only set to `FALSE`, if you know what you are doing.
        #' @param sep `string` separator used when auto-resolving step names
        #' @return returns new combined `Pipeline`.
        #' @examples
        #' # Append pipeline
        #' p1 <- Pipeline$new("pipe1")
        #' p1$add("step1", \(x = 1) x)
        #' p2 <- Pipeline$new("pipe2")
        #' p2$add("step2", \(y = 1) y)
        #' p1$append(p2)
        #'
        #' # Append pipeline with potential name clashes
        #' p3 <- Pipeline$new("pipe3")
        #' p3$add("step1", \(z = 1) z)
        #' p1$append(p2)$append(p3)
        #'
        #' # Use output of first pipeline as input for second pipeline
        #' p1 <- Pipeline$new("pipe1", data = 8)
        #' p2 <- Pipeline$new("pipe2")
        #' p1$add("square", \(x = ~data) x^2)
        #' p2$add("log2", \(x = ~data) log2(x))
        #'
        #' p12 <- p1$append(p2, outAsIn = TRUE)
        #' p12$run()$get_out("log2")
        #' p12
        #'
        #' # Custom name separator
        #' p1$append(p2, sep = "___")
        append = function(
            p,
            outAsIn = FALSE,
            tryAutofixNames = TRUE,
            sep = "."
        ) {
            stopifnot(
                inherits(p, "Pipeline"),
                is.logical(outAsIn),
                is_string(sep)
            )

            # Clone both pipelines and then append the 2nd to the 1st
            p1 <- self$clone()
            p2 <- p$clone()
            stepNames <- c(p1$get_step_names(), p2$get_step_names())

            # Handle name clashes
            if (any(duplicated(stepNames))) {
                duplicatedNames <- stepNames[duplicated(stepNames)]
                if (!tryAutofixNames) {
                    stop_no_call(
                        "combined pipeline would have duplicated step names: ",
                        paste0("'", duplicatedNames, "'", sep = ", ")
                    )
                }
                for (name in duplicatedNames) {
                    newName <- paste(name, p2$name, sep = sep)
                    if (self$has_step(newName)) {
                        stop_no_call(
                            "Cannot auto-fix name clash for step '",
                            name, "' in pipeline '",
                            p2$name, "'. Step '", newName,
                            "' already exists in pipeline '", self$name, "'."
                        )
                    }
                    p2$rename_step(from = name, to = newName)
                }
            }


            # Build combined pipeline
            combinedName <- paste0(p1$name, sep, p2$name)
            combinedPipe <- Pipeline$new(combinedName)
            combinedPipe$pipeline <- rbind(p1$pipeline, p2$pipeline)
            combinedPipe$.__enclos_env__$private$.reindex()

            if (outAsIn) {
                # Replace first step of p2, with the output of last step of p1
                stepNames <- combinedPipe$get_step_names()
                lastStep1 = stepNames[p1$length()]
                firstStep2 = stepNames[p1$length() + 1]
                combinedPipe$replace_step(
                    step = firstStep2,
                    fun = function(data = ~-1) data,
                    description = "output of last step of first pipeline",
                    keepOut = p2$pipeline[["keepOut"]][[1]]
                )
            }

            combinedPipe
        },

        #' @description Appends string to all step names and takes care
        #' of updating step dependencies accordingly.
        #' @param postfix `string` to be appended to each step name.
        #' @param sep `string` separator between step name and postfix.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe")
        #' p$add("step1", \(x = 1) x)
        #' p$add("step2", \(y = 1) y)
        #' p$append_to_step_names("new")
        #' p
        #' p$append_to_step_names("foo", sep = "__")
        #' p
        append_to_step_names = function(postfix, sep = ".") {

            stopifnot(is_string(postfix))

            add_postfix <- function(x) {
                if (length(x) == 0) {
                    return(x)
                }
                paste(x, postfix, sep = sep) |>
                    stats::setNames(names(x))
            }

            # Add postfix to step names and update dependencies accordingly
            steps <- self$get_step_names()
            deps <- self$get_depends()

            self$pipeline[["step"]] <- add_postfix(steps)
            self$pipeline[["depends"]] <- lapply(deps, add_postfix)

            private$.reindex()

            invisible(self)
        },


        #' @description Collect outputs produced by the pipeline run.
        #' Only steps that were not skipped contribute results.
        #' The output is grouped by the user-defined group names
        #' (see `group` parameter in function `add()`), which by default
        #' are identical to the step names, that is, trivial groups of
        #' size 1. Use `groupBy = "state"` to group results by the step's
        #' state instead.
        #' @param groupBy `string` field of pipeline by which to group the
        #' output.
        #' @return `list` containing the output, named after the groups, which,
        #' by default, are the steps.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("step1", \(x = ~data) x + 2)
        #' p$add("step2", \(x = ~step1) x + 2)
        #' p$run()
        #' p$collect_out()
        #'
        #' # Grouped output
        #' p <- Pipeline$new("pipe", data = 1:2)
        # nolint start
        #' p$add("step1", \(x = ~data) x + 2, group = "add")
        #' p$add("step2", \(x = ~step1, y = 2) x + y, group = "add")
        #' p$add("step3", \(x = ~data) x * 3, group = "mult")
        #' p$add("step4", \(x = ~data, y = 2) x * y, group = "mult")
        # nolint end
        #' p
        #' p$run()$collect_out() |> str()
        #'
        #' # Grouped by state
        #' p$set_params(list(y = 5))
        #' p
        #' p$collect_out(groupBy = "state") |> str()
        collect_out = function(groupBy = c("group", "state"))
        {
            stopifnot(
                is.character(groupBy)
            )
            groupBy <- match.arg(groupBy)
            hasField <- groupBy %in% colnames(self$pipeline)

            keepOut <- !self$pipeline[["skip"]]

            if (!any(keepOut)) {
                return(list())
            }

            collect_results <- function(pipeline) {
                results = as.list(pipeline[["out"]])
                names(results) = pipeline[["step"]]
                results
            }

            pipeOut <- self$pipeline[keepOut, ]
            result <- list()

            groupLabels <- pipeOut[[groupBy]]

            # Group output if at least two steps have the same group label
            groupings <- table(groupLabels) |>
                as.list() |>
                Filter(f = function(x) x > 1) |>
                unlist()

            hasGroupings <- length(groupings) > 0
            if (hasGroupings) {
                indices <- groupLabels %in% names(groupings)

                groupedRes <- pipeOut[indices, ] |>
                    split(f = groupLabels[indices]) |>
                    lapply(collect_results)

                result <- append(result, groupedRes)
            }

            # Ungrouped result
            isUngrouped <- !(groupLabels %in% names(groupings))
            ungroupedRes <- pipeOut[isUngrouped, ] |>
                collect_results() |>
                stats::setNames(groupLabels[isUngrouped])

            result <- append(result, ungroupedRes)

            # Keep original order of groups as they were defined
            orderedGroupLabels <- intersect(groupLabels, names(result))
            result |> .subset(orderedGroupLabels)
        },

        #' @description Discard all steps that match a given `pattern`.
        #' @param pattern `string` containing a regular expression (or
        #' character string for `fixed = TRUE`) to be matched.
        #' @param fixed `logical` If `TRUE`, `pattern` is a string to
        #' be matched as is. Overrides all conflicting arguments.
        #' @param recursive `logical` if `TRUE` the step is removed together
        #' with all its downstream dependencies.
        #' @param ... further arguments passed to [grep()].
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(x = ~data) x + 1)
        #' p$add("add2", \(x = ~add1) x + 2)
        #' p$add("mult3", \(x = ~add1) x * 3)
        #' p$add("mult4", \(x = ~add2) x * 4)
        #' p
        #'
        #' p$discard_steps("mult")
        #' p
        #'
        #' # Re-add steps
        #' p$add("mult3", \(x = ~add1) x * 3)
        #' p$add("mult4", \(x = ~add2) x * 4)
        #' p
        #' # Discarding 'add1' does not work ...
        #' try(p$discard_steps("add1"))
        #'
        #' # ... unless we enforce to remove its downstream dependencies as well
        #' p$discard_steps("add1", recursive = TRUE)   # this works
        #' p
        #'
        #' # Trying to discard non-existent steps is just ignored
        #' p$discard_steps("non-existent")
        discard_steps = function(
            pattern,
            recursive = FALSE,
            fixed = TRUE,
            ...
        ) {
            steps2remove = grep(
                pattern = pattern,
                x = self$get_step_names(),
                fixed = fixed,
                value = TRUE,
                ...
            )

            # To respect dependencies, remove steps from last to first
            for (step in rev(steps2remove)) {
                self$remove_step(step, recursive = recursive)
                message(sprintf("step '%s' was removed", step))
            }

            invisible(self)
        },

        #' @description Get data
        #' @return the output defined in the `data` step, which by default is
        #' the first step of the pipeline
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$get_data()
        #' p$set_data(3:4)
        #' p$get_data()
        get_data = function()
        {
            if (!self$has_step("data")) {
                stop_no_call("no data step defined")
            }

            self$get_step_field("data", "fun")()
        },

        #' @description Get all dependencies defined in the pipeline
        #' @return named `list` of dependencies for each step
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(x = ~data) x + 1)
        #' p$add("add2", \(x = ~data, y = ~add1) x + y)
        #' p$get_depends()
        get_depends = function()
        {
            self$pipeline[["depends"]] |>
                stats::setNames(self$get_step_names())
        },

        #' @description Get all downstream dependencies of given step, by
        #' default descending recursively.
        #' @param step `string` name of step
        #' @param recursive `logical` if `TRUE`, dependencies of dependencies
        #' are also returned.
        #' @return `list` of downstream dependencies
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(x = ~data) x + 1)
        #' p$add("add2", \(x = ~data, y = ~add1) x + y)
        #' p$add("mult3", \(x = ~add1) x * 3)
        #' p$add("mult4", \(x = ~add2) x * 4)
        #' p$get_depends_down("add1")
        #' p$get_depends_down("add1", recursive = FALSE)
        get_depends_down = function(
            step,
            recursive = TRUE
        ) {
            private$.verify_step_exists(step)

            deps <- private$.get_downstream_depends(
                step = step,
                depends = self$get_depends(),
                recursive = recursive
            )

            # Ensure order matches the step order of the pipeline
            intersect(self$get_step_names(), deps)
        },

        #' @description Get all upstream dependencies of given step, by
        #' default descending recursively.
        #' @param step `string` name of step
        #' @param recursive `logical` if `TRUE`, dependencies of dependencies
        #' are also returned.
        #' @return `list` of upstream dependencies
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(x = ~data) x + 1)
        #' p$add("add2", \(x = ~data, y = ~add1) x + y)
        #' p$add("mult3", \(x = ~add1) x * 3)
        #' p$add("mult4", \(x = ~add2) x * 4)
        #' p$get_depends_up("mult4")
        #' p$get_depends_up("mult4", recursive = FALSE)
        get_depends_up = function(
            step,
            recursive = TRUE
        ) {
            private$.verify_step_exists(step)

            deps <- private$.get_upstream_depends(
                step = step,
                depends = self$get_depends(),
                recursive = recursive
            )

            # Ensure order matches the step order of the pipeline
            intersect(self$get_step_names(), deps)
        },

        #' @description Visualize the pipeline as a graph.
        #' @param groups `character` if not `NULL`, only steps belonging to the
        #' given groups are considered.
        #' @return two data frames, one for nodes and one for edges ready to be
        #' used with the `visNetwork` package.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = ~add1) x + y)
        #' p$add("mult1", \(x = ~add1, y = ~add2) x * y)
        #' graph <- pipe_get_graph(p)
        #' graph
        #'
        #' if (require("visNetwork", quietly = TRUE)) {
        #'     do.call(visNetwork, args = p$get_graph())
        #' }
        get_graph = function(
            groups = NULL
        ) {
            nodes <- private$.create_node_table(groups = groups)
            edges <- private$.create_edge_table(groups = groups)

            list(nodes = nodes, edges = edges)
        },

        #' @description Get output of given step
        #' @param step `string` name of step
        #' @return the output at the given step.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(x = ~data) x + 1)
        #' p$add("add2", \(x = ~data, y = ~add1) x + y)
        #' p$run()
        #' p$get_out("add1")
        #' p$get_out("add2")
        get_out = function(step)
        {
            self$get_step_field(step, "out")
        },

        #' @description Set unbound function parameters defined in
        #' the pipeline where 'unbound' means parameters that are not linked
        #' to other steps. Trying #' to set parameters that don't exist in
        #' the pipeline is ignored, by default, with a warning.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of parameters, sorted and named by step. Steps with
        #' no parameters are filtered out.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
        #' p$add("add3", \() 1 + 2)
        #' p$get_params() |> str()
        #' p$get_params(ignoreHidden = FALSE) |> str()
        get_params = function(ignoreHidden = TRUE)
        {
            res <- lapply(
                self$pipeline[["step"]],
                FUN = self$get_params_at_step,
                ignoreHidden = ignoreHidden
            )

            names(res) <- self$pipeline[["step"]]
            Filter(res, f = function(x) length(x) > 0)
        },

        #' @description Get all unbound (i.e. not referring to other steps)
        #' at given step name.
        #' @param step `string` name of step
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of parameters defined at given step.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
        #' p$add("add3", \() 1 + 2)
        #' p$get_params_at_step("add2")
        #' p$get_params_at_step("add2", ignoreHidden = FALSE)
        #' p$get_params_at_step("add3")
        get_params_at_step = function(step, ignoreHidden = TRUE)
        {
            isValue <- function(x) !is.name(x) && !is.call(x)
            areHidden <- function(x) {
                startsWith(names(x), ".")
            }

            filter_desired_parameters = function(x) {
                if (length(x) == 0) {
                    return(x)
                }
                params <- Filter(x, f = isValue)
                if (ignoreHidden) {
                    params <- params[!areHidden(params)]
                }
                params
            }

            self$get_step_field(step, "params") |>
                filter_desired_parameters() |>
                as.list()
        },

        #' @description Get all unbound (i.e. not referring to other steps)
        #' parameters defined in the pipeline,
        #' but only list each parameter once. The values of the parameters,
        #' will be the values of the first step where the parameter was defined.
        #' This is particularly useful after the parameters where set using
        #' the `set_params` function, which will set the same value
        #' for all steps.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of unique parameters
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
        #' p$add("mult1", \(x = 1, y = 2, .z = 3, b = ~add2) x * y * b)
        #' p$get_params_unique()
        #' p$get_params_unique(ignoreHidden = FALSE)
        get_params_unique = function(ignoreHidden = TRUE)
        {
            params <- self$get_params(ignoreHidden)

            if (length(params) == 0) {
                return(params)
            }

            paramNames <- sapply(params, FUN = names) |> unlist()
            paramValues <- unlist1(params) |> stats::setNames(paramNames)
            paramValues[!duplicated(names(paramValues))]
        },

        #' @description Get step of pipeline
        #' @param step `string` name of step
        #' @return `data.table` row containing the step.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = 2, z = ~add1) x + y + z)
        #' p$run()
        #' add1 <- p$get_step("add1")
        #' print(add1)
        #' add1[["params"]]
        #' add1[["fun"]]
        #' try()
        #' try(p$get_step("foo")) # error: step 'foo' does not exist
        get_step = function(step)
        {
            i <- self$get_step_number(step)
            self$pipeline[i, ]
        },

        #' @description Get a specific field/entry of a step
        #' @param step `string` name of step
        #' @param what `string` name of the pipeline column to return
        #' @return the requested entry at the given step
        get_step_field = function(step, what)
        {
            stopifnot(is_string(what))

            available <- colnames(self$pipeline)
            if (!what %in% available) {
                stop_no_call("'", what, "' is not a field of the pipeline")
            }

            res <- self$get_step(step)[[what]]

            if (data.class(self$pipeline[[what]]) == "list") {
               return(res[[1]])
            }

            res
        },

        #' @description Get step names of pipeline
        #' @return `character` vector of step names
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$get_step_names()
        get_step_names = function()
        {
            self$pipeline[["step"]]
        },

        #' @description Get step number
        #' @param step `string` name of step
        #' @return the step number in the pipeline
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$get_step_number("f2")
        get_step_number = function(step)
        {
            private$.verify_step_exists(step)
            private$.idx[[step]]
        },

        #' @description Check if pipeline has given step
        #' @param step `string` name of step
        #' @return `logical` whether step exists
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$has_step("f2")
        #' p$has_step("foo")
        has_step = function(step)
        {
            stopifnot(
                is_string(step),
                nzchar(step)
            )

            !is.na(private$.idx[step])
        },

        #' @description Insert step after a certain step
        #' @param afterStep `string` name of step after which to insert
        #' @param step `string` name of step to insert
        #' @param ... further arguments passed to `add` method of the pipeline
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(x = ~f1) x)
        #' p$insert_after("f1", "f3", \(x = ~f1) x)
        #' p
        insert_after = function(
            afterStep,
            step,
            ...
        ) {
            private$.verify_step_does_not_exist(step)

            if (!self$has_step(afterStep)) {
                stop_no_call("step '", afterStep, "' does not exist")
            }

            pos <- self$get_step_number(afterStep)
            pip <- Pipeline$new(name = self$name)
            pip$pipeline <- self$pipeline[seq_len(pos), ]
            pip$.__enclos_env__$private$.reindex()
            pip$add(step = step, ...)

            if (pos < self$length()) {
                pip$pipeline <- rbind(
                    pip$pipeline,
                    self$pipeline[-seq_len(pos), ]
                )
            }

            self$pipeline <- pip$pipeline
            private$.reindex()

            invisible(self)
        },

        #' @description Insert step before a certain step
        #' @param beforeStep `string` name of step before which to insert
        #' @param step `string` name of step to insert
        #' @param ... further arguments passed to `add` method of the pipeline
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(x = ~f1) x)
        #' p$insert_before("f2", "f3", \(x = ~f1) x)
        #' p
        insert_before = function(
            beforeStep,
            step,
            ...
        ) {
            private$.verify_step_does_not_exist(step)

            if (!self$has_step(beforeStep)) {
                stop_no_call("step '", beforeStep, "' does not exist")
            }

            pos <- self$get_step_number(beforeStep) - 1
            if (pos == 0) {
                stop_no_call("cannot insert before first step")
            }

            pip <- Pipeline$new(name = self$name)

            pip$pipeline <- self$pipeline[seq_len(pos), ]
            pip$.__enclos_env__$private$.reindex()
            pip$add(step = step, ...)

            pip$pipeline <- rbind(
                pip$pipeline,
                self$pipeline[-seq_len(pos), ]
            )

            self$pipeline <- pip$pipeline
            private$.reindex()

            invisible(self)
        },

        #' @description Length of the pipeline aka number of pipeline steps.
        #' @return `numeric` length of pipeline.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$length()
        length = function() nrow(self$pipeline),

        #' @description Locking a step means that both its parameters and its
        #' output (given it has output) are locked such that neither
        #' setting new pipeline parameters nor future pipeline runs can change
        #' the current parameter and output content.
        #' @param step `string` name of step
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = 1, data = ~data) x + data)
        #' p$add("add2", \(x = 1, data = ~data) x + data)
        #' p$run()
        #' p$get_out("add1")
        #' p$get_out("add2")
        #' p$lock_step("add1")
        #'
        #' p$set_data(3)
        #' p$set_params(list(x = 3))
        #' p$run()
        #' p$get_out("add1")
        #' p$get_out("add2")
        lock_step = function(step) {
            private$.set_at_step(step, "locked", TRUE)
            invisible(self)
        },

        #' @description Drop last step from the pipeline.
        #' @return `string` the name of the step that was removed
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p
        #' p$pop_step() # "f2"
        #' p
        pop_step = function() {
            len <- self$length()
            lastStepName <- self$get_step_names()[len]

            self$pipeline <- self$pipeline[-len, ]
            private$.reindex()

            lastStepName
        },

        #' @description Drop all steps after the given step.
        #' @param step `string` name of step
        #' @return `character` vector of steps that were removed.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$add("f3", \(z = 1) z)
        #' p$pop_steps_after("f1")  # "f2", "f3"
        #' p
        pop_steps_after = function(step) {

            stepNumber <- self$get_step_number(step)

            if (stepNumber == self$length()) {
                return(character(0))    # nothing to remove
            }

            nextStep <- stepNumber + 1
            numbers2remove <- seq(from = nextStep, to = self$length())
            removedSteps <- self$pipeline[["step"]][numbers2remove]

            self$pipeline <- self$pipeline[-numbers2remove, ]
            private$.reindex()

            removedSteps
        },

        #' @description Drop all steps from and including the given step.
        #' @param step `string` name of step
        #' @return `character` vector of steps that were removed.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$add("f3", \(z = 1) z)
        #' p$pop_steps_from("f2")  # "f2", "f3"
        #' p
        pop_steps_from = function(step) {

            stepNumber <- self$get_step_number(step)

            numbers2remove <- seq(from = stepNumber, to = self$length())
            removedStepNames <- self$pipeline[["step"]][numbers2remove]

            self$pipeline <- self$pipeline[-numbers2remove, ]
            private$.reindex()

            removedStepNames
        },


        #' @description Print the pipeline as a table.
        #' @param verbose `logical` if `TRUE`, print all columns of the
        #' pipeline, otherwise only the most relevant columns are displayed.
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$print()
        print = function(verbose = FALSE) {
            if (verbose) {
                print(self$pipeline)
            } else {
                columns2print <- c(
                    "step", "depends", "out", "keepOut", "group", "state"
                )
                print(self$pipeline[, columns2print, with = FALSE])
            }
            invisible(self)
        },


        #' @description Remove certain step from the pipeline.
        #' If other steps depend on the step to be removed, an error is
        #' given and the removal is blocked, unless `recursive` was set to
        #' `TRUE`.
        #' @param step `string` the name of the step to be removed.
        #' @param recursive `logical` if `TRUE` the step is removed together
        #' with all its downstream dependencies.
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = ~add1) x + y)
        #' p$add("mult1", \(x = 1, y = ~add2) x * y)
        #' p$remove_step("mult1")
        #' p
        #' try(p$remove_step("add1"))  # fails because "add2" depends on "add1"
        #' p$remove_step("add1", recursive = TRUE)  # removes "add1" and "add2"
        #' p
        remove_step = function(
            step,
            recursive = FALSE
        ) {
            private$.verify_step_exists(step)

            deps <- self$get_depends_down(step, recursive)
            hasDeps <- length(deps) > 0

            if (hasDeps) {
                stepsString <- paste0("'", deps, "'", collapse = ", ")

                if (!recursive) {
                    stop_no_call(
                        "cannot remove step '", step, "' because the ",
                        "following steps depend on it: ", stepsString
                    )
                }

                # Remove all downstream dependencies starting from the end
                message(
                    "Removing step '", step, "' and its downstream ",
                    "dependencies: ", stepsString
                )
                sapply(rev(deps), FUN = self$remove_step)
            }

            stepNumber <- self$get_step_number(step)
            self$pipeline = self$pipeline[-stepNumber, ]
            private$.reindex()

            invisible(self)
        },

        #' @description Safely rename a step in the pipeline. If new step
        #' name would result in a name clash, an error is given.
        #' @param from `string` the name of the step to be renamed.
        #' @param to `string` the new name of the step.
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = ~add1) x + y)
        #' p
        #' try(p$rename_step("add1", "add2"))  # fails because "add2" exists
        #' p$rename_step("add1", "first_add")  # Ok
        #' p
        rename_step = function(
            from,
            to
        ) {
            private$.verify_step_exists(from)
            private$.verify_step_does_not_exist(to)

            self$pipeline[["step"]] <- pipeflow_replace_string(
                self$pipeline[["step"]],
                target = from,
                replacement = to
            )

            self$pipeline[["depends"]] <- lapply(
                self$pipeline[["depends"]],
                FUN = pipeflow_replace_string,
                target = from,
                replacement = to
            )
            private$.reindex()

            invisible(self)
        },

        #' @description Replaces an existing pipeline step.
        #' @param step `string` the name of the step to be replaced. Step must
        #' exist.
        #' @param fun `string` or `function` operation to be applied at the
        #' step. Both existing and lambda/anonymous functions can be used.
        #' @param params `list` list of parameters to overwrite default
        #' parameters of existing functions.
        #' @param description `string` optional description of the step
        #' @param group `string` grouping information (by default the same as
        #' the name of the step. Any output collected later (see function
        #' `collect_out` by default is put together by these group names. This,
        #' for example, comes in handy when the pipeline is copy-appended
        #' multiple times to keep the results of the same function/step at one
        #' place.
        #' @param keepOut `logical` if `FALSE` the output of the function will
        #' be cleaned at the end of the whole pipeline execution. This option
        #' is used to only keep the results that matter.
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y)
        #' p$add("add2", \(x = ~data, y = 2) x + y)
        #' p$add("mult", \(x = 1, y = 2) x * y, keepOut = TRUE)
        #' p$run()$collect_out()
        #' p$replace_step("mult", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
        #' p$run()$collect_out()
        #' try(p$replace_step("foo", \(x = 1) x))   # step 'foo' does not exist
        replace_step = function(
            step,
            fun,
            params = list(),
            description = "",
            group = step,
            keepOut = FALSE
        ) {
            private$.verify_step_exists(step)
            stopifnot(
                is.function(fun) || is_string(fun),
                is.list(params),
                is_string(description),
                is_string(group) && nzchar(group),
                is.logical(keepOut)
            )

            if (is.function(fun)) {
                funcName <- as.character(substitute(fun))[[1]]
            }
            else {
                funcName <- fun
                fun <- get(fun, mode = "function")
            }

            params <- private$.prepare_and_verify_params(fun, funcName, params)

            # Derive and verify dependencies
            allSteps <- self$get_step_names()
            stepNumber <- self$get_step_number(step)
            toStep <- allSteps[stepNumber - 1]

            deps <- private$.derive_dependencies(
                params = params,
                step = step,
                toStep = toStep
            )
            sapply(
                deps,
                FUN = private$.verify_dependency,
                step = step,
                toStep = toStep
            )

            newStep <- list(
                step = step,
                fun = list(fun),
                funcName = funcName,
                params = list(params),
                depends = list(deps),
                out = list(NULL),
                keepOut = keepOut,
                group = group,
                description = description,
                time = Sys.time(),
                state = "New",
                locked = FALSE,
                skip = FALSE
            )

            self$pipeline[stepNumber, ] <- newStep
            private$.update_states_downstream(step, state = "Outdated")
            private$.reindex()

            invisible(self)
        },

        #' @description Resets the pipeline to the state before it was run.
        #' This means that all output is removed and the state of all steps
        #' is reset to 'New'.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("f1", \(x = 1) x)
        #' p$add("f2", \(y = 1) y)
        #' p$run()
        #' p
        #' p$reset()
        #' p
        reset = function() {
            self$pipeline[["out"]] <- list(NULL)
            self$pipeline[["state"]] <- "New"
            invisible(self)
        },

        #' @description Run all new and/or outdated pipeline steps.
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
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y)
        #' p$add("add2", \(x = ~add1, z = 2) x + z)
        #' p$add("final", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
        #' p$run()$collect_out()
        #' p$set_params(list(z = 4))  # outdates steps add2 and final
        #' p
        #' p$run()$collect_out()
        #' p
        #'
        #' # Recursive pipeline
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y)
        #' p$add("new_pipe", \(x = ~add1) {
        #'     pp <- Pipeline$new("new_pipe", data = x)
        #'     pp$add("add1", \(x = ~data) x + 1)
        #'     pp$add("add2", \(x = ~add1) x + 2, keepOut = TRUE)
        #'     }
        #' )
        #' p$run(recursive = TRUE)$collect_out()
        #'
        #' # Run pipeline with progress bar
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("first step", \() Sys.sleep(1))
        #' p$add("second step", \() Sys.sleep(1))
        #' p$add("last step", \() Sys.sleep(1))
        #' pb <- txtProgressBar(min = 1, max = p$length(), style = 3)
        #' fprogress <- function(value, detail) {
        #'    setTxtProgressBar(pb, value)
        #' }
        #' p$run(progress = fprogress, showLog = FALSE)
        run = function(
            force = FALSE,
            recursive = TRUE,
            progress = NULL,
            showLog = TRUE
        ) {
            stopifnot(
                is.logical(force),
                is.logical(recursive),
                is.null(progress) || is.function(progress),
                is.logical(showLog)
            )

            log_info <- function(msg, ...) {
                if (showLog) {
                    private$.lg(level = "info", msg = msg, ...)
                }
            }

            sprintf("Start run of '%s' pipeline:", self$name) |> log_info()

            to <- self$length()
            for (i in seq(from = 1, to = to)) {
                step <- as.character(self$pipeline[i, "step"])
                if (is.function(progress)) {
                    progress(value = i, detail = step)
                }
                info <- sprintf("Step %i/%i %s", i, to, step)

                state <- tolower(self$get_step_field(step, "state"))
                isSkipped <- self$get_step_field(step, "skip")
                isLocked <- self$get_step_field(step, "locked")
                skipThis <- isLocked || isSkipped || (state == "done" && !force)

                if (skipThis) {
                    paste0(info, " - skip '", state, "' step") |> log_info()
                    next()
                }
                log_info(info)

                res <- private$.run_step(step)

                if (inherits(res, "Pipeline") && recursive) {
                    log_info("Abort pipeline execution and restart on new.")
                    self <- res
                    self$run(
                        force = force,
                        recursive = TRUE,
                        progress = progress,
                        showLog = showLog
                    )
                    return(invisible(self))
                }
            }

            log_info("Finished execution of steps.")

            log_info("Done.")
            invisible(self)
        },

        #' @description Run given pipeline step possibly together with
        #' upstream and downstream dependencies.
        #' @param step `string` name of step
        #' @param upstream `logical` if `TRUE`, run all dependent upstream
        #' steps first.
        #' @param downstream `logical` if `TRUE`, run all depdendent
        #' downstream afterwards.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y)
        #' p$add("add2", \(x = ~add1, z = 2) x + z)
        #' p$add("mult", \(x = ~add1, y = ~add2) x * y)
        #' p$run_step("add2")
        #' p$run_step("add2", downstream = TRUE)
        #' p$run_step("mult", upstream = TRUE)
        run_step = function(
            step,
            upstream = TRUE,
            downstream = FALSE
        ) {
            private$.verify_step_exists(step)
            stopifnot(
                is.logical(upstream),
                is.logical(downstream)
            )

            upstreamSteps <- character(0)
            steps <- step
            downstreamSteps <- character(0)

            if (upstream) {
                upstreamSteps <- self$get_depends_up(
                    step = step,
                    recursive = TRUE
                )
                steps <- c(upstreamSteps, step)
            }

            if (downstream) {
                downstreamSteps <- self$get_depends_down(
                    step = step,
                    recursive = TRUE
                )
                steps <- c(steps, downstreamSteps)
            }

            log_info <- function(msg, ...) {
                private$.lg(level = "info", msg = msg, ...)
            }

            nStep <- length(steps)
            sprintf("Start step run of '%s' pipeline:", self$name) |> log_info()
            for (s in steps) {
                state <- tolower(self$get_step_field(s, "state"))
                stream <- ""
                if (s %in% upstreamSteps) {
                    stream <- " (upstream)"
                }
                if (s %in% downstreamSteps) {
                    stream <- " (downstream)"
                }

                iStep <- match(s, steps)
                info <- sprintf("Step %i/%i %s%s",  iStep, nStep, s, stream)

                isSkipped <- self$get_step_field(s, "skip")
                isLocked <- self$get_step_field(s, "locked")
                skipThis <- isLocked || isSkipped

                if (skipThis) {
                    paste0(info, " - skip '", state, "' step") |> log_info()
                    next()
                }
                log_info(info)

                private$.run_step(s)
            }

            log_info("Finished execution of steps.")
            log_info("Done.")
            invisible(self)
        },

        #' @description Set data in first step of pipeline.
        #' @param data initial data set
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
        #' p$run()$collect_out()
        #' p$set_data(3)
        #' p$run()$collect_out()
        set_data = function(data)
        {
            step <- self$get_step_names()[1]
            self$replace_step(
                step = step,
                fun = function() data,
                keepOut = FALSE
            )
        },


        #' @description Set parameters in the pipeline. If a parameter occurs
        #' in several steps, the parameter is set commonly in all steps.
        #' Trying to set parameters that don't exist in the pipeline is ignored,
        #' by default, with a warning.
        #' @param params `list` of parameters to be set
        #' @param warnUndefined `logical` whether to give a warning when trying
        #' to set a parameter that is not defined in the pipeline.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 2) x + y)
        #' p$add("add2", \(x = ~data, y = 3) x + y)
        #' p$add("mult", \(x = 4, z = 5) x * z)
        #' p$get_params()
        #' p$set_params(list(x = 3, y = 3))
        #' p$get_params()
        #' p$set_params(list(x = 5, z = 3))
        #' p$get_params()
        #' suppressWarnings(
        #'     p$set_params(list(foo = 3)) # gives warning as 'foo' is undefined
        #' )
        #' p$set_params(list(foo = 3), warnUndefined = FALSE)
        set_params = function(params, warnUndefined = TRUE)
        {
            if (!is.list(params)) {
                stop_no_call("params must be a list")
            }
            definedParams <- self$get_params_unique(ignoreHidden = FALSE)
            extra <- setdiff(names(params), names(definedParams))

            if (warnUndefined && length(extra) > 0) {
                warning(
                    "Trying to set parameters not defined in the pipeline: ",
                    toString(extra)
                )
            }

            for (step in self$get_step_names()) {
                paramsAtStep <- self$get_params_at_step(
                    step,
                    ignoreHidden = FALSE
                )
                overlap <- intersect(names(params), names(paramsAtStep))

                if (length(overlap) > 0) {
                    paramsAtStep[overlap] <- params[overlap]
                    self$set_params_at_step(step, paramsAtStep)
                }
            }
            invisible(self)
        },

        #' @description Set unbound function parameters defined at given
        #' pipeline step where 'unbound' means parameters that are not
        #' linked to other steps.
        #' @param step `string` the name of the step
        #' @param params `list` of parameters to be set
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 2, z = 3) x + y)
        #' p$set_params_at_step("add1", list(y = 5, z = 6))
        #' p$get_params()
        #' try(p$set_params_at_step("add1", list(foo = 3))) # foo not defined
        set_params_at_step = function(
            step,
            params
        ) {
            stopifnot(
                is_string(step),
                is.list(params)
            )

            if (self$get_step_field(step, "locked")) {
                message(
                    "skipping setting parameters ",
                    paste0(names(params), collapse = ", "),
                    " at locked step '", step, "'"
                )
                return(invisible(self))
            }

            current <- self$get_params_at_step(step, ignoreHidden = FALSE)

            extra <- setdiff(names(params), names(current))
            if (length(extra) > 0) {
                stop_no_call(
                    "Unable to set parameter(s) ", toString(extra),
                    " at step ", step, " - candidates are ",
                    toString(names(current))
                )
            }

            toUpdate <- intersect(names(params), names(current))
            hasUpdate <- length(toUpdate) > 0

            if (hasUpdate) {
                # Update params
                current <- self$get_step_field(step, "params")
                current[toUpdate] <- params[toUpdate]
                private$.set_at_step(step, "params", value = current)

                # Update state if applicable
                state <- tolower(self$get_step_field(step, "state"))
                if (state == "done") {
                    private$.set_at_step(step, "state", "Outdated")
                    private$.update_states_downstream(step, "Outdated")
                }
            }

            invisible(self)
        },

        #' @description Skips all steps that belong to the specified group.
        #' Works like calling `skip_step` on every step in that group. Skipped
        #' steps are not executed during `run()` and their outputs (if any)
        #' are not considered for `collect_out()`.
        #' @param group `string` name of the group whose steps should be
        #' skipped.
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 15)
        #' p$add("f1", \(data = ~data, x = 1) data + x, keepOut = TRUE)
        #' p$add("log2", \(x = ~f1) log2(x), group = "prep")
        #' p$add("sqrt", \(x = ~log2) sqrt(x), group = "prep")
        #' p$add("final",  \(x = ~sqrt, y = ~f1) x + y, keepOut = TRUE)
        #' p$run()$collect_out()
        #'
        #' p$set_params_at_step("f1", list(x = 5))$skip_group("prep")
        #' p$run()$collect_out()
        #' p$unskip_group("prep")$run()$collect_out()
        skip_group = function(group) {
            steps <- private$.get_steps_in_group(group)
            lapply(steps, \(step) self$skip_step(step))
            invisible(self)
        },

        #' @description Skipping a step means that it is skipped during a
        #' pipeline run and therefore it's output (if existing) remains
        #' untouched. In addition, it is skipped when collecting output via
        #' `collect_out`, that is, it's output will not be part of the
        #' collected output list.
        #' In contrast to `lock_step`, skipping a step does not "protect" the
        #' step against changing the step's parameters.
        #' @param step `string` name of step
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = 2, data = ~data) x + data, keepOut = TRUE)
        #' p$add("add2", \(x = 2, data = ~add1) x + data, keepOut = TRUE)
        #' p$run()$collect_out()
        #'
        #' p$skip_step("add1")$set_params(list(x = 3))
        #' p$run()$collect_out()
        skip_step = function(step) {
            isSkipped <- self$get_step_field(step, "skip")
            if (isSkipped) {
                message("Step '", step, "' was already skipped - skip ignored")
            }

            private$.set_at_step(step, "skip", TRUE)
            invisible(self)
        },


        #' @description Splits pipeline into its independent parts.
        #' @return list of `Pipeline` objects
        #' @examples
        #' # Example for two independent calculation paths
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("f1", \(x = ~data) x)
        #' p$add("f2", \(x = 1) x)
        #' p$add("f3", \(x = ~f1) x)
        #' p$add("f4", \(x = ~f2) x)
        #' p$split()
        split = function()
        {
            groups <- private$.get_depends_grouped()
            name <- self$name
            newNames <- paste0(name, seq_along(groups))
            pips <- lapply(newNames, \(name) Pipeline$new(name))

            for (i in seq_along(groups)) {
                pip <- pips[[i]]
                stepNumbers <- which(self$pipeline[["step"]] %in% groups[[i]])
                pip$pipeline <- self$pipeline[stepNumbers, ]
            }

            pips
        },

        #' @description Unlock previously locked step. If step was not locked,
        #' the command is ignored and a message is printed hinting at that.
        #' @param step `string` name of step
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = 1, data = ~data) x + data)
        #' p$add("add2", \(x = 1, data = ~data) x + data)
        #' p$lock_step("add1")
        #' p$set_params(list(x = 3))
        #' p$get_params()
        #' p$unlock_step("add1")
        #' p$set_params(list(x = 3))
        #' p$get_params()
        unlock_step = function(step) {
            isLocked <- isTRUE(self$get_step_field(step, "locked"))
            if (isLocked) {
                private$.set_at_step(step, "locked", FALSE)
            } else {
                message("Step '", step, "' was not locked - unlock ignored")
            }

            invisible(self)
        },

        #' @description Unskips all steps that belong to the specified group.
        #' Works like calling `unskip_step` on every step in that group.
        #' @param group `string` name of the group whose steps should be
        #' unskipped.
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 15)
        #' p$add("f1", \(data = ~data, x = 1) data + x, keepOut = TRUE)
        #' p$add("log2", \(x = ~f1) log2(x), group = "prep")
        #' p$add("sqrt", \(x = ~log2) sqrt(x), group = "prep")
        #' p$add("final",  \(x = ~sqrt, y = ~f1) x + y, keepOut = TRUE)
        #' p$run()$collect_out()
        #'
        #' p$set_params_at_step("f1", list(x = 5))$skip_group("prep")
        #' p$run()$collect_out()
        #' p$unskip_group("prep")$run()$collect_out()
        unskip_group = function(group) {
            steps <- private$.get_steps_in_group(group)
            lapply(steps, \(step) self$unskip_step(step))
            invisible(self)
        },

        #' @description Unskip previously skipped step. If step was not skipped,
        #' the command is ignored, but a warning is given.
        #' @param step `string` name of step
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = 2, data = ~data) x + data, keepOut = TRUE)
        #' p$add("add2", \(x = 2, data = ~add1) x + data, keepOut = TRUE)
        #' p$run()$collect_out()
        #'
        #' p$skip_step("add1")$set_params(list(x = 3))
        #' p$run()$collect_out()
        #'
        #' p$unskip_step("add1")$run()$collect_out()
        unskip_step = function(step) {
            wasSkipped <- self$get_step(step)[["skip"]]
            private$.set_at_step(step, "skip", FALSE)

            if (!isTRUE(wasSkipped)) {
                message("Step '", step, "' was not skipped")
            }
            invisible(self)
        }
    ),

    private = list(
        deep_clone = function(name, value) {
            if (name == "pipeline")
                return(data.table::copy(value))

            value
        },

        .idx = numeric(),   # step name to row index mapping
        .lg = NULL,         # the logger function

        ._empty_pipeline = function() {
            data.table::data.table(
                step = character(0),
                fun = list(),
                funcName = character(0),
                params = list(),
                depends = list(),
                out = list(),
                keepOut = logical(0),
                group = character(0),
                description = character(0),
                time = as.POSIXct(character(0)),
                state = character(0),
                locked = logical(0),
                skip = logical(0)
            )
        },

        .create_edge_table = function(
            groups = NULL
        ) {
            deps <- self$get_depends()

            if (!is.null(groups)) {
                # Only use dependencies defined by the given groups
                stopifnot(
                    is.character(groups),
                    all(groups %in% self$pipeline[["group"]])
                )
                deps <- deps[self$pipeline[["group"]] %in% groups]
            }

            deps <- Filter(deps, f = function(x) length(x) > 0)

            if (length(deps) == 0) {
                return(
                    data.frame(
                        from = numeric(0),
                        to = numeric(0),
                        arrows = character()
                    )
                )
            }

            create_edges_for_step <- function(step, depsAtStep) {
                if (is.list(depsAtStep)) {
                    # Handle nested dependencies, for example, when combining
                    # after data split
                    depsAtStep <- unlist(depsAtStep)
                }
                data.frame(
                    from = sapply(depsAtStep, FUN = self$get_step_number),
                    to = self$get_step_number(step)
                )
            }

            edges <- mapply(
                step = names(deps),
                depsAtStep = deps,
                FUN = create_edges_for_step,
                SIMPLIFY = FALSE
            ) |>
                do.call(what = rbind, args = _) |>
                replace("arrows", value = "to")

            edges[order(edges[["from"]], edges[["to"]]), ]
        },

        .create_node_table = function(
            groups = NULL
        ) {
            pip <- self$pipeline

            nodes <- data.frame(
                "id" = seq_len(self$length()),
                "label" = self$get_step_names(),
                "group" = pip[["group"]],
                "shape" = "box",
                "color" = "grey",
                "title" = paste0("<p>", pip[["description"]], "</p>")
            )

            nodes[["color"]][pip[["state"]] == "New"] <- "lightblue"
            nodes[["color"]][pip[["state"]] == "Done"] <- "lightgreen"
            nodes[["color"]][pip[["state"]] == "Outdated"] <- "orange"
            nodes[["color"]][pip[["state"]] == "Failed"] <- "red"
            areDataNodes <- nodes[, "label"] |> startsWith("data")
            if (any(areDataNodes)) {
                nodes[areDataNodes, "shape"] <- "database"
            }
            nodes[["shape"]][pip[["keepOut"]]] <- "circle"

            if (!is.null(groups)) {
                stopifnot(
                    is.character(groups),
                    all(groups %in% self$pipeline[["group"]])
                )
                nodes <- nodes[self$pipeline[["group"]] %in% groups, ]
            }

            nodes
        },

        .derive_dependencies = function(
            params,
            step,
            toStep = private$.get_last_step()
        ) {
            if (length(toStep) == 0) {
                return(character())
            }

            stopifnot(
                is_string(step),
                is_string(toStep)
            )

            private$.verify_step_exists(toStep)

            deps <- private$.extract_depends_from_param_list(params)

            # Handle any relative dependencies
            allSteps <- self$get_step_names()
            toIndex <- self$get_step_number(toStep)
            consideredSteps <- allSteps[seq_len(toIndex)]
            relativeDeps <- deps |>
                Filter(f = function(x) startsWith(x, "-")) |>
                sapply(as.numeric)

            stepIndices <- mapply(
                relative_dep = relativeDeps,
                dependencyName = names(relativeDeps),
                FUN = private$.relative_dependency_to_index,
                MoreArgs = list(
                    startIndex = toIndex + 1,
                    step = step
                )
            ) |> as.integer()
            deps[names(relativeDeps)] <- consideredSteps[stepIndices]

            deps
        },

        .extract_dependent_out = function(
            depends,
            out
        ) {
            stopifnot(
                is.character(depends) || is.list(depends),
                is.list(out),
                all(unlist(depends) %in% names(out))
            )

            if (length(depends) == 0) {
                return(NULL)
            }

            extract_out <- function(x) {
                if (length(x) == 1) {
                    out[[x]]
                } else {
                    # If multiple dependencies are given, return a list
                    out[x]
                }
            }

            lapply(depends, FUN = extract_out)
        },

        .extract_depends_from_param_list = function(
            params
        ) {
            if (length(params) == 0) {
                return(character())
            }

            stopifnot(is.list(params))

            # Extract the dependency name from the formula, that is, ~x
            # becomes "x" and ~-1 becomes -1
            deps <- params |>
                Filter(f = function(x) methods::is(x, "formula")) |>
                sapply(FUN = function(x) deparse(x[[2]]))

            if (length(deps) == 0) {
                return(character()) # otherwise named list() would be returned
            }

            deps
        },

        .get_depends_grouped = function()
        {
            res <- list()
            steps <- self$get_step_names()

            for (step in rev(steps)) {
                if (!step %in% unlist(res)) {
                    stepGroup <- self$get_step_names() |>
                        intersect(c(step, self$get_depends_up(step)))
                    res <- c(list(stepGroup), res)
                }
            }
            res
        },

        .get_downstream_depends = function(
            step,
            depends,
            recursive = TRUE
        ) {
            stopifnot(
                is_string(step),
                is.character(depends) || is.list(depends),
                is.logical(recursive)
            )

            if (length(depends) == 0) {
                return(character())
            }

            # Build reverse adjacency lookup (dep -> steps that depend on it)
            rev_adj <- list()
            dn <- names(depends)
            for (i in seq_along(depends)) {
                s  <- dn[[i]]
                ds <- private$.normalize_depvec(depends[[i]])
                if (!length(ds)) next
                for (d in ds) {
                    key <- d
                    rev_adj[[key]] <- c(rev_adj[[key]], s)
                }
            }

            key <- as.character(step)
            if (!recursive) {
                return(
                    unique(
                        rev_adj[[key]] %||% character()
                    )
                )
            }

            # BFS from immediate children outwards
            q <- rev_adj[[key]] %||% character()
            head <- 1L
            seen <- new.env(parent = emptyenv(), hash = TRUE)
            out  <- character()

            while (head <= length(q)) {
                s <- q[[head]]
                head <- head + 1L
                if (!exists(s, envir = seen, inherits = FALSE)) {
                    assign(s, TRUE, envir = seen)
                    out <- c(out, s)
                    nbrs <- rev_adj[[s]] %||% character()
                    if (length(nbrs)) q <- c(q, nbrs)
                }
            }

            out
        },

        .get_last_step = function() {
            self$get_step_names() |> utils::tail(1)
        },

        .get_steps_in_group = function(group) {
            stopifnot(is_string(group), nzchar(group))
            groups <- self$pipeline[["group"]]
            if (!(group %in% groups)) {
                stop("group '", group, "' does not exist", call. = FALSE)
            }
            self$pipeline[["step"]][groups == group]
        },

        .get_upstream_depends = function(
            step,
            depends,
            recursive = TRUE
        ) {
            stopifnot(
                is_string(step),
                is.character(depends) || is.list(depends),
                is.logical(recursive)
            )

            if (length(depends) == 0) {
                return(character())
            }

            if (!recursive) {
                return(
                    unique(
                        private$.normalize_depvec(
                            depends[[step]] %||% character()
                        )
                    )
                )
            }

            # BFS from immediate parents outward
            q <- private$.normalize_depvec(depends[[step]] %||% character())
            head <- 1L
            seen <- new.env(parent = emptyenv(), hash = TRUE)
            out  <- character()

            while (head <= length(q)) {
                s <- as.character(q[[head]])
                head <- head + 1L
                if (!exists(s, envir = seen, inherits = FALSE)) {
                    assign(s, TRUE, envir = seen)
                    out <- c(out, s)
                    next_up <- private$.normalize_depvec(
                        depends[[s]] %||% character()
                    )
                    if (length(next_up)) q <- c(q, next_up)
                }
            }

            out
        },

        .normalize_depvec = function(x) {
            if (is.null(x) || length(x) == 0) return(character())
            if (is.list(x)) return(unlist(x, use.names = FALSE))
            as.character(x)
        },

        .prepare_and_verify_params = function(
            fun,
            funcName,
            params = list()
        ) {
            stopifnot(
                is.function(fun),
                is_string(funcName),
                is.list(params)
            )

            params <- replace(formals(fun), names(params), params)
            params <- params[!names(params) %in% "..."] # ignore dots

            private$.verify_fun_params(fun, funcName, as.list(params))
            params <- lapply(params, eval)

            params
        },

        .reindex = function() {
            private$.idx <- seq_len(nrow(self$pipeline))
            names(private$.idx) <- self$pipeline[["step"]]
        },

        .relative_dependency_to_index = function(
            relative_dep,
            dependencyName,
            startIndex,
            step   # required for error message
        ) {
            stopifnot(
                is_number(relative_dep),
                relative_dep < 0,
                is_string(dependencyName),
                is_number(startIndex),
                startIndex > 0,
                is_string(step)
            )

            absIndex <- relative_dep + startIndex

            if (absIndex < 1) {
                stop_no_call(
                    "step '", step, "': relative dependency ",
                    paste0(dependencyName, "=", relative_dep),
                    " points to outside the pipeline"
                )
            }

            absIndex
        },

        .run_step = function(step)
        {
            private$.verify_step_exists(step)
            pip <- self$pipeline
            thisWasRunSuccessfully <- FALSE
            on.exit(
                if (!thisWasRunSuccessfully) {
                    private$.set_at_step(step, "state", value = "Failed")
                },
                add = TRUE
            )

            row <- self$get_step(step) |> unlist1()
            fun <- row[["fun"]]
            args <- row[["params"]]
            deps <- row[["depends"]]

            # If calculation depends on results of earlier steps, get them from
            # respective output slots of the pipeline.
            hasDeps <- length(deps) > 0
            if (hasDeps) {
                out <- pip[["out"]] |> stats::setNames(pip[["step"]])
                depdendentOut <- private$.extract_dependent_out(deps, out)
                args[names(depdendentOut)] <- depdendentOut
            }

            iStep <- self$get_step_number(step)
            context <- sprintf("Step %i ('%s')", iStep, step)

            res <- withCallingHandlers(
                do.call(fun, args = args),

                error = function(e) {
                    msg <- e$message
                    private$.lg(level = "error", msg = msg, context = context)
                    stop_no_call(e$message)
                },
                warning = function(w) {
                    msg <- w$message
                    private$.lg(level = "warn", msg = msg, context = context)
                }
            )

            if (self$has_step(step)) {
                private$.set_at_step(step, "time", value = Sys.time())
                private$.set_at_step(step, "out", value = res)
                private$.set_at_step(step, "state", value = "Done")
                private$.update_states_downstream(step, "Outdated")
            }
            thisWasRunSuccessfully <- TRUE

            invisible(res)
        },

        .set_at_step = function(
            step,
            field,
            value
        ) {
            i <- self$get_step_number(step)

            stopifnot(
                is_string(field),
                field %in% names(self$pipeline)
            )

            class <- data.class(self$pipeline[[field]])
            if (class == "list") {
                self$pipeline[[field]][i] <- list(value)
            } else {
                stopifnot(data.class(value) == class)
                self$pipeline[[field]][i] <- value
            }

            if (field == "step") {
                private$.reindex()
            }
        },

        .update_states_downstream = function(step, state)
        {
            private$.verify_step_exists(step)
            stopifnot(is_string(state))

            deps <- self$get_depends_down(step, recursive = TRUE)
            for (dep in deps) {
                isLocked <- self$get_step_field(dep, "locked")
                if (!isLocked) {
                    private$.set_at_step(dep, "state", value = state)
                }
            }
        },


        .verify_dependency = function(
            dep,
            step,   # required for error message
            toStep = private$.get_last_step()
        ) {
            stopifnot(
                is_string(dep),
                is_string(step),
                is_string(toStep)
            )

            toIndex <- self$get_step_number(toStep)
            allSteps <- self$get_step_names()
            consideredSteps <- allSteps[seq_len(toIndex)]

            if (!(dep %in% consideredSteps)) {
                msg = paste0(
                    "step '", step, "': dependency '", dep, "' not found"
                )

                if (toStep != private$.get_last_step()) {
                    msg = paste0(
                        msg, " up to step '", consideredSteps[toIndex], "'"
                    )
                }
                stop_no_call(msg)
            }

            invisible(TRUE)
        },


        .verify_from_to = function(from, to)
        {
            stopifnot(
                is_number(from),
                is_number(to),
                from > 0,
                from <= to
            )

            if (to > self$length()) {
                stop_no_call("'to' must not be larger than pipeline length")
            }

            invisible(TRUE)
        },

        .verify_fun_params = function(
            fun,
            funcName,
            params = as.list(formals(fun))
        ) {
            stopifnot(
                is.function(fun),
                is_string(funcName),
                is.list(params)
            )

            fargs <- formals(fun)
            hasDots <- "..." %in% names(fargs)

            # Unless there are dots, all parameters should appear in the
            # function definition
            if (hasDots) {
                fargs <- fargs[!names(fargs) %in% "..."]
            } else {
                unknownParams <- setdiff(names(params), names(fargs))
                if (length(unknownParams) > 0) {
                    stop_no_call(
                        paste0("'", unknownParams, "'", collapse = ", "),
                        " are no function parameters of '", funcName, "'"
                    )
                }
            }

            # Signal undefined parameters, e.g. things like function(x, y = 1)
            isUndefined <- function(x) {
                is.name(x) && toString(x) == ""
            }
            undefinedParams <- Filter(params, f = isUndefined)
            if (length(undefinedParams) > 0) {
                stop_no_call(
                    paste0("'", names(undefinedParams), "'", collapse = ", "),
                    " parameter(s) must have default values"
                )
            }

            invisible(TRUE)
        },

        .verify_step_does_not_exist = function(step)
        {
            if (self$has_step(step)) {
                stop_no_call("step '", step, "' already exists")
            }
        },

        .verify_step_exists = function(step)
        {
            if (!self$has_step(step)) {
                stop_no_call("step '", step, "' does not exist")
            }
        }
    )
)
