
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
Pipeline = R6::R6Class("Pipeline", #nolint
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
                stop("name must be a string")
            }

            if (!nzchar(name)) {
                stop("name must not be empty")
            }

            stopifnot(is.null(logger) || is.function(logger))
            if (is.null(logger)) {
                private$.lg <- lgr::get_logger(name = .this_package_name())$log
            }
            if (is.function(logger)) {
                expectedFormals <- c("level", "msg", "...")
                if (!setequal(names(formals(logger)), expectedFormals)) {
                    stop(
                        "logger function must have the following signature: ",
                        "function(", paste(expectedFormals, collapse = ", "),
                        ")"
                    )
                }
                private$.lg <- logger
            }

            self$name <- name
            self$pipeline <- data.table::data.table(
                step = character(0),
                fun = list(),
                funcName = character(0),
                params = list(),
                depends = list(),
                out = list(),
                keepOut = logical(),
                group = character(0),
                description = character(0),
                time = structure(numeric(0), class = c("POSIXct", "POSIXt")),
                state = character(0)
            )

            self$add("data", function() data, keepOut = FALSE)

            invisible(self)
        },

        #' @description Add pipeline step
        #' @param step `string` the name of the step. Each step name must
        #' be unique.
        #' @param fun `function` or name of the function to be applied at
        #' the step. Both existing and lambda/anonymous functions can be used.
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
        #' p$run()$collect_out(all = TRUE)
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
                        state = "New"
                    )
                )

            invisible(self)
        },

        #' @description Append another pipeline. The append takes care of name
        #' clashes and dependencies, which will be changed after the append.
        #' @param p `Pipeline` object to be appended.
        #' @param outAsIn `logical` if `TRUE`, output of first pipeline is used
        #' @param sep `string` separator used when updating step names to
        #' prevent name clashes.
        #' @return returns new combined `Pipeline`.
        #' @examples
        #' # Append pipeline
        #' p1 <- Pipeline$new("pipe1")
        #' p1$add("step1", \(x) x)
        #' p2 <- Pipeline$new("pipe2")
        #' p2$add("step2", \(y) y)
        #' p1$append(p2)
        #'
        # Append pipeline with potential name clashes
        #' p3 <- Pipeline$new("pipe3")
        #' p3$add("step1", \(z) z)
        #' p1$append(p2)$append(p3)
        append = function(p, outAsIn = FALSE, sep = ".")
        {
            stopifnot(
                inherits(p, "Pipeline"),
                is.logical(outAsIn),
                is_string(sep)
            )

            p1 <- self$clone()
            p2 <- p$clone()


            # Adapt step names and their dependencies to avoid name clashes
            p2$append_to_step_names(p2$name, sep = sep)

            # Build combined pipeline
            combinedName <- paste0(p1$name, sep, p2$name)
            combinedPipe <- Pipeline$new(combinedName)

            combinedPipe$pipeline <- rbind(p1$pipeline, p2$pipeline)
            newStepNames <- combinedPipe$get_step_names()

            if (outAsIn) {
                # Replace first step of p2, with the output of last step of p1
                lastStep1 = newStepNames[p1$length()]
                firstStep2 = newStepNames[p1$length() + 1]
                combinedPipe$replace_step(
                    step = firstStep2,
                    fun = function(data = ~-1) data,
                    description = "output of last step of first pipeline",
                    keepOut = p2$pipeline[["keepOut"]][[1]]
                )
            }

            if (any(duplicated(newStepNames))) {
                duplicatedNames <- newStepNames[duplicated(newStepNames)]
                stop(
                    "Combined pipeline has duplicated step names: ",
                    paste0("'", duplicatedNames, "'", sep = ", ")
                )
            }

            combinedPipe
        },

        #' @description Append string to all step names. Also takes care
        #' of updating dependencies accordingly.
        #' @param postfix `string` to be appended to each step name.
        #' @param sep `string` separator between step name and postfix.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe")
        #' p$add("step1", \(x) x)
        #' p$add("step2", \(y) y)
        #' p$append_to_step_names("new")
        #' p
        #' p$append_to_step_names("new", sep = "_")
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

            invisible(self)
        },


        #' @description Collect output afer pipeline run, by default, from all
        #' steps for which `keepOut` was set to `TRUE`. The output is grouped
        #' by the group names (see `group` parameter in function `add`)
        #' which if not set explicitly corresponds to the step names.
        #' @param groupBy `string` column of pipeline by which to group the
        #' output.
        #' @param all `logical` if `TRUE` all output is collected
        #' regardless of the `keepOut` flag. This can be useful for debugging.
        #' @return `list` containing the output, named after the groups, which,
        #' by default, are the steps.
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("step1", \(x = ~data) x + 2)
        #' p$add("step2", \(x = ~step1) x + 2, keepOut = TRUE)
        #' p$run()
        #' p$collect_out()
        #' p$collect_out(all = TRUE) |> str()
        #'
        # Grouped output
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("step1", \(x = ~data) x + 2, group = "add")
        #' p$add("step2", \(x = ~step1, y = 2) x + y, group = "add")
        #' p$add("step3", \(x = ~data) x * 3, group = "mult")
        #' p$add("step4", \(x = ~data, y = 2) x * y, group = "mult")
        #' p
        #' p$run()
        #' p$collect_out(all = TRUE) |> str()
        #'
        #' # Grouped by state
        #' p$set_params(list(y = 5))
        #' p
        #' p$collect_out(groupBy = "state", all = TRUE) |> str()
        collect_out = function(groupBy = "group", all = FALSE)
        {
            stopifnot(
                is_string(groupBy),
                groupBy %in% colnames(self$pipeline)
            )

            keepOut <- if (all) {
                rep(TRUE, self$length())
            } else {
                self$pipeline[["keepOut"]]
            }

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
                    lapply(collect_results) |>
                    # keep original order of groups as they were defined
                    .subset(unique(groupLabels[indices]))

                result <- append(result, groupedRes)
            }

            # Ungrouped result
            isUngrouped <- !(groupLabels %in% names(groupings))
            ungroupedRes <- pipeOut[isUngrouped, ] |>
                collect_results() |>
                stats::setNames(groupLabels[isUngrouped])

            append(result, ungroupedRes)
        },

        #' @description Discard all steps that match the given `pattern`.
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
        #' p$discard_steps("mult")
        #' p
        #'
        #' # Re-add steps
        #' p$add("mult3", \(x = ~add1) x * 3)
        #' p$add("mult4", \(x = ~add2) x * 4)
        #' p
        #' # Discard step 'add1' does'nt work as 'add2' and 'mult3' depend on it
        #' try(p$discard_steps("add1"))
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
                message(gettextf("step '%s' was removed", step))
            }

            invisible(self)
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
        #' if (requireNamespace("visNetwork", quietly = TRUE)) {
        #'     do.call(visNetwork::visNetwork, args = p$get_graph())
        #' }
        get_graph = function(
            groups = NULL
        ) {
            nodes <- private$.create_node_table(groups = groups)
            edges <- private$.create_edge_table(groups = groups)

            list(nodes = nodes, edges = edges)
        },

        #' @description Get output of given step after pipeline run.
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
            self$get_step(step)[["out"]][[1]]
        },

        #' @description Get all unbound (i.e. not referring to other steps)
        #' function parameters defined in the pipeline.
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
            isValue = function(x) !is.name(x) && !is.call(x)
            areHidden = function(x) {
                startsWith(names(x), ".")
            }

            filter_desired_parameters = function(x) {

                if (length(x) == 0) {
                    return(x)
                }
                params <- Filter(x, f = isValue)

                if (ignoreHidden) {
                    params = params[!areHidden(params)]
                }
                params
            }

            step <- self$get_step(step)

            step[["params"]] |>
                unlist1() |>
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

        #' @description Get all unique function parameters in json format.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` flat unnamed json list of unique function parameters
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = 2, .z = 3) x + y + .z)
        #' p$add("mult1", \(x = 1, y = 2, .z = 3, b = ~add2) x * y * b)
        #' p$get_params_unique_json()
        #' p$get_params_unique_json(ignoreHidden = FALSE)
        get_params_unique_json = function(ignoreHidden = TRUE)
        {
            params = self$get_params_unique(ignoreHidden)

            param_to_list <- function(p, name) {
                if (methods::is(p, "Param")) {
                    p = as.list(attributes(eval(p)))
                    p[["name"]] = name
                } else {
                    p <- list(name = name, value = p)
                }
                p
            }

            mapply(
                params,
                name = names(params),
                FUN = param_to_list,
                SIMPLIFY = FALSE
            ) |>
                stats::setNames(NULL) |>
                jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)
        },

        #' @description Get step of pipeline
        #' @param step `string` name of step
        #' @return `data.table` row containing the step. If step not found, an
        #' @examples
        #' error is given.
        #' p <- Pipeline$new("pipe", data = 1:2)
        #' p$add("add1", \(data = ~data, x = 1) x + data)
        #' p$add("add2", \(x = 1, y = 2, z = ~add1) x + y + z)
        #' p$run()
        #' add1 <- p$get_step("add1")
        #' print(add1)
        #' add1[["params"]]
        #' add1[["out"]]
        #' try()
        #' try(p$get_step("foo")) # error: step 'foo' does not exist
        get_step = function(step)
        {
            private$.verify_step_exists(step)
            pos <- Position(
                self$pipeline[["step"]],
                f = function(x) x == step,
                nomatch = stop("step '", step, "' not found")
            )

            self$pipeline[pos, ]
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
            match(step, self$get_step_names())
        },

        #' @description Determine whether pipeline has given step.
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
            step %in% self$get_step_names()
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
                stop("step '", afterStep, "' does not exist")
            }

            pos <- match(afterStep, self$get_step_names())
            pip <- Pipeline$new(name = self$name)
            pip$pipeline <- self$pipeline[seq_len(pos), ]
            pip$add(step = step, ...)

            if (pos < self$length()) {
                pip$pipeline <- rbind(
                    pip$pipeline,
                    self$pipeline[-seq_len(pos), ]
                )
            }

            self$pipeline <- pip$pipeline
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
                stop("step '", beforeStep, "' does not exist")
            }

            pos <- match(beforeStep, self$get_step_names()) - 1
            if (pos == 0) {
                stop("cannot insert before first step")
            }

            pip <- Pipeline$new(name = self$name)

            pip$pipeline <- self$pipeline[seq_len(pos), ]
            pip$add(step = step, ...)

            pip$pipeline <- rbind(
                pip$pipeline,
                self$pipeline[-seq_len(pos), ]
            )

            self$pipeline <- pip$pipeline
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
        #' output (given it has output) are locked. If it does not have output,
        #' only the parameters are locked. Locking a step is useful if the
        #' step happens to share parameter names with other steps but should not
        #' be affected when parameters are set commonly for the entire pipeline
        #' (see function `set_params` below).
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
            private$.set_at_step(step, "state", "Locked")
            invisible(self)
        },

        #' @description Print the pipeline as a table.
        #' @param verbose `logical` if `TRUE`, print all columns of the
        #' pipeline, otherwise only a subset of columns is printed.
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

        #' @description Remove last step from the pipeline.
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
            lastStepName
        },

        #' @description Remove all steps after the given step.
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

            removedSteps
        },

        #' @description Remove all steps from and including the given step.
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

            removedStepNames
        },


        #' @description Remove certain step from the pipeline. If step does
        #' not exist, an error is given. If other steps depend on the step to
        #' be removed, an error is given, unless `recursive = TRUE`.
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
                    stop(
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
            invisible(self)
        },

        #' @description Replace pipeline step.
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
            toStep = allSteps[stepNumber - 1]

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
                state = "New"
            )

            self$pipeline[stepNumber, ] <- newStep
            private$.update_states_downstream(step, state = "Outdated")

            invisible(self)
        },

        #' @description Run all new and/or outdated pipeline steps.
        #' @param force `logical` if `TRUE` all steps are run regardless of
        #' whether they are outdated or not.
        #' @param recursive `logical` if `TRUE` and a step returns a new
        #' pipeline, the run of the current pipeline is aborted and the
        #' new pipeline is run recursively.
        #' @param cleanUnkept `logical` if `TRUE` all output that was not
        #' marked to be kept is removed after the pipeline run. This option
        #' can be useful if temporary results require a lot of memory.
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
        #' p$run(cleanUnkept = TRUE)  # clean up temporary results
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
        #' p$run()$collect_out()
        run = function(
            force = FALSE,
            recursive = TRUE,
            cleanUnkept = FALSE
        ) {
            stopifnot(
                is.logical(force),
                is.logical(recursive),
                is.logical(cleanUnkept)
            )

            log_info <- function(msg, ...) {
                private$.lg(level = "info", msg = msg, ...)
            }

            gettextf("Start run of '%s' pipeline:", self$name) |> log_info()

            to <- self$length()
            for (i in seq(from = 1, to = to)) {
                step <- as.character(self$pipeline[i, "step"])
                state <- self$get_step(step)[["state"]]
                info <- gettextf("Step %i/%i %s", i, to, step)

                if (state == "Locked" || (state == "Done" && !force)) {
                    paste0(info, " - skip '", tolower(state), "' step") |>
                        log_info()
                    next()
                }
                log_info(info)

                res <- private$.run_step(step)

                if (inherits(res, "Pipeline") && recursive) {
                    log_info("Abort pipeline execution and restart on new.")
                    self = res
                    self$run(recursive = TRUE)
                    return(invisible(self))
                }
            }

            log_info("Finished execution of steps.")

            if (cleanUnkept) {
                log_info("Clean temporary results.")
                private$.clean_out_not_kept()
            }

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
        #' @param cleanUnkept `logical` if `TRUE` all output that was not
        #' marked to be kept is removed after the pipeline run. This option
        #' can be useful if temporary results require a lot of memory.
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
            downstream = FALSE,
            cleanUnkept = FALSE
        ) {
            private$.verify_step_exists(step)
            stopifnot(
                is.logical(upstream),
                is.logical(downstream),
                is.logical(cleanUnkept)
            )

            upstreamSteps <- character(0)
            steps <- step
            downstreamSteps <- character(0)

            if (upstream) {
                upstreamSteps <- private$.get_upstream_depends(
                    step = step,
                    depends = self$get_depends(),
                    recursive = TRUE
                )
                steps <- c(upstreamSteps, step)
            }

            if (downstream) {
                downstreamSteps <- private$.get_downstream_depends(
                    step = step,
                    depends = self$get_depends(),
                    recursive = TRUE
                )
                steps <- c(steps, downstreamSteps)
            }

            nStep <- length(steps)

            log_info <- function(msg, ...) {
                private$.lg(level = "info", msg = msg, ...)
            }

            gettextf("Start step run of '%s' pipeline:", self$name) |>
                log_info()

            for (i in seq_along(steps)) {
                step <- steps[i]
                state <- self$get_step(step)[["state"]]
                stream <- ""
                if (step %in% upstreamSteps) {
                    stream <- " (upstream)"
                }
                if (step %in% downstreamSteps) {
                    stream <- " (downstream)"
                }
                info <- gettextf("Step %i/%i %s%s",  i, nStep, step, stream)

                if (state == "Locked") {
                    paste0(info, " - skip '", tolower(state), "' step") |>
                        log_info()
                    next()
                }
                log_info(info)

                private$.run_step(step)
            }

            log_info("Finished execution of steps.")

            if (cleanUnkept) {
                log_info("Clean temporary results.")
                private$.clean_out_not_kept()
            }

            log_info("Done.")
            invisible(self)
        },

        #' @description Set data in first step of pipeline.
        #' @param data `data.frame` initial data set.
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

        #' @description Split-copy pipeline by list of data sets. Each
        #' sub-pipeline will have one of the data sets set as input data.
        #' The step names of the sub-pipelines will be the original step names
        #' plus the name of the data set.
        #' @param dataList `list` of data sets
        #' @param toStep `string` step name marking optional subset of
        #' the pipeline, at which the data split should be applied to.
        #' @param groupBySplit `logical` whether to set step groups according
        #' to data split.
        #' @param sep `string` separator to be used between step name and
        #' data set name when creating the new step names.
        #' @return new combined `Pipeline` with each sub-pipeline having set
        #' one of the data sets.
        #' @examples
        #' # Split by three data sets
        #' dataList <- list(a = 1, b = 2, c = 3)
        #' p <- Pipeline$new("pipe")
        #' p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
        #' p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
        #' p3 <- p$set_data_split(dataList)
        #' p3
        #' p3$run()$collect_out() |> str()
        #'
        #' # Don't group output by split
        #' p <- Pipeline$new("pipe")
        #' p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
        #' p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
        #' p3 <- p$set_data_split(dataList, groupBySplit = FALSE)
        #' p3
        #' p3$run()$collect_out() |> str()
        set_data_split = function(
            dataList,
            toStep = utils::tail(self$get_step_names(), 1),
            groupBySplit = TRUE,
            sep = "."
        ) {
            stopifnot(
                is.list(dataList),
                is_string(toStep),
                is.logical(groupBySplit),
                is_string(sep)
            )
            private$.verify_step_exists(toStep)
            to <- self$get_step_number(toStep)
            upperSteps <- self$get_step_names()[seq_len(to)]

            dataNames <- names(dataList)
            if (is.null(dataNames)) {
                dataNames = as.character(seq_along(dataList))
            }

            init_new_pipeline_with_data = function(data) {
                self$clone(deep = TRUE)$set_data(data)
            }

            # Create new pipeline for each data set
            pipes <- lapply(dataList, init_new_pipeline_with_data)
            for (i in seq_along(pipes)) {
                name <- dataNames[[i]]
                pipes[[i]]$name <- name
                pipes[[i]]$pipeline <- pipes[[i]]$pipeline[seq_len(to), ]

                newGroups <- name
                if (!groupBySplit) {
                    oldGroups <- pipes[[i]]$pipeline[["group"]]
                    newGroups <- paste0(oldGroups, sep, name)
                }
                pipes[[i]]$pipeline[["group"]] <- newGroups
            }

            # Combine pipelines
            pipeNames <- sapply(pipes, function(x) x$name)
            combinedName <- paste(self$name, "split")
            pip <- Pipeline$new(combinedName)
            combined <- Reduce(
                c(pip, pipes),
                f = function(x, y) x$append(y, sep = sep)
            )
            combined$remove_step("data")

            # If subset was used for split, append the remaining steps and
            # update all of the (now changed) upstream dependencies.
            if (to < self$length()) {
                remainingPipe <- self$pipeline[(to + 1):self$length(), ]
                remainingDeps <- remainingPipe[["depends"]]
                update_if_needed <- function(x) {
                    needsUpdate <- x %in% upperSteps
                    if (needsUpdate) paste0(x, sep, pipeNames) else x
                }
                updatedDeps <- lapply(
                    remainingDeps,
                    function(deps) {
                        res = lapply(deps, update_if_needed)
                        if (all(sapply(res, function(x) length(x) == 1))) {
                            res = unlist(res)   # flatten single deps
                        }
                        res
                    }
                )
                remainingPipe[["depends"]] <- updatedDeps
                combined$pipeline <- rbind(combined$pipeline, remainingPipe)
            }

            self$pipeline <- combined$pipeline
            invisible(self)
        },

        #' @description Set pipeline to keep or omit output of given step.
        #' @param step `string` name of step
        #' @param keepOut `logical` whether to keep output of step
        #' @return the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
        #' p$add("add2", \(x = ~data, y = 2) x + y)
        #' p$add("mult", \(x = ~add1, y = ~add2) x * y)
        #' p$run()$collect_out()
        #' p$set_keep_out("add1", keepOut = FALSE)
        #' p$set_keep_out("mult", keepOut = TRUE)
        #' p$run()$collect_out()
        set_keep_out = function(step, keepOut = TRUE)
        {
            stopifnot(
                is.logical(keepOut)
            )
            private$.set_at_step(step, "keepOut", value = keepOut)

            invisible(self)
        },

        #' @description Set parameters in the pipeline. If a parameter occurs
        #' in several steps, the parameter is set commonly in all steps.
        #' @param params `list` of parameters to be set
        #' @param warnUndefined `logical` whether to give a warning if a
        #' parameter is not defined in the pipeline.
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1) x + y)
        #' p$add("add2", \(x = ~data, y = 1) x + y)
        #' p$add("mult", \(x = 1, z = 1) x * z)
        #' p$get_params()
        #' p$set_params(list(x = 3, y = 3))
        #' p$get_params()
        #' p$set_params(list(x = 5, z = 3))
        #' p$get_params()
        #' suppressWarnings(
        #'     p$set_params(list(foo = 3))  # warning: trying to set undefined
        #' )
        set_params = function(params, warnUndefined = TRUE)
        {
            definedParams <- self$get_params_unique(ignoreHidden = FALSE)
            extra <- setdiff(names(params), names(definedParams))

            if (warnUndefined && length(extra) > 0) {
                warning(
                    "Trying to set parameters not defined in the pipeline: ",
                    toString(extra)
                )
            }

            for (step in self$get_step_names()) {
                paramsAtStep = self$get_params_at_step(
                    step,
                    ignoreHidden = FALSE
                )
                overlap <- intersect(names(params), names(paramsAtStep))

                if (length(overlap) > 0) {
                    paramsAtStep[overlap] = params[overlap]
                    self$set_params_at_step(step, paramsAtStep)
                }
            }
            invisible(self)
        },

        #' @description Set unbound parameter values at given pipeline step.
        #' @param step `string` the name of the step
        #' @param params `list` of parameters to be set
        #' @return returns the `Pipeline` object invisibly
        #' @examples
        #' p <- Pipeline$new("pipe", data = 1)
        #' p$add("add1", \(x = ~data, y = 1, z = 2) x + y)
        #' p$add("add2", \(x = ~data, y = 1, z = 2) x + y)
        #' p$set_params_at_step("add1", list(y = 3, z = 3))
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

            isLocked <- self$get_step(step)[["state"]] == "Locked"
            if (isLocked) {
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
                stop(
                    "Unable to set parameter(s) ", toString(extra),
                    " at step ", step, " - candidates are ",
                    toString(names(current))
                )
            }

            toUpdate <- intersect(names(params), names(current))
            hasUpdate <- length(toUpdate) > 0
            if (hasUpdate) {
                # Update params
                old <- self$get_step(step)[["params"]] |> unlist1()
                new <- utils::modifyList(
                    x = old,
                    val = params[toUpdate],
                    keep.null = TRUE
                )
                private$.set_at_step(step, "params", value = new)

                # Update state if applicable
                state <- self$get_step(step)[["state"]]
                if (state == "Done") {
                    private$.set_at_step(step, "state", "Outdated")
                    private$.update_states_downstream(step, "Outdated")
                }
            }

            invisible(self)
        },

        #' @description Splits pipeline into its independent parts.
        #' @return list of `Pipeline` objects
        #' @examples
        #' dataList <- list(a = 1, b = 2, c = 3)
        #' p <- Pipeline$new("pipe")
        #' p$add("add1", \(x = ~data) x + 1, keepOut = TRUE)
        #' p$add("mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
        #' pips <- p$set_data_split(dataList)$split()
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
        #' the command is ignored.
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
            state <- self$get_step(step)[["state"]]
            if (state == "Locked") {
                private$.set_at_step(step, "state", "Unlocked")
            }

            invisible(self)
        }
    ),

    private = list(
        .lg = NULL, # the logger function
        deep_clone = function(name, value) {
            if (name == "pipeline")
                return(data.table::copy(value))

            value
        },

        .clean_out_not_kept = function()
        {
            areNotKept <- !self$pipeline[["keepOut"]]
            data.table::set(
                self$pipeline,
                i = which(areNotKept),
                j = "out",
                value = list(list(NULL))
            )
            data.table::set(
                self$pipeline,
                i = which(areNotKept),
                j = "state",
                value = "Outdated"
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
                "shape" = "circle",
                "color" = "grey",
                "title" = paste0("<p>", pip[["description"]], "</p>")
            )

            nodes[["color"]][pip[["state"]] == "New"] <- "lightblue"
            nodes[["color"]][pip[["state"]] == "Done"] <- "lightgreen"
            nodes[["color"]][pip[["state"]] == "Outdated"] <- "orange"
            nodes[["color"]][pip[["state"]] == "Failed"] <- "red"
            nodes[["color"]][pip[["state"]] == "Locked"] <- "grey"
            nodes[["shape"]][1] <- "database"
            nodes[["shape"]][pip[["keepOut"]]] <- "box"

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

            result <- depends |>
                Filter(f = function(x) step %in% unlist(x)) |>
                names()

            if (recursive) {
                result <- c(
                    result,
                    sapply(
                        result,
                        FUN = private$.get_downstream_depends,
                        depends = depends,
                        recursive = TRUE
                    )
                )
            }

            unique(unlist(result)) |> as.character()
        },

        .get_last_step = function() {
            self$get_step_names() |> utils::tail(1)
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

            result <- unlist(depends[[step]]) |> as.character()

            if (recursive) {
                result <- c(
                    result,
                    sapply(
                        result,
                        FUN = private$.get_upstream_depends,
                        depends = depends,
                        recursive = TRUE
                    )
                )
            }

            unique(unlist(result)) |> as.character()
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
                stop(
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

            # If arg is encapsulated in a Param object, get the value
            args <- lapply(
                args,
                FUN = function(arg) {
                    if (methods::is(arg, "Param")) arg@value else arg
                }
            )

            iStep <- self$get_step_number(step)
            context <- gettextf("Step %i ('%s')", iStep, step)

            res <- withCallingHandlers(
                do.call(fun, args = args),

                error = function(e) {
                    msg <- e$message
                    private$.lg(level = "error", msg = msg, context = context)
                    stop(e$message, call. = FALSE)
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
        },

        .update_states_downstream = function(step, state)
        {
            private$.verify_step_exists(step)
            stopifnot(is_string(state))

            deps <- self$get_depends_down(step, recursive = TRUE)
            for (dep in deps) {
                current <- self$get_step(dep)[["state"]]
                if (current != "Locked") {
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

            private$.verify_step_exists(toStep)

            allSteps <- self$get_step_names()
            toIndex <- match(toStep, allSteps)
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
                stop(msg, call. = FALSE)
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
                stop(
                    "'to' must not be larger than pipeline length",
                    call. = FALSE
                )
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
                    stop(
                        paste0("'", unknownParams, "'", collapse = ", "),
                        " are no function parameters of '", funcName, "'",
                        call. = FALSE
                    )
                }
            }

            # Signal undefined parameters, e.g. things like function(x, y = 1)
            isUndefined <- function(x) {
                is.name(x) && toString(x) == ""
            }
            undefinedParams <- Filter(params, f = isUndefined)
            if (length(undefinedParams) > 0) {
                stop(
                    paste0("'", names(undefinedParams), "'", collapse = ", "),
                    " parameter(s) must have default values",
                    call. = FALSE
                )
            }

            invisible(TRUE)
        },

        .verify_step_does_not_exist = function(step)
        {
            if (self$has_step(step)) {
                stop("step '", step, "' already exists", call. = FALSE)
            }
        },

        .verify_step_exists = function(step)
        {
            if (!self$has_step(step)) {
                stop("step '", step, "' does not exist", call. = FALSE)
            }
        }
    )
)


# nocov start

#' @title Pipeline functions
#' @description Helper functions to enable pipeline construction via R's pipe
#' @param pip A pipeline object
#' @param ... Arguments passed to the respective pipeline method
#' @return The result of the respective pipeline method
#' @name pipelineHelpers
NULL

#' @rdname pipelineHelpers
#' @export
pipe_add = function(pip, ...)
    pip$add(...)


#' @rdname pipelineHelpers
#' @export
pipe_append = function(pip, ...)
    pip$append(...)


#' @rdname pipelineHelpers
#' @export
pipe_append_to_step_names = function(pip, ...)
    pip$append_to_step_names(...)


#' @rdname pipelineHelpers
#' @export
pipe_clone = function(pip, ...)
    pip$clone(...)


#' @rdname pipelineHelpers
#' @export
pipe_collect_out = function(pip, ...)
    pip$collect_out(...)


#' @rdname pipelineHelpers
#' @export
pipe_discard_steps = function(pip, ...)
    pip$discard_steps(...)



#' @rdname pipelineHelpers
#' @export
pipe_get_depends = function(pip, ...)
    pip$get_depends(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_depends_down = function(pip, ...)
    pip$get_depends_down(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_depends_up = function(pip, ...)
    pip$get_depends_up(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_graph = function(pip, ...)
    pip$get_graph(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_out = function(pip, ...)
    pip$get_out(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_params = function(pip, ...)
    pip$get_params(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_params_at_step = function(pip, ...)
    pip$get_params_at_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_params_unique = function(pip, ...)
    pip$get_params_unique(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_params_unique_json = function(pip, ...)
    pip$get_params_unique_json(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_step = function(pip, ...)
    pip$get_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_step_names = function(pip, ...)
    pip$get_step_names(...)


#' @rdname pipelineHelpers
#' @export
pipe_get_step_number = function(pip, ...)
    pip$get_step_number(...)


#' @rdname pipelineHelpers
#' @export
pipe_has_step = function(pip, ...)
    pip$has_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_insert_after = function(pip, ...)
    pip$insert_after(...)


#' @rdname pipelineHelpers
#' @export
pipe_insert_before = function(pip, ...)
    pip$insert_before(...)


#' @rdname pipelineHelpers
#' @export
pipe_length = function(pip, ...)
    pip$length(...)


#' @rdname pipelineHelpers
#' @export
pipe_lock_step = function(pip, ...)
    pip$lock_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_print = function(pip, ...)
    pip$print(...)


#' @rdname pipelineHelpers
#' @export
pipe_pop_step = function(pip, ...)
    pip$pop_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_pop_steps_after = function(pip, ...)
    pip$pop_steps_after(...)


#' @rdname pipelineHelpers
#' @export
pipe_pop_steps_from = function(pip, ...)
    pip$pop_steps_from(...)


#' @rdname pipelineHelpers
#' @export
pipe_remove_step = function(pip, ...)
    pip$remove_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_replace_step = function(pip, ...)
    pip$replace_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_run = function(pip, ...)
    pip$run(...)


#' @rdname pipelineHelpers
#' @export
pipe_run_step = function(pip, ...)
    pip$run_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_set_data = function(pip, ...)
    pip$set_data(...)


#' @rdname pipelineHelpers
#' @export
pipe_set_data_split = function(pip, ...)
    pip$set_data_split(...)


#' @rdname pipelineHelpers
#' @export
pipe_set_keep_out = function(pip, ...)
    pip$set_keep_out(...)


#' @rdname pipelineHelpers
#' @export
pipe_set_params = function(pip, ...)
    pip$set_params(...)


#' @rdname pipelineHelpers
#' @export
pipe_set_params_at_step = function(pip, ...)
    pip$set_params_at_step(...)


#' @rdname pipelineHelpers
#' @export
pipe_split = function(pip, ...)
    pip$split(...)


#' @rdname pipelineHelpers
#' @export
pipe_unlock_step = function(pip, ...)
    pip$unlock_step(...)

# nocov end
