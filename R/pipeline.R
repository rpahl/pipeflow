
#' @title Pipeline Class
#'
#' @description This class implements an analysis pipeline. A pipeline consists
#' of a sequence of analysis steps, which can be added one by one. Each added
#' step may or may not depend on one or more previous steps. The pipeline
#' keeps track of the dependencies among these steps and will ensure that
#' all dependencies are met on creation of the pipeline, that is, before the
#' the pipeline is executed. Once the pipeline is executed, the output is
#' stored in the pipeline along with each step and can be accessed later.
#' Different pipelines can be bound together while preserving all dependencies
#' within each pipeline.
#' @field name `string` name of the pipeline
#' @field pipeline `data.table` the pipeline where each row represents on step.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @import utils
#' @export
Pipeline = R6::R6Class("Pipeline", #nolint
    public = list(
        name = NULL,
        pipeline = NULL,

        #' @description constructor
        #' @param name the name of the Pipeline
        #' @param data optional data used in the pipeline (can be set later)
        #' @return returns the `Pipeline` object invisibly
        initialize = function(name, data = NULL)
        {
            if (!is_string(name)) {
                stop("name must be a string")
            }

            if (!nzchar(name)) {
                stop("name must not be empty")
            }

            self$name = name
            self$pipeline <- data.table::data.table(
                step = character(0),
                fun = character(0),
                funcName = character(0),
                params = list(),
                keepOut = logical(),
                deps = character(0),
                out = list(),
                group = character(0),
                description = character(0)
            )

            self$add(".data", function() data, keepOut = FALSE)

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
        #' @param group `string` grouping information (by default the same as
        #' the name of the step. Any output collected later (see function
        #' `collect_out` by default is put together by these group names. This,
        #' for example, comes in handy when the pipeline is copy-appended
        #' multiple times to keep the results of the same function/step at one
        #' place.
        #' @param keepOut `logical` if `FALSE` the output of the function will
        #' be cleaned at the end of the whole pipeline execution. This option
        #' is used to only keep the results that matter.
        #' @return returns the `Pipeline` object invisibly
        add = function(
            step,
            fun,
            params = list(),
            description = "",
            group = step,
            keepOut = FALSE
        ) {
            private$.verify_step_does_not_exist(step)

            # Function can be either a function or a character string
            if (is.function(fun)) {
                funcName = as.character(substitute(fun))[[1]]
            }
            else {
                funcName = fun
                fun = get(fun, mode = "function")
            }

            # Update default function params by custom params
            if (length(params) == 0) {
                params = formals(fun)
            } else {
                params = replace(formals(fun), names(params), params)
            }

            # Make sure parameters defined as Param object or call are evaluated
            isDefined = sapply(
                params,
                function(x) {
                    methods::is(x, "Param") || methods::is(x, "call")
                }
            )
            params[isDefined] = lapply(params[isDefined], eval)

            private$.verify_fun_params(fun, funcName, as.list(params))

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
                        keepOut = keepOut,
                        deps = list(deps),
                        out = list(list()),
                        group = group,
                        description = description
                    )
                )

            invisible(self)
        },

        #' @description Append another pipeline. The append takes care of name
        #' clashes and dependencies, which will be changed after the append.
        #' @param p `Pipeline` object to be appended.
        #' @param outAsIn `logical` if `TRUE`, output of first pipeline is used
        #'  as data input of appended pipeline.
        #' @return returns new combined `Pipeline`.
        append = function(p, outAsIn = FALSE)
        {
            stopifnot(
                inherits(p, "Pipeline"),
                is.logical(outAsIn)
            )

            p1 = self$clone()
            p2 = p$clone()

            if (p1$name == p2$name) {
                stop("pipelines cannot have the same name ('", p2$name, "')")
            }

            # Adapt step names and their dependencies to prevent name clashes
            p2$append_to_step_names(p2$name)

            if (outAsIn) {
                len1 = p1$length()
                last_step1 = p1$pipeline[len1, "step"][[1]]
                first_step2 = p2$pipeline[1, "step"][[1]]
                deps2_updated = lapply(
                    p2$pipeline[["deps"]],
                    FUN = .replace_string,
                    target = first_step2,
                    replacement = last_step1
                )
                p2$pipeline[["deps"]] = deps2_updated
            }

            # Build combined pipeline
            combined_name = paste0(p1$name, ".", p2$name)
            combined_pipe = Pipeline$new(combined_name)

            combined_pipe$pipeline = rbind(p1$pipeline, p2$pipeline)

            step_names = combined_pipe$pipeline[["step"]]
            if (any(duplicated(step_names))) {  # this should never happen
                duplicatedNames = step_names[duplicated(step_names)]
                stop("Combined pipeline has duplicated step names:",
                    paste0("'", duplicatedNames, "'", sep = ", ")
                )
            }

            combined_pipe
        },

        #' @description Append string to all step names.
        #' @param postfix `string` to be appended to each step name.
        #' @return returns the `Pipeline` object invisibly
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
            deps <- self$get_dependencies()

            self$pipeline[["step"]] <- add_postfix(steps)
            self$pipeline[["deps"]] <- lapply(deps, add_postfix)

            invisible(self)
        },

        #' @description Collect all output that was stored and kept during the
        #' pipeline execution.
        #' @param groupBy `string` column of pipeline by which to group the
        #' output.
        #' @return `list` containing the output, named after the groups, which,
        #' by default, are the steps.
        collect_out = function(groupBy = "group") {
            subpipe = self$pipeline[self$pipeline[["keepOut"]], ]
            if (nrow(subpipe) == 0)
                return(list())

            collect_by_group = function(subpipe) {
                results = as.list(subpipe[["out"]])
                names(results) = subpipe[["step"]]
                results
            }

            out = split(subpipe, subpipe[[groupBy]]) |>
                lapply(collect_by_group)

            out
        },

        #' @description Discard all steps that match the given `pattern`.
        #' @param pattern `string` containing a regular expression (or
        #' character string for `fixed = TRUE`) to be matched.
        #' @param fixed `logical` If `TRUE`, `pattern` is a string to
        #' be matched as is. Overrides all conflicting arguments.
        #' @param ... further arguments passed to `[grep()]`.
        #' #' @seealso `[grep()]`
        #' @return the `Pipeline` object invisibly
        discard_steps = function(pattern, fixed = TRUE, ...) {
            step_names = self$pipeline[["step"]]
            steps2remove = grep(
                pattern = pattern,
                x = step_names,
                fixed = fixed,
                value = TRUE,
                ...
            )

            # To respect dependencies, remove steps from last to first
            for (step in rev(steps2remove)) {
                log_info(gettextf("Removing step '%s'", step))
                self$remove_step(step)
            }

            invisible(self)
        },

        #' @description Execute pipeline steps.
        #' @param from `numeric` start from this step
        #' @param to `numeric` execute until this step
        #' @param recursive `logical` if `TRUE` and a step returns a new
        #' pipeline, the run of the current pipeline is aborted and the
        #' new pipeline is executed recursively.
        #' @return returns the `Pipeline` object invisibly
        execute = function(
            from = 1,
            to = self$length(),
            recursive = TRUE
        ) {
            stopifnot(
                is_number(from),
                is_number(to),
                from <= to,
                is.logical(recursive)
            )

            # nolint start
            # TODO: create function
            #   execute_step(
            #       step,
            #       downstream = TRUE,  # execute all depdendent steps downstream
            #       upstream = TRUE,    # execute all depdendent upstream steps first
            #       overwrite = FALSE,  # overwrite output if already present
            #       recursive = TRUE    # if step returns a pipeline, execute it recursive
            #   ),
            # TODO: change interface to  execute(steps, ...)
            # nolint end
            gettextf("Start execution of '%s' pipeline:", self$name) |>
                log_info(type = "start_pipeline", pipeline_name = self$name)

            for (i in seq(from = from, to = to)) {
                step <- as.character(self$pipeline[i, "step"])
                gettextf("Step %i/%i %s",  i, to, step) |> log_info()

                res = private$.execute_step(step)

                if (inherits(res, "Pipeline") && recursive) {
                    log_info("Abort pipeline execution and restart on new.")
                    self = res
                    self$execute(recursive = TRUE)
                    return(invisible(self))
                }

                if (length(res) > 0) {
                    self$pipeline[["out"]][[i]] <- res
                }
            }

            log_info("Finished execution of steps.")

            # Clean output that shall not be kept
            log_info("Clean temporary results.")
            areNotKept = !self$pipeline[["keepOut"]]
            data.table::set(
                self$pipeline,
                i = which(areNotKept),
                j = "out",
                value = list(list())
            )

            log_info("Done.")
            invisible(self)
        },

        #' @description Get data currently loaded into the pipeline at step 1.
        #' @return returns the data
        get_data = function()
        {
            self$pipeline[["fun"]][[1]]()
        },

        #' @description Get all dependencies defined in the pipeline
        #' @return named `list` of dependencies for each step
        get_dependencies = function()
        {
            self$pipeline[["deps"]] |>
                stats::setNames(self$get_step_names())
        },

        #' @description Get all downstream dependencies of given step, by
        #' default descending recursively.
        #' @param step `string` name of step
        #' @param recursive `logical` if `TRUE`, dependencies of dependencies
        #' are also returned.
        #' @return `list` of downstream dependencies
        get_downstream_dependencies = function(
            step,
            recursive = TRUE
        ) {
            private$.verify_step_exists(step)

            deps <- private$.get_downstream_deps(
                step = step,
                deps = self$get_dependencies(),
                recursive = recursive
            )

            # Ensure order matches the step order of the pipeline
            intersect(self$get_step_names(), deps)
        },

        #' @description Get all function parameters defined in the pipeline.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of parameters, sorted and named by step. Steps with
        #' no parameters are filtered out.
        get_parameters = function(ignoreHidden = TRUE)
        {
            res <- lapply(
                self$pipeline[["step"]],
                FUN = self$get_parameters_at_step,
                ignoreHidden = ignoreHidden
            )

            names(res) <- self$pipeline[["step"]]
            Filter(res, f = function(x) length(x) > 0)
        },

        #' @description Get all function parameters at given step name.
        #' @param step `string` name of step
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of parameters defined at given step.
        get_parameters_at_step = function(step, ignoreHidden = TRUE)
        {
            isValue = function(x) !is.name(x) && !is.call(x)
            areHidden = function(x) {
                startsWith(names(x), ".")
            }

            filter_desired_parameters = function(x) {

                if (length(x) == 0) {
                    return(x)
                }

                params = Filter(x, f = isValue)

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


        #' @description Get step of pipeline
        #' @param step `string` name of step
        #' @return `data.table` row containing the step. If step not found, an
        #' error is given.
        get_step = function(step)
        {
            pos = Position(
                self$pipeline[["step"]],
                f = function(x) x == step,
                nomatch = stop("step '", step, "' not found")
            )

            self$pipeline[pos, ]
        },

        #' @description Get step names of pipeline
        #' @return `character` vector of step names
        get_step_names = function()
        {
            self$pipeline[["step"]]
        },

        #' @description Get all function parameters defined in the pipeline,
        #' but only list each parameter once, that is, once a parameter is used
        #' in a step, it is ignored in the listings of later steps.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of unique parameters, sorted and named by step. Steps
        #' with no parameters are filtered out.
        get_unique_parameters = function(ignoreHidden = TRUE)
        {
            params = self$get_parameters(ignoreHidden)

            if (length(params) == 0)
                return(params)

            used_param_names = NULL
            res = NULL

            for (name in names(params)) {
                p = params[[name]]
                new = setdiff(names(p), used_param_names)
                res[[name]] = as.list(p[new])
                used_param_names = c(used_param_names, new)
            }

            Filter(res, f = function(x) length(x) > 0)
        },

        #' @description Get all unique function parameters in json format.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` flat unnamed json list of unique function parameters,
        #'  at this point with no information of the steps were parameters are
        #'  defined first.
        get_unique_parameters_json = function(ignoreHidden = TRUE)
        {
            params = self$get_unique_parameters(ignoreHidden) |>
                Reduce(f = c) # remove step information

            isParam = sapply(params, function(p) methods::is(p, "Param"))

            for (name in names(params)) {
                p = params[[name]]

                if (methods::is(p, "Param")) {
                    p = as.list(attributes(eval(p)))
                    p[["name"]] = name
                } else {
                    p = list(name = name, value = p)
                }
                params[[name]] = p
            }

            # Clean names to have unnamed json elements
            names(params) = NULL

            jsonlite::toJSON(params, auto_unbox = TRUE, pretty = TRUE)
        },

        #' @description Get all upstream dependencies of given step, by
        #' default descending recursively.
        #' @param step `string` name of step
        #' @param recursive `logical` if `TRUE`, dependencies of dependencies
        #' are also returned.
        #' @return `list` of upstream dependencies
        get_upstream_dependencies = function(
            step,
            recursive = TRUE
        ) {
            private$.verify_step_exists(step)

            deps <- private$.get_upstream_deps(
                step = step,
                deps = self$get_dependencies(),
                recursive = recursive
            )

            # Ensure order matches the step order of the pipeline
            intersect(self$get_step_names(), deps)
        },

        #' @description Determine whether pipeline has given step.
        #' @param step `string` name of step
        #' @return `logical` whether step exists
        has_step = function(step)
        {
            stopifnot(
                is_string(step),
                nzchar(step)
            )
            step %in% self$get_step_names()
        },

        #' @description Set pipeline to keep all output regardless of the flags
        #' that were set for the individual steps. This can be useful for
        #' debugging.
        #' @return `logical` original set of output flags.
        keep_all_out = function() {
            old = self$pipeline[["keepOut"]]
            names(old) = self$pipeline[["step"]]

            self$pipeline[, "keepOut"] = TRUE

            old
        },

        #' @description Length of the pipeline aka number of pipeline steps.
        #' @return `numeric` length of pipeline.
        length = function() nrow(self$pipeline),

        #' @description Print the pipeline.
        print = function() print(self$pipeline),

        #' @description Remove last step from the pipeline.
        #' @return `string` the name of the step that was removed
        pop_step = function() {
            len = self$length()
            last_step_name = self$pipeline[["step"]][[len]]

            self$pipeline = self$pipeline[-len, ]
            last_step_name
        },

        #' @description Remove all steps after the given step.
        #' @param step `string` name of step
        #' @return `character` vector of steps that were removed.
        pop_steps_after = function(step) {

            existing_steps = self$pipeline[["step"]]
            step_number = match(step, existing_steps)

            if (is.na(step_number)) {
                stop("step '", step, "' does not exist", call. = FALSE)
            }

            if (step_number == self$length())
                return(character(0))    # nothing to remove

            next_step = step_number + 1
            removed_step_numbers = next_step:self$length()
            removed_step_names = self$pipeline[["step"]][removed_step_numbers]

            self$pipeline = self$pipeline[-removed_step_numbers, ]

            removed_step_names
        },

        #' @description Remove all steps from and including the given step.
        #' @param step `string` name of step
        #' @return `character` vector of steps that were removed.
        pop_steps_from = function(step) {

            existing_steps = self$pipeline[["step"]]
            step_number = match(step, existing_steps)

            if (is.na(step_number)) {
                stop("step '", step, "' does not exist", call. = FALSE)
            }

            removed_step_numbers = step_number:self$length()
            removed_step_names = self$pipeline[["step"]][removed_step_numbers]

            self$pipeline = self$pipeline[-removed_step_numbers, ]

            removed_step_names
        },


        #' @description Remove certain step from the pipeline. If step does
        #' not exist, an error is given.
        #' @param step `string` the name of the step to be removed.
        #' @return the `Pipeline` object invisibly
        remove_step = function(step) {

            existing_steps = self$pipeline[["step"]]
            step_number = match(step, existing_steps)

            if (is.na(step_number)) {
                stop("step '", step, "' does not exist", call. = FALSE)
            }

            # Verify that step to be removed has no dependencies relying on it
            deps = self$pipeline[["deps"]]
            names(deps) = self$pipeline[["step"]]
            dependents = names(Filter(x = deps, f = function(x) step %in% x))

            if (length(dependents) > 0) {
                stop("cannot remove step '", step,
                    "' as other steps depend on it, namely, ",
                    paste0("'", dependents, "'", collapse = ", ")
                )
            }

            self$pipeline = self$pipeline[-step_number, ]
            invisible(self)
        },

        #' @description Replace pipeline step
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
        replace_step = function(
            step,
            fun,
            params = NULL,
            description = "",
            group = step,
            keepOut = FALSE
        ) {
            private$.verify_step_exists(step)

            # Function can be either a function or a character string
            if (is.function(fun)) {
                funcName <- as.character(substitute(fun))[[1]]
            }
            else {
                funcName <- fun
                fun <- get(fun, mode = "function")
            }

            # Update default function params by custom params
            if (is.null(params)) {
                params <- formals(fun)
            } else {
                params <- replace(formals(fun), names(params), params)
            }

            # Make sure parameters defined as Param object or call are evaluated
            isDefined <- sapply(
                params,
                function(x) {
                    methods::is(x, "Param") ||
                    methods::is(x, "call")
                }
            )
            params[isDefined] <- lapply(params[isDefined], eval)

            private$.verify_fun_params(fun, funcName, params)

            # Derive and verify dependencies
            all_steps <- self$get_step_names()
            step_index <- match(step, all_steps)

            deps <- private$.derive_dependencies(
                params = params,
                step = step,
                to_step = all_steps[step_index - 1]
            )
            sapply(
                deps,
                FUN = private$.verify_dependency,
                step = step,
                to_step = all_steps[step_index - 1]
            )

            new_step <- list(
                step = step,
                fun = list(fun),
                funcName = funcName,
                params = list(params),
                keepOut = keepOut,
                deps = list(deps),
                out = list(list()),
                group = group,
                description = description
            )

            self$pipeline[step_index, ] <- new_step

            invisible(self)
        },

        #' @description Set data in first step of pipeline.
        #' @param data `data.frame` initial data set.
        #' @return returns the `Pipeline` object invisibly
        set_data = function(data)
        {
            data.table::set(
                self$pipeline,
                i = 1L,
                j = "fun",
                value = {function() data}
            )
            invisible(self)
        },

        #' @description Split-copy pipeline by list of data sets
        #' @param dataList `list` of data sets
        #' @param to_step `string` step name marking optional subset of
        #' the pipeline, at which the data split should be applied to.
        #' @param groupBySplit `logical` whether to set step groups according
        #' to data split.
        #' @return new combined `Pipeline` with each sub-pipeline having set
        #' one of the data sets.
        set_data_split = function(
            dataList,
            to_step = utils::tail(self$get_step_names(), 1),
            groupBySplit = TRUE
        ) {
            stopifnot(
                is.list(dataList),
                to_step %in% self$get_step_names()
            )
            to = match(to_step, self$get_step_names())
            upper_steps = unlist(self$pipeline[1:to, ][["step"]])

            dataNames = names(dataList)
            if (is.null(dataNames))
                dataNames = as.character(seq_along(dataList))

            init_new_pipeline_with_data = function(data) {
                self$clone(deep = TRUE)$set_data(data)
            }

            pipes = lapply(dataList, init_new_pipeline_with_data)
            for (i in seq_along(pipes)) {
                name <- dataNames[[i]]
                pipes[[i]]$name <- name
                pipes[[i]]$pipeline <- pipes[[i]]$pipeline[1:to, ]

                new_groups <- name
                if (!groupBySplit) {
                    old_groups = pipes[[i]]$pipeline[["group"]]
                    new_groups = paste0(name, ".", old_groups)
                }
                pipes[[i]]$pipeline[["group"]] = new_groups
            }

            # Combine pipelines
            pipe_names = sapply(pipes, function(x) x$name)
            combined_name = paste(self$name, "split")
            pip = Pipeline$new(combined_name)
            combined = Reduce(c(pip, pipes), f = function(x, y) x$append(y))
            combined$remove_step(".data")

            # If subset was used for split, append the remaining steps and
            # update all of the (now changed) upper dependencies.
            if (to < self$length()) {
                remaining_pipe = self$pipeline[(to + 1):self$length(), ]
                remaining_deps = remaining_pipe[["deps"]]
                update_if_needed = function(x) {
                    needs_update = x %in% upper_steps
                    if (needs_update) paste0(x, ".", pipe_names) else x
                        #paste0(pipe_names, ".", x) else x
                }
                updated_deps = lapply(
                    remaining_deps,
                    function(deps) {
                        res = lapply(deps, update_if_needed)
                        if (all(sapply(res, function(x) length(x) == 1))) {
                            res = unlist(res)   # flatten single deps
                        }
                        res
                    }
                )
                remaining_pipe[["deps"]] = updated_deps
                combined$pipeline = rbind(combined$pipeline, remaining_pipe)
            }

            self$pipeline = combined$pipeline
            invisible(self)
        },

        #' @description Set parameters in the pipeline. If a parameter occurs
        #' in several steps, the parameter is set commonly in all steps.
        #' @param params `list` of parameters to be set
        #' @param warnUndefined `logical` whether to give a warning if a
        #' parameter is not defined in the pipeline.
        #' @return returns the `Pipeline` object invisibly
        set_parameters = function(params, warnUndefined = TRUE)
        {
            pipe_param_names = lapply(
                self$get_unique_parameters(ignoreHidden = FALSE),
                FUN = names
            )
            extra = setdiff(names(params), unlist(pipe_param_names))

            if (warnUndefined && length(extra) > 0) {
                warning(
                    "Trying to set parameters not defined in the pipeline: ",
                    toString(extra)
                )
            }

            for (i in seq_len(nrow(self$pipeline))) {
                params_ith_step = self$pipeline[["params"]][[i]]
                overlap = intersect(names(params), names(params_ith_step))

                if (length(overlap) > 0) {
                    params_ith_step[overlap] = params[overlap]
                    self$pipeline[["params"]][[i]] = params_ith_step
                }
            }
            invisible(self)
        },

        #' @description Set unbound parameter values at given pipeline step.
        #' @param step `string` the name of the step
        #' @param params `list` of parameters to be set
        #' @param warnUndefined `logical` whether to give a warning if a
        #' parameter is not defined in the pipeline.
        #' @return returns the `Pipeline` object invisibly
        set_parameters_at_step = function(
            step,
            params
        ) {
            stopifnot(
                is_string(step),
                is.list(params)
            )
            current = self$get_parameters_at_step(step, ignoreHidden = FALSE)

            extra <- setdiff(names(params), names(current))
            if (length(extra) > 0) {
                stop(
                    "Unable to set parameter(s) ", toString(extra),
                    " at step ", step, " - candidates are ",
                    toString(names(current))
                )
            }

            toUpdate = intersect(names(params), names(current))
            hasUpdate = length(toUpdate) > 0
            if (hasUpdate) {
                row <- which(self$pipeline[["step"]] == step)
                old <- self$pipeline[["params"]][[row]]
                new <- utils::modifyList(old, params[toUpdate])
                self$pipeline[["params"]][[row]] <- new
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

        .derive_dependencies = function(
            params,
            step,
            to_step = private$.get_last_step()
        ) {
            if (length(to_step) == 0) {
                return(character())
            }

            stopifnot(
                is_string(step),
                is_string(to_step)
            )

            private$.verify_step_exists(to_step)

            deps <- private$.extract_deps_from_param_list(params)

            # Handle any relative dependencies
            all_steps <- self$get_step_names()
            to_index <- private$.get_step_index(to_step)
            considered_steps = all_steps[seq_len(to_index)]
            relative_deps <- deps |>
                Filter(f = function(x) startsWith(x, "-")) |>
                sapply(as.numeric)

            step_indices <- mapply(
                relative_dep = relative_deps,
                dependency_name = names(relative_deps),
                FUN = private$.relative_dependency_to_index,
                MoreArgs = list(
                    start_index = to_index + 1,
                    step = step
                )
            ) |> as.integer()
            deps[names(relative_deps)] = considered_steps[step_indices]


            deps
        },

        .execute_step = function(step)
        {
            private$.verify_step_exists(step)
            pip <- self$pipeline
            step_number <- private$.get_step_index(step)

            row <- pip[step_number, ] |> unlist1()
            fun <- row[["fun"]]
            args <- row[["params"]]
            deps <- row[["deps"]]

            # If calculation depends on results of earlier steps, get them from
            # respective output slots of the pipeline.
            hasDeps <- length(deps) > 0
            if (hasDeps) {
                out <- pip[["out"]] |> stats::setNames(pip[["step"]])
                dependent_out <- private$.extract_dependent_out(deps, out)
                args[names(dependent_out)] <- dependent_out
            }

            # If arg is encapsulated in a Param object, get the value
            args <- lapply(
                args,
                FUN = function(arg) {
                    if (methods::is(arg, "Param")) arg@value else arg
                }
            )

            step <- pip[["step"]][[step_number]]
            context <- gettextf("pipeline at step %i ('%s')", step_number, step)

            res = tryCatchLog(
                do.call(fun, args = args),
                execution_context = context
            )

            res
        },

        .extract_dependent_out = function(
            deps,
            out
        ) {
            stopifnot(
                is.character(deps) || is.list(deps),
                is.list(out),
                all(unlist(deps) %in% names(out))
            )

            if (length(deps) == 0) {
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

            lapply(deps, FUN = extract_out)
        },

        .extract_deps_from_param_list = function(
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

        .get_downstream_deps = function(
            step,
            deps,
            recursive = TRUE
        ) {
            stopifnot(
                is_string(step),
                is.character(deps) || is.list(deps),
                is.logical(recursive)
            )

            result <- deps |>
                Filter(f = function(x) step %in% unlist(x)) |>
                names()

            if (recursive) {
                result <- c(
                    result,
                    sapply(
                        result,
                        FUN = private$.get_downstream_deps,
                        deps = deps,
                        recursive = TRUE
                    )
                )
            }

            unique(unlist(result)) |> as.character()
        },

        .get_last_step = function() {
            self$get_step_names() |> utils::tail(1)
        },

        .get_step_index = function(step) {
            private$.verify_step_exists(step)
            match(step, self$get_step_names())
        },

        .get_upstream_deps = function(
            step,
            deps,
            recursive = TRUE
        ) {
            stopifnot(
                is_string(step),
                is.character(deps) || is.list(deps),
                is.logical(recursive)
            )

            if (length(deps) == 0) {
                return(character())
            }

            result <- unlist(deps[[step]]) |> as.character()

            if (recursive) {
                result <- c(
                    result,
                    sapply(
                        result,
                        FUN = private$.get_upstream_deps,
                        deps = deps,
                        recursive = TRUE
                    )
                )
            }

            unique(unlist(result)) |> as.character()
        },

        .relative_dependency_to_index = function(
            relative_dep,
            dependency_name,
            start_index,
            step   # required for error message
        ) {
            stopifnot(
                is_number(relative_dep),
                relative_dep < 0,
                is_string(dependency_name),
                is_number(start_index),
                start_index > 0,
                is_string(step)
            )
            dep_name <- names(relative_dep)

            abs_index <- relative_dep + start_index

            if (abs_index < 1) {
                stop(
                    "step '", step, "': relative dependency ",
                    paste0(dependency_name, "=", relative_dep),
                    " points to outside the pipeline"
                )
            }

            abs_index
        },

        .verify_dependency = function(
            dep,
            step,   # required for error message
            to_step = private$.get_last_step()
        ) {
            stopifnot(
                is_string(dep),
                is_string(step),
                is_string(to_step)
            )

            private$.verify_step_exists(to_step)

            all_steps <- self$get_step_names()
            to_index <- match(to_step, all_steps)
            considered_steps <- all_steps[seq_len(to_index)]

            if (dep %!in% considered_steps) {
                msg = paste0(
                    "step '", step, "': dependency '", dep, "' not found"
                )

                if (to_step != private$.get_last_step()) {
                    msg = paste0(
                        msg, " up to step '", considered_steps[to_index], "'"
                    )
                }
                stop(msg, call. = FALSE)
            }

            invisible(TRUE)
        },


        .verify_fun_params = function(fun, funcName, params = list())
        {
            stopifnot(
                is.function(fun),
                is_string(funcName),
                is.list(params)
            )

            fargs = formals(fun)
            unknown_params = setdiff(names(params), names(fargs))
            if (length(unknown_params) > 0) {
                stop(
                    paste0("'", unknown_params, "'", collapse = ", "),
                    " are no function parameters of '", funcName, "'",
                    call. = FALSE
                )
            }


            defined_params = Filter(params, f = Negate(is.name))
            argsWithNoDefault = names(Filter(fargs, f = \(x) is.name(x)))
            undefinedArgs = setdiff(argsWithNoDefault, names(defined_params))
            if (length(undefinedArgs) > 0) {
                stop(
                    paste0("'", undefinedArgs, "'", collapse = ", "),
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
                stop("step '", step, "' does not exists", call. = FALSE)
            }
        }
    )
)

#' @title Add to pipeline
#' @description Helper functions to enable pipeline construction via R's pipe
#' @param x A pipeline object
#' @param ... Arguments passed to the pipeline's add() method
#' @return The pipeline object
#' @export
pipe_add = function(x, ...)
    x$add(...)
