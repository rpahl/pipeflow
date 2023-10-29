.replace_string = function(x, target, replacement) {

    if (length(x) == 0) {
        return(x)
    }

    stopifnot(
        is.character(x),
        is_string(target),  # nolint
        is_string(replacement)
    )
    x[x %in% target] = replacement
    x
}


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
                name = character(0),
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
        #' @param name `string` the name of the step. Each step name must be
        #' unique.
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
        #' @return returns the `Pipeline` object invisibly
        add = function(name,
                       fun,
                       params = NULL,
                       description = "",
                       group = name,
                       keepOut = FALSE)
        {
            private$.verify_name(name)

            # Function can be either a function or a character string
            if (is.function(fun)) {
                funcName = as.character(substitute(fun))[[1]]
            }
            else {
                funcName = fun
                fun = get(fun, mode = "function")
            }

            # Update default function params by custom params
            if (is.null(params))
                params = formals(fun)
            else
                params = replace(formals(fun), names(params), params)

            # Make sure parameters defined as Param object or call are evaluated
            isDefined = sapply(
                params,
                function(x) {
                    methods::is(x, "Param") || methods::is(x, "call")
                }
            )
            params[isDefined] = lapply(params[isDefined], eval)


            # Verify function parameters and determine and verify dependencies
            private$.verify_fun_params(fun, funcName, params)
            deps = private$.derive_deps(params, name)
            private$.verify_deps(deps, name)

            self$pipeline =
                rbind(self$pipeline,
                list(name = name,
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

            # Adapt step names and their deps to prevent name clashes
            private$.add_prefix_to_step_names(p2, p2$name)

            if (outAsIn) {
                len1 = p1$length()
                last_step1 = p1$pipeline[len1, "name"][[1]]
                first_step2 = p2$pipeline[1, "name"][[1]]
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

            step_names = combined_pipe$pipeline[["name"]]
            if (any(duplicated(step_names))) {  # this should never happen
                duplicatedNames = step_names[duplicated(step_names)]
                stop("Combined pipeline has duplicated step names:",
                    paste0("'", duplicatedNames, "'", sep = ", ")
                )
            }

            combined_pipe
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
                names(results) = subpipe[["name"]]
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
            step_names = self$pipeline[["name"]]
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
        #' @return returns the `Pipeline` object invisibly
        execute = function(from = 1, to = self$length())
        {
            gettextf("Start execution of '%s' pipeline:", self$name) |>
                log_info(type = "start_pipeline", pipeline_name = self$name)

            for (i in from:to) {
                stepName = as.character(self$pipeline[i, "name"])
                gettextf("Step %i/%i %s",  i, to, stepName) |> log_info()

                res = private$.execute_row(i)

                if (inherits(res, "Pipeline")) {
                    # Allows to run pipelines created by pipelines
                    log_info("Abort pipeline execution and restart on new.")
                    self = res
                    self$execute()
                    return(invisible(self))
                }

                if (length(res) > 0) {
                    self$pipeline[["out"]][[i]] = res
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

        #' @description Get all function parameters defined in the pipeline.
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of parameters, sorted and named by step. Steps with
        #' no parameters are filtered out.
        get_parameters = function(ignoreHidden = TRUE)
        {
            res <- lapply(
                self$pipeline[["name"]],
                FUN = self$get_parameters_at_step,
                ignoreHidden = ignoreHidden
            )

            names(res) <- self$pipeline[["name"]]
            Filter(res, f = function(x) length(x) > 0)
        },

        #' @description Get all function parameters at given step name.
        #' @param name `string` name of step
        #' @param ignoreHidden `logical` if TRUE, hidden parameters (i.e. all
        #' names starting with a dot) are ignored and thus not returned.
        #' @return `list` of parameters defined at given step.
        get_parameters_at_step = function(name, ignoreHidden = TRUE)
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

            step <- self$get_step(name)

            step[["params"]] |>
                unlist1() |>
                filter_desired_parameters() |>
                as.list()
        },


        #' @description Get step of pipeline
        #' @param name `string` name of step
        #' @return `data.table` row containing the step. If step not found, an
        #' error is given.
        get_step = function(name)
        {
            pos = Position(
                self$pipeline[["name"]],
                f = function(x) x == name,
                nomatch = stop("step '", name, "' not found")
            )

            self$pipeline[pos, ]
        },

        #' @description Get step names of pipeline
        #' @return `character` vector of step names
        get_step_names = function()
        {
            self$pipeline[["name"]]
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

        #' @description Set pipeline to keep all output regardless of the flags
        #' that were set for the individual steps. This can be useful for
        #' debugging.
        #' @return `logical` original set of output flags.
        keep_all_out = function() {
            old = self$pipeline[["keepOut"]]
            names(old) = self$pipeline[["name"]]

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
            last_step_name = self$pipeline[["name"]][[len]]

            self$pipeline = self$pipeline[-len, ]
            last_step_name
        },

        #' @description Remove all steps after the given step.
        #' @param name `string` name of step
        #' @return `character` vector of steps that were removed.
        pop_steps_after = function(name) {

            existing_steps = self$pipeline[["name"]]
            step_number = match(name, existing_steps)

            if (is.na(step_number)) {
                stop("step '", name, "' does not exist", call. = FALSE)
            }

            if (step_number == self$length())
                return(character(0))    # nothing to remove

            next_step = step_number + 1
            removed_step_numbers = next_step:self$length()
            removed_step_names = self$pipeline[["name"]][removed_step_numbers]

            self$pipeline = self$pipeline[-removed_step_numbers, ]

            removed_step_names
        },

        #' @description Remove all steps from and including the given step.
        #' @param name `string` name of step
        #' @return `character` vector of steps that were removed.
        pop_steps_from = function(name) {

            existing_steps = self$pipeline[["name"]]
            step_number = match(name, existing_steps)

            if (is.na(step_number)) {
                stop("step '", name, "' does not exist", call. = FALSE)
            }

            removed_step_numbers = step_number:self$length()
            removed_step_names = self$pipeline[["name"]][removed_step_numbers]

            self$pipeline = self$pipeline[-removed_step_numbers, ]

            removed_step_names
        },


        #' @description Remove certain step from the pipeline. If step does
        #' not exist, an error is given.
        #' @param name `string` the name of the step to be removed.
        #' @return the `Pipeline` object invisibly
        remove_step = function(name) {

            existing_steps = self$pipeline[["name"]]
            step_number = match(name, existing_steps)

            if (is.na(step_number)) {
                stop("step '", name, "' does not exist", call. = FALSE)
            }

            # Verify that step to be removed has no dependencies relying on it
            deps = self$pipeline[["deps"]]
            names(deps) = self$pipeline[["name"]]
            dependents = names(Filter(x = deps, f = function(x) name %in% x))

            if (length(dependents) > 0) {
                stop("cannot remove step '", name,
                    "' as other steps depend on it, namely, ",
                    paste0("'", dependents, "'", collapse = ", ")
                )
            }

            self$pipeline = self$pipeline[-step_number, ]
            invisible(self)
        },

        #' @description Replace pipeline step
        #' @param name `string` the name of the step to be replaced. Step must
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
            name,
            fun,
            params = NULL,
            description = "",
            group = name,
            keepOut = FALSE
        ) {
            existing_steps = self$pipeline[["name"]]
            step_number = match(name, existing_steps)

            if (is.na(step_number)) {
                stop("cannot replace '", name,
                    "' - step does not exist", call. = FALSE
                    )
            }

            # Function can be either a function or a character string
            if (is.function(fun)) {
                funcName = as.character(substitute(fun))[[1]]
            }
            else {
                funcName = fun
                fun = get(fun, mode = "function")
            }

            # Update default function params by custom params
            if (is.null(params)) {
                params = formals(fun)
            } else {
                params = replace(formals(fun), names(params), params)
            }

            # Make sure parameters defined as Param object or call are evaluated
            isDefined = sapply(
                params,
                function(x) {
                    methods::is(x, "Param") ||
                    methods::is(x, "call")
                }
            )
            params[isDefined] = lapply(params[isDefined], eval)


            # Verify function parameters and determine and verify dependencies
            private$.verify_fun_params(fun, funcName, params)
            deps = private$.derive_deps(params, name, to = step_number - 1)
            private$.verify_deps(deps, name, to = step_number - 1)

            new_step = list(
                name = name,
                fun = list(fun),
                funcName = funcName,
                params = list(params),
                keepOut = keepOut,
                deps = list(deps),
                out = list(list()),
                group = group,
                description = description
            )

            self$pipeline[step_number, ] = new_step

            invisible(self)
        },

        #' @description Set parameters in the pipeline. If a parameter occurs
        #' in several steps, the parameter is set commonly in all steps.
        #' @param params `list` of parameters to be set
        #' @param warnUndefined `logical` whether to give a warning if a
        #' parameter is not defined in the pipeline.
        #' @return returns the `Pipeline` object invisibly
        set_common_parameters = function(params, warnUndefined = TRUE)
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
            upper_steps = unlist(self$pipeline[1:to, ][["name"]])

            dataNames = names(dataList)
            if (is.null(dataNames))
                dataNames = as.character(seq_along(dataList))

            init_new_pipeline_with_data = function(data) {
                self$clone(deep = TRUE)$set_data(data)
            }

            pipes = lapply(dataList, init_new_pipeline_with_data)
            for (i in seq_along(pipes)) {
                name = dataNames[[i]]
                pipes[[i]]$name = name
                pipes[[i]]$pipeline = pipes[[i]]$pipeline[1:to, ]

                new_groups = name
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
                    if (needs_update) paste0(pipe_names, ".", x) else x
                }
                updated_deps = lapply(remaining_deps,
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

        #' @description Set unbound parameter values at given pipeline step.
        #' @param name `string` the name of the step
        #' @param params `list` of parameters to be set
        #' @param warnUndefined `logical` whether to give a warning if a
        #' parameter is not defined in the pipeline.
        #' @return returns the `Pipeline` object invisibly
        set_parameters_at_step = function(
            name,
            params
        ) {
            stopifnot(
                is_string(name),
                is.list(params)
            )
            current = self$get_parameters_at_step(name, ignoreHidden = FALSE)

            extra <- setdiff(names(params), names(current))
            if (length(extra) > 0) {
                stop(
                    "Unable to set parameter(s) ", toString(extra),
                    " at step ", name, " - candidates are ",
                    toString(names(current))
                )
            }

            toUpdate = intersect(names(params), names(current))
            hasUpdate = length(toUpdate) > 0
            if (hasUpdate) {
                row <- which(self$pipeline[["name"]] == name)
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

        .add_prefix_to_step_names = function(pip, prefix) {

            stopifnot(is_string(prefix))
            add_prefix = function(x) paste0(prefix, ".", x)

            pip$pipeline[["name"]] = add_prefix(pip$pipeline[["name"]])
            pip$pipeline[["deps"]] = pip$pipeline[["deps"]] |>
                lapply(function(x) sapply(x, add_prefix))
        },

        .execute_row = function(i)
        {
            fun = self$pipeline[["fun"]][[i]]
            args = as.list(self$pipeline[["params"]][[i]])

            deps = self$pipeline[["deps"]][[i]]

            # If calculation depends on earlier results, get them from
            # respective output slots of the pipeline.
            hasDeps = length(deps) > 0
            if (hasDeps) {
                out = self$pipeline[["out"]][1:i]
                names(out) = self$pipeline[["name"]][1:i]

                dependent_args = lapply(
                    deps,
                    function(x) if (length(x) == 1) out[[x]] else out[x]
                )
                args[names(dependent_args)] = dependent_args
            }

            # If arg is encapsulated in a Param object, get the value
            args = lapply(args,
                          function(arg) {
                              if (methods::is(arg, "Param"))
                                  arg@value else arg
                          }
            )

            name = self$pipeline[["name"]][[i]]
            context = paste0("pipeline at step ", i, " ('", name, "')")

            res = tryCatchLog(
                do.call(fun, args = args),
                execution_context = context
            )

            res
        },

        .derive_deps = function(params, name, to = self$length())
        {
            if (is.null(params)) {
                return(list())
            }

            deps <- params |>
                Filter(f = function(x) methods::is(x, "formula")) |>
                sapply(FUN = function(x) deparse(x[[2]]))

            if (length(deps) == 0) {
                return(list())
            }

            available_steps = self$pipeline[["name"]][seq_len(to)]

            # Handle relative index numbers
            are_negative_numbers = function(x) grepl("^-[0-9]*", x = x)
            relative_deps = Filter(deps, f = are_negative_numbers)
            indices = sapply(relative_deps, as.numeric)
            abs_indices = private$.relative_to_absolute_indices(
                indices, name, to
            )
            deps[names(relative_deps)] = available_steps[abs_indices]

            deps
        },

        .relative_to_absolute_indices = function(
            relative_indices, name, to = self$length()
        ) {
            if (length(relative_indices) == 0)
                return(integer(0))

            absolute_indices = to + relative_indices + 1
            exceeding = Filter(absolute_indices, f = function(x) x < 1)
            if (length(exceeding) > 0) {
                stop(
                    "in step '", name, "' one or more relative indices ",
                    "exceed the pipeline: ",
                    paste0("'", names(exceeding), "'", collapse = ", ")
                )
            }

            absolute_indices
        },


        .verify_deps = function(deps, name, to = self$length())
        {
            existing_steps = self$pipeline[["name"]][seq_len(to)]
            if (length(deps) == 0)
                return(invisible(TRUE))

            stop_on_missing_dep = function(dep) {
                msg = paste0(
                    "step '", name, "': dependency '", dep, "' not found"
                )

                if (to != self$length()) {
                    msg = paste0(msg,
                        " up to step '", existing_steps[to], "'"
                    )
                }
                stop(msg, call. = FALSE)
            }

            for (dep in deps) {
                if (dep %!in% existing_steps) {
                    stop_on_missing_dep(dep)
                }
            }

            invisible(TRUE)
        },

        .verify_fun_params = function(fun, funcName, params)
        {
            fargs = formals(fun)
            defined_params = Filter(params, f = Negate(is.name))
            unknown_params = setdiff(names(params), names(fargs))

            if (length(unknown_params) > 0)
                stop(paste0("'", unknown_params, "'", collapse = ", "),
                     " are no function parameters of '", funcName, "'",
                     call. = FALSE)

            argsWithNoDefault = names(Filter(fargs, f = function(x) is.name(x)))
            undefinedArgs = setdiff(argsWithNoDefault, names(defined_params))
            if (length(undefinedArgs) > 0) {
                stop(paste0("'", undefinedArgs, "'", collapse = ", "),
                     " parameter(s) must have default values", call. = FALSE)
            }
            invisible(TRUE)
        },

        .verify_name = function(name)
        {
            stopifnot(
                is_string(name),
                nzchar(name)
            )
            if (name %in% self$pipeline[["name"]])
                stop("name '", name, "' already exists", call. = FALSE)
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
