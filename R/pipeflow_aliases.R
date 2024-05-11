
# nocov start
#' @title Pipeline alias functions
#' @description Alias functions, one for each member function of a Pipeline
#'  object.
#' @param pip A pipeline object
#' @param ... Arguments passed to the respective pipeline method
#' @return The result of the respective pipeline method
#' @name pipelineAliases
NULL

#' @rdname pipelineAliases
#' @export
pipe_add = function(pip, ...)
    pip$add(...)


#' @rdname pipelineAliases
#' @export
pipe_append = function(pip, ...)
    pip$append(...)


#' @rdname pipelineAliases
#' @export
pipe_append_to_step_names = function(pip, ...)
    pip$append_to_step_names(...)


#' @rdname pipelineAliases
#' @export
pipe_clone = function(pip, ...)
    pip$clone(...)


#' @rdname pipelineAliases
#' @export
pipe_collect_out = function(pip, ...)
    pip$collect_out(...)


#' @rdname pipelineAliases
#' @export
pipe_discard_steps = function(pip, ...)
    pip$discard_steps(...)


#' @rdname pipelineAliases
#' @export
pipe_get_data = function(pip, ...)
    pip$get_data(...)


#' @rdname pipelineAliases
#' @export
pipe_get_depends = function(pip, ...)
    pip$get_depends(...)


#' @rdname pipelineAliases
#' @export
pipe_get_depends_down = function(pip, ...)
    pip$get_depends_down(...)


#' @rdname pipelineAliases
#' @export
pipe_get_depends_up = function(pip, ...)
    pip$get_depends_up(...)


#' @rdname pipelineAliases
#' @export
pipe_get_graph = function(pip, ...)
    pip$get_graph(...)


#' @rdname pipelineAliases
#' @export
pipe_get_out = function(pip, ...)
    pip$get_out(...)


#' @rdname pipelineAliases
#' @export
pipe_get_params = function(pip, ...)
    pip$get_params(...)


#' @rdname pipelineAliases
#' @export
pipe_get_params_at_step = function(pip, ...)
    pip$get_params_at_step(...)


#' @rdname pipelineAliases
#' @export
pipe_get_params_unique = function(pip, ...)
    pip$get_params_unique(...)


#' @rdname pipelineAliases
#' @export
pipe_get_params_unique_json = function(pip, ...)
    pip$get_params_unique_json(...)


#' @rdname pipelineAliases
#' @export
pipe_get_step = function(pip, ...)
    pip$get_step(...)


#' @rdname pipelineAliases
#' @export
pipe_get_step_names = function(pip, ...)
    pip$get_step_names(...)


#' @rdname pipelineAliases
#' @export
pipe_get_step_number = function(pip, ...)
    pip$get_step_number(...)


#' @rdname pipelineAliases
#' @export
pipe_has_step = function(pip, ...)
    pip$has_step(...)


#' @rdname pipelineAliases
#' @export
pipe_insert_after = function(pip, ...)
    pip$insert_after(...)


#' @rdname pipelineAliases
#' @export
pipe_insert_before = function(pip, ...)
    pip$insert_before(...)


#' @rdname pipelineAliases
#' @export
pipe_length = function(pip, ...)
    pip$length(...)


#' @rdname pipelineAliases
#' @export
pipe_lock_step = function(pip, ...)
    pip$lock_step(...)

#' @rdname pipelineAliases
#' @export
pipe_new = function(...)
    Pipeline$new(...)


#' @rdname pipelineAliases
#' @export
pipe_print = function(pip, ...)
    pip$print(...)


#' @rdname pipelineAliases
#' @export
pipe_pop_step = function(pip, ...)
    pip$pop_step(...)


#' @rdname pipelineAliases
#' @export
pipe_pop_steps_after = function(pip, ...)
    pip$pop_steps_after(...)


#' @rdname pipelineAliases
#' @export
pipe_pop_steps_from = function(pip, ...)
    pip$pop_steps_from(...)


#' @rdname pipelineAliases
#' @export
pipe_remove_step = function(pip, ...)
    pip$remove_step(...)


#' @rdname pipelineAliases
#' @export
pipe_replace_step = function(pip, ...)
    pip$replace_step(...)


#' @rdname pipelineAliases
#' @export
pipe_reset = function(pip, ...)
    pip$reset(...)


#' @rdname pipelineAliases
#' @export
pipe_run = function(pip, ...)
    pip$run(...)


#' @rdname pipelineAliases
#' @export
pipe_run_step = function(pip, ...)
    pip$run_step(...)


#' @rdname pipelineAliases
#' @export
pipe_set_data = function(pip, ...)
    pip$set_data(...)


#' @rdname pipelineAliases
#' @export
pipe_set_data_split = function(pip, ...)
    pip$set_data_split(...)


#' @rdname pipelineAliases
#' @export
pipe_set_keep_out = function(pip, ...)
    pip$set_keep_out(...)


#' @rdname pipelineAliases
#' @export
pipe_set_params = function(pip, ...)
    pip$set_params(...)


#' @rdname pipelineAliases
#' @export
pipe_set_params_at_step = function(pip, ...)
    pip$set_params_at_step(...)


#' @rdname pipelineAliases
#' @export
pipe_split = function(pip, ...)
    pip$split(...)


#' @rdname pipelineAliases
#' @export
pipe_unlock_step = function(pip, ...)
    pip$unlock_step(...)

# nocov end
