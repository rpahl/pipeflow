# Package index

## Getting started

Create a pipeline, add steps, run it, and collect results.

- [`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
  : Create a pipeline
- [`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
  : Add a step
- [`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
  : Run a pipeline
- [`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect_out.md)
  : Collect step outputs
- [`pipeflow`](https://github.com/rpahl/pipeflow/reference/pipeflow-package.md)
  [`pipeflow-package`](https://github.com/rpahl/pipeflow/reference/pipeflow-package.md)
  : pipeflow: Lightweight, General-Purpose Data Analysis Pipelines

## Modify steps

Replace, rename, or remove steps; adjust tunable parameters.

- [`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md)
  : Replace a step
- [`pip_rename()`](https://github.com/rpahl/pipeflow/reference/pip_rename.md)
  : Rename a step
- [`pip_remove()`](https://github.com/rpahl/pipeflow/reference/pip_remove.md)
  : Remove a step
- [`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
  : Set independent parameters

## Pipeline composition

Combine, clone, or copy steps between pipelines.

- [`pip_add_from()`](https://github.com/rpahl/pipeflow/reference/pip_add_from.md)
  : Copy a step from another pipeline
- [`pip_bind()`](https://github.com/rpahl/pipeflow/reference/pip_bind.md)
  : Bind pipelines
- [`pip_clone()`](https://github.com/rpahl/pipeflow/reference/pip_clone.md)
  : Clone a pipeline

## Inspection and views

Inspect the pipeline structure and create filtered views.

- [`pip_get_graph()`](https://github.com/rpahl/pipeflow/reference/pip_get_graph.md)
  : Build pipeline graph data
- [`pip_get_params()`](https://github.com/rpahl/pipeflow/reference/pip_get_params.md)
  : Get independent parameters
- [`pip_has_step()`](https://github.com/rpahl/pipeflow/reference/pip_has_step.md)
  : Check whether a step exists
- [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
  : Create a pipeline view

## Locking and tagging

Lock steps against changes or tag them for filtering.

- [`pip_lock()`](https://github.com/rpahl/pipeflow/reference/pip_lock.md)
  : Lock selected steps against updates
- [`pip_unlock()`](https://github.com/rpahl/pipeflow/reference/pip_unlock.md)
  : Unlock selected steps
- [`pip_tag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md)
  : Add tags to selected steps
- [`pip_untag()`](https://github.com/rpahl/pipeflow/reference/pip_untag.md)
  : Remove tags from selected steps

## S3 methods

Generic methods for pipeflow objects.

- [`length(`*`<pipeflow_pip>`*`)`](https://github.com/rpahl/pipeflow/reference/length.pipeflow.md)
  [`length(`*`<pipeflow_view>`*`)`](https://github.com/rpahl/pipeflow/reference/length.pipeflow.md)
  : Length of a pipeflow pipeline or view
- [`print(`*`<pipeflow_pip>`*`)`](https://github.com/rpahl/pipeflow/reference/print.md)
  [`print(`*`<pipeflow_view>`*`)`](https://github.com/rpahl/pipeflow/reference/print.md)
  : Print pipeflow objects
- [`` `[`( ``*`<pipeflow_pip>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow_pip.md)
  [`` `[[`( ``*`<pipeflow_pip>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow_pip.md)
  : Extract or subset a pipeline
- [`set_log_layout()`](https://github.com/rpahl/pipeflow/reference/set_log_layout.md)
  : Set pipeflow log layout
