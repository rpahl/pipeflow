# Package index

## Getting started

Create a pipeline, add steps, and collect results.

- [`pipeflow`](https://github.com/rpahl/pipeflow/reference/pipeflow-package.md)
  [`pipeflow-package`](https://github.com/rpahl/pipeflow/reference/pipeflow-package.md)
  : pipeflow: Fast Interactive Data Analysis Pipelines
- [`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
  : Create a pipeline
- [`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
  : Add a step
- [`pip_collect()`](https://github.com/rpahl/pipeflow/reference/pip_collect.md)
  [`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect.md)
  : Collect step outputs

## Modify steps

Replace, rename, remove, reset, or re-configure steps.

- [`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md)
  : Replace a step
- [`pip_rename()`](https://github.com/rpahl/pipeflow/reference/pip_rename.md)
  : Rename a step
- [`pip_remove()`](https://github.com/rpahl/pipeflow/reference/pip_remove.md)
  : Remove a step
- [`pip_reset()`](https://github.com/rpahl/pipeflow/reference/pip_reset.md)
  : Reset a pipeline to its initial state
- [`pip_get_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
  [`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
  : Get or set unbound parameters

## Pipeline composition

Combine or clone pipelines.

- [`rbind(`*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/rbind.pipeflow.md)
  : Bind pipelines
- [`pip_clone()`](https://github.com/rpahl/pipeflow/reference/pip_clone.md)
  : Clone a pipeline

## Running pipelines

Execute pipeline steps or views.

- [`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
  : Run a pipeline

## Locking and tagging

Lock steps against changes or tag them for filtering.

- [`pip_lock()`](https://github.com/rpahl/pipeflow/reference/pip_lock.md)
  [`pip_unlock()`](https://github.com/rpahl/pipeflow/reference/pip_lock.md)
  : Lock or unlock steps
- [`pip_tag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md)
  [`pip_untag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md)
  : Add or remove tags

## Inspection and views

Inspect the pipeline structure, access the step table, and create
filtered views.

- [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
  : Create a pipeline view
- [`pip_data()`](https://github.com/rpahl/pipeflow/reference/pip_data.md)
  : Access the underlying step table
- [`pip_graph()`](https://github.com/rpahl/pipeflow/reference/pip_graph.md)
  [`pip_get_graph()`](https://github.com/rpahl/pipeflow/reference/pip_graph.md)
  : Build pipeline graph data

## Extract and replace operators

Subset, read from and write to a pipeline using base R operators.

- [`` `[`( ``*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow.md)
  [`` `[[`( ``*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow.md)
  [`` `[[<-`( ``*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow.md)
  [`` `[<-`( ``*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow.md)
  [`` `$`( ``*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow.md)
  [`` `$<-`( ``*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/Extract.pipeflow.md)
  : Extract or replace parts of a pipeline

## S3 methods

Generic methods for pipeflow objects.

- [`length(`*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/dim.pipeflow.md)
  [`dim(`*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/dim.pipeflow.md)
  : Dimensions of a pipeflow pipeline or view
- [`str(`*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/print.md)
  [`print(`*`<pipeflow>`*`)`](https://github.com/rpahl/pipeflow/reference/print.md)
  : Print pipeflow objects

## Filtering helpers

Filter operators from data.table re-exported for use in view and subset
expressions.

- [`pipeflow-operators`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%like%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%ilike%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%flike%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%plike%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%chin%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%between%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%inrange%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  [`%notin%`](https://github.com/rpahl/pipeflow/reference/pipeflow-operators.md)
  : Row-filter operators re-exported from data.table
