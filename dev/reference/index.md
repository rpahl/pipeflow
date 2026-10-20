# Package index

## Getting started

Create a pipeline, add steps, and collect results.

- [`pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-package.md)
  [`pipeflow-package`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-package.md)
  : pipeflow: Fast Interactive Data Analysis Pipelines
- [`pip_new()`](https://rpahl.github.io/pipeflow/dev/reference/pip_new.md)
  : Create a pipeline
- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  : Add a step
- [`pip_collect()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md)
  [`pip_collect_out()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md)
  : Collect step outputs

## Modify steps

Replace, rename, remove, reset, or re-configure steps.

- [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  : Replace a step
- [`pip_rename()`](https://rpahl.github.io/pipeflow/dev/reference/pip_rename.md)
  : Rename a step
- [`pip_remove()`](https://rpahl.github.io/pipeflow/dev/reference/pip_remove.md)
  : Remove a step
- [`pip_reset()`](https://rpahl.github.io/pipeflow/dev/reference/pip_reset.md)
  : Reset a pipeline to its initial state
- [`pip_get_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  : Get or set unbound parameters

## Pipeline composition

Combine or clone pipelines, or turn a pipeline into a function.

- [`rbind(`*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/rbind.pipeflow.md)
  : Bind pipelines
- [`pip_clone()`](https://rpahl.github.io/pipeflow/dev/reference/pip_clone.md)
  : Clone a pipeline
- [`as.function(`*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md)
  : Turn a pipeline into a function
- [`pip_inner()`](https://rpahl.github.io/pipeflow/dev/reference/pip_inner.md)
  : Inner pipelines of a pipeline function step

## Running pipelines

Execute pipeline steps or views.

- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  : Run a pipeline
- [`pip_run_state()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run_state.md)
  : Get the run state of a pipeline
- [`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
  [`` `pip_param_value<-`() ``](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
  : Annotated parameter values
- [`pip_output_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_output_value.md)
  : Annotated step outputs
- [`pipeflow_lgr()`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow_lgr.md)
  : Default logger of pipeflow

## Locking and tagging

Lock steps against changes or tag them for filtering.

- [`pip_lock()`](https://rpahl.github.io/pipeflow/dev/reference/pip_lock.md)
  [`pip_unlock()`](https://rpahl.github.io/pipeflow/dev/reference/pip_lock.md)
  : Lock or unlock steps
- [`pip_tag()`](https://rpahl.github.io/pipeflow/dev/reference/pip_tag.md)
  [`pip_untag()`](https://rpahl.github.io/pipeflow/dev/reference/pip_tag.md)
  : Add or remove tags

## Inspection and views

Inspect the pipeline structure, access the step table, and create
filtered views.

- [`pip_view()`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md)
  : Create a pipeline view
- [`pip_data()`](https://rpahl.github.io/pipeflow/dev/reference/pip_data.md)
  : Access the underlying step table
- [`pip_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md)
  [`pip_get_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md)
  : Build pipeline graph data

## Extract and replace operators

Subset, read from and write to a pipeline using base R operators.

- [`` `[`( ``*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)
  [`` `[[`( ``*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)
  [`` `[[<-`( ``*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)
  [`` `[<-`( ``*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)
  [`` `$`( ``*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)
  [`` `$<-`( ``*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md)
  : Extract or replace parts of a pipeline

## S3 methods

Generic methods for pipeflow objects.

- [`length(`*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/dim.pipeflow.md)
  [`dim(`*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/dim.pipeflow.md)
  : Dimensions of a pipeflow pipeline or view
- [`str(`*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/print.md)
  [`print(`*`<pipeflow>`*`)`](https://rpahl.github.io/pipeflow/dev/reference/print.md)
  : Print pipeflow objects

## Filtering helpers

Filter operators from data.table re-exported for use in view and subset
expressions.

- [`pipeflow-operators`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%like%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%ilike%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%flike%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%plike%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%chin%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%between%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%inrange%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  [`%notin%`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow-operators.md)
  : Row-filter operators re-exported from data.table
