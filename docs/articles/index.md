# Articles

### All vignettes

- [Get started with
  pipeflow](https://github.com/rpahl/pipeflow/articles/v01-get-started.md):

  Start here if this is your first time using pipeflow.

- [Modify existing
  pipelines](https://github.com/rpahl/pipeflow/articles/v02-modify-pipeline.md):

  How to insert, replace, and remove steps in a pipeline.

- [Combine
  pipelines](https://github.com/rpahl/pipeflow/articles/v03a-combine-pipelines.md):

  How to combine different pipelines to a single pipeline.

- [Pipeline
  views](https://github.com/rpahl/pipeflow/articles/v03b-pipeline-views.md):

  How to filter pipelines with
  [`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
  and the `[` operator, compose views, and run only a subset of steps.

- [Collect and group
  output](https://github.com/rpahl/pipeflow/articles/v04-collect-output.md):

  How to collect and group pipeline output.

- [Split, map, and
  reduce](https://github.com/rpahl/pipeflow/articles/v05a-split-map-reduce.md):

  Shows how to split data, apply the pipeline to each subset, and then
  reduce the results back into a combined output.

- [Nested
  pipelines](https://github.com/rpahl/pipeflow/articles/v05b-nested-pipeline.md):

  Shows how to embed pipelines within pipeline steps and how to mark
  their parameters so that changing them re-executes the outer step.

- [Self-modifying
  pipelines](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md):

  Shows how you can setup pipelines to modify themselves at runtime,
  which, for example, allows for changing pipeline parameters based on
  intermediate results or even dynamically modify the pipeline’s own
  structure during a pipeline run.

- [pipeflow vs
  targets](https://github.com/rpahl/pipeflow/articles/v07-vs-targets.md):

  A detailed comparison and benchmark between pipeflow and targets.
