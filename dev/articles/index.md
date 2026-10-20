# Articles

### All vignettes

- [Get started with
  pipeflow](https://rpahl.github.io/pipeflow/dev/articles/v01-get-started.md):

  Start here if this is your first time using pipeflow.

- [Modify existing
  pipelines](https://rpahl.github.io/pipeflow/dev/articles/v02-modify-pipeline.md):

  How to insert, replace, and remove steps in a pipeline.

- [Combine
  pipelines](https://rpahl.github.io/pipeflow/dev/articles/v03a-combine-pipelines.md):

  How to combine different pipelines to a single pipeline.

- [Pipeline
  views](https://rpahl.github.io/pipeflow/dev/articles/v03b-pipeline-views.md):

  How to filter pipelines with
  [`pip_view()`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md)
  and the `[` operator, compose views, and run only a subset of steps.

- [Collect and group
  output](https://rpahl.github.io/pipeflow/dev/articles/v04-collect-output.md):

  How to collect and group pipeline output.

- [Split, map, and
  reduce](https://rpahl.github.io/pipeflow/dev/articles/v05a-split-map-reduce.md):

  Shows how to split data, apply the pipeline to each subset, and then
  reduce the results back into a combined output.

- [Nested
  pipelines](https://rpahl.github.io/pipeflow/dev/articles/v05b-nested-pipeline.md):

  Shows how to run a pipeline as a step of another pipeline with
  as.function(), so that changing its parameters re-executes the outer
  step and only the inner steps that are affected.

- [Handling
  failures](https://rpahl.github.io/pipeflow/dev/articles/v05c-error-handling.md):

  Shows what happens when a step fails, how to keep a pipeline running
  with on_error = “continue”, how to react to failed inputs with ~try()
  references, and how to recover once the cause is fixed.

- [Annotated parameter
  values](https://rpahl.github.io/pipeflow/dev/articles/v05d-param-value.md):

  Shows how to store annotated values (e.g. values with units or labels)
  as step parameters and how to unwrap them right before a step function
  is called, using the pip_param_value() generic.

- [Annotated step
  outputs](https://rpahl.github.io/pipeflow/dev/articles/v05e-output-value.md):

  Shows how to annotate the output of a step when the step returns, for
  example to stamp a result with the step it came from and the time it
  was computed, using the pip_output_value() generic.

- [Self-modifying
  pipelines](https://rpahl.github.io/pipeflow/dev/articles/v06-self-modify-pipeline.md):

  Shows how you can setup pipelines to modify themselves at runtime,
  which, for example, allows for changing pipeline parameters based on
  intermediate results or even dynamically modify the pipeline’s own
  structure during a pipeline run.

- [pipeflow vs
  targets](https://rpahl.github.io/pipeflow/dev/articles/v07-vs-targets.md):

  Comparing general properties and performance of {pipeflow} vs
  {targets} in a scoped latency comparison.
