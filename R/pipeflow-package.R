#' @keywords internal
#' @useDynLib pipeflow, .registration = TRUE
#' @import data.table
#' @importFrom Rcpp evalCpp
"_PACKAGE"


.env <- new.env(parent = emptyenv())

.step_states <- new.env(parent = emptyenv())
.step_states[["new"]] <- c(name = "new", color = "#47b8ffff")
.step_states[["done"]] <- c(name = "done", color = "#87ff6fff")
.step_states[["outdated"]] <- c(name = "outdated", color = "#ffa040ff")
.step_states[["failed"]] <- c(name = "failed", color = "#ff4c4cff")

# The step call that runs (see `.pip_call_step()`): the name of the step,
# and for a call mapped over partitions its partition key and all keys
# (`NULL` for a single call). All `NULL` outside of a run.
.step_call <- new.env(parent = emptyenv())
.step_call[["step"]] <- NULL
.step_call[["key"]] <- NULL
.step_call[["keys"]] <- NULL

# Run states of a pipeline (see `pip_run_state()`). A list, not an
# environment, since the order defines the levels of `.run_state`.
.run_states <- list(
    ready = c(name = "ready", color = "#87ff6fff"),
    restart = c(name = "restart", color = "#b48cffff"),
    running = c(name = "running", color = "#47b8ffff"),
    halted = c(name = "halted", color = "#ffa040ff"),
    failed = c(name = "failed", color = "#ff4c4cff"),
    continued = c(name = "continued", color = "#ffd84cff")
)


# These columns are recomputed whenever a function, its parameters,
# or the step name change, and they cannot be assigned to directly
# (see e.g. `[[<-.pipeflow`).
.derived_cols <- c("depends", "unbound", "nodeId")
