expect_equivalent <- function(...) {
    testthat::expect_equal(..., ignore_attr = TRUE)
}

expect_error_fixed <- function(...) {
    testthat::expect_error(..., fixed = TRUE)
}

expect_no_error <- function(...) {
    testthat::expect_error(..., regexp = NA)
}

expect_no_warning <- function(...) {
    testthat::expect_warning(..., regexp = NA)
}

get_run_state <- function(x) {
    as.character(.pip_pipenv(x)[[".run_state"]])
}

# The parts of a pipeline that editing its steps changes: the step table,
# the DAG and the two step maps. The step table is copied, because edits
# change it in place.
pip_internals <- function(x) {
    env <- .pip_pipenv(x)
    d <- .pip_dag(env)
    nodes <- dag_get_nodes_order(d)
    sortedList <- \(e) mget(ls(e, all.names = TRUE, sorted = TRUE), envir = e)
    list(
        data = as.list(data.table::copy(env[["data"]])),
        nodes = nodes,
        incoming = lapply(nodes, \(id) dag_get_incoming(d, id)),
        stepsToNodes = sortedList(env[[".steps_to_nodes"]]),
        currentOut = sortedList(env[[".steps_with_current_out"]])
    )
}

# Write `x` with saveRDS() and read it back with readRDS(). Objects that are
# saved together share their environments after reading, like a pipeline and
# its views.
save_and_read <- function(x) {
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f))
    saveRDS(x, f)
    readRDS(f)
}

# The rows of the step table for the DAG node ids `nodes`, in the order of
# `nodes`, not in pipeline order. A view does not restrict the rows. An
# unknown id gives a row of NAs with that node id.
.pip_filter_nodes <- function(x, nodes) {
    .pip_pipenv(x)[["data"]][list(nodes), on = "nodeId"]
}

# The kept pipelines of a function made by as.function(), named by
# partition key.
.fun_pips <- function(fun) {
    cache <- .fun_cache(fun)
    pips <- lapply(unname(cache$slots), .fun_slot_pips)
    as.list(unlist(pips, recursive = FALSE))
}
