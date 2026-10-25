# Checks for tests that run long chains of edits (test_edit_chains.R): the
# invariants of the pipeline (dev/pipeflow.md, section 3), the bookkeeping of
# failed keys (section 8, "Errors and failures") and the freshness of the
# outputs. Each check collects its violations as messages, so that a failed
# expectation lists all of them.

# The violations of the invariants of the pipeline that `x` is a handle of.
pip_invariant_violations <- function(x) {
    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    steps <- dat[["step"]]
    ids <- dat[["nodeId"]]
    d <- .pip_dag(env)
    violations <- character()
    report <- function(...) violations <<- c(violations, paste0(...))

    if (anyDuplicated(steps) > 0L || anyDuplicated(ids) > 0L) {
        report("step names and node ids must be unique")
    }

    # 2. The nodeId column, the step to node map and the DAG agree
    stepsToNodes <- as.list(env[[".steps_to_nodes"]], all.names = TRUE)
    if (
        !setequal(names(stepsToNodes), steps) ||
            !identical(as.integer(unlist(stepsToNodes[steps])), ids)
    ) {
        report("the step to node map does not match the step table")
    }
    if (!identical(as.integer(dag_get_nodes_order(d)), ids)) {
        report("the node order of the DAG does not match the step table")
    }

    for (k in seq_along(steps)) {
        depends <- dat[["depends"]][[k]]
        params <- dat[["params"]][[k]]
        check <- function(ok, msg) {
            if (!ok) report("step '", steps[[k]], "': ", msg)
        }

        # 1. The row order is a topological order
        check(
            all(depends %in% steps[seq_len(k - 1L)]),
            "depends on a step that is not in an earlier row"
        )

        # 2. The edges of the DAG are the dependencies
        isDependent <- vapply(dat[["depends"]], \(x) steps[[k]] %in% x, NA)
        check(
            setequal(dag_get_incoming(d, ids[[k]]), ids[match(depends, steps)]),
            "its incoming DAG edges do not match its dependencies"
        )
        check(
            setequal(dag_get_outgoing(d, ids[[k]]), ids[isDependent]),
            "its outgoing DAG edges do not match the dependent steps"
        )

        # 4. The derived columns
        args <- setdiff(names(formals(dat[["fun"]][[k]])), "...")
        check(
            all(c(args, names(depends)) %in% names(params)),
            "an argument or a dependency has no param"
        )
        check(
            identical(
                dat[["unbound"]][[k]],
                setdiff(names(params), names(depends))
            ),
            "its unbound params are not its params without dependency"
        )

        # 5. The reference formulas mirror the dependencies. A relative
        # reference (e.g. ~-1) was resolved when the step was added.
        refs <- formula_deps(params)
        isName <- !grepl("^-?[0-9]+$", refs)
        check(
            setequal(names(refs), names(depends)) &&
                identical(
                    unname(refs[isName]),
                    unname(depends[names(refs)[isName]])
                ),
            "its reference formulas do not match its dependencies"
        )
    }

    # 7. No step call is left over
    if (!all(vapply(.pip_step_call(), is.null, NA))) {
        report("the step call was not restored")
    }

    c(violations, failure_violations(x))
}


# The violations of the bookkeeping of failed keys:
# - The failure of a failed key of a failed step names its key and steps of
#   the pipeline: the step itself, or an upstream step that failed for the
#   key (also after renames, see .pip_rename_failures()). Its message matches
#   these names.
# - A step marked in `.steps_with_current_out` is not done, and if it
#   failed, it failed for some keys.
failure_violations <- function(x) {
    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    steps <- dat[["step"]]
    ids <- dat[["nodeId"]]
    d <- .pip_dag(env)
    marks <- env[[".steps_with_current_out"]]
    violations <- character()
    report <- function(k, ...) {
        violations <<- c(violations, paste0("step '", steps[[k]], "': ", ...))
    }

    for (k in seq_along(steps)) {
        state <- dat[["state"]][[k]]
        failedKeys <- .pip_failed_keys(dat[["out"]][[k]])

        if (state == "failed") {
            upSteps <- steps[ids %in% dag_get_reachable_nodes_up(d, ids[[k]])]
            for (key in failedKeys) {
                f <- dat[["out"]][[k]][[key]]
                rebuilt <- .pip_failure(
                    f[["step"]],
                    parent = f[["parent"]],
                    failedStep = f[["failed_step"]],
                    key = key
                )
                isInLine <- identical(f[["key"]], key) &&
                    all(c(f[["step"]], f[["failed_step"]]) %in% upSteps) &&
                    identical(conditionMessage(f), conditionMessage(rebuilt))
                if (!isInLine) {
                    report(k, "the failure of key '", key, "' is out of date")
                }
            }
        }

        # A mark is only kept by a step that failed for some keys, or that
        # was outdated or reset since
        mayBeMarked <- state != "done" &&
            (state != "failed" || length(failedKeys) > 0L)
        if (isTRUE(marks[[steps[[k]]]]) && !mayBeMarked) {
            report(k, "is marked to hold the output of its last attempt")
        }
    }
    violations
}


# The violations of freshness: a step that is done must hold the output of a
# run from scratch, and a step that failed must fail in the same way (for
# the same keys, with the same outputs for the other keys). The run from
# scratch is a continued run of a reset clone of the pipeline, in which the
# locked steps keep their outputs. So do the `pinned` steps, whose outputs
# were set by hand, if they are done. Locked and pinned steps are not
# compared.
freshness_violations <- function(x, pinned = character()) {
    dat <- .pip_pipenv(x)[["data"]]
    fresh <- pip_clone(.wrap_pipenv(.pip_pipenv(x), name = x[["name"]]))
    isDone <- dat[["state"]] == "done"
    pip_lock(pip_view(fresh, step = intersect(pinned, dat[["step"]][isDone])))
    suppressMessages(pip_reset(fresh))
    suppressWarnings(pip_run(fresh, lgr = NULL, on_error = "continue"))

    freshDat <- .pip_pipenv(fresh)[["data"]]
    isRun <- dat[["state"]] %in% c("done", "failed")
    isKept <- dat[["locked"]] | dat[["step"]] %in% pinned
    rows <- which(isRun & !isKept)
    isFresh <- vapply(
        rows,
        FUN = function(k) {
            identical(dat[["state"]][[k]], freshDat[["state"]][[k]]) &&
                isTRUE(all.equal(
                    comparable_out(dat[["out"]][[k]]),
                    comparable_out(freshDat[["out"]][[k]])
                ))
        },
        FUN.VALUE = NA
    )
    sprintf(
        "step '%s' (%s): its output is not the one of a run from scratch",
        dat[["step"]][rows[!isFresh]],
        dat[["state"]][rows[!isFresh]]
    )
}


# A step output in which conditions (whole-step failures, failed keys) are
# replaced by their messages, as they differ in their calls.
comparable_out <- function(out) {
    asText <- function(value) {
        if (inherits(value, "condition")) {
            return(paste("<condition>", conditionMessage(value)))
        }
        value
    }
    if (.is_pipeflow_partitioned(out)) {
        out[] <- lapply(out, asText)
        return(out)
    }
    asText(out)
}


# Follow a pipeline through a chain of edits. The returned function checks
# the pipeline after each edit: its invariants, the freshness of its outputs
# (unless `fresh = FALSE`, see freshness_violations()), and that its steps
# keep their node ids and that the ids of removed steps are not used again
# (invariant 3). It can follow a copy of the pipeline instead (pip_clone(),
# readRDS(), rbind() with the pipeline as first argument), which keeps the
# node ids. `info` is added to the message of a failed check.
edit_chain <- function() {
    ids <- stats::setNames(integer(), character())
    retired <- integer()
    function(x, pinned = character(), fresh = TRUE, info = NULL) {
        violations <- pip_invariant_violations(x)
        if (fresh) {
            violations <- c(violations, freshness_violations(x, pinned))
        }

        dat <- .pip_pipenv(x)[["data"]]
        now <- stats::setNames(dat[["nodeId"]], dat[["step"]])
        kept <- intersect(names(now), names(ids))
        if (!identical(now[kept], ids[kept])) {
            violations <- c(violations, "a step changed its node id")
        }
        retired <<- union(retired, setdiff(ids, now))
        if (any(now %in% retired)) {
            violations <- c(violations, "the node id of a removed step is used")
        }
        ids <<- now

        expect(
            length(violations) == 0L,
            paste(c(violations, info), collapse = "\n")
        )
        invisible(x)
    }
}


# Run `x` with on_error = "continue" and return the log messages, e.g. to see
# which keys were run again. Muffles the warning about failed steps.
run_log <- function(x, ...) {
    msgs <- character()
    lgr <- function(level, msg, ...) msgs <<- c(msgs, msg)
    withCallingHandlers(
        pip_run(x, lgr = lgr, on_error = "continue", ...),
        pipeflow_run_failed = function(w) invokeRestart("muffleWarning")
    )
    msgs
}
