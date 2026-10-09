# ---------------------
# Pipeline construction
# ---------------------
describe(".empty_pipeline", {
    it("returns an empty data.table", {
        dt <- .empty_pipeline()
        expect_true(data.table::is.data.table(dt))
        expect_equal(nrow(dt), 0)
        expect_equal(
            names(dt),
            c(
                "step",
                "fun",
                "params",
                "out",
                "state",
                "tags",
                "locked",
                "exec",
                "time",
                "depends",
                "unbound",
                "nodeId"
            )
        )
    })
})


describe(".new_step", {
    step <- .new_step(
        step = "step2",
        fun = function(x) x^2,
        params = list(x = 1, y = ~step1),
        depends = c(y = "step1"),
        tags = c("t1", "t2"),
        nodeId = 0
    )

    it("contains the expected elements", {
        expect_equal(step$step, "step2")
        expect_equal(step$fun[[1]](2), 4)
        expect_equivalent(step$params[[1]], list(x = 1, y = ~step1))
        expect_equal(step$depends, list(c(y = "step1")))
        expect_equal(step$out, list(NULL))
        expect_equal(step$tags, list(c("t1", "t2")))
        expect_equal(step$state, "new")
        expect_true(inherits(step$time, "POSIXct"))
        expect_equal(step$locked, FALSE)
        expect_equal(step$nodeId, 0)
        expect_equal(step$unbound, list("x"))
    })

    it("aligns with the empty pipeline", {
        expect_equal(names(step), names(.empty_pipeline()))
    })

    it("can be appended to the empty pipeline", {
        dt <- data.table::rbindlist(list(.empty_pipeline(), step))
        expect_equal(nrow(dt), 1)
    })
})

# ------------------------------
# Parameter & dependency parsing
# ------------------------------

describe(".extract_fun_params", {
    it("returns TRUE if function has no args", {
        expect_equal(
            .extract_fun_params(function() 1),
            list()
        )
    })

    it("returns default values for function arguments", {
        expect_equal(
            .extract_fun_params(function(x = 1) x),
            list(x = 1)
        )
        expect_equal(
            .extract_fun_params(function(x = 1, y = 2) x + y),
            list(x = 1, y = 2)
        )
    })

    it("signals parameters with no default values", {
        expect_error(
            .extract_fun_params(function(x, y = 1) x + y),
            "'x' has no default value",
            fixed = TRUE
        )

        expect_error(
            .extract_fun_params(function(x, y) x + y),
            "'x', 'y' have no default value",
            fixed = TRUE
        )
    })

    it("supports ...", {
        expect_equal(
            .extract_fun_params(function(x = 1, ...) x),
            list(x = 1)
        )
    })

    it("supports formula", {
        expect_equal(
            .extract_fun_params(function(x = 1, y = ~foo) x),
            list(x = 1, y = ~foo)
        )
    })

    it("works with default values declared outside the function", {
        default_value <- 1
        expect_equal(
            .extract_fun_params(function(x = default_value) x),
            list(x = 1)
        )
    })
})


describe(".extract_depends", {
    steps <- c("s1", "s2", "s3")

    it("returns an empty character vector if no params are defined", {
        expect_equal(
            .extract_depends(list(), steps),
            character(0)
        )
    })

    it("returns an empty character vector if no dependencies are defined", {
        expect_equal(
            .extract_depends(list(a = 1), steps),
            character(0)
        )
    })

    it("extracts all dependencies defined via step name", {
        expect_equal(
            .extract_depends(list(a = ~s1, b = 2), steps),
            c(a = "s1")
        )
        expect_equal(
            .extract_depends(list(a = ~s1, b = ~s2), steps),
            c(a = "s1", b = "s2")
        )
    })

    it("extracts all dependencies defined relative", {
        expect_equal(
            .extract_depends(list(x = ~ -1), steps = c("f1", "f2")),
            c(x = "f1")
        )
        expect_equal(
            .extract_depends(list(a = ~ -1, b = 2), steps),
            c(a = "s2")
        )
        expect_equal(
            .extract_depends(list(a = ~ -2, b = ~ -1), steps),
            c(a = "s1", b = "s2")
        )
    })

    it("signals bad steps input", {
        expect_error(
            .extract_depends(list(a = ~ -1), character(0)),
            "toPos (0) must be at least 1",
            fixed = TRUE
        )
    })
    it("signals relative index out of bound", {
        expect_error(
            .extract_depends(list(a = ~ -1), "s1"),
            "relative index a = ~-1 points outside pipeline",
            fixed = TRUE
        )
        expect_error(
            .extract_depends(list(a = ~ -4), steps),
            "relative index a = ~-4 points outside pipeline"
        )
    })

    it("extracts all dependencies if defined both ways", {
        expect_equal(
            .extract_depends(list(a = ~ -1, b = ~s1, c = 3), steps),
            c(a = "s2", b = "s1")
        )
    })

    it("signals toPos exceeding number of steps", {
        expect_error(
            .extract_depends(list(), steps, 4L),
            "toPos (4) exceeds number of steps (3)",
            fixed = TRUE
        )
    })

    it("signals bad arg types", {
        f <- .extract_depends
        expect_error(
            f("not a list", steps),
            "params must be a list"
        )
        expect_error(
            f(list(), steps = list("not a character")),
            "steps must be a character vector"
        )
    })
})

# -------------
# State updates
# -------------

describe(".pip_restart", {
    counter_env <- function(...) {
        env <- new.env(parent = emptyenv())
        vals <- list(...)
        for (nm in names(vals)) {
            env[[nm]] <- vals[[nm]]
        }
        env
    }

    it("signals invalid inputs", {
        p <- pip_new()

        expect_error(
            p$restart(force = "yes"),
            "force must be a single logical value"
        )
        expect_error(
            p$restart(force = c(TRUE, FALSE)),
            "force must be a single logical value"
        )
        expect_error(
            p$restart(times = 0),
            "times must be a single integer value >= 1"
        )
        expect_error(
            p$restart(times = NA),
            "times must be a single integer value >= 1"
        )
        expect_error(
            p$restart(times = "a"),
            "times must be a single integer value >= 1"
        )
        expect_error(
            p$restart(times = c(1, 2)),
            "times must be a single integer value >= 1"
        )
    })

    it("restarts the pipeline when a step requests a restart", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    .self$restart()
                }
                c[["n"]]
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 2L)
        expect_equal(get_run_state(p), "ready")
    })

    it("stops recursive restarts after the announced times", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                .self$restart(times = 2L)
                c[["n"]]
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 3L)
        expect_equal(p[["pipenv"]][[".restart_count"]], 0L)
    })

    it("re-runs all steps on restart when force = TRUE", {
        c <- counter_env(a = 0L, b = 0L, cc = 0L)
        p <- pip_new() |>
            pip_add("a", function(x = 1) {
                c[["a"]] <- c[["a"]] + 1L
                x
            }) |>
            pip_add("b", function(x = ~a) {
                c[["b"]] <- c[["b"]] + 1L
                if (c[["b"]] == 1L) {
                    .self$restart(force = TRUE)
                }
                x + 1
            }) |>
            pip_add("cc", function(x = ~b) {
                c[["cc"]] <- c[["cc"]] + 1L
                x + 1
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["a"]], 2L)
        expect_equal(c[["b"]], 2L)
        expect_equal(c[["cc"]], 1L)
        expect_equal(unname(p[["out"]]), list(1, 2, 3))
    })

    it("skips already done steps on restart when force = FALSE", {
        c <- counter_env(a = 0L, b = 0L, cc = 0L)
        p <- pip_new() |>
            pip_add("a", function(x = 1) {
                c[["a"]] <- c[["a"]] + 1L
                x
            }) |>
            pip_add("b", function(x = ~a) {
                c[["b"]] <- c[["b"]] + 1L
                if (c[["b"]] == 1L) {
                    .self$restart(force = FALSE)
                }
                x + 1
            }) |>
            pip_add("cc", function(x = ~b) {
                c[["cc"]] <- c[["cc"]] + 1L
                x + 1
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["a"]], 1L)
        expect_equal(c[["b"]], 1L)
        expect_equal(c[["cc"]], 1L)
        expect_equal(unname(p[["out"]]), list(1, 2, 3))
    })

    it("marks the pipeline as restarting when called before a run", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)

        p$restart()
        expect_equal(get_run_state(p), "restart")
        expect_equal(p[["pipenv"]][[".restart_count"]], 1L)

        logs <- character(0)
        lgr <- function(level, msg) logs <<- c(logs, msg)
        pip_run(p, lgr = lgr)

        expect_true(any(grepl("Restarting run", logs)))
        expect_equal(get_run_state(p), "ready")
    })

    it("restarts the underlying pipeline when called on a view", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)
        v <- pip_view(p, step = "s1")

        v$restart()

        expect_equal(get_run_state(p), "restart")
        expect_equal(p[["pipenv"]][[".restart_count"]], 1L)
        expect_identical(v[["pipenv"]][["data"]], p[["pipenv"]][["data"]])
    })

    it("restarts a view run when a step requests a restart", {
        c <- counter_env(n = 0L)
        p <- pip_new("view-pipeline") |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    .self$restart()
                }
                x
            }) |>
            pip_add("s2", function(x = ~s1) x + 1)
        v <- pip_view(p, step = "s2")

        pip_run(v, lgr = NULL)

        expect_equal(c[["n"]], 2L)
        expect_equal(unname(p[["out"]]), list(1, 2))
        expect_equal(get_run_state(p), "ready")
    })

    it("restarts without declaring .self in the step signature", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    .self$restart()
                }
                x
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 2L)
        expect_equal(get_run_state(p), "ready")
    })

    it("re-runs done steps downstream of steps executed before restart", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    .self$restart(force = FALSE)
                }
                x
            }) |>
            pip_add("s2", function(x = ~s1) x + 1)
        p[["s2", "state"]] <- "done"

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 1L)
        expect_equal(unname(p[["out"]]), list(1, 2))
        expect_equal(unname(p[["state"]]), c("done", "done"))
    })
})

describe(".pip_halt", {
    it("marks the pipeline as halted when called before a run", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)

        p$halt()

        expect_equal(get_run_state(p), "halted")
        expect_equal(get_run_state(p), "halted")
    })

    it("aborts the run at the halting step and marks downstream outdated", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)
        p[["s3", "state"]] <- "done"

        pip_run(p, lgr = NULL)

        expect_equal(unname(p[["out"]]), list(1, 2, NULL))
        expect_equal(
            unname(p[["state"]]),
            c("done", "done", "outdated")
        )
        expect_equal(get_run_state(p), "halted")
    })

    it("continues after the halt in the next run", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        pip_run(p, lgr = NULL)
        expect_equal(get_run_state(p), "halted")

        pip_run(p, lgr = NULL)
        expect_equal(unname(p[["out"]]), list(1, 2, 3))
        expect_equal(unname(p[["state"]]), rep("done", 3))
        expect_equal(get_run_state(p), "ready")
    })

    it("keeps the halted run state if steps failed before the halt", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) stop("boom")) |>
            pip_add("s2", function(x = 1) {
                .self$halt()
                x
            }) |>
            pip_add("s3", function(x = 1) x)

        expect_warning(
            pip_run(p, lgr = NULL, on_error = "continue"),
            class = "pipeflow_run_failed"
        )
        expect_equal(get_run_state(p), "halted")
        expect_equal(unname(p[["state"]]), c("failed", "done", "new"))
    })

    it("leaves new steps not reached due to the halt as new", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        pip_run(p, lgr = NULL)

        expect_equal(unname(p[["state"]]), c("done", "done", "new"))
    })

    it("logs the manual halt message during the run", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        logs <- character(0)
        lgr <- function(level, msg) logs <<- c(logs, msg)
        pip_run(p, lgr = lgr)

        expect_true(any(
            grepl("Aborting pipeline execution on manual halt", logs)
        ))
        expect_true(any(grepl("Step 1/3 s1", logs)))
        expect_true(any(grepl("Step 2/3 s2", logs)))
        expect_false(any(grepl("Step 3/3 s3", logs)))
    })

    it("does not execute steps after the halting step", {
        ran <- character(0)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                ran <<- c(ran, "s1")
                x
            }) |>
            pip_add("s2", function(x = ~s1) {
                ran <<- c(ran, "s2")
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) {
                ran <<- c(ran, "s3")
                x + 1
            })

        pip_run(p, lgr = NULL)

        expect_equal(ran, c("s1", "s2"))
    })

    it("halts at the first step and marks all later steps outdated", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                .self$halt()
                x
            }) |>
            pip_add("s2", function(x = ~s1) x + 1) |>
            pip_add("s3", function(x = ~s2) x + 1)
        p[c("s2", "s3"), "state"] <- "done"

        pip_run(p, lgr = NULL)

        expect_equal(unname(p[["out"]]), list(1, NULL, NULL))
        expect_equal(
            unname(p[["state"]]),
            c("done", "outdated", "outdated")
        )
    })

    it("halts without declaring .self in the step signature", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        pip_run(p, lgr = NULL)

        expect_equal(unname(p[["out"]]), list(1, 2, NULL))
        expect_equal(
            unname(p[["state"]]),
            c("done", "done", "new")
        )
    })

    it("marks the underlying pipeline as halted when called on a view", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)
        v <- pip_view(p, step = "s1")

        v$halt()

        expect_equal(get_run_state(p), "halted")
        expect_identical(v[["pipenv"]][["data"]], p[["pipenv"]][["data"]])
    })

    it("aborts a view run at the halting step", {
        p <- pip_new("view-pipeline") |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                .self$halt()
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1) |>
            pip_add("s4", function(x = ~s3) x + 1)
        p[c("s3", "s4"), "state"] <- "done"
        v <- pip_view(p, step = "s4")

        pip_run(v, lgr = NULL)

        expect_equal(unname(p[["out"]]), list(1, 2, NULL, NULL))
        expect_equal(
            unname(p[["state"]]),
            c("done", "done", "outdated", "outdated")
        )
        expect_equal(get_run_state(p), "halted")
    })
})

describe(".pip_outdate_downstream", {
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x) |>
            pip_run(lgr = NULL)
    }

    it("outdates states downstream of single node as expected", {
        p <- test_pip()

        expect_true(all(p[["state"]] == "done"))
        .pip_outdate_downstream(p, "a1")

        expect_equal(
            unname(p[["state"]]),
            c("outdated", "outdated", "done", "outdated", "done")
        )
    })

    it("can outdate states downstream of multiple nodes", {
        p <- test_pip()
        .pip_outdate_downstream(p, steps = c("a1", "b1"))
        expect_true(all(p[["state"]] == "outdated"))

        p <- test_pip()
        .pip_outdate_downstream(p, steps = c("a1", "b2"))

        expect_equal(
            unname(p[["state"]]),
            c("outdated", "outdated", "done", "outdated", "outdated")
        )
    })

    it("does not outdate the steps listed in 'keep'", {
        p <- test_pip()
        .pip_outdate_downstream(p, steps = "a1", keep = c("a1", "a3"))

        expect_equal(
            unname(p[["state"]]),
            c("done", "outdated", "done", "done", "done")
        )
    })

    it("leaves new steps as new", {
        p <- test_pip()
        pip_reset(p[c("a1", "a3")])
        .pip_outdate_downstream(p, steps = "a1")

        expect_equal(
            unname(p[["state"]]),
            c("new", "outdated", "done", "new", "done")
        )
    })

    it("ignores unknown steps and handles empty input", {
        p <- test_pip()
        .pip_outdate_downstream(p, steps = c("nope", character()))
        .pip_outdate_downstream(p, steps = character())

        expect_true(all(p[["state"]] == "done"))
    })
})


# ---------------------------
# Step lookup & DAG traversal
# ---------------------------

describe(".pip_steps_to_rows", {
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x) |>
            pip_add("s3", \(x = ~s2) x)
    }

    it("maps step names to row positions for pipelines and views", {
        p <- test_pip()
        expect_equal(.pip_steps_to_rows(p, c("s3", "s1")), c(3L, 1L))

        v <- pip_view(p, step = c("s2", "s3"))
        expect_equal(.pip_steps_to_rows(v, c("s1", "s3")), c(1L, 3L))
    })

    it("signals invalid step selectors with default messages", {
        p <- test_pip()

        expect_error(
            .pip_steps_to_rows(p, c("s1", "")),
            "step names must be non-empty strings"
        )
        expect_error(
            .pip_steps_to_rows(p, c("s1", NA_character_)),
            "step names must not contain NA"
        )
        expect_error(
            .pip_steps_to_rows(p, c("s1", "unknown")),
            "Unknown step names: unknown"
        )
    })

    it("uses simple error messages consistently", {
        p <- test_pip()

        expect_error(
            .pip_steps_to_rows(p, c("s1", "")),
            "step names must be non-empty strings"
        )
        expect_error(
            .pip_steps_to_rows(p, c("s1", "unknown")),
            "Unknown step names: unknown"
        )
    })
})


# -----------------
# Pipeline addition
# -----------------

# ---------------------------
# Exported pipeline functions
# ---------------------------

describe("pip_new", {
    it("creates a pipeflow pipeline with expected base structure", {
        p <- pip_new()

        expect_true(.is_pipeflow(p))
        expect_true(is.list(p))
        expect_equal(p[["name"]], "pipe")
        expect_null(p[["view"]])
        expect_true(is.environment(p[["pipenv"]]))
        expect_true(data.table::is.data.table(p[["pipenv"]][["data"]]))
        expect_equal(nrow(p[["pipenv"]][["data"]]), 0L)
        expect_true(is.environment(p[["pipenv"]][[".steps_to_nodes"]]))
        expect_equal(
            ls(envir = p[["pipenv"]][[".steps_to_nodes"]]),
            character(0)
        )
        expect_equal(length(dag_get_nodes_order(p[["pipenv"]][[".dag"]])), 0L)
    })

    it("supports custom pipeline names", {
        p <- pip_new("custom")
        expect_equal(p[["name"]], "custom")
    })

    it("signals invalid names", {
        expect_error(pip_new(c("a", "b")), "name must be a single string")
        expect_error(pip_new(1), "name must be a single string")
        expect_error(pip_new(NA_character_), "name must not be NA")
    })
})


describe("pip_add params overlapping the formals of fun", {
    it("warns if params overlap the defaults of fun", {
        p <- pip_new()
        expect_warning(
            pip_add(p, "s", \(x = 1, y = 2) x + y, params = list(x = 5, y = 6)),
            "step 's': the defaults of fun take precedence over params: x, y",
            fixed = TRUE
        )
        expect_equal(p[["params"]][["s"]][["x"]], 1)
        expect_equal(p[["params"]][["s"]][["y"]], 2)
    })

    it("names only the overlapping params", {
        p <- pip_new()
        expect_warning(
            pip_add(p, "s", \(x = 1) x, params = list(x = 5, z = 6)),
            "over params: x$"
        )
    })

    it("warns if the step is inserted via after", {
        p <- pip_new() |> pip_add("a", \(x = 1) x)
        expect_warning(
            pip_add(p, "b", \(x = 1) x, params = list(x = 5), after = 0),
            "step 'b': the defaults of fun take precedence over params: x",
            fixed = TRUE
        )
    })

    it("does not warn for params that are not formals of fun", {
        p <- pip_new()
        expect_no_warning(
            pip_add(p, "s", \(x = 1) x, params = list(z = 5))
        )
    })

    it("does not warn for params that only match '...'", {
        p <- pip_new()
        expect_no_warning(
            pip_add(p, "s", \(x = 1, ...) x, params = list(... = 5))
        )
    })

    it("does not warn without params", {
        p <- pip_new()
        expect_no_warning(pip_add(p, "s", \(x = 1) x))
    })
})


describe("pip_add", {
    it("signals if step is not a single non-empty string", {
        p <- pip_new()
        expect_error(pip_add(p, ""), "step must be a non-empty string")
        expect_error(pip_add(p, c("a", "b")), "step must be a single string")
    })

    it("signals if fun is not a function", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", fun = "not a function"),
            "fun must be a function"
        )
    })

    it("signals if fun is a primitive function", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", fun = sum),
            "fun must not be a primitive function; wrap it"
        )
        expect_equal(length(p), 0L)
    })

    it("signals duplicate step names", {
        p <- pip_new()
        pip_add(p, "s1", \(a = 0) a)
        expect_error(pip_add(p, "s1", \(a = 0) a), "step 's1' already exists")
    })

    it("signals undefined dependencies", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", \(x = ~undefined) x),
            "cannot reference unknown steps: 'undefined'"
        )
    })

    it("step can refer to previous step by relative number", {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)
        pip_add(p, "s2", \(x = ~ -1) 2 * x)

        expect_equal(p[["pipenv"]][["data"]]$depends[[2]], c(x = "s1"))
    })

    it("a bad relative step referal is signalled", {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)

        expect_error(pip_add(p, "s2", \(x = ~ -2) 2 * x))
        expect_equal(length(p), 1L)
    })

    it("if add is aborted, pipeline remains unchanged", {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)
        expect_error(
            pip_add(p, "s2", \(x = ~ -2) 2 * x),
            "relative index x = ~-2 points outside pipeline",
            fixed = TRUE
        )
    })

    it("can refer to the pipeline itself via the .self argument", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) .self)

        pip_run(p, lgr = NULL)

        expect_true(identical(p[["out"]][[1]], p))
    })

    it("allows functions with wildcard arguments", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1, ...) x)

        pip_set_params(p, list(x = 2))
        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[1]], 2)
    })

    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = ~f1) x + 1)
    }

    it("can insert after a step name", {
        p <- test_pip()
        pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")

        expect_equal(unname(p[["step"]]), c("f1", "f3", "f2"))
        expect_equal(p[["depends"]][[2]], c(x = "f1"))
        expect_equal(p[["depends"]][[3]], c(x = "f1"))
    })

    it("can insert by numeric index and supports insertion at beginning", {
        p <- test_pip()
        pip_add(p, "f0", \(x = 0) x, after = 0)

        expect_equal(unname(p[["step"]]), c("f0", "f1", "f2"))
        expect_equal(p[["depends"]][[2]], character(0))
        expect_equal(p[["depends"]][[3]], c(x = "f1"))
    })

    it("uses default insertion position at end", {
        p <- test_pip()
        pip_add(p, "f3", \(x = ~f2) x + 1)

        expect_equal(unname(p[["step"]]), c("f1", "f2", "f3"))
        expect_equal(p[["depends"]][[3]], c(x = "f2"))
    })

    it("returns invisibly also for default append path", {
        p <- test_pip()
        expect_invisible(
            pip_add(p, "f3", \(x = ~f2) x + 1)
        )
    })

    it("can append a step after pipeline was run", {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        expect_invisible(
            pip_add(p, "f3", \(x = ~f2) x + 1)
        )

        expect_equal(unname(p[["step"]]), c("f1", "f2", "f3"))
        expect_equal(p[["depends"]][[3]], c(x = "f2"))
    })

    it("can insert a step after pipeline was run", {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        expect_invisible(
            pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")
        )

        expect_equal(unname(p[["step"]]), c("f1", "f3", "f2"))
        expect_equal(p[["depends"]][[2]], c(x = "f1"))
        expect_equal(p[["depends"]][[3]], c(x = "f1"))
    })

    it("keeps existing step state and output for appended tail steps", {
        p <- test_pip()
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "out",
            value = list(42)
        )
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "state",
            value = "done"
        )

        pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")

        i <- match("f2", p[["step"]])
        expect_equal(p[["out"]][[i]], 42)
        expect_equal(p[["state"]][[i]], "done")
    })

    it("signals invalid insertion position and unknown step reference", {
        p <- test_pip()
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = "unknown"),
            "step 'unknown' does not exist"
        )
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = 3.1),
            "after index must be a whole number"
        )
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = -1),
            "after index must be between 0 and 2"
        )
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = 3),
            "after index must be between 0 and 2"
        )
        expect_error(
            pip_add(p, "f3", \(x = ~f2) x, after = "f1"),
            "cannot reference unknown steps: 'f2'"
        )
    })

    it("signals duplicate step names also when inserting at position", {
        p <- test_pip()

        expect_error(
            pip_add(p, "f2", \(x = 1) x, after = "f1"),
            "step 'f2' already exists in the pipeline"
        )
    })

    it("merges params with function defaults, letting defaults win", {
        p <- pip_new()
        expect_warning(
            pip_add(
                p,
                "s1",
                function(x = 1, y = 2) x + y,
                params = list(x = 10, y = 20, z = 99)
            ),
            "defaults of fun take precedence over params: x, y"
        )

        pars <- p[["params"]][[1]]
        expect_equal(pars[["x"]], 1) # fun default takes precedence
        expect_equal(pars[["y"]], 2)
        expect_equal(pars[["z"]], 99) # extra param carried through
    })

    it("uses extra params passed via ... at runtime", {
        p <- pip_new()
        pip_add(
            p,
            "s1",
            function(x = 1, ...) c(x, ...),
            params = list(size = 42)
        )

        pars <- p[["params"]][[1]]
        expect_equal(pars[["x"]], 1)
        expect_equal(pars[["size"]], 42)

        pip_run(p, lgr = NULL)
        expect_equal(unname(p[["out"]][[1]]), c(1, 42))
    })

    it("resolves step references given via params", {
        p <- pip_new()
        pip_add(p, "s1", function(x = 1) x)
        pip_add(
            p,
            "s2",
            function(...) list(...),
            params = list(y = ~s1)
        )

        expect_equal(unname(p[["depends"]][[2]]), "s1")
        expect_equal(names(p[["depends"]][[2]]), "y")

        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[2]]$y, 1)
    })

    it("resolves relative step references given via params", {
        p <- pip_new()
        pip_add(p, "s1", function(x = 5) x)
        pip_add(
            p,
            "s2",
            function(...) list(...),
            params = list(y = ~ -1)
        )

        expect_equal(unname(p[["depends"]][[2]]), "s1")

        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[2]]$y, 5)
    })

    it("signals unknown steps referenced via params", {
        p <- pip_new()
        expect_error(
            pip_add(
                p,
                "s1",
                function(...) list(...),
                params = list(x = ~undefined)
            ),
            "cannot reference unknown steps: 'undefined'"
        )
    })

    it("keeps referenced params out of the independent params", {
        p <- pip_new()
        pip_add(p, "s1", function(x = 1) x)
        pip_add(
            p,
            "s2",
            function(...) list(...),
            params = list(y = ~s1, z = 2)
        )

        expect_equal(
            names(p[["params"]][[2]]),
            c("y", "z")
        )
        # y is a dependency, so it is not unbound
        expect_equal(p[["unbound"]][[2]], "z")
        expect_equal(names(pip_get_params(p)), c("x", "z"))
    })

    it("outdates the step state if one of the params is updated", {
        p <- pip_new()
        pip_add(
            p,
            "s1",
            function(x = 1, y = 2, ...) x + y,
            params = list(z = 99)
        )
        pip_run(p, lgr = NULL)

        expect_equal(p[["state"]][[1]], "done")
        pip_set_params(p, list(z = 100))
        expect_equal(p[["state"]][[1]], "outdated")
    })

    it("signals parameter without default value", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", function(x) x),
            "has no default value"
        )
    })

    it("auto-provides .self to steps that do not declare it", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) .self)

        pip_run(p, lgr = NULL)

        expect_true(identical(p[["out"]][[1]], p))
        expect_false(
            ".self" %in% names(p[["params"]][[1]])
        )
        expect_false(
            ".self" %in% names(formals(p[["fun"]][[1]]))
        )
    })

    it("signals if .self is declared as a step parameter", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", function(x = 1, .self = NULL) x),
            paste(
                "'.self' is a reserved parameter name and must not be",
                "declared in step 's1' - it is provided automatically"
            ),
            fixed = TRUE
        )
    })

    it("signals if .self is provided via params", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", function(x = 1) x, params = list(.self = p)),
            "'.self' is a reserved parameter name"
        )
    })

    it("refreshes auto-provided .self to the clone when run", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) .self)
        pip_run(p, lgr = NULL)

        p2 <- pip_clone(p)
        pip_run(p2, lgr = NULL, force = TRUE)

        expect_true(identical(p2[["out"]][[1]], p2))
        expect_true(identical(p[["out"]][[1]], p))
    })
})

describe("pip_add exec modes", {
    it("signals invalid exec modes", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", \(x = 1) x, exec = "invalid"),
            "exec must be one of: auto, split, reduce, plain"
        )
        expect_error(
            pip_add(p, "s1", \(x = 1) x, exec = NA_character_),
            "exec must be a single string"
        )
        expect_error(
            pip_add(p, "s1", \(x = 1) x, exec = c("auto", "split")),
            "exec must be a single string"
        )
    })

    it("accepts all valid exec modes", {
        for (mode in c("auto", "split", "reduce", "plain")) {
            p <- pip_new()
            expect_no_error(pip_add(p, "s1", \(x = 1) x, exec = mode))
            expect_equal(p[["exec"]][[1]], mode)
        }
    })
})


describe("pip_add try() references", {
    it("resolves the referenced step and keeps the formula in the params", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("a2", \(x = 2) x) |>
            pip_add("a3", \(x = 3) x) |>
            pip_add("b", \(x = ~a, y = ~ try(a2), z = ~ try(-1)) x)

        expect_equal(p[["b", "depends"]], c(x = "a", y = "a2", z = "a3"))
        expect_equal(p[["b", "params"]][["y"]], ~ try(a2), ignore_attr = TRUE)
    })

    it("resolves the referenced step when inserting a step", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("c", \(x = ~a) x) |>
            pip_add("b", \(x = ~ try(a)) x, after = "a")

        expect_equal(unname(p[["step"]]), c("a", "b", "c"))
        expect_equal(p[["b", "depends"]], c(x = "a"))
    })

    it("signals references to unknown steps", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x)

        expect_error_fixed(
            pip_add(p, "b", \(x = ~ try(nope)) x),
            "cannot reference unknown steps: 'nope'"
        )
        expect_error_fixed(
            pip_add(p, "b", \(x = ~ try(a, silent = TRUE)) x),
            "cannot reference unknown steps: 'try(a, silent = TRUE)'"
        )
        expect_equal(unname(p[["step"]]), "a")
    })
})


describe("pip_rename", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(a = 1) a) |>
            pip_add("f2", \(b = ~f1) b) |>
            pip_add("f3", \(a = ~f1, b = ~f2) a + b)
    }

    it("signals invalid inputs", {
        p <- test_pip()
        expect_error(pip_rename(1, "f1", "first"))
        expect_error(pip_rename(p, c("f1", "f2"), "first"))
        expect_error(pip_rename(p, NA_character_, "first"))
        expect_error(pip_rename(p, "", "first"))
        expect_error(pip_rename(p, "f1", c("a", "b")))
        expect_error(pip_rename(p, "f1", NA_character_))
        expect_error(pip_rename(p, "f1", ""))
    })

    it("signals missing old step and name clashes", {
        p <- test_pip()
        expect_error(
            pip_rename(p, "unknown", "first"),
            "step 'unknown' does not exist"
        )
        expect_error(
            pip_rename(p, "f1", "f2"),
            "step 'f2' already exists"
        )
    })

    it("renames step and updates dependencies", {
        p <- test_pip()
        pip_rename(p, from = "f1", to = "first")

        expect_equal(unname(p[["step"]]), c("first", "f2", "f3"))
        expect_equal(
            unname(p[["depends"]]),
            list(
                character(0),
                c(b = "first"),
                c(a = "first", b = "f2")
            )
        )
    })

    it("updates the formula references in params", {
        p <- test_pip()
        pip_rename(p, from = "f1", to = "first")

        expect_equal(
            lapply(unname(p[["params"]]), \(par) {
                vapply(par, deparse1, character(1))
            }),
            list(
                c(a = "1"),
                c(b = "~first"),
                c(a = "~first", b = "~f2")
            )
        )
    })

    it("keeps DAG and step mapping consistent", {
        p <- test_pip()
        pip_rename(p, from = "f1", to = "first")

        expect_true(is.na(.pip_steps_to_nodes(p, "f1")[[1]]))
        expect_true(!is.na(.pip_steps_to_nodes(p, "first")[[1]]))

        nodes <- .pip_get_reachable_nodes(p, "first")
        steps <- .pip_filter_nodes(p, nodes)[["step"]]
        expect_setequal(steps, c("first", "f2", "f3"))
    })

    it("renames a step through a view", {
        p <- test_pip()
        v <- pip_view(p, step = c("f2", "f3"))

        expect_error(
            pip_rename(v, from = "f1", to = "first"),
            "step 'f1' is not part of the view"
        )

        pip_rename(v, from = "f2", to = "second")

        expect_equal(unname(p[["step"]]), c("f1", "second", "f3"))
        expect_equal(
            unname(p[["depends"]]),
            list(
                character(0),
                c(b = "f1"),
                c(a = "f1", b = "second")
            )
        )
    })

    it("returns the pipeline as-is if from and to are identical", {
        p <- test_pip()
        pip_rename(p, from = "f1", to = "f1")

        expect_equal(unname(p[["step"]]), c("f1", "f2", "f3"))
        expect_equal(
            unname(p[["depends"]]),
            list(
                character(0),
                c(b = "f1"),
                c(a = "f1", b = "f2")
            )
        )
    })

    it("keeps the params of a single-step pipeline intact", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 5) x * 2)

        pip_rename(p, from = "s1", to = "first")

        expect_equal(p[["params"]][[1]], list(x = 5))
        expect_equal(
            p[["pipenv"]][["data"]][["params"]][[1]],
            list(x = 5)
        )
    })

    it("runs a bound pipeline whose steps were renamed", {
        p <- pip_new("pipe") |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x + 1)
        a <- pip_new("a") |>
            pip_add("a", \(x = 5) x * 2)
        b <- pip_new("b") |>
            pip_add("a", \(x = 1) x)

        out <- rbind(p, a, b)
        pip_run(out, lgr = NULL)

        expect_equal(
            pip_collect(out),
            list(a = 1, b = 2, a2 = 10, a3 = 1)
        )
    })
})


describe("pip_remove", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = ~f1) x) |>
            pip_add("f3", \(x = ~f2) x) |>
            pip_add("f4", \(x = ~f1) x) |>
            pip_add("g1", \(x = 9) x)
    }

    it("signals invalid inputs", {
        p <- test_pip()
        expect_error(pip_remove(1, "f1"), "x must be a pipeflow pip")
        expect_error(
            pip_remove(p, c("f1", "f2")),
            "step must be a single string"
        )
        expect_error(pip_remove(p, NA_character_), "step must not be NA")
        expect_error(pip_remove(p, "unknown"), "step 'unknown' does not exist")
        expect_error(
            pip_remove(p, "f1", force = NA),
            "force must be a single logical value"
        )
        expect_error(
            pip_remove(p, "f1", force = c(TRUE, FALSE)),
            "force must be a single logical value"
        )
    })

    it("removes a leaf step", {
        p <- test_pip()
        node <- as.integer(.pip_steps_to_nodes(p, "g1")[[1]])
        beforeOrder <- dag_get_nodes_order(p[["pipenv"]][[".dag"]])
        beforeReach <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "f1")
        )[["step"]]

        expect_true(dag_has_node(p[["pipenv"]][[".dag"]], node))
        expect_true(node %in% beforeOrder)
        expect_setequal(beforeReach, c("f1", "f2", "f3", "f4"))

        pip_remove(p, "g1")

        afterOrder <- dag_get_nodes_order(p[["pipenv"]][[".dag"]])
        afterReach <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "f1")
        )[["step"]]

        expect_equal(
            unname(p[["step"]]),
            c("f1", "f2", "f3", "f4")
        )
        expect_true(is.na(.pip_steps_to_nodes(p, "g1")[[1]]))
        expect_false(dag_has_node(p[["pipenv"]][[".dag"]], node))
        expect_false(node %in% afterOrder)
        expect_equal(length(afterOrder), length(beforeOrder) - 1L)
        expect_setequal(afterReach, c("f1", "f2", "f3", "f4"))
    })

    it("errors when direct downstream dependencies exist", {
        p <- test_pip()
        expect_error(
            pip_remove(p, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f4'"
            )
        )
    })

    it("removes step and all downstream dependencies recursively", {
        p <- test_pip()
        steps <- c("f1", "f2", "f3", "f4", "g1")
        nodeMap <- vapply(
            steps,
            FUN = \(s) as.integer(.pip_steps_to_nodes(p, s)[[1]]),
            FUN.VALUE = integer(1)
        )
        beforeOrder <- dag_get_nodes_order(p[["pipenv"]][[".dag"]])
        dag <- p[["pipenv"]][[".dag"]]

        expect_true(all(vapply(
            nodeMap,
            FUN = \(nid) dag_has_node(dag, nid),
            FUN.VALUE = logical(1)
        )))

        out <- utils::capture.output(
            pip_remove(p, "f1", force = TRUE),
            type = "message"
        )

        afterOrder <- dag_get_nodes_order(dag)
        remainingNode <- as.integer(nodeMap[["g1"]])

        expect_equal(unname(p[["step"]]), "g1")
        expect_equal(unname(p[["nodeId"]]), remainingNode)
        expect_equal(afterOrder, remainingNode)
        expect_equal(length(afterOrder), length(beforeOrder) - 4L)
        expect_true(dag_has_node(dag, remainingNode))
        expect_false(dag_has_node(dag, as.integer(nodeMap[["f1"]])))
        expect_false(dag_has_node(dag, as.integer(nodeMap[["f2"]])))
        expect_false(dag_has_node(dag, as.integer(nodeMap[["f3"]])))
        expect_false(dag_has_node(dag, as.integer(nodeMap[["f4"]])))
        expect_equal(
            .pip_filter_nodes(p, .pip_get_reachable_nodes(p, "g1"))[["step"]],
            "g1"
        )
        expect_equal(
            out,
            paste(
                "Removing step 'f1' and its downstream dependencies:",
                "'f2', 'f3', 'f4'"
            )
        )
    })

    it("removes a step through a view and remaps the selector", {
        p <- test_pip()
        v <- pip_view(p, step = c("f4", "g1"))
        expect_equal(v[["view"]], c(4L, 5L))

        expect_error(
            pip_remove(v, "f1"),
            "step 'f1' is not part of the view"
        )

        v <- pip_remove(v, "f4")

        expect_equal(
            unname(p[["step"]]),
            c("f1", "f2", "f3", "g1")
        )
        expect_equal(v[["step"]], c(g1 = "g1"))
        expect_equal(v[["view"]], 4L)
    })

    it("force-removes downstream steps through a view", {
        p <- test_pip()
        v <- pip_view(p, step = c("f1", "f2"))

        suppressMessages(v <- pip_remove(v, "f1", force = TRUE))

        expect_equal(unname(p[["step"]]), "g1")
        expect_equal(length(v), 0L)
    })
})


describe("pip_replace", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = 2) x) |>
            pip_add("f3", \(x = ~f2) x + 1)
    }

    it("warns if params overlap the defaults of fun", {
        p <- test_pip()
        expect_warning(
            pip_replace(p, "f2", \(x = 20) x, params = list(x = 5)),
            "step 'f2': the defaults of fun take precedence over params: x",
            fixed = TRUE
        )
        expect_equal(p[["params"]][["f2"]][["x"]], 20)
    })

    it("does not warn if params do not overlap the defaults of fun", {
        p <- test_pip()
        expect_no_warning(
            pip_replace(p, "f2", \(x = 20) x, params = list(z = 5))
        )
    })

    it("signals invalid inputs", {
        p <- test_pip()

        expect_error(
            pip_replace(1, "f1", \(x = 1) x),
            "x must be a pipeflow pip"
        )
        expect_error(
            pip_replace(p, c("f1", "f2"), \(x = 1) x),
            "step must be a single string"
        )
        expect_error(
            pip_replace(p, NA_character_, \(x = 1) x),
            "step must not be NA"
        )
        expect_error(
            pip_replace(p, "", \(x = 1) x),
            "step must be a non-empty string"
        )
        expect_error(
            pip_replace(p, "unknown", \(x = 1) x),
            "step 'unknown' does not exist"
        )
        expect_error(pip_replace(p, "f1", 1), "fun must be a function")
    })

    it("signals if fun is a primitive function", {
        p <- test_pip()
        expect_error(
            pip_replace(p, "f1", sum),
            "fun must not be a primitive function; wrap it"
        )
        expect_error(
            p[["f1", "fun"]] <- sum,
            "fun must not be a primitive function; wrap it"
        )
        expect_false(is.primitive(p[["f1", "fun"]]))
    })

    it("replaces a step in-place while keeping the original order", {
        p <- test_pip()
        pip_run(p, lgr = NULL)
        expect_equal(p[["pipenv"]][["data"]][step == "f3", out][[1]], 3)

        pip_replace(p, "f2", \(x = 4) x * 2)
        expect_equal(unname(p[["step"]]), c("f1", "f2", "f3"))

        pip_run(p, lgr = NULL)
        expect_equal(p[["pipenv"]][["data"]][step == "f2", out][[1]], 8)
        expect_equal(p[["pipenv"]][["data"]][step == "f3", out][[1]], 9)
    })

    it("verifies replacement dependencies against earlier steps only", {
        p <- test_pip()

        expect_error(
            pip_replace(p, "f2", \(x = ~f3) x),
            "cannot reference unknown steps: 'f3'"
        )
        expect_error(
            pip_replace(p, "f2", \(x = ~foo) x),
            "cannot reference unknown steps: 'foo'"
        )
    })

    it("marks only downstream dependent steps as outdated", {
        p <- pip_new("pipe") |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x + 1) |>
            pip_add("b1", \(x = 10) x) |>
            pip_add("a3", \(x = ~a2) x + 1)

        pip_run(p, lgr = NULL)
        pip_replace(p, "a2", \(x = ~a1) x + 2)

        expect_equal(
            unname(p[["state"]]),
            c("done", "new", "done", "outdated")
        )
    })

    it("leaves downstream steps that have never run as new", {
        p <- pip_new("pipe") |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x + 1) |>
            pip_add("a3", \(x = ~a2) x + 1)

        pip_replace(p, "a2", \(x = ~a1) x + 2)

        expect_equal(unname(p[["state"]]), c("new", "new", "new"))
    })

    it("updates tags on replaced step", {
        p <- pip_new("pipe") |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x + 1)

        pip_replace(
            p,
            "a2",
            \(x = ~a1) x + 10,
            tags = c("updated", "core")
        )

        i <- match("a2", p[["step"]])
        expect_equal(
            p[["tags"]][[i]],
            c("updated", "core")
        )
    })

    it("replaces a step through a view", {
        p <- test_pip()
        v <- pip_view(p, step = c("f2", "f3"))

        expect_error(
            pip_replace(v, "f1", \(x = 1) x),
            "step 'f1' is not part of the view"
        )

        pip_replace(v, "f2", \(x = 4) x * 2)

        expect_equal(unname(p[["step"]]), c("f1", "f2", "f3"))
        pip_run(p, lgr = NULL)
        expect_equal(p[["pipenv"]][["data"]][step == "f2", out][[1]], 8)
        expect_equal(p[["pipenv"]][["data"]][step == "f3", out][[1]], 9)
    })

    it("takes try() references from the new function", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) stop("boom")) |>
            pip_add("b", \(x = ~ try(a)) conditionMessage(x))

        pip_replace(p, "b", \(y = ~a) y)
        expect_equal(p[["b", "depends"]], c(y = "a"))
        suppressWarnings(pip_run(p, lgr = NULL, on_error = "continue"))
        expect_equal(p[["b", "state"]], "new")

        pip_replace(p, "b", \(y = ~ try(a)) conditionMessage(y))
        suppressWarnings(pip_run(p, lgr = NULL, on_error = "continue"))
        expect_equal(p[["b", "out"]], "step 'a' failed: boom")
    })
})


describe("pip_clone", {
    test_pip <- function() {
        pip_new("p1") |>
            pip_add("s1", \(x = 1) .self[["name"]]) |>
            pip_add("s2", \(x = ~s1) x)
    }

    it("signals invalid inputs", {
        expect_error(pip_clone(1), "x must be a pipeflow pip")
        expect_error(
            pip_clone(pip_new(), name = NA_character_),
            "name must be a single non-NA string"
        )
        expect_error(
            pip_clone(pip_new(), name = c("a", "b")),
            "name must be a single non-NA string"
        )
    })

    it("returns a pipeflow pipeline copy with same content", {
        p <- test_pip()
        p2 <- pip_clone(p)

        expect_true(.is_pipeflow(p2))
        expect_false(identical(p2, p))
        expect_equal(p2[["name"]], p[["name"]])
        expect_equal(
            p2[["step"]],
            p[["step"]]
        )
        expect_equal(
            p2[["depends"]],
            p[["depends"]]
        )
    })

    it("supports overriding the clone name", {
        p <- test_pip()
        p2 <- pip_clone(p, name = "copied")
        expect_equal(p2[["name"]], "copied")
        expect_equal(p[["name"]], "p1")
    })

    it("creates an independent copy", {
        p <- test_pip()
        p2 <- pip_clone(p)

        env <- p2[["pipenv"]]
        env[["data"]][["state"]][1] <- "done"
        expect_equal(unname(p[["state"]][1]), "new")

        pip_add(p2, "s3", \(x = ~s2) x)
        expect_false("s3" %in% p[["step"]])
        expect_true("s3" %in% p2[["step"]])
    })

    it("rebinds .self params to the cloned pipeline", {
        p <- test_pip()
        p2 <- pip_clone(p)

        pip_run(p2, lgr = NULL)
        expect_equal(p2[["out"]][[1]], "p1")

        # The original pipeline still points at itself.
        pip_run(p, lgr = NULL, force = TRUE)
        expect_equal(p[["out"]][[1]], "p1")
    })

    it("does not change .self of the original when the clone runs", {
        p <- test_pip()
        p2 <- pip_clone(p, name = "p2")
        pip_run(p2, lgr = NULL)

        expect_equal(p2[["out"]][[1]], "p2")
        self <- environment(p[["s1", "fun"]])[[".self"]]
        expect_identical(self[["pipenv"]], p[["pipenv"]])
    })

    it("keeps .self of a running step when a clone of it runs", {
        p <- pip_new("self") |>
            pip_add("s", \(x = 1) {
                before <- .self[["name"]]
                if (x == 1) {
                    cl <- pip_clone(.self, name = "inner-clone")
                    pip_set_params(cl, list(x = 2)) |> pip_run(lgr = NULL)
                }
                c(before = before, after = .self[["name"]])
            })
        pip_run(p, lgr = NULL)

        expect_equal(p[["s", "out"]], c(before = "self", after = "self"))
    })

    it("clones an empty pipeline", {
        p <- pip_new()
        p2 <- pip_clone(p)

        expect_equal(length(p2), 0L)
        expect_equal(p2[["name"]], p[["name"]])
        expect_false(identical(p2, p))
    })
})


describe("pip_collect", {
    it("returns empty list for empty pipeline", {
        p <- pip_new()
        expect_equal(pip_collect(p), list())
    })

    it("returns named flat list", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = "data")
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")

        data.table::set(
            p[["pipenv"]][["data"]],
            j = "out",
            value = list(10, 20)
        )

        out <- pip_collect(p)
        expect_equal(names(out), c("s1", "s2"))
        expect_equal(unname(out), list(10, 20))
    })

    it("works on pipeline views", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = "data")
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~ -1) x + 1, tags = "model")

        data.table::set(
            p[["pipenv"]][["data"]],
            j = "out",
            value = list("o1", "o2", "o3")
        )

        v <- pip_view(p, tags = "model")
        out <- pip_collect(v)

        expect_equal(names(out), c("s2", "s3"))
        expect_equal(unname(out), list("o2", "o3"))
    })

    it("preserves NULL outputs", {
        p <- pip_new("test") |>
            pip_add("s1", \(x = NULL) x) |>
            pip_add("s2", \(x = 3) x, tags = "g2") |>
            pip_add("s3", \(x = ~s2, factor = 2) x * factor) |>
            pip_add("s4", \(x = ~s2) x^2, tags = "g4")

        out <- pip_collect(p)
        expect_equal(out, list(s1 = NULL, s2 = NULL, s3 = NULL, s4 = NULL))

        pip_run(p, lgr = NULL)
        out <- pip_collect(p)
        expect_equal(out, list(s1 = NULL, s2 = 3, s3 = 2 * 3, s4 = 3^2))
    })

    it("signals invalid arguments", {
        p <- pip_new()
        expect_error(pip_collect(1), "x must be a pipeflow pip or view")
    })

    test_group_pip <- function() {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = c("io", "daily"))
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "io")
        pip_add(p, "s3", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s4", \(x = ~ -1) x + 1)
        data.table::set(
            p[["pipenv"]][["data"]],
            j = "out",
            value = list("o1", "o2", "o3", "o4")
        )
        p
    }

    it("groups outputs by tag", {
        p <- test_group_pip()
        out <- pip_collect(p, by = "tags")

        expect_equal(
            out,
            list(
                io = list(s1 = "o1", s2 = "o2"),
                daily = list(s1 = "o1"),
                model = list(s3 = "o3")
            )
        )
    })

    it("returns the grouped table with as.table = TRUE", {
        p <- test_group_pip()
        out <- pip_collect(p, by = "tags", as.table = TRUE)

        expect_true(data.table::is.data.table(out))
        expect_equal(colnames(out), c("tags", "out"))
        expect_equal(out[["tags"]], c("io", "daily", "model"))
        expect_equal(
            out[["out"]],
            list(
                list(s1 = "o1", s2 = "o2"),
                list(s1 = "o1"),
                list(s3 = "o3")
            )
        )
    })

    it("returns the table results visibly", {
        p <- test_group_pip()

        expect_true(withVisible(
            pip_collect(p, by = "tags", as.table = TRUE)
        )[["visible"]])
        expect_true(withVisible(
            pip_collect(p, as.table = TRUE)
        )[["visible"]])
    })

    it("returns a flat table with if groups are of size 1", {
        p <- test_group_pip()
        out <- pip_collect(p, as.table = TRUE)

        expect_true(data.table::is.data.table(out))
        expect_equal(colnames(out), c("step", "out"))
        expect_equal(out[["step"]], c("s1", "s2", "s3", "s4"))
        expect_equal(out[["out"]], list("o1", "o2", "o3", "o4"))

        out <- pip_collect(p, by = "nodeId", as.table = TRUE)
        expect_true(data.table::is.data.table(out))
        expect_equal(colnames(out), c("nodeId", "out"))
        expect_equal(out[["nodeId"]], 0:3)
        expect_equal(out[["out"]], list("o1", "o2", "o3", "o4"))
    })

    it("returns a flat list with if groups are of size 1", {
        p <- test_group_pip()
        out <- pip_collect(p)
        expect_equal(
            out,
            list(s1 = "o1", s2 = "o2", s3 = "o3", s4 = "o4")
        )

        out <- pip_collect(p, by = "nodeId")
        expect_equal(
            out,
            list(`0` = "o1", `1` = "o2", `2` = "o3", `3` = "o4")
        )
    })

    it("keeps single-step groups nested with simplify = FALSE", {
        p <- test_group_pip()
        v <- pip_view(p, step = "s3") # single tag: "model"

        expect_equal(pip_collect(v, by = "tags"), list(model = "o3"))
        expect_equal(
            pip_collect(v, by = "tags", simplify = FALSE),
            list(model = list(s3 = "o3"))
        )
        expect_error(
            pip_collect(v, by = "tags", simplify = "yes"),
            "simplify must be a single logical value"
        )
    })

    it("groups by scalar columns", {
        p <- test_group_pip()
        out <- pip_collect(p, by = "state")

        expect_equal(
            out,
            list(new = list(s1 = "o1", s2 = "o2", s3 = "o3", s4 = "o4"))
        )
    })

    it("groups within a view", {
        p <- test_group_pip()
        v <- pip_view(p, tags = "io")
        out <- pip_collect(v, by = "tags")

        expect_equal(
            out,
            list(
                io = list(s1 = "o1", s2 = "o2"),
                daily = list(s1 = "o1")
            )
        )
    })

    it("returns an empty result for untagged or empty pipelines", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)

        expect_equal(pip_collect(p, by = "tags"), list())
        tbl <- pip_collect(p, by = "tags", as.table = TRUE)
        expect_true(data.table::is.data.table(tbl))
        expect_equal(colnames(tbl), c("tags", "out"))
        expect_equal(nrow(tbl), 0L)

        e <- pip_new()
        expect_equal(pip_collect(e, by = "state"), list())
        flat <- pip_collect(e, as.table = TRUE)
        expect_true(data.table::is.data.table(flat))
        expect_equal(colnames(flat), c("step", "out"))
        expect_equal(nrow(flat), 0L)
    })

    it("signals invalid by and as.table arguments", {
        p <- test_group_pip()

        expect_error(
            pip_collect(p, by = 1),
            "by must be a single column name"
        )
        expect_error(
            pip_collect(p, by = c("tags", "state")),
            "by must be a single column name"
        )
        expect_error(pip_collect(p, by = "nope"), "unknown column: nope")
        expect_error(
            pip_collect(p, by = "out"),
            "cannot be used as a grouping column"
        )
        expect_error(
            pip_collect(p, by = "params"),
            "cannot be used as a grouping column"
        )
        expect_error(
            pip_collect(p, as.table = "yes"),
            "as.table must be a single logical value"
        )
    })

    it("keeps pip_collect_out as a deprecated alias", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x, tags = "io") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "model")
        pip_run(p, lgr = NULL)

        expect_warning(
            out <- pip_collect_out(p),
            "pip_collect_out"
        )
        expect_equal(out, pip_collect(p))
        expect_equal(
            suppressWarnings(
                pip_collect_out(p, by = "tags", as.table = TRUE)
            ),
            pip_collect(p, by = "tags", as.table = TRUE)
        )
    })
})


describe("pip_get_params", {
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(data = data.frame(a = 1:2)) data) |>
            pip_add("s2", \(data = ~s1, x = 1) data[, 2] + x) |>
            pip_add("s3", \(y = ~s2) y) |>
            pip_add("s4", \(x = 9, y = ~s2) x + y)
    }

    it("returns independent params from a pipeline", {
        p <- test_pip()
        params <- pip_get_params(p)

        expect_named(params, c("data", "x"))
        expect_equal(params[["data"]], data.frame(a = 1:2))
        expect_equal(params[["x"]], 1)
    })

    it("returns params from a view subset only", {
        p <- pip_new()
        data <- data.frame(a = 1:2, b = 3:4)

        pip_add(p, "s1", \(data = data) data)
        pip_add(p, "s2", \(data = ~s1, x = 1) data[, 2] + x)
        pip_add(p, "s3", \(y = ~s2) y)
        pip_add(p, "s4", \(x = 9, y = ~s2) x + y)

        v <- pip_view(p, step = c("s3", "s4"))
        params <- pip_get_params(v)

        expect_named(params, "x")
        expect_equal(params[["x"]], 9)
    })

    it("returns empty list for view without independent params", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(y = ~s1) y)

        v <- pip_view(p, step = "s2")
        expect_equal(pip_get_params(v), list())
    })

    it("signals invalid input", {
        expect_error(
            pip_get_params(1),
            "x must be a pipeflow pip or view"
        )
    })
})


describe("pip_graph", {
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1) x, tags = "io", exec = "split") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "model", exec = "reduce") |>
            pip_add("s3", \(x = ~s2) x + 2, tags = "model")
    }

    map_edge_labels <- function(graph) {
        idToLabel <- stats::setNames(
            graph[["nodes"]][["label"]],
            as.character(graph[["nodes"]][["id"]])
        )
        paste0(
            idToLabel[as.character(graph[["edges"]][["from"]])],
            "->",
            idToLabel[as.character(graph[["edges"]][["to"]])]
        )
    }

    it("returns visNetwork-compatible nodes and edges for pipelines", {
        p <- test_pip()
        env <- p[["pipenv"]]
        env[["data"]][["state"]] <- c("new", "done", "failed")

        g <- pip_graph(p)
        nodes <- g[["nodes"]]
        edges <- g[["edges"]]

        expect_named(g, c("nodes", "edges"))
        expect_s3_class(nodes, "data.frame")
        expect_s3_class(edges, "data.frame")
        expect_equal(names(nodes), c("id", "label", "shape", "color"))
        expect_equal(names(edges), c("from", "to", "arrows"))
        nodeShape <- stats::setNames(nodes[["shape"]], nodes[["label"]])
        expect_equal(nodeShape[["s1"]], "star")
        expect_equal(nodeShape[["s2"]], "dot")
        expect_equal(nodeShape[["s3"]], "hexagon")
        expect_true(all(edges[["arrows"]] == "to"))

        expectedColors <- vapply(
            p[["state"]],
            FUN = \(st) .step_states[[st]][["color"]],
            FUN.VALUE = character(1)
        )
        expect_equal(nodes[["color"]], unname(expectedColors))
        expect_setequal(map_edge_labels(g), c("s1->s2", "s2->s3"))
    })

    it("supports view-only graph or graph with upstream closure", {
        p <- test_pip()
        v <- pip_view(p, step = "s3")

        gView <- pip_graph(v, include_upstream = FALSE)
        expect_equal(gView[["nodes"]][["label"]], "s3")
        expect_equal(nrow(gView[["edges"]]), 0L)

        gUp <- pip_graph(v, include_upstream = TRUE)
        expect_setequal(gUp[["nodes"]][["label"]], c("s1", "s2", "s3"))
        expect_setequal(map_edge_labels(gUp), c("s1->s2", "s2->s3"))
    })

    it("signals invalid inputs", {
        p <- test_pip()

        expect_error(pip_graph(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_graph(p, include_upstream = c(TRUE, FALSE)),
            "include_upstream must be a single logical value"
        )
    })

    it("returns empty nodes and edges for empty pipeline", {
        g <- pip_graph(pip_new())
        expect_equal(nrow(g[["nodes"]]), 0L)
        expect_equal(nrow(g[["edges"]]), 0L)
        expect_named(g, c("nodes", "edges"))
    })

    it("keeps pip_get_graph as a deprecated alias", {
        p <- test_pip()

        expect_warning(
            g <- pip_get_graph(p),
            "pip_get_graph"
        )
        expect_equal(pip_graph(p), g)

        expect_error(
            suppressWarnings(pip_get_graph(1)),
            "x must be a pipeflow pip or view"
        )
    })
})


describe("pip_data", {
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("returns the underlying step table", {
        p <- test_pip()
        d <- pip_data(p)

        expect_true(data.table::is.data.table(d))
        expect_equal(nrow(d), 2L)
        expect_equal(d[["step"]], c("s1", "s2"))
    })

    it("returns the full table for views", {
        p <- test_pip()
        v <- pip_view(p, step = "s2")

        expect_equal(nrow(pip_data(v)), 2L)
    })

    it("is exposed as a virtual method", {
        p <- test_pip()

        expect_true(data.table::is.data.table(p$data()))
        expect_true(data.table::is.data.table(p[["data"]]()))
    })

    it("signals invalid inputs", {
        expect_error(pip_data(1), "x must be a pipeflow pip or view")
    })
})


describe("pip_run", {
    test_pip <- function() {
        pip_new() |>
            pip_add("load_raw", \(x = 1) x, tags = "io") |>
            pip_add("fit_model", \(x = ~ -1) x + 1, tags = "model") |>
            pip_add("eval_model", \(x = ~fit_model) x, tags = "model") |>
            pip_add("bla_bla", \(bla = "blabla") bla, tags = "bla")
    }

    describe("standard runs", {
        it("runs all steps of the pipeline and marks them as done", {
            p <- test_pip()
            pip_run(p, lgr = NULL)
            expect_equal(
                unname(p[["out"]]),
                list(1, 2, 2, "blabla")
            )
            expect_equal(unname(p[["state"]]), rep("done", 4))
        })

        it("marks downstream steps not reached due to abort as outdated", {
            fail <- FALSE
            p <- pip_new() |>
                pip_add(
                    "load_raw",
                    \(x = 1) if (fail) stop("io error") else x,
                    tags = "io"
                ) |>
                pip_add("fit_model", \(x = ~ -1) x + 1, tags = "model") |>
                pip_add("eval_model", \(x = ~fit_model) x, tags = "model")
            pip_run(p, lgr = NULL)

            fail <- TRUE
            expect_error(pip_run(p, lgr = NULL, force = TRUE), "io error")
            expect_equal(
                unname(p[["state"]]),
                c("failed", "outdated", "outdated")
            )
        })

        it("leaves new steps not reached due to abort as new", {
            p <- pip_new() |>
                pip_add("load_raw", \(x = 1) stop("io error"), tags = "io") |>
                pip_add("fit_model", \(x = ~ -1) x + 1, tags = "model") |>
                pip_add("eval_model", \(x = ~fit_model) x, tags = "model")

            expect_error(pip_run(p, lgr = NULL), "io error")
            expect_equal(
                unname(p[["state"]]),
                c("failed", "new", "new")
            )
        })

        it("keeps the state of unreached steps whose inputs did not run", {
            p <- pip_new() |>
                pip_add("data", \(n = 3) n) |>
                pip_add("a", \(d = ~data) d + 1) |>
                pip_add("fails", \(x = 1) stop("boom")) |>
                pip_add("b", \(d = ~data) d + 2)
            p[c("data", "a", "b"), "state"] <- "done"

            expect_error(pip_run(p, lgr = NULL), "boom")
            expect_equal(
                unname(p[["state"]]),
                c("done", "done", "failed", "done")
            )
        })

        it("marks the run state as failed when a step errors", {
            p <- pip_new() |>
                pip_add("s1", \(x = 1) x) |>
                pip_add("s2", \(x = ~s1) stop("boom")) |>
                pip_add("s3", \(x = ~s2) x + 1)

            expect_error(pip_run(p, lgr = NULL), "boom")
            expect_equal(get_run_state(p), "failed")

            # A subsequent successful run resets the state to ready.
            pip_replace(p, "s2", \(x = ~s1) x + 1)
            pip_run(p, lgr = NULL)
            expect_equal(get_run_state(p), "ready")
        })

        it("marks the run state as failed when a view run errors", {
            p <- pip_new() |>
                pip_add("a", \(x = 1) x) |>
                pip_add("b", \(x = ~a) stop("view boom"))
            v <- pip_view(p, step = "b")

            expect_error(pip_run(v, lgr = NULL), "view boom")
            expect_equal(get_run_state(p), "failed")
        })
    })

    describe("signals step errors", {
        catch_step_error <- function(x, ...) {
            tryCatch(
                pip_run(x, lgr = NULL, ...),
                pipeflow_step_error = \(e) e
            )
        }

        it("names the failed step and keeps the original condition", {
            p <- pip_new() |>
                pip_add("data", \(x = 1) x) |>
                pip_add("a", \(x = ~data) stop("boom"))

            expect_error(
                pip_run(p, lgr = NULL),
                "step 'a': boom",
                fixed = TRUE,
                class = "pipeflow_step_error"
            )

            e <- catch_step_error(p)
            expect_s3_class(
                e,
                c("pipeflow_step_error", "error", "condition"),
                exact = TRUE
            )
            expect_equal(conditionMessage(e), "step 'a': boom")
            expect_equal(e[["step"]], "a")
            expect_s3_class(e[["parent"]], "simpleError")
            expect_equal(conditionMessage(e[["parent"]]), "boom")
            expect_null(e[["call"]])
            expect_equal(get_run_state(p), "failed")
        })

        it("keeps the class of the original condition", {
            skip_if_not_installed("rlang")
            p <- pip_new() |>
                pip_add("a", \(x = 1) {
                    rlang::abort(
                        "boom",
                        class = "my_error",
                        body = c(i = "hint")
                    )
                })

            e <- catch_step_error(p)

            expect_s3_class(e[["parent"]], "my_error")
            expect_s3_class(e[["parent"]], "rlang_error")
            expect_equal(
                conditionMessage(e),
                paste0("step 'a': ", conditionMessage(e[["parent"]]))
            )
            expect_match(conditionMessage(e), "hint", fixed = TRUE)
        })

        it("names the failed upstream step of a view run", {
            p <- pip_new() |>
                pip_add("a", \(x = 1) stop("upstream boom")) |>
                pip_add("b", \(x = ~a) x)

            e <- catch_step_error(pip_view(p, step = "b"))

            expect_equal(e[["step"]], "a")
            expect_equal(conditionMessage(e), "step 'a': upstream boom")
        })

        it("keeps the step error of a run after a restart", {
            cnt <- new.env()
            cnt[["n"]] <- 0L
            p <- pip_new() |>
                pip_add("s1", function(x = 1) {
                    cnt[["n"]] <- cnt[["n"]] + 1L
                    if (cnt[["n"]] == 1L) {
                        .self$restart()
                    }
                    x
                }) |>
                pip_add("s2", \(x = ~s1) stop("boom"))

            e <- catch_step_error(p)

            expect_equal(cnt[["n"]], 2L)
            expect_equal(e[["step"]], "s2")
            expect_equal(conditionMessage(e), "step 's2': boom")
            expect_equal(get_run_state(p), "failed")
        })

        it("nests the step errors of inner pipelines", {
            inner <- pip_new("inner") |>
                pip_add("i", \(x = 1) stop("boom"))
            p <- pip_new("outer") |>
                pip_add("o", \(x = 1) pip_run(inner, lgr = NULL))

            e <- catch_step_error(p)

            expect_equal(conditionMessage(e), "step 'o': step 'i': boom")
            expect_equal(e[["step"]], "o")
            expect_s3_class(e[["parent"]], "pipeflow_step_error")
            expect_equal(e[["parent"]][["step"]], "i")
        })

        it("keeps other errors of the run as they are", {
            p <- pip_new() |>
                pip_add("a", \(x = 1) x)
            progress <- function(value, detail) stop("progress boom")

            e <- tryCatch(
                pip_run(p, lgr = NULL, progress = progress),
                error = \(e) e
            )

            expect_false(inherits(e, "pipeflow_step_error"))
            expect_equal(conditionMessage(e), "progress boom")
            expect_null(e[["call"]])
            expect_equal(get_run_state(p), "failed")
        })
    })

    describe("running views", {
        it("can run parts of the pipeline via views", {
            p <- test_pip()
            v <- pip_view(p, tags = "bla")
            pip_run(v, lgr = NULL)
            expect_equal(
                unname(p[["out"]]),
                list(NULL, NULL, NULL, "blabla")
            )
        })

        it("runs all steps of the view plus upstream dependencies", {
            p <- test_pip()
            v <- pip_view(p, tags = "model")
            pip_run(v, lgr = NULL)
            expect_equal(unname(p[["out"]]), list(1, 2, 2, NULL))

            p <- pip_new() |>
                pip_add("f1", \(x = 1) x) |>
                pip_add("f2", \(x = ~f1) x + 1) |>
                pip_add("f3", \(x = 1) x + 1) |>
                pip_add("f4", \(x = ~f2) x + 1) |>
                pip_add("f5", \(x = ~f4) x + 1)

            v <- pip_view(p, step = "f2")
            pip_run(v, lgr = NULL)
            expect_equal(
                unname(p[["out"]]),
                list(1, 2, NULL, NULL, NULL)
            )

            v <- pip_view(p, step = "f5")
            pip_run(v, lgr = NULL)
            expect_equal(
                unname(p[["out"]]),
                list(1, 2, NULL, 3, 4)
            )
        })

        it("marks downstream steps outside the view as outdated", {
            p <- test_pip()
            pip_run(p, lgr = NULL)
            v <- pip_view(p, tags = "io")
            pip_run(v, lgr = NULL, force = TRUE)
            expect_equal(
                unname(p[["state"]]),
                c("done", "outdated", "outdated", "done")
            )
        })

        it("leaves new steps outside the view as new", {
            p <- test_pip()
            v <- pip_view(p, tags = "io")
            pip_run(v, lgr = NULL)
            expect_equal(
                unname(p[["state"]]),
                c("done", "new", "new", "new")
            )
        })

        it("does not outdate steps when the view run executes nothing", {
            p <- pip_new("p") |>
                pip_add("data", \(n = 3) n) |>
                pip_add("a", \(d = ~data) d + 1) |>
                pip_add("b", \(d = ~data) d + 2)
            pip_run(p, lgr = NULL)

            pip_run(pip_view(p, step = "a"), lgr = NULL)

            expect_equal(unname(p[["state"]]), c("done", "done", "done"))
        })

        it("outdates only steps downstream of executed view steps", {
            p <- pip_new("p") |>
                pip_add("data", \(n = 3) n) |>
                pip_add("a", \(d = ~data) d + 1) |>
                pip_add("a2", \(x = ~a) x) |>
                pip_add("b", \(d = ~data) d + 2)
            pip_run(p, lgr = NULL)
            p[["a", "state"]] <- "outdated"

            pip_run(pip_view(p, step = "a"), lgr = NULL)

            expect_equal(
                unname(p[["state"]]),
                c("done", "done", "outdated", "done")
            )
        })

        it("does not outdate locked steps", {
            p <- pip_new("p") |>
                pip_add("data", \(n = 3) n) |>
                pip_add("a", \(d = ~data) d + 1) |>
                pip_add("b", \(d = ~data) d + 2)
            pip_run(p, lgr = NULL)
            pip_lock(p["b"])

            pip_run(pip_view(p, step = "a"), lgr = NULL, force = TRUE)

            expect_equal(unname(p[["state"]]), c("done", "done", "done"))
        })

        it("adds [view]/[upstream] markers to view-run logs", {
            p <- test_pip()
            v <- pip_view(p, tags = "model")

            logs <- character(0)
            lgr <- function(level, msg) {
                logs <<- c(logs, paste(level, msg))
            }

            pip_run(v, lgr = lgr)

            expect_true(any(grepl("\\[upstream\\] load_raw", logs)))
            expect_true(any(grepl("\\[view\\] fit_model", logs)))
            expect_true(any(grepl("\\[view\\] eval_model", logs)))
        })
    })

    describe("partition-aware execution", {
        test_pip_partitioned <- function() {
            pip_new() |>
                pip_add(
                    "load",
                    \(
                        x = data.frame(
                            grp = c("a", "a", "b", "b"),
                            value = c(1, 3, 10, 20)
                        )
                    ) {
                        x
                    }
                ) |>
                pip_add(
                    "split",
                    \(x = ~load) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "mean_by_grp",
                    \(x = ~split) mean(x$value)
                ) |>
                pip_add(
                    "plus_one",
                    \(x = ~mean_by_grp) x + 1
                ) |>
                pip_add(
                    "overall",
                    \(x = ~plus_one) mean(unlist(x)),
                    exec = "reduce"
                )
        }

        it("maps downstream calls over split output in auto mode", {
            p <- test_pip_partitioned()
            pip_run(p, lgr = NULL)

            splitOut <- p[["pipenv"]][["data"]]$out[[2]]
            meanOut <- p[["pipenv"]][["data"]]$out[[3]]
            plusOneOut <- p[["pipenv"]][["data"]]$out[[4]]

            expect_true(inherits(splitOut, "pipeflow_partitioned"))
            expect_true(inherits(meanOut, "pipeflow_partitioned"))
            expect_true(inherits(plusOneOut, "pipeflow_partitioned"))

            expect_equal(meanOut[["a"]], 2)
            expect_equal(meanOut[["b"]], 15)
            expect_equal(plusOneOut[["a"]], 3)
            expect_equal(plusOneOut[["b"]], 16)
            expect_equal(p[["pipenv"]][["data"]]$out[[5]], 9.5)
        })

        it("errors when reduce mode receives only non-partitioned inputs", {
            p <- pip_new() |>
                pip_add("load", \(x = 1) x) |>
                pip_add("sum", \(x = ~load) x + 1, exec = "reduce")

            expect_error(
                pip_run(p, lgr = NULL),
                "reduce mode requires at least one partitioned input"
            )
        })

        it("errors when plain mode receives partitioned input", {
            p <- pip_new() |>
                pip_add(
                    "load",
                    \(
                        x = data.frame(
                            grp = c("a", "a", "b", "b"),
                            value = c(1, 3, 10, 20)
                        )
                    ) {
                        x
                    }
                ) |>
                pip_add(
                    "split",
                    \(x = ~load) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "strict",
                    \(x = ~split) mean(unlist(x)),
                    exec = "plain"
                )

            expect_error(
                pip_run(p, lgr = NULL),
                "plain mode does not accept partitioned inputs"
            )
        })

        it("maps with two partitioned and one scalar input", {
            p <- pip_new() |>
                pip_add(
                    "left_raw",
                    \(
                        x = data.frame(
                            grp = c("a", "a", "b", "b"),
                            value = c(1, 3, 10, 20)
                        )
                    ) {
                        x
                    }
                ) |>
                pip_add(
                    "right_raw",
                    \(
                        x = data.frame(
                            grp = c("a", "a", "b", "b"),
                            value = c(2, 4, 6, 8)
                        )
                    ) {
                        x
                    }
                ) |>
                pip_add(
                    "left_split",
                    \(x = ~left_raw) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "right_split",
                    \(x = ~right_raw) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add("offset", \(x = 100) x) |>
                pip_add(
                    "combine",
                    \(a = ~left_split, b = ~right_split, c = ~offset) {
                        mean(a$value) + mean(b$value) + c
                    }
                )

            pip_run(p, lgr = NULL)

            out <- p[["pipenv"]][["data"]]$out[[6]]
            expect_true(inherits(out, "pipeflow_partitioned"))
            expect_equal(out[["a"]], 105)
            expect_equal(out[["b"]], 122)
        })

        it("includes failing partition key in mapped errors", {
            p <- pip_new() |>
                pip_add(
                    "load",
                    \(
                        x = data.frame(
                            grp = c("a", "a", "b", "b"),
                            value = c(1, 2, 3, 4)
                        )
                    ) {
                        x
                    }
                ) |>
                pip_add(
                    "split",
                    \(x = ~load) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "fragile",
                    \(x = ~split) {
                        key <- unique(x$grp)
                        if (identical(key, "b")) {
                            stop("boom")
                        }
                        sum(x$value)
                    }
                )

            expect_error(
                pip_run(p, lgr = NULL),
                "key 'b': boom"
            )

            e <- tryCatch(pip_run(p, lgr = NULL), pipeflow_step_error = \(e) e)
            expect_equal(conditionMessage(e), "step 'fragile': key 'b': boom")
            keyError <- e[["parent"]]
            expect_s3_class(
                keyError,
                c("pipeflow_key_error", "error", "condition"),
                exact = TRUE
            )
            expect_equal(keyError[["key"]], "b")
            expect_equal(conditionMessage(keyError), "key 'b': boom")
            expect_equal(conditionMessage(keyError[["parent"]]), "boom")
            expect_null(keyError[["call"]])
        })
    })

    describe("can be run with dynamic pipeline modifications", {
        it(
            paste(
                "supports modifying the pipeline at runtime and",
                "continues with the updated version"
            ),
            {
                pip <- pip_new("my-pipeline") |>
                    pip_add("init", function(xInit = 0) xInit) |>
                    pip_add("f1", function(x = ~init) x + 1) |>
                    pip_add(
                        "f2",
                        function(x = ~f1) {
                            if (x > 10) {
                                .self |>
                                    pip_replace("f3", function(x = ~f1) x * 3)
                                return(x / 2)
                            }
                            x + 2
                        }
                    ) |>
                    pip_add("f3", function(x = ~f2) x + 3)

                pip_run(pip, lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(0, 1, 3, 6))

                pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(15, 16, 16 / 2, 16 * 3))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )
            }
        )

        it(
            paste(
                "supports modifying the pipeline at runtime by",
                "a step that was replaced"
            ),
            {
                pip <- pip_new("my-pipeline") |>
                    pip_add("init", function(xInit = 0) xInit) |>
                    pip_add("f1", function(x = ~init) x + 1) |>
                    pip_add("f2", function(x = ~f1) x + 2) |>
                    pip_add("f3", function(x = ~f2) x + 3)

                pip |>
                    pip_replace(
                        "f2",
                        function(x = ~f1) {
                            if (x > 10) {
                                .self |>
                                    pip_replace("f3", function(x = ~f1) x * 3)
                                return(x / 2)
                            }
                            x + 2
                        }
                    )

                pip_run(pip, lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(0, 1, 3, 6))

                pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(15, 16, 16 / 2, 16 * 3))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )
            }
        )

        it(
            paste(
                "keeps .self rebound correctly for downstream steps",
                "that were copied during replacement"
            ),
            {
                pip <- pip_new("my-pipeline") |>
                    pip_add("init", function(xInit = 0) xInit) |>
                    pip_add("f1", function(x = ~init) x + 1) |>
                    pip_add("f2", function(x = ~f1) x + 2) |>
                    pip_add(
                        "f3",
                        function(x = ~f2) {
                            if (x > 10) {
                                .self |>
                                    pip_replace("f4", function(x = ~f1) x * 4)
                            }
                            x + 3
                        }
                    ) |>
                    pip_add("f4", function(x = ~f3) x + 4)

                pip |> pip_replace("f2", function(x = ~f1) x + 10)

                pip_run(pip, lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(0, 1, 11, 14, 4))
                expect_equal(
                    body(pip[["f4", "fun"]]),
                    body(function(x = ~f1) x * 4)
                )
            }
        )

        it(
            paste(
                "persists runtime self-modifications across runs",
                "without losing .self routing"
            ),
            {
                pip <- pip_new("my-pipeline") |>
                    pip_add("init", function(xInit = 0) xInit) |>
                    pip_add("f1", function(x = ~init) x + 1) |>
                    pip_add(
                        "f2",
                        function(x = ~f1) {
                            if (x > 10) {
                                .self |>
                                    pip_replace("f3", function(x = ~f1) x * 3)
                                return(x / 2)
                            }
                            x + 2
                        }
                    ) |>
                    pip_add("f3", function(x = ~f2) x + 3)

                pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(15, 16, 8, 48))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )

                pip |> pip_set_params(list(xInit = 20)) |> pip_run(lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(20, 21, 10.5, 63))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )
            }
        )

        it(
            paste(
                "applies runtime replacements when running only a view",
                "and updates steps outside the view"
            ),
            {
                pip <- pip_new("my-pipeline") |>
                    pip_add("init", function(xInit = 0) xInit) |>
                    pip_add("f1", function(x = ~init) x + 1) |>
                    pip_add(
                        "f2",
                        function(x = ~f1) {
                            if (x > 10) {
                                .self |>
                                    pip_replace("f3", function(x = ~f1) x * 3)
                                return(x / 2)
                            }
                            x + 2
                        }
                    ) |>
                    pip_add("f3", function(x = ~f2) x + 3) |>
                    pip_add("f4", function(x = ~f3) x + 1)

                pip_run(pip, lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(0, 1, 3, 6, 7))

                pip |> pip_set_params(list(xInit = 15))
                v <- pip_view(pip, step = c("f1", "f2"))
                pip_run(v, lgr = NULL, force = TRUE)

                expect_equal(unname(pip[["out"]]), list(15, 16, 8, NULL, 7))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )
                expect_equal(
                    pip[["pipenv"]][["data"]][step == "f4", state][[1]],
                    "outdated"
                )

                pip_run(pip, lgr = NULL)
                expect_equal(unname(pip[["out"]]), list(15, 16, 8, 48, 49))
            }
        )
    })

    it("inserts and removes steps at runtime as expected", {
        pip <- pip_new("my-pipeline") |>
            pip_add("init", function(xInit = 0) xInit) |>
            pip_add("f1", function(x = ~init) x + 1) |>
            pip_add(
                "f2",
                function(x = ~f1) {
                    if (x > 10) {
                        .self |>
                            pip_add(
                                "f2a",
                                function(x = ~f1) x + 21,
                                after = "f1",
                            ) |>
                            pip_add(
                                "f2b",
                                function(x = ~f2a) x + 22,
                                after = "f2a"
                            ) |>
                            pip_replace(
                                "f3",
                                function(x = ~f2b) x + 30
                            ) |>
                            pip_remove("f2")
                    }
                    x + 2
                }
            ) |>
            pip_add("f3", function(x = ~f2) x + 3)

        pip_set_params(pip, list(xInit = 11))
        pip_run(pip, lgr = NULL)
        expect_equal(unname(pip[["step"]]), c("init", "f1", "f2a", "f2b", "f3"))
        expect_equal(
            unname(pip[["state"]]),
            c("done", "done", "new", "done", "new")
        )
        expect_equal(unname(pip[["out"]]), list(11, 12, NULL, NULL + 22, NULL))

        pip_set_params(pip, list(xInit = 11))
        pip_run(pip, lgr = NULL)

        expect_equal(unname(pip[["out"]]), list(11, 12, 33, 55, 85))
    })

    it("can insert and remove steps at runtime", {
        test_pip <- function() {
            pip_new("my-pipeline") |>
                pip_add("init", function(xInit = 0) xInit) |>
                pip_add("f1", function(x = ~init) x + 1) |>
                pip_add(
                    "f2",
                    function(x = ~f1) {
                        if (x > 10) {
                            .self |>
                                pip_add(
                                    "f2a",
                                    function(x = ~f1) x + 21,
                                    after = "f1",
                                ) |>
                                pip_add(
                                    "f2b",
                                    function(x = ~f2a) x + 22,
                                    after = "f2a"
                                ) |>
                                pip_replace(
                                    "f3",
                                    function(x = ~f2b) x + 30
                                ) |>
                                pip_remove("f2")

                            .self$restart()
                        }

                        x + 2
                    }
                ) |>
                pip_add("f3", function(x = ~f2) x + 3)
        }

        pip <- test_pip()
        pip_set_params(pip, list(xInit = 11)) |> pip_run(lgr = NULL)
        expect_equal(unname(pip[["step"]]), c("init", "f1", "f2a", "f2b", "f3"))
        expect_equal(unname(pip[["state"]]), rep("done", 5))
        expect_equal(unname(pip[["out"]]), list(11, 12, 33, 55, 85))
    })

    it("keeps .self bound to the same pipeline across a restart", {
        captured <- list()
        p <- pip_new("self-bound") |>
            pip_add("init", function(xInit = 0) xInit) |>
            pip_add("f1", function(x = ~init) x + 1) |>
            pip_add("f2", function(x = ~f1) {
                captured[[1]] <<- .self
                .self$restart()
                x + 2
            })

        pip_run(p, lgr = NULL)

        expect_true(identical(captured[[1]], p))
    })

    it("limits restarts with times while the pipeline is self-modifying", {
        count <- 0L
        p <- pip_new("mod-times") |>
            pip_add("init", function(xInit = 0) xInit) |>
            pip_add("f1", function(x = ~init) x + 1) |>
            pip_add(
                "f2",
                function(x = ~f1) {
                    count <<- count + 1L
                    if (x > 10 && count < 3L) {
                        .self |>
                            pip_replace("f3", function(x = ~f1) x * 3)
                        .self$restart(times = 2L)
                    }
                    x + 2
                }
            ) |>
            pip_add("f3", function(x = ~f2) x + 3)

        p |> pip_set_params(list(xInit = 11)) |> pip_run(lgr = NULL)

        expect_equal(count, 3L)
        expect_equal(unname(p[["out"]]), list(11, 12, 14, 36))
        expect_equal(unname(p[["state"]]), rep("done", 4))
        expect_equal(
            body(p[["fun"]][[4]]),
            body(function(x = ~f1) x * 3)
        )
    })

    it("force = TRUE re-executes all steps regardless of state", {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        env <- p[["pipenv"]]
        env[["data"]][["out"]] <- list(99, 99, 99, 99)
        pip_run(p, lgr = NULL, force = TRUE)

        expect_equal(unname(p[["out"]]), list(1, 2, 2, "blabla"))
        expect_equal(unname(p[["state"]]), rep("done", 4))
    })

    it("calls the progress callback before each step", {
        p <- test_pip()
        calls <- character(0)

        progress <- function(value, detail) {
            calls <<- c(calls, detail)
        }

        pip_run(p, lgr = NULL, progress = progress)
        expect_equal(calls, c("load_raw", "fit_model", "eval_model", "bla_bla"))
    })

    it("skips locked steps during run", {
        p <- test_pip()
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "locked",
            value = TRUE
        )

        pip_run(p, lgr = NULL)
        expect_equal(
            unname(p[["state"]]),
            c("done", "new", "done", "done")
        )
        expect_equal(
            unname(p[["out"]]),
            list(1, NULL, NULL, "blabla")
        )
    })

    it("skips locked steps even with force = TRUE", {
        p <- test_pip()
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "locked",
            value = TRUE
        )
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "out",
            value = list(99)
        )

        pip_run(p, lgr = NULL, force = TRUE)
        expect_equal(p[["out"]][[2]], 99)
    })

    it("forwards warnings and messages from steps to the logger", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                warning("careful now")
                message("hey there")
                x
            })

        logs <- character(0)
        lgr <- function(level, msg) logs <<- c(logs, paste(level, msg))
        suppressWarnings(suppressMessages(pip_run(p, lgr = lgr)))

        expect_true(any(grepl("warn careful now", logs)))
        expect_true(any(grepl("info hey there", logs)))
    })

    describe("continuing after errors", {
        # 'b' and 'd' depend on the failing step 'a', 'c' does not
        test_pip <- function() {
            pip_new("p") |>
                pip_add("data", \(n = 3) n) |>
                pip_add("a", \(x = ~data) stop("boom")) |>
                pip_add("b", \(x = ~a) x + 1) |>
                pip_add("c", \(x = ~data) x * 2) |>
                pip_add("d", \(x = ~b) x + 1)
        }
        run_continue <- function(x, ...) {
            suppressWarnings(
                pip_run(x, lgr = NULL, on_error = "continue", ...),
                classes = "pipeflow_run_failed"
            )
        }

        it("signals invalid on_error values", {
            expect_error(
                pip_run(test_pip(), lgr = NULL, on_error = "nope"),
                "should be one of"
            )
        })

        it("runs all steps that do not depend on a failed step", {
            p <- test_pip()
            run_continue(p)

            expect_equal(
                unname(p[["state"]]),
                c("done", "failed", "new", "done", "new")
            )
            expect_equal(p[["c", "out"]], 6)
            expect_null(p[["b", "out"]])
        })

        it("still aborts at the first failed step by default", {
            p <- test_pip()
            expect_error(pip_run(p, lgr = NULL), "boom")

            expect_equal(
                unname(p[["state"]]),
                c("done", "failed", "new", "new", "new")
            )
            expect_null(p[["a", "out"]])
            expect_equal(get_run_state(p), "failed")
        })

        it("keeps the original condition as output of the failed step", {
            p <- pip_new() |>
                pip_add("a", \(x = 1) {
                    stop(structure(
                        class = c("my_error", "error", "condition"),
                        list(message = "custom", call = NULL)
                    ))
                })
            run_continue(p)

            out <- p[["a", "out"]]
            expect_s3_class(out, "my_error")
            expect_false(inherits(out, "pipeflow_step_error"))
            expect_equal(conditionMessage(out), "custom")
        })

        it("does not repeat the step name in failures and the warning", {
            skip_if_not_installed("rlang")
            p <- pip_new("p") |>
                pip_add("a", \(x = 1) {
                    rlang::abort("boom", class = "my_error")
                }) |>
                pip_add("doc", \(x = ~ try(a)) x)
            w <- tryCatch(
                pip_run(p, lgr = NULL, on_error = "continue"),
                pipeflow_run_failed = \(w) w
            )

            expect_s3_class(p[["a", "out"]], "my_error")
            expect_false(inherits(p[["a", "out"]], "pipeflow_step_error"))
            expect_equal(
                conditionMessage(p[["doc", "out"]]),
                "step 'a' failed: boom"
            )
            expect_s3_class(w[["failed"]][["a"]], "my_error")
            expect_match(conditionMessage(w), "'a': boom", fixed = TRUE)
            expect_no_match(conditionMessage(w), "step 'a': ", fixed = TRUE)
        })

        it("sets the run state to 'continued' and signals a warning", {
            p <- test_pip()
            w <- tryCatch(
                pip_run(p, lgr = NULL, on_error = "continue"),
                pipeflow_run_failed = \(w) w
            )

            expect_s3_class(
                w,
                c("pipeflow_run_failed", "warning", "condition"),
                exact = TRUE
            )
            expect_equal(names(w[["failed"]]), "a")
            expect_equal(conditionMessage(w[["failed"]][["a"]]), "boom")
            expect_equal(w[["skipped"]], c("b", "d"))
            expect_match(
                conditionMessage(w),
                "Run of pipeline 'p' continued after 1 failed step:",
                fixed = TRUE
            )
            expect_match(conditionMessage(w), "'a': boom", fixed = TRUE)
            expect_match(
                conditionMessage(w),
                "Steps not run because an input failed: 'b', 'd'",
                fixed = TRUE
            )
            expect_null(w[["call"]])
            expect_equal(get_run_state(p), "continued")
        })

        it("signals the warning after all states and outputs are stored", {
            p <- test_pip()
            seen <- NULL
            withCallingHandlers(
                pip_run(p, lgr = NULL, on_error = "continue"),
                pipeflow_run_failed = function(w) {
                    seen <<- list(state = p[["state"]], c = p[["c", "out"]])
                    invokeRestart("muffleWarning")
                }
            )

            expect_equal(seen[["state"]], p[["state"]])
            expect_equal(seen[["c"]], 6)
        })

        it("neither warns nor changes the run state if nothing fails", {
            p <- pip_new() |>
                pip_add("a", \(x = 1) x)

            expect_no_warning(pip_run(p, lgr = NULL, on_error = "continue"))
            expect_equal(get_run_state(p), "ready")
        })

        it("passes failure objects to arguments with try() references", {
            p <- test_pip() |>
                pip_add(
                    "doc",
                    \(c = ~c, a = ~ try(a), d = ~ try(d)) {
                        list(c = c, a = a, d = d)
                    }
                )
            run_continue(p)

            expect_equal(p[["doc", "state"]], "done")
            out <- p[["doc", "out"]]
            expect_equal(out[["c"]], 6)

            a <- out[["a"]]
            expect_s3_class(
                a,
                c("pipeflow_failure", "error", "condition"),
                exact = TRUE
            )
            expect_equal(conditionMessage(a), "step 'a' failed: boom")
            expect_equal(a[["step"]], "a")
            expect_equal(a[["failed_step"]], "a")
            expect_equal(conditionMessage(a[["parent"]]), "boom")
            expect_null(a[["call"]])

            d <- out[["d"]]
            expect_s3_class(d, "pipeflow_failure")
            expect_equal(
                conditionMessage(d),
                "step 'd' was not run: step 'a' failed: boom"
            )
            expect_equal(d[["step"]], "d")
            expect_equal(d[["failed_step"]], "a")
            expect_identical(d[["parent"]], a[["parent"]])
        })

        it("does not run a step if any of its failed inputs is not allowed", {
            p <- test_pip() |>
                pip_add("doc", \(a = ~ try(a), d = ~d) list(a = a, d = d)) |>
                pip_add("final", \(x = ~doc) x)
            w <- tryCatch(
                pip_run(p, lgr = NULL, on_error = "continue"),
                pipeflow_run_failed = \(w) w
            )

            expect_equal(
                unname(p[["state"]]),
                c("done", "failed", "new", "done", "new", "new", "new")
            )
            expect_equal(w[["skipped"]], c("b", "d", "doc", "final"))
        })

        it("marks skipped done steps outdated and keeps new ones new", {
            fail <- FALSE
            p <- pip_new() |>
                pip_add("a", \(x = 1) if (fail) stop("boom") else x) |>
                pip_add("b", \(x = ~a) x + 1) |>
                pip_add("c", \(x = 1) x) |>
                pip_add("d", \(x = ~b, y = ~c) x + y)
            pip_run(p, lgr = NULL)
            pip_add(p, "e", \(x = ~b) x)

            fail <- TRUE
            p[["a", "state"]] <- "outdated"
            run_continue(p)

            expect_equal(
                unname(p[["state"]]),
                c("failed", "outdated", "done", "outdated", "new")
            )
            expect_equal(p[["b", "out"]], 2)
        })

        it("retries failed steps and re-runs the steps given failures", {
            fail <- TRUE
            p <- pip_new() |>
                pip_add("a", \(x = 1) if (fail) stop("boom") else x) |>
                pip_add("b", \(x = ~a) x + 1) |>
                pip_add(
                    "doc",
                    \(x = ~ try(b)) {
                        if (inherits(x, "pipeflow_failure")) NA else x
                    }
                ) |>
                pip_add("final", \(x = ~doc) x * 10)
            run_continue(p)
            expect_equal(
                unname(p[["state"]]),
                c("failed", "new", "done", "done")
            )
            expect_equal(p[["final", "out"]], NA_real_)

            fail <- FALSE
            run_continue(p)

            expect_equal(unname(p[["state"]]), rep("done", 4))
            expect_equal(p[["final", "out"]], 20)
            expect_equal(get_run_state(p), "ready")
        })

        it("re-runs steps given failures if the step fails again", {
            msg <- "first"
            p <- pip_new() |>
                pip_add("a", \(x = 1) stop(msg)) |>
                pip_add("doc", \(x = ~ try(a)) conditionMessage(x))
            run_continue(p)
            expect_equal(p[["doc", "out"]], "step 'a' failed: first")

            msg <- "second"
            run_continue(p)
            expect_equal(p[["doc", "out"]], "step 'a' failed: second")
        })

        it("keeps the key prefix of failed partitions", {
            calc <- function(x = ~parts) if (x > 1) stop("too big") else x
            p <- pip_new() |>
                pip_add("parts", \(x = 1) list(a = 1, b = 2), exec = "split") |>
                pip_add("calc", calc) |>
                pip_add("doc", \(x = ~ try(calc)) x)
            run_continue(p)

            f <- p[["doc", "out"]]
            expect_s3_class(f, "pipeflow_failure")
            expect_equal(
                conditionMessage(f),
                "step 'calc' failed: key 'b': too big"
            )
            expect_equal(conditionMessage(f[["parent"]]), "key 'b': too big")

            out <- p[["calc", "out"]]
            expect_s3_class(out, "pipeflow_key_error")
            expect_equal(out[["key"]], "b")
            expect_equal(conditionMessage(out[["parent"]]), "too big")
            expect_identical(f[["parent"]], out)
        })

        it("does not treat locked steps with failed inputs as failed", {
            fail <- FALSE
            p <- pip_new() |>
                pip_add("a", \(x = 1) if (fail) stop("boom") else x) |>
                pip_add("locked", \(x = ~a) x + 1) |>
                pip_add("after", \(x = ~locked) x * 10)
            pip_run(p, lgr = NULL)
            pip_lock(p["locked"])

            fail <- TRUE
            run_continue(p, force = TRUE)

            expect_equal(
                unname(p[["state"]]),
                c("failed", "done", "done")
            )
            expect_equal(p[["after", "out"]], 20)
        })

        it("continues in view runs", {
            p <- test_pip() |>
                pip_add("doc", \(c = ~c, b = ~ try(b)) b)
            run_continue(pip_view(p, step = "doc"))

            expect_equal(
                unname(p[["state"]]),
                c("done", "failed", "new", "done", "new", "done")
            )
            expect_s3_class(p[["doc", "out"]], "pipeflow_failure")
        })

        it("keeps continuing after a restart", {
            cnt <- new.env()
            cnt[["n"]] <- 0L
            p <- pip_new() |>
                pip_add("s1", function(x = 1) {
                    cnt[["n"]] <- cnt[["n"]] + 1L
                    if (cnt[["n"]] == 1L) {
                        .self$restart()
                    }
                    x
                }) |>
                pip_add("s2", \(x = ~s1) stop("boom")) |>
                pip_add("s3", \(x = 1) x)

            expect_warning(
                pip_run(p, lgr = NULL, on_error = "continue"),
                class = "pipeflow_run_failed"
            )
            expect_equal(cnt[["n"]], 2L)
            expect_equal(unname(p[["state"]]), c("done", "failed", "done"))
            expect_equal(get_run_state(p), "continued")
        })

        it("logs skipped steps and the summary of the failures", {
            p <- test_pip()
            logs <- character(0)
            lgr <- function(level, msg) logs <<- c(logs, paste(level, msg))
            suppressWarnings(pip_run(p, lgr = lgr, on_error = "continue"))

            expect_true(any(grepl("error boom", logs)))
            expect_true(any(grepl(
                "Step 3/5 b - skipping step, input 'x' failed",
                logs,
                fixed = TRUE
            )))
            expect_true(any(grepl(
                "warn Run of pipeline 'p' continued after 1 failed step",
                logs,
                fixed = TRUE
            )))
        })
    })
})


describe("pip_reset", {
    it("resets states and outputs of all steps", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x + 1)
        pip_run(p, lgr = NULL)

        expect_equal(unname(p[["state"]]), c("done", "done"))

        pip_reset(p)

        expect_equal(unname(p[["state"]]), c("new", "new"))
        expect_true(all(vapply(
            p[["out"]],
            is.null,
            logical(1)
        )))
        expect_equal(get_run_state(p), "ready")
    })

    it("keeps params and tags but clears outputs", {
        p <- pip_new() |>
            pip_add("a", \(x = 1, n = 5) x, tags = "io")
        pip_run(p, lgr = NULL)
        pip_set_params(p, list(n = 10))

        pip_reset(p)

        expect_equal(p[["params"]][[1]][["n"]], 10)
        expect_equal(p[["tags"]][[1]], "io")
    })

    it("resets only the steps covered by a view", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x + 1)
        pip_run(p, lgr = NULL)

        v <- pip_view(p, step = "b")
        pip_reset(v)

        expect_equal(unname(p[["state"]]), c("done", "new"))
    })

    it("allows re-running the pipeline from scratch after reset", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x + 1)
        pip_run(p, lgr = NULL)

        pip_reset(p)
        pip_run(p, lgr = NULL)

        expect_equal(unname(p[["out"]]), list(1, 2))
        expect_equal(get_run_state(p), "ready")
    })

    it("skips locked steps, keeping their state and output", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x + 1)
        pip_run(p, lgr = NULL)

        pip_lock(pip_view(p, step = "b"))
        pip_reset(p)

        expect_equal(unname(p[["state"]]), c("new", "done"))
        expect_null(p[["out"]][[1]])
        expect_equal(p[["out"]][[2]], 2)
        expect_equal(unname(p[["locked"]]), c(FALSE, TRUE))
    })

    it("warns when all selected steps are locked", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x)
        pip_run(p, lgr = NULL)

        pip_lock(p)

        expect_message(pip_reset(p), "all selected steps are locked")
        expect_equal(unname(p[["state"]]), "done")
        expect_equal(p[["out"]][[1]], 1)
    })
})

describe("pip_set_params", {
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1, data = data.frame(a = 1:2)) x) |>
            pip_add("s2", \(x = ~s1, y = 2) x + y) |>
            pip_add("s3", \(x = 1, z = 3) x + z) |>
            pip_add("s4", \(a = ~s1, b = ~s3) a + b)
    }

    it("sets independent parameters in a pipeline", {
        p <- test_pip()
        params <- list(x = 11, z = 33, data = data.frame(b = 3:4))

        pip_set_params(p, params = params)
        after <- p[["params"]]
        expect_equal(after[[1]][["x"]], 11)
        expect_equal(after[[1]][["data"]], data.frame(b = 3:4))
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["x"]], 11)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("can be used with pip or view", {
        p <- test_pip()

        res <- pip_set_params(p, params = list(x = 11, y = 22))
        expect_true(inherits(res, "pipeflow"))

        v <- pip_view(p, step = "s2")
        res <- pip_set_params(v, params = list(y = 22))
        expect_true(.is_pipeflow_view(res))
    })

    it("ignores locked steps", {
        p <- test_pip()
        env <- p[["pipenv"]]
        env[["data"]][["locked"]][[1]] <- TRUE
        pip_set_params(p, params = list(x = 99))
        after <- p[["params"]]
        expect_equal(after[[1]][["x"]], 1)
    })

    it("does not warn for parameters defined only in locked steps", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(y = 2) y)
        pip_lock(p["s1"])

        expect_no_warning(pip_set_params(p, params = list(x = 99, y = 22)))
        after <- p[["params"]]
        expect_equal(after[[1]][["x"]], 1)
        expect_equal(after[[2]][["y"]], 22)
    })

    it("warns for unused parameters", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1, ...) x)

        expect_warning(
            pip_set_params(p, params = list(foo = 1)),
            "Trying to set parameters not defined in the target: foo"
        )
    })

    it("ignores unused parameters silently if unknown = 'ignore'", {
        p <- test_pip()
        expect_no_warning(
            pip_set_params(p, params = list(x = 5, foo = 1), unknown = "ignore")
        )
        expect_equal(p[["params"]][[1]][["x"]], 5)
    })

    it("fails for unused parameters if unknown = 'error'", {
        p <- test_pip()
        expect_error(
            pip_set_params(p, params = list(foo = 1), unknown = "error"),
            "Trying to set parameters not defined in the target: foo"
        )
    })

    it("changes nothing if unknown = 'error' fails", {
        p <- test_pip() |> pip_run(lgr = NULL)
        params <- p[["params"]]
        expect_error(
            pip_set_params(p, params = list(x = 5, foo = 1), unknown = "error"),
            "foo"
        )
        expect_equal(p[["params"]], params)
        expect_equal(unname(p[["state"]]), rep("done", 4))
    })

    it("checks unknown parameters against the selected view", {
        p <- test_pip()
        v <- pip_view(p, step = "s3")
        expect_error(
            pip_set_params(v, params = list(y = 22), unknown = "error"),
            "Trying to set parameters not defined in the target: y"
        )
    })

    it("fails for unused parameters if all steps are locked", {
        p <- test_pip() |> pip_lock()
        expect_no_message(
            expect_error(
                pip_set_params(p, params = list(foo = 1), unknown = "error"),
                "not defined in the target: foo"
            )
        )
    })

    it("does not fail if unknown = 'error' and all params are known", {
        p <- test_pip()
        expect_no_error(
            pip_set_params(p, params = list(x = 5, y = 3), unknown = "error")
        )
        expect_equal(p[["params"]][[2]][["y"]], 3)
    })

    it("fails for invalid values of unknown", {
        p <- test_pip()
        expect_error(
            pip_set_params(p, params = list(x = 5), unknown = "foo"),
            "'arg' should be one of"
        )
    })

    it("warns by default for unused parameters via the $ method", {
        p <- test_pip()
        expect_warning(
            p$set_params(params = list(foo = 1)),
            "not defined in the target: foo"
        )
        expect_error(
            p$set_params(params = list(foo = 1), unknown = "error"),
            "not defined in the target: foo"
        )
    })

    it("sets parameters only within the selected view", {
        p <- test_pip()

        v <- pip_view(p, step = "s3")
        expect_warning(
            pip_set_params(v, params = list(z = 33, x = 11, y = 22)),
            "Trying to set parameters not defined in the target: y"
        )
        after <- p[["params"]]

        expect_equal(after[[1]][["x"]], 1)
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["x"]], 11)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("marks changed and dependent downstream steps as 'outdated'", {
        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(x = 5))
        expect_equal(
            unname(p[["state"]]),
            c("outdated", "outdated", "outdated", "outdated")
        )

        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(y = 5))
        expect_equal(
            unname(p[["state"]]),
            c("done", "outdated", "done", "done")
        )

        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(z = 5))
        expect_equal(
            unname(p[["state"]]),
            c("done", "done", "outdated", "outdated")
        )
    })

    it("leaves steps that have never run as 'new'", {
        p <- test_pip()
        pip_set_params(p, params = list(x = 5))
        expect_equal(unname(p[["state"]]), rep("new", 4))

        p <- test_pip() |> pip_run(lgr = NULL)
        pip_reset(p[3:4])
        pip_set_params(p, params = list(x = 5))
        expect_equal(
            unname(p[["state"]]),
            c("outdated", "outdated", "new", "new")
        )
    })

    it("does not outdate steps if the values are identical", {
        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(x = 1, data = data.frame(a = 1:2)))
        expect_equal(unname(p[["state"]]), rep("done", 4))
    })

    it("outdates steps with identical values if force = TRUE", {
        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(y = 2), force = TRUE)
        expect_equal(
            unname(p[["state"]]),
            c("done", "outdated", "done", "done")
        )
        pip_set_params(p, params = list(x = 1), force = TRUE)
        expect_equal(unname(p[["state"]]), rep("outdated", 4))
    })

    it("refreshes steps with environments modified in place if forced", {
        e <- new.env()
        p <- pip_new() |> pip_add("s", \(e = NULL) e)
        pip_set_params(p, params = list(e = e))
        pip_run(p, lgr = NULL)
        e$a <- 1
        pip_set_params(p, params = list(e = e))
        expect_equal(p[["state"]][[1]], "done")
        pip_set_params(p, params = list(e = e), force = TRUE)
        expect_equal(p[["state"]][[1]], "outdated")
    })

    it("keeps steps that have never run 'new' if force = TRUE", {
        p <- test_pip()
        pip_set_params(p, params = list(x = 1), force = TRUE)
        expect_equal(unname(p[["state"]]), rep("new", 4))
    })

    it("fails if force is not TRUE or FALSE", {
        p <- test_pip()
        expect_error(
            pip_set_params(p, params = list(x = 1), force = NA),
            "force must be TRUE or FALSE"
        )
        expect_error(
            pip_set_params(p, params = list(x = 1), force = "yes"),
            "force must be TRUE or FALSE"
        )
    })

    it("outdates only the steps whose values change", {
        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(x = 1, y = 2, z = 5))
        expect_equal(
            unname(p[["state"]]),
            c("done", "done", "outdated", "outdated")
        )
        expect_equal(p[["params"]][[3]][["z"]], 5)
    })

    it("outdates a step if one of its values changes", {
        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(x = 1, data = data.frame(a = 3:4)))
        expect_equal(
            unname(p[["state"]]),
            c("outdated", "outdated", "done", "outdated")
        )
        expect_equal(p[["params"]][[1]][["data"]], data.frame(a = 3:4))
    })

    it("compares the values of each step separately", {
        p <- test_pip()
        pip_set_params(pip_view(p, step = "s3"), params = list(x = 5))
        pip_run(p, lgr = NULL)

        pip_set_params(p, params = list(x = 1))
        expect_equal(
            unname(p[["state"]]),
            c("done", "done", "outdated", "outdated")
        )
        expect_equal(p[["params"]][[3]][["x"]], 1)
    })

    it("compares values with identical()", {
        p <- test_pip() |> pip_run(lgr = NULL)
        pip_set_params(p, params = list(y = 2L))
        expect_equal(
            unname(p[["state"]]),
            c("done", "outdated", "done", "done")
        )
    })

    it("no-ops on empty params list", {
        p <- test_pip()
        pip_set_params(p, params = list())
        expect_equal(unname(p[["state"]]), rep("new", 4))
    })

    it("no-ops if all considered steps are locked, with a warning", {
        p <- test_pip() |> pip_lock()
        params <- pip_get_params(p)

        expect_message(
            pip_set_params(p, params = list(x = 5)),
            "all selected steps are locked"
        )
        expect_equal(pip_get_params(p), params) # verify that nothing changed
    })

    it("warns for unused parameters if all considered steps are locked", {
        p <- test_pip() |> pip_lock()

        expect_message(
            expect_warning(
                pip_set_params(p, params = list(x = 5, foo = 1)),
                "Trying to set parameters not defined in the target: foo"
            ),
            "all selected steps are locked"
        )
        expect_equal(p[["params"]][[1]][["x"]], 1)
    })

    it("signals unnamed params", {
        p <- test_pip()
        expect_error(
            pip_set_params(p, params = list(1, 2)),
            "All parameters must be named"
        )
    })

    it("signals non-list params", {
        p <- test_pip()
        expect_error(
            pip_set_params(p, params = c(a = 1, b = 2)),
            "params must be a list"
        )
    })
})


tag_lock_test_pip <- function() {
    pip_new("pipe") |>
        pip_add("s1", \(x = 1) x, tags = c("init", "daily")) |>
        pip_add("s2", \(x = ~s1) x + 1, tags = c("daily", "model")) |>
        pip_add("s3", \(x = ~s2) x + 1, tags = "report")
}


describe("pip_tag", {
    it("signals invalid inputs", {
        p <- tag_lock_test_pip()
        expect_error(pip_tag(1), "x must be a pipeflow pip or view")
        expect_error(pip_tag(p, tags = 1), "tags must be a character vector")
    })

    it("adds tags to all selected steps while preserving existing tags", {
        p <- tag_lock_test_pip()
        pip_tag(p, tags = c("daily", "core"))

        expect_equal(
            p[["tags"]][[1]],
            c("init", "daily", "core")
        )
        expect_equal(
            p[["tags"]][[2]],
            c("daily", "model", "core")
        )
        expect_equal(
            p[["tags"]][[3]],
            c("report", "daily", "core")
        )
    })

    it("updates only rows in a view and skips locked steps", {
        p <- tag_lock_test_pip()
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "locked",
            value = TRUE
        )
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "tags",
            value = list("keep")
        )

        v <- pip_view(p, step = c("s2", "s3"))
        pip_tag(v, tags = "view")

        expect_equal(p[["tags"]][[1]], c("init", "daily"))
        expect_equal(p[["tags"]][[2]], "keep")
        expect_equal(
            p[["tags"]][[3]],
            c("report", "view")
        )
    })
})


describe("pip_untag", {
    it("signals invalid inputs", {
        p <- tag_lock_test_pip()
        expect_error(pip_untag(1), "x must be a pipeflow pip or view")
        expect_error(pip_untag(p, tags = 1), "tags must be a character vector")
    })

    it("removes tags from all selected steps", {
        p <- tag_lock_test_pip()
        pip_untag(p, tags = c("daily", "report"))

        expect_equal(p[["tags"]][[1]], "init")
        expect_equal(p[["tags"]][[2]], "model")
        expect_equal(p[["tags"]][[3]], character(0))
    })

    it("updates only rows in a view and skips locked steps", {
        p <- tag_lock_test_pip()
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "locked",
            value = TRUE
        )
        data.table::set(
            p[["pipenv"]][["data"]],
            i = 2,
            j = "tags",
            value = list(c("daily", "model"))
        )

        v <- pip_view(p, step = c("s2", "s3"))
        pip_untag(v, tags = "daily")

        expect_equal(p[["tags"]][[1]], c("init", "daily"))
        expect_equal(
            p[["tags"]][[2]],
            c("daily", "model")
        )
        expect_equal(p[["tags"]][[3]], "report")
    })
})


describe("pip_lock", {
    it("signals invalid input", {
        expect_error(pip_lock(1), "x must be a pipeflow pip or view")
    })

    it("locks selected steps", {
        p <- tag_lock_test_pip()
        pip_lock(p)
        expect_true(all(p[["locked"]]))
    })

    it("locks only rows covered by a view", {
        p <- tag_lock_test_pip()
        v <- pip_view(p, step = c("s2", "s3"))
        pip_lock(v)

        expect_false(p[["locked"]][[1]])
        expect_true(p[["locked"]][[2]])
        expect_true(p[["locked"]][[3]])
    })
})


describe("pip_unlock", {
    it("signals invalid input", {
        expect_error(pip_unlock(1), "x must be a pipeflow pip or view")
    })

    it("unlocks selected steps", {
        p <- tag_lock_test_pip()
        env <- p[["pipenv"]]
        env[["data"]][["locked"]] <- rep(TRUE, nrow(env[["data"]]))

        pip_unlock(p)
        expect_false(any(env[["data"]][["locked"]]))
    })

    it("unlocks only rows covered by a view", {
        p <- tag_lock_test_pip()
        data.table::set(
            p[["pipenv"]][["data"]],
            j = "locked",
            value = rep(TRUE, nrow(p[["pipenv"]][["data"]]))
        )

        v <- pip_view(p, step = c("s2", "s3"))
        pip_unlock(v)

        expect_true(p[["locked"]][[1]])
        expect_false(p[["locked"]][[2]])
        expect_false(p[["locked"]][[3]])
    })
})


describe("pip_view", {
    it("returns a view object with the expected structure", {
        p <- pip_new("test_pipeline")
        pip_add(p, "load", \(x = 1) x, tags = "io")

        v <- pip_view(p)
        expect_true(.is_pipeflow_view(v))
        expect_true("view" %in% names(v))
        expect_identical(v[["pipenv"]][["data"]], p[["pipenv"]][["data"]])
        expect_identical(v[["name"]], "test_pipeline view")
    })

    it("can filter by multiple properties with fixed matching", {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, tags = "io")
        pip_add(p, "fit", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "eval", \(x = ~fit) x, tags = "model")

        p[["pipenv"]][["data"]][2, state := "done"]

        v <- pip_view(p, tags = "model", state = "done")

        expect_equal(v[["view"]], 2L)
    })

    it("can filter by depends column with multiple entries", {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, tags = "io")
        pip_add(p, "fit", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "eval", \(x = ~load, y = ~fit) x, tags = "model")

        v <- pip_view(p, depends = "fit", tags = "model")
        v
        expect_equal(v[["view"]], 3L)
    })

    it("can filter by tags", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~ -1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["view"]], c(1L, 3L))
    })

    it("can filter by step names", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, tags = "g1")
        pip_add(p, "a2", \(x = ~ -1) x, tags = "g2")
        pip_add(p, "a3", \(x = ~ -1) x, tags = "g2")

        v <- pip_view(p, step = c("a1", "a3"))
        expect_equal(v[["view"]], c(1L, 3L))

        v <- pip_view(p, step = c("a1", "a2"), tags = "g2")
        expect_equal(v[["view"]], 2L)
    })

    it("can filter by parameter names", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1, n = 5) x, tags = "g1")
        pip_add(p, "s2", \(y = ~ -1, z = 2) y, tags = "g2")

        v <- pip_view(p, params = "n")
        expect_equal(v[["view"]], 1L)

        v <- pip_view(p, params = c("x", "z"))
        expect_equal(v[["view"]], c(1L, 2L))
    })

    it("matches dependency parameters via the params filter", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(y = ~s1) y)

        v <- pip_view(p, params = "y")
        expect_equal(v[["view"]], 2L)
    })

    it("can filter by exec mode", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(x = 2) x, exec = "split")

        v <- pip_view(p, exec = "split")
        expect_equal(v[["view"]], 2L)
    })

    it("can filter by regex when fixed is FALSE", {
        p <- pip_new()
        pip_add(p, "data", \(x = 1) x)
        pip_add(p, "fit_model", \(x = ~ -1) x + 1)
        pip_add(p, "eval_model", \(x = ~data, y = ~fit_model) x)

        v <- pip_view(p, step = "_model$", fixed = FALSE)
        expect_equal(v[["view"]], c(2L, 3L))
    })

    it("intersects multiple filters", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, tags = "g1")
        pip_add(p, "a2", \(x = ~ -1) x, tags = "g2")
        pip_add(p, "a3", \(x = ~ -1) x, tags = "g2")

        v <- pip_view(p, step = c("a1", "a2"), tags = "g2")
        expect_equal(v[["view"]], 2L)
    })

    it("unions multiple filters with join = union", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, tags = "g1")
        pip_add(p, "a2", \(x = ~ -1) x, tags = "g2")
        pip_add(p, "a3", \(x = ~ -1) x, tags = "g2")

        v <- pip_view(p, step = "a1", tags = "g2", join = "union")
        expect_equal(v[["view"]], c(1L, 2L, 3L))
    })

    it("defaults to intersect and allows partial join names", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, tags = "g1")
        pip_add(p, "a2", \(x = ~ -1) x, tags = "g2")

        expect_equal(
            pip_view(p, step = "a1", tags = "g2")[["view"]],
            integer(0)
        )
        expect_equal(
            pip_view(p, step = "a1", tags = "g2", join = "uni")[["view"]],
            c(1L, 2L)
        )
    })

    it("signals invalid join values", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x)

        expect_error(
            pip_view(p, step = "a1", join = "bogus"),
            "should be one of"
        )
    })

    it("can be called on views to further subset rows", {
        p <- pip_new("test_pipeline")
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~ -1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["view"]], c(1L, 3L))
        expect_equal(v[["name"]], "test_pipeline view")

        v2 <- pip_view(v, tags = "report")
        expect_equal(v2[["view"]], 3L)
        expect_equal(v2[["name"]], "test_pipeline view view")
    })

    it("signals invalid filter names", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, not_a_column = "x"),
            "Invalid filter name"
        )
    })

    it("signals non-character filter values", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, step = 2L),
            "filter 'step' must be a character vector, not integer"
        )
        expect_error(
            pip_view(p, tags = 1L),
            "filter 'tags' must be a character vector, not integer"
        )
        expect_error(
            pip_view(p, params = TRUE),
            "filter 'params' must be a character vector, not logical"
        )
    })
})


describe("benchmarking", {
    skip("benchmarking tests are skipped by default")
    v <- c("hello", "world")
    w <- c("hello", "this", "is", "my", "world")
    grepv(pattern = v, x = w)
    `%chin%` <- data.table::`%chin%`

    N <- 1e3
    u <- as.character(as.hexmode(1:10000))
    y <- sample(u, N, replace = TRUE)
    x <- sample(u, 100)
    system.time(x %in% y)
    system.time(x %chin% y)

    microbenchmark(
        y %in% x,
        y %chin% x,
        times = 1000
    )

    # Please type 'example(chmatch)' to run this and see timings on your machine

    microbenchmark(x %in% y, x %chin% y, times = 100)
    system.time(a <- x %in% y) #  4.5s
    system.time(b <- x %chin% y) #  1.7s
    identical(a, b)

    # Different example with more unique strings ...
    u <- as.character(as.hexmode(1:(N / 10)))
    y <- sample(u, N, replace = TRUE)
    x <- sample(u, N, replace = TRUE)
    system.time(a <- match(x, y)) # 46s
    system.time(b <- chmatch(x, y)) # 16s
    identical(a, b)
})
