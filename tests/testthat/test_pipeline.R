describe(".empty_pipeline", {
    it("returns an empty data.table", {
        dt <- .empty_pipeline()
        expect_true(data.table::is.data.table(dt))
        expect_equal(nrow(dt), 0)
    })
})


describe(".new_step", {
    step <- .new_step(
        step = "step2",
        fun = function(x) x^2,
        params = list(x = 1, y = ~step1),
        depends = c(y = "step1"),
        tags = c("t1", "t2"),
        .nodeId = 0
    )

    it("contains the expected elements", {
        expect_equal(step$step, "step2")
        expect_equal(step$fun[[1]](2), 4)
        expect_equivalent(step$params[[1]], list(x = 1, y = ~step1))
        expect_equal(step$signature, "(x)")
        expect_equal(step$depends, list(c(y = "step1")))
        expect_equal(step$out, list(NULL))
        expect_equal(step$tags, list(c("t1", "t2")))
        expect_equal(step$state, "new")
        expect_true(inherits(step$time, "POSIXct"))
        expect_equal(step$locked, FALSE)
        expect_equal(step$.nodeId, 0)
        expect_equal(step$.indeps, list("x"))
    })

    it("aligns with the empty pipeline", {
        expect_equal(names(step), names(.empty_pipeline()))
    })

    it("can be appended to the empty pipeline", {
        dt <- data.table::rbindlist(list(.empty_pipeline(), step))
        expect_equal(nrow(dt), 1)
    })
})


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
            "startPos must be at least 1"
        )
    })
    it("signals relative index out of bound", {
        expect_error(
            .extract_depends(list(a = ~ -1), "s1"),
            "relative index -1 points outside pipeline"
        )
        expect_error(
            .extract_depends(list(a = ~ -4), steps),
            "relative index -4 points outside pipeline"
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
            "toPos exceeds number of steps"
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
        expect_error(
            f(list(), steps, toPos = 2.0),
            "toPos must be an integer"
        )
    })
})


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

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
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


describe(".pip_update_downstream", {
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("updates states downstream of single node as expected", {
        p <- test_pip()

        expect_true(all(p$pipeline[["state"]] == "new"))
        .pip_update_downstream(p, "a1", what = "state", value = "outdated")

        expect_equal(
            p$pipeline[["state"]],
            c("outdated", "outdated", "new", "outdated", "new")
        )
    })

    it("can update states downstream of multiple nodes", {
        p <- test_pip()
        .pip_update_downstream(
            p,
            steps = c("a1", "b1"),
            what = "state",
            value = "outdated"
        )
        expect_true(all(p$pipeline[["state"]] == "outdated"))

        p <- test_pip()
        .pip_update_downstream(
            p,
            steps = c("a1", "b2"),
            what = "state",
            value = "outdated"
        )

        expect_equal(
            p$pipeline[["state"]],
            c("outdated", "outdated", "new", "outdated", "outdated")
        )
    })
})


# ---------------------------
# Exported pipeline functions
# ---------------------------

describe("pip_new", {
    it("creates a pipeflow pipeline with expected base structure", {
        p <- pip_new()

        expect_true(.is_pipeflow_pip(p))
        expect_true(is.environment(p))
        expect_equal(p[["name"]], "pipe")
        expect_true(data.table::is.data.table(p[["pipeline"]]))
        expect_equal(nrow(p[["pipeline"]]), 0L)
        expect_true(is.environment(p[[".steps_to_nodes"]]))
        expect_equal(ls(envir = p[[".steps_to_nodes"]]), character(0))
        expect_equal(length(dag_get_nodes_order(p[[".dag"]])), 0L)
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

        expect_equal(p$pipeline$depends[[2]], c(x = "s1"))
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
            "relative index -2 points outside pipeline"
        )
    })

    it("can refer to the pipeline itself via the .self argument", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) .self)

        pip_run(p, lgr = NULL)

        expect_true(identical(p$pipeline[["out"]][[1]], p))
    })

    it("allows functions with wildcard arguments", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1, ...) x)

        pip_set_params(p, list(x = 2))
        pip_run(p, lgr = NULL)
        expect_equal(p$pipeline[["out"]][[1]], 2)
    })

    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = ~f1) x + 1)
    }

    it("can insert after a step name", {
        p <- test_pip()
        pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f3", "f2"))
        expect_equal(p[["pipeline"]][["depends"]][[2]], c(x = "f1"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f1"))
    })

    it("can insert by numeric index and supports insertion at beginning", {
        p <- test_pip()
        pip_add(p, "f0", \(x = 0) x, after = 0)

        expect_equal(p[["pipeline"]][["step"]], c("f0", "f1", "f2"))
        expect_equal(p[["pipeline"]][["depends"]][[2]], character(0))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f1"))
    })

    it("uses default insertion position at end", {
        p <- test_pip()
        pip_add(p, "f3", \(x = ~f2) x + 1)

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f2"))
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

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f2"))
    })

    it("can insert a step after pipeline was run", {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        expect_invisible(
            pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")
        )

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f3", "f2"))
        expect_equal(p[["pipeline"]][["depends"]][[2]], c(x = "f1"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f1"))
    })

    it("keeps existing step state and output for appended tail steps", {
        p <- test_pip()
        p[["pipeline"]][["out"]][[2]] <- 42
        p[["pipeline"]][["state"]][[2]] <- "done"

        pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")

        i <- match("f2", p[["pipeline"]][["step"]])
        expect_equal(p[["pipeline"]][["out"]][[i]], 42)
        expect_equal(p[["pipeline"]][["state"]][[i]], "done")
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
        pip_add(
            p,
            "s1",
            function(x = 1, y = 2) x + y,
            params = list(x = 10, y = 20, z = 99)
        )

        pars <- p[["pipeline"]][["params"]][[1]]
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

        pars <- p[["pipeline"]][["params"]][[1]]
        expect_equal(pars[["x"]], 1)
        expect_equal(pars[["size"]], 42)

        pip_run(p, lgr = NULL)
        expect_equal(unname(p[["pipeline"]][["out"]][[1]]), c(1, 42))
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

        expect_equal(unname(p[["pipeline"]][["depends"]][[2]]), "s1")
        expect_equal(names(p[["pipeline"]][["depends"]][[2]]), "y")

        pip_run(p, lgr = NULL)
        expect_equal(p[["pipeline"]][["out"]][[2]]$y, 1)
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

        expect_equal(unname(p[["pipeline"]][["depends"]][[2]]), "s1")

        pip_run(p, lgr = NULL)
        expect_equal(p[["pipeline"]][["out"]][[2]]$y, 5)
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

        expect_equal(names(p[["pipeline"]][["params"]][[2]]), c("y", "z"))
        expect_equal(p[["pipeline"]][[".indeps"]][[2]], "z") # y is a dependency
        expect_equal(names(pip_get_params(p)), c("x", "z"))
    })

    it("outdates the step state if one of the params is updated", {
        p <- pip_new()
        pip_add(
            p,
            "s1",
            function(x = 1, y = 2, ...) x + y,
            params = list(x = 10, y = 20, z = 99)
        )
        pip_run(p, lgr = NULL)

        expect_equal(p[["pipeline"]][["state"]][[1]], "done")
        pip_set_params(p, list(z = 100))
        expect_equal(p[["pipeline"]][["state"]][[1]], "outdated")
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

        expect_true(identical(p[["pipeline"]][["out"]][[1]], p))
        expect_false(".self" %in% names(p[["pipeline"]][["params"]][[1]]))
        expect_false(".self" %in% names(formals(p[["pipeline"]][["fun"]][[1]])))
    })

    it("signals if .self is declared as a step parameter", {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", function(x = 1, .self = NULL) x),
            "'.self' is a reserved parameter name"
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

        expect_true(identical(p2[["pipeline"]][["out"]][[1]], p2))
        expect_true(identical(p[["pipeline"]][["out"]][[1]], p))
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
            expect_equal(p[["pipeline"]][["exec"]][[1]], mode)
        }
    })
})


describe("pip_bind", {
    test_pip <- function(name = "p") {
        pip_new(name) |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("signals invalid inputs", {
        p <- test_pip()
        expect_error(pip_bind(1, p), "x must be a pipeflow pip")
        expect_error(pip_bind(p, 1), "y must be a pipeflow pip")
    })

    it("binds pipelines without mutating inputs", {
        p1 <- test_pip("left")
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        out <- pip_bind(p1, p2)
        expect_true(.is_pipeflow_pip(out))
        expect_equal(out[["name"]], "left-right")
        expect_equal(out[["pipeline"]][["step"]], c("s1", "s2", "t1", "t2"))

        pip_add(out, "extra", \(x = ~t2) x)
        expect_false(pip_has_step(p1, "extra"))
        expect_false(pip_has_step(p2, "extra"))
    })

    it("auto-renames duplicated step names from second pipeline", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")

        out <- pip_bind(p1, p2)
        steps <- out[["pipeline"]][["step"]]
        expect_identical(anyDuplicated(steps), 0L)
        expect_true(all(c("s1", "s2", "s12", "s22") %in% steps))

        dep_new_s2 <- out[["pipeline"]][step == "s22", depends][[1]]
        expect_equal(unname(dep_new_s2), "s12")
    })

    it("handles collisions of auto-fixed names", {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s12", \(x = 2) x)
        p2 <- pip_new("right") |>
            pip_add("s1", \(x = 3) x)

        out <- pip_bind(p1, p2)
        expect_true(pip_has_step(out, "s13"))
    })

    it("rebuilds DAG and keeps dependencies valid in result", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")
        out <- pip_bind(p1, p2)

        nodes <- .pip_get_reachable_nodes(out, "s1")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s1", "s2"))

        nodes <- .pip_get_reachable_nodes(out, "s12")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s12", "s22"))
    })

    it("rebinds .self references to the bound pipeline", {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) .self[["name"]])
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 1) .self[["name"]])

        out <- pip_bind(p1, p2)
        pip_run(out, lgr = NULL)
        expect_equal(
            out[["pipeline"]][["out"]],
            list("left-right", "left-right")
        )
    })

    it("preserves runtime state from both source pipelines", {
        p1 <- test_pip("left")
        data.table::set(
            p1[["pipeline"]],
            i = 1L,
            j = "out",
            value = list(10)
        )
        data.table::set(
            p1[["pipeline"]],
            i = 1L,
            j = "state",
            value = "done"
        )
        data.table::set(
            p1[["pipeline"]],
            i = 1L,
            j = "locked",
            value = TRUE
        )

        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        data.table::set(
            p2[["pipeline"]],
            j = "out",
            value = list(7, 9)
        )
        data.table::set(
            p2[["pipeline"]],
            j = "state",
            value = c("done", "outdated")
        )
        data.table::set(
            p2[["pipeline"]],
            j = "locked",
            value = c(FALSE, TRUE)
        )

        out <- pip_bind(p1, p2)
        actualOut <- out[["pipeline"]][["out"]]
        expectedOut <- list(10, NULL, 7, 9)
        expect_equal(actualOut, expectedOut)
        expect_equal(
            out[["pipeline"]][["state"]],
            c("done", "new", "done", "outdated")
        )
        expect_equal(
            out[["pipeline"]][["locked"]],
            c(TRUE, FALSE, FALSE, TRUE)
        )
    })
})


describe("pip_add_from", {
    test_source <- function() {
        pip_new("src") |>
            pip_add("base", \(x = 2) x, tags = "g1") |>
            pip_add(
                "calc",
                \(x = ~base, m = 3) x * m,
                tags = c("g2", "reuse", "math")
            )
    }

    it("signals invalid inputs", {
        src <- test_source()
        trg <- pip_new("target")

        expect_error(pip_add_from(1, "base", src), "x must be a pipeflow pip")
        expect_error(pip_add_from(trg, "base", 1), "y must be a pipeflow pip")
        expect_error(pip_add_from(trg, src, c("a", "b")))
        expect_error(pip_add_from(trg, src, NA_character_))
        expect_error(pip_add_from(trg, src, ""))
        expect_error(
            pip_add_from(trg, src, "unknown"),
            "does not exist in source pipeline"
        )
    })

    it("adds an independent step preserving tags", {
        src <- test_source()
        trg <- pip_new("target")

        res <- pip_add_from(trg, src, "base")
        expect_true(.is_pipeflow_pip(res))
        expect_true(pip_has_step(trg, "base"))

        tgs <- trg[["pipeline"]][step == "base", tags][[1]]
        expect_equal(tgs, "g1")
    })

    it("adds dependent step when dependencies exist in target", {
        src <- test_source()
        trg <- pip_new("target") |>
            pip_add("base", \(x = 5) x)

        pip_add_from(trg, src, "calc")
        expect_true(pip_has_step(trg, "calc"))

        dep <- trg[["pipeline"]][step == "calc", depends][[1]]
        expect_equal(unname(dep), "base")

        pip_run(trg, lgr = NULL)
        out <- trg[["pipeline"]][step == "calc", out][[1]]
        expect_equal(out, 15)
    })

    it("signals when copied step depends on missing steps in target", {
        src <- test_source()
        trg <- pip_new("target")

        expect_error(
            pip_add_from(trg, src, "calc"),
            "cannot reference unknown steps: 'base'"
        )
    })

    it("rebinds .self to target pipeline through pip_add", {
        src <- pip_new("src") |>
            pip_add("self", \(x = 1) .self[["name"]])
        trg <- pip_new("target")

        pip_add_from(trg, src, "self")
        pip_run(trg, lgr = NULL)
        expect_equal(trg[["pipeline"]][["out"]][[1]], "target")
    })

    it("preserves tags and exec mode from source step", {
        src <- pip_new("src") |>
            pip_add(
                "calc",
                \(x = 1) x,
                tags = c("math", "core"),
                exec = "split"
            )
        trg <- pip_new("target")

        pip_add_from(trg, src, "calc")
        expect_equal(
            trg[["pipeline"]][step == "calc", tags][[1]],
            c("math", "core")
        )
        expect_equal(trg[["pipeline"]][step == "calc", exec][[1]], "split")
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

        expect_equal(p[["pipeline"]][["step"]], c("first", "f2", "f3"))
        expect_equal(
            p[["pipeline"]][["depends"]],
            list(
                character(0),
                c(b = "first"),
                c(a = "first", b = "f2")
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
        beforeOrder <- dag_get_nodes_order(p[[".dag"]])
        beforeReach <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "f1")
        )[["step"]]

        expect_true(dag_has_node(p[[".dag"]], node))
        expect_true(node %in% beforeOrder)
        expect_setequal(beforeReach, c("f1", "f2", "f3", "f4"))

        pip_remove(p, "g1")

        afterOrder <- dag_get_nodes_order(p[[".dag"]])
        afterReach <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "f1")
        )[["step"]]

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3", "f4"))
        expect_true(is.na(.pip_steps_to_nodes(p, "g1")[[1]]))
        expect_false(dag_has_node(p[[".dag"]], node))
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
        beforeOrder <- dag_get_nodes_order(p[[".dag"]])

        expect_true(all(vapply(
            nodeMap,
            FUN = \(nid) dag_has_node(p[[".dag"]], nid),
            FUN.VALUE = logical(1)
        )))

        out <- utils::capture.output(
            pip_remove(p, "f1", force = TRUE),
            type = "message"
        )

        afterOrder <- dag_get_nodes_order(p[[".dag"]])
        remainingNode <- as.integer(nodeMap[["g1"]])

        expect_equal(p[["pipeline"]][["step"]], "g1")
        expect_equal(p[["pipeline"]][[".nodeId"]], remainingNode)
        expect_equal(afterOrder, remainingNode)
        expect_equal(length(afterOrder), length(beforeOrder) - 4L)
        expect_true(dag_has_node(p[[".dag"]], remainingNode))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f1"]])))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f2"]])))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f3"]])))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f4"]])))
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
})


describe("pip_replace", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = 2) x) |>
            pip_add("f3", \(x = ~f2) x + 1)
    }

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

    it("replaces a step in-place while keeping the original order", {
        p <- test_pip()
        pip_run(p, lgr = NULL)
        expect_equal(p[["pipeline"]][step == "f3", out][[1]], 3)

        pip_replace(p, "f2", \(x = 4) x * 2)
        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3"))

        pip_run(p, lgr = NULL)
        expect_equal(p[["pipeline"]][step == "f2", out][[1]], 8)
        expect_equal(p[["pipeline"]][step == "f3", out][[1]], 9)
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
            p[["pipeline"]][["state"]],
            c("done", "new", "done", "outdated")
        )
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

        i <- match("a2", p[["pipeline"]][["step"]])
        expect_equal(p[["pipeline"]][["tags"]][[i]], c("updated", "core"))
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

        expect_true(.is_pipeflow_pip(p2))
        expect_false(identical(p2, p))
        expect_equal(p2[["name"]], p[["name"]])
        expect_equal(p2[["pipeline"]][["step"]], p[["pipeline"]][["step"]])
        expect_equal(
            p2[["pipeline"]][["depends"]],
            p[["pipeline"]][["depends"]]
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

        p2[["pipeline"]][["state"]][1] <- "done"
        expect_equal(p[["pipeline"]][["state"]][1], "new")

        pip_add(p2, "s3", \(x = ~s2) x)
        expect_false(pip_has_step(p, "s3"))
        expect_true(pip_has_step(p2, "s3"))
    })

    it("rebinds .self params to the cloned pipeline", {
        p <- test_pip()
        p2 <- pip_clone(p)

        pip_run(p2, lgr = NULL)
        expect_equal(p2[["pipeline"]][["out"]][[1]], "p1")

        # The original pipeline still points at itself.
        pip_run(p, lgr = NULL, force = TRUE)
        expect_equal(p[["pipeline"]][["out"]][[1]], "p1")
    })

    it("clones an empty pipeline", {
        p <- pip_new()
        p2 <- pip_clone(p)

        expect_equal(length(p2), 0L)
        expect_equal(p2[["name"]], p[["name"]])
        expect_false(identical(p2, p))
    })
})


describe("pip_collect_out", {
    it("returns empty list for empty pipeline", {
        p <- pip_new()
        expect_equal(pip_collect_out(p), list())
    })

    it("returns named flat list", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = "data")
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")

        p[["pipeline"]][["out"]] <- list(10, 20)

        out <- pip_collect_out(p)
        expect_equal(names(out), c("s1", "s2"))
        expect_equal(unname(out), list(10, 20))
    })

    it("works on pipeline views", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = "data")
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~ -1) x + 1, tags = "model")

        p[["pipeline"]][["out"]] <- list("o1", "o2", "o3")

        v <- pip_view(p, tags = "model")
        out <- pip_collect_out(v)

        expect_equal(names(out), c("s2", "s3"))
        expect_equal(unname(out), list("o2", "o3"))
    })

    it("preserves NULL outputs", {
        p <- pip_new("test") |>
            pip_add("s1", \(x = NULL) x) |>
            pip_add("s2", \(x = 3) x, tags = "g2") |>
            pip_add("s3", \(x = ~s2, factor = 2) x * factor) |>
            pip_add("s4", \(x = ~s2) x^2, tags = "g4")

        out <- pip_collect_out(p)
        expect_equal(out, list(s1 = NULL, s2 = NULL, s3 = NULL, s4 = NULL))

        pip_run(p, lgr = NULL)
        out <- pip_collect_out(p)
        expect_equal(out, list(s1 = NULL, s2 = 3, s3 = 2 * 3, s4 = 3^2))
    })

    it("signals invalid arguments", {
        p <- pip_new()
        expect_error(pip_collect_out(1), "x must be a pipeflow pip or view")
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

        v <- pip_view(p, filter = list(step = c("s3", "s4")))
        params <- pip_get_params(v)

        expect_named(params, "x")
        expect_equal(params[["x"]], 9)
    })

    it("returns empty list for view without independent params", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(y = ~s1) y)

        v <- pip_view(p, filter = list(step = "s2"))
        expect_equal(pip_get_params(v), list())
    })

    it("signals invalid input", {
        expect_error(
            pip_get_params(1),
            "x must be a pipeflow pip or view"
        )
    })
})


describe("pip_get_graph", {
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
        p[["pipeline"]][["state"]] <- c("new", "done", "failed")

        g <- pip_get_graph(p)
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
            p[["pipeline"]][["state"]],
            FUN = \(st) .step_states[[st]][["color"]],
            FUN.VALUE = character(1)
        )
        expect_equal(nodes[["color"]], unname(expectedColors))
        expect_setequal(map_edge_labels(g), c("s1->s2", "s2->s3"))
    })

    it("supports view-only graph or graph with upstream closure", {
        p <- test_pip()
        v <- pip_view(p, i = "s3")

        gView <- pip_get_graph(v, include_upstream = FALSE)
        expect_equal(gView[["nodes"]][["label"]], "s3")
        expect_equal(nrow(gView[["edges"]]), 0L)

        gUp <- pip_get_graph(v, include_upstream = TRUE)
        expect_setequal(gUp[["nodes"]][["label"]], c("s1", "s2", "s3"))
        expect_setequal(map_edge_labels(gUp), c("s1->s2", "s2->s3"))
    })

    it("signals invalid inputs", {
        p <- test_pip()

        expect_error(pip_get_graph(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_get_graph(p, include_upstream = c(TRUE, FALSE)),
            "include_upstream must be a single logical value"
        )
    })

    it("returns empty nodes and edges for empty pipeline", {
        g <- pip_get_graph(pip_new())
        expect_equal(nrow(g[["nodes"]]), 0L)
        expect_equal(nrow(g[["edges"]]), 0L)
        expect_named(g, c("nodes", "edges"))
    })
})


describe("pip_has_step", {
    it("can be checked if pipeline has a step", {
        p <- pip_new()
        expect_false(pip_has_step(p, "s1"))
        pip_add(p, "s1", \(a = 1) a)
        expect_true(pip_has_step(p, "s1"))
    })

    it("errors at bad step argument", {
        f <- pip_has_step
        expect_error(f(p, list("not a character string")))
        expect_error(f(p, c("not", "a", "single", "string")))
        expect_error(f(p, NA))
        expect_error(f(p, ""))
        expect_error(f(p, 1))
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
            expect_equal(p$pipeline[["out"]], list(1, 2, 2, "blabla"))
            expect_equal(p$pipeline[["state"]], rep("done", 4))
        })

        it("marks downstream steps not reached due to abort as outdated", {
            p <- pip_new() |>
                pip_add("load_raw", \(x = 1) stop("io error"), tags = "io") |>
                pip_add("fit_model", \(x = ~ -1) x + 1, tags = "model") |>
                pip_add("eval_model", \(x = ~fit_model) x, tags = "model")

            expect_error(pip_run(p, lgr = NULL), "io error")
            expect_equal(
                p$pipeline[["state"]],
                c("failed", "outdated", "outdated")
            )
        })

        it("marks the run state as failed when a step errors", {
            p <- pip_new() |>
                pip_add("s1", \(x = 1) x) |>
                pip_add("s2", \(x = ~s1) stop("boom")) |>
                pip_add("s3", \(x = ~s2) x + 1)

            expect_error(pip_run(p, lgr = NULL), "boom")
            expect_equal(as.character(p[[".run_state"]]), "failed")

            # A subsequent successful run resets the state to ready.
            pip_replace(p, "s2", \(x = ~s1) x + 1)
            pip_run(p, lgr = NULL)
            expect_equal(as.character(p[[".run_state"]]), "ready")
        })

        it("marks the run state as failed when a view run errors", {
            p <- pip_new() |>
                pip_add("a", \(x = 1) x) |>
                pip_add("b", \(x = ~a) stop("view boom"))
            v <- pip_view(p, i = "b")

            expect_error(pip_run(v, lgr = NULL), "view boom")
            expect_equal(as.character(p[[".run_state"]]), "failed")
        })
    })

    describe("running views", {
        it("can run parts of the pipeline via views", {
            p <- test_pip()
            v <- pip_view(p, tags = "bla")
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(NULL, NULL, NULL, "blabla"))
        })

        it("runs all steps of the view plus upstream dependencies", {
            p <- test_pip()
            v <- pip_view(p, tags = "model")
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, 2, NULL))

            p <- pip_new() |>
                pip_add("f1", \(x = 1) x) |>
                pip_add("f2", \(x = ~f1) x + 1) |>
                pip_add("f3", \(x = 1) x + 1) |>
                pip_add("f4", \(x = ~f2) x + 1) |>
                pip_add("f5", \(x = ~f4) x + 1)

            v <- pip_view(p, i = "f2")
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, NULL, NULL, NULL))

            v <- pip_view(p, i = "f5")
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, NULL, 3, 4))
        })

        it("marks downstream steps outside the view as outdated", {
            p <- test_pip()
            v <- pip_view(p, tags = "io")
            pip_run(v, lgr = NULL)
            expect_equal(
                p$pipeline[["state"]],
                c("done", "outdated", "outdated", "new")
            )
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

            splitOut <- p$pipeline$out[[2]]
            meanOut <- p$pipeline$out[[3]]
            plusOneOut <- p$pipeline$out[[4]]

            expect_true(inherits(splitOut, "pipeflow_partitioned"))
            expect_true(inherits(meanOut, "pipeflow_partitioned"))
            expect_true(inherits(plusOneOut, "pipeflow_partitioned"))

            expect_equal(meanOut[["a"]], 2)
            expect_equal(meanOut[["b"]], 15)
            expect_equal(plusOneOut[["a"]], 3)
            expect_equal(plusOneOut[["b"]], 16)
            expect_equal(p$pipeline$out[[5]], 9.5)
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

            out <- p$pipeline$out[[6]]
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
                expect_equal(pip[["out"]], list(0, 1, 3, 6))

                pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
                expect_equal(pip[["out"]], list(15, 16, 16 / 2, 16 * 3))
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
                expect_equal(pip[["out"]], list(0, 1, 3, 6))

                pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
                expect_equal(pip[["out"]], list(15, 16, 16 / 2, 16 * 3))
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
                expect_equal(pip[["out"]], list(0, 1, 11, 14, 4))
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
                expect_equal(pip[["out"]], list(15, 16, 8, 48))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )

                pip |> pip_set_params(list(xInit = 20)) |> pip_run(lgr = NULL)
                expect_equal(pip[["out"]], list(20, 21, 10.5, 63))
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
                expect_equal(pip[["out"]], list(0, 1, 3, 6, 7))

                pip |> pip_set_params(list(xInit = 15))
                v <- pip_view(pip, i = c("f1", "f2"))
                pip_run(v, lgr = NULL, force = TRUE)

                expect_equal(pip[["out"]], list(15, 16, 8, NULL, 7))
                expect_equal(
                    body(pip[["f3", "fun"]]),
                    body(function(x = ~f1) x * 3)
                )
                expect_equal(
                    pip[["pipeline"]][step == "f4", state][[1]],
                    "outdated"
                )

                pip_run(pip, lgr = NULL)
                expect_equal(pip[["out"]], list(15, 16, 8, 48, 49))
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
        expect_equal(pip[["step"]], c("init", "f1", "f2a", "f2b", "f3"))
        expect_equal(pip[["state"]], c("done", "done", "new", "done", "new"))
        expect_equal(pip[["out"]], list(11, 12, NULL, NULL + 22, NULL))

        pip_set_params(pip, list(xInit = 11))
        pip_run(pip, lgr = NULL)

        expect_equal(pip[["out"]], list(11, 12, 33, 55, 85))
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

                            pip_restart(.self)
                        }

                        x + 2
                    }
                ) |>
                pip_add("f3", function(x = ~f2) x + 3)
        }

        pip <- test_pip()
        pip_set_params(pip, list(xInit = 11)) |> pip_run(lgr = NULL)
        expect_equal(pip[["step"]], c("init", "f1", "f2a", "f2b", "f3"))
        expect_equal(pip[["state"]], rep("done", 5))
        expect_equal(pip[["out"]], list(11, 12, 33, 55, 85))
    })

    it("keeps .self bound to the same pipeline across a restart", {
        captured <- list()
        p <- pip_new("self-bound") |>
            pip_add("init", function(xInit = 0) xInit) |>
            pip_add("f1", function(x = ~init) x + 1) |>
            pip_add("f2", function(x = ~f1) {
                captured[[1]] <<- .self
                pip_restart(.self)
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
                        pip_restart(.self, times = 2L)
                    }
                    x + 2
                }
            ) |>
            pip_add("f3", function(x = ~f2) x + 3)

        p |> pip_set_params(list(xInit = 11)) |> pip_run(lgr = NULL)

        expect_equal(count, 3L)
        expect_equal(p[["pipeline"]][["out"]], list(11, 12, 14, 36))
        expect_equal(p[["pipeline"]][["state"]], rep("done", 4))
        expect_equal(
            body(p[["pipeline"]][["fun"]][[4]]),
            body(function(x = ~f1) x * 3)
        )
    })

    it("force = TRUE re-executes all steps regardless of state", {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        p[["pipeline"]][["out"]] <- list(99, 99, 99, 99)
        pip_run(p, lgr = NULL, force = TRUE)

        expect_equal(p[["pipeline"]][["out"]], list(1, 2, 2, "blabla"))
        expect_equal(p[["pipeline"]][["state"]], rep("done", 4))
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
        p[["pipeline"]][["locked"]][[2]] <- TRUE

        pip_run(p, lgr = NULL)
        expect_equal(
            p[["pipeline"]][["state"]],
            c("done", "new", "done", "done")
        )
        expect_equal(p[["pipeline"]][["out"]], list(1, NULL, NULL, "blabla"))
    })

    it("skips locked steps even with force = TRUE", {
        p <- test_pip()
        p[["pipeline"]][["locked"]][[2]] <- TRUE
        p[["pipeline"]][["out"]][[2]] <- 99

        pip_run(p, lgr = NULL, force = TRUE)
        expect_equal(p[["pipeline"]][["out"]][[2]], 99)
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
})


describe("pip_restart", {
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

        expect_error(pip_restart(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_restart(p, force = "yes"),
            "force must be a single logical value"
        )
        expect_error(
            pip_restart(p, force = c(TRUE, FALSE)),
            "force must be a single logical value"
        )
        expect_error(
            pip_restart(p, times = 0),
            "times must be a single integer value >= 1"
        )
        expect_error(
            pip_restart(p, times = NA),
            "times must be a single integer value >= 1"
        )
        expect_error(
            pip_restart(p, times = "a"),
            "times must be a single integer value >= 1"
        )
        expect_error(
            pip_restart(p, times = c(1, 2)),
            "times must be a single integer value >= 1"
        )
    })

    it("restarts the pipeline when a step requests a restart", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    pip_restart(.self)
                }
                c[["n"]]
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 2L)
        expect_equal(as.character(p[[".run_state"]]), "ready")
    })

    it("stops recursive restarts after the announced times", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                pip_restart(.self, times = 2L)
                c[["n"]]
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 3L)
        expect_equal(p[[".restart_count"]], 0L)
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
                    pip_restart(.self, force = TRUE)
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
        expect_equal(p[["pipeline"]][["out"]], list(1, 2, 3))
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
                    pip_restart(.self, force = FALSE)
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
        expect_equal(p[["pipeline"]][["out"]], list(1, 2, 3))
    })

    it("marks the pipeline as restarting when called before a run", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)

        pip_restart(p)
        expect_equal(as.character(p[[".run_state"]]), "restart")
        expect_equal(p[[".restart_count"]], 1L)

        logs <- character(0)
        lgr <- function(level, msg) logs <<- c(logs, msg)
        pip_run(p, lgr = lgr)

        expect_true(any(grepl("Restarting run", logs)))
        expect_equal(as.character(p[[".run_state"]]), "ready")
    })

    it("restarts the underlying pipeline when called on a view", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)
        v <- pip_view(p, i = "s1")

        pip_restart(v)

        expect_equal(as.character(p[[".run_state"]]), "restart")
        expect_equal(p[[".restart_count"]], 1L)
        expect_identical(v[["pip"]], p)
    })

    it("restarts a view run when a step requests a restart", {
        c <- counter_env(n = 0L)
        p <- pip_new("view-pipeline") |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    pip_restart(.self)
                }
                x
            }) |>
            pip_add("s2", function(x = ~s1) x + 1)
        v <- pip_view(p, i = "s2")

        pip_run(v, lgr = NULL)

        expect_equal(c[["n"]], 2L)
        expect_equal(p[["pipeline"]][["out"]], list(1, 2))
        expect_equal(as.character(p[[".run_state"]]), "ready")
    })

    it("restarts without declaring .self in the step signature", {
        c <- counter_env(n = 0L)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                c[["n"]] <- c[["n"]] + 1L
                if (c[["n"]] == 1L) {
                    pip_restart(.self)
                }
                x
            })

        pip_run(p, lgr = NULL)

        expect_equal(c[["n"]], 2L)
        expect_equal(as.character(p[[".run_state"]]), "ready")
    })
})

describe("pip_stop", {
    it("signals invalid inputs", {
        expect_error(pip_stop(1), "x must be a pipeflow pip or view")
    })

    it("marks the pipeline as stopping when called before a run", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)

        pip_stop(p)

        expect_equal(as.character(p[[".run_state"]]), "stop")
        expect_equal(as.character(p[[".run_state"]][]), "stop")
    })

    it("aborts the run at the stopping step and marks downstream outdated", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                pip_stop(.self)
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        pip_run(p, lgr = NULL)

        expect_equal(p[["pipeline"]][["out"]], list(1, 2, NULL))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("done", "done", "outdated")
        )
        expect_equal(as.character(p[[".run_state"]]), "ready")
    })

    it("logs the manual stop message during the run", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                pip_stop(.self)
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        logs <- character(0)
        lgr <- function(level, msg) logs <<- c(logs, msg)
        pip_run(p, lgr = lgr)

        expect_true(any(
            grepl("Aborting pipeline execution on manual stop", logs)
        ))
        expect_true(any(grepl("Step 1/3 s1", logs)))
        expect_true(any(grepl("Step 2/3 s2", logs)))
        expect_false(any(grepl("Step 3/3 s3", logs)))
    })

    it("does not execute steps after the stopping step", {
        ran <- character(0)
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                ran <<- c(ran, "s1")
                x
            }) |>
            pip_add("s2", function(x = ~s1) {
                ran <<- c(ran, "s2")
                pip_stop(.self)
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) {
                ran <<- c(ran, "s3")
                x + 1
            })

        pip_run(p, lgr = NULL)

        expect_equal(ran, c("s1", "s2"))
    })

    it("stops at the first step and marks all later steps outdated", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) {
                pip_stop(.self)
                x
            }) |>
            pip_add("s2", function(x = ~s1) x + 1) |>
            pip_add("s3", function(x = ~s2) x + 1)

        pip_run(p, lgr = NULL)

        expect_equal(p[["pipeline"]][["out"]], list(1, NULL, NULL))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("done", "outdated", "outdated")
        )
    })

    it("stops without declaring .self in the step signature", {
        p <- pip_new() |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                pip_stop(.self)
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1)

        pip_run(p, lgr = NULL)

        expect_equal(p[["pipeline"]][["out"]], list(1, 2, NULL))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("done", "done", "outdated")
        )
    })

    it("marks the underlying pipeline as stopping when called on a view", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)
        v <- pip_view(p, i = "s1")

        pip_stop(v)

        expect_equal(as.character(p[[".run_state"]]), "stop")
        expect_identical(v[["pip"]], p)
    })

    it("aborts a view run at the stopping step", {
        p <- pip_new("view-pipeline") |>
            pip_add("s1", function(x = 1) x) |>
            pip_add("s2", function(x = ~s1) {
                pip_stop(.self)
                x + 1
            }) |>
            pip_add("s3", function(x = ~s2) x + 1) |>
            pip_add("s4", function(x = ~s3) x + 1)
        v <- pip_view(p, i = "s4")

        pip_run(v, lgr = NULL)

        expect_equal(p[["pipeline"]][["out"]], list(1, 2, NULL, NULL))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("done", "done", "outdated", "outdated")
        )
        expect_equal(as.character(p[[".run_state"]]), "ready")
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
        after <- p[["pipeline"]][["params"]]
        expect_equal(after[[1]][["x"]], 11)
        expect_equal(after[[1]][["data"]], data.frame(b = 3:4))
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["x"]], 11)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("can be used with pip or view", {
        p <- test_pip()

        res <- pip_set_params(p, params = list(x = 11, y = 22))
        expect_true(inherits(res, "pipeflow_pip"))

        v <- pip_view(p, filter = list(step = "s2"))
        res <- pip_set_params(v, params = list(y = 22))
        expect_true(inherits(res, "pipeflow_view"))
    })

    it("ignores locked steps", {
        p <- test_pip()
        p$pipeline[["locked"]][[1]] <- TRUE
        pip_set_params(p, params = list(x = 99))
        after <- p[["pipeline"]][["params"]]
        expect_equal(after[[1]][["x"]], 1)
    })

    it("warns for unused parameters", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1, ...) x)

        expect_warning(
            pip_set_params(p, params = list(foo = 1)),
            "Trying to set parameters not defined in the target: foo"
        )
    })

    it("sets parameters only within the selected view", {
        p <- test_pip()

        v <- pip_view(p, filter = list(step = "s3"))
        expect_warning(
            pip_set_params(v, params = list(z = 33, x = 11, y = 22)),
            "Trying to set parameters not defined in the target: y"
        )
        after <- p[["pipeline"]][["params"]]

        expect_equal(after[[1]][["x"]], 1)
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["x"]], 11)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("marks changed and dependent downstream steps as 'outdated'", {
        p <- test_pip()
        pip_set_params(p, params = list(x = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("outdated", "outdated", "outdated", "outdated")
        )

        p <- test_pip()
        pip_set_params(p, params = list(y = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("new", "outdated", "new", "new")
        )

        p <- test_pip()
        pip_set_params(p, params = list(z = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("new", "new", "outdated", "outdated")
        )
    })

    it("no-ops on empty params list", {
        p <- test_pip()
        pip_set_params(p, params = list())
        expect_equal(p[["pipeline"]][["state"]], rep("new", 4))
    })

    it("no-ops if all considered steps are locked, with a warning", {
        p <- test_pip() |> pip_lock()
        params <- pip_get_params(p)

        expect_warning(
            pip_set_params(p, params = list(x = 5)),
            "all selected steps are locked"
        )
        expect_equal(pip_get_params(p), params) # verify that nothing changed
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

        expect_equal(p[["pipeline"]][["tags"]][[1]], c("init", "daily", "core"))
        expect_equal(
            p[["pipeline"]][["tags"]][[2]],
            c("daily", "model", "core")
        )
        expect_equal(
            p[["pipeline"]][["tags"]][[3]],
            c("report", "daily", "core")
        )
    })

    it("updates only rows in a view and skips locked steps", {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]][[2]] <- TRUE
        p[["pipeline"]][["tags"]][[2]] <- "keep"

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_tag(v, tags = "view")

        expect_equal(p[["pipeline"]][["tags"]][[1]], c("init", "daily"))
        expect_equal(p[["pipeline"]][["tags"]][[2]], "keep")
        expect_equal(p[["pipeline"]][["tags"]][[3]], c("report", "view"))
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

        expect_equal(p[["pipeline"]][["tags"]][[1]], "init")
        expect_equal(p[["pipeline"]][["tags"]][[2]], "model")
        expect_equal(p[["pipeline"]][["tags"]][[3]], character(0))
    })

    it("updates only rows in a view and skips locked steps", {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]][[2]] <- TRUE
        p[["pipeline"]][["tags"]][[2]] <- c("daily", "model")

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_untag(v, tags = "daily")

        expect_equal(p[["pipeline"]][["tags"]][[1]], c("init", "daily"))
        expect_equal(p[["pipeline"]][["tags"]][[2]], c("daily", "model"))
        expect_equal(p[["pipeline"]][["tags"]][[3]], "report")
    })
})


describe("pip_lock", {
    it("signals invalid input", {
        expect_error(pip_lock(1), "x must be a pipeflow pip or view")
    })

    it("locks selected steps", {
        p <- tag_lock_test_pip()
        pip_lock(p)
        expect_true(all(p[["pipeline"]][["locked"]]))
    })

    it("locks only rows covered by a view", {
        p <- tag_lock_test_pip()
        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_lock(v)

        expect_false(p[["pipeline"]][["locked"]][[1]])
        expect_true(p[["pipeline"]][["locked"]][[2]])
        expect_true(p[["pipeline"]][["locked"]][[3]])
    })
})


describe("pip_unlock", {
    it("signals invalid input", {
        expect_error(pip_unlock(1), "x must be a pipeflow pip or view")
    })

    it("unlocks selected steps", {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]] <- rep(TRUE, nrow(p[["pipeline"]]))

        pip_unlock(p)
        expect_false(any(p[["pipeline"]][["locked"]]))
    })

    it("unlocks only rows covered by a view", {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]] <- rep(TRUE, nrow(p[["pipeline"]]))

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_unlock(v)

        expect_true(p[["pipeline"]][["locked"]][[1]])
        expect_false(p[["pipeline"]][["locked"]][[2]])
        expect_false(p[["pipeline"]][["locked"]][[3]])
    })
})


describe("pip_view", {
    it("returns a view object with the expected structure", {
        p <- pip_new("test_pipeline")
        pip_add(p, "load", \(x = 1) x, tags = "io")

        v <- pip_view(p)
        expect_true(.is_pipeflow_view(v))
        expect_true("rows" %in% names(v))
        expect_identical(v[["pip"]], p)
        expect_identical(v[["name"]], "test_pipeline view")
    })

    it("can filter by columns with fixed matching", {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, tags = "io")
        pip_add(p, "fit", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "eval", \(x = ~fit) x, tags = "model")

        p[["pipeline"]][2, state := "done"]

        v <- pip_view(p, tags = "model", filter = list(state = "done"))

        expect_equal(v[["rows"]], 2L)
    })

    it("can filter by depends column with multiple entries", {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, tags = "io")
        pip_add(p, "fit", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "eval", \(x = ~load, y = ~fit) x, tags = "model")

        v <- pip_view(
            p,
            filter = list(depends = "fit"),
            tags = "model"
        )
        v
        expect_equal(v[["rows"]], 3L)
    })

    it("can filter by tags", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~ -1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["rows"]], c(1L, 3L))
    })

    it("can filter by regex when fixed is FALSE", {
        p <- pip_new()
        pip_add(p, "data", \(x = 1) x)
        pip_add(p, "fit_model", \(x = ~ -1) x + 1)
        pip_add(p, "eval_model", \(x = ~data, y = ~fit_model) x)

        v <- pip_view(
            p,
            filter = list(step = "_model$"),
            fixed = FALSE
        )
        expect_equal(v[["rows"]], c(2L, 3L))
    })

    it("intersects filtered rows with explicit i", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, tags = "g1")
        pip_add(p, "a2", \(x = ~ -1) x, tags = "g2")
        pip_add(p, "a3", \(x = ~ -1) x, tags = "g2")

        v <- pip_view(
            p,
            i = c(1L, 2L),
            tags = "g2"
        )

        expect_equal(v[["rows"]], 2L)
    })

    it("can select rows via step names in i", {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, tags = "g1")
        pip_add(p, "a2", \(x = ~ -1) x, tags = "g2")
        pip_add(p, "a3", \(x = ~ -1) x, tags = "g2")

        v <- pip_view(p, i = c("a1", "a3"))
        expect_equal(v[["rows"]], c(1L, 3L))

        v <- pip_view(p, i = c("a1", "a2"), tags = "g2")
        expect_equal(v[["rows"]], 2L)
    })

    it("can be called on views to further subset rows", {
        p <- pip_new("test_pipeline")
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~ -1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~ -1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["rows"]], c(1L, 3L))
        expect_equal(v[["name"]], "test_pipeline view")

        v2 <- pip_view(v, tags = "report")
        expect_equal(v2[["rows"]], 3L)
        expect_equal(v2[["name"]], "test_pipeline view view")
    })

    it("signals invalid filter names", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, filter = list(not_a_column = "x")),
            "Invalid filter name"
        )
    })

    it("signals invalid row indices", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, i = c(0L, 1L)),
            "Invalid row indices in 'i'"
        )
        expect_error(
            pip_view(p, i = c(2L)),
            "Invalid row indices in 'i'"
        )
    })

    it("signals invalid step names in i", {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, i = c("s1", "")),
            "step names must be non-empty strings"
        )
        expect_error(
            pip_view(p, i = c("s1", NA_character_)),
            "step names must not contain NA"
        )
        expect_error(
            pip_view(p, i = "unknown"),
            "Unknown step names: unknown"
        )
    })
})


# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

describe("length", {
    it("returns an integer value", {
        p <- pip_new()
        expect_true(is.integer(length(p)))
    })

    it("returns the expected value", {
        p <- pip_new()
        expect_equal(length(p), 0L)
        pip_add(p, "s1", \(a = 1) a)
        pip_add(p, "s2", \(a = 1) a)
        expect_equal(length(p), 2L)

        v <- pip_view(p, 1)
        expect_equal(length(v), 1L)
    })
})

describe("extract operator [", {
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("returns a pipeline subset including upstream dependencies by row", {
        p <- test_pip()
        sub <- p[5L]

        expect_true(.is_pipeflow_pip(sub))
        expect_equal(sub[["pipeline"]][["step"]], c("b1", "b2"))
    })

    it("returns a pipeline subset including upstream dependencies by step", {
        p <- test_pip()
        sub <- p[c("a2", "b2")]

        expect_equal(sub[["pipeline"]][["step"]], c("a1", "a2", "b1", "b2"))
    })

    it("returns the full pipeline when i is missing", {
        p <- test_pip()
        sub <- p[]

        expect_equal(sub[["pipeline"]][["step"]], p[["pipeline"]][["step"]])
    })

    it("returns an empty pipeline for empty selectors", {
        p <- test_pip()

        expect_equal(length(p[integer()]), 0L)
        expect_equal(length(p[character()]), 0L)
    })

    it("signals invalid row indices", {
        p <- test_pip()

        expect_error(p[c(0, 1)], "Invalid row indices in 'i'")
        expect_error(p[99], "Invalid row indices in 'i'")
        expect_error(p[c(1, NA)], "row indices in 'i' must not contain NA")
        expect_error(p[c(1.1, 2)], "must be whole numbers")
    })

    it("signals invalid step names", {
        p <- test_pip()

        expect_error(p[c("a1", "")], "must be non-empty strings")
        expect_error(p[c("a1", NA)], "must not contain NA")
        expect_error(p[c("a1", "unknown")], "Unknown step names")
    })

    it("returns an independent copy", {
        p <- test_pip()
        sub <- p[c("a2")]

        sub[["pipeline"]][["state"]][1] <- "done"
        expect_equal(p[["pipeline"]][["state"]][1], "new")
    })

    it("copies DAG edges for the extracted subset", {
        p <- test_pip()
        sub <- p[c("a2", "b2")]

        nodes_a1 <- .pip_get_reachable_nodes(sub, "a1")
        steps_a1 <- .pip_filter_nodes(sub, nodes_a1)[["step"]]
        expect_setequal(steps_a1, c("a1", "a2"))

        nodes_b1 <- .pip_get_reachable_nodes(sub, "b1")
        steps_b1 <- .pip_filter_nodes(sub, nodes_b1)[["step"]]
        expect_setequal(steps_b1, c("b1", "b2"))
    })

    it("uses an independent DAG copy in the extracted subset", {
        p <- test_pip()
        sub <- p[c("a2", "b2")]

        from <- as.integer(.pip_steps_to_nodes(sub, "a2")[[1]])
        to <- as.integer(.pip_steps_to_nodes(sub, "b1")[[1]])
        dag_add_edges_to(sub[[".dag"]], from = from, to = to)

        sub_steps <- .pip_filter_nodes(
            sub,
            .pip_get_reachable_nodes(sub, "a1")
        )[["step"]]
        expect_setequal(sub_steps, c("a1", "a2", "b1", "b2"))

        original_steps <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "a1")
        )[["step"]]
        expect_setequal(original_steps, c("a1", "a2", "a3"))
    })
})


describe("extract operator [[", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "init") |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("keeps environment-style extraction for internal bindings", {
        p <- test_pip()
        expect_equal(p[["name"]], "pipe")
        expect_true(data.table::is.data.table(p[["pipeline"]]))
        expect_false(is.null(p[[".dag"]]))
    })

    it("extracts full columns when j is missing", {
        p <- test_pip()
        expect_equal(p[["step"]], c("s1", "s2"))
        expect_equal(p[[1]], c("s1", "s2"))
        expect_null(p[["unknown"]])
    })

    it("extracts by row selector and column selector", {
        p <- test_pip()

        expect_equal(p[[2L, "step"]], "s2")
        expect_equal(p[["s1", "state"]], "new")
        expect_equal(p[[c(1L, 2L), "state"]], c("new", "new"))
        expect_equal(p[[c("s1", "s2"), "step"]], c("s1", "s2"))
    })

    it("extracts list-columns for single and multiple rows consistently", {
        p <- test_pip() |> pip_run(lgr = NULL)

        # Single-row extraction returns the row value from the list-column.
        expect_equal(p[[1L, "tags"]], "init")
        expect_equal(p[[2L, "tags"]], character(0))
        expect_equal(p[[1L, "params"]], list(x = 1))
        expect_named(p[[2L, "params"]], "x")
        expect_equal(p[[1L, "depends"]], character(0))
        expect_equal(p[[2L, "depends"]], c(x = "s1"))
        expect_equal(p[[1L, "out"]], 1)
        expect_equal(p[[2L, "out"]], 2)

        # Multi-row extraction returns a list of row values.
        tags <- p[[c(1L, 2L), "tags"]]
        expect_true(is.list(tags))
        expect_equal(tags[[1]], "init")
        expect_equal(tags[[2]], character(0))

        depends <- p[[c("s1", "s2"), "depends"]]
        expect_true(is.list(depends))
        expect_equal(depends[[1]], character(0))
        expect_equal(depends[[2]], c(x = "s1"))

        outs <- p[[c("s1", "s2"), "out"]]
        expect_true(is.list(outs))
        expect_equal(outs[[1]], 1)
        expect_equal(outs[[2]], 2)
    })

    it(
        paste(
            "can distinguish between steps called 'name' and 'pipeline'",
            "and the internal 'name' and 'pipeline' elements"
        ),
        {
            p <- pip_new("pipe") |>
                pip_add("name", \(x = "hello") x) |>
                pip_add("pipeline", \(x = ~name, y = "world") paste(x, y))

            expect_equal(p[["name"]], "pipe")
            expect_equal(p[["pipeline"]], p$pipeline)

            expect_equal(p[["name", "params"]], p[[1, "params"]])
            expect_equal(p[["pipeline", "params"]], p[[2, "params"]])
        }
    )

    it("signals invalid row and column selectors", {
        p <- test_pip()

        expect_equal(p[[c(1, NA), "step"]], c("s1", NA_character_))
        expect_error(
            p[[c("s1", ""), "step"]],
            "step names must be non-empty strings"
        )
        expect_error(
            p[[c("s1", "unknown"), "step"]],
            "Unknown step names"
        )
        expect_null(p[[1, "unknown"]])
        expect_error(
            p[[1, c("step", "state")]],
            "subscript out of bounds"
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


describe("print.pipeflow_pip", {
    get_print_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    it("shows step/depends/out/state plus tags when steps have tags", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        header <- get_print_header(p)

        expect_equal(header, c("step", "depends", "out", "state", "tags"))
    })

    it("shows tags when at least one step has tags", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe")
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(x = ~s1) x + 1, tags = "model")

        header <- get_print_header(p)

        expect_equal(header, c("step", "depends", "out", "state", "tags"))
    })

    it("prints the tags column last when any step defines tags", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        header <- get_print_header(p)

        expect_equal(header, c("step", "depends", "out", "state", "tags"))
    })

    it("prints the exec column last when any step defines non-auto exec", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2", exec = "split")

        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "depends", "out", "state", "tags", "exec")
        )
    })
})


describe("print.pipeflow_view", {
    get_view_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    it("prints a view header and uses the pipeline column layout", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "io") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "model")

        v <- pip_view(p, tags = "model")
        out <- capture.output(print(v))

        expect_true(any(grepl("<pipeflow_view>", out)))
        expect_equal(
            get_view_header(v),
            c("step", "depends", "out", "state", "tags")
        )
    })

    it("prints the tags column last for tagged views", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        v <- pip_view(p, tags = "core")

        expect_equal(
            get_view_header(v),
            c("step", "depends", "out", "state", "tags")
        )
    })

    it("prints the exec column last when any step defines non-auto exec", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, exec = "split", tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        v <- pip_view(p, tags = "core")

        expect_equal(
            get_view_header(v),
            c("step", "depends", "out", "state", "tags", "exec")
        )
    })
})
