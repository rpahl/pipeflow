# -------
# Helpers
# -------

describe(".pip_update_downstream",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("updates states downstream of single node as expected",
    {
        p <- test_pip()

        expect_true(all(p$pipeline[["state"]] == "new"))
        .pip_update_downstream(p, "a1", what = "state", value = "outdated")

        expect_equal(
            p$pipeline[["state"]],
            c("outdated", "outdated", "new", "outdated", "new")
        )
    })

    it("can update states downstream of multiple nodes",
    {
        p <- test_pip()
        .pip_update_downstream(p, c("a1", "b1"), what = "state", value = "outdated")
        expect_true(all(p$pipeline[["state"]] == "outdated"))

        p <- test_pip()
        .pip_update_downstream(p, c("a1", "b2"), what = "state", value = "outdated")

        expect_equal(
            p$pipeline[["state"]],
            c("outdated", "outdated", "new", "outdated", "outdated")
        )
    })
})


# ---------------------------
# Exported pipeline functions
# ---------------------------

describe("pip_new",
{
    it("creates a new pipeline",
    {
        p <- pip_new()
        expect_true(.is_pipeflow_pip(p))
    })
})


describe("pip_add",
{
    it("signals if step is not a single non-empty string",
    {
        p <- pip_new()
        expect_error(pip_add(p, ""), "step must be a non-empty string")
        expect_error(pip_add(p, c("a", "b")), "step must be a single string")
    })

    it("signals if fun is not a function",
    {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", fun = "not a function"),
            "fun must be a function"
        )
    })

    it("signals if group is not a non-empty single defined string",
    {
        p <- pip_new()

        expect_error(
            pip_add(p, "s1", fun = \() 1, group = list("not a string")),
            "group must be a non-empty valid string"
        )
        expect_error(
            pip_add(p, "s1", fun = \() 1, group = ""),
            "group must be a non-empty valid string"
        )
        expect_error(
            pip_add(p, "s1", fun = \() 1, group = c("a", "b")),
            "group must be a non-empty valid string"
        )
    })

    it("signals duplicate step names",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 0) a)
        expect_error(pip_add(p, "s1", \(a = 0) a), "step 's1' already exists")
    })

    it("signals undefined dependencies",
    {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", \(x = ~undefined) x),
            "cannot reference unknown steps: 'undefined'"
        )
    })

    it("step can refer to previous step by relative number",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)
        pip_add(p, "s2", \(x = ~-1) 2*x)

        expect_equal(p$pipeline$depends[[2]], c(x = "s1"))
    })

    it("a bad relative step referal is signalled",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)

        expect_error(pip_add(p, "s2", \(x = ~-2) 2*x))
        expect_equal(length(p), 1L)
    })

    it("if add is aborted, pipeline remains unchanged",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)
        expect_error(
            pip_add(p, "s2", \(x = ~-2) 2*x),
            "relative index -2 points outside pipeline"
        )
    })

    it("can refer to the pipeline itself via the .self argument",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1, .self = NULL) length(.self))
        expect_equal(p$pipeline[["params"]][[1]]$.self, p)
    })

    it("supports functions with wildcard arguments",
    {
        skip("move to test for pip_run_step")
        foo <-\(x, ...) {
        }
        v <- c(1, 2, NA, 3, 4)
        pip <- Pipeline$new("pipe", data = v)

        fargs <- list(x = ~data, na.rm = TRUE)
        pip$add("mean", fun = foo, fargs = fargs)

        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], mean(v, na.rm = TRUE))

        pip$set_params_at_step("mean", list(na.rm = FALSE))
        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], as.numeric(NA))
    })
})


describe("pip_bind",
{
    skip("TODO")
})


describe("pip_collect_out",
{
    it("returns empty list for empty pipeline",
    {
        p <- pip_new()
        expect_equal(pip_collect_out(p), list())
    })

    it("returns named flat list when grouped is FALSE",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, group = "data")
        pip_add(p, "s2", \(x = ~-1) x + 1, group = "model")

        p[["pipeline"]][["out"]] <- list(10, 20)

        out <- pip_collect_out(p, grouped = FALSE)
        expect_equal(names(out), c("s1", "s2"))
        expect_equal(unname(out), list(10, 20))
    })

    it("groups outputs if at least two steps share a group label",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, group = "data")
        pip_add(p, "s2", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "s3", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "s4", \(x = ~-1) x + 1, group = "final")

        p[["pipeline"]][["out"]] <- list("o1", "o2", "o3", "o4")

        out <- pip_collect_out(p, grouped = TRUE)

        expect_equal(names(out), c("data", "model", "final"))
        expect_equal(out[["data"]], "o1")
        expect_equal(out[["model"]], list(s2 = "o2", s3 = "o3"))
        expect_equal(out[["final"]], "o4")
    })

    it("works on pipeline views",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, group = "data")
        pip_add(p, "s2", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "s3", \(x = ~-1) x + 1, group = "model")

        p[["pipeline"]][["out"]] <- list("o1", "o2", "o3")

        v <- pip_view(p, filter = list(group = "model"))
        out <- pip_collect_out(v)

        expect_equal(names(out), "model")
        expect_equal(out[["model"]], list(s2 = "o2", s3 = "o3"))
    })

    it("signals invalid arguments",
    {
        p <- pip_new()
        expect_error(pip_collect_out(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_collect_out(p, grouped = c(TRUE, FALSE)),
            "grouped must be a single logical value"
        )
    })
})


describe("pip_get_params",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(data = data.frame(a = 1:2)) data) |>
            pip_add("s2", \(data = ~s1, x = 1) data[, 2] + x) |>
            pip_add("s3", \(y = ~s2) y) |>
            pip_add("s4", \(x = 9, y = ~s2) x + y)
    }

    it("returns independent params from a pipeline",
    {
        p <- test_pip()
        params <- pip_get_params(p)

        expect_named(params, c("data", "x"))
        expect_equal(params[["data"]], data.frame(a = 1:2))
        expect_equal(params[["x"]], 1)
    })

    it("returns params from a view subset only",
    {
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

    it("returns empty list for view without independent params",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(y = ~s1) y)

        v <- pip_view(p, filter = list(step = "s2"))
        expect_equal(pip_get_params(v), list())
    })

    it("signals invalid input",
    {
        expect_error(
            pip_get_params(1),
            "x must be a pipeflow pip or view"
        )
    })
})


describe("pip_has_step",
{
    it("can be checked if pipeline has a step",
    {
        p <- pip_new()
        expect_false(pip_has_step(p, "s1"))
        pip_add(p, "s1", \(a = 1) a)
        expect_true(pip_has_step(p, "s1"))
    })

    it("errors at bad step argument",
    {
        f <- pip_has_step
        expect_error(f(p, list("not a character string")))
        expect_error(f(p, c("not", "a", "single", "string")))
        expect_error(f(p, NA))
        expect_error(f(p, ""))
        expect_error(f(p, 1))
    })
})


describe("pip_run",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("load_raw", \(x = 1) x, group = "io") |>
            pip_add("fit_model", \(x = ~-1) x + 1, group = "model") |>
            pip_add("eval_model", \(x = ~fit_model) x, group = "model") |>
            pip_add("bla_bla", \(bla = "blabla") bla, group = "bla")
    }

    describe("standard runs",
    {
        it("runs all steps of the pipeline and marks them as done",
        {
            p <- test_pip()
            pip_run(p, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, 2, "blabla"))
            expect_equal(p$pipeline[["state"]], rep("done", 4))
        })

        it("marks downstream steps not reached due to abort as outdated",
        {
            p <- pip_new() |>
                pip_add("load_raw", \(x = 1) stop("io error"), group = "io") |>
                pip_add("fit_model", \(x = ~-1) x + 1, group = "model") |>
                pip_add("eval_model", \(x = ~fit_model) x, group = "model")

            expect_error(pip_run(p, lgr = NULL), "io error")
            expect_equal(p$pipeline[["state"]], c("failed", "outdated", "outdated"))
        })
    })

    describe("running views",
    {
        it("can run parts of the pipeline via views",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "bla"))
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(NULL, NULL, NULL, "blabla"))
        })

        it("runs all steps of the view plus upstream dependencies",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "model"))
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, 2, NULL))
        })

        it("marks downstream steps outside the view as outdated",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "io"))
            pip_run(v, lgr = NULL)
            expect_equal(
                p$pipeline[["state"]],
                c("done", "outdated", "outdated", "new")
            )
        })
    })
})


describe("pip_set_params",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1, data = data.frame(a = 1:2)) x) |>
            pip_add("s2", \(x = ~s1, y = 2) x + y) |>
            pip_add("s3", \(z = 3) z) |>
            pip_add("s4", \(a = ~s1, b = ~s3) a + b)
    }

    it("can be used with pip or view",
    {
        p <- test_pip()
        params <- list(x = 11, y = 22)

        res <- pip_set_params(p, params = params)
        expect_true(inherits(res, "pipeflow_pip"))

        v <- pip_view(p, filter = list(step = "s2"))
        res <- pip_set_params(v, params = params)
        expect_true(inherits(res, "pipeflow_view"))
    })

    it("sets independent parameters in a pipeline",
    {
        p <- test_pip()
        params <- list(x = 11, z = 33, data = data.frame(b = 3:4))

        pip_set_params(p, params = params)
        after <- p[["pipeline"]][["params"]]
        expect_equal(after[[1]][["x"]], 11)
        expect_equal(after[[1]][["data"]], data.frame(b = 3:4))
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("sets parameters only within the selected view",
    {
        p <- test_pip()

        v <- pip_view(p, filter = list(step = "s3"))
        pip_set_params(v, params = list(z = 33, x = 11, y = 22))
        after <- p[["pipeline"]][["params"]]

        expect_equal(after[[1]][["x"]], 1)
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("ignores locked steps",
    {
        p <- test_pip()
        p$pipeline[["locked"]][[1]] <- TRUE
        pip_set_params(p, params = list(x = 99))
        after <- p[["pipeline"]][["params"]]
        expect_equal(after[[1]][["x"]], 1)
    })

    it("warns for unused parameters when requested",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_warning(
            pip_set_params(p, params = list(foo = 1), warnUnused = TRUE),
            "Trying to set parameters not defined in the target: foo"
        )
    })

    it("marks changed and dependent downstream steps as 'outdated'",
    {
        p <- test_pip()
        pip_set_params(p, params = list(x = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("outdated", "outdated", "new", "outdated")
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
})


describe("pip_view",
{
    it("returns a view object with the expected structure",
    {
        p <- pip_new("test_pipeline")
        pip_add(p, "load", \(x = 1) x, group = "io")

        v <- pip_view(p)
        expect_true(.is_pipeflow_view(v))
        expect_true("rows" %in% names(v))
        expect_identical(v[["pip"]], p)
        expect_identical(v[["name"]], "test_pipeline view")
    })

    it("can filter by character columns with fixed matching",
    {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, group = "io")
        pip_add(p, "fit", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "eval", \(x = ~fit) x, group = "model")

        p[["pipeline"]][2, state := "done"]

        v <- pip_view(p,
            filter = list(group = "model", state = "done")
        )

        expect_equal(v[["rows"]], 2L)
    })

    it("can filter by tags",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~-1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~-1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["rows"]], c(1L, 3L))
    })

    it("can filter by regex when fixed is FALSE",
    {
        p <- pip_new()
        pip_add(p, "load_raw", \(x = 1) x, group = "io")
        pip_add(p, "fit_model", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "eval_model", \(x = ~fit_model) x, group = "model")

        v <- pip_view(
            p,
            filter = list(step = "_model$"),
            fixed = FALSE
        )

        expect_equal(v[["rows"]], c(2L, 3L))
    })

    it("intersects filtered rows with explicit i",
    {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, group = "g1")
        pip_add(p, "a2", \(x = ~-1) x, group = "g2")
        pip_add(p, "a3", \(x = ~-1) x, group = "g2")

        v <- pip_view(
            p,
            i = c(1L, 2L),
            filter = list(group = "g2")
        )

        expect_equal(v[["rows"]], 2L)
    })

    it("can be called on views to further subset rows",
    {
        p <- pip_new("test_pipeline")
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~-1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~-1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["rows"]], c(1L, 3L))
        expect_equal(v[["name"]], "test_pipeline view")

        v2 <- pip_view(v, tags = "report")
        expect_equal(v2[["rows"]], 3L)
        expect_equal(v2[["name"]], "test_pipeline view view")
    })

    it("signals invalid filter names",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, filter = list(not_a_column = "x")),
            "Invalid filter name"
        )
    })

    it("signals invalid row indices",
    {
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
})


# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

describe("length",
{
    it("returns an integer value",
    {
        p <- pip_new()
        expect_true(is.integer(length(p)))
    })

    it("returns the expected value",
    {
        p <- pip_new()
        expect_equal(length(p), 0L)
        pip_add(p, "s1", \(a = 1) a)
        pip_add(p, "s2", \(a = 1) a)
        expect_equal(length(p), 2L)

        v <- pip_view(p, 1)
        expect_equal(length(v), 1L)
    })
})


describe("benchmarking",
{
    skip("benchmarking tests are skipped by default")
    v <- c("hello", "world")
    w <- c("hello", "this", "is", "my", "world")
    grepv(pattern = v, x = w)
    `%chin%` <- data.table::`%chin%`

    N = 1e3
    u = as.character(as.hexmode(1:10000))
    y = sample(u,N,replace=TRUE)
    x = sample(u, 100)
    system.time(x %in% y)
    system.time(x %chin% y)

    microbenchmark(
        y %in% x,
        y %chin% x,
        times = 1000
    )


    # Please type 'example(chmatch)' to run this and see timings on your machine

    microbenchmark(x %in% y, x %chin% y, times = 100)
    system.time(a <- x %in% y)               #  4.5s
    system.time(b <- x %chin% y)             #  1.7s
    identical(a,b)

    # Different example with more unique strings ...
    u = as.character(as.hexmode(1:(N/10)))
    y = sample(u,N,replace=TRUE)
    x = sample(u,N,replace=TRUE)
    system.time(a <- match(x,y))               # 46s
    system.time(b <- chmatch(x,y))             # 16s
    identical(a,b)
})

