# -------
# Helpers
# -------

describe(".pip_update_downstream",
{
    it("can update state downstream",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(x = ~-1) x)
        pip_add(p, "s3", \(x = ~s2) x)

        expect_equal(p$pipeline[["state"]], c("new", "new", "new"))
        .pip_update_downstream(p, "s1", what = "state", value = "outdated")
        expect_equal(p$pipeline[["state"]], c("new", "outdated", "outdated"))
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
        expect_true(.pip_is_pipeflow_pip(p))
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

        expect_equal(p$pipeline$refs[[2]], c(x = "s1"))
    })

    it("a bad relative step referal is signalled",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)

        expect_error(pip_add(p, "s2", \(x = ~-2) 2*x))
        expect_equal(pip_length(p), 1L)
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
        pip_add(p, "s1", \(x = 1, .self = NULL) pip_length(.self))
        expect_equal(p$pipeline[["fargs"]][[1]]$.self, p)
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


describe("pip_length",
{
    it("returns an integer value",
    {
        p <- pip_new()
        expect_true(is.integer(pip_length(p)))
    })

    it("returns the expected value",
    {
        p <- pip_new()
        expect_equal(pip_length(p), 0L)
        pip_add(p, "s1", \(a = 1) a)
        expect_equal(pip_length(p), 1L)
    })
})


describe("pip_run",
{
    it("runs all steps of the pipeline",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1:2) x)
        pip_add(p, "s2", \(x = ~-1) x + 1)
        pip_add(p, "s3", \(x = ~s2, y = ~s1) x + y)
        pip_run(p, lgr = NULL)
        expect_equal(p$pipeline[["out"]], list(1:2, c(2, 3), c(3, 5)))
    })
})


describe("pip_view",
{
    it("returns a view object",
    {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, group = "io")

        v <- pip_view(p)
        expect_true(.pip_is_pipeflow_view(v))
        expect_identical(v[["pip"]], p)
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
