
describe("pipe_new",
{
    it("creates a new pipeline",
    {
        p <- pipe_new()
        expect_true(.pip_is_pipeflow_pipe(p))
    })
})


describe("pipe_add",
{
    foo <- \(a = 0) a

    it("signals if step is not a single non-empty string",
    {
        p <- pipe_new()
        expect_error(pipe_add(p, ""), "step must be a non-empty string")
        expect_error(pipe_add(p, c("a", "b")), "step must be a single string")
    })

    it("signals if fun is not a function",
    {
        p <- pipe_new()
        expect_error(
            pipe_add(p, "s1", fun = "not a function"),
            "fun must be a function"
        )
    })

    it("signals if group is not a non-empty single defined string",
    {
        p <- pipe_new()

        expect_error(
            pipe_add(p, "s1", fun = \() 1, group = list("not a string")),
            "group must be a non-empty valid string"
        )
        expect_error(
            pipe_add(p, "s1", fun = \() 1, group = ""),
            "group must be a non-empty valid string"
        )
        expect_error(
            pipe_add(p, "s1", fun = \() 1, group = c("a", "b")),
            "group must be a non-empty valid string"
        )
    })

    it("signals duplicate step names",
    {
        p <- pipe_new()
        pipe_add(p, "s1", foo)
        expect_error(pipe_add(p, "s1", foo), "step 's1' already exists")
    })

    it("signals undefined dependencies",
    {
        p <- pipe_new()
        expect_error(
            pipe_add(p, "s1", \(x = ~undefined) x),
            "dependency 'undefined' not found"

        )
    })

    it("step can refer to previous step by relative number",
    {
        p <- pipe_new()
        pipe_add(p, "s1", \(a = 5) a)
        pipe_add(p, "s2", \(x = ~-1) 2*x)

        expect_equal(p$pipeline$depends[[2]], c(x = "s1"))
    })

    it("a bad relative step referal is signalled",
    {
        p <- pipe_new()
        pipe_add(p, "s1", \(a = 5) a)

        expect_error(pipe_add(p, "s2", \(x = ~-2) 2*x))
        expect_equal(pipe_length(p), 1L)
    })

    it("if add is aborted, pipeline remains unchanged",
    {
        p <- pipe_new()
        pipe_add(p, "s1", \(a = 5) a)
        expect_error(
            pipe_add(p, "s2", \(x = ~-2) 2*x),
            "relative index -2 points outside pipeline"
        )
    })

    it("can refer to the pipeline itself via the .self argument",
    {
        p <- pipe_new()
        pipe_add(p, "s1", \(x = 1, .self = NULL) pipe_length(.self))
        expect_equal(p$pipeline[["params"]][[1]]$.self, p)
    })

    it("supports functions with wildcard arguments",
    {
        skip("move to test for pipe_run_step")
        foo <-\(x, ...) {
            my_mean(x, ...)
        }
        v <- c(1, 2, NA, 3, 4)
        pip <- Pipeline$new("pipe", data = v)

        params <- list(x = ~data, na.rm = TRUE)
        pip$add("mean", fun = foo, params = params)

        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], mean(v, na.rm = TRUE))

        pip$set_params_at_step("mean", list(na.rm = FALSE))
        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], as.numeric(NA))
    })
})


describe("pipe_get_step_number",
{
    it("returns the step number for a given step",
    {
        p <- pipe_new()
        pipe_add(p, "s1", \(a = 1) a)
        pipe_add(p, "s2", \(a = 1) a)
        expect_equal(pipe_get_step_number(p, "s1"), 1L)
        expect_equal(pipe_get_step_number(p, "s2"), 2L)
    })

    it("signals if step does not exist",
    {
        p <- pipe_new()
        expect_error(
            pipe_get_step_number(p, "non-existent"),
            "step 'non-existent' does not exist in the pipeline"
        )
    })
})


describe("pipe_has_step",
{
    it("can be checked if pipeline has a step",
    {
        p <- pipe_new()
        expect_false(pipe_has_step(p, "s1"))
        pipe_add(p, "s1", \(a = 1) a)
        expect_true(pipe_has_step(p, "s1"))
    })

    it("errors at bad step argument",
    {
        f <- pipe_has_step
        expect_error(f(p, list("not a character string")))
        expect_error(f(p, c("not", "a", "single", "string")))
        expect_error(f(p, NA))
        expect_error(f(p, ""))
        expect_error(f(p, 1))
    })
})


describe("pipe_length",
{
    it("returns an integer value",
    {
        p <- pipe_new()
        expect_true(is.integer(pipe_length(p)))
    })

    it("returns the expected value",
    {
        p <- pipe_new()
        expect_equal(pipe_length(p), 0L)
        pipe_add(p, "s1", \(a = 1) a)
        expect_equal(pipe_length(p), 1L)
    })
})
