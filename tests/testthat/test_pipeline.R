
describe("pipe_new",
{
    it("creates a new pipeline",
    {
        p <- pipe_new()
    })
})


describe("pipe_add",
{
    it("can add a new step",
    {
        p <- pipe_new()
        pipe_add(p, "s1", \(a = 1) a)
        expect_equal(pipe_length(p), 1)

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
            pipe_add(p, "step1", fun = "not a function"),
            "fun must be a function"
        )
    })

    it("signals if group is not a non-empty single defined string",
    {
        p <- pipe_new()

        expect_error(
            pipe_add(p, "step1", fun = \() 1, group = list("not a string")),
            "group must be a non-empty valid string"
        )
        expect_error(
            pipe_add(p, "step1", fun = \() 1, group = ""),
            "group must be a non-empty valid string"
        )
        expect_error(
            pipe_add(p, "step1", fun = \() 1, group = c("a", "b")),
            "group must be a non-empty valid string"
        )
    })

    it("signals duplicate step names",
    {
        p <- pipe_new()
        pipe_add(p, "f1", foo)
        expect_error(pipe_add(p, "f1", foo), "step 'f1' already exists")
    })

    it("signals undefined dependencies",
    {
        p <- pipe_new()
        pipe_add(p, "f1", \(x = ~undefined) x)

        expect_error(
            pipe_add(p, "f2", \(x = ~undefined) x),
            "dependency 'undefined' not found"
        )
    })

    it("step can refer to previous step by relative number",
    {
        pip <- Pipeline$new("pipe1")
        pip$add("f1", \(a = 5) a)
        pip$add("f2", \(x = ~-1) 2*x)

        out <- pip$run()$collect_out()
        expect_equal(out[["f2"]][[1]], 10)

        pip$add("f3", \(x = ~-1, a = ~-2) x + a)
        out <- pip$run()$collect_out()
        expect_equal(out[["f3"]][[1]], 10 + 5)
    })

    it("a bad relative step referal is signalled",
    {
        pip <- Pipeline$new("pipe1")
        expect_error(
            pip$add("f1", \(x = ~-10) x),
            paste(
                "step 'f1': relative dependency x=-10",
                "points to outside the pipeline"
            ),
            fixed = TRUE
        )
    })

    it("added step can use lambda functions",
    {
        data <- 9
        pip <- Pipeline$new("pipe1", data = data)

        pip$add("f1", \(data = ~data) data)
        a <- 1
        pip$add("f2", \(a, b) a + b,
            params = list(a = a, b = ~f1)
        )

        expect_equal(unlist(pip$get_step("f1")[["depends"]]), c(data = "data"))
        expect_equal(unlist(pip$get_step("f2")[["depends"]]), c(b = "f1"))

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]][[1]], data)
        expect_equal(out[["f2"]][[1]], a + data)
    })


    it("supports functions with wildcard arguments",
    {
        my_mean <-\(x, na.rm = FALSE) {
            mean(x, na.rm = na.rm)
        }
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

    it("can have a variable defined outside as parameter default",
    {
        x <- 1

        pip <- Pipeline$new("pipe")$
            add("f1", \(a) a, params = list(a = x))

        expect_equal(pip$get_params_at_step("f1")$a, x)

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], x)
    })


    it("function can be passed as a string",
    {
        pip <- Pipeline$new("pipe")$
            add("f1", fun = "mean", params = list(x = 1:5))

        out <- pip$run()$collect_out()
        expect_equal(out[["f1"]], mean(1:5))

        expect_equal(pip$get_step("f1")[["funcName"]], "mean")
    })

    it("if passed as a function, name is derived from the function",
    {
        pip <- Pipeline$new("pipe")
        pip$add("f1", fun = mean, params = list(x = 1:5))
        expect_equal(pip$get_step("f1")[["funcName"]], "mean")

        pip <- Pipeline$new("pipe")$
            add("f1", fun = mean, params = list(x = 1:5))
        expect_equal(pip$get_step("f1")[["funcName"]], "mean")
    })

    it("lampda functions, are named 'function'",
    {
        pip <- Pipeline$new("pipe")$add("f1", fun = \(x = 1) x)
        expect_equal(pip$get_step("f1")[["funcName"]], "function")

        pip <- Pipeline$new("pipe")$
            add("f1", fun = \(x = 1) x)
        expect_equal(pip$get_step("f1")[["funcName"]], "function")
    })
})

describe("pipe_has_step",
{
    p <- pipe_new()

    it("can be checked if pipeline has a step",
    {
        expect_false(pipe_has_step(p, "f1"))
        pipe_add(p, "s1", \(a = 1) a)
        # expect_true(pipe_has_step(p, "s1"))
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
})
