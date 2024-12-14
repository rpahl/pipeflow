
test_that("an alias function is defined for each member function
    with the correct body",
{
    skip_if(Sys.getenv("R_CODECOV_ENV") == "GITHUB_ACTION")

    pip <- Pipeline$new("pipe")
    funs2check <- sapply(names(pip), \(x) is.function(pip[[x]])) |>
        Filter(f = isTRUE) |>
        names() |>
        setdiff("initialize")

    for (fun in funs2check) {
        alias_fun <- paste0("pipe_", fun)
        expect_true(exists(alias_fun), info = fun)
    }
})


describe("pipe_replace_step",
{

    it("can have a variable defined outside as parameter default",
    {
        x <- 3
        pip <- pipe_new("pipe") |> pipe_add("f1", function(x = 1) x)
        pip |> pipe_replace_step("f1", fun = \(a) a, params = list(a = x))

        out <- pip$run()$get_out("f1")
        expect_equal(out, x)
    })

    it("handles Param object args",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", function(x = 1) x)
        pip |> pipe_replace_step(
            "f1",
            fun = \(a = new("NumericParam", "a", value = 3)) a
        )

        out <- pip$run()$get_out("f1")
        expect_equal(out, 3)
    })

    it("can have a Param object defined outside as parameter default",
    {
        x <- 3
        pip <- pipe_new("pipe") |> pipe_add("f1", function(x = 1) x)
        p <- new("NumericParam", "a", value = x)

        pip |> pipe_replace_step("f1", fun = \(a) a, params = list(a = p))

        pip <- Pipeline$new("pipe") |>
            pipe_add("f1", function(a) a, params = list(a = p))

        expect_equal(pip$get_params_at_step("f1")$a, p)
        out <- pip$run()$get_out("f1")
        expect_equal(out, x)
    })

    it("function can be passed as a string",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", function(x = 1) x)
        pip |> pipe_replace_step("f1", fun = "mean", params = list(x = 1:5))

        out <- pip$run()$get_out("f1")
        expect_equal(out, mean(1:5))
        expect_equal(pip$get_step("f1")[["funcName"]], "mean")
    })

    it("if passed as a function, name is derived from the function",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", function(x = 1) x)
        pip |> pipe_replace_step("f1", fun = mean, params = list(x = 1:5))

        out <- pip$run()$get_out("f1")
        expect_equal(out, mean(1:5))
        expect_equal(pip$get_step("f1")[["funcName"]], "mean")
    })

    it("lampda functions, are named 'function'",
    {
        pip <- pipe_new("pipe") |> pipe_add("f1", function(x = 1) x)
        pip |> pipe_replace_step("f1", fun = \(x = 1) x)
        expect_equal(pip$get_step("f1")[["funcName"]], "function")
    })
})
