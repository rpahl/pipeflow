

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
        helper_fun <- paste0("pipe_", fun)
        expect_true(exists(helper_fun), info = fun)

        f <- get(helper_fun, envir = asNamespace("pipeflow"))
        expect_equal(
            toString(body(f)),
            gettextf("pip$%s, ...", fun),
            info = fun
        )
    }
})
