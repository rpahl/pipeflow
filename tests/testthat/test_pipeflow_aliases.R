

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
