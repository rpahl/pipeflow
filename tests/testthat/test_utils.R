
test_that("is_string",
{
    f <- is_string
    expect_true(is.function(f))

    test_that("detects a string",
    {
        expect_true(f(""))
        expect_true(f(as.character(NA)))
        expect_true(f("a"))
    })

    test_that("does not falsely detect a string",
    {
        expect_false(f(character(0)))
        expect_false(f(NULL))
        expect_false(f(1))
        expect_false(f(c("a", "b")))
        expect_false(f(as.character(c(NA, NA))))
    })
})

test_that("is_number",
{
    f <- is_number
    expect_true(is.function(f))

    test_that("detects a number",
    {
        expect_true(f(0))
        expect_true(f(as.numeric(NA)))
        expect_true(f(Inf))
    })

    test_that("does not falsely detect a number",
    {
        # FALSE
        expect_false(f(numeric(0)))
        expect_false(f(NULL))
        expect_false(f("1"))
        expect_false(f(1:2))
        expect_false(f(as.numeric(c(NA, NA))))
    })
})


test_that("unlist1",
{
    f <- unlist1
    expect_true(is.function(f))

    test_that("unlist1 unravels a list by one level",
    {
        l = list(a = list(x = list(1, 2)))

        expect_equal(as.numeric(unlist(l)), 1:2)
        expect_equal(f(l), list(a.x = list(1, 2)))
        expect_equal(f(l, use.names = FALSE), list(list(1, 2)))
    })
})



test_that("pipeflow_replace_string",
{
    f <- pipeflow_replace_string
    expect_true(is.function(f))

    test_that("x must be a character unless its of zero length",
    {
        expect_error(f(1:4, "1", "2"))
        expect_no_error(f(as.character(1:4), "1", "2"))

        x <- list()
        expect_equal(f(x, "1", "2"), x)

        x <- numeric(0)
        expect_equal(f(x, "1", "2"), x)
    })

    test_that("target and replacement must be strings",
    {
        x <- c("1", "2")

        expect_no_error(f(x, target = "1", replacement = "2"))

        expect_error(f(x, target = 1, replacement = "2"))
        expect_error(f(x, target = c("1", "2"), replacement = "2"))
        expect_error(f(x, target = "1", replacement = 2))
        expect_error(f(x, target = "1", replacement = c("1", "2")))
    })

    test_that("if target string does not exist, x is not altered",
    {
        x <- character(0)
        target <- "old"
        res <- f(x, target, "new")
        expect_equal(res, x)

        x <- c("a", "b", "c")
        res <- f(x, target, "new")
        expect_equal(res, x)
    })

    test_that("if target string exists, it is replaced correctly",
    {
        x <- c("a", "b", "c", "a")
        res <- f(x, target = "a", replacement = "z")
        expect_equal(res, c("z", "b", "c", "z"))

        res <- f(x, target = "b", replacement = "z")
        expect_equal(res, c("a", "z", "c", "a"))
    })
})


test_that("pipe_filter_params",
{
    f <- pipe_filter_params
    expect_true(is.function(f))

    x1 = StringParam(name = "x1", source = "s1", advanced = TRUE)
    x2 = StringParam(name = "x2", source = "s2", advanced = TRUE)
    z1 = StringParam(name = "z1", source = "s1", advanced = FALSE)
    z2 = StringParam(name = "z2", source = "s2", advanced = FALSE)

    pip <- Pipeline$new("testpipe") |>
        pipe_add(
            "step1",
            function(
                x1 = new("StringParam", "x1", source = "s1", advanced = TRUE),
                x2 = new("StringParam", "x2", source = "s2", advanced = TRUE),
                y = 3
            ) {}
        ) |>
        pipe_add(
            "step2",
            function(
                z1 = new("StringParam", "z1", source = "s1", advanced = FALSE),
                z2 = new("StringParam", "z2", source = "s2", advanced = FALSE)
            ) {}
        )

    test_that("filters parameters as expected",
    {
        res <- f(pip, "source" = "s1")
        expect_equal(res, list(x1 = x1, z1 = z1))

        res <- f(pip, "source" = "s2")
        expect_equal(res, list(x2 = x2, z2 = z2))

        res <- f(pip, "advanced" = TRUE)
        expect_equal(res, list(x1 = x1, x2 = x2))

        res <- f(pip, "advanced" = FALSE)
        expect_equal(res, list(z1 = z1, z2 = z2))

        res <- f(pip, "source" = "s1", "advanced" = TRUE)
        expect_equal(res, list(x1 = x1))

        res <- f(pip, "source" = "s2", "advanced" = FALSE)
        expect_equal(res, list(z2 = z2))

        res <- f(pip, name = "x2")
        expect_equal(res, list(x2 = x2))
    })

    test_that("if value does not exist, an empty list is returned",
    {
        res <- f(pip, "source" = "foo")
        expect_equivalent(res, list())
    })

    test_that("if slot does not exist, an error is given",
    {
        expect_error(
            f(pip, "foo" = "s1"),
            "no slot of name \"foo\""
        )
    })
})


describe("stop_no_call",
{
    f1 <- function(x) {
        if (x > 5) stop("x is too big")
    }
    f2 <- function(x) {
        if (x > 5) stop_no_call("x is too big")
    }

    it("in contrast to standard stop gives an error without the call",
    {
        res1 <- tryCatch(f1(10), error = identity)
        expect_equal(deparse(res1$call), "f1(10)")

        res2 <- tryCatch(f2(10), error = identity)
        expect_true(is.null(res2$call))
    })
})
