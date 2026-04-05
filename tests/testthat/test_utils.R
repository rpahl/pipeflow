describe(".is_single",
{
    it("signals bad mode input",
    {
        expect_error(
            .is_single(1, mode = 1),
            "mode must be a single character string"
        )
        expect_error(
            .is_single(1, mode = c("not", "of", "length", "one")),
            "mode must be a single character string"
        )
    })

    it("works correctly for single character",
    {
        # TRUE
        expect_true(.is_single("", "character"))
        expect_true(.is_single(as.character(NA), "character"))
        expect_true(.is_single("a", "character"))

        # FALSE
        expect_false(.is_single(character(0), "character"))
        expect_false(.is_single(NULL, "character"))
        expect_false(.is_single(1, "character"))
        expect_false(.is_single(c("a", "b"), "character"))
        expect_false(.is_single(as.character(c(NA, NA)), "character"))
    })

    it("works correctly for single numeric", {
        # TRUE
        expect_true(.is_single(0, "numeric"))
        expect_true(.is_single(as.numeric(NA), "numeric"))
        expect_true(.is_single(Inf, "numeric"))

        # FALSE
        expect_false(.is_single(numeric(0), "numeric"))
        expect_false(.is_single(NULL, "numeric"))
        expect_false(.is_single("1", "numeric"))
        expect_false(.is_single(1:2, "numeric"))
        expect_false(.is_single(as.numeric(c(NA, NA)), "numeric"))
    })

    it("works correctly for single logical", {
        # TRUE
        expect_true(.is_single(TRUE, "logical"))
        expect_true(.is_single(FALSE, "logical"))
        expect_true(.is_single(as.logical(NA), "logical"))

        # FALSE
        expect_false(.is_single(logical(0), "logical"))
        expect_false(.is_single(c(TRUE, FALSE), "logical"))
        expect_false(.is_single(NULL, "logical"))
        expect_false(.is_single("not a boolean", "logical"))
        expect_false(.is_single(1, "logical"))
    })
})


describe("is_string",
{
    f <- is_string

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


describe("is_number",
{
    f <- is_number

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


describe("unlist1",
{
    f <- unlist1

    test_that("unlist1 unravels a list by one level",
    {
        l = list(a = list(x = list(1, 2)))

        expect_equal(as.numeric(unlist(l)), 1:2)
        expect_equal(f(l), list(a.x = list(1, 2)))
        expect_equal(f(l, use.names = FALSE), list(list(1, 2)))
    })
})



describe("pipeflow_replace_string",
{
    f <- pipeflow_replace_string

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
