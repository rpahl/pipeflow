describe(".is_single", {
    it("signals bad mode input", {
        expect_error(
            .is_single(1, mode = 1),
            "mode must be a single character string"
        )
        expect_error(
            .is_single(1, mode = c("not", "of", "length", "one")),
            "mode must be a single character string"
        )
    })

    it("works correctly for single character", {
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

describe("unlist1", {
    f <- unlist1

    test_that("unlist1 unravels a list by one level", {
        l <- list(a = list(x = list(1, 2)))

        expect_equal(as.numeric(unlist(l)), 1:2)
        expect_equal(f(l), list(a.x = list(1, 2)))
        expect_equal(f(l, use.names = FALSE), list(list(1, 2)))
    })
})

describe("stop_no_call", {
    f1 <- function(x) {
        if (x > 5) stop("x is too big")
    }
    f2 <- function(x) {
        if (x > 5) stop_no_call("x is too big")
    }

    it("in contrast to standard stop gives an error without the call", {
        res1 <- tryCatch(f1(10), error = identity)
        expect_equal(deparse(res1$call), "f1(10)")

        res2 <- tryCatch(f2(10), error = identity)
        expect_true(is.null(res2$call))
    })
})


describe("formula_deps", {
    it("extracts dependencies from one-sided formulas", {
        expect_equal(formula_deps(list(x = ~s1)), c(x = "s1"))
        expect_equal(
            formula_deps(list(x = ~s1, y = ~s2)),
            c(x = "s1", y = "s2")
        )
    })

    it("works with unnamed lists", {
        expect_equal(formula_deps(list(~s1, ~s2)), c("s1", "s2"))
    })

    it("ignores non-formula values, keeping formula order", {
        expect_equal(
            formula_deps(list(x = ~s1, y = 2, z = "abc", w = TRUE)),
            c(x = "s1")
        )
        expect_equal(
            formula_deps(list(x = ~s1, y = ~s2, z = "~s3")),
            c(x = "s1", y = "s2")
        )
    })

    it("returns an empty vector for empty or formula-free lists", {
        expect_equal(formula_deps(list()), character(0))
        expect_equal(formula_deps(list(a = 1, b = "x")), character(0))
        expect_equal(
            formula_deps(list(a = NULL, b = NA, c = 1.5)),
            character(0)
        )
    })

    it("extracts relative positional dependencies", {
        expect_equal(formula_deps(list(x = ~ -1)), c(x = "-1"))
        expect_equal(formula_deps(list(x = ~ -2)), c(x = "-2"))
    })

    it("treats strings starting with ~ as plain values, not formulas", {
        expect_equal(formula_deps(list(x = "~s1")), character(0))
    })

    it("ignores two-sided formulas", {
        expect_equal(formula_deps(list(f = y ~ x)), character(0))
    })

    it("detects formulas created by as.formula()", {
        expect_equal(
            formula_deps(list(x = as.formula("~s1"))),
            c(x = "s1")
        )
    })

    it("ignores plain language objects and calls", {
        # quote(~s1) is an unevaluated "~" call, not a formula
        expect_equal(formula_deps(list(x = quote(~s1))), character(0))
        expect_equal(formula_deps(list(x = quote(s1))), character(0))
        expect_equal(formula_deps(list(x = quote(foo()))), character(0))
    })

    it("ignores functions and vectors", {
        expect_equal(
            formula_deps(list(f = function(x) x, v = c(1, 2))),
            character(0)
        )
    })

    it("keeps the tail after the tilde for complex RHS", {
        expect_equal(formula_deps(list(x = ~ s1 + s2)), c(x = "s1 + s2"))
        expect_equal(formula_deps(list(x = ~ sqrt(s1))), c(x = "sqrt(s1)"))
        expect_equal(formula_deps(list(x = ~1)), c(x = "1"))
    })

    it("handles backticked step names", {
        expect_equal(
            formula_deps(list(x = ~`my step`)),
            c(x = "`my step`")
        )
    })
})
