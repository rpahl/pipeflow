
test_that("test for string works correctly",
{
    # TRUE
    expect_true(is_string(""))
    expect_true(is_string(as.character(NA)))
    expect_true(is_string("a"))

    # FALSE
    expect_false(is_string(character(0)))
    expect_false(is_string(NULL))
    expect_false(is_string(1))
    expect_false(is_string(c("a", "b")))
    expect_false(is_string(as.character(c(NA, NA))))
})

test_that("test for number works correctly",
{
    f = is_number

    # TRUE
    expect_true(f(0))
    expect_true(f(as.numeric(NA)))
    expect_true(f(Inf))

    # FALSE
    expect_false(f(numeric(0)))
    expect_false(f(NULL))
    expect_false(f("1"))
    expect_false(f(1:2))
    expect_false(f(as.numeric(c(NA, NA))))
})


test_that("unlist1 unravels a list by one level",
{
    l = list(a = list(x = list(1, 2)))

    expect_equal(as.numeric(unlist(l)), 1:2)
    expect_equal(unlist1(l), list(a.x = list(1, 2)))
    expect_equal(unlist1(l, use.names = FALSE), list(list(1, 2)))
})

test_that("not-in operator works as expected",
{
    expect_false(5 %in% 1:4)
    expect_true(5 %!in% 1:4)
})



# ternary NULL operator

test_that("ternary NULL-operator returns first value if not NULL",
{
    y = 9

    x = 0
    expect_equal(x %||% y, x)

    x = 1:10
    expect_equal(x %||% y, x)

    x = "a"
    expect_equal(x %||% y, x)
})

test_that("returns first value even if of zero length or NA",
{
    y = 9

    x = list()
    expect_equal(x %||% y, x)

    x = numeric(0)
    expect_equal(x %||% y, x)

    x = NA
    expect_equal(x %||% y, x)
})

test_that("returns second value if first is NULL",
{
    x = NULL

    y = 9
    expect_equal(x %||% y, y)

    y = list()
    expect_equal(x %||% y, y)

    y = NA
    expect_equal(x %||% y, y)

    y = NULL
    expect_equal(x %||% y, y)
})
