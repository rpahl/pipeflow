
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


# .replace_string

test_that("x must be a character unless its of zero length", {
    expect_error(.replace_string(1:4, "1", "2"))
    expect_no_error(.replace_string(as.character(1:4), "1", "2"))

    x = list()
    expect_equal(.replace_string(x, "1", "2"), x)

    x = numeric(0)
    expect_equal(.replace_string(x, "1", "2"), x)
})

test_that("target and replacement must be strings", {

    f = .replace_string
    x = c("1", "2")

    expect_no_error(f(x, target = "1", replacement = "2"))

    expect_error(f(x, target = 1, replacement = "2"))
    expect_error(f(x, target = c("1", "2"), replacement = "2"))
    expect_error(f(x, target = "1", replacement = 2))
    expect_error(f(x, target = "1", replacement = c("1", "2")))
})

test_that("if target string does not exist, x is not altered", {

    x = character(0)
    target = "old"
    res = .replace_string(x, target, "new")
    expect_equal(res, x)

    x = c("a", "b", "c")
    res = .replace_string(x, target, "new")
    expect_equal(res, x)
})

test_that("if target string exists, it is replaced correctly", {

    x = c("a", "b", "c", "a")
    res = .replace_string(x, target = "a", replacement = "z")
    expect_equal(res, c("z", "b", "c", "z"))

    res = .replace_string(x, target = "b", replacement = "z")
    expect_equal(res, c("a", "z", "c", "a"))
})
