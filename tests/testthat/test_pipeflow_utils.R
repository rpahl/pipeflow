
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


# pipeflow_replace_string

test_that("x must be a character unless its of zero length", {
    expect_error(pipeflow_replace_string(1:4, "1", "2"))
    expect_no_error(pipeflow_replace_string(as.character(1:4), "1", "2"))

    x = list()
    expect_equal(pipeflow_replace_string(x, "1", "2"), x)

    x = numeric(0)
    expect_equal(pipeflow_replace_string(x, "1", "2"), x)
})

test_that("target and replacement must be strings", {

    f = pipeflow_replace_string
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
    res = pipeflow_replace_string(x, target, "new")
    expect_equal(res, x)

    x = c("a", "b", "c")
    res = pipeflow_replace_string(x, target, "new")
    expect_equal(res, x)
})

test_that("if target string exists, it is replaced correctly", {

    x = c("a", "b", "c", "a")
    res = pipeflow_replace_string(x, target = "a", replacement = "z")
    expect_equal(res, c("z", "b", "c", "z"))

    res = pipeflow_replace_string(x, target = "b", replacement = "z")
    expect_equal(res, c("a", "z", "c", "a"))
})
