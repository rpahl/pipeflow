# Tests for pop_step and related functions

describe("pop_step", {
    test_that("last pipeline step can be popped", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        pip_copy <- pip$clone()

        pip$add("f2", \(b = 2) b)

        expect_equal(pip$length(), 3)
        expect_equal(pip_copy$length(), 2)

        res <- pip$pop_step()
        expect_equal(res, "f2")

        expect_equal(pip$length(), 2)
        expect_equal(pip, pip_copy)
    })

    test_that("pop_step returns the name of the removed step", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)

        res <- pip$pop_step()
        expect_equal(res, "f2")
        expect_equal(pip$get_step_names(), c("data", "f1"))
    })

    test_that("pop_step on empty pipeline (only data step) removes data", {
        pip <- Pipeline$new("pipe1", data = 1)
        expect_equal(pip$length(), 1)
        
        res <- pip$pop_step()
        expect_equal(res, "data")
        expect_equal(pip$length(), 0)
    })
})

describe("pop_steps_after", {
    test_that("all steps after a given step can be removed", {
        pip <- Pipeline$new("pipe1", data = 0)$
            add("f1", \(x = 1) x)$
            add("f2", \(x = ~f1) x)$
            add("f3", \(x = ~f2) x)

        steps <- pip$pop_steps_after("f1")
        expect_equal(steps, c("f2", "f3"))
        expect_equal(pip$get_step_names(), c("data", "f1"))
    })

    test_that("pop_steps_after signals if step does not exist", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        expect_error(
            pip$pop_steps_after("bad_step"),
            "step 'bad_step' does not exist"
        )
    })

    test_that("pop_steps_after returns empty character if no steps to remove", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        res <- pip$pop_steps_after("f1")
        expect_equal(res, character(0))
        expect_equal(pip$get_step_names(), c("data", "f1"))
    })
})

describe("pop_steps_from", {
    test_that("all steps from and including a given step can be removed", {
        pip <- Pipeline$new("pipe1", data = 0)$
            add("f1", \(x = 1) x)$
            add("f2", \(x = ~f1) x)$
            add("f3", \(x = ~f2) x)

        steps <- pip$pop_steps_from("f2")
        expect_equal(steps, c("f2", "f3"))
        expect_equal(pip$get_step_names(), c("data", "f1"))
    })

    test_that("pop_steps_from signals if step does not exist", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        expect_error(
            pip$pop_steps_from("bad_step"),
            "step 'bad_step' does not exist"
        )
    })

    test_that("pop_steps_from can remove all steps including data", {
        pip <- Pipeline$new("pipe1", data = 0)$
            add("f1", \(x = 1) x)

        res <- pip$pop_steps_from("data")
        expect_equal(res, c("data", "f1"))
        expect_equal(pip$get_step_names(), character(0))
    })
})