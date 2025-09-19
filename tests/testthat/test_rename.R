# Tests for rename_step functions

describe("rename_step", {
    test_that("pipeline step can be renamed", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)

        pip$rename_step(from = "f1", to = "first")

        expect_equal(pip$get_step_names(), c("data", "first", "f2"))
    })

    test_that("rename_step signals name clash", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)

        expect_error(
            pip$rename_step(from = "f1", to = "f2"),
            "step 'f2' already exists"
        )
    })

    test_that("rename_step renames dependencies as well", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = ~f1) b)$
            add("f3", \(a = ~f1, b = ~f2) a + b)

        pip$rename_step(from = "f1", to = "first")

        expect_equal(
            pip$get_depends(),
            list(
                data = character(0),
                first = character(0),
                f2 = c(b = "first"),
                f3 = c(a = "first", b = "f2")
            )
        )
    })

    test_that("rename_step validates that source step exists", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        expect_error(
            pip$rename_step(from = "nonexistent", to = "new_name"),
            "step 'nonexistent' does not exist"
        )
    })

    test_that("rename_step validates target name is not empty", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        expect_error(
            pip$rename_step(from = "f1", to = ""),
            "step name must not be empty"
        )
    })

    test_that("rename_step updates complex dependency chains correctly", {
        pip <- Pipeline$new("pipe1", data = 1)$
            add("step_a", \(x = ~data) x)$
            add("step_b", \(x = ~step_a) x)$
            add("step_c", \(x = ~step_b, y = ~step_a) x + y)$
            add("final", \(result = ~step_c) result)

        pip$rename_step(from = "step_a", to = "renamed_step")

        deps <- pip$get_depends()
        expect_equal(deps$step_b, c(x = "renamed_step"))
        expect_equal(deps$step_c, c(x = "step_b", y = "renamed_step"))
        expect_equal(deps$final, c(result = "step_c"))
    })

    test_that("rename_step works with data step", {
        pip <- Pipeline$new("pipe1", data = 1)$
            add("f1", \(x = ~data) x)

        pip$rename_step(from = "data", to = "input_data")

        expect_equal(pip$get_step_names(), c("input_data", "f1"))
        expect_equal(pip$get_depends()$f1, c(x = "input_data"))
    })

    test_that("rename_step preserves step execution order", {
        pip <- Pipeline$new("pipe1")$
            add("a", \(x = 1) x)$
            add("b", \(x = ~a) x)$
            add("c", \(x = ~b) x)

        original_order <- pip$get_step_names()
        pip$rename_step(from = "b", to = "middle")
        new_order <- pip$get_step_names()

        expect_equal(length(original_order), length(new_order))
        expect_equal(which(new_order == "middle"), which(original_order == "b"))
    })
})