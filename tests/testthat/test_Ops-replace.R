# Tests for replacement operations on Pipeline objects

describe("Pipeline replacement operations", {
    test_that("step replacement works correctly", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)$
            add("f3", \(c = ~f2) c, keepOut = TRUE)

        # Verify initial state
        out <- unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 2)

        # Replace step and verify change
        pip$replace_step("f2", \(z = 4) z)
        out <- unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 4)
    })

    test_that("data step can be replaced", {
        pip <- Pipeline$new("pipe1", data = 1)$
            add("f1", \(x = ~data) x * 2, keepOut = TRUE)

        # Initial result
        result1 <- pip$run()$collect_out()$f1
        expect_equal(result1, 2)

        # Replace data step
        pip$set_data(5)
        pip$reset()
        result2 <- pip$run()$collect_out()$f1
        expect_equal(result2, 10)
    })

    test_that("step replacement preserves dependencies", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = ~f1) b)$
            add("f3", \(c = ~f2) c)

        original_deps <- pip$get_depends()

        # Replace middle step but preserve dependency structure
        pip$replace_step("f2", \(new_b = ~f1) new_b * 2)

        new_deps <- pip$get_depends()
        
        # Dependencies should remain the same structure
        expect_equal(names(original_deps), names(new_deps))
        expect_equal(original_deps$f3, new_deps$f3)
        expect_equal(original_deps$f2, new_deps$f2)
    })

    test_that("step replacement with parameter changes", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x)

        # Get original parameters
        original_params <- pip$get_params_at_step("f1")
        expect_equal(original_params$x, 1)

        # Replace with different parameters
        pip$replace_step("f1", \(y = 5) y)
        new_params <- pip$get_params_at_step("f1")
        
        expect_equal(new_params$y, 5)
        expect_false("x" %in% names(new_params))
    })

    test_that("step replacement updates function name", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x)

        original_func_name <- pip$get_step("f1")$funcName
        expect_equal(original_func_name, "function")

        # Replace with named function
        pip$replace_step("f1", fun = "mean", params = list(x = 1:5))

        new_func_name <- pip$get_step("f1")$funcName
        expect_equal(new_func_name, "mean")
    })

    test_that("step replacement with keepOut changes", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x, keepOut = FALSE)

        expect_false(pip$get_step("f1")$keepOut)

        # Replace and change keepOut
        pip$replace_step("f1", \(x = 1) x, keepOut = TRUE)
        
        expect_true(pip$get_step("f1")$keepOut)
    })

    test_that("step replacement resets pipeline state", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x)

        # Run pipeline first
        pip$run()
        expect_equal(pip$get_step("f1")$state, "Done")

        # Replace step should reset state
        pip$replace_step("f1", \(y = 2) y)
        expect_equal(pip$get_step("f1")$state, "New")
    })

    test_that("step replacement handles string function names", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 3) x, keepOut = TRUE)

        # Replace with string function name
        .my_func <- \(x = 3) 2 * x
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        pip$replace_step("f1", fun = ".my_func", keepOut = TRUE)

        out <- unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 6)
    })

    test_that("step replacement allows parameter override", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1:3) x, keepOut = TRUE)

        .my_func <- \(x = 3) 2 * x
        assign(".my_func", .my_func, envir = globalenv())
        on.exit(rm(".my_func", envir = globalenv()))

        # Replace function and override default parameters
        pip$replace_step("f1", fun = ".my_func", params = list(x = 5), keepOut = TRUE)

        out <- unname(unlist(pip$run()$collect_out()))
        expect_equal(out, 10)
    })

    test_that("step replacement validates step existence", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x)

        expect_error(
            pip$replace_step("nonexistent", \(y = 2) y),
            "step 'nonexistent' does not exist"
        )
    })

    test_that("step replacement handles description and group changes", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x, description = "original", group = "group1")

        original_step <- pip$get_step("f1")
        expect_equal(original_step$description, "original")
        expect_equal(original_step$group, "group1")

        # Replace with new description and group
        pip$replace_step("f1", \(x = 2) x, description = "updated", group = "group2")

        updated_step <- pip$get_step("f1")
        expect_equal(updated_step$description, "updated")
        expect_equal(updated_step$group, "group2")
    })

    test_that("step replacement preserves step position", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)$
            add("f3", \(c = 3) c)

        original_names <- pip$get_step_names()
        f2_position <- which(original_names == "f2")

        # Replace middle step
        pip$replace_step("f2", \(new_b = 5) new_b)

        new_names <- pip$get_step_names()
        expect_equal(length(new_names), length(original_names))
        expect_equal(new_names[f2_position], "f2")
        expect_equal(new_names[-f2_position], original_names[-f2_position])
    })
})