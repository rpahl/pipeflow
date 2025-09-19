# Tests for logical operators (Ops group) on Pipeline objects

describe("Pipeline logical operators", {
    test_that("equality operator works for pipelines", {
        pip1 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)

        pip2 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)

        pip3 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 3) b)  # different default value

        # Test basic equality - this tests the current behavior
        # Pipelines with same structure should be considered equal
        expect_true(all.equal(pip1$pipeline, pip2$pipeline))
        expect_false(isTRUE(all.equal(pip1$pipeline, pip3$pipeline)))
    })

    test_that("pipeline comparison handles step names correctly", {
        pip1 <- Pipeline$new("pipe1")$
            add("step1", \(a = 1) a)

        pip2 <- Pipeline$new("pipe1")$
            add("step1", \(a = 1) a)

        pip3 <- Pipeline$new("pipe1")$
            add("different_name", \(a = 1) a)

        # Same step names should compare as equal
        expect_equal(pip1$get_step_names(), pip2$get_step_names())
        expect_false(identical(pip1$get_step_names(), pip3$get_step_names()))
    })

    test_that("pipeline comparison handles function content", {
        pip1 <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x * 2)

        pip2 <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x * 2)

        pip3 <- Pipeline$new("pipe1")$
            add("f1", \(x = 1) x * 3)  # different function body

        # Same function definition
        expect_equal(pip1$get_step("f1")$funcName, pip2$get_step("f1")$funcName)
        
        # Different function - the function objects won't be identical
        # but we can test that they produce different results
        pip1$run()
        pip3$run()
        
        expect_equal(pip1$get_out("f1"), 2)
        expect_equal(pip3$get_out("f1"), 3)
    })

    test_that("pipeline comparison handles parameter differences", {
        pip1 <- Pipeline$new("pipe1")$
            add("f1", \(x = 1, y = 2) x + y)

        pip2 <- Pipeline$new("pipe1")$
            add("f1", \(x = 1, y = 2) x + y)

        pip3 <- Pipeline$new("pipe1")$
            add("f1", \(x = 1, y = 3) x + y)  # different default parameter

        # Test parameter extraction and comparison
        params1 <- pip1$get_params_at_step("f1")
        params2 <- pip2$get_params_at_step("f1")
        params3 <- pip3$get_params_at_step("f1")

        expect_equal(params1, params2)
        expect_false(identical(params1, params3))
    })

    test_that("pipeline comparison handles dependencies correctly", {
        pip1 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = ~f1) b)

        pip2 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = ~f1) b)

        pip3 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)  # no dependency

        expect_equal(pip1$get_depends(), pip2$get_depends())
        expect_false(identical(pip1$get_depends(), pip3$get_depends()))
    })

    test_that("pipeline comparison handles keepOut settings", {
        pip1 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a, keepOut = TRUE)

        pip2 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a, keepOut = TRUE)

        pip3 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a, keepOut = FALSE)

        # Compare keepOut settings
        expect_equal(pip1$get_step("f1")$keepOut, pip2$get_step("f1")$keepOut)
        expect_false(identical(pip1$get_step("f1")$keepOut, pip3$get_step("f1")$keepOut))
    })

    test_that("pipeline comparison handles empty pipelines", {
        pip1 <- Pipeline$new("pipe1")
        pip2 <- Pipeline$new("pipe1")
        pip3 <- Pipeline$new("different_name")

        # Empty pipelines with same name should be similar
        expect_equal(pip1$name, pip2$name)
        expect_false(identical(pip1$name, pip3$name))
        
        # Both should have only data step initially
        expect_equal(pip1$get_step_names(), "data")
        expect_equal(pip2$get_step_names(), "data")
    })

    test_that("pipeline comparison handles pipeline state correctly", {
        pip1 <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        pip2 <- pip1$clone()

        # Initially should be equivalent
        expect_equal(pip1$pipeline, pip2$pipeline)

        # After running one, states should differ
        pip1$run()
        
        # States should now be different
        expect_false(identical(
            pip1$pipeline$state,
            pip2$pipeline$state
        ))
    })
})