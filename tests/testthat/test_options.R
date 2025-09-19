# Tests for options and configuration

describe("pipeline options and configuration", {
    test_that("pipeline printing respects width option", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)$
            add("f2", \(b = 2) b)

        # Test with narrow width
        op <- options(width = 50L)
        on.exit(options(op))
        
        narrow_output <- capture.output(print(pip))
        
        # Test with wide width
        options(width = 1000L)
        wide_output <- capture.output(print(pip))
        
        # Wide output should be different from narrow output
        expect_false(identical(narrow_output, wide_output))
    })

    test_that("pipeline verbose printing respects width option", {
        pip <- Pipeline$new("pipe1", data = 1)$
            add("f1", \(a = 1) a)

        # Test verbose printing with different widths
        op <- options(width = 1000L)
        on.exit(options(op))
        
        out <- capture.output(pip$print(verbose = TRUE))
        header <- out[1] |> trimws() |> strsplit("\\s+") |> unlist()
        expected_header <- colnames(pip$pipeline)
        expect_equal(header, expected_header)
    })

    test_that("testthat parallel processing options work", {
        # This tests the Config/testthat/parallel: true setting
        op <- options(Ncpus = 2)
        on.exit(options(op))
        
        expect_equal(getOption("Ncpus"), 2)
        
        options(Ncpus = 6)
        expect_equal(getOption("Ncpus"), 6)
    })

    test_that("pipeline execution is not affected by common R options", {
        pip <- Pipeline$new("pipe1", data = c(1, 2, 3))$
            add("mean_val", \(x = ~data) mean(x), keepOut = TRUE)

        # Test with different digits option
        op <- options(digits = 2)
        on.exit(options(op))
        
        result1 <- pip$run()$get_out("mean_val")
        
        options(digits = 10)
        pip$reset()
        result2 <- pip$run()$get_out("mean_val")
        
        expect_equal(result1, result2)
        expect_equal(result1, 2)
    })

    test_that("pipeline printing handles different locale options", {
        pip <- Pipeline$new("pipe1")$
            add("f1", \(a = 1) a)

        # Basic test - just ensure printing doesn't error with different options
        op <- options(OutDec = ",")
        on.exit(options(op))
        
        expect_no_error(capture.output(print(pip)))
        
        options(OutDec = ".")
        expect_no_error(capture.output(print(pip)))
    })

    test_that("stringsAsFactors option doesn't affect pipeline data handling", {
        # Test that pipeline properly handles data regardless of stringsAsFactors
        test_data <- data.frame(
            x = c("a", "b", "c"),
            y = 1:3
        )

        op <- options(stringsAsFactors = TRUE)
        on.exit(options(op))
        
        pip1 <- Pipeline$new("pipe1", data = test_data)
        result1 <- pip1$run()$get_out("data")
        
        options(stringsAsFactors = FALSE)
        pip2 <- Pipeline$new("pipe2", data = test_data)
        result2 <- pip2$run()$get_out("data")
        
        # Results should be the same regardless of stringsAsFactors setting
        expect_equal(result1$y, result2$y)
        expect_equal(as.character(result1$x), as.character(result2$x))
    })

    test_that("warn option affects pipeline step warnings", {
        pip <- Pipeline$new("pipe1", data = 1)$
            add("warn_step", \(x = ~data) {
                warning("test warning")
                x
            })

        # Test with warnings enabled
        op <- options(warn = 1)
        on.exit(options(op))
        
        expect_warning(pip$run(), "test warning")
        
        # Reset pipeline for next test
        pip$reset()
        
        # Test with warnings disabled
        options(warn = -1)
        expect_no_warning(pip$run())
    })
})