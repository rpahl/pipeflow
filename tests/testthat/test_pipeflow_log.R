# utils log

test_that("time can be formatted to contain GMT offset", {
    ft <- .get_formatted_time()

    hasCorrectFormat <- grepl(
        "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[+|-][0-9]{1}:00",
        x = ft
    )
    expect_true(hasCorrectFormat)
})



test_that("json logging works as expected",
{
    expect_no_error(set_log_layout("json"))
    on.exit(set_log_layout("text"))

    test_that("standard log call has all expected fields", {
        out <- utils::capture.output(.log("my message"))
        fields <- jsonlite::fromJSON(out)

        expected_fields = c(
            "application",
            "level",
            "time",
            "message"
        )
        expect_equal(names(fields), expected_fields)
    })

    test_that("log application is this package name", {
        out <- utils::capture.output(.log("my message"))
        fields <- jsonlite::fromJSON(out)

        expect_equal(fields[["application"]], methods::getPackageName())
    })

    test_that("log has time", {
        out <- utils::capture.output(.log("my message"))
        fields <- jsonlite::fromJSON(out)

        expect_true(hasName(fields, "time"))
    })

    test_that("additional log fields can be added on the fly", {
        out <- utils::capture.output({
            .log("my message", my_field = "hello", some_type = "foo")
        })
        fields <- jsonlite::fromJSON(out)
        expect_equal(fields[["my_field"]], "hello")
        expect_equal(fields[["some_type"]], "foo")
    })

    test_that("additional log fields can be passed as variables", {
        s <- "hello"
        out <- utils::capture.output(.log("my message", my_field = s))
        fields <- jsonlite::fromJSON(out)

        expect_equal(fields[["my_field"]], s)
    })

    test_that("info log has level 'info'", {
        out <- utils::capture.output(log_info("my message"))
        fields <- jsonlite::fromJSON(out)

        expect_equal(fields[["level"]], "info")
    })

    test_that("warn log has level 'warn'", {
        out <- utils::capture.output(log_warn("my message"))
        fields <- jsonlite::fromJSON(out)

        expect_equal(fields[["level"]], "warn")
    })

    test_that("error log has level 'error'", {
        out <- utils::capture.output(log_error("my message"))
        fields <- jsonlite::fromJSON(out)

        expect_equal(fields[["level"]], "error")
    })
})
