# utils log

test_that("time can be formatted to contain GMT offset", {
    ft <- .get_formatted_time()

    hasCorrectFormat <- grepl(
        "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[+|-][0-9]{1}:00",
        x = ft
    )
    expect_true(hasCorrectFormat)
})


# json logger

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


# tryCatchLog

test_that("tryCatchLog works as expected",
{
    expect_no_error(set_log_layout("json"))
    on.exit(set_log_layout("text"))

    test_that("message is catched and logged without interrupting computation",
    {
        foo = function(a, b) {
            res = a
            message("hi")
            res + b
        }

        res <- lgr::without_logging(tryCatchLog(foo(1, 2)))
        expect_equal(res, 1 + 2)

        out = capture.output(tryCatchLog(foo(1, 2)))
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "info")
        expect_equal(logFields[["message"]], "hi\n")
    })

    test_that("execution context is added to logged message",
    {
        foo = function(a, b) {
            res = a
            message("hi")
            res + b
        }

        out = capture.output(
            tryCatchLog(foo(1, 2), execution_context = "just testing")
        )
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "info")
        expect_equal(logFields[["message"]], "Context: just testing, hi\n")
    })


    test_that("warning is catched and logged without interrupting computation",
    {
        foo = function(a, b) {
            res = a
            warning("this is a warning")
            res + b
        }

        res <- lgr::without_logging(tryCatchLog(foo(1, 2)))
        expect_equal(res, 1 + 2)

        out = capture.output(tryCatchLog(foo(1, 2)))
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "warn")
        expect_equal(logFields[["message"]], "this is a warning")
        expect_equal(logFields[["warn"]], "this is a warning")
    })

    test_that("execution context is added to logged warning",
    {
        foo = function(a, b) {
            res = a
            warning("this is a warning")
            res + b
        }

        out = capture.output(
            tryCatchLog(foo(1, 2), execution_context = "just testing")
        )
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "warn")
        expect_equal(logFields[["message"]],
            "Context: just testing, this is a warning"
        )
        expect_equal(logFields[["warn"]], "this is a warning")
    })


    test_that("error is catched and logged and computation interrupted",
    {
        foo = function(a, b) {
            res = a
            stop("stop here")
            res + b
        }

        expect_error(lgr::without_logging(tryCatchLog(foo(1, 2))), "stop here")

        out = capture.output(expect_error(tryCatchLog(foo(1, 2))))
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "error")
        expect_equal(logFields[["message"]], "stop here")
        expect_equal(logFields[["error"]], "stop here")
    })

    test_that("execution context is added to logged error",
    {
        foo = function(a, b) {
            res = a
            warning("warn here")
            res + b
        }

        out = capture.output(
            tryCatchLog(foo(1, 2), execution_context = "just testing")
        )
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "warn")
        expect_equal(logFields[["message"]],
            "Context: just testing, warn here"
        )
        expect_equal(logFields[["warn"]], "warn here")
    })

    test_that("a custom error condition is catched as well",
    {
        cond = structure(list(message = "stop here", call = NULL))
        class(cond) = c("my_error", "error", "condition")

        foo = function(a, b) {
            res = a
            stop(cond)
            res + b
        }

        expect_error(
            lgr::without_logging(tryCatchLog(foo(1, 2))),
            class = "my_error"
        )

        out = capture.output(expect_error(tryCatchLog(foo(1, 2))))
        log = out[1]
        logFields = jsonlite::fromJSON(log)

        expect_equal(logFields[["level"]], "error")
        expect_equal(logFields[["message"]], "stop here")
        expect_equal(logFields[["error"]], "stop here")
    })

    test_that("unsupported condition throws",
    {
        cond = structure(list(message = "stop here", call = NULL))
        class(cond) = c("my_cond", "unsupported", "condition")

        foo = function(a, b) {
            res = a
            stop(cond)
            res + b
        }

        expect_error(
            lgr::without_logging(tryCatchLog(foo(1, 2))),
            "Unsupported condition"
        )
    })
})
