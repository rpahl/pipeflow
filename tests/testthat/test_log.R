
describe(".formatted_time",
{
    expectedTimePattern <- paste0(
        "^",
        "[0-9]{4}-[0-9]{2}-[0-9]{2} ",
        "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,6})? UTC",
        "$"
    )

    test_that("time is formatted in UTC",
    {
        ft <- .formatted_time()

        hasCorrectFormat <- grepl(pattern = expectedTimePattern, x = ft)
        expect_true(hasCorrectFormat)
    })

    test_that("time formatting is stable across local time zones",
    {
        timezone <- Sys.getenv("TZ")
        on.exit(Sys.setenv(TZ = timezone))

        fixedTime <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")

        Sys.setenv(TZ = "UTC")
        ftUtc <- .formatted_time(fixedTime)

        Sys.setenv(TZ = "America/Los_Angeles")
        ftUsWest <- .formatted_time(fixedTime)

        expect_equal(ftUtc, ftUsWest)
        expect_true(endsWith(ftUsWest, " UTC"))
    })
})



describe("set_log_layout",
{
    it("signals undefined layout",
    {
        expect_error(
            set_log_layout("foo"),
            "unknown log layout 'foo'"
        )
    })

    describe("text layout",
    {
        lg <- lgr::get_logger(.this_package_name())

        it("returns a string with the message at the end",
        {
            out <- utils::capture.output(lg$info("my message"))
            expect_true(out |> endsWith("my message"))
        })


        test_that("info log has level 'info'",
        {
            out <- utils::capture.output(lg$info("my message"))
            expect_true(startsWith(out, "INFO"))
        })

        test_that("warn log has level 'warn'",
        {
            out <- utils::capture.output(lg$warn("my message"))
            expect_true(out |> startsWith("WARN"))
        })

        test_that("error log has level 'error'",
        {
            out <- utils::capture.output(lg$error("my message"))
            expect_true(out |> startsWith("ERROR"))
        })
    })

    describe("json layout",
    {
        lg <- set_log_layout("json")
        on.exit(set_log_layout("text"))

        test_that("standard log call has all expected fields",
        {
            out <- utils::capture.output(lg$info("my message"))
            fields <- jsonlite::fromJSON(out)

            expected_fields = c(
                "application",
                "level",
                "time",
                "message"
            )
            expect_equal(names(fields), expected_fields)
        })

        test_that("log application is this package name",
        {
            out <- utils::capture.output(lg$info("my message"))
            fields <- jsonlite::fromJSON(out)

            expect_equal(fields[["application"]], .this_package_name())
        })

        test_that("log has time",
        {
            out <- utils::capture.output(lg$info("my message"))
            fields <- jsonlite::fromJSON(out)

            expect_true(hasName(fields, "time"))
        })

        test_that("additional log fields can be added on the fly",
        {
            out <- utils::capture.output({
                lg$info("my message", my_field = "hello", some_type = "foo")
            })
            fields <- jsonlite::fromJSON(out)
            expect_equal(fields[["my_field"]], "hello")
            expect_equal(fields[["some_type"]], "foo")
        })

        test_that("additional log fields can be passed as variables",
        {
            s <- "hello"
            out <- utils::capture.output(lg$info("my message", my_field = s))
            fields <- jsonlite::fromJSON(out)

            expect_equal(fields[["my_field"]], s)
        })

        test_that("info log has level 'info'",
        {
            out <- utils::capture.output(lg$info("my message"))
            fields <- jsonlite::fromJSON(out)

            expect_equal(fields[["level"]], "info")
        })

        test_that("warn log has level 'warn'",
        {
            out <- utils::capture.output(lg$warn("my message"))
            fields <- jsonlite::fromJSON(out)

            expect_equal(fields[["level"]], "warn")
        })

        test_that("error log has level 'error'",
        {
            out <- utils::capture.output(lg$error("my message"))
            fields <- jsonlite::fromJSON(out)

            expect_equal(fields[["level"]], "error")
        })
    })
})
