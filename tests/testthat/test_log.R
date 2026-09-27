describe(".formatted_time", {
    expectedTimePattern <- paste0(
        "^",
        "[0-9]{4}-[0-9]{2}-[0-9]{2} ",
        "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,6})? UTC",
        "$"
    )

    test_that("time is formatted in UTC", {
        ft <- .formatted_time()

        hasCorrectFormat <- grepl(pattern = expectedTimePattern, x = ft)
        expect_true(hasCorrectFormat)
    })

    test_that("time formatting is stable across local time zones", {
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
