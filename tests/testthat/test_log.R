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


describe("pipeflow_lgr", {
    timePattern <- paste0(
        "[0-9]{4}-[0-9]{2}-[0-9]{2} ",
        "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,6})? UTC"
    )

    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = ~f1) x + 1)
    }

    it("is exported", {
        expect_true("pipeflow_lgr" %in% getNamespaceExports("pipeflow"))
    })

    it("writes '<level> [<UTC time>]: <msg>'", {
        expect_output(
            pipeflow_lgr("info", "hello"),
            paste0("^info \\[", timePattern, "\\]: hello$")
        )
    })

    it("writes the given level", {
        for (level in c("info", "warn", "error")) {
            expect_output(
                pipeflow_lgr(level, "msg"),
                paste0("^", level, " \\[", timePattern, "\\]: msg$")
            )
        }
    })

    it("writes one line per call", {
        out <- capture.output({
            pipeflow_lgr("info", "first")
            pipeflow_lgr("warn", "second")
        })

        expect_length(out, 2L)
        expect_match(out[[1]], "]: first$")
        expect_match(out[[2]], "]: second$")
    })

    it("returns NULL invisibly", {
        expect_output(expect_invisible(pipeflow_lgr("info", "x")))
        expect_output(res <- pipeflow_lgr("info", "x"))
        expect_null(res)
    })

    it("is the default logger of pip_run()", {
        expect_no_error(.assert_logger(pipeflow_lgr))
        expect_identical(formals(pip_run)[["lgr"]], quote(pipeflow_lgr))

        p <- test_pip()
        expect_output(
            pip_run(p),
            paste0("info \\[", timePattern, "\\]: Starting run of pipeflow")
        )
    })

    it("can be wrapped to add a prefix", {
        prefixed <- function(level, msg) {
            pipeflow_lgr(level, paste("[demo]", msg))
        }
        p <- test_pip()

        out <- capture.output(pip_run(p, lgr = prefixed))

        expect_gt(length(out), 0L)
        expect_true(all(grepl("]: [demo] ", out, fixed = TRUE)))
    })

    it("can be wrapped to drop levels and delegate the rest", {
        dropInfo <- function(level, msg) {
            if (level != "info") {
                pipeflow_lgr(level, msg)
            }
        }
        p <- pip_new("pipe") |>
            pip_add("f1", \(x = 1) {
                warning("careful")
                x
            })

        out <- capture.output(suppressWarnings(pip_run(p, lgr = dropInfo)))

        expect_length(out, 1L)
        expect_match(out, paste0("^warn \\[", timePattern, "\\]: careful$"))
    })
})
