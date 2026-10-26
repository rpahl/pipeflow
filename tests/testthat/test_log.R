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


describe(".formatted_duration", {
    it("shows seconds with milliseconds below a minute", {
        expect_equal(.formatted_duration(0), "0.000 s")
        expect_equal(.formatted_duration(0.2514), "0.251 s")
        expect_equal(.formatted_duration(59.5), "59.500 s")
    })

    it("shows minutes and seconds below an hour", {
        expect_equal(.formatted_duration(60), "1 min 0 s")
        expect_equal(.formatted_duration(125.4), "2 min 5 s")
        expect_equal(.formatted_duration(3599.4), "59 min 59 s")
    })

    it("shows hours and minutes from an hour on", {
        expect_equal(.formatted_duration(3600), "1 h 0 min")
        expect_equal(.formatted_duration(3 * 3600 + 5 * 60 + 20), "3 h 5 min")
    })

    it("takes a difftime in any unit", {
        expect_equal(
            .formatted_duration(as.difftime(2, units = "mins")),
            "2 min 0 s"
        )
        expect_equal(
            .formatted_duration(as.difftime(250, units = "secs") / 1000),
            "0.250 s"
        )
    })
})


describe("pip_run total time", {
    capture_logs <- function(expr_fun) {
        msgs <- character(0)
        lgr <- function(level, msg) msgs <<- c(msgs, msg)
        expr_fun(lgr)
        msgs
    }
    finished_secs <- function(msgs) {
        last <- msgs[grepl("^Finished in", msgs)]
        expect_length(last, 1L)
        as.numeric(sub(".* in ([0-9.]+) s .*$", "\\1", last))
    }

    it("is logged at the end of a run", {
        p <- pip_new("demo") |>
            pip_add("slow", \(x = 1) {
                Sys.sleep(0.1)
                x
            })
        msgs <- capture_logs(\(lgr) pip_run(p, lgr = lgr))
        expect_match(
            msgs[[length(msgs)]],
            paste0(
                "^Finished in [0-9]+\\.[0-9]{3} s ",
                "\\(1 done, 0 skipped, 0 failed\\)$"
            )
        )
        expect_gte(finished_secs(msgs), 0.09)
    })

    it("covers the whole run after a restart", {
        p <- pip_new("demo") |>
            pip_add("slow", \(x = 1) {
                Sys.sleep(0.1)
                x
            }) |>
            pip_add("restart", \(x = ~slow, n = 0) {
                if (n == 0) {
                    .self$set_params(list(n = 1))
                    .self$restart(force = FALSE)
                }
                x
            })
        msgs <- capture_logs(\(lgr) pip_run(p, lgr = lgr))
        expect_true(any(grepl("Restarting run", msgs, fixed = TRUE)))
        # The restarted run skips the done step "slow", so only the time of the first
        # run makes the total exceed its sleep.
        expect_gte(finished_secs(msgs), 0.09)
        expect_false(exists(".run_start", envir = p[["pipenv"]]))
    })

    it("is logged after a halt", {
        p <- pip_new("demo") |>
            pip_add("halt", \(x = 1) .self$halt())
        msgs <- capture_logs(\(lgr) pip_run(p, lgr = lgr))
        expect_match(msgs[[length(msgs)]], "^Finished in .* s .*$")
    })
})


describe("pip_run step counts", {
    finished_counts <- function(p, ...) {
        msgs <- character(0)
        lgr <- function(level, msg) msgs <<- c(msgs, msg)
        suppressWarnings(
            pip_run(p, lgr = lgr, ...),
            classes = "pipeflow_run_failed"
        )
        last <- msgs[grepl("^Finished in", msgs)]
        sub("^.*\\((.*)\\)$", "\\1", last)
    }

    it("counts done, skipped and failed steps", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("bad", \(x = ~a) stop("boom")) |>
            pip_add("after", \(x = ~bad) x) |>
            pip_add("other", \(x = ~a) x)

        expect_equal(
            finished_counts(p, on_error = "continue"),
            "2 done, 1 skipped, 1 failed"
        )
        # a and other are done now, bad fails again
        expect_equal(
            finished_counts(p, on_error = "continue"),
            "0 done, 3 skipped, 1 failed"
        )
    })

    it("counts a step that failed for some keys as failed", {
        p <- pip_new() |>
            pip_add("parts", \(x = list(a = 1, b = -1)) x, exec = "split") |>
            pip_add("fit", \(x = ~parts) if (x < 0) stop("negative") else x)

        expect_equal(
            finished_counts(p, on_error = "continue"),
            "1 done, 0 skipped, 1 failed"
        )
    })

    it("counts locked steps as skipped", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x)
        pip_run(p, lgr = NULL)
        pip_lock(p["b"])

        expect_equal(
            finished_counts(p, force = TRUE),
            "1 done, 1 skipped, 0 failed"
        )
    })

    it("counts each step once over a restart", {
        restart_pip <- function(force) {
            pip_new() |>
                pip_add("a", \(x = 1) x) |>
                pip_add("b", \(x = 1) x) |>
                pip_add("restart", \(x = ~a, n = 0) {
                    if (n == 0) {
                        .self$set_params(list(n = 1))
                        .self$restart(force = force)
                    }
                    x
                }) |>
                pip_add("c", \(x = 1) x)
        }
        # The restarted run skips the steps done before the restart
        p <- restart_pip(force = FALSE)
        pip_run(p["b"], lgr = NULL)
        expect_equal(finished_counts(p), "3 done, 1 skipped, 0 failed")

        # or runs them again
        p <- restart_pip(force = TRUE)
        pip_run(p["b"], lgr = NULL)
        expect_equal(finished_counts(p), "4 done, 0 skipped, 0 failed")
    })

    it("does not count a step twice over a halted restart", {
        n <- 0L
        p <- pip_new() |>
            pip_add("a", \(x = 1) {
                n <<- n + 1L
                if (n == 2L) {
                    .self$halt()
                }
                x
            }) |>
            pip_add("restart", \(x = 1) {
                .self$restart(force = TRUE)
                x
            })

        expect_equal(finished_counts(p), "2 done, 0 skipped, 0 failed")
    })

    it("counts the steps after a halt as skipped", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("halt", \(x = 1) .self$halt()) |>
            pip_add("b", \(x = 1) x)

        expect_equal(finished_counts(p), "2 done, 1 skipped, 0 failed")
    })

    it("counts a failed step that a later step outdated as failed", {
        p <- pip_new() |>
            pip_add("bad", \(x = 1) stop("boom")) |>
            pip_add("fix", \(x = 1) {
                .self$set_params(list(x = 2), step = "bad")
                x
            })

        expect_equal(
            finished_counts(p, on_error = "continue"),
            "1 done, 0 skipped, 1 failed"
        )
        expect_equal(unname(p[["bad", "state"]]), "outdated")
    })

    it("does not count steps removed in the run", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("modify", \(x = 1) {
                if ("a" %in% .self[["step"]]) {
                    .self$remove("a")
                    .self$restart(force = FALSE)
                }
                x
            })

        expect_equal(finished_counts(p), "1 done, 0 skipped, 0 failed")
    })

    it("counts a step that failed before a restart as failed", {
        p <- pip_new() |>
            pip_add("bad", \(x = 1) stop("boom")) |>
            pip_add("modify", \(x = 1) {
                if (!.self[["bad", "locked"]]) {
                    pip_lock(.self["bad"])
                    .self$restart(force = FALSE)
                }
                x
            })

        expect_equal(
            finished_counts(p, on_error = "continue"),
            "1 done, 0 skipped, 1 failed"
        )
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
