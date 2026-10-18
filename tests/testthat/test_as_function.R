describe("as.function.pipeflow", {
    # Fixtures are defined here and not in a helper file: closures from a
    # helper would have the attached package environment as an ancestor, which
    # cannot be serialised without a warning when loaded with load_all().

    # A param value with a label; the step functions see only the value.
    annotated <- function(value, label) {
        structure(list(value = value, label = label), class = "pf_annotated")
    }

    registerS3method(
        "pip_param_value",
        class = "pf_annotated",
        method = function(x, ...) unclass(x)$value,
        envir = asNamespace("pipeflow")
    )

    demo_batches <- function() {
        noise <- c(0.31, -0.27, 0.42, -0.18, 0.05, -0.33)
        data.frame(
            batch = rep(c("A", "B", "C"), each = 6),
            x = rep(1:6, 3),
            y = rep(c(1, 3, 5), each = 6) + 2 * rep(1:6, 3) + noise
        )
    }

    # The inner pipeline: fits a polynomial to a batch (the slow step, with
    # `delay` seconds) and rounds the coefficients. `calls` counts how often
    # each step ran.
    demo_inner <- function(calls, delay = 0) {
        pip_new("inner") |>
            pip_add("data", \(data = NULL) data) |>
            pip_add(
                "fit",
                \(data = ~data, degree = annotated(1, "Degree")) {
                    calls$fit <- calls$fit + 1L
                    Sys.sleep(delay)
                    stats::lm(
                        y ~ stats::poly(x, degree, raw = TRUE),
                        data = data
                    )
                }
            ) |>
            pip_add(
                "coefs",
                \(fit = ~fit, digits = annotated(2, "Digits")) {
                    calls$coefs <- calls$coefs + 1L
                    round(stats::coef(fit), digits)
                }
            )
    }

    # The outer pipeline: split by batch, one inner pipeline per batch (`fun`),
    # one table.
    demo_outer <- function(fun, data = demo_batches(), params = list()) {
        outer <- pip_new("outer") |>
            pip_add("data", \(data = NULL) data) |>
            pip_add(
                "split_data",
                \(data = ~data, byVar = "batch") split(data, data[[byVar]]),
                exec = "split"
            ) |>
            pip_add("by_batch", fun, params = params) |>
            pip_add(
                "coef_table",
                \(coefs = ~by_batch) {
                    data.frame(
                        batch = names(coefs),
                        do.call(rbind, unname(coefs))
                    )
                },
                exec = "reduce"
            )
        pip_set_params(outer, list(data = data))
        outer
    }

    demo_nested <- function(delay = 0, cache = 16L) {
        calls <- new.env(parent = emptyenv())
        calls$fit <- 0L
        calls$coefs <- 0L
        inner <- demo_inner(calls, delay)
        fun <- as.function(
            inner,
            out = "coefs",
            bind = list(data = ~split_data),
            cache = cache
        )
        list(outer = demo_outer(fun), inner = inner, fun = fun, calls = calls)
    }

    run_outer <- function(demo) {
        pip_run(demo$outer, lgr = NULL)
        invisible(demo)
    }

    describe("as a step of an outer pipeline", {
        it("maps over the partitions and gives a partitioned output", {
            demo <- run_outer(demo_nested())

            out <- demo$outer[["by_batch", "out"]]
            expect_s3_class(out, "pipeflow_partitioned")
            expect_named(out, c("A", "B", "C"))
            expect_equal(unname(out$B[2L]), 1.92, tolerance = 1e-8)

            tbl <- demo$outer[["coef_table", "out"]]
            expect_equal(tbl$batch, c("A", "B", "C"))
            expect_equal(tbl[[2L]], c(1.28, 3.28, 5.28), tolerance = 1e-8)
        })

        it("makes the inner params params of the outer pipeline", {
            demo <- demo_nested()

            params <- pip_get_params(demo$outer)
            expect_true(all(c("degree", "digits") %in% names(params)))
            # the annotation travels with the stored value
            expect_s3_class(params$digits, "pf_annotated")
            expect_identical(params$digits$label, "Digits")
            # the bound input is a reference to the split step
            expect_identical(
                demo$outer[["by_batch", "depends"]][["data"]],
                "split_data"
            )
        })

        it("outdates the step when an inner param changes", {
            demo <- run_outer(demo_nested())
            expect_identical(demo$outer[["by_batch", "state"]], "done")

            pip_set_params(demo$outer, list(digits = 3))

            expect_identical(demo$outer[["by_batch", "state"]], "outdated")
            expect_identical(demo$outer[["coef_table", "state"]], "outdated")
        })

        it("hands plain values to the inner pipeline", {
            demo <- run_outer(demo_nested())

            expect_equal(
                unname(demo$outer[["by_batch", "out"]]$A),
                c(1.28, 1.92),
                tolerance = 1e-8
            )
        })

        it("can be added to two outer pipelines", {
            demo <- demo_nested()
            second <- demo_outer(demo$fun)

            run_outer(demo)
            run_outer(list(outer = second))
            expect_identical(demo$calls$fit, 3L) # all from the kept copies
            expect_equal(
                second[["coef_table", "out"]],
                demo$outer[["coef_table", "out"]]
            )

            # the kept copies hold the params of the last call, which is
            # not an obstacle for the other pipeline
            pip_set_params(demo$outer, list(digits = 3))
            run_outer(demo)
            run_outer(list(outer = second))
            expect_equal(
                second[["coef_table", "out"]][[2L]],
                c(1.28, 3.28, 5.28),
                tolerance = 1e-8
            )
        })

        it("takes the values of inner params from params of pip_add()", {
            demo <- demo_nested()
            outer <- demo_outer(demo$fun, params = list(digits = 1))

            expect_equal(pip_get_params(outer)$digits, 1)
            pip_run(outer, lgr = NULL)
            expect_equal(
                unname(outer[["by_batch", "out"]]$A),
                c(1.3, 1.9),
                tolerance = 1e-8
            )
        })

        it("binds via params of pip_add(), but keeps one pipeline", {
            calls <- new.env(parent = emptyenv())
            calls$fit <- 0L
            calls$coefs <- 0L
            fun <- as.function(demo_inner(calls), out = "coefs")
            outer <- demo_outer(fun, params = list(data = ~split_data))

            expect_identical(
                outer[["by_batch", "depends"]][["data"]],
                "split_data"
            )
            pip_run(outer, lgr = NULL)
            expect_equal(
                outer[["coef_table", "out"]][[2L]],
                c(1.28, 3.28, 5.28),
                tolerance = 1e-8
            )
            # no bound argument tells the partitions apart
            expect_length(.fun_pips(fun), 1L)
        })
    })

    describe("as a plain function", {
        it("has the unbound params as arguments", {
            inner <- demo_inner(new.env())
            f <- as.function(inner, out = "coefs")

            expect_named(formals(f), c("data", "degree", "digits"))
            expect_identical(formals(f)$digits, annotated(2, "Digits"))

            out <- f(data = demo_batches()[1:6, ], digits = 1)
            expect_equal(unname(out), c(1.3, 1.9))
        })

        it("has no arguments if there are no unbound params", {
            p <- pip_new("z") |> pip_add("a", \() 42)

            f <- as.function(p, out = "a")

            expect_null(formals(f))
            expect_identical(f(), 42)
        })

        it("takes NULL values given via params of pip_add()", {
            p <- pip_new("n") |>
                pip_add("a", \(x) is.null(x), params = list(x = NULL))

            f <- as.function(p, out = "a")

            expect_named(formals(f), "x")
            expect_true(f())
            expect_false(f(x = 1))
        })

        it("returns all outputs for out = NULL", {
            inner <- demo_inner(new.env())
            d <- demo_batches()[1:6, ]

            out <- as.function(inner)(data = d)

            expect_named(out, c("data", "fit", "coefs"))
            expect_identical(out$data, d)
        })

        it("returns one output or a named list of outputs for out", {
            inner <- demo_inner(new.env())
            d <- demo_batches()[1:6, ]

            one <- as.function(inner, out = "data")(data = d)
            two <- as.function(inner, out = c("data", "coefs"))(data = d)

            expect_identical(one, d)
            expect_named(two, c("data", "coefs"))
        })

        it("needs the bound arguments in a direct call", {
            inner <- demo_inner(new.env())
            f <- as.function(inner, bind = list(data = ~split_data))

            expect_error(f(), "'data' is bound to a step")
            expect_no_error(f(data = demo_batches()[1:6, ]))
        })

        it("takes all arguments, also those starting with a dot", {
            p <- pip_new("dot") |> pip_add("s", \(.x = 1, .y = 2) .x - .y)

            f <- as.function(p, out = "s")

            expect_named(formals(f), c(".x", ".y"))
            expect_identical(f(), -1)
            expect_identical(f(.x = 9), 7)
            expect_identical(f(.y = 0), 1)
        })

        it("needs a bound argument that starts with a dot", {
            p <- pip_new("dot") |> pip_add("s", \(.x = 1) .x)

            f <- as.function(p, out = "s", bind = list(.x = ~input))

            expect_error(f(), "'.x' is bound to a step \\(~input\\)")
            expect_identical(f(.x = 9), 9)
        })

        it("is not confused by arguments named like the functions it uses", {
            p <- pip_new("shadow") |>
                pip_add(
                    "s",
                    \(
                        list = \(x) x + 1,
                        c = \(x) x + 2,
                        missing = \(x) x + 3,
                        .pipRunner = \(x) x + 4
                    ) {
                        list(1) + c(1) + missing(1) + .pipRunner(1)
                    }
                )

            f <- as.function(p, out = "s", bind = list(c = ~input))

            expect_identical(f(c = \(x) x * 10), 21)
            expect_identical(f(c = \(x) x * 10, list = \(x) x), 20)
            expect_identical(as.function(p, out = "s")(), 14)
        })

        it("evaluates stored calls and symbols as a plain pipeline does", {
            p <- pip_new("lang") |>
                pip_add("s", \(x = quote(quote(1 + 2)), y = quote(pi)) {
                    list(x = x, y = y)
                })
            pip_run(p, lgr = NULL)
            expected <- p[["s", "out"]]
            expect_identical(expected, list(x = quote(1 + 2), y = pi))

            f <- as.function(p, out = "s")
            expect_identical(f(), expected)
        })

        it("takes a formula as the value of a bound argument", {
            p <- pip_new("formula") |> pip_add("s", \(model = NULL) model)
            f <- as.function(p, out = "s", bind = list(model = ~source))
            model <- y ~ x

            expect_identical(f(model = model), model)

            outer <- pip_new("outer") |>
                pip_add("source", \(source = NULL) source) |>
                pip_add("run", f)
            pip_set_params(outer, list(source = model))
            pip_run(outer, lgr = NULL)

            expect_identical(outer[["run", "out"]], model)
        })

        it("takes NULL as the value of a bound argument", {
            p <- pip_new("null") |>
                pip_add("s", \(x = 1) if (is.null(x)) "none" else x)
            f <- as.function(p, out = "s", bind = list(x = ~source))

            expect_identical(f(x = NULL), "none")
            expect_identical(f(x = 2), 2)
        })

        it("runs a pipeline with split and reduce steps", {
            inner <- pip_new("i") |>
                pip_add("data", \(data = NULL) data) |>
                pip_add(
                    "parts",
                    \(data = ~data) split(data, data$batch),
                    exec = "split"
                ) |>
                pip_add("n", \(parts = ~parts) nrow(parts)) |>
                pip_add(
                    "total",
                    \(n = ~n) sum(unlist(n)),
                    exec = "reduce"
                )

            f <- as.function(inner, out = "total")

            expect_identical(f(data = demo_batches()), 18L)
        })
    })

    describe("snapshot of the pipeline", {
        it("does not change the pipeline it was made from", {
            demo <- run_outer(demo_nested())
            expect_true(all(demo$inner[["state"]] == "new"))

            f <- as.function(demo$inner)
            pip_replace(demo$inner, "coefs", \(fit = ~fit) "changed")
            out <- f(data = demo_batches()[1:6, ])

            expect_false(identical(out$coefs, "changed"))
            expect_true(all(demo$inner[["state"]] == "new"))
        })

        it("does not depend on whether the pipeline has run before", {
            inner <- demo_inner(new.env())
            d <- demo_batches()[1:6, ]
            fresh <- as.function(inner, out = "coefs")(data = d)

            pip_set_params(inner, list(data = demo_batches()[7:12, ]))
            pip_run(inner, lgr = NULL)
            afterRun <- as.function(inner, out = "coefs")(data = d)

            expect_identical(afterRun, fresh)
        })

        it("keeps the outputs of locked steps", {
            p <- pip_new("l") |>
                pip_add("a", \(x = 1) x * 2) |>
                pip_add("b", \(a = ~a, k = 10) a + k)
            pip_run(p, lgr = NULL)
            pip_lock(pip_view(p, step = "a"))

            f <- as.function(p, out = "b")

            expect_identical(f(x = 5), 12) # a stays 2
            expect_identical(f(k = 1), 3)
        })

        it("is made from a part of a pipeline with view = FALSE", {
            demo <- demo_nested()
            part <- demo$inner[1:2, view = FALSE]

            f <- as.function(part, out = "fit")

            expect_s3_class(f(data = demo_batches()[1:6, ]), "lm")
        })
    })

    describe("input checks", {
        it("rejects views", {
            inner <- demo_inner(new.env())

            expect_error(as.function(inner[1:2]), "not a view")
        })

        it("rejects an invalid out", {
            inner <- demo_inner(new.env())

            expect_error(as.function(inner, out = "nope"), "nope")
            expect_error(as.function(inner, out = 1L), "step names")
            expect_error(as.function(inner, out = character()), "step names")
            expect_error(as.function(inner, out = NA_character_), "step names")
        })

        it("rejects an invalid bind", {
            inner <- demo_inner(new.env())
            msg <- "named list of one-sided formulas"

            expect_error(as.function(inner, bind = list(~x)), msg)
            expect_error(as.function(inner, bind = list(data = "x")), msg)
            expect_error(as.function(inner, bind = list(data = y ~ x)), msg)
            expect_error(as.function(inner, bind = ~x), msg)
            expect_error(
                as.function(inner, bind = list(data = ~x, data = ~y)),
                msg
            )
        })

        it("rejects bindings of unknown params", {
            inner <- demo_inner(new.env())

            expect_error(
                as.function(inner, bind = list(nope = ~x)),
                "no unbound params of the pipeline: nope"
            )
        })

        it("rejects an invalid cache", {
            inner <- demo_inner(new.env())
            msg <- "cache must be a single whole number >= 0"

            expect_error(as.function(inner, cache = -1), msg)
            expect_error(as.function(inner, cache = "a"), msg)
            expect_error(as.function(inner, cache = c(1, 2)), msg)
            expect_error(as.function(inner, cache = NA_real_), msg)
            expect_error(as.function(inner, cache = NaN), msg)
            expect_error(as.function(inner, cache = -Inf), msg)
            expect_error(as.function(inner, cache = 0.5), msg)
            expect_error(as.function(inner, cache = 2.5), msg)
        })

        it("rejects an invalid on_error", {
            inner <- demo_inner(new.env())

            expect_error(
                as.function(inner, on_error = "later"),
                "should be one of"
            )
        })

        it("rejects an invalid lgr", {
            inner <- demo_inner(new.env())

            expect_error(as.function(inner, lgr = "a"), "lgr must be a")
            expect_error(
                as.function(inner, lgr = \(x) x),
                "lgr must be a function with arguments 'level' and 'msg'"
            )
        })

        it("accepts whole numbers as cache, also Inf and large ones", {
            inner <- demo_inner(new.env())
            d <- demo_batches()[1:6, ]

            for (cache in list(0, 1L, 16, Inf, .Machine$integer.max + 1)) {
                f <- as.function(inner, out = "data", cache = cache)
                expect_identical(f(data = d), d)
            }
        })

        it("does not capture the environment of the caller in bind", {
            inner <- demo_inner(new.env())
            plain <- as.function(inner, bind = list(data = ~split_data))
            f <- local({
                big <- numeric(1e6)
                as.function(inner, bind = list(data = ~split_data))
            })

            diff <- length(serialize(f, NULL)) - length(serialize(plain, NULL))

            expect_lt(abs(diff), 1e4)
        })
    })

    describe("reuse of the inner pipelines", {
        it("runs no inner step again for an unchanged outer run", {
            demo <- run_outer(demo_nested())
            expect_identical(demo$calls$fit, 3L)

            run_outer(demo)

            expect_identical(demo$calls$fit, 3L)
        })

        it("reruns only the steps after a changed param", {
            demo <- run_outer(demo_nested())

            pip_set_params(demo$outer, list(digits = 3))
            run_outer(demo)
            expect_identical(c(demo$calls$fit, demo$calls$coefs), c(3L, 6L))
            a <- demo_batches()[1:6, ]
            expected <- round(stats::coef(stats::lm(y ~ x, data = a)), 3)
            expect_equal(
                unname(demo$outer[["by_batch", "out"]]$A),
                unname(expected),
                tolerance = 1e-8
            )

            pip_set_params(demo$outer, list(degree = 2))
            run_outer(demo)
            expect_identical(c(demo$calls$fit, demo$calls$coefs), c(6L, 9L))
        })

        it("reruns only the partition whose data changed", {
            demo <- run_outer(demo_nested())
            d <- demo_batches()
            d$y[d$batch == "B"] <- d$y[d$batch == "B"] + 1

            pip_set_params(demo$outer, list(data = d))
            run_outer(demo)

            expect_identical(c(demo$calls$fit, demo$calls$coefs), c(4L, 4L))
            expect_equal(
                demo$outer[["coef_table", "out"]][[2L]],
                c(1.28, 4.28, 5.28),
                tolerance = 1e-8
            )
        })

        it("keeps nothing with cache = 0", {
            demo <- run_outer(demo_nested(cache = 0))

            pip_set_params(demo$outer, list(digits = 3))
            run_outer(demo)

            expect_identical(demo$calls$fit, 6L)
            expect_length(.fun_pips(demo$fun), 0L)
        })

        it("keeps at most `cache` pipelines, the least recently used goes", {
            demo <- run_outer(demo_nested(cache = 2))
            expect_length(.fun_pips(demo$fun), 2L)

            # 3 partitions through 2 slots in the same order: no hit ever
            pip_set_params(demo$outer, list(digits = 3))
            run_outer(demo)

            expect_identical(demo$calls$fit, 6L)
        })

        it("drops the least recently used, not the oldest pipeline", {
            calls <- new.env()
            calls$fit <- 0L
            calls$coefs <- 0L
            f <- as.function(
                demo_inner(calls),
                out = "coefs",
                bind = list(data = ~split_data),
                cache = 2
            )
            d <- demo_batches()
            a <- d[d$batch == "A", ]
            b <- d[d$batch == "B", ]
            c <- d[d$batch == "C", ]

            f(data = a)
            f(data = b)
            f(data = a) # A is now more recent than B
            f(data = c) # B goes, not A
            expect_identical(calls$fit, 3L)

            f(data = a)
            expect_identical(calls$fit, 3L) # A is still kept

            f(data = b)
            expect_identical(calls$fit, 4L) # B was dropped
        })

        it("keeps all pipelines with cache = Inf", {
            demo <- demo_nested(cache = Inf)
            d <- demo_batches()[1:6, ]

            for (i in 1:20) {
                demo$fun(data = transform(d, y = y + i))
            }

            expect_length(.fun_pips(demo$fun), 20L)
        })

        it("keeps one pipeline without bound arguments", {
            calls <- new.env()
            calls$fit <- 0L
            calls$coefs <- 0L
            f <- as.function(demo_inner(calls), out = "coefs")
            d <- demo_batches()[1:6, ]

            f(data = d, digits = 1)
            f(data = d, digits = 2)

            expect_length(.fun_pips(f), 1L)
            expect_identical(c(calls$fit, calls$coefs), c(1L, 2L))
        })

        it("keeps the values of the last call in the pipelines", {
            demo <- run_outer(demo_nested())

            pip_set_params(demo$outer, list(digits = 3))
            run_outer(demo)

            kept <- .fun_pips(demo$fun)
            expect_length(kept, 3L)
            expect_identical(pip_get_params(kept[[1L]])$digits, 3)
        })

        it("is correct when a call matches the 'wrong' kept pipeline", {
            demo <- demo_nested()
            d <- demo_batches()
            a <- d[d$batch == "A", ]
            b <- d[d$batch == "B", ]

            first <- demo$fun(data = a)
            demo$fun(data = b)
            # same inputs as before, other params: from the kept pipeline
            again <- demo$fun(data = a, digits = 0)

            expect_equal(unname(first), c(1.28, 1.92), tolerance = 1e-8)
            expect_equal(unname(again), c(1, 2))
            # a fresh pipeline gives the same result as the reused one
            fresh <- as.function(demo$inner, out = "coefs", cache = 0)
            expect_equal(fresh(data = a, digits = 0), again)
        })

        it("reuses the steps that ran before a failed one", {
            state <- new.env()
            state$up <- 0L
            state$fail <- TRUE
            inner <- pip_new("retry") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("up", \(x = ~input) {
                    state$up <- state$up + 1L
                    x + 1
                }) |>
                pip_add("down", \(u = ~up) {
                    if (state$fail) {
                        stop("boom")
                    }
                    u * 2
                })
            f <- as.function(inner, out = "down", bind = list(x = ~source))

            expect_error(f(x = 1), "boom")
            expect_identical(state$up, 1L)

            # same input, so the same kept pipeline: only `down` runs again
            state$fail <- FALSE
            expect_identical(f(x = 1), 4)
            expect_identical(state$up, 1L)
            expect_length(.fun_pips(f), 1L)
        })
    })

    describe("binding", {
        it("takes relative references", {
            inner <- pip_new("rel") |> pip_add("s", \(x = 1) x * 2)
            outer <- pip_new("outer") |>
                pip_add("a", \(v = 5) v) |>
                pip_add(
                    "run",
                    as.function(inner, out = "s", bind = list(x = ~ -1))
                )

            pip_run(outer, lgr = NULL)

            expect_identical(outer[["run", "depends"]][["x"]], "a")
            expect_identical(outer[["run", "out"]], 10)
        })

        it("hands the failure of ~try(step) to the inner pipeline", {
            inner <- pip_new("try") |>
                pip_add("s", \(x = 1) {
                    failed <- inherits(x, "pipeflow_failure")
                    if (failed) "failed upstream" else x
                })
            outer <- pip_new("outer") |>
                pip_add("bad", \() stop("nope")) |>
                pip_add(
                    "run",
                    as.function(inner, out = "s", bind = list(x = ~ try(bad)))
                )

            suppressWarnings(
                pip_run(outer, lgr = NULL, on_error = "continue")
            )

            expect_identical(outer[["bad", "state"]], "failed")
            expect_identical(outer[["run", "out"]], "failed upstream")
        })

        it("matches on all bound arguments", {
            calls <- new.env()
            calls$n <- 0L
            inner <- pip_new("two") |>
                pip_add("in_a", \(a = NULL) a) |>
                pip_add("in_b", \(b = NULL) b) |>
                pip_add("total", \(a = ~in_a, b = ~in_b) {
                    calls$n <- calls$n + 1L
                    a + b
                })
            fun <- as.function(
                inner,
                out = "total",
                bind = list(
                    a = ~parts,
                    b = ~offset
                )
            )
            outer <- pip_new("outer") |>
                pip_add(
                    "parts",
                    \(values = list(p = 1, q = 2)) values,
                    exec = "split"
                ) |>
                pip_add("offset", \(offset = 10) offset) |>
                pip_add("run", fun)

            run_outer(list(outer = outer))
            expect_identical(calls$n, 2L)
            expect_equal(unlist(outer[["run", "out"]]), c(p = 11, q = 12))

            # a changed bound input is a new input: all partitions run
            pip_set_params(outer, list(offset = 20))
            run_outer(list(outer = outer))
            expect_identical(calls$n, 4L)
            expect_equal(unlist(outer[["run", "out"]]), c(p = 21, q = 22))

            # back to the old input: the kept pipelines are found
            pip_set_params(outer, list(offset = 10))
            run_outer(list(outer = outer))
            expect_identical(calls$n, 4L)
            expect_length(.fun_pips(fun), 4L)

            # only the partition with the changed input runs
            pip_set_params(outer, list(values = list(p = 5, q = 2)))
            run_outer(list(outer = outer))
            expect_identical(calls$n, 5L)
            expect_equal(unlist(outer[["run", "out"]]), c(p = 15, q = 12))
        })
    })

    describe("continuing after errors of inner steps", {
        # `bad` fails for the input "boom". `report` takes its output via
        # `~try()`, `use` plainly, and `other` does not depend on it.
        failing_inner <- function(calls = new.env()) {
            calls$other <- 0L
            pip_new("failing") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("bad", \(x = ~input) {
                    if (identical(x, "boom")) {
                        stop("boom")
                    }
                    x
                }) |>
                pip_add("report", \(b = ~ try(bad)) {
                    if (inherits(b, "pipeflow_failure")) "no result" else b
                }) |>
                pip_add("use", \(b = ~bad) b) |>
                pip_add("other", \(x = ~input) {
                    calls$other <- calls$other + 1L
                    "other output"
                })
        }

        it("stops at the first failed inner step by default", {
            f <- as.function(failing_inner(), out = "report")

            # `~try(bad)` has no effect, since the run does not continue
            expect_error(f(x = "boom"), "step 'bad': boom")
            expect_identical(f(x = "fine"), "fine")
        })

        it("hands the failure to a ~try() step with on_error = continue", {
            f <- as.function(
                failing_inner(),
                out = "report",
                on_error = "continue"
            )

            expect_warning(
                out <- f(x = "boom"),
                "continued after 1 failed step"
            )
            expect_identical(out, "no result")
        })

        it("runs and keeps the steps that do not depend on the failure", {
            calls <- new.env()
            f <- as.function(
                failing_inner(calls),
                out = "other",
                on_error = "continue"
            )

            expect_warning(out <- f(x = "boom"), "continued")
            expect_identical(out, "other output")
            expect_identical(calls$other, 1L)

            pip <- .fun_pips(f)[[1L]]
            expect_identical(pip[["other", "state"]], "done")
            expect_identical(pip[["bad", "state"]], "failed")

            # the kept pipeline only retries the failed step
            expect_warning(f(x = "boom"), "continued")
            expect_identical(calls$other, 1L)
        })

        it("fails if a requested step failed or was not run", {
            failed <- as.function(
                failing_inner(),
                out = c("other", "bad"),
                on_error = "continue"
            )
            skipped <- as.function(
                failing_inner(),
                out = "use",
                on_error = "continue"
            )

            expect_error(
                suppressWarnings(failed(x = "boom")),
                "step 'bad': boom"
            )
            expect_error(
                suppressWarnings(skipped(x = "boom")),
                "step 'bad': boom"
            )
        })

        it("names the step that kept a requested step from running", {
            inner <- pip_new("two_failures") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("bad_a", \(x = ~input) stop("A failed")) |>
                pip_add("rescue", \(a = ~ try(bad_a)) "recovered") |>
                pip_add("bad_b", \(x = ~input) stop("B failed")) |>
                pip_add("result", \(r = ~rescue, b = ~bad_b) list(r, b))
            f <- as.function(inner, out = "result", on_error = "continue")

            err <- tryCatch(suppressWarnings(f(x = 1)), error = \(e) e)

            # `rescue` handled the failure of `bad_a`, so only `bad_b` kept
            # `result` from running
            expect_identical(err[["step"]], "bad_b")
            expect_match(conditionMessage(err), "step 'bad_b': B failed")
        })

        it("does not follow a failure past a locked step", {
            state <- new.env()
            state$fail <- FALSE
            inner <- pip_new("locked") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("bad_a", \(x = ~input) {
                    if (state$fail) {
                        stop("A failed")
                    }
                    x
                }) |>
                pip_add("kept", \(a = ~bad_a) "kept output") |>
                pip_add("bad_b", \(x = ~input) {
                    if (state$fail) {
                        stop("B failed")
                    }
                    x
                }) |>
                pip_add("result", \(k = ~kept, b = ~bad_b) list(k, b))
            pip_run(inner, lgr = NULL)
            pip_lock(pip_view(inner, step = "kept"))
            state$fail <- TRUE
            f <- as.function(inner, out = "result", on_error = "continue")

            err <- tryCatch(suppressWarnings(f(x = 1)), error = \(e) e)

            # `kept` is not run, so the failure of `bad_a` stops there
            expect_identical(err[["step"]], "bad_b")
        })

        it("keeps the key of a failed mapped inner step in the error", {
            inner <- pip_new("mapped") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("parts", \(x = ~input) x, exec = "split") |>
                pip_add("double", \(p = ~parts) {
                    if (is.character(p)) {
                        stop("not a number")
                    }
                    p * 2
                })
            f <- as.function(inner, out = "double", on_error = "continue")

            err <- tryCatch(
                suppressWarnings(f(x = list(a = 1, b = "x"))),
                error = \(e) e
            )

            expect_match(
                conditionMessage(err),
                "step 'double': key 'b': not a number"
            )
            expect_s3_class(err[["parent"]], "pipeflow_key_error")
            expect_identical(err[["parent"]][["key"]], "b")
        })

        it("does not return a kept output when an input fails later", {
            inner <- pip_new("stale") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("bad", \(x = ~input, mode = "ok") {
                    if (identical(mode, "boom")) {
                        stop("boom")
                    }
                    x
                }) |>
                pip_add("use", \(b = ~bad) b * 2)
            f <- as.function(
                inner,
                out = "use",
                bind = list(x = ~source),
                on_error = "continue"
            )

            expect_identical(f(x = 2), 4)
            expect_error(
                suppressWarnings(f(x = 2, mode = "boom")),
                "step 'bad': boom"
            )
        })

        it("reruns the ~try() step when the failed step succeeds again", {
            state <- new.env()
            state$fail <- TRUE
            inner <- pip_new("recover") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("bad", \(x = ~input) {
                    if (state$fail) {
                        stop("boom")
                    }
                    x
                }) |>
                pip_add("report", \(b = ~ try(bad)) {
                    if (inherits(b, "pipeflow_failure")) "no result" else b * 10
                })
            f <- as.function(
                inner,
                out = "report",
                bind = list(x = ~source),
                on_error = "continue"
            )

            expect_warning(out <- f(x = 3), "continued")
            expect_identical(out, "no result")

            state$fail <- FALSE
            expect_identical(f(x = 3), 30)
        })

        it("returns the requested outputs beside an unrequested failure", {
            for (cache in c(0, 16)) {
                f <- as.function(
                    failing_inner(),
                    out = c("report", "other"),
                    cache = cache,
                    on_error = "continue"
                )

                expect_warning(out <- f(x = "boom"), "continued")
                expect_identical(
                    out,
                    list(report = "no result", other = "other output")
                )
            }
        })

        it("lets the outer run succeed when the inner pipeline recovers", {
            fun <- as.function(
                failing_inner(),
                out = "report",
                bind = list(x = ~parts),
                on_error = "continue"
            )
            outer <- pip_new("outer") |>
                pip_add(
                    "parts",
                    \(values = list(a = "fine", b = "boom")) values,
                    exec = "split"
                ) |>
                pip_add("run", fun)

            # the outer run never sees an error, although `bad` failed for 'b'
            suppressWarnings(pip_run(outer, lgr = NULL))

            expect_identical(outer[["run", "state"]], "done")
            expect_identical(
                unlist(outer[["run", "out"]]),
                c(a = "fine", b = "no result")
            )
        })

        it("requests all steps with out = NULL", {
            f <- as.function(failing_inner(), on_error = "continue")

            expect_error(suppressWarnings(f(x = "boom")), "step 'bad': boom")
            expect_named(
                f(x = "fine"),
                c("input", "bad", "report", "use", "other")
            )
        })

        it("leaves it to the outer run what happens to the outer step", {
            fun <- as.function(
                failing_inner(),
                out = "use",
                bind = list(x = ~source),
                on_error = "continue"
            )
            outer <- pip_new("outer") |>
                pip_add("source", \(x = "boom") x) |>
                pip_add("run", fun) |>
                pip_add("after", \(r = ~run) r) |>
                pip_add("rescue", \(r = ~ try(run)) {
                    if (inherits(r, "pipeflow_failure")) "rescued" else r
                })

            suppressWarnings(pip_run(outer, lgr = NULL, on_error = "continue"))

            expect_identical(outer[["run", "state"]], "failed")
            expect_match(
                conditionMessage(outer[["run", "out"]]),
                "step 'bad': boom"
            )
            expect_null(outer[["after", "out"]])
            expect_identical(outer[["rescue", "out"]], "rescued")
        })

        it("retries only the failed step when the input comes again", {
            state <- new.env()
            state$up <- 0L
            state$fail <- TRUE
            inner <- pip_new("retry") |>
                pip_add("input", \(x = NULL) x) |>
                pip_add("up", \(x = ~input) {
                    state$up <- state$up + 1L
                    x + 1
                }) |>
                pip_add("down", \(u = ~up) {
                    if (state$fail) {
                        stop("boom")
                    }
                    u * 2
                })
            f <- as.function(
                inner,
                out = "down",
                bind = list(x = ~source),
                on_error = "continue"
            )

            expect_error(suppressWarnings(f(x = 1)), "step 'down': boom")
            expect_identical(state$up, 1L)

            state$fail <- FALSE
            expect_identical(f(x = 1), 4)
            expect_identical(state$up, 1L)
            expect_length(.fun_pips(f), 1L)
        })
    })

    describe("logging of the inner run", {
        collect_logs <- function(logs) {
            function(level, msg) logs$msgs <- c(logs$msgs, msg)
        }

        it("runs the inner pipeline silently by default", {
            f <- as.function(demo_inner(new.env()), out = "coefs")

            expect_silent(f(data = demo_batches()[1:6, ]))
        })

        it("logs the steps of the inner run with lgr", {
            logs <- new.env()
            f <- as.function(
                demo_inner(new.env()),
                out = "coefs",
                lgr = collect_logs(logs)
            )

            f(data = demo_batches()[1:6, ])

            expect_match(logs$msgs[[1L]], "Starting run of pipeflow 'inner'")
            expect_true(any(grepl("Step 3/3 coefs", logs$msgs, fixed = TRUE)))
        })

        it("logs the steps that are skipped on a later call", {
            logs <- new.env()
            d <- demo_batches()[1:6, ]
            f <- as.function(
                demo_inner(new.env()),
                out = "coefs",
                lgr = collect_logs(logs)
            )

            f(data = d)
            logs$msgs <- character()
            f(data = d)

            skipped <- grepl("skipping done step", logs$msgs, fixed = TRUE)
            expect_identical(sum(skipped), 3L)
        })

        it("logs one inner run per partition", {
            logs <- new.env()
            fun <- as.function(
                demo_inner(new.env()),
                out = "coefs",
                bind = list(data = ~split_data),
                lgr = collect_logs(logs)
            )

            pip_run(demo_outer(fun), lgr = NULL)

            starts <- grepl("Starting run", logs$msgs, fixed = TRUE)
            expect_identical(sum(starts), 3L)
        })
    })

    describe("limits of the reuse", {
        it("takes the first default of params with the same name", {
            p <- pip_new("same") |>
                pip_add("a", \(x = 1) x) |>
                pip_add("b", \(x = 2) x)
            pip_run(p, lgr = NULL)

            f <- as.function(p)

            # the pipeline itself keeps both defaults ...
            expect_identical(c(p[["a", "out"]], p[["b", "out"]]), c(1, 2))
            # ... the function has one formal, as pip_get_params() has
            expect_identical(formals(f)$x, 1)
            expect_equal(unlist(f()), c(a = 1, b = 1))
            expect_equal(unlist(f(x = 5)), c(a = 5, b = 5))
        })

        it("does not see changes made in place to a reference input", {
            p <- pip_new("ref") |> pip_add("s", \(e = NULL) e$value)
            e <- new.env()
            e$value <- 1
            f <- as.function(p, out = "s")

            expect_identical(f(e = e), 1)
            e$value <- 2

            # as for the pipeline itself, which needs force = TRUE
            expect_identical(f(e = e), 1)
            expect_identical(as.function(p, out = "s", cache = 0)(e = e), 2)
        })

        it("does not rerun the inner pipelines in a forced outer run", {
            demo <- run_outer(demo_nested())
            expect_identical(demo$calls$fit, 3L)

            pip_run(demo$outer, lgr = NULL, force = TRUE)

            expect_identical(demo$calls$fit, 3L)
            expect_identical(demo$outer[["by_batch", "state"]], "done")
        })
    })

    describe("serialisation", {
        big_batch <- function(n = 1e5) {
            data.frame(batch = "A", x = seq_len(n), y = sqrt(seq_len(n)))
        }

        it("copies the kept pipelines with the function", {
            demo <- demo_nested()
            before <- length(serialize(demo$fun, NULL))

            demo$fun(data = big_batch())

            expect_length(.fun_pips(demo$fun), 1L)
            # the kept pipeline holds the input and the outputs: megabytes
            expect_gt(length(serialize(demo$fun, NULL)) - before, 1e6)
        })

        it("is small if nothing is kept", {
            demo <- demo_nested(cache = 0)
            before <- length(serialize(demo$fun, NULL))

            demo$fun(data = big_batch())

            expect_lt(length(serialize(demo$fun, NULL)) - before, 1e4)
        })

        it("gives the copy its own copy of the kept pipelines", {
            demo <- demo_nested()
            demo$fun(data = demo_batches()[1:6, ])

            copy <- unserialize(serialize(demo$fun, NULL))
            expect_length(.fun_pips(copy), 1L)

            demo$fun(data = demo_batches()[7:12, ])
            expect_length(.fun_pips(demo$fun), 2L)
            expect_length(.fun_pips(copy), 1L)
        })

        it("works after a round trip of the outer pipeline", {
            demo <- demo_nested()
            run_outer(demo)

            copy <- save_and_read(demo$outer)
            pip_set_params(copy, list(digits = 3))
            run_outer(list(outer = copy))

            expect_equal(
                copy[["coef_table", "out"]][[2L]],
                c(1.284, 3.284, 5.284),
                tolerance = 1e-8
            )
            # the original is not touched
            expect_equal(
                demo$outer[["coef_table", "out"]][[2L]],
                c(1.28, 3.28, 5.28),
                tolerance = 1e-8
            )
        })
    })

    describe("lifetime", {
        # The finalizer only knows the flag, not the function.
        set_flag <- function(flag) {
            force(flag)
            function(e) flag$collected <- TRUE
        }
        track_collection <- function(fun, flag) {
            runnerEnv <- environment(body(fun)[[1L]])
            reg.finalizer(runnerEnv, set_flag(flag))
        }

        it("is collected with its kept pipelines", {
            flag <- new.env()
            flag$collected <- FALSE
            f <- as.function(demo_inner(new.env()), out = "coefs")
            f(data = demo_batches()[1:6, ])
            expect_length(.fun_pips(f), 1L)
            track_collection(f, flag)

            rm(f)
            gc()

            expect_true(flag$collected)
        })

        it("is collected if a step of the pipeline refers to it", {
            flag <- new.env()
            flag$collected <- FALSE
            # the step closure refers to the frame that holds `f`
            make <- function() {
                p <- pip_new("local") |> pip_add("s", \(x = 1) x)
                f <- as.function(p, out = "s")
                f()
                track_collection(f, flag)
                invisible(NULL)
            }

            make()
            gc()

            expect_true(flag$collected)
        })
    })

    describe("failures and messages", {
        it("is a step error of the outer step that names the inner step", {
            demo <- demo_nested()
            bad <- data.frame(batch = "D", x = 1:3, y = NA_real_)
            data <- rbind(demo_batches(), bad)
            pip_set_params(demo$outer, list(data = data))

            err <- expect_error(
                pip_run(demo$outer, lgr = NULL),
                class = "pipeflow_step_error"
            )

            expect_match(conditionMessage(err), "by_batch")
            expect_match(conditionMessage(err), "key 'D'")
            expect_match(conditionMessage(err), "fit")
            expect_identical(demo$outer[["by_batch", "state"]], "failed")
        })

        it("passes on warnings of inner steps to the outer run", {
            inner <- pip_new("w") |>
                pip_add("s", \(x = 1) {
                    warning("inner warning")
                    x
                })
            outer <- pip_new("o") |> pip_add("in", as.function(inner))
            msgs <- character()
            log <- \(level, msg) msgs <<- c(msgs, paste(level, msg))

            expect_warning(pip_run(outer, lgr = log), "inner warning")
            expect_true(any(grepl("inner warning", msgs)))
        })

        it("passes on messages of inner steps to the outer run", {
            inner <- pip_new("m") |>
                pip_add("s", \(x = 1) {
                    message("inner message")
                    x
                })
            outer <- pip_new("o") |> pip_add("in", as.function(inner))
            msgs <- character()
            log <- \(level, msg) msgs <<- c(msgs, paste(level, msg))

            expect_message(pip_run(outer, lgr = log), "inner message")

            expect_true(any(grepl("inner message", msgs)))
        })
    })
})
