# Long chains of edits, with runs in between. After each edit, the checks of
# helper_invariants.R make sure that the pipeline keeps its invariants (dev/pipeflow.md,
# section 3), that its steps keep their node ids, that the steps that are done hold the
# outputs of a run from scratch, and that the failures of failed keys and the marks of
# `.steps_with_current_out` are in line with the step table.

# Fails for the multiples of the values in the option `pipeflow.test.bad`. Removing a
# value lets a step succeed in the next run without an edit of the pipeline, so that the
# run only re-runs its failed keys. An option, unlike a variable, also reaches the
# copies of the step functions that readRDS() makes.
fail_if_bad <- function(value) {
    if (any(outer(round(value), getOption("pipeflow.test.bad"), "%%") == 0)) {
        stop("bad value ", paste(value, collapse = " "))
    }
    value
}

# `square` is mapped over the keys of `parts` and fails for the bad values. For its
# failed keys, `plus` is not run, `total` is skipped (its input failed) and `safe` gets
# the failures via ~try().
keys_pip <- function() {
    pip_new("keys") |>
        pip_add(
            "parts",
            \(n = 3) as.list(stats::setNames(seq_len(n), letters[seq_len(n)])),
            exec = "split"
        ) |>
        pip_add("square", \(x = ~parts) fail_if_bad(x^2)) |>
        pip_add("plus", \(x = ~square, k = 1) x + k) |>
        pip_add("total", \(x = ~square) sum(unlist(x)), exec = "reduce") |>
        pip_add(
            "safe",
            \(x = ~ try(square)) sum(unlist(Filter(is.numeric, x))),
            exec = "reduce"
        )
}

is_retry <- function(log) any(grepl("re-running failed keys", log))


describe("a chain of structural edits", {
    it("keeps the invariants, the views and the node ids", {
        check <- edit_chain()
        p <- pip_new("chain") |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a, k = 2) x * k) |>
            pip_add("c", \(x = ~b, y = ~a) x + y) |>
            pip_add("d", \(x = ~ -1) x * 10)
        v <- pip_view(p, step = c("b", "d"))
        check(p)
        pip_run(p, lgr = NULL)
        check(p)

        # Insert a step: the view keeps covering its steps
        pip_add(p, "a2", \(x = ~a) x + 1, after = "a")
        check(p)
        expect_equal(v[["view"]], c("b", "d"))

        # Rename, replace and set params through the view
        pip_rename(v, "b", "beta")
        check(p)
        expect_equal(v[["view"]], c("beta", "d"))
        pip_replace(v, "beta", \(x = ~a, k = 2) x * k + 1)
        check(p)
        pip_replace(v, "d", \(x = ~a2) x * 100)
        check(p)
        pip_set_params(v, params = list(k = 3))
        check(p)
        pip_set_params(v, params = list(k = ~a2))
        check(p)

        # Running the view runs its upstream steps and outdates the steps after it
        pip_run(v, lgr = NULL)
        check(p)
        expect_equal(
            p[["state"]],
            c(
                a = "done",
                a2 = "done",
                beta = "done",
                c = "outdated",
                d = "done"
            )
        )

        # A failed removal or bulk assignment changes nothing
        before <- pip_internals(p)
        expect_error(pip_remove(p, "beta"), "depend on it")
        expect_error(p[c("a", "d"), "exec"] <- c("plain", "bogus"))
        expect_equal(pip_internals(p), before)

        # A removal with the downstream steps shrinks the view
        expect_message(pip_remove(p, "beta", force = TRUE), "downstream")
        check(p)
        expect_equal(v[["view"]], "d")

        # Steps added with the names of removed steps get new node ids, and the view
        # doesn't cover them
        pip_add(p, "beta", \(x = ~a) x - 1)
        pip_add(p, "c", \(x = ~beta, y = ~d) x + y)
        check(p)
        expect_equal(v[["view"]], "d")

        # Change the exec mode of several steps and reset a step
        p[c("a", "d"), "exec"] <- "plain"
        check(p)
        pip_run(p, lgr = NULL)
        check(p)
        pip_reset(pip_view(p, step = "a2"))
        check(p)

        # Copy a step from a changed clone
        q <- pip_clone(p)
        pip_replace(q, "d", \(x = ~a2) x * 1000)
        pip_run(q, lgr = NULL)
        p["d", ] <- q["d", ]
        check(p)
        expect_equal(p[["d", "out"]], 2000)
        expect_equal(p[["c", "state"]], "outdated")

        # Save and read the pipeline with its view, then edit both
        saved <- save_and_read(list(p = p, v = v))
        p <- saved[["p"]]
        v <- saved[["v"]]
        check(p)
        pip_add(p, "e", \(x = ~c) x)
        check(p)
        pip_rename(v, "d", "delta")
        check(p)
        expect_equal(v[["view"]], "delta")
        pip_run(p, lgr = NULL)
        check(p)

        # Edits of a clone leave the pipeline as it was
        before <- pip_internals(p)
        q <- pip_clone(p)
        pip_rename(q, "a", "A")
        pip_remove(q, "e")
        pip_set_params(q, params = list(x = 5))
        pip_run(q, lgr = NULL)
        expect_equal(pip_internals(p), before)
        edit_chain()(q)

        # Bind the clone: its steps with taken names get a number suffix
        r <- rbind(p, q)
        check(r)
        expect_equal(
            unname(r[["step"]]),
            c(unname(p[["step"]]), "A", "a22", "delta2", "beta2", "c2")
        )
        pip_run(r, lgr = NULL)
        check(r)

        # A compacted part is a new pipeline with new node ids
        expect_message(s <- r[step == "c2", view = FALSE], "upstream")
        edit_chain()(s)
        expect_equal(
            unname(s[["step"]]),
            c("A", "a22", "delta2", "beta2", "c2")
        )
    })
})


describe("a chain of edits of a pipeline with failed keys", {
    it("keeps the failures and retries in line with the step table", {
        op <- options(pipeflow.test.bad = 4)
        on.exit(options(op))
        check <- edit_chain()
        p <- keys_pip()
        run_log(p)
        check(p)
        expect_equal(
            p[["state"]],
            c(
                parts = "done",
                square = "failed",
                plus = "failed",
                total = "new",
                safe = "done"
            )
        )
        expect_equal(.pip_failed_keys(p[["plus", "out"]]), "b")

        # Rename the failed step, with a name from the (named) step column: the failures
        # and the retry follow the new name
        pip_rename(p, p[["step"]][2], "sq")
        check(p)
        expect_equal(p[["plus", "out"]][["b"]][["failed_step"]], "sq")

        # Insert and remove a step before the failed step
        pip_add(p, "noop", \(x = ~parts) x, after = "parts")
        check(p)
        pip_remove(p, "noop")
        check(p)

        # With the bad value gone, only the failed key runs again
        options(pipeflow.test.bad = NULL)
        log <- run_log(p)
        check(p)
        expect_true(any(grepl("sq - re-running failed keys 'b'", log)))
        expect_true(all(p[["state"]] == "done"))

        # New partition keys: all steps and keys run again
        options(pipeflow.test.bad = 9)
        pip_set_params(p, list(n = 4))
        check(p)
        expect_false(is_retry(run_log(p)))
        check(p)
        expect_equal(.pip_failed_keys(p[["sq", "out"]]), "c")

        # Changing the exec mode through a view and back runs all keys again
        v <- pip_view(p, step = "sq")
        v[["sq", "exec"]] <- "reduce"
        check(p)
        expect_false(is_retry(run_log(p)))
        check(p)
        expect_s3_class(p[["sq", "out"]], "simpleError")
        v[["sq", "exec"]] <- "auto"
        check(p)
        expect_false(is_retry(run_log(p)))
        check(p)
        expect_equal(.pip_failed_keys(p[["sq", "out"]]), "c")

        # A clone runs all keys again, a pipeline read with readRDS() only the failed
        # ones
        q <- pip_clone(p)
        p <- save_and_read(p)
        options(pipeflow.test.bad = NULL)
        expect_false(is_retry(run_log(q)))
        edit_chain()(q)
        expect_true(is_retry(run_log(p)))
        check(p)

        # A failed bulk rename keeps the failures and the marks
        options(pipeflow.test.bad = 16)
        run_log(p, force = TRUE)
        check(p)
        before <- pip_internals(p)
        expect_error(p[c("sq", "plus"), "step"] <- c("sq3", "parts"))
        expect_equal(pip_internals(p), before)

        # An output set by hand is not reused for a retry
        p[["sq", "out"]] <- p[["sq", "out"]]
        check(p)
        expect_equal(p[["plus", "state"]], "outdated")
        options(pipeflow.test.bad = NULL)
        expect_false(is_retry(run_log(p)))
        check(p)

        # Binding renames the steps with taken names, also in the failures, and drops
        # the marks
        options(pipeflow.test.bad = 16)
        run_log(p, force = TRUE)
        pre <- pip_new("pre") |>
            pip_add("parts", \(x = 1) list(z = x), exec = "split") |>
            pip_add("sq", \(x = ~parts) x)
        r <- rbind(pre, p)
        edit_chain()(r)
        expect_equal(
            unname(r[["step"]]),
            c("parts", "sq", "parts2", "sq2", "plus", "total", "safe")
        )
        expect_equal(r[["plus", "out"]][["d"]][["failed_step"]], "sq2")
        options(pipeflow.test.bad = NULL)
        expect_false(is_retry(run_log(r)))
        edit_chain()(r)

        # A step that was reset and then locked: its dependents run again
        options(pipeflow.test.bad = 16)
        run_log(p, force = TRUE)
        pip_reset(pip_view(p, step = "parts"))
        pip_lock(pip_view(p, step = "parts"))
        check(p)
        expect_false(is_retry(run_log(p)))
        check(p)
    })
})


describe("random chains of edits", {
    # Step functions for random steps, which take single values, partitioned values and
    # failures (and fail for the latter): `map_step` and `map2_step` add their inputs
    # and fail for bad values, `split_step` splits the sum of its input into `n` keys.
    map_step <- function(x, k = 1) {
        fail_if_bad(sum(as.numeric(unlist(x))) + k)
    }
    map2_step <- function(x, y, k = 1) {
        fail_if_bad(sum(as.numeric(unlist(x)), as.numeric(unlist(y))) + k)
    }
    split_step <- function(x, n = 3, k = 0) {
        v <- sum(as.numeric(unlist(x))) + k
        as.list(stats::setNames(v + seq_len(n), letters[seq_len(n)]))
    }
    is_split_step <- function(fun) identical(body(fun), body(split_step))

    # Run a chain of `n` random edits and check the pipeline after each one. The bad
    # values only get fewer, as the outputs of done steps would be stale otherwise.
    run_random_chain <- function(seed, n) {
        oldSeed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
        on.exit({
            if (is.null(oldSeed)) {
                rm(".Random.seed", envir = globalenv())
            } else {
                assign(".Random.seed", oldSeed, envir = globalenv())
            }
        })
        set.seed(seed)
        op <- options(pipeflow.test.bad = c(4, 5, 7, 9, 11, 13))
        on.exit(options(op), add = TRUE)

        check <- edit_chain()
        p <- pip_new("random") |>
            pip_add("s0", split_step, params = list(x = 1), exec = "split")
        v <- pip_view(p, step = "s0")
        nSteps <- 0L
        removed <- character()
        pinned <- list() # outputs set by hand, by node id

        # A new step name, or the name of a removed step
        new_name <- function() {
            if (length(removed) > 0L && stats::runif(1) < 0.3) {
                name <- sample(removed, 1)
                removed <<- setdiff(removed, name)
                return(name)
            }
            nSteps <<- nSteps + 1L
            paste0("s", nSteps)
        }
        # A step function with params that refer to the steps in `prefix`
        new_step <- function(prefix) {
            input <- function() {
                if (length(prefix) == 0L || stats::runif(1) < 0.15) {
                    return(sample(5, 1))
                }
                ref <- as.name(sample(prefix, 1))
                if (stats::runif(1) < 0.2) {
                    eval(bquote(~ try(.(ref))))
                } else {
                    eval(bquote(~ .(ref)))
                }
            }
            switch(
                sample(c("map", "map2", "split"), 1, prob = c(6, 1.5, 2.5)),
                map = list(
                    fun = map_step,
                    params = list(x = input(), k = sample(0:3, 1)),
                    exec = sample(c("auto", "reduce", "plain"), 1, prob = 7:5)
                ),
                map2 = list(
                    fun = map2_step,
                    params = list(x = input(), y = input()),
                    exec = "auto"
                ),
                split = list(
                    fun = split_step,
                    params = list(x = input(), n = sample(4, 1)),
                    exec = "split"
                )
            )
        }
        # A random unbound param of `step` among `names`
        unbound_param <- function(x, step, names = c("k", "n", "x")) {
            params <- intersect(x[[step, "unbound"]], names)
            if (length(params) > 0L) sample(params, 1) else NULL
        }
        # The steps whose outputs were set by hand and not computed since
        pinned_steps <- function() {
            dat <- .pip_pipenv(p)[["data"]]
            rows <- match(as.integer(names(pinned)), dat[["nodeId"]])
            isPinned <- vapply(
                seq_along(pinned),
                FUN = \(i) {
                    !is.na(rows[[i]]) &&
                        isTRUE(all.equal(
                            comparable_out(dat[["out"]][[rows[[i]]]]),
                            comparable_out(pinned[[i]])
                        ))
                },
                FUN.VALUE = NA
            )
            pinned <<- pinned[isPinned]
            dat[["step"]][rows[isPinned]]
        }

        # Do `edit`, or if it `is_rejected`, expect it to fail with `error` and to
        # change nothing
        edit_or_reject <- function(edit, is_rejected, error) {
            if (!is_rejected) {
                return(force(edit))
            }
            before <- pip_internals(p)
            expect_error(edit, error)
            expect_equal(pip_internals(p), before)
        }

        # Each edit gets the step names and returns a description of what it did. In
        # `p[i, ...]`, `i` is evaluated in the step table, whose column `step` would
        # select all steps, so `sel` names the selected steps there.
        edits <- list(
            add = function(steps) {
                pos <- sample.int(length(steps) + 1L, 1) - 1L
                s <- new_step(steps[seq_len(pos)])
                name <- new_name()
                after <- if (pos == 0L) 0L else steps[[pos]]
                edit_or_reject(
                    pip_add(
                        p,
                        name,
                        s$fun,
                        after = after,
                        params = s$params,
                        exec = s$exec
                    ),
                    is_rejected = name %in% steps,
                    error = "already exists"
                )
                paste("add", name, "after", after)
            },
            remove = function(steps) {
                step <- sample(steps, 1)
                force <- stats::runif(1) < 0.5
                x <- if (stats::runif(1) < 0.3) pip_view(p, step = step) else p
                depends <- .pip_pipenv(p)[["data"]][["depends"]]
                edit_or_reject(
                    pip_remove(x, step, force = force),
                    is_rejected = !force &&
                        any(vapply(depends, \(dep) step %in% dep, NA)),
                    error = "depend on it"
                )
                removed <<- c(removed, setdiff(steps, p[["step"]]))
                paste("remove", step, if (force) "with force")
            },
            rename = function(steps) {
                step <- sample(steps, 1)
                to <- new_name()
                w <- pip_view(p, step = step)
                edit_or_reject(
                    switch(
                        sample(4, 1),
                        pip_rename(p, step, to),
                        pip_rename(w, step, to),
                        p[[step, "step"]] <- to,
                        w[[step, "step"]] <- to
                    ),
                    is_rejected = to %in% setdiff(steps, step),
                    error = "already exists"
                )
                removed <<- setdiff(c(removed, step), to)
                paste("rename", step, "to", to)
            },
            replace = function(steps) {
                k <- sample(seq_along(steps), 1)
                s <- new_step(steps[seq_len(k - 1L)])
                x <- if (stats::runif(1) < 0.5) {
                    p
                } else {
                    pip_view(p, step = steps[[k]])
                }
                pip_replace(
                    x,
                    steps[[k]],
                    s$fun,
                    params = s$params,
                    exec = s$exec
                )
                paste("replace", steps[[k]])
            },
            params = function(steps) {
                step <- sample(steps, 1)
                name <- unbound_param(p, step)
                if (is.null(name)) {
                    return("no params")
                }
                params <- stats::setNames(list(sample(0:4, 1)), name)
                w <- pip_view(p, step = unique(c(step, sample(steps, 1))))
                sel <- step
                switch(
                    sample(4, 1),
                    pip_set_params(pip_view(p, step = step), params),
                    pip_set_params(w, params),
                    p[[step, "params"]] <- params,
                    p[sel, "params"] <- params
                )
                paste("set", name, "of", step, "to", params[[1]])
            },
            bind = function(steps) {
                k <- sample(seq_along(steps), 1)
                name <- unbound_param(p, steps[[k]], c("k", "x", "y"))
                if (k == 1L || is.null(name)) {
                    return("no binding")
                }
                to <- sample(steps[seq_len(k - 1L)], 1)
                ref <- eval(bquote(~ .(as.name(to))))
                params <- stats::setNames(list(ref), name)
                pip_set_params(pip_view(p, step = steps[[k]]), params)
                paste("bind", name, "of", steps[[k]], "to", to)
            },
            exec = function(steps) {
                step <- sample(steps, 1)
                modes <- if (is_split_step(p[[step, "fun"]])) {
                    c("split", "plain", "auto")
                } else {
                    c("auto", "reduce", "plain")
                }
                mode <- sample(modes, 1)
                w <- pip_view(p, step = step)
                if (stats::runif(1) < 0.5) {
                    p[[step, "exec"]] <- mode
                } else {
                    w[[step, "exec"]] <- mode
                }
                paste("set exec of", step, "to", mode)
            },
            bulk = function(steps) {
                sel <- sample(steps, min(length(steps), sample(3, 1)))
                modes <- vapply(
                    sel,
                    FUN = \(s) {
                        if (is_split_step(p[[s, "fun"]])) {
                            return("split")
                        }
                        sample(c("auto", "reduce"), 1)
                    },
                    FUN.VALUE = ""
                )
                p[sel, "exec"] <- unname(modes)
                paste("set exec of", toString(sel), "to", toString(modes))
            },
            failed_bulk = function(steps) {
                sel <- sample(steps, min(length(steps), sample(3, 1)))
                before <- pip_internals(p)
                others <- setdiff(steps, sel)
                if (stats::runif(1) < 0.5 || length(others) == 0L) {
                    modes <- c(rep("reduce", length(sel) - 1L), "bogus")
                    expect_error(p[sel, "exec"] <- modes)
                } else {
                    to <- c(sprintf("%s_x", sel[-length(sel)]), others[[1]])
                    expect_error(p[sel, "step"] <- to, "already exists")
                }
                expect_equal(pip_internals(p), before)
                paste("failed bulk assignment to", toString(sel))
            },
            run = function(steps) {
                force <- stats::runif(1) < 0.1
                # The keys of mapped steps are run by `map` instead
                map <- if (stats::runif(1) < 0.5) lapply
                run_log(p, force = force, map = map)
                paste(
                    "run",
                    if (force) "with force",
                    if (!is.null(map)) "with map"
                )
            },
            run_view = function(steps) {
                sel <- sample(steps, min(length(steps), sample(2, 1)))
                run_log(pip_view(p, step = sel))
                paste("run", toString(sel))
            },
            heal = function(steps) {
                bad <- getOption("pipeflow.test.bad")
                if (length(bad) <= 1L) {
                    return("no healing")
                }
                options(pipeflow.test.bad = bad[-1])
                run_log(p)
                paste("heal", bad[[1]], "and run")
            },
            clone = function(steps) {
                p <<- pip_clone(p)
                v <<- pip_view(p, step = steps[[1]])
                "clone"
            },
            save = function(steps) {
                saved <- save_and_read(list(p = p, v = v))
                p <<- saved[["p"]]
                v <<- saved[["v"]]
                expect_identical(v[["pipenv"]], p[["pipenv"]])
                "save and read"
            },
            bind_pipeline = function(steps) {
                q <- pip_new("q") |>
                    pip_add(
                        "s1",
                        split_step,
                        params = list(x = 2, n = 2),
                        exec = "split"
                    ) |>
                    pip_add("s2", map_step, params = list(x = ~s1))
                if (stats::runif(1) < 0.5) {
                    run_log(q)
                }
                p <<- rbind(p, q)
                v <<- pip_view(p, step = steps[[1]])
                "rbind"
            },
            compact = function(steps) {
                sel <- sample(steps, min(length(steps), 2))
                kept <- intersect(pinned_steps(), steps)
                p <<- p[step %in% sel, view = FALSE]
                v <<- pip_view(p, step = p[["step"]][[1]])
                check <<- edit_chain()
                kept <- intersect(kept, p[["step"]])
                pinned <<- stats::setNames(
                    lapply(kept, \(s) p[[s, "out"]]),
                    vapply(kept, \(s) as.character(p[[s, "nodeId"]]), "")
                )
                paste("compact to", toString(sel))
            },
            reset = function(steps) {
                sel <- sample(steps, min(length(steps), 2))
                pip_reset(pip_view(p, step = sel))
                paste("reset", toString(sel))
            },
            out = function(steps) {
                step <- sample(steps, 1)
                out <- p[[step, "out"]]
                plus <- \(x) if (is.numeric(x)) x + 1000 else 1000
                if (.is_pipeflow_partitioned(out)) {
                    out[] <- lapply(out, plus)
                } else {
                    out <- plus(out)
                }
                p[[step, "out"]] <- out
                pinned[[as.character(p[[step, "nodeId"]])]] <<- out
                paste("set output of", step)
            },
            copy = function(steps) {
                # A copy is computed from the same inputs only if they are done, so that
                # the clone does not run them again
                env <- .pip_pipenv(p)
                ids <- env[["data"]][["nodeId"]]
                isReady <- vapply(
                    seq_along(steps),
                    FUN = \(k) {
                        up <- dag_get_reachable_nodes_up(
                            .pip_dag(env),
                            ids[[k]]
                        )
                        isUp <- ids %in% setdiff(up, ids[[k]])
                        all(env[["data"]][["state"]][isUp] == "done")
                    },
                    FUN.VALUE = NA
                )
                if (!any(isReady)) {
                    return("no step to copy")
                }
                sel <- sample(steps[isReady], 1)
                q <- pip_clone(p)
                if (!is.null(unbound_param(q, sel, "k"))) {
                    pip_set_params(pip_view(q, step = sel), list(k = 9))
                }
                run_log(q)
                p[sel, ] <- q[sel, ]
                paste("copy", sel, "from a clone")
            },
            lock = function(steps) {
                # A step that is not done may have passed a failure to its dependents,
                # which they would not get from a locked step
                isDone <- p[["state"]] == "done"
                if (!any(isDone)) {
                    return("no step to lock")
                }
                step <- sample(steps[isDone], 1)
                pip_lock(pip_view(p, step = step))
                paste("lock", step)
            },
            view = function(steps) {
                step <- v[["step"]]
                if (length(step) == 0L) {
                    return("empty view")
                }
                name <- unbound_param(v, step, c("k", "n"))
                if (stats::runif(1) < 0.5 || is.null(name)) {
                    run_log(v)
                    return(paste("run view of", step))
                }
                v[[step, "params"]] <- stats::setNames(list(sample(4, 1)), name)
                paste("set", name, "of", step, "through the view")
            }
        )
        weights <- c(
            add = 15,
            remove = 6,
            rename = 10,
            replace = 6,
            params = 10,
            bind = 3,
            exec = 8,
            bulk = 4,
            failed_bulk = 4,
            run = 14,
            run_view = 8,
            heal = 3,
            clone = 3,
            save = 3,
            bind_pipeline = 3,
            compact = 1,
            reset = 2,
            out = 3,
            copy = 3,
            lock = 1,
            view = 3
        )

        history <- character()
        for (i in seq_len(n)) {
            steps <- unname(p[["step"]])
            # Small pipelines keep the checks fast
            weights[["add"]] <- if (length(steps) < 12L) 15 else 0
            edit <- if (length(steps) == 0L) {
                "add"
            } else {
                sample(names(weights), 1, prob = weights)
            }
            # An edit that fails other than expected fails the test, with the edits so
            # far (failed expectations are reported as they are)
            what <- withCallingHandlers(
                suppressMessages(edits[[edit]](steps)),
                error = function(e) {
                    if (!inherits(e, "expectation")) {
                        stop(
                            sprintf("edit %d (%s) failed: ", i, edit),
                            conditionMessage(e),
                            "\n",
                            paste0(seq_along(history), ": ", history, "\n"),
                            call. = FALSE
                        )
                    }
                }
            )
            history <- c(history, what)
            check(
                p,
                pinned = pinned_steps(),
                info = paste0(seq_along(history), ": ", history)
            )
        }
    }

    # Set the environment variable PIPEFLOW_RANDOM_CHAINS to run more chains, e.g. after
    # changing how edits or runs work.
    nChains <- as.integer(Sys.getenv("PIPEFLOW_RANDOM_CHAINS", "5"))
    for (seed in seq_len(nChains)) {
        it(sprintf("keep the invariants after each edit (seed %d)", seed), {
            skip_on_cran()
            run_random_chain(seed, n = 100)
        })
    }
})
