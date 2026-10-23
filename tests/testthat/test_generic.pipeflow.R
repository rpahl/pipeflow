# ------
# Helper
# ------

describe(".pip_compact", {
    it("builds a compact self-contained pipeline from the kept nodes", {
        p <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x * 2) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("b2", \(x = ~b1) x + 1)
        pip_run(p, lgr = NULL)

        c <- .pip_compact(p, keepNodes = c(0L, 1L))

        expect_equal(unname(c[["step"]]), c("a1", "a2"))
        expect_equal(c[["nodeId"]], c(a1 = 0, a2 = 1))
        expect_equal(unname(c[["depends"]][[2]]), "a1")
        expect_setequal(
            .pip_filter_nodes(
                c,
                .pip_get_reachable_nodes(c, "a1")
            )[["step"]],
            c("a1", "a2")
        )

        # runtime state is preserved and re-running yields the same outputs
        expect_equal(unname(c[["state"]]), c("done", "done"))
        suppressMessages(pip_run(c, lgr = NULL))
        expect_equal(
            c[["out"]],
            p[["out"]][1:2]
        )
    })

    it("returns an empty pipeline when no nodes are kept", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)
        c <- .pip_compact(p, keepNodes = integer())

        expect_equal(nrow(c[["pipenv"]][["data"]]), 0L)
    })

    it("keeps a small subset via the node-by-node path", {
        p <- pip_new() |>
            pip_add("hub", \(x = 1) x)
        for (i in 1:9) {
            pip_add(p, sprintf("f%d", i), function(x = ~hub) x + 1)
        }
        # 2 of 10 steps -> below the rebuild fraction, from-scratch path
        c <- .pip_compact(p, keepNodes = c(0L, 9L))

        expect_equal(unname(c[["step"]]), c("hub", "f9"))
        expect_equal(unname(c[["nodeId"]]), 0:1)
        expect_equal(unname(c[["depends"]][[2]]), "hub")
    })

    it("compacts node ids also when the source has gaps", {
        p <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
        pip_remove(p, "a2") # leaves nodeId with a gap

        c <- .pip_compact(p, keepNodes = p[["nodeId"]])

        expect_equal(
            unname(c[["step"]]),
            c("a1", "b1", "b2")
        )
        expect_equal(unname(c[["nodeId"]]), 0:2)
        expect_equal(unname(c[["depends"]][[3]]), "b1")
    })

    it("gives identical results for any rebuildFrac", {
        check_equal_compact <- function(a, b) {
            expect_equal(
                unname(a[["step"]]),
                unname(b[["step"]])
            )
            expect_equal(
                a[["nodeId"]],
                b[["nodeId"]]
            )
            expect_equal(
                a[["depends"]],
                b[["depends"]]
            )
            expect_equal(
                a[["unbound"]],
                b[["unbound"]]
            )
            expect_equal(
                a[["params"]],
                b[["params"]]
            )
            expect_equal(
                a[["out"]],
                b[["out"]]
            )
            expect_equal(
                a[["state"]],
                b[["state"]]
            )
            expect_equal(
                a[["locked"]],
                b[["locked"]]
            )

            # DAG equivalence via downstream reachability per step
            for (s in a[["step"]]) {
                ra <- .pip_filter_nodes(
                    a,
                    .pip_get_reachable_nodes(a, s)
                )[["step"]]
                rb <- .pip_filter_nodes(
                    b,
                    .pip_get_reachable_nodes(b, s)
                )[["step"]]
                expect_setequal(ra, rb)
            }

            # step -> node lookup is identical
            nodesA <- a[["pipenv"]][[".steps_to_nodes"]]
            nodesB <- b[["pipenv"]][[".steps_to_nodes"]]
            expect_setequal(ls(nodesA), ls(nodesB))
            expect_equal(
                unname(unlist(mget(ls(nodesA), envir = nodesA))),
                unname(unlist(mget(ls(nodesA), envir = nodesB)))
            )
        }

        p <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x * 2) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x + 10) |>
            pip_add("b2", \(x = ~b1) x + 1)
        pip_run(p, lgr = NULL)

        keepSets <- list(
            c(0L, 1L), # small subset (from-scratch path)
            c(0L, 1L, 3L), # intermediate subset (both paths)
            0:4 # full set (rebuild path)
        )
        fracs <- c(0, 0.3, 0.6, 1, 2)

        for (keep in keepSets) {
            ref <- .pip_compact(p, keepNodes = keep, rebuildThresh = fracs[1])
            for (frac in fracs[-1]) {
                got <- .pip_compact(p, keepNodes = keep, rebuildThresh = frac)
                check_equal_compact(ref, got)
            }
        }

        # also on a pipeline whose node ids have gaps
        q <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
        pip_remove(q, "a2")
        keep <- q[["nodeId"]]

        ref <- .pip_compact(q, keepNodes = keep, rebuildThresh = fracs[1])
        for (frac in fracs[-1]) {
            got <- .pip_compact(q, keepNodes = keep, rebuildThresh = frac)
            check_equal_compact(ref, got)
        }
    })
})

# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

describe("length", {
    it("returns an integer value", {
        p <- pip_new()
        expect_true(is.integer(length(p)))
    })

    it("returns the expected value", {
        p <- pip_new()
        expect_equal(length(p), 0L)
        pip_add(p, "s1", \(a = 1) a)
        pip_add(p, "s2", \(a = 1) a)
        expect_equal(length(p), 2L)

        v <- pip_view(p, step = "s1")
        expect_equal(length(v), 1L)
    })
})

describe("nrow", {
    it("matches length for pipelines and views", {
        p <- pip_new() |>
            pip_add("s1", \(a = 1) a) |>
            pip_add("s2", \(a = ~s1) a)
        pip_add(p, "s3", \(a = 1) a)

        expect_equal(nrow(p), length(p))
        expect_equal(nrow(p), 3L)
        expect_equal(ncol(p), ncol(p[["pipenv"]][["data"]]))

        v <- pip_view(p, step = c("s1", "s3"))
        expect_equal(nrow(v), length(v))
        expect_equal(nrow(v), 2L)
        expect_equal(ncol(v), ncol(p[["pipenv"]][["data"]]))
    })

    it("returns zero rows for an empty pipeline", {
        p <- pip_new()
        expect_equal(nrow(p), 0L)
        expect_equal(ncol(p), ncol(p[["pipenv"]][["data"]]))
    })
})


describe("extract operator [", {
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("returns a view into the selected steps by default", {
        p <- test_pip()
        v <- p[5L]

        expect_true(.is_pipeflow_view(v))
        expect_equal(v[["view"]], "b2")
        expect_identical(v[["pipenv"]][["data"]], p[["pipenv"]][["data"]])
    })

    it("returns a view by step names by default", {
        p <- test_pip()
        v <- p[c("a2", "b2")]

        expect_true(.is_pipeflow_view(v))
        expect_equal(v[["view"]], c("a2", "b2"))
    })

    it("keeps the pipeline name for views and views of views", {
        p <- test_pip()
        v <- p[c("a2", "b2")]
        vv <- v["b2"]

        expect_equal(v[["name"]], p[["name"]])
        expect_equal(vv[["name"]], p[["name"]])
    })

    it("returns a pipeline including upstream dependencies with view = FALSE", {
        p <- test_pip()
        suppressMessages(sub <- p[5L, view = FALSE])

        expect_true(.is_pipeflow(sub))
        expect_equal(unname(sub[["step"]]), c("b1", "b2"))
    })

    it("keeps try() references with view = FALSE", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~ try(a)) x)
        suppressMessages(sub <- p["b", view = FALSE])

        expect_equal(formula_try_args(sub[["b", "params"]]), "x")
    })

    it("returns a pipeline by step names with view = FALSE", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2", "b2"), view = FALSE])

        expect_true(.is_pipeflow(sub))
        expect_equal(
            unname(sub[["step"]]),
            c("a1", "a2", "b1", "b2")
        )
    })

    it("returns the full pipeline when i is missing", {
        p <- test_pip()
        sub <- p[]

        expect_equal(
            sub[["step"]],
            p[["step"]]
        )
    })

    it("returns an empty pipeline for empty selectors with view = FALSE", {
        p <- test_pip()

        expect_equal(length(p[integer(), view = FALSE]), 0L)
        expect_equal(length(p[character(), view = FALSE]), 0L)
    })

    it("signals invalid row indices", {
        p <- test_pip()

        expect_error(p[c(0, 1)], "Invalid row indices in 'i'")
        expect_error(p[99], "Invalid row indices in 'i'")
        expect_error(p[c(1, NA)], "row indices in 'i' must not contain NA")
    })

    it("supports negative row indices", {
        p <- test_pip()

        expect_equal(
            unname(p[-2][["step"]]),
            c("a1", "b1", "a3", "b2")
        )
        expect_equal(
            unname(p[-(1:2)][["step"]]),
            c("b1", "a3", "b2")
        )
        expect_equal(
            unname(p[c(-5, -2)][["step"]]),
            c("a1", "b1", "a3")
        )
        expect_equal(length(p[-c(1:9)]), 0L)
        expect_error(p[c(-1, 2)], "only 0's may be mixed with negative")
    })

    it("signals invalid step names", {
        p <- test_pip()

        expect_error(p[c("a1", "")], "must be non-empty strings")
        expect_error(p[c("a1", NA)], "must not contain NA")
        expect_error(p[c("a1", "unknown")], "Unknown step names")
    })

    it("returns an independent copy with view = FALSE", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2"), view = FALSE])
        env <- .pip_pipenv(sub)

        env[["data"]][["state"]][1] <- "done"
        expect_equal(unname(p[["state"]][1]), "new")
    })

    it("works with view = FALSE for a pipeline read with readRDS()", {
        p <- test_pip()
        pip_remove(p, "a3")
        q <- save_and_read(p)

        # Few steps are copied to a new DAG, more are rebuilt from a clone
        for (i in list("a1", c("a2", "b2"))) {
            suppressMessages({
                expected <- p[i, view = FALSE]
                sub <- q[i, view = FALSE]
            })
            expect_equal(sub[, "step"], expected[, "step"])
            expect_equal(sub[["nodeId"]], expected[["nodeId"]])
            expect_equal(
                dag_structure(.pip_dag(sub[["pipenv"]])),
                dag_structure(.pip_dag(expected[["pipenv"]]))
            )
        }
    })

    it("does not change .self of the source steps with view = FALSE", {
        p <- pip_new("p") |>
            pip_add("a", \(x = 1) .self[["name"]]) |>
            pip_add("b", \(x = ~a) x)
        sub <- p["a", view = FALSE]
        sub[["name"]] <- "sub"
        pip_run(sub, lgr = NULL)
        pip_run(p, lgr = NULL)

        expect_equal(sub[["a", "out"]], "sub")
        expect_equal(p[["a", "out"]], "p")
    })

    it("copies DAG edges for the extracted subset", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2", "b2"), view = FALSE])

        nodes_a1 <- .pip_get_reachable_nodes(sub, "a1")
        steps_a1 <- .pip_filter_nodes(sub, nodes_a1)[["step"]]
        expect_setequal(steps_a1, c("a1", "a2"))

        nodes_b1 <- .pip_get_reachable_nodes(sub, "b1")
        steps_b1 <- .pip_filter_nodes(sub, nodes_b1)[["step"]]
        expect_setequal(steps_b1, c("b1", "b2"))
    })

    it("uses an independent DAG copy in the extracted subset", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2", "b2"), view = FALSE])

        from <- as.integer(.pip_steps_to_nodes(sub, "a2")[[1]])
        to <- as.integer(.pip_steps_to_nodes(sub, "b1")[[1]])
        dag_add_edges_to(sub[["pipenv"]][[".dag"]], from = from, to = to)

        sub_steps <- .pip_filter_nodes(
            sub,
            .pip_get_reachable_nodes(sub, "a1")
        )[["step"]]
        expect_setequal(sub_steps, c("a1", "a2", "b1", "b2"))

        original_steps <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "a1")
        )[["step"]]
        expect_setequal(original_steps, c("a1", "a2", "a3"))
    })

    it("signals the number of pulled-in upstream dependencies", {
        p <- test_pip()

        expect_message(p[5L, view = FALSE], "pulled in 1 upstream")
        expect_message(p[c("a2", "b2"), view = FALSE], "pulled in 2 upstream")
        expect_silent(p[c("a1", "b1"), view = FALSE])
    })

    describe("boolean expression filtering", {
        filter_pip <- function() {
            pip_new("pipe") |>
                pip_add("load", \(x = 1) x, tags = c("io", "daily")) |>
                pip_add("fit", \(x = ~load) x, tags = c("model", "daily")) |>
                pip_add("eval", \(x = ~fit) x, tags = c("model", "report"))
        }

        set_state <- function(p, step, state = "done") {
            env <- .pip_pipenv(p)
            env[["data"]][["state"]][env[["data"]][["step"]] == step] <- state
            p
        }

        it("selects steps matching a boolean expression", {
            p <- set_state(filter_pip(), "load")
            v <- p[state == "done"]

            expect_true(.is_pipeflow_view(v))
            expect_equal(unname(v[["step"]]), "load")
        })

        it("combines conditions with & and |", {
            p <- set_state(filter_pip(), "load")
            v <- p[tags %like% "model" & state == "new"]

            expect_equal(unname(v[["step"]]), c("fit", "eval"))

            v2 <- p[state == "done" | step %like% "^eval$"]
            expect_equal(unname(v2[["step"]]), c("load", "eval"))
        })

        it("supports %in% set membership", {
            p <- filter_pip()
            v <- p[step %in% c("load", "eval")]

            expect_equal(v[["view"]], c("load", "eval"))
        })

        it("mirrors the data.table filter on the step table", {
            p <- set_state(filter_pip(), "load")
            v <- p[tags %like% "model"]

            raw <- p[["pipenv"]][["data"]][tags %like% "model"]
            expect_equal(unname(v[["step"]]), raw[["step"]])
        })

        it("recycles scalar logical filters", {
            p <- filter_pip()

            expect_equal(length(p[TRUE]), 3L)
            expect_equal(length(p[FALSE]), 0L)
        })

        it("accepts a logical vector variable", {
            p <- filter_pip()
            keep <- p[["step"]] %in% c("load", "eval")
            v <- p[keep]

            expect_equal(v[["view"]], c("load", "eval"))
        })

        it("extracts upstream dependencies with view = FALSE", {
            p <- set_state(filter_pip(), "load")
            suppressMessages(sub <- p[tags %like% "report", view = FALSE])

            expect_equal(
                unname(sub[["step"]]),
                c("load", "fit", "eval")
            )
        })

        it("selects relative to the steps covered by a view", {
            p <- set_state(filter_pip(), "fit")
            v <- pip_view(p, step = c("fit", "eval"))

            expect_true(.is_pipeflow_view(v[1L]))
            expect_equal(v[1L][["view"]], "fit")
            expect_equal(v[][["view"]], c("fit", "eval"))
            expect_equal(unname(v[state == "done"][["step"]]), "fit")

            expect_error(v["load"], "not part of the view")
            expect_error(v[3L], "Invalid row indices")
        })

        it("materializes all covered steps with v[, view = FALSE]", {
            p <- set_state(filter_pip(), "fit")
            v <- pip_view(p, step = c("fit", "eval"))

            expect_message(
                sub <- v[, view = FALSE],
                "pulled in 1 upstream dependency"
            )

            expect_false(.is_pipeflow_view(sub))
            expect_equal(unname(sub[["step"]]), c("load", "fit", "eval"))

            # Same result as spelling the selector out
            suppressMessages(explicit <- v[TRUE, view = FALSE])
            expect_equal(sub[["step"]], explicit[["step"]])

            # and it is independent of the source pipeline
            pip_remove(p, "eval")
            expect_equal(unname(sub[["step"]]), c("load", "fit", "eval"))
        })

        it("rejects named filters and stray arguments", {
            p <- filter_pip()

            expect_error(p[state = "new"], "unused argument")
            expect_error(p[tags = "io"], "unused argument")
        })

        it("signals invalid logical filters", {
            p <- filter_pip()

            expect_error(p[c(TRUE, FALSE)], "logical filter has length")
            expect_error(p[c(TRUE, NA, FALSE)], "must not contain NA")
        })

        it("signals unknown columns in boolean expressions", {
            p <- filter_pip()

            expect_error(p[not_a_column == 1], "not_a_column")
        })
    })

    describe("two-index extraction", {
        it("extracts columns from the full step table with p[, j]", {
            p <- test_pip()

            expect_s3_class(p[, "step"], "data.table")
            expect_equal(
                p[, "step"][["step"]],
                c("a1", "a2", "b1", "a3", "b2")
            )
            expect_equal(nrow(p[, "step"]), 5L)
            expect_equal(
                names(p[, c("step", "tags")]),
                c("step", "tags")
            )
        })

        it("extracts columns for the rows selected by i", {
            p <- test_pip()

            expect_equal(p[c("a2", "b2"), "step"][["step"]], c("a2", "b2"))
            expect_equal(
                p[tags %like% "unused", "step"][["step"]],
                character(0)
            )
            expect_error(p[1:2, "nope"], "nope")
        })

        it("requires j to be a character vector", {
            p <- test_pip()

            expect_error(p[, 1L], "character vector of column names")
            expect_error(p[, TRUE], "character vector of column names")
            expect_error(
                p[1:2, list("step")],
                "character vector of column names"
            )
        })

        it("warns and ignores view when j is specified", {
            p <- test_pip()

            expect_warning(
                p[1:2, "step", view = FALSE],
                "is ignored when 'j' is specified"
            )
        })

        it("accepts a column-name vector from the calling scope", {
            p <- test_pip()
            cols <- c("step", "tags")

            out <- p[, cols]
            expect_equal(names(out), c("step", "tags"))
            expect_equal(nrow(out), 5L)
        })

        it("extracts from views relative to the covered rows", {
            p <- test_pip()
            v <- pip_view(p, step = c("a2", "b2"))

            expect_equal(v[, "step"][["step"]], c("a2", "b2"))
            expect_equal(v[1L, "step"][["step"]], "a2")
            expect_error(v[3L, "step"], "Invalid row indices")
            expect_error(v["a1", "step"], "not part of the view")
        })
    })
})


describe("extract operator [[", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "init") |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("does not forward inner-env bindings", {
        p <- test_pip()
        expect_equal(p[["name"]], "pipe")
        expect_true(data.table::is.data.table(p[["pipenv"]][["data"]]))
        expect_false(is.null(p[["pipenv"]][[".dag"]]))

        # Inner-env bindings are not exposed through `[[`; the step table is
        # available via the `data` virtual method (and pip_data()).
        expect_true(data.table::is.data.table(p[["data"]]()))
        expect_null(p[[".dag"]])
        expect_null(p[[".steps_to_nodes"]])
    })

    it("extracts full columns when j is missing", {
        p <- test_pip()
        expect_equal(p[["step"]], c(s1 = "s1", s2 = "s2"))
        expect_equal(p[[1]], c(s1 = "s1", s2 = "s2"))
        expect_null(p[["unknown"]])
    })

    it("extracts a single cell by row selector and column selector", {
        p <- test_pip()

        expect_equal(p[[2L, "step"]], "s2")
        expect_equal(p[["s1", "state"]], "new")
    })

    it("extracts list-column values for a single row", {
        p <- test_pip() |> pip_run(lgr = NULL)

        expect_equal(p[[1L, "tags"]], "init")
        expect_equal(p[[2L, "tags"]], character(0))
        expect_equal(p[[1L, "params"]], list(x = 1))
        expect_named(p[[2L, "params"]], "x")
        expect_equal(p[[1L, "depends"]], character(0))
        expect_equal(p[[2L, "depends"]], c(x = "s1"))
        expect_equal(p[[1L, "out"]], 1)
        expect_equal(p[[2L, "out"]], 2)
    })

    it("signals multi-step column access and points to pip_view", {
        p <- test_pip()

        expect_error(
            p[[c(1L, 2L), "state"]],
            "i must be a single step name or row index"
        )
        expect_error(
            p[[c("s1", "s2"), "step"]],
            "i must be a single step name or row index"
        )
    })

    it(
        paste(
            "can distinguish between steps called 'name' and 'data'",
            "and the internal 'name' element"
        ),
        {
            p <- pip_new("pipe") |>
                pip_add("name", \(x = "hello") x) |>
                pip_add("data", \(x = ~name, y = "world") paste(x, y))

            expect_equal(p[["name"]], "pipe")
            # "data" is a virtual method (the step-table accessor), not a
            # column; a step of that name stays reachable via p[[step, col]]
            expect_true(is.function(p[["data"]]))
            expect_true(is.function(p$data))
            expect_equal(pip_data(p)[["step"]], c("name", "data"))

            expect_equal(p[["name", "params"]], p[[1, "params"]])
            expect_equal(p[["data", "params"]], p[[2, "params"]])
        }
    )

    it("signals invalid row and column selectors", {
        p <- test_pip()

        expect_error(
            p[[c("s1", ""), "step"]],
            "i must be a single step name or row index"
        )
        expect_error(
            p[[c("s1", "unknown"), "step"]],
            "i must be a single step name or row index"
        )
        expect_null(p[[1, "unknown"]])
        expect_error(
            p[[1, c("step", "state")]],
            "j must be a single column name"
        )
    })

    it("extracts internal bindings and columns from a view", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = c("s1", "s2"))

        expect_identical(v[["pipenv"]][["data"]], p[["pipenv"]][["data"]])
        expect_equal(v[["view"]], c("s1", "s2"))
        expect_equal(v[["step"]], c(s1 = "s1", s2 = "s2"))
        expect_equal(v[["out"]], list(s1 = 1, s2 = 2))
    })

    it("extracts a single cell from a view by step name or row index", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = c("s1", "s2"))

        expect_equal(v[["s2", "out"]], 2)
        expect_equal(v[[2, "step"]], "s2")
    })

    it("extracts the cell of the named step from a view of later rows", {
        p <- pip_new("p") |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(y = 2) y) |>
            pip_add("c", \(z = 3) z)
        v <- pip_view(p, step = c("b", "c"))

        expect_equal(v[["b", "params"]], list(y = 2))
        expect_equal(v[["c", "params"]], list(z = 3))
        expect_equal(v[[1, "step"]], "b")
        expect_error(v[["a", "params"]], "selected step not part of view: a")
        expect_error(v[["unknown", "params"]], "Unknown step names: unknown")
    })

    it("signals steps not covered by the view", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = "s1")

        expect_error(
            v[["s2", "out"]],
            "selected step not part of view: s2"
        )
    })

    it("signals out-of-bounds row indices for pipelines and views", {
        p <- test_pip()
        v <- pip_view(p, step = "s1")

        expect_error(p[[0L, "step"]], "row index out of bounds")
        expect_error(
            p[[nrow(p[["pipenv"]][["data"]]) + 1L, "step"]],
            "row index out of bounds"
        )
        expect_error(v[[0L, "step"]], "row index out of bounds")
        expect_error(v[[2L, "step"]], "row index out of bounds")
        expect_error(v[[length(v) + 1L, "step"]], "row index out of bounds")
    })
})


describe("virtual methods", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "init") |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("exposes methods via $ and [[ but not as stored fields", {
        p <- test_pip()

        expect_true(is.function(p$add))
        expect_true(is.function(p[["run"]]))
        expect_true(is.function(p$restart))
        expect_true(is.function(p$halt))
        expect_false(any(c("add", "run", "restart", "halt") %in% names(p)))
    })

    it("adds steps through the virtual method", {
        p <- test_pip()$add("s3", \(x = ~s2) x * 10)

        expect_equal(unname(p[["step"]]), c("s1", "s2", "s3"))
        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[3]], 20)
    })

    it("chains mutating methods", {
        p <- pip_new("pipe")$add("s1", \(x = 1) x)$add("s2", \(x = ~s1) x + 1)

        expect_equal(unname(p[["step"]]), c("s1", "s2"))
    })

    it("runs the pipeline through the virtual method", {
        p <- test_pip()$run(lgr = NULL)

        expect_equal(p[["out"]][[1]], 1)
        expect_equal(p[["out"]][[2]], 2)
    })

    it("renames and collects outputs through virtual methods", {
        p <- test_pip()$rename("s1", "first")$run(lgr = NULL)

        expect_equal(unname(p[["step"]]), c("first", "s2"))
        expect_equal(p$collect(), list(first = 1, s2 = 2))
    })

    it("applies methods to views", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = "s2")

        expect_equal(v$collect(), list(s2 = 2))
        expect_true(is.function(v$restart))
    })

    it("keeps the same pipeline when chaining on a view", {
        p <- test_pip()
        v <- pip_view(p, step = "s2")

        expect_identical(v$pipenv, p$pipenv)
    })

    it("does not shadow step-table columns", {
        p <- test_pip()

        expect_equal(p[["step"]], c(s1 = "s1", s2 = "s2"))
        expect_equal(p[["state"]], c(s1 = "new", s2 = "new"))
        expect_null(p[["unknown"]])
    })

    it("signals restart and halt on the underlying pipeline", {
        p <- test_pip()
        v <- pip_view(p, step = "s2")

        p$restart()
        expect_equal(as.character(p[["pipenv"]][[".run_state"]]), "restart")
        expect_equal(p[["pipenv"]][[".restart_count"]], 1L)

        v$halt()
        expect_equal(as.character(p[["pipenv"]][[".run_state"]]), "halted")
    })

    it("builds the method table lazily and caches it", {
        expect_identical(
            .pip_method_table(),
            .pip_method_table()
        )
        expect_true(
            exists("add", envir = .pip_method_table(), inherits = FALSE)
        )
        expect_false(
            exists("view", envir = .pip_method_table(), inherits = FALSE)
        )
    })
})


describe("assignment operator [[<-", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "init") |>
            pip_add("s2", \(x = ~s1) x + 1) |>
            pip_add("s3", \(x = ~s2) x + 1)
    }

    it("can assign a new name", {
        p <- test_pip()
        p[["name"]] <- "renamed"
        expect_equal(p[["name"]], "renamed")
    })

    it("can assign or remove a view", {
        p <- test_pip()
        p[["view"]] <- c("s1", "s3")

        expect_true(inherits(p, "pipeflow"))
        expect_true(.is_pipeflow_view(p))
        expect_equal(p[["view"]], c("s1", "s3"))
        expect_equal(unname(p[["step"]]), c("s1", "s3"))
        expect_equal(nrow(p), 2L)

        # Re-assigning replaces the current view
        p[["view"]] <- "s2"
        expect_equal(p[["view"]], "s2")
        expect_equal(unname(p[["step"]]), "s2")

        # Re-assigning a view keeps the pipeline name
        expect_equal(p[["name"]], "pipe")

        # Clearing the view turns it back into a full pipeline
        p[["view"]] <- NULL
        expect_false(.is_pipeflow_view(p))
        expect_null(p[["view"]])
        expect_equal(nrow(p), 3L)
    })

    it("signals invalid view assignments", {
        p <- test_pip()

        expect_error(
            p[["view"]] <- 2L,
            "filter 'step' must be a character vector"
        )
        expect_error(
            p[["view"]] <- list("s1"),
            "filter 'step' must be a character vector"
        )
    })

    it("can rename a step", {
        p <- test_pip()
        p[["s2", "step"]] <- "s2_renamed"
        expect_equal(
            unname(p[["step"]]),
            c("s1", "s2_renamed", "s3")
        )
        expect_equal(unname(p[["depends"]][[2]]), "s1")
    })

    it("removes a step by assigning NULL to the step property", {
        p <- test_pip()

        p[["s3", "step"]] <- NULL

        expect_equal(unname(p[["step"]]), c("s1", "s2"))
        expect_equal(unname(p[["depends"]][[2]]), "s1")
    })

    it("removes a step and its downstream steps via NULL", {
        p <- test_pip()

        expect_message(
            p[["s2", "step"]] <- NULL,
            "Removing step 's2' and its downstream dependencies"
        )
        expect_equal(unname(p[["step"]]), "s1")
    })

    it("keeps views valid when removing a step via NULL", {
        p <- test_pip()
        v <- pip_view(p, step = c("s1", "s2"))
        other <- pip_view(p, step = c("s1", "s2"))

        expect_message(
            v[["s2", "step"]] <- NULL,
            "Removing step 's2' and its downstream dependencies"
        )

        expect_equal(unname(p[["step"]]), "s1")
        expect_equal(unname(v[["step"]]), "s1")
        expect_equal(v[["view"]], "s1")
        expect_equal(other[["view"]], "s1")
    })

    it("can assign and clear tags", {
        p <- test_pip()
        p[["s1", "tags"]] <- c("a", "b")
        expect_equal(p[["tags"]][[1]], c("a", "b"))
        p[["s1", "tags"]] <- NULL
        expect_equal(p[["tags"]][[1]], character(0))
    })

    it("can assign locked status", {
        p <- test_pip()
        p[["s1", "locked"]] <- TRUE
        expect_true(p[["locked"]][[1]])
        p[["s1", "locked"]] <- FALSE
        expect_false(p[["locked"]][[1]])
    })

    it("can assign execution mode", {
        p <- test_pip()
        p[["s2", "exec"]] <- "plain"
        expect_equal(p[["exec"]][[2]], "plain")

        expect_error(p[["s2", "exec"]] <- "bad_mode", "exec must be one of")
        expect_error(p[["s2", "exec"]] <- NULL, "must be a single string")
    })

    it("can assign states", {
        p <- test_pip()
        p[["s1", "state"]] <- "outdated"
        expect_equal(p[["state"]][[1]], "outdated")

        expect_error(p[["s2", "state"]] <- "bad_state", "state must be one of")
        expect_error(p[["s2", "state"]] <- NULL, "must be a single string")
    })

    it("can assign time stamps", {
        p <- test_pip()
        when <- as.POSIXct("2020-01-01 12:00:00", tz = "UTC")
        p[["s1", "time"]] <- when
        expect_equal(
            format(p[["time"]][[1]], tz = "UTC"),
            "2020-01-01 12:00:00"
        )

        expect_error(
            p[["s2", "time"]] <- 1,
            "time must be a single POSIXct value"
        )
        expect_error(
            p[["s2", "time"]] <- as.Date("2020-01-01"),
            "time must be a single POSIXct value"
        )
        expect_error(
            p[["s2", "time"]] <- as.POSIXct(NA),
            "time must be a single POSIXct value"
        )
    })

    it("can assign output values", {
        p <- test_pip()
        p[["s1", "out"]] <- 10
        expect_equal(p[["out"]][[1]], 10)
    })

    it("supports assigning by row index", {
        p <- test_pip()

        p[[2, "tags"]] <- "byrow"
        expect_equal(p[["tags"]][[2]], "byrow")
    })

    it("replaces a step function while keeping tags and exec mode", {
        p <- test_pip()
        p[["s2", "tags"]] <- "model"
        p[["s2", "exec"]] <- "plain"
        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[2]], 2)

        p[["s2", "fun"]] <- \(x = ~s1) x * 3

        expect_equal(p[["tags"]][[2]], "model")
        expect_equal(p[["exec"]][[2]], "plain")
        expect_equal(unname(p[["depends"]][[2]]), "s1")
        expect_equal(p[["state"]][[2]], "new")

        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[2]], 3)
    })

    it("keeps multiple tags and a non-default exec mode via [[<-", {
        p <- test_pip()
        p[["s2", "tags"]] <- c("model", "core")
        p[["s2", "exec"]] <- "reduce"

        p[["s2", "fun"]] <- \(x = ~s1) x * 3

        expect_equal(p[["s2", "tags"]], c("model", "core"))
        expect_equal(p[["s2", "exec"]], "reduce")
    })

    it("keeps empty tags and the default exec mode via [[<-", {
        p <- test_pip()

        p[["s2", "fun"]] <- \(x = ~s1) x * 3

        expect_equal(p[["s2", "tags"]], character(0))
        expect_equal(p[["s2", "exec"]], "auto")
    })

    it("keeps tags and exec mode when replacing through a view", {
        p <- test_pip()
        p[["s2", "tags"]] <- "model"
        p[["s2", "exec"]] <- "plain"
        v <- pip_view(p, step = c("s2", "s3"))

        # the integer index is relative to the view
        v[[1, "fun"]] <- \(x = ~s1) x * 3

        expect_equal(p[["s2", "tags"]], "model")
        expect_equal(p[["s2", "exec"]], "plain")
        expect_equal(p[["s2", "state"]], "new")
    })

    it("gives the same result as pip_replace() with default tags and exec", {
        p1 <- test_pip()
        p2 <- test_pip()
        for (p in list(p1, p2)) {
            p[["s2", "tags"]] <- c("a", "b")
            p[["s2", "exec"]] <- "plain"
        }
        f <- \(x = ~s1) x * 3

        p1[["s2", "fun"]] <- f
        pip_replace(p2, "s2", f)

        expect_equal(p1[["tags"]], p2[["tags"]])
        expect_equal(p1[["exec"]], p2[["exec"]])
        expect_equal(p1[["depends"]], p2[["depends"]])
    })

    it("marks downstream steps outdated when replacing a function", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1) |>
            pip_add("s3", \(x = ~s2) x + 1)
        pip_run(p, lgr = NULL)

        p[["s2", "fun"]] <- \(x = ~s1) x * 2

        expect_equal(
            unname(p[["state"]]),
            c("done", "new", "outdated")
        )
    })

    it("takes try() references from the new function", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~ try(s1)) x + 1)

        p[["s2", "fun"]] <- \(x = ~s1) x * 2
        expect_equal(formula_try_args(p[["s2", "params"]]), character(0))

        p[["s2", "fun"]] <- \(y = ~ try(s1)) y
        expect_equal(formula_try_args(p[["s2", "params"]]), "y")
    })

    it("updates the parameters of a single step", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1, k = 2) x * k)

        p[["s2", "params"]] <- list(k = 5)
        expect_equal(p[["params"]][[2]][["k"]], 5)
    })

    it("signals invalid shortcut assignments", {
        p <- test_pip()

        expect_error(
            p[["name"]] <- 1,
            "name must be a non-empty string"
        )
        expect_error(
            p[["name"]] <- "",
            "name must be a non-empty string"
        )
        expect_error(
            p[["unknown", "tags"]] <- "a",
            "element or step 'unknown' does not exist"
        )
        expect_error(
            p[[TRUE, "tags"]] <- "a",
            "i must be a step name or row index"
        )
        expect_error(
            p[["s1"]] <- \(x = 1) x,
            "j must be provided"
        )
        expect_error(
            p[["s1", "depends"]] <- 1,
            "direct assignment to column 'depends' is not supported"
        )
        expect_error(
            p[["s1", "nope"]] <- 1,
            "unknown step property: nope"
        )
        expect_error(
            p[["s1", "tags"]] <- 1,
            "tags must be a character vector"
        )
        expect_error(
            p[["s1", "locked"]] <- "yes",
            "locked must be a single logical value"
        )
        expect_error(
            p[["s1", "exec"]] <- "bogus",
            "exec must be one of"
        )
        expect_error(
            p[[99L, "tags"]] <- "a",
            "row index 99 out of bounds [1, 3]",
            fixed = TRUE
        )
        expect_error(
            p[[c("s1", "s2"), "tags"]] <- "a",
            "i must be of length 1"
        )
        expect_error(
            p[["s1", c("tags", "locked")]] <- "a",
            "j must be a single step property name"
        )
    })

    it("supports views with view-relative indices and step names", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "a") |>
            pip_add("s2", \(x = ~s1) x + 1) |>
            pip_add("s3", \(x = ~s2) x + 1, tags = "c")

        v <- pip_view(p, step = c("s2", "s3"))

        # Integer indices refer to the visible steps of the view
        v[[1, "tags"]] <- "b"
        expect_equal(p[["tags"]][[2]], "b")
        expect_equal(p[["tags"]][[1]], "a")

        # Step names must be covered by the view
        expect_error(
            v[["s1", "tags"]] <- "x",
            "step 's1' is not part of the view"
        )

        # Bounds are relative to the view
        expect_error(
            v[[3, "tags"]] <- "x",
            "row index 3 out of bounds [1, 2]",
            fixed = TRUE
        )

        # Assignments are shared with the originating pipeline
        v[[2, "state"]] <- "outdated"
        expect_equal(p[["state"]][[3]], "outdated")
    })

    it("supports rename, replace and params through views", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1, k = 2) x * k) |>
            pip_add("s3", \(x = ~s2) x + 1)

        v <- pip_view(p, step = c("s2", "s3"))

        v[["s2", "params"]] <- list(k = 5)
        expect_equal(p[["params"]][[2]][["k"]], 5)

        v[["s2", "step"]] <- "renamed"
        expect_equal(
            unname(p[["step"]]),
            c("s1", "renamed", "s3")
        )
        expect_equal(
            unname(p[["depends"]][[3]]),
            "renamed"
        )

        v <- pip_view(p, step = c("renamed", "s3"))
        v[["renamed", "fun"]] <- \(x = ~s1, k = 2) x * k
        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[2]], 2)
    })
})


describe("[<-.pipeflow", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1) |>
            pip_add("s3", \(x = ~s2) x + 1)
    }

    it("assigns element-wise by step name", {
        p <- test_pip()
        p[c("s1", "s3"), "tags"] <- c("io", "report")
        expect_equal(p[["tags"]][[1]], "io")
        expect_equal(p[["tags"]][[2]], character(0))
        expect_equal(p[["tags"]][[3]], "report")
    })

    it("replicates a length-1 value to all selected rows", {
        p <- test_pip()
        p[1:2, "state"] <- "outdated"
        expect_equal(unname(p[["state"]][1:2]), c("outdated", "outdated"))
        expect_equal(p[["state"]][[3]], "new")
    })

    it("replicates a single list value (e.g. tags) to all rows", {
        p <- test_pip()
        p[c("s1", "s2"), "tags"] <- list(c("a", "b"))
        expect_equal(p[["tags"]][[1]], c("a", "b"))
        expect_equal(p[["tags"]][[2]], c("a", "b"))
        expect_equal(p[["tags"]][[3]], character(0))
    })

    it("assigns list values element-wise", {
        p <- test_pip()
        p[c("s1", "s2"), "tags"] <- list("a", c("b", "c"))
        expect_equal(p[["tags"]][[1]], "a")
        expect_equal(p[["tags"]][[2]], c("b", "c"))
    })

    it("preserves the order of the selected rows", {
        p <- test_pip()
        p[c(3, 1), "state"] <- c("done", "new")
        expect_equal(unname(p[["state"]]), c("new", "new", "done"))
    })

    it("supports negative row indices", {
        p <- test_pip()

        p[-1, "tags"] <- "x"
        expect_equal(p[["tags"]][[1]], character(0))
        expect_equal(p[["tags"]][[2]], "x")
        expect_equal(p[["tags"]][[3]], "x")

        p[-c(1, 3), "state"] <- "outdated"
        expect_equal(unname(p[["state"]]), c("new", "outdated", "new"))

        expect_error(
            p[c(-1, 2), "tags"] <- "y",
            "only 0's may be mixed with negative"
        )

        # Excluding all rows is a no-op
        p[-(1:3), "tags"] <- "z"
        expect_equal(p[["tags"]][[2]], "x")
    })

    it("supports negative row indices on views", {
        p <- test_pip()
        v <- pip_view(p, step = c("s2", "s3"))

        # Negative indices are relative to the covered rows
        v[-1, "tags"] <- "y"
        expect_equal(p[["tags"]][[2]], character(0))
        expect_equal(p[["tags"]][[3]], "y")
    })

    it("applies the last write for duplicated row indices", {
        p <- test_pip()
        p[c(1, 1), "tags"] <- c("a", "b")
        expect_equal(p[["tags"]][[1]], "b")
    })

    it("recycles a divisor-length value", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a) x) |>
            pip_add("c", \(x = ~b) x) |>
            pip_add("d", \(x = ~c) x)
        p[1:4, "state"] <- c("done", "new")
        expect_equal(
            unname(p[["state"]]),
            c("done", "new", "done", "new")
        )
    })

    it("errors on a non-multiple replacement length", {
        p <- test_pip()
        expect_error(
            p[1:3, "tags"] <- c("a", "b"),
            "replacement has length 2, data has 3",
            fixed = TRUE
        )
    })

    it("stores an arbitrary value as-is for a single row", {
        p <- test_pip()
        p["s1", "out"] <- data.frame(a = 1:3)
        expect_equal(p[["out"]][[1]], data.frame(a = 1:3))

        p["s2", "tags"] <- c("x", "y")
        expect_equal(p[["tags"]][[2]], c("x", "y"))
    })

    it("clears tags with a NULL value", {
        p <- test_pip()
        p[c("s1", "s2"), "tags"] <- NULL
        expect_equal(p[["tags"]][[1]], character(0))
        expect_equal(p[["tags"]][[2]], character(0))
    })

    it("assigns all rows with a missing index", {
        p <- test_pip()
        p[, "state"] <- "outdated"
        expect_equal(unname(p[["state"]]), rep("outdated", 3))
    })

    it("is a no-op for an empty row selection", {
        p <- test_pip()
        p[integer(0), "state"] <- "outdated"
        expect_equal(unname(p[["state"]]), rep("new", 3))
    })

    it("supports boolean row filters", {
        p <- test_pip()
        p[step %like% "^s[12]$", "locked"] <- TRUE
        expect_equal(
            unname(p[["locked"]]),
            c(TRUE, TRUE, FALSE)
        )
    })

    it("supports the full set of writable properties", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1, k = 2) x + k) |>
            pip_add("s2", \(x = ~s1, k = 3) x + k)
        when <- as.POSIXct(
            c("2020-01-01 12:00:00", "2020-01-02 12:00:00"),
            tz = "UTC"
        )

        p[1:2, "exec"] <- "plain"
        p[1:2, "time"] <- when
        p[1:2, "out"] <- list(1, 2)
        p[1:2, "params"] <- list(list(k = 5), list(k = 6))
        p[1:2, "locked"] <- TRUE

        expect_true(all(p[["locked"]]))
        expect_equal(unname(p[["exec"]]), c("plain", "plain"))
        expect_equal(
            format(unname(p[["time"]]), tz = "UTC", usetz = FALSE),
            c("2020-01-01 12:00:00", "2020-01-02 12:00:00")
        )
        expect_equal(unname(p[["out"]]), list(1, 2))
        expect_equal(p[["params"]][[1]][["k"]], 5)
        expect_equal(p[["params"]][[2]][["k"]], 6)
    })

    it("replicates a single function to all selected rows", {
        p <- test_pip()
        p[1:2, "fun"] <- \(x = 5) x
        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[1]], 5)
        expect_equal(p[["out"]][[2]], 5)
    })

    it("renames multiple steps element-wise", {
        p <- test_pip()
        p[c("s2", "s3"), "step"] <- c("x", "y")
        expect_equal(unname(p[["step"]]), c("s1", "x", "y"))
    })

    it("fails on step name clashes during bulk renames", {
        p <- test_pip()
        expect_error(
            p[c("s1", "s2"), "step"] <- c("s2", "s1"),
            "step 's2' already exists"
        )
    })

    it("removes multiple steps via NULL on the step property", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = 2) x) |>
            pip_add("s3", \(x = 3) x) |>
            pip_add("s4", \(x = 4) x)

        p[c("s2", "s4"), "step"] <- NULL

        expect_equal(unname(p[["step"]]), c("s1", "s3"))
    })

    it("writes through views with view-relative indices", {
        p <- test_pip()
        v <- pip_view(p, step = c("s2", "s3"))

        v[1:2, "tags"] <- c("a", "b")
        expect_equal(p[["tags"]][[2]], "a")
        expect_equal(p[["tags"]][[3]], "b")

        expect_error(
            v[["s1", "tags"]] <- "x",
            "step 's1' is not part of the view"
        )
        expect_error(
            v[3, "tags"] <- "x",
            "Invalid row indices in 'i': 3"
        )
    })

    it("signals invalid inputs", {
        p <- test_pip()

        expect_error(p[1:2] <- 5, "j must be provided")
        expect_error(
            p[1:2, c("a", "b")] <- 5,
            "j must be a single step property name"
        )
        expect_error(
            p[1:2, ""] <- 5,
            "j must be a single step property name"
        )
        expect_error(
            p[1:2, NA_character_] <- 5,
            "j must be a single step property name"
        )
        expect_error(
            p[1:2, "depends"] <- "s1",
            "direct assignment to column 'depends' is not supported."
        )
        expect_error(
            p[1:2, "nope"] <- 1,
            "unknown step property: nope"
        )
    })
})


describe("cross-pipeline assignment", {
    test_pip <- function(name = "p") {
        pip_new(name) |>
            pip_add("s1", \(x = 1, k = 2) x + k) |>
            pip_add("s2", \(x = ~s1) x * 2)
    }

    it("copies the runtime state and annotation columns", {
        p <- test_pip()
        q <- pip_clone(p)
        q[1:2, "out"] <- list(10, 20)
        q[1:2, "tags"] <- c("a", "b")
        q[1:2, "state"] <- "done"
        q[1:2, "locked"] <- TRUE
        q[1:2, "exec"] <- "plain"

        p[1:2, ] <- q[1:2, ]

        expect_equal(unname(p[["out"]]), list(10, 20))
        expect_equal(p[["tags"]], q[["tags"]])
        expect_equal(unname(p[["state"]]), c("done", "done"))
        expect_equal(unname(p[["locked"]]), c(TRUE, TRUE))
        expect_equal(unname(p[["exec"]]), c("plain", "plain"))
    })

    it("copies the function and the unbound params, then runs", {
        p <- test_pip()
        q <- pip_clone(p)
        q[["s1", "fun"]] <- \(x = 1, k = 2) x * k
        q[["s2", "fun"]] <- \(x = ~s1, k = 2) x * k
        q[["s2", "params"]] <- list(k = 3)

        p[1:2, ] <- q[1:2, ]

        pip_run(p, lgr = NULL)
        expect_equal(p[["out"]][[1]], 2)
        expect_equal(p[["out"]][[2]], 6)
    })

    it("keeps the step names when both pipelines use the same names", {
        p <- test_pip()
        q <- pip_clone(p)
        q[1:2, "tags"] <- c("x", "y")

        p[1:2, ] <- q[1:2, ]

        expect_equal(unname(p[["step"]]), c("s1", "s2"))
        expect_equal(p[["tags"]], q[["tags"]])
    })

    it("renames the target steps to the source names", {
        p <- test_pip()
        q <- pip_clone(p)
        pip_rename(q, "s1", "first")
        pip_rename(q, "s2", "second")
        # Function defaults still reference the old names; point the renamed
        # step at the renamed upstream so the replacement resolves in `p`.
        q[["second", "fun"]] <- \(x = ~first, k = 2) x * k

        p[1:2, ] <- q[1:2, ]

        expect_equal(unname(p[["step"]]), c("first", "second"))
        expect_equal(unname(p[["depends"]][[2]]), "first")
    })

    it("takes the references from the params, also after a rename", {
        p <- test_pip()
        q <- pip_clone(p)
        pip_rename(q, "s1", "first")

        p[1:2, ] <- q[1:2, ]

        expect_equal(unname(p[["step"]]), c("first", "s2"))
        expect_equal(p[["depends"]][["s2"]], c(x = "first"))
    })

    it("copies params that were given via params", {
        p <- test_pip()
        q <- pip_new("q") |>
            pip_add("data", \(x = c(1, NA, 3)) x) |>
            pip_add("avg", mean, params = list(x = ~data, na.rm = TRUE))

        p[1:2, ] <- q[1:2, ]

        expect_equal(unname(p[["step"]]), c("data", "avg"))
        expect_equal(p[["params"]][["avg"]], q[["params"]][["avg"]])
        expect_equal(p[["depends"]][["avg"]], c(x = "data"))
        pip_run(p, lgr = NULL)
        expect_equal(p[["avg", "out"]], 2)
    })

    it("copies params into a view of the target", {
        p <- test_pip()
        q <- pip_clone(p)
        pip_replace(q, "s2", \(x = ~s1, ...) x, params = list(z = 99))

        vp <- pip_view(p, step = "s2")
        vp[1, ] <- q[2, ]

        expect_equal(p[["params"]][["s2"]], list(x = ~s1, z = 99))
    })

    it("errors when the source has a different number of steps", {
        p <- test_pip()
        q <- pip_clone(p)
        pip_add(q, "s3", \(x = ~s2) x)

        expect_error(
            p[1:2, ] <- q,
            "cannot assign from a pipeline with 3 steps to 2 selected rows"
        )
    })

    it("requires i and forbids j when assigning from a pipeline", {
        p <- test_pip()
        q <- pip_clone(p)

        expect_error(p[] <- q, "i must be provided")
        expect_error(
            p[1:2, "tags"] <- q,
            "j must not be provided when assigning from a pipeline"
        )
    })

    it("copies a single property via p[i, j] <- q[i, j]", {
        p <- test_pip()
        q <- pip_clone(p)
        q[1:2, "tags"] <- list(c("t1"), c("t2", "t3"))
        q[1:2, "state"] <- c("outdated", "done")

        p[1:2, "tags"] <- q[1:2, "tags"]
        p[1:2, "state"] <- q[1:2, "state"]

        expect_equal(p[["tags"]][[1]], "t1")
        expect_equal(p[["tags"]][[2]], c("t2", "t3"))
        expect_equal(unname(p[["state"]]), c("outdated", "done"))
    })

    it("copies a single-cell value via p[i, j] <- q[i, j]", {
        p <- test_pip()
        q <- pip_clone(p)
        q["s1", "out"] <- data.frame(a = 1:3)

        p["s1", "out"] <- q["s1", "out"]

        expect_equal(p[["out"]][[1]], data.frame(a = 1:3))
    })

    it("overwrites locked target steps", {
        p <- test_pip()
        q <- pip_clone(p)
        q[1:2, "locked"] <- c(TRUE, FALSE)
        p[1:2, "locked"] <- TRUE

        p[1:2, ] <- q[1:2, ]

        expect_equal(unname(p[["locked"]]), c(TRUE, FALSE))
    })

    it("is a no-op when copying a pipeline onto itself", {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        p[1:2, ] <- p[1:2, ]

        expect_equal(unname(p[["out"]]), list(3, 6))
        expect_equal(unname(p[["state"]]), c("done", "done"))
    })

    it("keeps the dependencies of relative references after an insertion", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = 2) x) |>
            pip_add("c", \(x = ~ -1) x) |>
            pip_add("d", \(x = ~b) x, params = list(x = ~ try(-2)))
        pip_add(p, "inserted", \(x = 99) x, after = "b")

        p[4:5, ] <- p[4:5, ]

        expect_equal(p[["depends"]][["c"]], c(x = "b"))
        expect_equal(p[["depends"]][["d"]], c(x = "b"))
        expect_equal(formula_try_args(p[["d", "params"]]), "x")
        pip_run(p, lgr = NULL)
        expect_equal(p[["c", "out"]], 2)
    })

    it("keeps the dependencies of relative references after a removal", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = 2) x) |>
            pip_add("c", \(x = 0) x, params = list(x = ~ -2))
        pip_remove(p, "b")

        p[2, ] <- p[2, ]

        expect_equal(p[["depends"]][["c"]], c(x = "a"))
    })

    it("refers to the steps of relative references by name", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = 2) x) |>
            pip_add("extra", \(x = 3) x) |>
            pip_add("c", \(x = 0) x)
        q <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = 2) x) |>
            pip_add("c", \(x = 0) x * 10, params = list(x = ~ -1))

        p["c", ] <- q["c", ]

        expect_equal(p[["depends"]][["c"]], c(x = "b"))
        pip_run(p, lgr = NULL)
        expect_equal(p[["c", "out"]], 20)
    })

    it("copies NULL values given via params", {
        p <- test_pip()
        q <- pip_new("q") |>
            pip_add("s1", \(x) is.null(x), params = list(x = NULL)) |>
            pip_add("s2", \(x = ~s1) x)

        p[1:2, ] <- q[1:2, ]

        expect_equal(p[["params"]][["s1"]], list(x = NULL))
        pip_run(p, lgr = NULL)
        expect_true(p[["s2", "out"]])
    })

    # The function made by as.function() sets formula arguments as values
    set_formula_value <- function(x, ...) {
        .pip_set_params(x, list(...), bind = FALSE)
    }

    it("keeps a formula value of an unbound param as a value", {
        q <- pip_new() |>
            pip_add("s", \(model = NULL) model)
        set_formula_value(q, model = ~x)

        q["s", ] <- q["s", ]

        expect_equal(q[["s", "params"]], list(model = ~x))
        expect_equal(q[["depends"]][["s"]], character(0))
        expect_equal(q[["unbound"]][["s"]], "model")
    })

    it("does not bind a formula value to a step of the same name", {
        p <- pip_new() |>
            pip_add("x", \() 1) |>
            pip_add("s", \() 0)
        q <- pip_new() |>
            pip_add("x", \() 1) |>
            pip_add("s", \(model = NULL, data = ~x) list(model, data))
        set_formula_value(q, model = ~x)

        p[2, ] <- q[2, ]

        expect_equal(p[["depends"]][["s"]], c(data = "x"))
        expect_equal(p[["unbound"]][["s"]], "model")
        pip_run(p, lgr = NULL)
        expect_equal(p[["s", "out"]], list(~x, 1))
    })

    it("keeps formula values in a pipeline of an as.function() run", {
        inner <- pip_new("inner") |>
            pip_add("x", \() 1) |>
            pip_add("s", \(model = NULL) model) |>
            pip_add("snap", \(dummy = ~s) pip_clone(.self))
        snap <- as.function(inner, out = "snap")(model = ~x)

        snap["s", ] <- snap["s", ]

        expect_equal(snap[["depends"]][["s"]], character(0))
        pip_run(snap, force = TRUE, lgr = NULL)
        expect_equal(snap[["s", "out"]], ~x)
    })

    it("copies a param that was bound via pip_set_params()", {
        p <- pip_new() |>
            pip_add("x", \() 1) |>
            pip_add("y", \() 2) |>
            pip_add("s", \() 0)
        q <- pip_new() |>
            pip_add("x", \() 1) |>
            pip_add("y", \() 2) |>
            pip_add("s", \(model = NULL) model)
        pip_set_params(q, list(model = ~ try(y)))

        p[3, ] <- q[3, ]

        expect_equal(p[["depends"]][["s"]], c(model = "y"))
        expect_equal(formula_try_args(p[["s", "params"]]), "model")
        pip_run(p, lgr = NULL)
        expect_equal(p[["s", "out"]], 2)
    })

    it("is a no-op for an empty row selection", {
        p <- test_pip()
        q <- pip_clone(p)

        p[integer(0), ] <- q[integer(0), ]

        expect_equal(unname(p[["step"]]), c("s1", "s2"))
    })

    it("errors when the source function references unknown steps", {
        p <- test_pip()
        q <- pip_new("q") |>
            pip_add("other", \(x = 1) x) |>
            pip_add("a", \(x = ~other) x)

        expect_error(
            p[1, ] <- q[2, ],
            "cannot reference unknown steps: 'other'"
        )
    })

    it("writes through views on both sides", {
        p <- test_pip()
        q <- pip_clone(p)
        q[1:2, "tags"] <- c("x", "y")
        q[["s2", "out"]] <- 99

        # View target: writes through to the underlying pipeline.
        vp <- pip_view(p, step = "s2")
        vp[1, ] <- q[2, ]
        expect_equal(p[["tags"]][[2]], "y")
        expect_equal(p[["out"]][[2]], 99)

        # View source: the copied rows are the covered steps.
        vq <- pip_view(q, step = "s1")
        vp <- pip_view(p, step = "s1")
        vp[1, ] <- vq[1, ]
        expect_equal(p[["tags"]][[1]], "x")
    })

    it("copies try() references with the function", {
        p <- pip_new() |>
            pip_add("s0", \(x = 0) x) |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~ try(s1)) x)
        q <- pip_new() |>
            pip_add("s0", \(x = 0) x) |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(y = ~ try(s1), z = ~s0) y)

        p[3, ] <- q[3, ]

        expect_equal(formula_try_args(p[["s2", "params"]]), "y")
    })
})


describe("benchmarking", {
    skip("benchmarking tests are skipped by default")
    v <- c("hello", "world")
    w <- c("hello", "this", "is", "my", "world")
    grepv(pattern = v, x = w)
    `%chin%` <- data.table::`%chin%`

    N <- 1e3
    u <- as.character(as.hexmode(1:10000))
    y <- sample(u, N, replace = TRUE)
    x <- sample(u, 100)
    system.time(x %in% y)
    system.time(x %chin% y)

    microbenchmark(
        y %in% x,
        y %chin% x,
        times = 1000
    )

    # Please type 'example(chmatch)' to run this and see timings on your machine

    microbenchmark(x %in% y, x %chin% y, times = 100)
    system.time(a <- x %in% y) #  4.5s
    system.time(b <- x %chin% y) #  1.7s
    identical(a, b)

    # Different example with more unique strings ...
    u <- as.character(as.hexmode(1:(N / 10)))
    y <- sample(u, N, replace = TRUE)
    x <- sample(u, N, replace = TRUE)
    system.time(a <- match(x, y)) # 46s
    system.time(b <- chmatch(x, y)) # 16s
    identical(a, b)
})


describe("rbind", {
    test_pip <- function(name = "p") {
        pip_new(name) |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("signals invalid inputs", {
        p <- test_pip()
        expect_error(rbind(1, p), "x must be a pipeflow pip")
        expect_error(rbind(p, 1), "x must be a pipeflow pip")
    })

    it("binds pipelines without mutating inputs", {
        p1 <- test_pip("left")
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        out <- rbind(p1, p2)
        expect_true(.is_pipeflow(out))
        expect_equal(out[["name"]], "left-right")
        expect_equal(
            unname(out[["step"]]),
            c("s1", "s2", "t1", "t2")
        )

        pip_add(out, "extra", \(x = ~t2) x)
        expect_false("extra" %in% p1[["step"]])
        expect_false("extra" %in% p2[["step"]])
    })

    it("binds any number of pipelines and returns a single one unchanged", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")

        out <- rbind(p1, p2, p2)
        expect_equal(out[["name"]], "left-right-right")
        steps <- out[["step"]]
        expect_true(all(c("s1", "s2", "s12", "s22", "s13", "s23") %in% steps))
        expect_identical(anyDuplicated(steps), 0L)

        expect_identical(rbind(p1), p1)
    })

    it("auto-renames duplicated step names from second pipeline", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")

        out <- rbind(p1, p2)
        steps <- out[["step"]]
        expect_identical(anyDuplicated(steps), 0L)
        expect_true(all(c("s1", "s2", "s12", "s22") %in% steps))

        dep_new_s2 <- out[["pipenv"]][["data"]][step == "s22", depends][[1]]
        expect_equal(unname(dep_new_s2), "s12")
    })

    it("handles collisions of auto-fixed names", {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s12", \(x = 2) x)
        p2 <- pip_new("right") |>
            pip_add("s1", \(x = 3) x)

        out <- rbind(p1, p2)
        expect_true("s13" %in% out[["step"]])
    })

    it("rebuilds DAG and keeps dependencies valid in result", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")
        out <- rbind(p1, p2)

        nodes <- .pip_get_reachable_nodes(out, "s1")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s1", "s2"))

        nodes <- .pip_get_reachable_nodes(out, "s12")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s12", "s22"))
    })

    it("binds steps whose params were given via params", {
        p1 <- test_pip("left")
        p2 <- pip_new("right") |>
            pip_add("s1", \(x = c(1, NA, 3)) x) |>
            pip_add("avg", mean, params = list(x = ~s1, na.rm = TRUE))

        out <- rbind(p1, p2)

        expect_equal(out[["depends"]][["avg"]], c(x = "s12"))
        expect_true(out[["params"]][["avg"]][["na.rm"]])
        pip_run(out, lgr = NULL)
        expect_equal(out[["avg", "out"]], 2)
    })

    it("keeps NULL values of params", {
        p2 <- pip_new("right") |>
            pip_add("n1", \(x = NULL) is.null(x)) |>
            pip_add("n2", \(x = 1) is.null(x), params = list(x = NULL)) |>
            pip_add("n3", \(x) is.null(x), params = list(x = NULL))

        out <- rbind(test_pip("left"), p2)

        for (step in c("n1", "n2", "n3")) {
            expect_named(formals(out[[step, "fun"]]), "x")
        }
        pip_run(out, lgr = NULL)
        expect_equal(
            pip_collect(out[c("n1", "n2", "n3")]),
            list(
                n1 = TRUE,
                n2 = TRUE,
                n3 = TRUE
            )
        )
    })

    it("rebinds .self references to the bound pipeline", {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) .self[["name"]])
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 1) .self[["name"]])

        out <- rbind(p1, p2)
        pip_run(out, lgr = NULL)
        expect_equal(
            unname(out[["out"]]),
            list("left-right", "left-right")
        )
    })

    it("preserves runtime state from both source pipelines", {
        p1 <- test_pip("left")
        data.table::set(
            p1[["pipenv"]][["data"]],
            i = 1L,
            j = "out",
            value = list(10)
        )
        data.table::set(
            p1[["pipenv"]][["data"]],
            i = 1L,
            j = "state",
            value = "done"
        )
        data.table::set(
            p1[["pipenv"]][["data"]],
            i = 1L,
            j = "locked",
            value = TRUE
        )

        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        data.table::set(
            p2[["pipenv"]][["data"]],
            j = "out",
            value = list(7, 9)
        )
        data.table::set(
            p2[["pipenv"]][["data"]],
            j = "state",
            value = c("done", "outdated")
        )
        data.table::set(
            p2[["pipenv"]][["data"]],
            j = "locked",
            value = c(FALSE, TRUE)
        )

        out <- rbind(p1, p2)
        actualOut <- unname(out[["out"]])
        expectedOut <- list(10, NULL, 7, 9)
        expect_equal(actualOut, expectedOut)
        expect_equal(
            unname(out[["state"]]),
            c("done", "new", "done", "outdated")
        )
        expect_equal(
            unname(out[["locked"]]),
            c(TRUE, FALSE, FALSE, TRUE)
        )
    })

    it("keeps try() references when steps are renamed", {
        p1 <- pip_new("left") |>
            pip_add("data", \(x = 1) x)
        p2 <- pip_new("right") |>
            pip_add("data", \(x = 2) stop("boom")) |>
            pip_add("other", \(x = 3) x) |>
            pip_add("doc", \(d = ~ try(data), e = ~ try(-1)) {
                conditionMessage(d)
            })

        out <- rbind(p1, p2)

        expect_equal(
            unname(out[["step"]]),
            c("data", "data2", "other", "doc")
        )
        expect_equal(
            out[["doc", "depends"]],
            c(d = "data2", e = "other")
        )
        expect_equal(
            out[["doc", "params"]][c("d", "e")],
            list(d = ~ try(data2), e = ~ try(other)),
            ignore_attr = TRUE
        )
        expect_equal(
            formals(out[["doc", "fun"]])[["d"]],
            ~ try(data2),
            ignore_attr = TRUE
        )

        suppressWarnings(pip_run(out, lgr = NULL, on_error = "continue"))
        expect_equal(out[["doc", "out"]], "step 'data2' failed: boom")
    })

    it("binds pipelines read with readRDS()", {
        p1 <- test_pip("left")
        p2 <- test_pip("right") |>
            pip_add("s3", \(x = ~s1) x * 2)
        pip_remove(p2, "s2")

        expected <- rbind(p1, p2)
        out <- rbind(save_and_read(p1), save_and_read(p2))
        cols <- c("step", "depends")
        expect_equal(out[, cols], expected[, cols])
        expect_equal(out[["nodeId"]], expected[["nodeId"]])
        expect_equal(
            dag_structure(.pip_dag(out[["pipenv"]])),
            dag_structure(.pip_dag(expected[["pipenv"]]))
        )

        pip_run(out, lgr = NULL)
        expect_equal(
            out[["out"]],
            list(s1 = 1, s2 = 2, s12 = 1, s3 = 2)
        )
    })

    it("binds a step referencing the same step twice without warning", {
        p <- pip_new("pipe") |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a, y = ~a) x + y)
        q <- pip_new("q") |>
            pip_add("a", \(x = 3) x)

        expect_no_warning(out <- rbind(q, p))
        pip_run(out, lgr = NULL)
        expect_equal(pip_collect(out), list(a = 3, a2 = 1, b = 2))
    })

    it("binds a pipeline referencing a step with a non-syntactic name", {
        p <- pip_new("p") |>
            pip_add("source data", \(x = 1) x)
        q <- pip_new("q") |>
            pip_add("source data", \(x = 3) x) |>
            pip_add("use", \(x = ~`source data`, y = ~ try(-1)) x + y)

        out <- rbind(p, q)
        expect_equal(
            out[["use", "depends"]],
            c(x = "source data2", y = "source data2")
        )
        expect_equal(
            out[["use", "params"]],
            list(x = ~`source data2`, y = ~ try(`source data2`)),
            ignore_attr = TRUE
        )
        pip_run(out, lgr = NULL)
        expect_equal(out[["use", "out"]], 6)
    })
})


describe("rbind followed by pip_set_params", {
    left_pip <- function() {
        pip_new("a") |>
            pip_add("prep", \(x = 1) x * 2)
    }
    right_pip <- function() {
        pip_new("b") |>
            pip_add("fit", \(data = NULL, k = 10) data + k)
    }

    it("binds an unbound param of the second pipeline to a step", {
        out <- rbind(left_pip(), right_pip())
        pip_set_params(out, list(data = ~prep))

        expect_equal(out[["fit", "depends"]], c(data = "prep"))
        expect_equal(out[["fit", "unbound"]], "k")
        expect_equal(pip_get_params(out), list(x = 1, k = 10))
        nodes <- .pip_get_reachable_nodes(out, "prep")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("prep", "fit"))

        pip_run(out, lgr = NULL)
        expect_equal(out[["fit", "out"]], 12)
    })

    it("outdates bound steps that have been run, with their downstream", {
        b <- right_pip() |>
            pip_add("other", \(y = 1) y) |>
            pip_add("report", \(fit = ~fit) fit * 2)
        pip_run(b, lgr = NULL)

        out <- rbind(left_pip(), b)
        pip_set_params(out, list(data = ~prep))
        expect_equal(
            out[["state"]],
            c(
                prep = "new",
                fit = "outdated",
                other = "done",
                report = "outdated"
            )
        )
        expect_equal(unname(b[["state"]]), rep("done", 3))
    })

    it("binds to the step of the first pipeline if step names clash", {
        b <- pip_new("b") |>
            pip_add("prep", \(x = 100) x) |>
            pip_add("fit", \(data = NULL, own = ~prep) data + own)

        out <- rbind(left_pip(), b)
        pip_set_params(out, list(data = ~prep))
        expect_equal(unname(out[["step"]]), c("prep", "prep2", "fit"))
        expect_equal(
            out[["fit", "depends"]],
            c(data = "prep", own = "prep2")
        )

        pip_run(out, lgr = NULL)
        expect_equal(out[["fit", "out"]], 102)
    })

    it("binds only the steps of the second pipeline via a view", {
        a <- pip_new("a") |>
            pip_add("prep", \(data = 1) data * 2)
        b <- pip_new("b") |>
            pip_add("prep", \(data = 0) data + 1) |>
            pip_add("fit", \(data = 0) data + 2)

        out <- rbind(a, b)
        expect_error(
            pip_set_params(out, list(data = ~prep)),
            "cannot bind 'data' to step 'prep' in steps that don't come after"
        )
        expect_equal(pip_get_params(out), list(data = 1))

        pip_set_params(out[-seq_along(a)], list(data = ~prep))
        expect_equal(out[["prep", "unbound"]], "data")
        expect_equal(out[["prep2", "depends"]], c(data = "prep"))
        expect_equal(out[["fit", "depends"]], c(data = "prep"))

        pip_run(out, lgr = NULL)
        expect_equal(
            pip_collect(out),
            list(prep = 2, prep2 = 3, fit = 4)
        )
    })

    it("binds a locked step only after unlocking it", {
        b <- right_pip()
        pip_run(b, lgr = NULL)
        pip_lock(b)

        out <- rbind(left_pip(), b)
        pip_set_params(out, list(data = ~prep))
        expect_equal(out[["fit", "depends"]], character(0))
        expect_equal(out[["fit", "unbound"]], c("data", "k"))

        v <- out["fit"]
        pip_unlock(v)
        pip_set_params(v, list(data = ~prep))
        pip_lock(v)
        expect_equal(out[["fit", "depends"]], c(data = "prep"))
        expect_true(out[["fit", "locked"]])

        pip_unlock(out)
        pip_run(out, lgr = NULL)
        expect_equal(out[["fit", "out"]], 12)
    })

    it("can be chained to bind more than two pipelines", {
        c3 <- pip_new("c") |>
            pip_add("fit", \(data = NULL) data * 100)

        ab <- rbind(left_pip(), right_pip())
        pip_set_params(ab, list(data = ~prep))
        out <- rbind(ab, c3)
        pip_set_params(out, list(data = ~fit))
        expect_equal(unname(out[["step"]]), c("prep", "fit", "fit2"))
        expect_equal(out[["fit2", "depends"]], c(data = "fit"))

        pip_run(out, lgr = NULL)
        expect_equal(out[["fit2", "out"]], 1200)
    })
})
