# -------
# Helpers
# -------

describe(".pip_update_downstream",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("updates states downstream of single node as expected",
    {
        p <- test_pip()

        expect_true(all(p$pipeline[["state"]] == "new"))
        .pip_update_downstream(p, "a1", what = "state", value = "outdated")

        expect_equal(
            p$pipeline[["state"]],
            c("outdated", "outdated", "new", "outdated", "new")
        )
    })

    it("can update states downstream of multiple nodes",
    {
        p <- test_pip()
        .pip_update_downstream(p,
            steps = c("a1", "b1"),
            what = "state",
            value = "outdated"
        )
        expect_true(all(p$pipeline[["state"]] == "outdated"))

        p <- test_pip()
        .pip_update_downstream(p,
            steps = c("a1", "b2"),
            what = "state",
            value = "outdated"
        )

        expect_equal(
            p$pipeline[["state"]],
            c("outdated", "outdated", "new", "outdated", "outdated")
        )
    })
})


# ---------------------------
# Exported pipeline functions
# ---------------------------

describe("pip_new",
{
    it("creates a new pipeline",
    {
        p <- pip_new()
        expect_true(.is_pipeflow_pip(p))
    })
})


describe("pip_add",
{
    it("signals if step is not a single non-empty string",
    {
        p <- pip_new()
        expect_error(pip_add(p, ""), "step must be a non-empty string")
        expect_error(pip_add(p, c("a", "b")), "step must be a single string")
    })

    it("signals if fun is not a function",
    {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", fun = "not a function"),
            "fun must be a function"
        )
    })

    it("signals if group is not a non-empty single defined string",
    {
        p <- pip_new()

        expect_error(
            pip_add(p, "s1", fun = \() 1, group = list("not a string")),
            "group must be a non-empty valid string"
        )
        expect_error(
            pip_add(p, "s1", fun = \() 1, group = ""),
            "group must be a non-empty valid string"
        )
        expect_error(
            pip_add(p, "s1", fun = \() 1, group = c("a", "b")),
            "group must be a non-empty valid string"
        )
    })

    it("signals duplicate step names",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 0) a)
        expect_error(pip_add(p, "s1", \(a = 0) a), "step 's1' already exists")
    })

    it("signals undefined dependencies",
    {
        p <- pip_new()
        expect_error(
            pip_add(p, "s1", \(x = ~undefined) x),
            "cannot reference unknown steps: 'undefined'"
        )
    })

    it("step can refer to previous step by relative number",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)
        pip_add(p, "s2", \(x = ~-1) 2*x)

        expect_equal(p$pipeline$depends[[2]], c(x = "s1"))
    })

    it("a bad relative step referal is signalled",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)

        expect_error(pip_add(p, "s2", \(x = ~-2) 2*x))
        expect_equal(length(p), 1L)
    })

    it("if add is aborted, pipeline remains unchanged",
    {
        p <- pip_new()
        pip_add(p, "s1", \(a = 5) a)
        expect_error(
            pip_add(p, "s2", \(x = ~-2) 2*x),
            "relative index -2 points outside pipeline"
        )
    })

    it("can refer to the pipeline itself via the .self argument",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1, .self = NULL) length(.self))
        expect_equal(p$pipeline[["params"]][[1]]$.self, p)
    })

    it("supports functions with wildcard arguments",
    {
        skip("move to test for pip_run_step")
        foo <-\(x, ...) {
        }
        v <- c(1, 2, NA, 3, 4)
        pip <- Pipeline$new("pipe", data = v)

        fargs <- list(x = ~data, na.rm = TRUE)
        pip$add("mean", fun = foo, fargs = fargs)

        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], mean(v, na.rm = TRUE))

        pip$set_params_at_step("mean", list(na.rm = FALSE))
        out <- pip$run()$collect_out()
        expect_equal(out[["mean"]], as.numeric(NA))
    })
})


describe("pip_bind",
{
    test_pip <- function(name = "p") {
        pip_new(name) |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("signals invalid inputs",
    {
        p <- test_pip()
        expect_error(pip_bind(1, p), "x must be a pipeflow pip")
        expect_error(pip_bind(p, 1), "y must be a pipeflow pip")
    })

    it("binds pipelines without mutating inputs",
    {
        p1 <- test_pip("left")
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        out <- pip_bind(p1, p2)
        expect_true(.is_pipeflow_pip(out))
        expect_equal(out[["name"]], "left-right")
        expect_equal(out[["pipeline"]][["step"]], c("s1", "s2", "t1", "t2"))

        pip_add(out, "extra", \(x = ~t2) x)
        expect_false(pip_has_step(p1, "extra"))
        expect_false(pip_has_step(p2, "extra"))
    })

    it("auto-renames duplicated step names from second pipeline",
    {
        p1 <- test_pip("left")
        p2 <- test_pip("right")

        out <- pip_bind(p1, p2)
        steps <- out[["pipeline"]][["step"]]
        expect_identical(anyDuplicated(steps), 0L)
        expect_true(all(c("s1", "s2", "s12", "s22") %in% steps))

        dep_new_s2 <- out[["pipeline"]][step == "s22", depends][[1]]
        expect_equal(unname(dep_new_s2), "s12")
    })

    it("handles collisions of auto-fixed names",
    {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s12", \(x = 2) x)
        p2 <- pip_new("right") |>
            pip_add("s1", \(x = 3) x)

        out <- pip_bind(p1, p2)
        expect_true(pip_has_step(out, "s13"))
    })

    it("rebuilds DAG and keeps dependencies valid in result",
    {
        p1 <- test_pip("left")
        p2 <- test_pip("right")
        out <- pip_bind(p1, p2)

        nodes <- .pip_get_downstream_nodes(out, "s1")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s1", "s2"))

        nodes <- .pip_get_downstream_nodes(out, "s12")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s12", "s22"))
    })

    it("rebinds .self references to the bound pipeline",
    {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1, .self = NULL) .self[["name"]])
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 1, .self = NULL) .self[["name"]])

        out <- pip_bind(p1, p2)
        expect_identical(out[["pipeline"]][["params"]][[1]]$.self, out)
        expect_identical(out[["pipeline"]][["params"]][[2]]$.self, out)
    })
})


describe("pip_add_from",
{
    test_source <- function() {
        pip_new("src") |>
            pip_add("base", \(x = 2) x, group = "g1") |>
            pip_add("calc", \(x = ~base, m = 3) x * m,
                group = "g2",
                tags = c("reuse", "math")
            )
    }

    it("signals invalid inputs",
    {
        src <- test_source()
        trg <- pip_new("target")

        expect_error(pip_add_from(1, "base", src), "x must be a pipeflow pip")
        expect_error(pip_add_from(trg, "base", 1), "y must be a pipeflow pip")
        expect_error(pip_add_from(trg, c("a", "b"), src))
        expect_error(pip_add_from(trg, NA_character_, src))
        expect_error(pip_add_from(trg, "", src))
        expect_error(
            pip_add_from(trg, "unknown", src),
            "does not exist in source pipeline"
        )
    })

    it("adds an independent step preserving group and tags",
    {
        src <- test_source()
        trg <- pip_new("target")

        res <- pip_add_from(trg, "base", src)
        expect_true(.is_pipeflow_pip(res))
        expect_true(pip_has_step(trg, "base"))

        grp <- trg[["pipeline"]][step == "base", group][[1]]
        tgs <- trg[["pipeline"]][step == "base", tags][[1]]
        expect_equal(grp, "g1")
        expect_equal(tgs, character(0))
    })

    it("adds dependent step when dependencies exist in target",
    {
        src <- test_source()
        trg <- pip_new("target") |>
            pip_add("base", \(x = 5) x)

        pip_add_from(trg, "calc", src)
        expect_true(pip_has_step(trg, "calc"))

        dep <- trg[["pipeline"]][step == "calc", depends][[1]]
        expect_equal(unname(dep), "base")

        pip_run(trg, lgr = NULL)
        out <- trg[["pipeline"]][step == "calc", out][[1]]
        expect_equal(out, 15)
    })

    it("signals when copied step depends on missing steps in target",
    {
        src <- test_source()
        trg <- pip_new("target")

        expect_error(
            pip_add_from(trg, "calc", src),
            "cannot reference unknown steps: 'base'"
        )
    })

    it("rebinds .self to target pipeline through pip_add",
    {
        src <- pip_new("src") |>
            pip_add("self", \(x = 1, .self = NULL) .self[["name"]])
        trg <- pip_new("target")

        pip_add_from(trg, "self", src)
        expect_identical(trg[["pipeline"]][["params"]][[1]]$.self, trg)
    })
})


describe("pip_rename_step",
{
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(a = 1) a) |>
            pip_add("f2", \(b = ~f1) b) |>
            pip_add("f3", \(a = ~f1, b = ~f2) a + b)
    }

    it("signals invalid inputs",
    {
        p <- test_pip()
        expect_error(pip_rename_step(1, "f1", "first"))
        expect_error(pip_rename_step(p, c("f1", "f2"), "first"))
        expect_error(pip_rename_step(p, NA_character_, "first"))
        expect_error(pip_rename_step(p, "", "first"))
        expect_error(pip_rename_step(p, "f1", c("a", "b")))
        expect_error(pip_rename_step(p, "f1", NA_character_))
        expect_error(pip_rename_step(p, "f1", ""))
    })

    it("signals missing old step and name clashes",
    {
        p <- test_pip()
        expect_error(
            pip_rename_step(p, "unknown", "first"),
            "step 'unknown' does not exist"
        )
        expect_error(
            pip_rename_step(p, "f1", "f2"),
            "step 'f2' already exists"
        )
    })

    it("renames step and updates dependencies",
    {
        p <- test_pip()
        pip_rename_step(p, from = "f1", to = "first")

        expect_equal(p[["pipeline"]][["step"]], c("first", "f2", "f3"))
        expect_equal(
            p[["pipeline"]][["depends"]],
            list(
                character(0),
                c(b = "first"),
                c(a = "first", b = "f2")
            )
        )
    })

    it("keeps DAG and step mapping consistent",
    {
        p <- test_pip()
        pip_rename_step(p, from = "f1", to = "first")

        expect_true(is.na(.pip_steps_to_nodes(p, "f1")[[1]]))
        expect_true(!is.na(.pip_steps_to_nodes(p, "first")[[1]]))

        nodes <- .pip_get_downstream_nodes(p, "first")
        steps <- .pip_filter_nodes(p, nodes)[["step"]]
        expect_setequal(steps, c("first", "f2", "f3"))
    })
})


describe("pip_clone",
{
    test_pip <- function() {
        pip_new("p1") |>
            pip_add("s1", \(x = 1, .self = NULL) .self[["name"]]) |>
            pip_add("s2", \(x = ~s1) x)
    }

    it("signals invalid inputs",
    {
        expect_error(pip_clone(1), "x must be a pipeflow pip")
        expect_error(
            pip_clone(pip_new(), name = NA_character_),
            "name must be a single non-NA string"
        )
        expect_error(
            pip_clone(pip_new(), name = c("a", "b")),
            "name must be a single non-NA string"
        )
    })

    it("returns a pipeflow pipeline copy with same content",
    {
        p <- test_pip()
        p2 <- pip_clone(p)

        expect_true(.is_pipeflow_pip(p2))
        expect_false(identical(p2, p))
        expect_equal(p2[["name"]], p[["name"]])
        expect_equal(p2[["pipeline"]][["step"]], p[["pipeline"]][["step"]])
        expect_equal(
            p2[["pipeline"]][["depends"]],
            p[["pipeline"]][["depends"]]
        )
    })

    it("supports overriding the clone name",
    {
        p <- test_pip()
        p2 <- pip_clone(p, name = "copied")
        expect_equal(p2[["name"]], "copied")
        expect_equal(p[["name"]], "p1")
    })

    it("creates an independent copy",
    {
        p <- test_pip()
        p2 <- pip_clone(p)

        p2[["pipeline"]][["state"]][1] <- "done"
        expect_equal(p[["pipeline"]][["state"]][1], "new")

        pip_add(p2, "s3", \(x = ~s2) x)
        expect_false(pip_has_step(p, "s3"))
        expect_true(pip_has_step(p2, "s3"))
    })

    it("rebinds .self params to the cloned pipeline",
    {
        p <- test_pip()
        p2 <- pip_clone(p)

        expect_identical(p2[["pipeline"]][["params"]][[1]]$.self, p2)
        expect_identical(p[["pipeline"]][["params"]][[1]]$.self, p)

        pip_run(p2, lgr = NULL)
        expect_equal(p2[["pipeline"]][["out"]][[1]], "p1")
    })
})


describe("pip_collect_out",
{
    it("returns empty list for empty pipeline",
    {
        p <- pip_new()
        expect_equal(pip_collect_out(p), list())
    })

    it("returns named flat list when grouped is FALSE",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, group = "data")
        pip_add(p, "s2", \(x = ~-1) x + 1, group = "model")

        p[["pipeline"]][["out"]] <- list(10, 20)

        out <- pip_collect_out(p, grouped = FALSE)
        expect_equal(names(out), c("s1", "s2"))
        expect_equal(unname(out), list(10, 20))
    })

    it("groups outputs if at least two steps share a group label",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, group = "data")
        pip_add(p, "s2", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "s3", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "s4", \(x = ~-1) x + 1, group = "final")

        p[["pipeline"]][["out"]] <- list("o1", "o2", "o3", "o4")

        out <- pip_collect_out(p, grouped = TRUE)

        expect_equal(names(out), c("data", "model", "final"))
        expect_equal(out[["data"]], "o1")
        expect_equal(out[["model"]], list(s2 = "o2", s3 = "o3"))
        expect_equal(out[["final"]], "o4")
    })

    it("works on pipeline views",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, group = "data")
        pip_add(p, "s2", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "s3", \(x = ~-1) x + 1, group = "model")

        p[["pipeline"]][["out"]] <- list("o1", "o2", "o3")

        v <- pip_view(p, filter = list(group = "model"))
        out <- pip_collect_out(v)

        expect_equal(names(out), "model")
        expect_equal(out[["model"]], list(s2 = "o2", s3 = "o3"))
    })

    it("signals invalid arguments",
    {
        p <- pip_new()
        expect_error(pip_collect_out(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_collect_out(p, grouped = c(TRUE, FALSE)),
            "grouped must be a single logical value"
        )
    })
})


describe("pip_get_params",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(data = data.frame(a = 1:2)) data) |>
            pip_add("s2", \(data = ~s1, x = 1) data[, 2] + x) |>
            pip_add("s3", \(y = ~s2) y) |>
            pip_add("s4", \(x = 9, y = ~s2) x + y)
    }

    it("returns independent params from a pipeline",
    {
        p <- test_pip()
        params <- pip_get_params(p)

        expect_named(params, c("data", "x"))
        expect_equal(params[["data"]], data.frame(a = 1:2))
        expect_equal(params[["x"]], 1)
    })

    it("returns params from a view subset only",
    {
        p <- pip_new()
        data <- data.frame(a = 1:2, b = 3:4)

        pip_add(p, "s1", \(data = data) data)
        pip_add(p, "s2", \(data = ~s1, x = 1) data[, 2] + x)
        pip_add(p, "s3", \(y = ~s2) y)
        pip_add(p, "s4", \(x = 9, y = ~s2) x + y)

        v <- pip_view(p, filter = list(step = c("s3", "s4")))
        params <- pip_get_params(v)

        expect_named(params, "x")
        expect_equal(params[["x"]], 9)
    })

    it("returns empty list for view without independent params",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(y = ~s1) y)

        v <- pip_view(p, filter = list(step = "s2"))
        expect_equal(pip_get_params(v), list())
    })

    it("signals invalid input",
    {
        expect_error(
            pip_get_params(1),
            "x must be a pipeflow pip or view"
        )
    })
})


describe("pip_has_step",
{
    it("can be checked if pipeline has a step",
    {
        p <- pip_new()
        expect_false(pip_has_step(p, "s1"))
        pip_add(p, "s1", \(a = 1) a)
        expect_true(pip_has_step(p, "s1"))
    })

    it("errors at bad step argument",
    {
        f <- pip_has_step
        expect_error(f(p, list("not a character string")))
        expect_error(f(p, c("not", "a", "single", "string")))
        expect_error(f(p, NA))
        expect_error(f(p, ""))
        expect_error(f(p, 1))
    })
})


describe("pip_run",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("load_raw", \(x = 1) x, group = "io") |>
            pip_add("fit_model", \(x = ~-1) x + 1, group = "model") |>
            pip_add("eval_model", \(x = ~fit_model) x, group = "model") |>
            pip_add("bla_bla", \(bla = "blabla") bla, group = "bla")
    }

    describe("standard runs",
    {
        it("runs all steps of the pipeline and marks them as done",
        {
            p <- test_pip()
            pip_run(p, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, 2, "blabla"))
            expect_equal(p$pipeline[["state"]], rep("done", 4))
        })

        it("marks downstream steps not reached due to abort as outdated",
        {
            p <- pip_new() |>
                pip_add("load_raw", \(x = 1) stop("io error"), group = "io") |>
                pip_add("fit_model", \(x = ~-1) x + 1, group = "model") |>
                pip_add("eval_model", \(x = ~fit_model) x, group = "model")

            expect_error(pip_run(p, lgr = NULL), "io error")
            expect_equal(p$pipeline[["state"]], c("failed", "outdated", "outdated"))
        })
    })

    describe("running views",
    {
        it("can run parts of the pipeline via views",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "bla"))
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(NULL, NULL, NULL, "blabla"))
        })

        it("runs all steps of the view plus upstream dependencies",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "model"))
            pip_run(v, lgr = NULL)
            expect_equal(p$pipeline[["out"]], list(1, 2, 2, NULL))
        })

        it("marks downstream steps outside the view as outdated",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "io"))
            pip_run(v, lgr = NULL)
            expect_equal(
                p$pipeline[["state"]],
                c("done", "outdated", "outdated", "new")
            )
        })
    })
})


describe("pip_set_params",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1, data = data.frame(a = 1:2)) x) |>
            pip_add("s2", \(x = ~s1, y = 2) x + y) |>
            pip_add("s3", \(z = 3) z) |>
            pip_add("s4", \(a = ~s1, b = ~s3) a + b)
    }

    it("can be used with pip or view",
    {
        p <- test_pip()
        params <- list(x = 11, y = 22)

        res <- pip_set_params(p, params = params)
        expect_true(inherits(res, "pipeflow_pip"))

        v <- pip_view(p, filter = list(step = "s2"))
        res <- pip_set_params(v, params = params)
        expect_true(inherits(res, "pipeflow_view"))
    })

    it("sets independent parameters in a pipeline",
    {
        p <- test_pip()
        params <- list(x = 11, z = 33, data = data.frame(b = 3:4))

        pip_set_params(p, params = params)
        after <- p[["pipeline"]][["params"]]
        expect_equal(after[[1]][["x"]], 11)
        expect_equal(after[[1]][["data"]], data.frame(b = 3:4))
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("sets parameters only within the selected view",
    {
        p <- test_pip()

        v <- pip_view(p, filter = list(step = "s3"))
        pip_set_params(v, params = list(z = 33, x = 11, y = 22))
        after <- p[["pipeline"]][["params"]]

        expect_equal(after[[1]][["x"]], 1)
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["z"]], 33)
    })

    it("ignores locked steps",
    {
        p <- test_pip()
        p$pipeline[["locked"]][[1]] <- TRUE
        pip_set_params(p, params = list(x = 99))
        after <- p[["pipeline"]][["params"]]
        expect_equal(after[[1]][["x"]], 1)
    })

    it("warns for unused parameters when requested",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_warning(
            pip_set_params(p, params = list(foo = 1), warnUnused = TRUE),
            "Trying to set parameters not defined in the target: foo"
        )
    })

    it("marks changed and dependent downstream steps as 'outdated'",
    {
        p <- test_pip()
        pip_set_params(p, params = list(x = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("outdated", "outdated", "new", "outdated")
        )

        p <- test_pip()
        pip_set_params(p, params = list(y = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("new", "outdated", "new", "new")
        )

        p <- test_pip()
        pip_set_params(p, params = list(z = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("new", "new", "outdated", "outdated")
        )
    })
})


describe("pip_view",
{
    it("returns a view object with the expected structure",
    {
        p <- pip_new("test_pipeline")
        pip_add(p, "load", \(x = 1) x, group = "io")

        v <- pip_view(p)
        expect_true(.is_pipeflow_view(v))
        expect_true("rows" %in% names(v))
        expect_identical(v[["pip"]], p)
        expect_identical(v[["name"]], "test_pipeline view")
    })

    it("can filter by character columns with fixed matching",
    {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, group = "io")
        pip_add(p, "fit", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "eval", \(x = ~fit) x, group = "model")

        p[["pipeline"]][2, state := "done"]

        v <- pip_view(p,
            filter = list(group = "model", state = "done")
        )

        expect_equal(v[["rows"]], 2L)
    })

    it("can filter by tags",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~-1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~-1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["rows"]], c(1L, 3L))
    })

    it("can filter by regex when fixed is FALSE",
    {
        p <- pip_new()
        pip_add(p, "load_raw", \(x = 1) x, group = "io")
        pip_add(p, "fit_model", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "eval_model", \(x = ~fit_model) x, group = "model")

        v <- pip_view(
            p,
            filter = list(step = "_model$"),
            fixed = FALSE
        )

        expect_equal(v[["rows"]], c(2L, 3L))
    })

    it("intersects filtered rows with explicit i",
    {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, group = "g1")
        pip_add(p, "a2", \(x = ~-1) x, group = "g2")
        pip_add(p, "a3", \(x = ~-1) x, group = "g2")

        v <- pip_view(
            p,
            i = c(1L, 2L),
            filter = list(group = "g2")
        )

        expect_equal(v[["rows"]], 2L)
    })

    it("can be called on views to further subset rows",
    {
        p <- pip_new("test_pipeline")
        pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
        pip_add(p, "s2", \(x = ~-1) x + 1, tags = "model")
        pip_add(p, "s3", \(x = ~-1) x, tags = c("daily", "report"))

        v <- pip_view(p, tags = "daily")
        expect_equal(v[["rows"]], c(1L, 3L))
        expect_equal(v[["name"]], "test_pipeline view")

        v2 <- pip_view(v, tags = "report")
        expect_equal(v2[["rows"]], 3L)
        expect_equal(v2[["name"]], "test_pipeline view view")
    })

    it("signals invalid filter names",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, filter = list(not_a_column = "x")),
            "Invalid filter name"
        )
    })

    it("signals invalid row indices",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, i = c(0L, 1L)),
            "Invalid row indices in 'i'"
        )
        expect_error(
            pip_view(p, i = c(2L)),
            "Invalid row indices in 'i'"
        )
    })
})


# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

describe("length",
{
    it("returns an integer value",
    {
        p <- pip_new()
        expect_true(is.integer(length(p)))
    })

    it("returns the expected value",
    {
        p <- pip_new()
        expect_equal(length(p), 0L)
        pip_add(p, "s1", \(a = 1) a)
        pip_add(p, "s2", \(a = 1) a)
        expect_equal(length(p), 2L)

        v <- pip_view(p, 1)
        expect_equal(length(v), 1L)
    })
})

describe("extract operator [",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("returns a pipeline subset including upstream dependencies by row",
    {
        p <- test_pip()
        sub <- p[5L]

        expect_true(.is_pipeflow_pip(sub))
        expect_equal(sub[["pipeline"]][["step"]], c("b1", "b2"))
    })

    it("returns a pipeline subset including upstream dependencies by step",
    {
        p <- test_pip()
        sub <- p[c("a2", "b2")]

        expect_equal(sub[["pipeline"]][["step"]], c("a1", "a2", "b1", "b2"))
    })

    it("returns the full pipeline when i is missing",
    {
        p <- test_pip()
        sub <- p[]

        expect_equal(sub[["pipeline"]][["step"]], p[["pipeline"]][["step"]])
    })

    it("returns an empty pipeline for empty selectors", {
        p <- test_pip()

        expect_equal(length(p[integer()]), 0L)
        expect_equal(length(p[character()]), 0L)
    })

    it("signals invalid row indices",
    {
        p <- test_pip()

        expect_error(p[c(0, 1)], "Invalid row indices in 'i'")
        expect_error(p[99], "Invalid row indices in 'i'")
        expect_error(p[c(1, NA)], "row indices in 'i' must not contain NA")
        expect_error(p[c(1.1, 2)], "must be whole numbers")
    })

    it("signals invalid step names",
    {
        p <- test_pip()

        expect_error(p[c("a1", "")], "must be non-empty strings")
        expect_error(p[c("a1", NA)], "must not contain NA")
        expect_error(p[c("a1", "unknown")], "Unknown step names in 'i'")
    })

    it("returns an independent copy",
    {
        p <- test_pip()
        sub <- p[c("a2")]

        sub[["pipeline"]][["state"]][1] <- "done"
        expect_equal(p[["pipeline"]][["state"]][1], "new")
    })

    it("copies DAG edges for the extracted subset",
    {
        p <- test_pip()
        sub <- p[c("a2", "b2")]

        nodes_a1 <- .pip_get_downstream_nodes(sub, "a1")
        steps_a1 <- .pip_filter_nodes(sub, nodes_a1)[["step"]]
        expect_setequal(steps_a1, c("a1", "a2"))

        nodes_b1 <- .pip_get_downstream_nodes(sub, "b1")
        steps_b1 <- .pip_filter_nodes(sub, nodes_b1)[["step"]]
        expect_setequal(steps_b1, c("b1", "b2"))
    })

    it("uses an independent DAG copy in the extracted subset",
    {
        p <- test_pip()
        sub <- p[c("a2", "b2")]

        from <- as.integer(.pip_steps_to_nodes(sub, "a2")[[1]])
        to <- as.integer(.pip_steps_to_nodes(sub, "b1")[[1]])
        dag_add_edges_to(sub[[".dag"]], from = from, to = to)

        sub_steps <- .pip_filter_nodes(
            sub,
            .pip_get_downstream_nodes(sub, "a1")
        )[["step"]]
        expect_setequal(sub_steps, c("a1", "a2", "b1", "b2"))

        original_steps <- .pip_filter_nodes(
            p,
            .pip_get_downstream_nodes(p, "a1")
        )[["step"]]
        expect_setequal(original_steps, c("a1", "a2", "a3"))
    })
})


describe("benchmarking",
{
    skip("benchmarking tests are skipped by default")
    v <- c("hello", "world")
    w <- c("hello", "this", "is", "my", "world")
    grepv(pattern = v, x = w)
    `%chin%` <- data.table::`%chin%`

    N = 1e3
    u = as.character(as.hexmode(1:10000))
    y = sample(u,N,replace=TRUE)
    x = sample(u, 100)
    system.time(x %in% y)
    system.time(x %chin% y)

    microbenchmark(
        y %in% x,
        y %chin% x,
        times = 1000
    )


    # Please type 'example(chmatch)' to run this and see timings on your machine

    microbenchmark(x %in% y, x %chin% y, times = 100)
    system.time(a <- x %in% y)               #  4.5s
    system.time(b <- x %chin% y)             #  1.7s
    identical(a,b)

    # Different example with more unique strings ...
    u = as.character(as.hexmode(1:(N/10)))
    y = sample(u,N,replace=TRUE)
    x = sample(u,N,replace=TRUE)
    system.time(a <- match(x,y))               # 46s
    system.time(b <- chmatch(x,y))             # 16s
    identical(a,b)
})

