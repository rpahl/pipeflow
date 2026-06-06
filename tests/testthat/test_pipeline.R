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


describe(".pip_steps_to_rows",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x) |>
            pip_add("s3", \(x = ~s2) x)
    }

    it("maps step names to row positions for pipelines and views",
    {
        p <- test_pip()
        expect_equal(.pip_steps_to_rows(p, c("s3", "s1")), c(3L, 1L))

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        expect_equal(.pip_steps_to_rows(v, c("s1", "s3")), c(1L, 3L))
    })

    it("signals invalid step selectors with default messages",
    {
        p <- test_pip()

        expect_error(
            .pip_steps_to_rows(p, c("s1", "")),
            "step names must be non-empty strings"
        )
        expect_error(
            .pip_steps_to_rows(p, c("s1", NA_character_)),
            "step names must not contain NA"
        )
        expect_error(
            .pip_steps_to_rows(p, c("s1", "unknown")),
            "Unknown step names: unknown"
        )
    })

    it("uses simple error messages consistently",
    {
        p <- test_pip()

        expect_error(
            .pip_steps_to_rows(p, c("s1", "")),
            "step names must be non-empty strings"
        )
        expect_error(
            .pip_steps_to_rows(p, c("s1", "unknown")),
            "Unknown step names: unknown"
        )
    })
})


# ---------------------------
# Exported pipeline functions
# ---------------------------

describe("pip_new",
{
    it("creates a pipeflow pipeline with expected base structure",
    {
        p <- pip_new()

        expect_true(.is_pipeflow_pip(p))
        expect_true(is.environment(p))
        expect_equal(p[["name"]], "pipe")
        expect_true(data.table::is.data.table(p[["pipeline"]]))
        expect_equal(nrow(p[["pipeline"]]), 0L)
        expect_true(is.environment(p[[".steps_to_nodes"]]))
        expect_equal(ls(envir = p[[".steps_to_nodes"]]), character(0))
        expect_equal(length(dag_get_nodes_order(p[[".dag"]])), 0L)
    })

    it("supports custom pipeline names",
    {
        p <- pip_new("custom")
        expect_equal(p[["name"]], "custom")
    })

    it("signals invalid names",
    {
        expect_error(pip_new(c("a", "b")), "name must be a single string")
        expect_error(pip_new(1), "name must be a single string")
        expect_error(pip_new(NA_character_), "name must not be NA")
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

    it("allows functions with wildcard arguments",
    {
        p <- pip_new() |>
            pip_add("s1", \(x = 1, ...) x)

        pip_set_params(p, list(x = 2))
        pip_run(p, lgr = NULL)
        expect_equal(p$pipeline[["out"]][[1]], 2)
    })

    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = ~f1) x + 1)
    }

    it("can insert after a step name",
    {
        p <- test_pip()
        pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f3", "f2"))
        expect_equal(p[["pipeline"]][["depends"]][[2]], c(x = "f1"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f1"))
    })

    it("can insert by numeric index and supports insertion at beginning",
    {
        p <- test_pip()
        pip_add(p, "f0", \(x = 0) x, after = 0)

        expect_equal(p[["pipeline"]][["step"]], c("f0", "f1", "f2"))
        expect_equal(p[["pipeline"]][["depends"]][[2]], character(0))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f1"))
    })

    it("uses default insertion position at end",
    {
        p <- test_pip()
        pip_add(p, "f3", \(x = ~f2) x + 1)

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f2"))
    })

    it("returns invisibly also for default append path",
    {
        p <- test_pip()
        expect_invisible(
            pip_add(p, "f3", \(x = ~f2) x + 1)
        )
    })

    it("can append a step after pipeline was run",
    {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        expect_invisible(
            pip_add(p, "f3", \(x = ~f2) x + 1)
        )

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f2"))
    })

    it("can insert a step after pipeline was run",
    {
        p <- test_pip()
        pip_run(p, lgr = NULL)

        expect_invisible(
            pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")
        )

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f3", "f2"))
        expect_equal(p[["pipeline"]][["depends"]][[2]], c(x = "f1"))
        expect_equal(p[["pipeline"]][["depends"]][[3]], c(x = "f1"))
    })

    it("keeps existing step state and output for appended tail steps",
    {
        p <- test_pip()
        p[["pipeline"]][["out"]][[2]] <- 42
        p[["pipeline"]][["state"]][[2]] <- "done"

        pip_add(p, "f3", \(x = ~f1) x + 10, after = "f1")

        i <- match("f2", p[["pipeline"]][["step"]])
        expect_equal(p[["pipeline"]][["out"]][[i]], 42)
        expect_equal(p[["pipeline"]][["state"]][[i]], "done")
    })

    it("signals invalid insertion position and unknown step reference",
    {
        p <- test_pip()
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = "unknown"),
            "step 'unknown' does not exist"
        )
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = 3.1),
            "after index must be a whole number"
        )
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = -1),
            "after index must be between 0 and 2"
        )
        expect_error(
            pip_add(p, "f3", \(x = 1) x, after = 3),
            "after index must be between 0 and 2"
        )
        expect_error(
            pip_add(p, "f3", \(x = ~f2) x, after = "f1"),
            "cannot reference unknown steps: 'f2'"
        )
    })

    it("signals duplicate step names also when inserting at position",
    {
        p <- test_pip()

        expect_error(
            pip_add(p, "f2", \(x = 1) x, after = "f1"),
            "step 'f2' already exists in the pipeline"
        )
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
        expect_error(pip_add_from(trg, src, c("a", "b")))
        expect_error(pip_add_from(trg, src, NA_character_))
        expect_error(pip_add_from(trg, src, ""))
        expect_error(
            pip_add_from(trg, src, "unknown"),
            "does not exist in source pipeline"
        )
    })

    it("adds an independent step preserving group and tags",
    {
        src <- test_source()
        trg <- pip_new("target")

        res <- pip_add_from(trg, src, "base")
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

        pip_add_from(trg, src, "calc")
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
            pip_add_from(trg, src, "calc"),
            "cannot reference unknown steps: 'base'"
        )
    })

    it("rebinds .self to target pipeline through pip_add",
    {
        src <- pip_new("src") |>
            pip_add("self", \(x = 1, .self = NULL) .self[["name"]])
        trg <- pip_new("target")

        pip_add_from(trg, src, "self")
        expect_identical(trg[["pipeline"]][["params"]][[1]]$.self, trg)
    })
})


describe("pip_rename",
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
        expect_error(pip_rename(1, "f1", "first"))
        expect_error(pip_rename(p, c("f1", "f2"), "first"))
        expect_error(pip_rename(p, NA_character_, "first"))
        expect_error(pip_rename(p, "", "first"))
        expect_error(pip_rename(p, "f1", c("a", "b")))
        expect_error(pip_rename(p, "f1", NA_character_))
        expect_error(pip_rename(p, "f1", ""))
    })

    it("signals missing old step and name clashes",
    {
        p <- test_pip()
        expect_error(
            pip_rename(p, "unknown", "first"),
            "step 'unknown' does not exist"
        )
        expect_error(
            pip_rename(p, "f1", "f2"),
            "step 'f2' already exists"
        )
    })

    it("renames step and updates dependencies",
    {
        p <- test_pip()
        pip_rename(p, from = "f1", to = "first")

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
        pip_rename(p, from = "f1", to = "first")

        expect_true(is.na(.pip_steps_to_nodes(p, "f1")[[1]]))
        expect_true(!is.na(.pip_steps_to_nodes(p, "first")[[1]]))

        nodes <- .pip_get_downstream_nodes(p, "first")
        steps <- .pip_filter_nodes(p, nodes)[["step"]]
        expect_setequal(steps, c("first", "f2", "f3"))
    })
})


describe("pip_remove",
{
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = ~f1) x) |>
            pip_add("f3", \(x = ~f2) x) |>
            pip_add("f4", \(x = ~f1) x) |>
            pip_add("g1", \(x = 9) x)
    }

    it("signals invalid inputs",
    {
        p <- test_pip()
        expect_error(pip_remove(1, "f1"), "x must be a pipeflow pip")
        expect_error(
            pip_remove(p, c("f1", "f2")),
            "step must be a single string"
        )
        expect_error(pip_remove(p, NA_character_), "step must not be NA")
        expect_error(pip_remove(p, "unknown"), "step 'unknown' does not exist")
        expect_error(
            pip_remove(p, "f1", recursive = NA),
            "recursive must be a single logical value"
        )
        expect_error(
            pip_remove(p, "f1", recursive = c(TRUE, FALSE)),
            "recursive must be a single logical value"
        )
    })

    it("removes a leaf step", {
        p <- test_pip()
        node <- as.integer(.pip_steps_to_nodes(p, "g1")[[1]])
        beforeOrder <- dag_get_nodes_order(p[[".dag"]])
        beforeReach <- .pip_filter_nodes(
            p,
            .pip_get_downstream_nodes(p, "f1")
        )[["step"]]

        expect_true(dag_has_node(p[[".dag"]], node))
        expect_true(node %in% beforeOrder)
        expect_setequal(beforeReach, c("f1", "f2", "f3", "f4"))

        pip_remove(p, "g1")

        afterOrder <- dag_get_nodes_order(p[[".dag"]])
        afterReach <- .pip_filter_nodes(
            p,
            .pip_get_downstream_nodes(p, "f1")
        )[["step"]]

        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3", "f4"))
        expect_true(is.na(.pip_steps_to_nodes(p, "g1")[[1]]))
        expect_false(dag_has_node(p[[".dag"]], node))
        expect_false(node %in% afterOrder)
        expect_equal(length(afterOrder), length(beforeOrder) - 1L)
        expect_setequal(afterReach, c("f1", "f2", "f3", "f4"))
    })

    it("errors when direct downstream dependencies exist", {
        p <- test_pip()
        expect_error(
            pip_remove(p, "f1"),
            paste(
                "cannot remove step 'f1' because the following",
                "steps depend on it: 'f2', 'f4'"
            )
        )
    })

    it("removes step and all downstream dependencies recursively", {
        p <- test_pip()
        steps <- c("f1", "f2", "f3", "f4", "g1")
        nodeMap <- vapply(
            steps,
            FUN = \(s) as.integer(.pip_steps_to_nodes(p, s)[[1]]),
            FUN.VALUE = integer(1)
        )
        beforeOrder <- dag_get_nodes_order(p[[".dag"]])

        expect_true(all(vapply(
            nodeMap,
            FUN = \(nid) dag_has_node(p[[".dag"]], nid),
            FUN.VALUE = logical(1)
        )))

        out <- utils::capture.output(
            pip_remove(p, "f1", recursive = TRUE),
            type = "message"
        )

        afterOrder <- dag_get_nodes_order(p[[".dag"]])
        remainingNode <- as.integer(nodeMap[["g1"]])

        expect_equal(p[["pipeline"]][["step"]], "g1")
        expect_equal(p[["pipeline"]][[".nodeId"]], remainingNode)
        expect_equal(afterOrder, remainingNode)
        expect_equal(length(afterOrder), length(beforeOrder) - 4L)
        expect_true(dag_has_node(p[[".dag"]], remainingNode))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f1"]])))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f2"]])))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f3"]])))
        expect_false(dag_has_node(p[[".dag"]], as.integer(nodeMap[["f4"]])))
        expect_equal(
            .pip_filter_nodes(p, .pip_get_downstream_nodes(p, "g1"))[["step"]],
            "g1"
        )
        expect_equal(
            out,
            paste(
                "Removing step 'f1' and its downstream dependencies:",
                "'f2', 'f3', 'f4'"
            )
        )
    })
})


describe("pip_replace",
{
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("f1", \(x = 1) x) |>
            pip_add("f2", \(x = 2) x) |>
            pip_add("f3", \(x = ~f2) x + 1)
    }

    it("signals invalid inputs",
    {
        p <- test_pip()

        expect_error(
            pip_replace(1, "f1", \(x = 1) x),
            "x must be a pipeflow pip"
        )
        expect_error(
            pip_replace(p, c("f1", "f2"), \(x = 1) x),
            "step must be a single string"
        )
        expect_error(
            pip_replace(p, NA_character_, \(x = 1) x),
            "step must not be NA"
        )
        expect_error(
            pip_replace(p, "", \(x = 1) x),
            "step must be a non-empty string"
        )
        expect_error(
            pip_replace(p, "unknown", \(x = 1) x),
            "step 'unknown' does not exist"
        )
        expect_error(pip_replace(p, "f1", 1), "fun must be a function")
        expect_error(
            pip_replace(p, "f1", \(x = 1) x, group = ""),
            "group must be a non-empty valid string"
        )
    })

    it("replaces a step in-place while keeping the original order",
    {
        p <- test_pip()
        pip_run(p, lgr = NULL)
        expect_equal(p[["pipeline"]][step == "f3", out][[1]], 3)

        pip_replace(p, "f2", \(x = 4) x * 2)
        expect_equal(p[["pipeline"]][["step"]], c("f1", "f2", "f3"))

        pip_run(p, lgr = NULL)
        expect_equal(p[["pipeline"]][step == "f2", out][[1]], 8)
        expect_equal(p[["pipeline"]][step == "f3", out][[1]], 9)
    })

    it("verifies replacement dependencies against earlier steps only",
    {
        p <- test_pip()

        expect_error(
            pip_replace(p, "f2", \(x = ~f3) x),
            "cannot reference unknown steps: 'f3'"
        )
        expect_error(
            pip_replace(p, "f2", \(x = ~foo) x),
            "cannot reference unknown steps: 'foo'"
        )
    })

    it("marks only downstream dependent steps as outdated",
    {
        p <- pip_new("pipe") |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x + 1) |>
            pip_add("b1", \(x = 10) x) |>
            pip_add("a3", \(x = ~a2) x + 1)

        pip_run(p, lgr = NULL)
        pip_replace(p, "a2", \(x = ~a1) x + 2)

        expect_equal(
            p[["pipeline"]][["state"]],
            c("done", "new", "done", "outdated")
        )
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


describe("pip_get_graph",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1) x, group = "io", exec = "split") |>
            pip_add("s2", \(x = ~s1) x + 1, group = "model", exec = "reduce") |>
            pip_add("s3", \(x = ~s2) x + 2, group = "model")
    }

    map_edge_labels <- function(graph) {
        idToLabel <- stats::setNames(
            graph[["nodes"]][["label"]],
            as.character(graph[["nodes"]][["id"]])
        )
        paste0(
            idToLabel[as.character(graph[["edges"]][["from"]])],
            "->",
            idToLabel[as.character(graph[["edges"]][["to"]])]
        )
    }

    it("returns visNetwork-compatible nodes and edges for pipelines",
    {
        p <- test_pip()
        p[["pipeline"]][["state"]] <- c("new", "done", "failed")

        g <- pip_get_graph(p)
        nodes <- g[["nodes"]]
        edges <- g[["edges"]]

        expect_named(g, c("nodes", "edges"))
        expect_s3_class(nodes, "data.frame")
        expect_s3_class(edges, "data.frame")
        expect_equal(names(nodes), c("id", "label", "group", "shape", "color"))
        expect_equal(names(edges), c("from", "to", "arrows"))
        nodeShape <- stats::setNames(nodes[["shape"]], nodes[["label"]])
        expect_equal(nodeShape[["s1"]], "star")
        expect_equal(nodeShape[["s2"]], "dot")
        expect_equal(nodeShape[["s3"]], "hexagon")
        expect_true(all(edges[["arrows"]] == "to"))

        expectedColors <- vapply(
            p[["pipeline"]][["state"]],
            FUN = \(st) .step_states[[st]][["color"]],
            FUN.VALUE = character(1)
        )
        expect_equal(nodes[["color"]], unname(expectedColors))
        expect_setequal(map_edge_labels(g), c("s1->s2", "s2->s3"))
    })

    it("supports view-only graph or graph with upstream closure",
    {
        p <- test_pip()
        v <- pip_view(p, i = "s3")

        gView <- pip_get_graph(v, include_upstream = FALSE)
        expect_equal(gView[["nodes"]][["label"]], "s3")
        expect_equal(nrow(gView[["edges"]]), 0L)

        gUp <- pip_get_graph(v, include_upstream = TRUE)
        expect_setequal(gUp[["nodes"]][["label"]], c("s1", "s2", "s3"))
        expect_setequal(map_edge_labels(gUp), c("s1->s2", "s2->s3"))
    })

    it("signals invalid inputs",
    {
        p <- test_pip()

        expect_error(pip_get_graph(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_get_graph(p, include_upstream = c(TRUE, FALSE)),
            "include_upstream must be a single logical value"
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
            expect_equal(
                p$pipeline[["state"]],
                c("failed", "outdated", "outdated")
            )
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

        it("adds [view]/[upstream] markers to view-run logs",
        {
            p <- test_pip()
            v <- pip_view(p, filter = list(group = "model"))

            logs <- character(0)
            lgr <- function(level, msg) {
                logs <<- c(logs, paste(level, msg))
            }

            pip_run(v, lgr = lgr)

            expect_true(any(grepl("\\[upstream\\] load_raw", logs)))
            expect_true(any(grepl("\\[view\\] fit_model", logs)))
            expect_true(any(grepl("\\[view\\] eval_model", logs)))
        })
    })

    describe("partition-aware execution",
    {
        test_pip_partitioned <- function() {
            pip_new() |>
                pip_add(
                    "load",
                    \(x = data.frame(
                        grp = c("a", "a", "b", "b"),
                        value = c(1, 3, 10, 20)
                    )) x
                ) |>
                pip_add(
                    "split",
                    \(x = ~load) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "mean_by_grp",
                    \(x = ~split) mean(x$value)
                ) |>
                pip_add(
                    "plus_one",
                    \(x = ~mean_by_grp) x + 1
                ) |>
                pip_add(
                    "overall",
                    \(x = ~plus_one) mean(unlist(x)),
                    exec = "reduce"
                )
        }

        it("maps downstream calls over split output in auto mode",
        {
            p <- test_pip_partitioned()
            pip_run(p, lgr = NULL)

            splitOut <- p$pipeline$out[[2]]
            meanOut <- p$pipeline$out[[3]]
            plusOneOut <- p$pipeline$out[[4]]

            expect_true(inherits(splitOut, "pipeflow_partitioned"))
            expect_true(inherits(meanOut, "pipeflow_partitioned"))
            expect_true(inherits(plusOneOut, "pipeflow_partitioned"))

            expect_equal(meanOut[["a"]], 2)
            expect_equal(meanOut[["b"]], 15)
            expect_equal(plusOneOut[["a"]], 3)
            expect_equal(plusOneOut[["b"]], 16)
            expect_equal(p$pipeline$out[[5]], 9.5)
        })

        it("errors when reduce mode receives only non-partitioned inputs",
        {
            p <- pip_new() |>
                pip_add("load", \(x = 1) x) |>
                pip_add("sum", \(x = ~load) x + 1, exec = "reduce")

            expect_error(
                pip_run(p, lgr = NULL),
                "reduce mode requires at least one partitioned input"
            )
        })

        it("errors when plain mode receives partitioned input",
        {
            p <- pip_new() |>
                pip_add(
                    "load",
                    \(x = data.frame(
                        grp = c("a", "a", "b", "b"),
                        value = c(1, 3, 10, 20)
                    )) x
                ) |>
                pip_add(
                    "split",
                    \(x = ~load) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "strict",
                    \(x = ~split) mean(unlist(x)),
                    exec = "plain"
                )

            expect_error(
                pip_run(p, lgr = NULL),
                "plain mode does not accept partitioned inputs"
            )
        })

        it("maps with two partitioned and one scalar input",
        {
            p <- pip_new() |>
                pip_add(
                    "left_raw",
                    \(x = data.frame(
                        grp = c("a", "a", "b", "b"),
                        value = c(1, 3, 10, 20)
                    )) x
                ) |>
                pip_add(
                    "right_raw",
                    \(x = data.frame(
                        grp = c("a", "a", "b", "b"),
                        value = c(2, 4, 6, 8)
                    )) x
                ) |>
                pip_add(
                    "left_split",
                    \(x = ~left_raw) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "right_split",
                    \(x = ~right_raw) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add("offset", \(x = 100) x) |>
                pip_add(
                    "combine",
                    \(a = ~left_split, b = ~right_split, c = ~offset) {
                        mean(a$value) + mean(b$value) + c
                    }
                )

            pip_run(p, lgr = NULL)

            out <- p$pipeline$out[[6]]
            expect_true(inherits(out, "pipeflow_partitioned"))
            expect_equal(out[["a"]], 105)
            expect_equal(out[["b"]], 122)
        })

        it("includes failing partition key in mapped errors",
        {
            p <- pip_new() |>
                pip_add(
                    "load",
                    \(x = data.frame(
                        grp = c("a", "a", "b", "b"),
                        value = c(1, 2, 3, 4)
                    )) x
                ) |>
                pip_add(
                    "split",
                    \(x = ~load) split(x, f = x$grp),
                    exec = "split"
                ) |>
                pip_add(
                    "fragile",
                    \(x = ~split) {
                        key <- unique(x$grp)
                        if (identical(key, "b")) {
                            stop("boom")
                        }
                        sum(x$value)
                    }
                )

            expect_error(
                pip_run(p, lgr = NULL),
                "key 'b': boom"
            )
        })
    })

    describe("can be run with dynamic pipeline modifications",
    {
        it(paste(
            "supports modifying the pipeline at runtime and",
            "continues with the updated version"
        ), {
            pip <- pip_new("my-pipeline") |>
                pip_add("init", function(xInit = 0) xInit) |>
                pip_add("f1", function(x = ~init) x + 1) |>
                pip_add(
                    "f2",
                    function(x = ~f1, .self = NULL) {
                        if (x > 10) {
                            .self |> pip_replace("f3", function(x = ~f1) x * 3)
                            return(x / 2)
                        }
                        x + 2
                    }
                ) |>
                pip_add("f3", function(x = ~f2) x + 3)

            pip_run(pip, lgr = NULL)
            expect_equal(pip[["out"]], list(0, 1, 3, 6))

            pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
            expect_equal(pip[["out"]], list(15, 16, 16 / 2, 16 * 3))
            expect_equal(
                body(pip[["f3", "fun"]]),
                body(function(x = ~f1) x * 3)
            )
        })

        it(paste(
            "supports modifying the pipeline at runtime by",
            "a step that was replaced"
        ), {
            pip <- pip_new("my-pipeline") |>
                pip_add("init", function(xInit = 0) xInit) |>
                pip_add("f1", function(x = ~init) x + 1) |>
                pip_add("f2", function(x = ~f1) x + 2) |>
                pip_add("f3", function(x = ~f2) x + 3)

                pip |> pip_replace(
                    "f2",
                    function(x = ~f1, .self = NULL)
                    {
                        if (x > 10) {
                            .self |> pip_replace("f3", function(x = ~f1) x * 3)
                            return(x / 2)
                        }
                        x + 2
                    }
                )

            pip_run(pip, lgr = NULL)
            expect_equal(pip[["out"]], list(0, 1, 3, 6))

            pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
            expect_equal(pip[["out"]], list(15, 16, 16 / 2, 16 * 3))
            expect_equal(
                body(pip[["f3", "fun"]]),
                body(function(x = ~f1) x * 3)
            )
        })

        it(paste(
            "keeps .self rebound correctly for downstream steps",
            "that were copied during replacement"
        ), {
            pip <- pip_new("my-pipeline") |>
                pip_add("init", function(xInit = 0) xInit) |>
                pip_add("f1", function(x = ~init) x + 1) |>
                pip_add("f2", function(x = ~f1) x + 2) |>
                pip_add(
                    "f3",
                    function(x = ~f2, .self = NULL) {
                        if (x > 10) {
                            .self |>
                                pip_replace("f4", function(x = ~f1) x * 4)
                        }
                        x + 3
                    }
                ) |>
                pip_add("f4", function(x = ~f3) x + 4)

            pip |> pip_replace("f2", function(x = ~f1) x + 10)

            pip_run(pip, lgr = NULL)
            expect_equal(pip[["out"]], list(0, 1, 11, 14, 4))
            expect_equal(
                body(pip[["f4", "fun"]]),
                body(function(x = ~f1) x * 4)
            )
        })

        it(paste(
            "persists runtime self-modifications across runs",
            "without losing .self routing"
        ), {
            pip <- pip_new("my-pipeline") |>
                pip_add("init", function(xInit = 0) xInit) |>
                pip_add("f1", function(x = ~init) x + 1) |>
                pip_add(
                    "f2",
                    function(x = ~f1, .self = NULL) {
                        if (x > 10) {
                            .self |> pip_replace("f3", function(x = ~f1) x * 3)
                            return(x / 2)
                        }
                        x + 2
                    }
                ) |>
                pip_add("f3", function(x = ~f2) x + 3)

            pip |> pip_set_params(list(xInit = 15)) |> pip_run(lgr = NULL)
            expect_equal(pip[["out"]], list(15, 16, 8, 48))
            expect_equal(
                body(pip[["f3", "fun"]]),
                body(function(x = ~f1) x * 3)
            )

            pip |> pip_set_params(list(xInit = 20)) |> pip_run(lgr = NULL)
            expect_equal(pip[["out"]], list(20, 21, 10.5, 63))
            expect_equal(
                body(pip[["f3", "fun"]]),
                body(function(x = ~f1) x * 3)
            )
        })

        it(paste(
            "applies runtime replacements when running only a view",
            "and updates steps outside the view"
        ), {
            pip <- pip_new("my-pipeline") |>
                pip_add("init", function(xInit = 0) xInit) |>
                pip_add("f1", function(x = ~init) x + 1) |>
                pip_add(
                    "f2",
                    function(x = ~f1, .self = NULL) {
                        if (x > 10) {
                            .self |> pip_replace("f3", function(x = ~f1) x * 3)
                            return(x / 2)
                        }
                        x + 2
                    }
                ) |>
                pip_add("f3", function(x = ~f2) x + 3) |>
                pip_add("f4", function(x = ~f3) x + 1)

            pip_run(pip, lgr = NULL)
            expect_equal(pip[["out"]], list(0, 1, 3, 6, 7))

            pip |> pip_set_params(list(xInit = 15))
            v <- pip_view(pip, i = c("f1", "f2"))
            pip_run(v, lgr = NULL, force = TRUE)

            expect_equal(pip[["out"]], list(15, 16, 8, NULL, 7))
            expect_equal(
                body(pip[["f3", "fun"]]),
                body(function(x = ~f1) x * 3)
            )
            expect_equal(
                pip[["pipeline"]][step == "f4", state][[1]],
                "outdated"
            )

            pip_run(pip, lgr = NULL)
            expect_equal(pip[["out"]], list(15, 16, 8, 48, 49))
        })
    })
})


describe("pip_set_params",
{
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1, data = data.frame(a = 1:2)) x) |>
            pip_add("s2", \(x = ~s1, y = 2) x + y) |>
            pip_add("s3", \(x = 1, z = 3) x + z) |>
            pip_add("s4", \(a = ~s1, b = ~s3) a + b)
    }

    it("can be used with pip or view",
    {
        p <- test_pip()

        res <- pip_set_params(p, params = list(x = 11, y = 22))
        expect_true(inherits(res, "pipeflow_pip"))

        v <- pip_view(p, filter = list(step = "s2"))
        res <- pip_set_params(v, params = list(y = 22))
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
        expect_equal(after[[3]][["x"]], 11)
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

    it("warns for unused parameters",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1, ...) x)

        expect_warning(
            pip_set_params(p, params = list(foo = 1)),
            "Trying to set parameters not defined in the target: foo"
        )
    })

    it("sets parameters only within the selected view",
    {
        p <- test_pip()

        v <- pip_view(p, filter = list(step = "s3"))
        expect_warning(
            pip_set_params(v, params = list(z = 33, x = 11, y = 22)),
            "Trying to set parameters not defined in the target: y"
        )
        after <- p[["pipeline"]][["params"]]

        expect_equal(after[[1]][["x"]], 1)
        expect_equal(after[[2]][["y"]], 2)
        expect_equal(after[[3]][["x"]], 11)
        expect_equal(after[[3]][["z"]], 33)
    })


    it("marks changed and dependent downstream steps as 'outdated'",
    {
        p <- test_pip()
        pip_set_params(p, params = list(x = 5))
        expect_equal(
            p[["pipeline"]][["state"]],
            c("outdated", "outdated", "outdated", "outdated")
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


tag_lock_test_pip <- function() {
    pip_new("pipe") |>
        pip_add("s1", \(x = 1) x, tags = c("init", "daily")) |>
        pip_add("s2", \(x = ~s1) x + 1, tags = c("daily", "model")) |>
        pip_add("s3", \(x = ~s2) x + 1, tags = "report")
}


describe("pip_tag",
{
    it("signals invalid inputs",
    {
        p <- tag_lock_test_pip()
        expect_error(pip_tag(1), "x must be a pipeflow pip or view")
        expect_error(pip_tag(p, tags = 1), "tags must be a character vector")
    })

    it("adds tags to all selected steps while preserving existing tags",
    {
        p <- tag_lock_test_pip()
        pip_tag(p, tags = c("daily", "core"))

        expect_equal(p[["pipeline"]][["tags"]][[1]], c("init", "daily", "core"))
        expect_equal(
            p[["pipeline"]][["tags"]][[2]],
            c("daily", "model", "core")
        )
        expect_equal(
            p[["pipeline"]][["tags"]][[3]],
            c("report", "daily", "core")
        )
    })

    it("updates only rows in a view and skips locked steps",
    {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]][[2]] <- TRUE
        p[["pipeline"]][["tags"]][[2]] <- "keep"

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_tag(v, tags = "view")

        expect_equal(p[["pipeline"]][["tags"]][[1]], c("init", "daily"))
        expect_equal(p[["pipeline"]][["tags"]][[2]], "keep")
        expect_equal(p[["pipeline"]][["tags"]][[3]], c("report", "view"))
    })
})


describe("pip_untag",
{
    it("signals invalid inputs",
    {
        p <- tag_lock_test_pip()
        expect_error(pip_untag(1), "x must be a pipeflow pip or view")
        expect_error(pip_untag(p, tags = 1), "tags must be a character vector")
    })

    it("removes tags from all selected steps",
    {
        p <- tag_lock_test_pip()
        pip_untag(p, tags = c("daily", "report"))

        expect_equal(p[["pipeline"]][["tags"]][[1]], "init")
        expect_equal(p[["pipeline"]][["tags"]][[2]], "model")
        expect_equal(p[["pipeline"]][["tags"]][[3]], character(0))
    })

    it("updates only rows in a view and skips locked steps",
    {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]][[2]] <- TRUE
        p[["pipeline"]][["tags"]][[2]] <- c("daily", "model")

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_untag(v, tags = "daily")

        expect_equal(p[["pipeline"]][["tags"]][[1]], c("init", "daily"))
        expect_equal(p[["pipeline"]][["tags"]][[2]], c("daily", "model"))
        expect_equal(p[["pipeline"]][["tags"]][[3]], "report")
    })
})


describe("pip_lock",
{
    it("signals invalid input",
    {
        expect_error(pip_lock(1), "x must be a pipeflow pip or view")
    })

    it("locks selected steps",
    {
        p <- tag_lock_test_pip()
        pip_lock(p)
        expect_true(all(p[["pipeline"]][["locked"]]))
    })

    it("locks only rows covered by a view",
    {
        p <- tag_lock_test_pip()
        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_lock(v)

        expect_false(p[["pipeline"]][["locked"]][[1]])
        expect_true(p[["pipeline"]][["locked"]][[2]])
        expect_true(p[["pipeline"]][["locked"]][[3]])
    })
})


describe("pip_unlock",
{
    it("signals invalid input",
    {
        expect_error(pip_unlock(1), "x must be a pipeflow pip or view")
    })

    it("unlocks selected steps",
    {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]] <- rep(TRUE, nrow(p[["pipeline"]]))

        pip_unlock(p)
        expect_false(any(p[["pipeline"]][["locked"]]))
    })

    it("unlocks only rows covered by a view",
    {
        p <- tag_lock_test_pip()
        p[["pipeline"]][["locked"]] <- rep(TRUE, nrow(p[["pipeline"]]))

        v <- pip_view(p, filter = list(step = c("s2", "s3")))
        pip_unlock(v)

        expect_true(p[["pipeline"]][["locked"]][[1]])
        expect_false(p[["pipeline"]][["locked"]][[2]])
        expect_false(p[["pipeline"]][["locked"]][[3]])
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

    it("can filter by columns with fixed matching",
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

    it("can filter by depends column with multiple entries",
    {
        p <- pip_new()
        pip_add(p, "load", \(x = 1) x, group = "io")
        pip_add(p, "fit", \(x = ~-1) x + 1, group = "model")
        pip_add(p, "eval", \(x = ~load, y = ~fit) x, group = "model")

        v <- pip_view(
            p,
            filter = list(depends = "fit", group = "model")
        )
        v
        expect_equal(v[["rows"]], 3L)
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
        pip_add(p, "data", \(x = 1) x)
        pip_add(p, "fit_model", \(x = ~-1) x + 1)
        pip_add(p, "eval_model", \(x = ~data, y = ~fit_model) x)

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

    it("can select rows via step names in i",
    {
        p <- pip_new()
        pip_add(p, "a1", \(x = 1) x, group = "g1")
        pip_add(p, "a2", \(x = ~-1) x, group = "g2")
        pip_add(p, "a3", \(x = ~-1) x, group = "g2")

        v <- pip_view(p, i = c("a1", "a3"))
        expect_equal(v[["rows"]], c(1L, 3L))

        v <- pip_view(p,
            i = c("a1", "a2"),
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

    it("signals invalid step names in i",
    {
        p <- pip_new()
        pip_add(p, "s1", \(x = 1) x)

        expect_error(
            pip_view(p, i = c("s1", "")),
            "step names must be non-empty strings"
        )
        expect_error(
            pip_view(p, i = c("s1", NA_character_)),
            "step names must not contain NA"
        )
        expect_error(
            pip_view(p, i = "unknown"),
            "Unknown step names: unknown"
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
        expect_error(p[c("a1", "unknown")], "Unknown step names")
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


describe("extract operator [[",
{
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "init") |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("keeps environment-style extraction for internal bindings",
    {
        p <- test_pip()
        expect_equal(p[["name"]], "pipe")
        expect_true(data.table::is.data.table(p[["pipeline"]]))
        expect_false(is.null(p[[".dag"]]))
    })

    it("extracts full columns when j is missing",
    {
        p <- test_pip()
        expect_equal(p[["step"]], c("s1", "s2"))
        expect_equal(p[[1]], c("s1", "s2"))
        expect_null(p[["unknown"]])
    })

    it("extracts by row selector and column selector",
    {
        p <- test_pip()

        expect_equal(p[[2L, "step"]], "s2")
        expect_equal(p[["s1", "state"]], "new")
        expect_equal(p[[c(1L, 2L), "state"]], c("new", "new"))
        expect_equal(p[[c("s1", "s2"), "step"]], c("s1", "s2"))
    })

    it("extracts list-columns for single and multiple rows consistently",
    {
        p <- test_pip() |> pip_run(lgr = NULL)

        # Single-row extraction returns the row value from the list-column.
        expect_equal(p[[1L, "tags"]], "init")
        expect_equal(p[[2L, "tags"]], character(0))
        expect_equal(p[[1L, "params"]], list(x = 1))
        expect_named(p[[2L, "params"]], "x")
        expect_equal(p[[1L, "depends"]], character(0))
        expect_equal(p[[2L, "depends"]], c(x = "s1"))
        expect_equal(p[[1L, "out"]], 1)
        expect_equal(p[[2L, "out"]], 2)

        # Multi-row extraction returns a list of row values.
        tags <- p[[c(1L, 2L), "tags"]]
        expect_true(is.list(tags))
        expect_equal(tags[[1]], "init")
        expect_equal(tags[[2]], character(0))

        depends <- p[[c("s1", "s2"), "depends"]]
        expect_true(is.list(depends))
        expect_equal(depends[[1]], character(0))
        expect_equal(depends[[2]], c(x = "s1"))

        outs <- p[[c("s1", "s2"), "out"]]
        expect_true(is.list(outs))
        expect_equal(outs[[1]], 1)
        expect_equal(outs[[2]], 2)
    })

    it(paste(
        "can distinguish between steps called 'name' and 'pipeline'",
        "and the internal 'name' and 'pipeline' elements"),
    {
        p <- pip_new("pipe") |>
            pip_add("name", \(x = "hello") x) |>
            pip_add("pipeline", \(x = ~name, y = "world") paste(x, y))

        expect_equal(p[["name"]], "pipe")
        expect_equal(p[["pipeline"]], p$pipeline)

        expect_equal(p[["name", "params"]], p[[1, "params"]])
        expect_equal(p[["pipeline", "params"]], p[[2, "params"]])
    })

    it("signals invalid row and column selectors",
    {
        p <- test_pip()

        expect_equal(p[[c(1, NA), "step"]], c("s1", NA_character_))
        expect_error(
            p[[c("s1", ""), "step"]],
            "step names must be non-empty strings"
        )
        expect_error(
            p[[c("s1", "unknown"), "step"]],
            "Unknown step names"
        )
        expect_null(p[[1, "unknown"]])
        expect_error(
            p[[1, c("step", "state")]],
            "subscript out of bounds"
        )
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
    y = sample(u, N, replace = TRUE)
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
    identical(a, b)

    # Different example with more unique strings ...
    u = as.character(as.hexmode(1:(N/10)))
    y = sample(u, N, replace = TRUE)
    x = sample(u, N, replace = TRUE)
    system.time(a <- match(x, y))               # 46s
    system.time(b <- chmatch(x, y))             # 16s
    identical(a, b)
})


describe("print.pipeflow_pip",
{
    get_print_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    it("shows step/depends/out/state by default when group equals step",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        header <- get_print_header(p)

        expect_equal(header, c("step", "depends", "out", "state"))
    })

    it("shows group column by default when at least one group differs",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe")
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(x = ~s1) x + 1, group = "model")

        header <- get_print_header(p)

        expect_equal(header, c("step", "group", "depends", "out", "state"))
    })

    it("prints the tags column last when any step defines tags",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1)

        header <- get_print_header(p)

        expect_equal(header, c("step", "depends", "out", "state", "tags"))
    })

    it("prints the exec column last when any step defines non-auto exec",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, exec = "split")

        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "depends", "out", "state", "tags", "exec")
        )
    })
})


describe("print.pipeflow_view",
{
    get_view_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    it("prints a view header and uses the pipeline column layout",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, group = "io") |>
            pip_add("s2", \(x = ~s1) x + 1, group = "model")

        v <- pip_view(p, filter = list(group = "model"))
        out <- capture.output(print(v))

        expect_true(any(grepl("<pipeflow_view>", out)))
        expect_equal(
            get_view_header(v),
            c("step", "group", "depends", "out", "state")
        )
    })

    it("prints the tags column last for tagged views",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1)

        v <- pip_view(p, tags = "core")

        expect_equal(
            get_view_header(v),
            c("step", "depends", "out", "state", "tags")
        )
    })

    it("prints the exec column last when any step defines non-auto exec",
    {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, exec = "split", tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1)

        v <- pip_view(p, tags = "core")

        expect_equal(
            get_view_header(v),
            c("step", "depends", "out", "state", "tags", "exec")
        )
    })
})
