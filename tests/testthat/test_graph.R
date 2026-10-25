describe("pip_graph", {
    test_pip <- function() {
        pip_new() |>
            pip_add("s1", \(x = 1) x, tags = "io", exec = "split") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "model", exec = "reduce") |>
            pip_add("s3", \(x = ~s2) x + 2, tags = "model")
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

    it("returns visNetwork-compatible nodes and edges for pipelines", {
        p <- test_pip()
        env <- p[["pipenv"]]
        env[["data"]][["state"]] <- c("new", "done", "failed")

        g <- pip_graph(p)
        nodes <- g[["nodes"]]
        edges <- g[["edges"]]

        expect_named(g, c("nodes", "edges"))
        expect_s3_class(nodes, "data.frame")
        expect_s3_class(edges, "data.frame")
        expect_equal(
            names(nodes),
            c("id", "label", "shape", "color", "title")
        )
        expect_equal(names(edges), c("from", "to", "arrows", "dashes"))
        nodeShape <- stats::setNames(nodes[["shape"]], nodes[["label"]])
        expect_equal(nodeShape[["s1"]], "star")
        expect_equal(nodeShape[["s2"]], "dot")
        expect_equal(nodeShape[["s3"]], "hexagon")
        expect_true(all(edges[["arrows"]] == "to"))
        expect_false(any(edges[["dashes"]]))

        expectedColors <- vapply(
            p[["state"]],
            FUN = \(st) .step_states[[st]][["color"]],
            FUN.VALUE = character(1)
        )
        expect_equal(nodes[["color"]], unname(expectedColors))
        expect_setequal(map_edge_labels(g), c("s1->s2", "s2->s3"))
    })

    it("supports view-only graph or graph with upstream closure", {
        p <- test_pip()
        v <- pip_view(p, step = "s3")

        gView <- pip_graph(v, include_upstream = FALSE)
        expect_equal(gView[["nodes"]][["label"]], "s3")
        expect_equal(nrow(gView[["edges"]]), 0L)

        gUp <- pip_graph(v, include_upstream = TRUE)
        expect_setequal(gUp[["nodes"]][["label"]], c("s1", "s2", "s3"))
        expect_setequal(map_edge_labels(gUp), c("s1->s2", "s2->s3"))
    })

    it("signals invalid inputs", {
        p <- test_pip()

        expect_error(pip_graph(1), "x must be a pipeflow pip or view")
        expect_error(
            pip_graph(p, include_upstream = c(TRUE, FALSE)),
            "include_upstream must be a single logical value"
        )
    })

    it("returns empty nodes and edges for empty pipeline", {
        g <- pip_graph(pip_new())
        expect_equal(nrow(g[["nodes"]]), 0L)
        expect_equal(nrow(g[["edges"]]), 0L)
        expect_named(g, c("nodes", "edges"))
        expect_named(g[["nodes"]], c("id", "label", "shape", "color", "title"))
        expect_named(g[["edges"]], c("from", "to", "arrows", "dashes"))
    })

    it("dashes an edge if all references to the step are ~try()", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~ try(a)) x) |>
            pip_add("c", \(x = ~a, y = ~ try(a)) x) |>
            pip_add("d", \(x = ~ try(a), y = ~ try(-1)) x)

        edges <- pip_graph(p)[["edges"]]

        dashes <- stats::setNames(
            edges[["dashes"]],
            map_edge_labels(
                pip_graph(p)
            )
        )
        expect_equal(
            dashes,
            c("a->b" = TRUE, "a->c" = FALSE, "a->d" = TRUE, "c->d" = TRUE)
        )
    })

    it("has a hover title with the state, exec mode and dependencies", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x) |>
            pip_add("b", \(x = ~a, y = ~ try(a), n = 3) x, exec = "reduce")

        title <- pip_graph(p)[["nodes"]][["title"]]

        expect_equal(
            title[[1L]],
            "<b>a</b><br>state: new<br>exec: auto<br>params: x = 1"
        )
        expect_equal(
            title[[2L]],
            paste0(
                "<b>b</b><br>state: new<br>exec: reduce<br>",
                "params: n = 3<br>depends: a"
            )
        )
    })

    it("shows tags, the lock and the last run in the hover title", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) x, tags = c("io", "raw")) |>
            pip_run(lgr = NULL) |>
            pip_lock()
        start <- as.POSIXct("2026-01-02 03:04:05")
        p[["a", "time_start"]] <- start
        p[["a", "time"]] <- start + 1.5

        title <- pip_graph(p)[["nodes"]][["title"]]

        expect_match(title, "<br>tags: io, raw<br>", fixed = TRUE)
        expect_match(title, "<br>locked: yes<br>", fixed = TRUE)
        expect_match(
            title,
            "<br>last run: 2026-01-02 03:04:05 (1.5 secs)<br>",
            fixed = TRUE
        )
    })

    it("shows no duration in the hover title if the last run failed", {
        p <- pip_new() |>
            pip_add("a", \(x = 1) stop("boom"))
        suppressWarnings(pip_run(p, lgr = NULL, on_error = "continue"))

        title <- pip_graph(p)[["nodes"]][["title"]]

        expect_match(title, "state: failed", fixed = TRUE)
        expect_match(title, "last run: [0-9: -]+<br>")
        expect_no_match(title, "secs")
    })

    it("shows the failed keys of a mapped step in the hover title", {
        p <- pip_new() |>
            pip_add("a", \() list(k1 = 1, k2 = "x", k3 = 3), exec = "split") |>
            pip_add("b", \(x = ~a) log(x))
        suppressWarnings(pip_run(p, lgr = NULL, on_error = "continue"))

        title <- pip_graph(p)[["nodes"]][["title"]][[2L]]

        expect_match(title, "<br>failed keys: k2<br>", fixed = TRUE)
    })

    it("formats the params in the hover title in short form", {
        p <- pip_new() |>
            pip_add(
                "a",
                \(
                    data = datasets::iris,
                    s = strrep("x", 50),
                    f = \(z) z,
                    v = 1:3,
                    nul = NULL
                ) {
                    1
                }
            )
        title <- pip_graph(p)[["nodes"]][["title"]]

        expect_match(title, "data = &lt;data.frame&gt;", fixed = TRUE)
        expect_match(
            title,
            sprintf("s = &quot;%s...", strrep("x", 29)),
            fixed = TRUE
        )
        expect_match(title, "v = 1:3", fixed = TRUE)
    })

    it("escapes HTML in the hover title", {
        p <- pip_new() |>
            pip_add("<a & 'b'>", \(x = "<i>") x, tags = "<b>")

        title <- pip_graph(p)[["nodes"]][["title"]]

        expect_equal(
            title,
            paste0(
                "<b>&lt;a &amp; &#39;b&#39;&gt;</b><br>state: new<br>",
                "exec: auto<br>tags: &lt;b&gt;<br>",
                "params: x = &quot;&lt;i&gt;&quot;"
            )
        )
    })
})
