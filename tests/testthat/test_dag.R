
describe("Dag creation and properties",
{
    it("can add nodes and edges",
    {
        d <- new(Dag)
        expect_equal(d$size(), 0)
        expect_false(d$has_node(0))
        expect_false(d$has_node(1))

        expect_equal(d$add_node(), 0)
        expect_true(d$has_node(0))
        expect_equal(d$size(), 1)

        expect_equal(d$add_node(), 1)
        expect_true(d$has_node(1))
        expect_equal(d$size(), 2)

        expect_equal(d$add_node(), 2)
        expect_true(d$has_node(2))
        expect_equal(d$size(), 3)

        expect_false(d$has_edge(0, 1))
        expect_true(dag_add_edge(d, 0, 1))
        expect_true(d$has_edge(0, 1))

        expect_false(d$has_edge(0, 2))
        expect_true(dag_add_edge(d, 0, 2))
        expect_true(d$has_edge(0, 2))
    })

    it("can be cloned",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        dag_add_edge(d, 0, 1)
        d_clone <- d$clone()
        expect_equal(d$size(), d_clone$size())
        expect_equal(d$get_nodes_order(), d_clone$get_nodes_order())
        expect_equal(d$get_nodes_pos(), d_clone$get_nodes_pos())

        d$add_node()
        expect_equal(d$size(), d_clone$size() + 1)
        expect_true(d$has_node(2))
        expect_false(d_clone$has_node(2))
    })

    it("can inspect the min and max node id",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()
        expect_equal(d$get_min_id(), 0)
        expect_equal(d$get_max_id(), 2)
        d$add_node_at(1)
        expect_equal(d$get_max_id(), 3)
        dag_remove_node(d, 0)
        d$tidy_up()
        expect_equal(d$get_min_id(), 1)
    })

    it("can insert node in between",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()
        # nodes order
        # 0 - 1 - 2

        expect_equal(d$get_nodes_order(), c(0, 1, 2))
        expect_equal(d$get_nodes_pos(), c(0, 1, 2))

        expect_equal(d$add_node_at(1), 3)
        d$tidy_up()
        # new nodes order
        # 0 - 3 - 1 - 2
        expect_equal(d$get_nodes_order(), c(0, 3, 1, 2))

        # new nodes pos
        # 0 => 0
        # 1 => 2
        # 2 => 3
        # 3 => 1
        expect_equal(d$get_nodes_pos(), c(0, 2, 3, 1))
    })

    it("can insert several nodes without rebuilding the node pos",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()

        expect_equal(d$get_nodes_pos(), c(0, 1, 2))

        expect_equal(d$add_node_at(1), 3)
        expect_equal(d$add_node_at(0), 4)
        expect_equal(d$get_nodes_order(), c(4, 0, 3, 1, 2))
        expect_equal(d$get_nodes_pos(), c(0, 1, 2))

        d$tidy_up() # rebuilds nodes pos
        expect_equal(d$get_nodes_order(), c(4, 0, 3, 1, 2))
        expect_equal(d$get_nodes_pos(), c(1, 3, 4, 2, 0))
    })

    it("warns if edge existence check refers to non-existent nodes",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        expect_false(d$has_edge(0, 1))
        expect_warning(
            expect_false(d$has_edge(0, 2)),
            "node id 2 not in DAG"
        )
    })

    it("prevents adding edges from or to non-existent nodes",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()

        expect_warning(
            expect_false(dag_add_edge(d, 0, to = 2)),
            "node id 2 not in DAG - operation ignored"
        )
        expect_warning(
            expect_false(dag_add_edge(d, from = 3, to = 0)),
            "node id 3 not in DAG - operation ignored"
        )
    })

    it("prevents re-adding existing edges",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        expect_true(dag_add_edge(d, 0, 1))
        expect_warning(
            expect_false(dag_add_edge(d, 0, 1)),
            "edge 0 -> 1 already exists - operation ignored"
        )
    })

    it("prevents adding self-loop edges",
    {
        d <- new(Dag)
        d$add_node()
        expect_warning(
            expect_false(dag_add_edge(d, 0, 0)),
            "edge 0 -> 0 not in topological order and thus not added"
        )
    })

    it("by default prevents adding edges that are not in topological order",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        expect_warning(
            expect_false(dag_add_edge(d, 1, 0)),
            "edge 1 -> 0 not in topological order and thus not added"
        )
        expect_false(d$has_edge(1, 0))

        expect_no_warning(
            expect_true(dag_add_edge(d, 1, 0, checkTopo = FALSE))
        )
        expect_true(d$has_edge(1, 0))
    })
})

describe("add dag",
{
    it("can add one dag to another",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        dag_add_edge(d, 0, 1)

        other <- new(Dag)
        other$add_node()
        other$add_node()
        dag_add_edge(other, 0, 1)

        expect_equal(d$get_nodes_order(), c(0, 1))
        expect_equal(other$get_nodes_order(), c(0, 1))

        res <- d$add_dag(other)
        expect_equal(res, 2:3)
        expect_equal(d$get_nodes_order(), 0:3)
        expect_equal(other$get_nodes_order(), 0:1) # other was not changed

        expect_equal(d$get_outgoing(0), 1)
        expect_equal(d$get_incoming(1), 0)
        expect_equal(d$get_outgoing(2), 3)
        expect_equal(d$get_incoming(3), 2)
    })

    it("can add snake to diamond graph",
    {
        d <- create_diamond_dag()
        n <- d$size()
        s <- create_snake_dag()

        expect_equal(d$get_nodes_order(), 0:3)
        s_nodes <- s$get_nodes_order()
        expect_equal(s_nodes, c(0, 3, 1, 2))
        res <- d$add_dag(s)
        expect_equal(res, s_nodes + n)
        expect_equal(d$get_nodes_order(), c(0:3, s_nodes + n))
    })
})

describe("node removal",
{
    it("unconnected nodes can always be removed",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        expect_equal(d$get_nodes_order(), c(0, 1))
        expect_equal(d$get_nodes_pos(), c(0, 1))
        expect_true(d$is_tidy())

        expect_true(dag_remove_node(d, 0))
        expect_false(d$has_node(0))
        expect_true(d$has_node(1))
        expect_equal(d$get_nodes_order(), c(0, 1))
        expect_false(d$is_tidy())

        expect_true(dag_remove_node(d, 1))
        expect_false(d$has_node(1))
        expect_warning(
            expect_equal(dag_get_reachable_nodes_down(d, 1), numeric()),
            "node id 1 not in DAG"
        )
        expect_equal(d$get_nodes_order(), c(0, 1))
        d$tidy_up()
        expect_equal(d$get_nodes_order(), numeric())
        expect_equal(d$get_nodes_pos(), numeric())
    })

    it("connected nodes can be removed directly if they have no outgoing edges",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        dag_add_edge(d, 0, 1)

        expect_true(d$is_tidy())
        expect_false(dag_remove_node(d, 0)) |>
            expect_warning("would leave downstream node 1 dangling")

        expect_true(d$is_tidy())
        expect_true(dag_remove_node(d, 1)) |> expect_no_warning()
        expect_false(d$has_node(1))
        expect_false(d$is_tidy())
    })


    it("signals if removal would leave downstream nodes dangling",
    {
        # Diamond
        d <- create_diamond_dag()
        expect_true(d$is_tidy())
        expect_true(dag_remove_node(d, 1)) |> expect_no_warning()
        expect_false(d$is_tidy())

        expect_false(dag_remove_node(d, 2)) |>
            expect_warning("would leave downstream node 3 dangling")
        expect_true(dag_remove_node(d, 3))
        expect_true(dag_remove_node(d, 2))

        # Tree
        d <- create_bin_tree_dag()
        expect_false(dag_remove_node(d, 4)) |>
            expect_warning("would leave downstream node 5 dangling")
        expect_true(dag_remove_node(d, 5))
        expect_false(dag_remove_node(d, 4)) |>
            expect_warning("would leave downstream node 6 dangling")
        expect_true(dag_remove_node(d, 6))
        expect_true(dag_remove_node(d, 4))

        # Snake
        d <- create_snake_dag()
        expect_true(dag_remove_node(d, 3))
        expect_false(dag_remove_node(d, 1)) |>
            expect_warning("would leave downstream node 2 dangling")
    })

    it("can enforce removal of nodes resulting in dangling nodes",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()
        dag_add_edge(d, 0, 1)
        dag_add_edge(d, 1, 2)
        expect_false(dag_remove_node(d, 1)) |>
            expect_warning("would leave downstream node 2 dangling")

        expect_true(dag_remove_node(d, 1, force = TRUE)) |> expect_no_warning()
        expect_equal(dag_get_reachable_nodes_down(d, 0), 0)
        d$tidy_up()
        expect_equal(d$get_nodes_order(), c(0, 2))
        expect_equal(d$get_dangling_nodes(), 2)
    })
})

describe("edge removal",
{
    it("connected nodes can be removed directly if not dangling afterwards",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()
        dag_add_edge(d, 0, 2)

        expect_false(dag_remove_edge(d, 0, 2)) |>
            expect_warning("would leave downstream node 2 dangling")
        expect_true(d$has_edge(0, 2))

        dag_add_edge(d, 1, 2)
        expect_true(dag_remove_edge(d, 0, 2)) |> expect_no_warning()
        expect_false(d$has_edge(0, 2))
    })

    it("can enforce removal of edges resulting in dangling nodes",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        dag_add_edge(d, 0, 1)
        expect_true(d$has_edge(0, 1))
        expect_false(dag_remove_edge(d, 0, 1)) |>
            expect_warning("would leave downstream node 1 dangling")

        expect_true(dag_remove_edge(d, 0, 1, force = TRUE)) |>
            expect_no_warning()
        expect_false(d$has_edge(0, 1))
    })
})


describe("reachable nodes",
{
    it("can determine reachable nodes up- and downstream",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()
        d$add_node()
        expect_equal(dag_get_reachable_nodes_down(d, 0), 0)
        expect_equal(dag_get_reachable_nodes_down(d, 1), 1)
        expect_equal(dag_get_reachable_nodes_up(d, 2), 2)
        expect_equal(dag_get_reachable_nodes_up(d, 3), 3)

        dag_add_edge(d, 0, 1)
        dag_add_edge(d, 2, 3)
        expect_equal(dag_get_reachable_nodes_down(d, 0), c(0, 1))
        expect_equal(dag_get_reachable_nodes_down(d, 1), 1)
        expect_equal(dag_get_reachable_nodes_down(d, 2), c(2, 3))
        expect_equal(dag_get_reachable_nodes_down(d, 3), 3)
        expect_equal(dag_get_reachable_nodes_down(d, c(0, 2)), c(0, 2, 3, 1))
        expect_equal(
            dag_get_reachable_nodes_down(d, c(0, 2), inTopoOrder = TRUE),
            c(0, 1, 2, 3)
        )

        expect_equal(dag_get_reachable_nodes_up(d, 0), 0)
        expect_equal(dag_get_reachable_nodes_up(d, 1), c(1, 0))
        expect_equal(dag_get_reachable_nodes_up(d, 2), 2)
        expect_equal(dag_get_reachable_nodes_up(d, 3), c(3, 2))
        expect_equal(
            dag_get_reachable_nodes_up(d, 3, inTopoOrder = TRUE),
            c(2, 3)
        )

        # Connecting both subgraphs should make all nodes reachable
        dag_add_edge(d, 1, 2)
        expect_equal(dag_get_reachable_nodes_down(d, 0), c(0, 1, 2, 3))
        expect_equal(dag_get_reachable_nodes_up(d, 3), c(3, 2, 1, 0))
    })

    it("correctly determines reachable nodes if nodes were inserted",
    {
        d <- create_snake_dag()
        expect_equal(dag_get_reachable_nodes_down(d, 0), c(0, 3, 1, 2))
        expect_equal(dag_get_reachable_nodes_down(d, 3), c(3, 1, 2))
        expect_equal(dag_get_reachable_nodes_down(d, 1), c(1, 2))
        expect_equal(dag_get_reachable_nodes_down(d, 2), c(2))

        expect_equal(dag_get_reachable_nodes_up(d, 0), c(0))
        expect_equal(dag_get_reachable_nodes_up(d, 3), c(3, 0))
        expect_equal(dag_get_reachable_nodes_up(d, 1), c(1, 3, 0))
        expect_equal(dag_get_reachable_nodes_up(d, 2), c(2, 1, 3, 0))
    })

    it("can determine reachable nodes even if graph is not tidy after removal",
    {
        d <- create_diamond_dag()
        expect_true(d$is_tidy())
        expect_equal(d$get_nodes_order(), 0:3)
        expect_equal(dag_get_reachable_nodes_down(d, 0), 0:3)

        expect_true(dag_remove_node(d, 1)) |> expect_no_warning()
        expect_false(d$is_tidy())
        expect_equal(d$get_nodes_order(), 0:3)
        expect_equal(dag_get_reachable_nodes_down(d, 0), c(0, 2, 3))

        expect_equal(dag_get_reachable_nodes_up(d, 3), c(3, 2, 0))
        expect_equal(
            dag_get_reachable_nodes_up(d, 3, inTopoOrder = TRUE),
            c(0, 2, 3)
        )
        # Using inTopoOrder = TRUE automatically tidies the graph
        expect_true(d$is_tidy())
        expect_equal(d$get_nodes_order(), c(0, 2, 3))
    })

    it("trying to reach non-existent nodes gives warning",
    {
        d <- new(Dag)
        d$add_node()
        expect_warning(
            expect_equal(dag_get_reachable_nodes_down(d, 1), numeric()),
            "node id 1 not in DAG"
        )
        expect_warning(
            expect_equal(dag_get_reachable_nodes_up(d, 1), numeric()),
            "node id 1 not in DAG"
        )
    })
})


describe("dag rebuild",
{
    # Tree
    d <- create_bin_tree_dag()
    nord <- d$get_nodes_order()
    res <- d$rebuild()
    expect_equal(res, nord)
    expect_equal(d$get_nodes_order(), 1:d$size() - 1)

    # Snake
    d <- create_snake_dag()
    expect_true(dag_remove_node(d, 1))
    expect_equal(d$get_nodes_order(), c(0, 3, 1, 2))
    d$rebuild()
    expect_equal(d$get_nodes_order(), 0:2)
})


describe("dag shift",
{
    it("works with empty DAG",
    {
        d <- new(Dag)
        expect_equal(d$size(), 0)
        expect_no_error(d$shift(10))
        expect_equal(d$size(), 0)
    })

    it("correctly shifts node IDs and edges of bin snake example",
    {
        d <- create_snake_dag()
        expect_true(d$has_edge(0, 3))
        expect_true(d$has_edge(0, 1))
        expect_equal(d$get_outgoing(0), c(3, 1))
        ids <- d$get_nodes_order()
        pos <- d$get_nodes_pos()
        d$shift(10)
        expect_true(d$has_node(10))
        expect_equal(d$get_outgoing(10), c(13, 11))
        expect_equal(d$get_nodes_order(), ids + 10)
        # Relative pos should still be the same
        expect_equal(d$get_nodes_pos(), pos)

        expect_true(d$has_edge(10, 13))
        expect_true(d$has_edge(10, 11))
        expect_true(d$has_edge(11, 12))
        expect_true(d$has_edge(13, 12))
    })

    it("correctly shifts node IDs and edges of bin tree",
    {
        d <- create_bin_tree_dag()
        out0 <- d$get_outgoing(0)
        in0 <- d$get_incoming(0)
        ids <- d$get_nodes_order()
        pos <- d$get_nodes_pos()
        d$shift(10)
        expect_equal(d$get_nodes_order(), ids + 10)
        expect_equal(d$get_nodes_pos(), pos)
        expect_equal(d$get_outgoing(10), out0 + 10)
        expect_equal(d$get_incoming(10), in0 + 10)
    })
})
