
describe("Dag creation and properties",
{
    it("can add nodes and edges",
    {
        d <- dag_new()
        expect_equal(dag_size(d), 0)
        expect_false(dag_has_node(d, 0))
        expect_false(dag_has_node(d, 1))

        expect_equal(dag_add_node(d), 0)
        expect_true(dag_has_node(d, 0))
        expect_equal(dag_size(d), 1)

        expect_equal(dag_add_node(d), 1)
        expect_true(dag_has_node(d, 1))
        expect_equal(dag_size(d), 2)
        expect_equal(dag_add_node(d), 2)
        expect_true(dag_has_node(d, 2))
        expect_equal(dag_size(d), 3)

        expect_false(dag_has_edge(d, 0, 1))
        expect_true(dag_add_edge(d, 0, 1))
        expect_true(dag_has_edge(d, 0, 1))
        expect_false(dag_has_edge(d, 0, 2))
        expect_true(dag_add_edge(d, 0, 2))
        expect_true(dag_has_edge(d, 0, 2))
    })

    it("can add multiple nodes and edges at once",
    {
        d <- dag_new()
        expect_equal(dag_add_node(d), 0)
        expect_equal(dag_add_node(d), 1)
        expect_equal(dag_add_node(d), 2)
        expect_equal(dag_add_node(d), 3)
        res <- dag_add_edges(d, c(0, 1), c(1, 2))
        expect_equal(res, c(TRUE, TRUE))
        expect_true(dag_has_edge(d, 0, 1))
        expect_true(dag_has_edge(d, 1, 2))

        expect_warning(
            res <- dag_add_edges(d, c(2, 3), c(3, 4)),
            "node id 4 not in DAG - operation ignored"
        )
        expect_equal(res, c(TRUE, FALSE))
    })

    it("can be cloned",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_edge(d, 0, 1)
        d_clone <- dag_clone(d)
        expect_equal(dag_size(d), dag_size(d_clone))
        expect_equal(dag_get_nodes_order(d), dag_get_nodes_order(d_clone))

        dag_add_node(d)
        expect_equal(dag_size(d), dag_size(d_clone) + 1)
        expect_true(dag_has_node(d, 2))
        expect_false(dag_has_node(d_clone, 2))
    })

    it("can inspect the min and max node id",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
        expect_equal(dag_get_min_id(d), 0)
        expect_equal(dag_get_max_id(d), 2)
        dag_add_node_at(d, 1)
        expect_equal(dag_get_max_id(d), 3)
        dag_remove_node(d, 0)
        dag_tidy_up(d)
        expect_equal(dag_get_min_id(d), 1)
    })

    it("can insert node in between",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
        # nodes order
        # 0 - 1 - 2

        expect_equal(dag_get_nodes_order(d), c(0, 1, 2))

        expect_equal(dag_add_node_at(d, 1), 3)
        dag_tidy_up(d)
        # new nodes order
        # 0 - 3 - 1 - 2
        expect_equal(dag_get_nodes_order(d), c(0, 3, 1, 2))
    })

    it("warns if edge existence check refers to non-existent nodes",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        expect_false(dag_has_edge(d, 0, 1))
        expect_warning(
            expect_false(dag_has_edge(d, 0, 2)),
            "node id 2 not in DAG"
        )
    })

    it("prevents adding edges from or to non-existent nodes",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
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
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        expect_true(dag_add_edge(d, 0, 1))
        expect_warning(
            expect_false(dag_add_edge(d, 0, 1)),
            "edge 0 -> 1 already exists - operation ignored"
        )
    })
})

describe("add dag",
{
    it("can add one dag to another",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_edge(d, 0, 1)

        other <- dag_new()
        dag_add_node(other)
        dag_add_node(other)
        dag_add_edge(other, 0, 1)

        expect_equal(dag_get_nodes_order(d), c(0, 1))
        expect_equal(dag_get_nodes_order(other), c(0, 1))

        res <- dag_add_dag(d, other)
        expect_equal(res, 2:3)
        expect_equal(dag_get_nodes_order(d), 0:3)
        expect_equal(dag_get_nodes_order(other), 0:1) # other was not changed

        expect_equal(dag_get_outgoing(d, 0), 1)
        expect_equal(dag_get_incoming(d, 1), 0)
        expect_equal(dag_get_outgoing(d, 2), 3)
        expect_equal(dag_get_incoming(d, 3), 2)
    })

    it("can add snake to diamond graph",
    {
        d <- create_diamond_dag()
        n <- dag_size(d)
        s <- create_snake_dag()

        expect_equal(dag_get_nodes_order(d), 0:3)
        s_nodes <- dag_get_nodes_order(s)
        expect_equal(s_nodes, c(0, 3, 1, 2))
        res <- dag_add_dag(d, s)
        expect_equal(res, s_nodes + n)
        expect_equal(dag_get_nodes_order(d), c(0:3, s_nodes + n))
    })
})

describe("node removal",
{
    it("unconnected nodes can always be removed",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        expect_equal(dag_get_nodes_order(d), c(0, 1))
        expect_true(dag_is_tidy(d))

        expect_true(dag_remove_node(d, 0))
        expect_false(dag_has_node(d, 0))
        expect_true(dag_has_node(d, 1))
        expect_equal(dag_get_nodes_order(d), c(0, 1))
        expect_false(dag_is_tidy(d))

        expect_true(dag_remove_node(d, 1))
        expect_false(dag_has_node(d, 1))
        expect_warning(
            expect_equal(dag_get_reachable_nodes_down(d, 1), numeric()),
            "node id 1 not in DAG"
        )
        expect_equal(dag_get_nodes_order(d), c(0, 1))
        dag_tidy_up(d)
        expect_equal(dag_get_nodes_order(d), numeric())
    })

    it("connected nodes can be removed directly if they have no outgoing edges",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_edge(d, 0, 1)

        expect_true(dag_is_tidy(d))
        expect_false(dag_remove_node(d, 0)) |>
            expect_warning("would leave downstream node 1 dangling")

        expect_true(dag_is_tidy(d))
        expect_true(dag_remove_node(d, 1)) |> expect_no_warning()
        expect_false(dag_has_node(d, 1))
        expect_false(dag_is_tidy(d))
    })


    it("signals if removal would leave downstream nodes dangling",
    {
        # Diamond
        d <- create_diamond_dag()
        expect_true(dag_is_tidy(d))
        expect_true(dag_remove_node(d, 1)) |> expect_no_warning()
        expect_false(dag_is_tidy(d))

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
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
        dag_add_edge(d, 0, 1)
        dag_add_edge(d, 1, 2)
        expect_false(dag_remove_node(d, 1)) |>
            expect_warning("would leave downstream node 2 dangling")

        expect_true(dag_remove_node(d, 1, force = TRUE)) |> expect_no_warning()
        expect_equal(dag_get_reachable_nodes_down(d, 0), 0)
        dag_tidy_up(d)
        expect_equal(dag_get_nodes_order(d), c(0, 2))
        expect_equal(dag_get_dangling_nodes(d), 2)
    })
})

describe("edge removal",
{
    it("connected nodes can be removed directly if not dangling afterwards",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
        dag_add_edge(d, 0, 2)

        expect_false(dag_remove_edge(d, 0, 2)) |>
            expect_warning("would leave downstream node 2 dangling")
        expect_true(dag_has_edge(d, 0, 2))

        dag_add_edge(d, 1, 2)
        expect_true(dag_remove_edge(d, 0, 2)) |> expect_no_warning()
        expect_false(dag_has_edge(d, 0, 2))
    })

    it("can enforce removal of edges resulting in dangling nodes",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_edge(d, 0, 1)
        expect_true(dag_has_edge(d, 0, 1))
        expect_false(dag_remove_edge(d, 0, 1)) |>
            expect_warning("would leave downstream node 1 dangling")

        expect_true(dag_remove_edge(d, 0, 1, force = TRUE)) |>
            expect_no_warning()
        expect_false(dag_has_edge(d, 0, 1))
    })
})


describe("reachable nodes",
{
    it("can determine reachable nodes up- and downstream",
    {
        d <- dag_new()
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
        dag_add_node(d)
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

        expect_equal(dag_get_reachable_nodes_up(d, 0), 0)
        expect_equal(dag_get_reachable_nodes_up(d, 1), c(1, 0))
        expect_equal(dag_get_reachable_nodes_up(d, 2), 2)
        expect_equal(dag_get_reachable_nodes_up(d, 3), c(3, 2))

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
        expect_true(dag_is_tidy(d))
        expect_equal(dag_get_nodes_order(d), 0:3)
        expect_equal(dag_get_reachable_nodes_down(d, 0), 0:3)

        expect_true(dag_remove_node(d, 1)) |> expect_no_warning()
        expect_false(dag_is_tidy(d))
        expect_equal(dag_get_nodes_order(d), 0:3)
        expect_equal(dag_get_reachable_nodes_down(d, 0), c(0, 2, 3))
        expect_equal(dag_get_reachable_nodes_up(d, 3), c(3, 2, 0))

        dag_tidy_up(d)
        expect_true(dag_is_tidy(d))
        expect_equal(dag_get_nodes_order(d), c(0, 2, 3))
    })

    it("trying to reach non-existent nodes gives warning",
    {
        d <- dag_new()
        dag_add_node(d)
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
    nord <- dag_get_nodes_order(d)
    res <- dag_rebuild(d)
    expect_equal(res, nord)
    expect_equal(dag_get_nodes_order(d), 1:dag_size(d) - 1)

    # Snake
    d <- create_snake_dag()
    expect_true(dag_remove_node(d, 1))
    expect_equal(dag_get_nodes_order(d), c(0, 3, 1, 2))
    dag_rebuild(d)
    expect_equal(dag_get_nodes_order(d), 0:2)
})


describe("dag shift",
{
    it("works with empty DAG",
    {
        d <- dag_new()
        expect_equal(dag_size(d), 0)
        expect_no_error(dag_shift(d, 10))
        expect_equal(dag_size(d), 0)
    })

    it("correctly shifts node IDs and edges of bin snake example",
    {
        d <- create_snake_dag()
        expect_true(dag_has_edge(d, 0, 3))
        expect_true(dag_has_edge(d, 0, 1))
        expect_equal(dag_get_outgoing(d, 0), c(3, 1))
        ids <- dag_get_nodes_order(d)
        dag_shift(d, 10)
        expect_true(dag_has_node(d, 10))
        expect_equal(dag_get_outgoing(d, 10), c(13, 11))
        expect_equal(dag_get_nodes_order(d), ids + 10)

        expect_true(dag_has_edge(d, 10, 13))
        expect_true(dag_has_edge(d, 10, 11))
        expect_true(dag_has_edge(d, 11, 12))
        expect_true(dag_has_edge(d, 13, 12))
    })

    it("correctly shifts node IDs and edges of bin tree",
    {
        d <- create_bin_tree_dag()
        out0 <- dag_get_outgoing(d, 0)
        in0 <- dag_get_incoming(d, 0)
        ids <- dag_get_nodes_order(d)
        dag_shift(d, 10)
        expect_equal(dag_get_nodes_order(d), ids + 10)
        expect_equal(dag_get_outgoing(d, 10), out0 + 10)
        expect_equal(dag_get_incoming(d, 10), in0 + 10)
    })
})
