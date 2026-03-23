
describe("Dag",
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

    it("can insert node in between",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()

        # 0 1 2
        expect_equal(d$get_nodes_order(), c(0, 1, 2))
        expect_equal(d$get_nodes_pos(), c(0, 1, 2))

        expect_equal(dag_add_node_at(d, 1), 3)
        # 0 (3) 1 2 <- new nodes order
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

        expect_equal(dag_add_node_at(d, 1, rebuild_pos = FALSE), 3)
        expect_equal(dag_add_node_at(d, 0, rebuild_pos = FALSE), 4)
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

    it("can determine downstream and upstream nodes",
    {
        d <- new(Dag)
        d$add_node()
        d$add_node()
        d$add_node()
        d$add_node()
        expect_equal(d$get_reachable_nodes_down(0), 0)
        expect_equal(d$get_reachable_nodes_down(1), 1)
        expect_equal(d$get_reachable_nodes_up(2), 2)
        expect_equal(d$get_reachable_nodes_up(3), 3)

        dag_add_edge(d, 0, 1)
        dag_add_edge(d, 2, 3)
        expect_equal(d$get_reachable_nodes_down(0), c(0, 1))
        expect_equal(d$get_reachable_nodes_down(1), 1)
        expect_equal(d$get_reachable_nodes_down(2), c(2, 3))
        expect_equal(d$get_reachable_nodes_down(3), 3)
        expect_equal(d$get_reachable_nodes_down(c(0, 2)), c(0, 2, 3, 1))

        expect_equal(d$get_reachable_nodes_up(0), 0)
        expect_equal(d$get_reachable_nodes_up(1), c(1, 0))
        expect_equal(d$get_reachable_nodes_up(2), 2)
        expect_equal(d$get_reachable_nodes_up(3), c(3, 2))

        # Connecting both subgraphs should make all nodes reachable
        dag_add_edge(d, 1, 2)
        expect_equal(d$get_reachable_nodes_down(0), c(0, 1, 2, 3))
        expect_equal(d$get_reachable_nodes_up(3), c(3, 2, 1, 0))
    })


    it("downstream or upstream of non-existent nodes gives warning",
    {
        d <- new(Dag)
        d$add_node()
        expect_warning(
            expect_equal(d$get_reachable_nodes_down(1), numeric()),
            "node id 1 not in DAG"
        )
        expect_warning(
            expect_equal(d$get_reachable_nodes_up(1), numeric()),
            "node id 1 not in DAG"
        )
    })
})
