
describe("Dag creation and inspection",
{
    d <- new(Dag)

    it("can add nodes and edges",
    {
        expect_equal(d$add_node(), 0)
        expect_equal(d$add_node(), 1)
        expect_equal(d$add_node(), 2)
        expect_equal(d$size(), 3)
        expect_equal(d$get_nodes_order(), c(0, 1, 2))
        expect_equal(d$get_nodes_pos(), c(0, 1, 2))

        expect_false(d$has_edge(0, 1))
        expect_true(d$add_edge_in_order(0, 1))
        expect_true(d$has_edge(0, 1))

        expect_false(d$has_edge(1, 2))
        expect_true(d$add_edge_in_order(1, 2))
        expect_true(d$has_edge(1, 2))
        expect_equal(d$add_node_at(1), 3)
        expect_equal(d$size(), 4)
        expect_equal(d$get_nodes_order(), c(0, 3, 1, 2))
        expect_equal(d$get_nodes_pos(), c(0, 2, 3, 1))

        expect_false(d$has_edge(0, 3))
        expect_true(d$add_edge_in_order(0, 3))
        expect_true(d$has_edge(0, 3))

        expect_true(d$add_edge_in_order(3, 1))
        expect_true(d$has_edge(3, 1))

        expect_true(d$add_edge_in_order(3, 2))
        expect_true(d$has_edge(3, 2))
    })

    it("can check if node exists",
    {
        expect_warning(
            expect_false(d$has_node(4)),
            "node id 4 not in DAG"
        )
    })

    it("can check if edge exists",
    {
        expect_true(d$has_edge(0, 1))
        expect_false(d$has_edge(0, 2))
        expect_warning(
            expect_false(d$has_edge(0, 4)),
            "node id 4 not in DAG"
        )
    })

    it("prevents adding edges from or to non-existent nodes",
    {
        expect_warning(
            expect_false(d$add_edge_in_order(0, to = 4)),
            "node id 4 not in DAG"
        )
        expect_warning(
            expect_false(d$add_edge_in_order(from = 5, to = 2)),
            "node id 5 not in DAG"
        )
    })

    it("prevents re-adding existing edges",
    {
        expect_true(d$has_edge(0, 1))
        expect_warning(
            expect_false(d$add_edge_in_order(0, 1)),
            "edge 0 -> 1 already exists - operation ignored"
        )
    })

    it("prevents adding self-loop edges",
    {
        expect_warning(
            expect_false(d$add_edge_in_order(0, 0)),
            "edge 0 -> 0 not in topological order and thus not added"
        )
    })

    it("prevents adding edges that are not in topological order",
    {
        expect_false(d$has_edge(2, 3))
        expect_warning(
            expect_false(d$add_edge_in_order(2, 3)),
            "edge 2 -> 3 not in topological order and thus not added"
        )
    })

    it("can get downstream nodes",
    {
        expect_equal(d$get_downstream_nodes(0), c(1, 3, 2))
        expect_equal(d$get_downstream_nodes(3), c(1, 2))
        expect_equal(d$get_downstream_nodes(1), c(2))
        expect_equal(d$get_downstream_nodes(2), numeric())
    })

    it("can get upstream nodes",
    {
        expect_equal(d$get_upstream_nodes(0), numeric())
        expect_equal(d$get_upstream_nodes(3), c(0))
        expect_equal(d$get_upstream_nodes(1), c(0, 3))
        expect_equal(d$get_upstream_nodes(2), c(1, 3, 0))
    })
})
