
describe("Dag",
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

        expect_false(d$has_edge(0, 1, warn = FALSE))
        expect_true(d$add_edge_in_order(0, 1, warn = FALSE))
        expect_true(d$has_edge(0, 1, warn = FALSE))

        expect_false(d$has_edge(1, 2, warn = FALSE))
        expect_true(d$add_edge_in_order(1, 2, warn = FALSE))
        expect_true(d$has_edge(1, 2, warn = FALSE))
        expect_equal(d$add_node_at(1), 3)
        expect_equal(d$size(), 4)
        expect_equal(d$get_nodes_order(), c(0, 3, 1, 2))
        expect_equal(d$get_nodes_pos(), c(0, 2, 3, 1))

        expect_false(d$has_edge(0, 3, warn = FALSE))
        expect_true(d$add_edge_in_order(0, 3, warn = FALSE))
        expect_true(d$has_edge(0, 3, warn = FALSE))
    })

    it("can check if node exists",
    {
        expect_no_warning(
            expect_false(d$has_node(4, warn = FALSE))
        )
        expect_warning(
            expect_false(d$has_node(4, warn = TRUE)),
            "node id 4 not in DAG"
        )
    })

    it("can check if edge exists",
    {
        expect_true(d$has_edge(0, 1, warn = TRUE))
        expect_false(d$has_edge(0, 2, warn = TRUE))
        expect_warning(
            expect_false(d$has_edge(0, 4, warn = TRUE)),
            "node id 4 not in DAG"
        )
    })

    it("prevents adding edges from or to non-existent nodes",
    {
        expect_warning(
            expect_false(d$add_edge_in_order(0, to = 4, warn = TRUE)),
            "node id 4 not in DAG"
        )
        expect_warning(
            expect_false(d$add_edge_in_order(from = 5, to = 2, warn = TRUE)),
            "node id 5 not in DAG"
        )
    })

    it("prevents re-adding existing edges",
    {
        expect_true(d$has_edge(0, 1, warn = FALSE))
        expect_warning(
            expect_false(d$add_edge_in_order(0, 1, warn = TRUE)),
            "edge 0 -> 1 already exists - operation ignored"
        )
    })

    it("prevents adding self-loop edges",
    {
        expect_warning(
            expect_false(d$add_edge_in_order(0, 0, warn = TRUE)),
            "edge 0 -> 0 not in topological order and thus not added"
        )
    })

    it("prevents adding edges that are not in topological order",
    {
        expect_false(d$has_edge(2, 3, warn = TRUE))
        expect_warning(
            expect_false(d$add_edge_in_order(2, 3, warn = TRUE)),
            "edge 2 -> 3 not in topological order and thus not added"
        )
    })
})
