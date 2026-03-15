
describe("Dag",
{
    d <- new(Dag)
    expect_equal(d$add_node(), 0)
    expect_equal(d$add_node(), 1)
    expect_equal(d$add_node(), 2)

    expect_equal(d$size(), 3)

    expect_equal(d$add_node_at(1), 3)
    expect_equal(d$size(), 4)
    expect_equal(d$get_topo_order(), c(0, 3, 1, 2))
})
