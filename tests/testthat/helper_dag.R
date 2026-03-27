
create_diamond_dag <- function()
{
    d <- new(Dag)
    d$add_node()
    d$add_node()
    d$add_node()
    d$add_node()
    dag_add_edge(d, 0, 1)
    dag_add_edge(d, 0, 2)
    dag_add_edge(d, 1, 3)
    dag_add_edge(d, 2, 3)
    #   0
    #  / \
    # 1   2
    # \  /
    #  3
    d
}

create_snake_dag <- function()
{
    d <- new(Dag)
    d$add_node()
    d$add_node()
    d$add_node()
    expect_equal(dag_add_node_at(d, 1), 3)
    dag_add_edge(d, 0, 3)
    dag_add_edge(d, 3, 1)
    dag_add_edge(d, 1, 2)
    # 0 - 3 - 1 - 2

    dag_add_edge(d, 0, 1)
    dag_add_edge(d, 3, 2)
    #  /------\
    # 0 - 3 - 1 - 2
    #      \-----/
    d
}

create_bin_tree_dag <- function()
{
    d <- new(Dag)
    d$add_node()
    d$add_node()
    d$add_node()
    d$add_node()
    expect_equal(dag_add_node_at(d, 2), 4)
    d$add_node()
    d$add_node()
    # 0 1 4 2 3 5 6

    dag_add_edge(d, 0, 1)
    dag_add_edge(d, 0, 4)
    dag_add_edge(d, 1, 2)
    dag_add_edge(d, 1, 3)
    dag_add_edge(d, 4, 5)
    dag_add_edge(d, 4, 6)
    #      0
    #     / \
    #    1   4
    #   / \ / \
    #  2  3 5  6
    d
}
