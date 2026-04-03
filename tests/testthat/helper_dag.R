
create_diamond_dag <- function()
{
    d <- dag_new()
    dag_add_node(d)
    dag_add_node(d)
    dag_add_node(d)
    dag_add_node(d)
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
    d <- dag_new()
    dag_add_node(d)
    dag_add_node(d)
    dag_add_node(d)
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

    dag_tidy_up(d)
    d
}

create_bin_tree_dag <- function()
{
    d <- dag_new()
    dag_add_node(d)
    dag_add_node(d)
    dag_add_node(d)
    dag_add_node(d)
    expect_equal(dag_add_node_at(d, 2), 4)
    dag_add_node(d)
    dag_add_node(d)
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

    dag_tidy_up(d)
    d
}
