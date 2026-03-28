
dag_add_edge <- function(
    dag, from, to,
    checkTopo = TRUE,
    checkCycle = FALSE
) {
    dag$add_edge(from, to, checkTopo, checkCycle)
}

dag_get_reachable_nodes_down <- function(dag, start_ids, inTopoOrder = FALSE)
{
    dag$get_reachable_nodes_down(start_ids, inTopoOrder)
}

dag_get_reachable_nodes_up <- function(dag, start_ids, inTopoOrder = FALSE)
{
    dag$get_reachable_nodes_up(start_ids, inTopoOrder)
}

dag_remove_node <- function(dag, id, force = FALSE)
{
    dag$remove_node(id, force)
}

dag_remove_edge <- function(dag, from, to, force = FALSE)
{
    dag$remove_edge(from, to, force)
}
