
dag_add_edge <- function(
    d, from, to,
    checkTopo = TRUE,
    checkCycle = FALSE
) {
    d$add_edge(from, to, checkTopo, checkCycle)
}

dag_get_reachable_nodes_down <- function(d, start_ids, inTopoOrder = FALSE)
{
    d$get_reachable_nodes_down(start_ids, inTopoOrder)
}

dag_get_reachable_nodes_up <- function(d, start_ids, inTopoOrder = FALSE)
{
    d$get_reachable_nodes_up(start_ids, inTopoOrder)
}

dag_remove_node <- function(d, id, force = FALSE)
{
    d$remove_node(id, force)
}

dag_remove_edge <- function(d, from, to, force = FALSE)
{
    d$remove_edge(from, to, force)
}

dag_print <- function(d, from = 0)
{
    d$print(from)
}
