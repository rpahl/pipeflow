
dag_add_node_at <- function(dag, pos, rebuild_pos = TRUE)
{
    dag$add_node_at(pos, rebuild_pos)
}

dag_add_edge <- function(
    dag, from, to,
    checkTopo = TRUE,
    checkCycle = FALSE
) {
    dag$add_edge(from, to, checkTopo, checkCycle)
}

