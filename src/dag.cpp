// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <Rcpp.h>
// using namespace Rcpp;

using NodeId = uint32_t;

struct Node {
    std::vector<NodeId> incoming;
    std::vector<NodeId> outgoing;

    bool alive = true;
    bool dirty = false;
};


struct Dag {
    std::vector<Node> nodes;            // nodes stored by running id

    // We keep track of the topological order and position of nodes in the DAG,
    // which allows to insert nodes in between by just modifying these vectors,
    // that is, without having to update all dependencies, edges, etc
    std::vector<NodeId> nodes_pos;      // basically inverse mapping of order
    std::vector<NodeId> nodes_order;    // order of nodes

    bool needs_pos_rebuild = false;
    // std::vector<NodeId> free_ids;    // optional

    // Caching and memoization
    std::vector<uint16_t> visited;      // used to keep track of node visits
    uint16_t visitor_mark = 0;          // allows re-use of visited without reset
    std::vector<NodeId> work_stack;
};

// ---------------------------------------------------------------------------
// Method declaration
// ---------------------------------------------------------------------------

// Inspect
std::size_t size(const Dag* dag);
bool has_edge(const Dag* dag, NodeId from, NodeId to);
bool has_node(const Dag* dag, NodeId id);
bool is_cyclic(const Dag* dag);
std::vector<NodeId> get_nodes_order(const Dag* dag);
std::vector<NodeId> get_nodes_pos(const Dag* dag);
std::vector<NodeId> get_downstream_nodes(Dag* dag, NodeId id);
std::vector<NodeId> get_upstream_nodes(Dag* dag, NodeId id);

// Add
NodeId add_node(Dag* dag);
NodeId add_node_at(Dag* dag, NodeId pos, bool rebuild_pos = true);
bool add_edge(Dag* dag, NodeId from, NodeId to,
    bool checkTopo = true, bool checkCycle = false
);

// Remove
void discard_node(Dag* dag, NodeId id, bool recursive = true);
void remove_node(Dag* dag, NodeId id, bool recursive = true);

// Helpers for internal state
void init_visitor_marking(Dag* dag);
void rebuild_nodes_pos(Dag* dag);
void tidy_up(Dag* dag);


// ---------------------------------------------------------------------------
// Method implementation
// ---------------------------------------------------------------------------

// -------
// Inspect
// -------
// TODO: maybe size() should be the number of alive nodes
std::size_t size(const Dag* dag) { return dag->nodes.size(); }

bool has_edge(const Dag* dag, NodeId from, NodeId to)
{
    if (!has_node(dag, from)) {
        Rcpp::warning("node id %u not in DAG", from);
        return false;
    }
    if (!has_node(dag, to)) {
        Rcpp::warning("node id %u not in DAG", to);
        return false;
    }
    const auto& out = dag->nodes[from].outgoing;
    return std::find(out.begin(), out.end(), to) != out.end();
}

bool has_node(const Dag* dag, NodeId id)
{
    return id < dag->nodes.size() && dag->nodes[id].alive;
}

bool is_cyclic(const Dag* dag)
{
    throw Rcpp::exception("is_cyclic not yet implemented");
}

std::vector<NodeId> get_nodes_order(const Dag* dag) { return dag->nodes_order; }
std::vector<NodeId> get_nodes_pos(const Dag* dag) { return dag->nodes_pos; }
std::vector<NodeId> get_downstream_nodes(Dag* dag, NodeId id)
{
    if (!has_node(dag, id)) {
        Rcpp::warning("node id %u not in DAG", id);
        return {};
    }

    init_visitor_marking(dag);

    std::vector<NodeId> result;
    dag->work_stack.clear();
    dag->work_stack.push_back(id);
    dag->visited[id] = dag->visitor_mark;

    while (!dag->work_stack.empty()) {
        NodeId cur = dag->work_stack.back();
        dag->work_stack.pop_back();
        for (NodeId nxt : dag->nodes[cur].outgoing) {
            if (!dag->nodes[nxt].alive) continue;
            // TODO: evaluate if removing dead nodes from all incoming and
            // outgoing nodes could improve performance by avoiding the
            // above check entirely.

            if (dag->visited[nxt] == dag->visitor_mark) continue;

            dag->visited[nxt] = dag->visitor_mark;
            result.push_back(nxt);
            dag->work_stack.push_back(nxt);
        }
    }
    return result;
}

std::vector<NodeId> get_upstream_nodes(Dag* dag, NodeId id)
{
    if (!has_node(dag, id)) {
        Rcpp::warning("node id %u not in DAG", id);
        return {};
    }

    init_visitor_marking(dag);

    std::vector<NodeId> result;
    dag->work_stack.clear();
    dag->work_stack.push_back(id);
    dag->visited[id] = dag->visitor_mark;

    while (!dag->work_stack.empty()) {
        NodeId cur = dag->work_stack.back();
        dag->work_stack.pop_back();
        for (NodeId nxt : dag->nodes[cur].incoming) {
            if (!dag->nodes[nxt].alive) continue;
            // TODO: evaluate if removing dead nodes from all incoming and
            // outgoing nodes could improve performance by avoiding the
            // above check entirely.

            if (dag->visited[nxt] == dag->visitor_mark) continue;

            dag->visited[nxt] = dag->visitor_mark;
            result.push_back(nxt);
            dag->work_stack.push_back(nxt);
        }
    }
    return result;
}


// ---
// Add
// ---
NodeId add_node(Dag* dag)
{
    NodeId id = dag->nodes.size();
    dag->nodes.emplace_back();
    dag->nodes_order.push_back(id);
    dag->nodes_pos.push_back(id);
    return id;
}

NodeId add_node_at(Dag* dag, NodeId pos, bool rebuild_pos)
{
    NodeId id = dag->nodes.size();

    if (pos > id) {
        Rcpp::warning(
            "position %u exceeds number of nodes - no node added",
            pos
        );
        return id;
    }

    dag->nodes.emplace_back();
    dag->nodes_order.insert(dag->nodes_order.begin() + pos, id);
    dag->needs_pos_rebuild = true;

    if (rebuild_pos) {  // set this to false for efficient batch additions
        rebuild_nodes_pos(dag);
    }
    return id;
}

bool add_edge(
    Dag* dag,
    NodeId from,
    NodeId to,
    bool checkTopo,
    bool checkCycle
) {
    if (!has_node(dag, from)) {
        Rcpp::warning("node id %u not in DAG - operation ignored", from);
        return false;
    }
    if (!has_node(dag, to)) {
        Rcpp::warning("node id %u not in DAG - operation ignored", to);
        return false;
    }

    // Check if edge already exists
    const auto& out = dag->nodes[from].outgoing;
    bool hasEdge = std::find(out.begin(), out.end(), to) != out.end();
    if (hasEdge) {
        Rcpp::warning(
            "edge %u -> %u already exists - operation ignored",
            from, to
        );
        return false;
    }

    if (checkTopo) {
        if (dag->needs_pos_rebuild) {
            rebuild_nodes_pos(dag);
        }
        if (dag->nodes_pos[from] >= dag->nodes_pos[to]) {
            Rcpp::warning(
                "edge %u -> %u not in topological order and thus not added",
                from, to
            );
            return false;
        }
    }

    dag->nodes[from].outgoing.push_back(to);
    dag->nodes[to].incoming.push_back(from);

    if (checkCycle && is_cyclic(dag)) {
        // Undo edge addition and throw
        dag->nodes[from].outgoing.pop_back();
        dag->nodes[to].incoming.pop_back();
        Rcpp::warning(
            "adding edge %u -> %u creates a cycle - operation ignored",
            from, to
        );
        return false;
    }

    return true;
}


// ------
// Remove
// ------
void discard_node(Dag* dag, NodeId id, bool recursive)
{
    if (id >= dag->nodes.size()) return;
    dag->nodes[id].alive = false;
    if (recursive) {
        // TODO
    }
}
// void pop()

void remove_node(Dag* dag, NodeId id, bool recursive)
{
    if (id >= dag->nodes.size()) {
        std::cerr << "Invalid node ID: " << id << std::endl;
        return;
    }
    discard_node(dag, id, recursive);
}


// -------------
// House keeping
// -------------
void init_visitor_marking(Dag* dag)
{
    if (dag->visited.size() < dag->nodes.size()) {
        dag->visited.resize(dag->nodes.size(), 0);
    }
    // ++visitor_mark;
    ++dag->visitor_mark;

    // Handle wraparound
    if (dag->visitor_mark == 0) {
        std::fill(dag->visited.begin(), dag->visited.end(), 0);
        dag->visitor_mark = 1;
    }
}

void rebuild_nodes_pos(Dag* dag)
{
    dag->nodes_pos.resize(dag->nodes.size());
    for (NodeId i = 0; i < dag->nodes_order.size(); ++i) {
        dag->nodes_pos[dag->nodes_order[i]] = i;
    }
    dag->needs_pos_rebuild = false;
}

void tidy_up(Dag* dag)
{
    if (dag->needs_pos_rebuild) {
        rebuild_nodes_pos(dag);
    }
}


RCPP_MODULE(Dag){
    using namespace Rcpp;

    class_<Dag>("Dag")
    .default_constructor()

    // Inspect
    .const_method("size", &size)
    .const_method("has_edge", &has_edge)
    .const_method("has_node", &has_node)
    .const_method("get_nodes_order", &get_nodes_order)
    .const_method("get_nodes_pos", &get_nodes_pos)
    .method("get_downstream_nodes", &get_downstream_nodes)
    .method("get_upstream_nodes", &get_upstream_nodes)

    // Add
    .method("add_node", &add_node)
    .method("add_node_at", &add_node_at)
    .method("add_edge", &add_edge)

    // Remove

    // Helpers
    .method("tidy_up", &tidy_up)
    ;
}
