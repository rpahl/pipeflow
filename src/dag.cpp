// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <Rcpp.h>
// using namespace Rcpp;

using nodeId = uint32_t;
using nodeId_vec = std::vector<nodeId>;

struct Node {
    nodeId_vec incoming;
    nodeId_vec outgoing;
    bool alive = true;
    bool dirty = false;
};


struct Dag {
    std::vector<Node> nodes;            // nodes stored by running id

    // We keep track of the topological order and position of nodes in the DAG,
    // which allows to insert nodes in between by just modifying these vectors,
    // that is, without having to update all dependencies, edges, etc
    nodeId_vec nodes_pos;               // basically inverse mapping of order
    nodeId_vec nodes_order;             // order of nodes

    // Caching and memoization
    std::vector<uint16_t> visited;      // used to keep track of node visits
    uint16_t visitor_mark = 0;          // allows repeated re-use of visited
    nodeId_vec work_stack;

    // Dag state
    bool needs_pos_rebuild = false;
    bool needs_order_rebuild = false;
};

// ---------------------------------------------------------------------------
// Method declaration
// ---------------------------------------------------------------------------

// Inspect
std::size_t size(const Dag* dag);
bool has_edge(const Dag* dag, nodeId from, nodeId to);
bool has_node(const Dag* dag, nodeId id);
bool has_dangling_node(const Dag* dag);
bool is_cyclic(const Dag* dag);
bool is_dangling_node(const Node& node);
bool is_tidy(const Dag* dag);
nodeId_vec get_nodes_order(const Dag* dag);
nodeId_vec get_nodes_pos(const Dag* dag);
nodeId_vec get_dangling_nodes(const Dag* dag);

template <nodeId_vec Node::*EdgesMember>
nodeId_vec get_reachable_nodes(
    Dag* dag, const nodeId_vec& start_ids, bool inTopoOrder = false
);
nodeId_vec get_reachable_nodes_down(
    Dag* dag, const nodeId_vec& start_ids, bool inTopoOrder = false
);
nodeId_vec get_reachable_nodes_up(
    Dag* dag, const nodeId_vec& start_ids, bool inTopoOrder = false
);

// Add
nodeId add_node(Dag* dag);
nodeId add_node_at(Dag* dag, nodeId pos, bool stayTidy = true);
bool add_edge(Dag* dag, nodeId from, nodeId to,
    bool checkTopo = true, bool checkCycle = false
);
nodeId append_dag(Dag* dag, const Dag* other); // returns new node id of other's root node

// Remove
bool remove_node(Dag* dag, nodeId id, bool force = false);


// Helpers for internal state
void init_visitor_marking(Dag* dag);
void tidy_up(Dag* dag);

// ---------------------------------------------------------------------------
// Method implementation
// ---------------------------------------------------------------------------

// -------
// Inspect
// -------
std::size_t size(const Dag* dag) { return dag->nodes_order.size(); }

bool has_edge(const Dag* dag, nodeId from, nodeId to)
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

bool has_node(const Dag* dag, nodeId id)
{
    return id < dag->nodes.size() && dag->nodes[id].alive;
}

bool has_dangling_node(const Dag* dag)
{
    if (dag->nodes_order.size() < 2) return false;

    for (std::size_t i = 1; i < dag->nodes_order.size(); ++i) {
        nodeId nid = dag->nodes_order[i];
        if (is_dangling_node(dag->nodes[nid])) {
            return true;
        }
    }
    return false;
}

bool is_cyclic(const Dag* dag)
{
    throw Rcpp::exception("is_cyclic not yet implemented");
}

bool is_dangling_node(const Node& node)
{
    return node.alive && node.incoming.empty();
}

bool is_tidy(const Dag* dag)
{
    return !dag->needs_pos_rebuild && !dag->needs_order_rebuild;
}

nodeId_vec get_nodes_order(const Dag* dag) { return dag->nodes_order; }
nodeId_vec get_nodes_pos(const Dag* dag) { return dag->nodes_pos; }
nodeId_vec get_dangling_nodes(const Dag* dag)
{
    if (dag->nodes_order.size() < 2) return {};

    nodeId_vec dangling;
    for (std::size_t i = 1; i < dag->nodes_order.size(); ++i) {
        nodeId nid = dag->nodes_order[i];
        if (is_dangling_node(dag->nodes[nid])) {
            dangling.push_back(nid);
        }
    }

    return dangling;
}

// As the only difference in the implementation of reaching nodes is whether
// we traverse outgoing or incoming edges, we use a template parameter to
// specify which member of the Node struct to use.
template <nodeId_vec Node::*EdgesMember>
nodeId_vec get_reachable_nodes(
    Dag* dag, const nodeId_vec& start_ids, bool inTopoOrder
) {
    init_visitor_marking(dag);
    std::vector<nodeId> result;
    dag->work_stack.clear();

    for (nodeId id : start_ids) {
        if (!has_node(dag, id)) {
            Rcpp::warning("node id %u not in DAG", id);
            continue;
        }
        if (dag->visited[id] == dag->visitor_mark) continue;
        dag->visited[id] = dag->visitor_mark;
        dag->work_stack.push_back(id);
        result.push_back(id);
    }

    while (!dag->work_stack.empty()) {
        nodeId cur = dag->work_stack.back();
        dag->work_stack.pop_back();
        const auto& neighbors = dag->nodes[cur].*EdgesMember;

        for (nodeId nxt : neighbors) {
            if (dag->visited[nxt] == dag->visitor_mark) continue;

            dag->visited[nxt] = dag->visitor_mark;
            dag->work_stack.push_back(nxt);
            result.push_back(nxt);
        }
    }

    if (inTopoOrder) {
        tidy_up(dag);  // Ensure internal state is consistent

        // Sort in topological order
        std::sort(result.begin(), result.end(), [&](nodeId a, nodeId b) {
            return dag->nodes_pos[a] < dag->nodes_pos[b];
        });
    }

    return result;
}
nodeId_vec get_reachable_nodes_down(
    Dag* dag, const nodeId_vec& start_ids, bool inTopoOrder
) {
    return get_reachable_nodes<&Node::outgoing>(dag, start_ids, inTopoOrder);
}
nodeId_vec get_reachable_nodes_up(
    Dag* dag, const nodeId_vec& start_ids, bool inTopoOrder
) {
    return get_reachable_nodes<&Node::incoming>(dag, start_ids, inTopoOrder);
}


// ---
// Add
// ---
nodeId add_node(Dag* dag)
{
    nodeId id = dag->nodes.size();
    dag->nodes.emplace_back();
    dag->nodes_order.push_back(id);
    dag->nodes_pos.push_back(id);
    return id;
}

nodeId add_node_at(Dag* dag, nodeId pos, bool stayTidy)
{
    nodeId id = dag->nodes.size();

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

    if (stayTidy) {
        tidy_up(dag);
    }
    return id;
}

bool add_edge(
    Dag* dag,
    nodeId from,
    nodeId to,
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
        tidy_up(dag);
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
bool kill_node(Dag* dag, nodeId id)
{
    if (!has_node(dag, id)) {
        Rcpp::warning("node id %u not in DAG", id);
        return false;
    }
    auto& node = dag->nodes[id];

    // Cut all incoming edges
    for (nodeId incId : node.incoming) {
        auto& incomingNode = dag->nodes[incId];
        incomingNode.outgoing.erase(
            std::remove(
                incomingNode.outgoing.begin(),
                incomingNode.outgoing.end(),
                id
            ),
            incomingNode.outgoing.end()
        );
    }

    // Cut all outgoing edges
    for (nodeId outId : node.outgoing) {
        auto& outgoingNode = dag->nodes[outId];
        outgoingNode.incoming.erase(
            std::remove(
                outgoingNode.incoming.begin(),
                outgoingNode.incoming.end(),
                id
            ),
            outgoingNode.incoming.end()
        );
    }

    node.alive = false;
    dag->needs_order_rebuild = true;
    dag->needs_pos_rebuild = true;
    return true;
}

bool remove_node(Dag* dag, nodeId id, bool force)
{
    if (!has_node(dag, id)) {
        Rcpp::warning("node id %u not in DAG", id);
        return false;
    }

    Node& node = dag->nodes[id];
    bool hasOutgoing = node.outgoing.size() > 0;

    if (force || !hasOutgoing) {
        return kill_node(dag, id);
    }

    for (nodeId nid : node.outgoing) {
        // Each downstream node needs at least two incoming edges to not dangle.
        bool wouldBeDangling = dag->nodes[nid].incoming.size() < 2;
        if (wouldBeDangling) {
            std::string info = "removing node " + std::to_string(id) +
                " would leave downstream node " + std::to_string(nid) +
                " dangling - use `force = true` to remove it anyway";
            Rcpp::warning(info.c_str());
            return false;
        }
    }

    return kill_node(dag, id);
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

void tidy_up(Dag* dag)
{
    if (dag->needs_order_rebuild) {
        // Remove any dead nodes
        auto& order = dag->nodes_order;
        order.erase(
            std::remove_if(order.begin(), order.end(),
                [&](nodeId id) { return !dag->nodes[id].alive; }),
            order.end()
        );
        dag->needs_pos_rebuild = true;
        dag->needs_order_rebuild = false;
    }

    if (dag->needs_pos_rebuild) {
        dag->nodes_pos.resize(dag->nodes_order.size());
        for (nodeId i = 0; i < dag->nodes_order.size(); ++i) {
            dag->nodes_pos[dag->nodes_order[i]] = i;
        }
        dag->needs_pos_rebuild = false;
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
    .const_method("has_dangling_node", &has_dangling_node)
    .const_method("get_nodes_order", &get_nodes_order)
    .const_method("get_nodes_pos", &get_nodes_pos)
    .const_method("get_dangling_nodes", &get_dangling_nodes)
    .const_method("is_tidy", &is_tidy)
    .method("get_reachable_nodes_down", &get_reachable_nodes_down)
    .method("get_reachable_nodes_up", &get_reachable_nodes_up)

    // Add
    .method("add_node", &add_node)
    .method("add_node_at", &add_node_at)
    .method("add_edge", &add_edge)

    // Remove
    .method("remove_node", &remove_node)

    // Helpers
    .method("tidy_up", &tidy_up)
    ;
}
