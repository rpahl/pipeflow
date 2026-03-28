// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <Rcpp.h>
// using namespace Rcpp;

using nodeId = uint32_t;
using nodeId_vec = std::vector<nodeId>;

struct Node {
    nodeId_vec incoming;
    nodeId_vec outgoing;
    bool alive = true;
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

// Copy
Dag* clone(const Dag* dag);

// Inspect
std::size_t size(const Dag* dag) { return dag->nodes_order.size(); }
nodeId get_min_id(const Dag* dag);
nodeId get_max_id(const Dag* dag);
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
nodeId add_node_at(Dag* dag, nodeId pos);
bool add_edge(Dag* dag, nodeId from, nodeId to,
    bool checkTopo = true, bool checkCycle = false
);
// nodeId add_dag(Dag* dag, const Dag* other); // returns new node id of other's root node

// Remove
bool remove_node(Dag* dag, nodeId id, bool force = false);
bool remove_edge(Dag* dag, nodeId from, nodeId to, bool force = false);

// Reshape
void tidy_up(Dag* dag);
nodeId_vec rebuild(Dag* dag);
void shift(Dag* dag, nodeId offset);

// Serialization
void print(Dag* dag, nodeId from);

// House keeping
void init_visitor_marking(Dag* dag);

// ---------------------------------------------------------------------------
// Method implementation
// ---------------------------------------------------------------------------

// ----
// Copy
// ----
Dag* clone(const Dag* dag)
{
    if (dag == nullptr) {
        Rcpp::warning("cannot clone null Dag pointer");
        return new Dag();
    }

    Dag* out = new Dag(*dag);
    out->work_stack.clear();
    return out;
}


// -------
// Inspect
// -------
nodeId get_min_id(const Dag* dag)
{
    if (dag->nodes_order.empty()) return 0;
    return *std::min_element(dag->nodes_order.begin(), dag->nodes_order.end());
}
nodeId get_max_id(const Dag* dag)
{
    if (dag->nodes_order.empty()) return 0;
    return *std::max_element(dag->nodes_order.begin(), dag->nodes_order.end());
}

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

nodeId add_node_at(Dag* dag, nodeId pos)
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

bool remove_edge(Dag* dag, nodeId from, nodeId to, bool force)
{
    if (!has_edge(dag, from, to)) {
        Rcpp::warning("edge %u -> %u not in DAG", from, to);
        return false;
    }

    Node& fromNode = dag->nodes[from];
    Node& toNode = dag->nodes[to];

    bool wouldBeDangling = toNode.incoming.size() < 2;
    if (wouldBeDangling && !force) {
        std::string info = "removing edge " +
            std::to_string(from) + " -> " + std::to_string(to) +
            " would leave downstream node " + std::to_string(to) +
            " dangling - use `force = true` to remove it anyway";
        Rcpp::warning(info.c_str());
        return false;
    }

    // Remove the edge
    fromNode.outgoing.erase(
        std::remove(fromNode.outgoing.begin(), fromNode.outgoing.end(), to),
        fromNode.outgoing.end()
    );
    toNode.incoming.erase(
        std::remove(toNode.incoming.begin(), toNode.incoming.end(), from),
        toNode.incoming.end()
    );

    return true;
}

// -------
// Reshape
// -------
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

nodeId_vec rebuild(Dag* dag)
{
    tidy_up(dag);

    nodeId_vec old_order = dag->nodes_order;
    const nodeId n = static_cast<nodeId>(old_order.size());
    const nodeId kInvalid = static_cast<nodeId>(-1);

    // old id -> new id
    nodeId_vec old_to_new(dag->nodes.size(), kInvalid);
    for (nodeId new_id = 0; new_id < n; ++new_id) {
        old_to_new[old_order[new_id]] = new_id;
    }

    std::vector<Node> new_nodes;
    new_nodes.reserve(n);

    for (nodeId new_id = 0; new_id < n; ++new_id) {
        nodeId old_id = old_order[new_id];
        const Node& old_node = dag->nodes[old_id];

        Node nn;
        nn.alive = true;

        nn.incoming.reserve(old_node.incoming.size());
        for (nodeId old_inc : old_node.incoming) {
            nn.incoming.push_back(old_to_new[old_inc]);
        }

        nn.outgoing.reserve(old_node.outgoing.size());
        for (nodeId old_out : old_node.outgoing) {
            nn.outgoing.push_back(old_to_new[old_out]);
        }
        new_nodes.push_back(std::move(nn));
    }

    dag->nodes = std::move(new_nodes);

    dag->nodes_order.resize(n);
    dag->nodes_pos.resize(n);
    for (nodeId i = 0; i < n; ++i) {
        dag->nodes_order[i] = i;
        dag->nodes_pos[i] = i;
    }

    dag->needs_order_rebuild = false;
    dag->needs_pos_rebuild = false;
    dag->visited.assign(n, 0);
    dag->visitor_mark = 0;
    dag->work_stack.clear();

    return old_order;
}

void shift(Dag* dag, nodeId offset)
{
    if (size(dag) == 0) {
        return;
    }

    for (auto& node : dag->nodes) {
        for (auto& inc : node.incoming) {
            inc += offset;
        }
        for (auto& out : node.outgoing) {
            out += offset;
        }
    }

    for (auto& id : dag->nodes_order) {
        id += offset;
    }

    for (auto& pos : dag->nodes_pos) {
        pos += offset;
    }
}


// -------------
// Serialization
// -------------
void print(Dag* dag, nodeId from)
{
    // Below implementation was taken from GPT-5.3-Codex
    if (!has_node(dag, from)) {
        Rcpp::warning("node id %u not in DAG", from);
        return;
    }

    nodeId_vec reachable = get_reachable_nodes_down(dag, {from}, true);
    if (reachable.empty()) {
        return;
    }

    std::vector<bool> is_reachable(dag->nodes.size(), false);
    for (nodeId id : reachable) {
        is_reachable[id] = true;
    }

    // lane index -> node id currently occupying that lane
    // free lane marker
    const nodeId kFree = static_cast<nodeId>(-1);

    // node id -> lane index, -1 means no lane assigned
    std::vector<int> lane_of(dag->nodes.size(), -1);
    nodeId_vec active_lanes;

    auto ensure_lane = [&](nodeId id) -> int {
        if (lane_of[id] >= 0) {
            return lane_of[id];
        }

        for (std::size_t i = 0; i < active_lanes.size(); ++i) {
            if (active_lanes[i] == kFree) {
                active_lanes[i] = id;
                lane_of[id] = static_cast<int>(i);
                return static_cast<int>(i);
            }
        }

        active_lanes.push_back(id);
        lane_of[id] = static_cast<int>(active_lanes.size() - 1);
        return lane_of[id];
    };

    auto in_child_lanes = [](const std::vector<int>& lanes, int lane) -> bool {
        return std::find(lanes.begin(), lanes.end(), lane) != lanes.end();
    };

    for (nodeId nid : reachable) {
        int cur_lane = ensure_lane(nid);

        nodeId_vec children;
        for (nodeId ch : dag->nodes[nid].outgoing) {
            if (is_reachable[ch]) {
                children.push_back(ch);
            }
        }

        // deterministic child order in topological position
        std::sort(children.begin(), children.end(), [&](nodeId a, nodeId b) {
            return dag->nodes_pos[a] < dag->nodes_pos[b];
        });

        // Render current node line
        std::string row;
        for (std::size_t i = 0; i < active_lanes.size(); ++i) {
            if (static_cast<int>(i) == cur_lane) {
                row += "* ";
            } else if (active_lanes[i] != kFree) {
                row += "| ";
            } else {
                row += "  ";
            }
        }

        row += std::to_string(nid);
        if (!children.empty()) {
            row += " -> [";
            for (std::size_t i = 0; i < children.size(); ++i) {
                if (i > 0) {
                    row += ", ";
                }
                row += std::to_string(children[i]);
            }
            row += "]";
        }

        Rcpp::Rcout << row << std::endl;

        // Update lanes after printing this row
        std::vector<int> child_lanes;
        if (children.empty()) {
            active_lanes[cur_lane] = kFree;
        } else {
            nodeId first = children[0];

            if (lane_of[first] < 0) {
                lane_of[first] = cur_lane;
                active_lanes[cur_lane] = first;
            } else if (lane_of[first] != cur_lane) {
                // child already has another active lane (merge)
                active_lanes[cur_lane] = kFree;
            } else {
                active_lanes[cur_lane] = first;
            }

            child_lanes.push_back(lane_of[first]);

            for (std::size_t i = 1; i < children.size(); ++i) {
                int lane = ensure_lane(children[i]);
                child_lanes.push_back(lane);
            }
        }

        // Optional connector row for forks/merges (git-like feel)
        bool needs_connector = children.size() > 1 ||
            (!children.empty() && child_lanes[0] != cur_lane);

        if (needs_connector) {
            std::string conn;
            for (std::size_t i = 0; i < active_lanes.size(); ++i) {
                bool draw = active_lanes[i] != kFree ||
                    in_child_lanes(child_lanes, static_cast<int>(i));
                conn += draw ? "| " : "  ";
            }
            Rcpp::Rcout << conn << std::endl;
        }

        // Trim trailing free lanes
        while (!active_lanes.empty() && active_lanes.back() == kFree) {
            active_lanes.pop_back();
        }
    }
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


RCPP_MODULE(Dag){
    using namespace Rcpp;

    class_<Dag>("Dag")
    .default_constructor()
    .const_method("clone", static_cast<Dag* (*)(const Dag*)>(&::clone))

    // Inspect
    .const_method("size", &size)
    .const_method("get_min_id", &get_min_id)
    .const_method("get_max_id", &get_max_id)
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
    .method("remove_edge", &remove_edge)

    // Reshape
    .method("tidy_up", &tidy_up)
    .method("rebuild", &rebuild)
    .method("shift", &shift)

    // Serialization
    .method("print", &print)
    ;
}
