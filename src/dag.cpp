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

    // We keep track of the topological order of nodes in the DAG, which
    // allows to insert nodes in between by just modifying this vector,
    // that is, without having to update all dependencies, edges, etc
    nodeId_vec nodes_order;

    // Caching and memoization
    std::vector<uint16_t> visited;      // used to keep track of node visits
    uint16_t visitor_mark = 0;          // allows repeated re-use of visited
    nodeId_vec work_stack;

    // Dag state
    bool needs_order_rebuild = false;
};


// ---------------------------------------------------------------------------
// Method declaration
// ---------------------------------------------------------------------------

// Node methods
// ------------
bool is_dangling_node(const Node& node);
void shift_node(Node& node, nodeId offset);

// Copy
// ----
Dag* clone(const Dag* dag);

// Inspect
// -------
std::size_t size(const Dag* dag) { return dag->nodes_order.size(); }
nodeId get_min_id(const Dag* dag);
nodeId get_max_id(const Dag* dag);
bool has_edge(const Dag* dag, nodeId from, nodeId to);
bool has_node(const Dag* dag, nodeId id);
bool has_dangling_node(const Dag* dag);
bool is_cyclic(const Dag* dag);
bool is_tidy(const Dag* dag);
nodeId_vec get_outgoing(const Dag* dag, nodeId id);
nodeId_vec get_incoming(const Dag* dag, nodeId id);
nodeId_vec get_nodes_order(const Dag* dag);
nodeId_vec get_dangling_nodes(const Dag* dag);

template <nodeId_vec Node::*EdgesMember>
nodeId_vec get_reachable_nodes(Dag* dag, const nodeId_vec& start_ids);
nodeId_vec get_reachable_nodes_down(Dag* dag, const nodeId_vec& start_ids);
nodeId_vec get_reachable_nodes_up(Dag* dag, const nodeId_vec& start_ids);

// Add
// ---
nodeId add_node(Dag* dag);
nodeId add_node_at(Dag* dag, nodeId pos);
bool add_edge(Dag* dag, nodeId from, nodeId to, bool checkCycle = false);
nodeId_vec add_dag(Dag* dag, Dag* other);

// Remove
// ------
bool remove_node(Dag* dag, nodeId id, bool force = false);
bool remove_edge(Dag* dag, nodeId from, nodeId to, bool force = false);

// Reshape
// -------
void tidy_up(Dag* dag);
nodeId_vec rebuild(Dag* dag);
void shift(Dag* dag, nodeId offset);

// Internal state
// --------------
void init_visitor_marking(Dag* dag);

// ---------------------------------------------------------------------------
// Method implementation
// ---------------------------------------------------------------------------

// Node methods
// ------------
bool is_dangling_node(const Node& node)
{
    return node.alive && node.incoming.empty();
}

void shift_node(Node& node, nodeId offset)
{
    if (offset == 0) return;

    for (auto& inc : node.incoming) {
        inc += offset;
    }
    for (auto& out : node.outgoing) {
        out += offset;
    }
}

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

bool is_tidy(const Dag* dag)
{
    return !dag->needs_order_rebuild;
}

nodeId_vec get_outgoing(const Dag* dag, nodeId id)
{
    return dag->nodes[id].outgoing;
}
nodeId_vec get_incoming(const Dag* dag, nodeId id)
{
    return dag->nodes[id].incoming;
}
nodeId_vec get_nodes_order(const Dag* dag) { return dag->nodes_order; }
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
nodeId_vec get_reachable_nodes(Dag* dag, const nodeId_vec& start_ids)
{
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

    return result;
}
nodeId_vec get_reachable_nodes_down(Dag* dag, const nodeId_vec& start_ids)
{
    return get_reachable_nodes<&Node::outgoing>(dag, start_ids);
}
nodeId_vec get_reachable_nodes_up(Dag* dag, const nodeId_vec& start_ids)
{
    return get_reachable_nodes<&Node::incoming>(dag, start_ids);
}


// Add
// ---
nodeId add_node(Dag* dag)
{
    nodeId id = dag->nodes.size();
    dag->nodes.emplace_back();
    dag->nodes_order.push_back(id);
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

    return id;
}

bool add_edge(Dag* dag, nodeId from, nodeId to, bool checkCycle)
{
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

nodeId_vec add_dag(Dag* dag, Dag* other)
{
    if (other == nullptr) {
        Rcpp::warning("cannot add null Dag pointer");
        return {};
    }
    if (size(other) == 0) {
        return {};
    }

    nodeId offset = 0;
    if (size(dag) > 0) {
        offset = get_max_id(dag) + 1 - get_min_id(other);
    }

    Dag* copy = clone(other);
    shift(copy, offset);

    // Ensure id == index invariant on target storage
    if (dag->nodes.size() < copy->nodes.size()) {
        dag->nodes.resize(copy->nodes.size());
    }

    for (nodeId id : copy->nodes_order) {
        dag->nodes[id] = copy->nodes[id];
        dag->nodes_order.push_back(id);
    }

    nodeId_vec result = copy->nodes_order;
    delete copy;
    return result;
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
        dag->needs_order_rebuild = false;
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
    for (nodeId i = 0; i < n; ++i) {
        dag->nodes_order[i] = i;
    }

    dag->needs_order_rebuild = false;
    dag->visited.assign(n, 0);
    dag->visitor_mark = 0;
    dag->work_stack.clear();

    return old_order;
}

void shift(Dag* dag, nodeId offset)
{
    if (size(dag) == 0 || offset == 0) {
        return;
    }

    // Shift all edge references
    for (auto& node : dag->nodes) {
        // if (!node.alive) continue;
        shift_node(node, offset);
    }

    // Prepend `offset` dead padding nodes so that
    // id == index invariant holds after shifting.
    std::vector<Node> padded(offset);
    for (auto& n : padded) {
        n.alive = false;
    }
    padded.reserve(offset + dag->nodes.size());
    for (auto& node : dag->nodes) {
        padded.push_back(std::move(node));
    }

    dag->nodes = std::move(padded);

    // Shift order vector
    for (auto& id : dag->nodes_order) {
        id += offset;
    }
}


// Internal state
// --------------
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


// -------------------
// Rcpp export helpers
// --------------------

static Dag* cast_sexp_to_dag_ptr(SEXP dp)
{
    if (TYPEOF(dp) != EXTPTRSXP) {
        Rcpp::stop("expected external pointer to Dag");
    }
    Dag* dag = static_cast<Dag*>(R_ExternalPtrAddr(dp));
    if (dag == nullptr) {
        Rcpp::stop("null Dag pointer");
    }
    return dag;
}

static Rcpp::IntegerVector as_int_vec(const nodeId_vec& x)
{
    Rcpp::IntegerVector out(x.size());
    for (std::size_t i = 0; i < x.size(); ++i) {
        out[i] = static_cast<int>(x[i]);
    }
    return out;
}

static void dag_finalizer(SEXP dp)
{
    Dag* dag = static_cast<Dag*>(R_ExternalPtrAddr(dp));
    if (dag != nullptr) {
        delete dag;
        R_ClearExternalPtr(dp);
    }
}

static nodeId_vec as_node_vec(const Rcpp::IntegerVector& x)
{
    nodeId_vec out;
    out.reserve(x.size());
    for (int id : x) {
        out.push_back(static_cast<nodeId>(id));
    }
    return out;
}


// Rcpp exports
// ------------

// [[Rcpp::export]]
SEXP dag_new()
{
    Dag* dag = new Dag();
    SEXP dp = PROTECT(R_MakeExternalPtr(dag, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(dp, dag_finalizer, TRUE);
    UNPROTECT(1);
    return dp;
}

// [[Rcpp::export]]
SEXP dag_clone(SEXP dp)
{
    Dag* dag = cast_sexp_to_dag_ptr(dp);
    Dag* out = clone(dag);
    SEXP out_dp = PROTECT(R_MakeExternalPtr(out, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(out_dp, dag_finalizer, TRUE);
    UNPROTECT(1);
    return out_dp;
}


// Inspect
// -------

// [[Rcpp::export]]
int dag_size(SEXP dp)
{
    return static_cast<int>(size(cast_sexp_to_dag_ptr(dp)));
}

// [[Rcpp::export]]
int dag_get_min_id(SEXP dp)
{
    return static_cast<int>(get_min_id(cast_sexp_to_dag_ptr(dp)));
}

// [[Rcpp::export]]
int dag_get_max_id(SEXP dp)
{
    return static_cast<int>(get_max_id(cast_sexp_to_dag_ptr(dp)));
}

// [[Rcpp::export]]
bool dag_has_edge(SEXP dp, int from, int to)
{
    return has_edge(
        cast_sexp_to_dag_ptr(dp),
        static_cast<nodeId>(from),
        static_cast<nodeId>(to)
    );
}

// [[Rcpp::export]]
bool dag_has_node(SEXP dp, int id)
{
    return has_node(
        cast_sexp_to_dag_ptr(dp),
        static_cast<nodeId>(id)
    );
}

// [[Rcpp::export]]
bool dag_has_dangling_node(SEXP dp)
{
    return has_dangling_node(cast_sexp_to_dag_ptr(dp));
}

// [[Rcpp::export]]
bool dag_is_cyclic(SEXP dp)
{
    return is_cyclic(cast_sexp_to_dag_ptr(dp));
}

// [[Rcpp::export]]
bool dag_is_tidy(SEXP dp)
{
    return is_tidy(cast_sexp_to_dag_ptr(dp));
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_get_outgoing(SEXP dp, int id)
{
    return as_int_vec(
        get_outgoing(cast_sexp_to_dag_ptr(dp), static_cast<nodeId>(id))
    );
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_get_incoming(SEXP dp, int id)
{
    return as_int_vec(
        get_incoming(cast_sexp_to_dag_ptr(dp), static_cast<nodeId>(id))
    );
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_get_nodes_order(SEXP dp)
{
    return as_int_vec(
        get_nodes_order(cast_sexp_to_dag_ptr(dp))
    );
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_get_dangling_nodes(SEXP dp)
{
    return as_int_vec(
        get_dangling_nodes(cast_sexp_to_dag_ptr(dp))
    );
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_get_reachable_nodes_down(
    SEXP dp,
    const Rcpp::IntegerVector& start_ids
) {
    return as_int_vec(
        get_reachable_nodes_down(
            cast_sexp_to_dag_ptr(dp),
            as_node_vec(start_ids)
        )
    );
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_get_reachable_nodes_up(
    SEXP dp,
    const Rcpp::IntegerVector& start_ids
) {
    return as_int_vec(
        get_reachable_nodes_up(
            cast_sexp_to_dag_ptr(dp),
            as_node_vec(start_ids)
        )
    );
}

// Add
// ---

// [[Rcpp::export]]
int dag_add_node(SEXP dp)
{
    return static_cast<int>(add_node(cast_sexp_to_dag_ptr(dp)));
}

// [[Rcpp::export]]
int dag_add_node_at(SEXP dp, int pos)
{
    return static_cast<int>(
        add_node_at(cast_sexp_to_dag_ptr(dp), static_cast<nodeId>(pos))
    );
}

// [[Rcpp::export]]
bool dag_add_edge(SEXP dp, int from, int to)
{
    return add_edge(
        cast_sexp_to_dag_ptr(dp),
        static_cast<nodeId>(from),
        static_cast<nodeId>(to),
        false
    );
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_add_dag(SEXP dp, SEXP other_dp)
{
    return as_int_vec(
        add_dag(cast_sexp_to_dag_ptr(dp), cast_sexp_to_dag_ptr(other_dp))
    );
}

// Remove
// ------

// [[Rcpp::export]]
bool dag_remove_node(SEXP dp, int id, bool force = false)
{
    return remove_node(
        cast_sexp_to_dag_ptr(dp), static_cast<nodeId>(id), force
    );
}

// [[Rcpp::export]]
bool dag_remove_edge(SEXP dp, int from, int to, bool force = false)
{
    return remove_edge(
        cast_sexp_to_dag_ptr(dp),
        static_cast<nodeId>(from), static_cast<nodeId>(to),
        force
    );
}


// Reshape
// -------

// [[Rcpp::export]]
void dag_tidy_up(SEXP dp)
{
    tidy_up(cast_sexp_to_dag_ptr(dp));
}

// [[Rcpp::export]]
Rcpp::IntegerVector dag_rebuild(SEXP dp)
{
    return as_int_vec(rebuild(cast_sexp_to_dag_ptr(dp)));
}

// [[Rcpp::export]]
void dag_shift(SEXP dp, int offset)
{
    shift(cast_sexp_to_dag_ptr(dp), static_cast<nodeId>(offset));
}
