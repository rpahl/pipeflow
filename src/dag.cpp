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


class Dag {
public:
    // Inspection
    uint32_t size() const { return nodes.size(); }
    // TODO: maybe size() should be the number of alive nodes
    std::vector<NodeId> get_nodes_order() const { return nodes_order; }
    std::vector<NodeId> get_nodes_pos() const { return nodes_pos; }
    std::vector<NodeId> determine_downstream_nodes(NodeId id) {
        uint32_t len = nodes.size();
        if (id >= len) {
            throw Rcpp::exception("Invalid node ID");
        }
        if (!nodes[id].alive) {
            return {};
        }

        init_visitor_marking();

        std::vector<NodeId> result;
        work_stack.clear();
        work_stack.push_back(id);
        visited[id] = visitor_mark;

        while (!work_stack.empty()) {
            NodeId cur = work_stack.back();
            work_stack.pop_back();

            for (NodeId nxt : nodes[cur].outgoing) {
                if (!nodes[nxt].alive) continue;
                // TODO: evaluate if removing dead nodes from all incoming and
                // outgoing nodes could improve performance by avoiding the
                // above check entirely.

                if (visited[nxt] == visitor_mark) continue;

                visited[nxt] = visitor_mark;
                result.push_back(nxt);
                work_stack.push_back(nxt);
            }
        }

        std::sort(result.begin(), result.end(),
                [this](NodeId a, NodeId b) {
                    return nodes_pos[a] < nodes_pos[b];
                });

        return result;
    }

    // Add
    NodeId add_node() {
        NodeId id = nodes.size();
        nodes.emplace_back();
        nodes_order.push_back(id);
        nodes_pos.push_back(id);
        return id;
    }

    NodeId add_node_at(uint32_t pos) {
        NodeId id = nodes.size();
        if (pos > id) {
            throw Rcpp::exception("Invalid position");
        }
        nodes.emplace_back();
        nodes_order.insert(nodes_order.begin() + pos, id);
        rebuild_topo_pos();
        return id;
    }

    bool add_edge(NodeId from, NodeId to) {
        if (from >= nodes.size() || to >= nodes.size()) return false;
        nodes[to].incoming.push_back(from);
        nodes[from].outgoing.push_back(to);
        return true;
    }

    // Remove
    void discard_node(NodeId id, bool recursive = true) {
        if (id >= nodes.size()) return;
        nodes[id].alive = false;
        if (recursive) {
            // TODO
        }
    }
    // void pop()

    void remove_node(NodeId id, bool recursive = true) {
        if (id >= nodes.size()) {
            std::cerr << "Invalid node ID: " << id << std::endl;
            return;
        }
        this->discard_node(id, recursive);
    }

    // ---------
    // Internals
    // ---------
    void rebuild_topo_pos() {
        nodes_pos.resize(nodes.size());
        for (uint32_t i = 0; i < nodes_order.size(); ++i) {
            nodes_pos[nodes_order[i]] = i;
        }
    }

    void init_visitor_marking() {
        if (visited.size() < nodes.size()) {
            visited.resize(nodes.size(), 0);
        }
        ++visitor_mark;

        // Handle wraparound
        if (visitor_mark == 0) {
            std::fill(visited.begin(), visited.end(), 0);
            visitor_mark = 1;
        }
    }

    // -------
    // Members
    // -------
    std::vector<Node> nodes;            // nodes stored by running id

    // We keep the topological order of the nodes separately. This allows us
    // to later insert nodes by just modifying the order vector, that is,
    // without having to update all dependencies.
    std::vector<NodeId> nodes_order;     // logical order in DAG
    std::vector<NodeId> nodes_pos;       // inverse mapping
    // std::vector<NodeId> free_ids;    // optional

    // Caching/Memoization
    std::vector<uint16_t> visited;      // used to keep track of node visits
    uint16_t visitor_mark = 0;
    std::vector<NodeId> work_stack;
};

int dag_size(Dag* dag) { return dag->nodes.size(); }


RCPP_MODULE(Dag){
    using namespace Rcpp;

    class_<Dag>("Dag")
    .default_constructor()

    // Inspection
    .const_method("size", &Dag::size)
    .const_method("get_nodes_order", &Dag::get_nodes_order)
    .const_method("get_nodes_pos", &Dag::get_nodes_pos)

    // Add
    .method("add_node", &Dag::add_node)
    .method("add_node_at", &Dag::add_node_at)
    .method("add_edge", &Dag::add_edge)

    // Remove
    ;
}
