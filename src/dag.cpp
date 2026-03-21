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

    // -------
    // Inspect
    // -------
    std::vector<NodeId> get_downstream_nodes(NodeId id)
    {
        if (!has_node(id)) {
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

        return result;
    }
    std::vector<NodeId> get_nodes_order() const { return nodes_order; }
    std::vector<NodeId> get_nodes_pos() const { return nodes_pos; }
    bool has_edge(NodeId from, NodeId to, bool warn = true) const
    {
        if (!has_node(from, warn) || !has_node(to, warn)) {
            return false;
        }
        const auto& out = nodes[from].outgoing;
        return std::find(out.begin(), out.end(), to) != out.end();
    }
    bool has_node(NodeId id, bool warn = true) const
    {
        if (id < nodes.size() && nodes[id].alive) {
            return true;
        }
        if (warn) {
            Rcpp::warning("node id %u not in DAG", id);
        }
        return false;
    }
    bool is_cyclic()
    {
        throw Rcpp::exception("is_cyclic not yet implemented");
    }
    uint32_t size() const { return nodes.size(); }
    // TODO: maybe size() should be the number of alive nodes

    // ---
    // Add
    // ---
    NodeId add_node()
    {
        NodeId id = nodes.size();
        nodes.emplace_back();
        nodes_order.push_back(id);
        nodes_pos.push_back(id);
        return id;
    }

    NodeId add_node_at(uint32_t pos)
    {
        NodeId id = nodes.size();

        if (pos > id) {
            Rcpp::warning(
                "position %u exceeds number of nodes - no node added",
                pos
            );
            return id;
        }

        nodes.emplace_back();
        nodes_order.insert(nodes_order.begin() + pos, id);
        rebuild_nodes_pos();
        return id;
    }

    bool add_edge(NodeId from, NodeId to, bool warn = true)
    {
        if (!has_node(from, warn) || !has_node(to, warn)) {
            return false;
        }

        // Check if edge already exists
        const auto& out = nodes[from].outgoing;
        bool hasEdge = std::find(out.begin(), out.end(), to) != out.end();
        if (hasEdge) {
            if (warn) {
                Rcpp::warning(
                    "edge %u -> %u already exists - operation ignored",
                    from, to
                );
            }
            return false;
        }

        nodes[from].outgoing.push_back(to);
        nodes[to].incoming.push_back(from);

        if (this->is_cyclic()) {
            // Undo edge addition and throw
            nodes[from].outgoing.pop_back();
            nodes[to].incoming.pop_back();
            Rcpp::warning(
                "adding edge %u -> %u creates a cycle - operation ignored",
                from, to
            );
            return false;
        }

        return true;
    }

    bool add_edge_in_order(NodeId from, NodeId to, bool warn = true)
    {
        if (!has_node(from, warn) || !has_node(to, warn)) {
            return false;
        }

        if (nodes_pos[from] >= nodes_pos[to]) {
            if (warn) {
                Rcpp::warning(
                    "edge %u -> %u not in topological order and thus not added",
                    from, to
                );
            }
            return false;
        }

        const auto& out = nodes[from].outgoing;
        bool hasEdge = std::find(out.begin(), out.end(), to) != out.end();
        if (hasEdge) {
            if (warn) {
                Rcpp::warning(
                    "edge %u -> %u already exists - operation ignored",
                    from, to
                );
            }
            return false;
        }

        nodes[from].outgoing.push_back(to);
        nodes[to].incoming.push_back(from);
        return true;
    }


    // ------
    // Remove
    // ------
    void discard_node(NodeId id, bool recursive = true)
    {
        if (id >= nodes.size()) return;
        nodes[id].alive = false;
        if (recursive) {
            // TODO
        }
    }
    // void pop()

    void remove_node(NodeId id, bool recursive = true)
    {
        if (id >= nodes.size()) {
            std::cerr << "Invalid node ID: " << id << std::endl;
            return;
        }
        this->discard_node(id, recursive);
    }


    // -------------
    // House keeping
    // -------------
    void rebuild_nodes_pos()
    {
        nodes_pos.resize(nodes.size());
        for (uint32_t i = 0; i < nodes_order.size(); ++i) {
            nodes_pos[nodes_order[i]] = i;
        }
    }

    void init_visitor_marking()
    {
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

    // We keep track of the topological nodes order. This allows us to later
    // insert nodes by just modifying the order vector, that is, without
    // having to update all dependencies, edges, etc ...
    std::vector<NodeId> nodes_order;    // order in which nodes appear in DAG
    std::vector<NodeId> nodes_pos;      /* position of nodes in the DAG, i.e.,
                                           inverse mapping of nodes_order */
    // std::vector<NodeId> free_ids;    // optional

    // Caching and memoization
    std::vector<uint16_t> visited;      // used to keep track of node visits
    uint16_t visitor_mark = 0;          // allows re-use of visited without reset
    std::vector<NodeId> work_stack;
};



RCPP_MODULE(Dag){
    using namespace Rcpp;

    class_<Dag>("Dag")
    .default_constructor()

    // Inspection
    .method("get_downstream_nodes", &Dag::get_downstream_nodes)
    .const_method("get_nodes_order", &Dag::get_nodes_order)
    .const_method("get_nodes_pos", &Dag::get_nodes_pos)
    .const_method("has_edge", &Dag::has_edge)
    .const_method("has_node", &Dag::has_node)
    .const_method("size", &Dag::size)

    // Add
    .method("add_node", &Dag::add_node)
    .method("add_node_at", &Dag::add_node_at)
    .method("add_edge_in_order", &Dag::add_edge_in_order)

    // Remove
    ;
}
