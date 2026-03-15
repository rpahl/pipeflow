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
    // Dag() : nodes() {} ;

    // Inspection
    uint32_t size() const { return nodes.size(); }
    std::vector<NodeId> get_topo_order() const { return topo_order; }

    // Add
    NodeId add_node() {
        uint32_t pos = this->size();
        Rcpp::Rcout << "add pos:" << pos << std::endl;
        return this->add_node_at(pos);
    }

    NodeId add_node_at(uint32_t pos) {
        NodeId id = this->size();
        Rcpp::Rcout << "node id:" << id << std::endl;
        if (pos > id) {
            throw Rcpp::exception("Invalid position");
        }
        nodes.emplace_back();
        topo_order.insert(topo_order.begin() + pos, id);
        // rebuild_topo_pos(*this);
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

    // Members
    std::vector<Node> nodes;            // nodes stored by running id

    // We keep the topological order of the nodes separately. This allows us
    // to later insert nodes by just modifying the order vector, that is,
    // without having to update all dependencies.
    std::vector<NodeId> topo_order;     // logical order in DAG
    std::vector<uint32_t> topo_pos;     // inverse mapping
    // std::vector<NodeId> free_ids;    // optional
};

int dag_size(Dag* dag) { return dag->nodes.size(); }


RCPP_MODULE(Dag){
    using namespace Rcpp;

    class_<Dag>("Dag")
    .default_constructor()

    // Inspection
    .const_method("size", &Dag::size)
    .const_method("get_topo_order", &Dag::get_topo_order)
    // .field("nodes", &Dag::nodes)
    // .field_readonly("topo_order", &Dag::topo_order)
    // .field_readonly("topo_pos", &Dag::topo_pos)

    // Add
    .method("add_node", &Dag::add_node)
    .method("add_node_at", &Dag::add_node_at)
    .method("add_edge", &Dag::add_edge)

    // Remove
    ;
}
