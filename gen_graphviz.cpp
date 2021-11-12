/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007 Xin Chen. chenx@hawaii.edu

   Hyacc is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Hyacc is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Hyacc; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

/*
 * graphviz.cpp
 *
 * To produce an input file for graphviz.
 *
 * @author: Xin Chen
 * @created on: 12/14/2007
 * @last modified: 12/15/2007
 */

#include "y.hpp"
#include <fstream>
#include <iostream>
#include <list>
#include <ostream>
#include <string_view>

constexpr bool DEBUG_GEN_GVIZ = false;

struct GvNode
{
    StateHandle target_state;
    std::list<SymbolNode> labels{};

    explicit GvNode(StateHandle target_state)
      : target_state(target_state)
    {}
};

using GvNodeList = std::list<GvNode>;

static auto
find_gv_node_in_list(GvNodeList& list, StateHandle target_state) -> GvNode*
{
    for (auto& elem : list) {
        if (elem.target_state == target_state) {
            return &elem;
        }
    }
    return nullptr;
}

/// Find label in label list.
static void
insert_label_to_list(GvNode* n, std::shared_ptr<SymbolTableNode> snode)
{
    if (n == nullptr || snode == nullptr)
        return; // should not happen.

    if (n->labels.empty()) {
        n->labels.emplace_back(snode);
    } else {
        for (const auto& label : n->labels) {
            if (label.snode == snode) {
                return; // already exists.
            }
        }
        n->labels.emplace_back(snode);
    }
}

/// If exists a node n in list, s.t.
///   n->target_state == targetState, then add snode to n->label_list.
/// else
///   create a new node and insert it to list.
static void
add_gv_node_to_list(GvNodeList& list,
                    const StateHandle target_state,
                    std::shared_ptr<SymbolTableNode> snode)
{
    GvNode* n = find_gv_node_in_list(list, target_state);
    if (n == nullptr) { // targetState NOT found. Add to list.
        if constexpr (DEBUG_GEN_GVIZ) {
            std::cout << "target state " << target_state << " not found (label "
                      << *snode->symbol << ")" << std::endl;
        }
        auto new_node = GvNode(target_state);
        new_node.labels.emplace_back(snode);
        list.push_back(new_node);
    } else { // found.
             // add snode to the label list of n.
        if constexpr (DEBUG_GEN_GVIZ) {
            std::cout << "target state " << target_state << " found, add label "
                      << *snode->symbol << std::endl;
        }
        insert_label_to_list(n, snode);
    }
}

/// dump r list
static void
dump_gv_node_list_r(const GvNodeList& list,
                    const StateHandle src_state,
                    std::ostream& out)
{
    for (const auto& elem : list) {
        out << "  " << src_state << " -> r" << elem.target_state
            << " [ label=\"";
        // write label list
        bool first_label = true;
        for (const auto& label : elem.labels) {
            if (first_label) {
                first_label = false;
            } else {
                out << ",";
            }
            out << *label.snode->symbol;
        }
        out << R"(" style="dashed" ];)" << std::endl;
    }
}

/// dump s list
static void
dump_gv_node_list_s(const GvNodeList& list,
                    const StateHandle src_state,
                    std::ostream& out)
{
    for (const auto& elem : list) {
        out << "  " << src_state << " -> " << elem.target_state
            << " [ label=\"";
        // write label list
        bool first_label = true;
        for (const auto& label : elem.labels) {
            if (first_label) {
                first_label = false;
            } else {
                out << ",";
            }
            out << *label.snode->symbol;
        }
        out << "\" ];" << std::endl;
    }
}

/// Update the reduction list if the following is satisfied:
///
/// In case of --lr0 or --lalr:
///   If a reduction is the single only REDUCTION at this state,
///   replace it by ".". Similar to the use of final_state_list
///   in writing y.output. 3-11-2008.
/// Else (LR(1)):
///   If a reduction is the single only ACTION at this state,
///   do the same as above.
static auto
update_r_list(GvNodeList& r_list,
              const GvNodeList& s_list,
              const Options& options)
{
    if (((options.use_lr0 || options.use_lalr) && r_list.size() == 1) ||
        (s_list.empty() && r_list.size() == 1)) {
        const StateHandle state = r_list.front().target_state;
        r_list.clear();
        // "(any)" means: any terminal can cause reduction.
        add_gv_node_to_list(r_list, state, hash_tbl_insert("(any)"));
    }
}

/// Has the same logic as printParsingTable() in y.c.
/// For O0, O1.
void
gen_graphviz_input(const Grammar& grammar,
                   const std::string& y_gviz,
                   const Options& options)
{
    std::ofstream fp_gviz;
    fp_gviz.open(y_gviz);

    fp_gviz << "digraph abstract {" << std::endl
            << std::endl
            << "  node [shape = doublecircle]; 0 acc;" << std::endl
            << "  node [shape = circle];" << std::endl;

    for (size_t row = 0; row < ParsingTblRows; row++) {
        GvNodeList r_list{};
        GvNodeList s_list{};

        for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
            std::shared_ptr<SymbolTableNode> n = ParsingTblColHdr[col];
            if (!is_goal_symbol(grammar, n)) {
                auto [action, state] = get_action(n->type, col, row);
                // std::cout  <<  action <<  state<< "\t";
                if (!action.has_value()) {
                    // do nothing
                } else if (action == Action::Reduce) {
                    add_gv_node_to_list(r_list, state, n);
                } else if (action == Action::Shift || action == Action::Goto) {
                    add_gv_node_to_list(s_list, state, n);
                } else if (action == Action::Accept) {
                    fp_gviz << " " << row << " -> acc [ label = \""
                            << *n->symbol << "\" ];" << std::endl;
                }
            }
        }

        update_r_list(r_list, s_list, options);

        dump_gv_node_list_r(r_list, row, fp_gviz);
        dump_gv_node_list_s(s_list, row, fp_gviz);
    }

    fp_gviz << std::endl << "}" << std::endl;
    fp_gviz.close();
}

/// Has the same logic as printCondensedFinalParsingTable() in y.c.
/// For O2, O3.
void
gen_graphviz_input2(const Grammar& grammar,
                    const std::string& y_gviz,
                    const Options& options)
{
    std::ofstream fp_gviz;
    fp_gviz.open(y_gviz);

    fp_gviz << "digraph abstract {" << std::endl
            << std::endl
            << "  node [shape = doublecircle]; 0 acc;" << std::endl
            << "  node [shape = circle];" << std::endl;

    int i = 0;
    for (size_t row = 0; row < ParsingTblRows; row++) {
        GvNodeList r_list{};
        GvNodeList s_list{};
        if (is_reachable_state(row)) {
            for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
                std::shared_ptr<SymbolTableNode> n = ParsingTblColHdr[col];
                if (!is_goal_symbol(grammar, n) && !is_parent_symbol(n)) {
                    auto [action, state] = get_action(n->type, col, row);
                    if (action == Action::Shift || action == Action::Goto)
                        state = *get_actual_state(state);
                    // yyprintf("%c%d\t", action, state);
                    if (!action.has_value()) {
                        // do nothing
                    } else if (action == Action::Reduce) {
                        add_gv_node_to_list(r_list, state, n);
                    } else if (action == Action::Shift ||
                               action == Action::Goto) {
                        add_gv_node_to_list(s_list, state, n);
                    } else if (action == Action::Accept) {
                        fp_gviz << " " << row << " -> acc [ label = \""
                                << *n->symbol << "\" ];" << std::endl;
                    }
                }
            }
            i++;

            update_r_list(r_list, s_list, options);

            StateHandle src_state = *get_actual_state(row);
            dump_gv_node_list_r(r_list, src_state, fp_gviz);
            dump_gv_node_list_s(s_list, src_state, fp_gviz);
        }
    }

    fp_gviz << std::endl << "}" << std::endl;
    fp_gviz.close();
}
