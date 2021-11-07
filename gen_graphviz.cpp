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
 * graphviz.c
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
    int target_state;
    // Don't free those for now, wait for RAII
    SymbolNode* labels;
    // Don't free those for now, wait for RAII
    SymbolNode* labels_tail;

    explicit GvNode(int target_state)
      : target_state(target_state)
      , labels(nullptr)
      , labels_tail(nullptr)
    {}
};

using GvNodeList = std::list<GvNode>;

static auto
find_gv_node_in_list(GvNodeList& list, int target_state) -> GvNode*
{
    for (auto& elem : list) {
        if (elem.target_state == target_state) {
            return &elem;
        }
    }
    return nullptr;
}

/*
 * find label in label list.
 */
static void
insert_label_to_list(GvNode* n, std::shared_ptr<SymbolTableNode> snode)
{
    if (n == nullptr || snode == nullptr)
        return; // should not happen.

    if (nullptr == n->labels) {
        n->labels = n->labels_tail = SymbolNode::create(snode);
    } else {
        SymbolNode* m = n->labels;
        while (m->next != nullptr) {
            if (m->snode == snode)
                return; // already exists.
            m = m->next;
        }
        // now m->next == nullptr
        if (m->snode == snode)
            return; // exists as the last one.
        // else, not exist.
        m->next = SymbolNode::create(snode);
    }
}

/*
 * If exists a node n in list, s.t.
 *   n->target_state == targetState, then add snode to n->label_list.
 * else
 *   create a new node and insert it to list.
 */
static void
add_gv_node_to_list(GvNodeList& list,
                    int target_state,
                    std::shared_ptr<SymbolTableNode> snode)
{
    GvNode* n = find_gv_node_in_list(list, target_state);
    if (n == nullptr) { // targetState NOT found. Add to list.
        if constexpr (DEBUG_GEN_GVIZ) {
            std::cout << "target state " << target_state << " not found (label "
                      << snode->symbol << ")" << std::endl;
        }
        auto new_node = GvNode(target_state);
        new_node.labels = SymbolNode::create(snode);
        new_node.labels_tail = new_node.labels;
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
dump_gv_node_list_r(const GvNodeList& list, int src_state, std::ostream& out)
{
    for (const auto& elem : list) {
        out << "  " << src_state << " -> r" << elem.target_state
            << " [ label=\"";
        // write label list
        SymbolNode* labels = elem.labels;
        for (; labels->next != nullptr; labels = labels->next) {
            out << *labels->snode->symbol << ",";
        }
        out << *labels->snode->symbol;
        out << R"(" style="dashed" ];)" << std::endl;
    }
}

/// dump s list
static void
dump_gv_node_list_s(const GvNodeList& list, int src_state, std::ostream& out)
{
    for (const auto& elem : list) {
        out << "  " << src_state << " -> " << elem.target_state
            << " [ label=\"";
        // write label list
        const SymbolNode* labels = elem.labels;
        for (; labels->next != nullptr; labels = labels->next) {
            out << *labels->snode->symbol << ",";
        }
        out << *labels->snode->symbol;
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
        const int state = r_list.front().target_state;
        r_list.clear();
        // "(any)" means: any terminal can cause reduction.
        add_gv_node_to_list(r_list, state, hash_tbl_insert("(any)"));
    }
}

/*
 * Has the same logic as printParsingTable() in y.c.
 * For O0, O1.
 */
void
gen_graphviz_input(const Grammar& grammar,
                   const std::string& y_gviz,
                   const Options& options)
{

    int row_size = ParsingTblRows;

    std::ofstream fp_gviz;
    fp_gviz.open(y_gviz);

    fp_gviz << "digraph abstract {" << std::endl
            << std::endl
            << "  node [shape = doublecircle]; 0 acc;" << std::endl
            << "  node [shape = circle];" << std::endl;

    for (int row = 0; row < row_size; row++) {
        GvNodeList r_list{};
        GvNodeList s_list{};

        for (int col = 0; col < ParsingTblCols; col++) {
            std::shared_ptr<SymbolTableNode> n = ParsingTblColHdr[col];
            if (!is_goal_symbol(grammar, n)) {
                auto [action, state] = get_action(n->type, col, row);
                /*std::cout  <<  action <<  state<< "\t"; */
                if (action == 0) {
                    /* do nothing */
                } else if (action == 'r') {
                    add_gv_node_to_list(r_list, state, n);
                } else if (action == 's' || action == 'g') {
                    add_gv_node_to_list(s_list, state, n);
                } else if (action == 'a') {
                    fp_gviz << " " << row << " -> acc [ label = \"" << n->symbol
                            << "\" ];" << std::endl;
                }
            } // end of if
        }     // end of for.

        update_r_list(r_list, s_list, options);

        dump_gv_node_list_r(r_list, row, fp_gviz);
        dump_gv_node_list_s(s_list, row, fp_gviz);
    }

    fp_gviz << std::endl << "}" << std::endl;
    fp_gviz.close();
}

/*
 * Has the same logic as printCondensedFinalParsingTable() in y.c.
 * For O2, O3.
 */
void
gen_graphviz_input2(const Grammar& grammar,
                    const std::string& y_gviz,
                    const Options& options)
{
    /* value assigned at the end of generate_parsing_table(). */
    int row_size = ParsingTblRows;

    std::ofstream fp_gviz;
    fp_gviz.open(y_gviz);

    fp_gviz << "digraph abstract {" << std::endl
            << std::endl
            << "  node [shape = doublecircle]; 0 acc;" << std::endl
            << "  node [shape = circle];" << std::endl;

    int i = 0;
    for (int row = 0; row < row_size; row++) {
        GvNodeList r_list{};
        GvNodeList s_list{};
        if (is_reachable_state(row)) {
            for (int col = 0; col < ParsingTblCols; col++) {
                std::shared_ptr<SymbolTableNode> n = ParsingTblColHdr[col];
                if (!is_goal_symbol(grammar, n) && !is_parent_symbol(n)) {
                    auto [action, state] = get_action(n->type, col, row);
                    if (action == 's' || action == 'g')
                        state = get_actual_state(state);
                    /* yyprintf("%c%d\t", action, state); */
                    if (action == 0) {
                        /* do nothing */
                    } else if (action == 'r') {
                        add_gv_node_to_list(r_list, state, n);
                    } else if (action == 's' || action == 'g') {
                        add_gv_node_to_list(s_list, state, n);
                    } else if (action == 'a') {
                        fp_gviz << " " << row << " -> acc [ label = \""
                                << n->symbol << "\" ];" << std::endl;
                    }
                }
            }
            i++;

            update_r_list(r_list, s_list, options);

            int src_state = get_actual_state(row);
            dump_gv_node_list_r(r_list, src_state, fp_gviz);
            dump_gv_node_list_s(s_list, src_state, fp_gviz);
        } /* end if */
    }

    fp_gviz << std::endl << "}" << std::endl;
    fp_gviz.close();
}
