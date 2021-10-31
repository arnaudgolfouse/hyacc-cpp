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
#include <ostream>

constexpr bool DEBUG_GEN_GVIZ = false;

struct GvNode
{
    int target_state;
    SymbolNode* labels;
    SymbolNode* labels_tail;
    GvNode* next;
};

static auto
create_gv_node(int target_state) -> GvNode*
{
    auto* gvn = new GvNode;
    gvn->target_state = target_state;
    gvn->labels = nullptr;
    gvn->labels_tail = nullptr;
    gvn->next = nullptr;
    return gvn;
}

static void
destroy_gv_node(GvNode* n)
{
    if (nullptr == n)
        return;
    free_symbol_node_list(n->labels);
    delete n;
}

static void
destroy_gv_node_list(GvNode* list)
{
    GvNode* tmp = nullptr;
    if (list == nullptr)
        return;
    while ((tmp = list) != nullptr) {
        list = list->next;
        destroy_gv_node(tmp);
    }
}

static auto
find_gv_node_in_list(GvNode* list, int target_state) -> GvNode*
{
    if (nullptr == list)
        return nullptr;
    while (list != nullptr) {
        if (list->target_state == target_state)
            return list;
        list = list->next;
    }
    return nullptr;
}

/*
 * find label in label list.
 */
static void
insert_label_to_list(GvNode* n, SymbolTblNode* snode)
{
    if (nullptr == n || nullptr == snode)
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
static auto
add_gv_node_to_list(GvNode* list, int target_state, SymbolTblNode* snode)
  -> GvNode*
{
    GvNode* n = find_gv_node_in_list(list, target_state);
    if (nullptr == n) { // targetState NOT found. Add to list.
        if constexpr (DEBUG_GEN_GVIZ) {
            std::cout << "target state " << target_state << " not found (label "
                      << snode->symbol << ")" << std::endl;
        }
        n = create_gv_node(target_state);
        n->labels = n->labels_tail = SymbolNode::create(snode);
        if (nullptr == list) { // first node in list.
            list = n;
        } else { // add to tail of list.
            GvNode* m = list;
            while (m->next != nullptr) {
                m = m->next;
            }
            m->next = n;
        }
    } else { // found.
             // add snode to the label list of n.
        if constexpr (DEBUG_GEN_GVIZ) {
            std::cout << "target state " << target_state << " found, add label "
                      << snode->symbol << std::endl;
        }
        insert_label_to_list(n, snode);
    }
    return list;
}

/*
 * dump r list
 */
static void
dump_gv_node_list_r(GvNode* list, int src_state, std::ostream& out)
{
    if (nullptr == list)
        return;
    while (list != nullptr) {
        out << "  " << src_state << " -> r" << list->target_state
            << " [ label=\"";
        // write label list
        SymbolNode* labels = list->labels;
        for (; labels->next != nullptr; labels = labels->next) {
            out << labels->snode->symbol << ",";
        }
        out << labels->snode->symbol;
        out << R"(" style="dashed" ];)" << std::endl;
        list = list->next;
    }
}

/*
 * dump s list
 */
static void
dump_gv_node_list_s(GvNode* list, int src_state, std::ostream& out)
{
    if (nullptr == list)
        return;
    while (list != nullptr) {
        out << "  " << src_state << " -> r" << list->target_state
            << " [ label=\"";
        // write label list
        SymbolNode* labels = list->labels;
        for (; labels->next != nullptr; labels = labels->next) {
            out << labels->snode->symbol << ",";
        }
        out << labels->snode->symbol;
        out << "\" ];" << std::endl;
        list = list->next;
    }
}

static auto
get_gv_node_list_len(GvNode* list) -> int
{
    int len = 0;
    while (list != nullptr) {
        list = list->next;
        len++;
    }
    return len;
}

/*
 * Update the reduction list if the following is satisfied:
 *
 * In case of --lr0 or --lalr:
 *   If a reduction is the single only REDUCTION at this state,
 *   replace it by ".". Similar to the use of final_state_list
 *   in writing y.output. 3-11-2008.
 * Else (LR(1)):
 *   If a reduction is the single only ACTION at this state,
 *   do the same as above.
 *
 * @created on: 3/11/2008
 */
static auto
update_r_list(GvNode* r_list, GvNode* s_list) -> GvNode*
{
    auto& options = Options::get();
    const char* str_any = "(any)"; // means: any terminal can cause reduction.

    if (options.use_lr0 || options.use_lalr) {
        if (get_gv_node_list_len(r_list) == 1) {
            int state = r_list->target_state;
            destroy_gv_node_list(r_list);
            r_list = nullptr;
            r_list =
              add_gv_node_to_list(r_list, state, hash_tbl_insert(str_any));
        }
    } else {
        if (s_list == nullptr && get_gv_node_list_len(r_list) == 1) {
            int state = r_list->target_state;
            destroy_gv_node_list(r_list);
            r_list = nullptr;
            r_list =
              add_gv_node_to_list(r_list, state, hash_tbl_insert(str_any));
        }
    }

    return r_list;
}

/*
 * Has the same logic as printParsingTable() in y.c.
 * For O0, O1.
 */
void
gen_graphviz_input()
{
    char action = 0;
    int state = 0;
    int row_size = ParsingTblRows;
    int col_size = ParsingTblCols;

    std::ofstream fp_gviz;
    fp_gviz.open(y_gviz);

    fp_gviz << "digraph abstract {" << std::endl
            << std::endl
            << "  node [shape = doublecircle]; 0 acc;" << std::endl
            << "  node [shape = circle];" << std::endl;

    for (int row = 0; row < row_size; row++) {
        GvNode* r_list = nullptr;
        GvNode* s_list = nullptr;

        for (int col = 0; col < ParsingTblCols; col++) {
            SymbolTblNode* n = ParsingTblColHdr[col];
            if (!is_goal_symbol(n)) {
                get_action(n->type, col, row, &action, &state);
                /*std::cout  <<  action <<  state<< "\t"; */
                if (action == 0) {
                    /* do nothing */
                } else if (action == 'r') {
                    r_list = add_gv_node_to_list(r_list, state, n);
                } else if (action == 's' || action == 'g') {
                    s_list = add_gv_node_to_list(s_list, state, n);
                } else if (action == 'a') {
                    fp_gviz << " " << row << " -> acc [ label = \"" << n->symbol
                            << "\" ];" << std::endl;
                }
            } // end of if
        }     // end of for.

        r_list = update_r_list(r_list, s_list);

        dump_gv_node_list_r(r_list, row, fp_gviz);
        dump_gv_node_list_s(s_list, row, fp_gviz);
        destroy_gv_node_list(r_list);
        destroy_gv_node_list(s_list);
    }

    fp_gviz << std::endl << "}" << std::endl;
    fp_gviz.close();
}

/*
 * Has the same logic as printCondensedFinalParsingTable() in y.c.
 * For O2, O3.
 */
void
gen_graphviz_input2()
{
    char action = 0;
    int state = 0;
    int col_size = ParsingTblCols;
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
        GvNode* r_list = nullptr;
        GvNode* s_list = nullptr;
        if (is_reachable_state(row)) {
            for (int col = 0; col < ParsingTblCols; col++) {
                SymbolTblNode* n = ParsingTblColHdr[col];
                if (!is_goal_symbol(n) && !is_parent_symbol(n)) {
                    get_action(n->type, col, row, &action, &state);
                    if (action == 's' || action == 'g')
                        state = get_actual_state(state);
                    /* yyprintf("%c%d\t", action, state); */
                    if (action == 0) {
                        /* do nothing */
                    } else if (action == 'r') {
                        r_list = add_gv_node_to_list(r_list, state, n);
                    } else if (action == 's' || action == 'g') {
                        s_list = add_gv_node_to_list(s_list, state, n);
                    } else if (action == 'a') {
                        fp_gviz << " " << row << " -> acc [ label = \""
                                << n->symbol << "\" ];" << std::endl;
                    }
                }
            }
            i++;

            r_list = update_r_list(r_list, s_list);

            int src_state = get_actual_state(row);
            dump_gv_node_list_r(r_list, src_state, fp_gviz);
            dump_gv_node_list_s(s_list, src_state, fp_gviz);
            destroy_gv_node_list(r_list);
            destroy_gv_node_list(s_list);
        } /* end if */
    }

    fp_gviz << std::endl << "}" << std::endl;
    fp_gviz.close();
}
