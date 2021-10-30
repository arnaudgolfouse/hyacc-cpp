/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007, 2008 Xin Chen. chenx@hawaii.edu

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
 * upe.c
 *
 * For unit production elimination and further remove redundant states.
 * This is separated out from y.c to make y.c size smaller.
 *
 * @Author: Xin Chen
 * @Date started: March 9, 2007
 * @Last modified: March 9, 2007
 * @Copyright (C) 2007, 2008
 */

#include "mrt.hpp"
#include "y.hpp"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

/*
 * State numbers are added to the dynamic array cmbined_states
 * in function createNewUPSState(), where the size of
 * combined_states is given by old_states_count.
 * combined_states is dynamically allocated there and does not
 * need expansion later.
 */
struct UnitProdState
{
    std::vector<int> combined_states{};
    int state_no{ 0 }; // state_no of this new state.
};

/* assume 500 new states maximal. */
constexpr size_t UPS_SIZE = 64;
constexpr size_t UPS_MAX_SIZE = 65536;

/*
 * sort an integer array increasingly.
 * Uses insertion sort.
 */
// void
// sort_int(int array[], int array_len)
// {
//     for (int i = 1; i < array_len; i++) {
//         int tmp = array[i];
//         int j = i;
//         for (; j > 0 && array[j - 1] > tmp; j--) {
//             array[j] = array[j - 1];
//         }
//         array[j] = tmp;
//     }
// }

void
print_int_array(const std::vector<int>& a)
{
    yyprintf("count = %d\n", a.size());
    bool first = true;
    for (int elem : a) {
        if (!first)
            yyprintf(", ");
        first = false;
        yyprintf("%d", elem);
    }
    yyprintf("\n");
}

/*
 * Called by function remove_unit_production_step1and2() only.
 *
 * This function does this:
 *
 * For given state s and leaf x, we have
 * non-terminal symbols y such that y => x.
 * if goto actions exist for some y's, then
 * get target state numbers of all y's goto actions,
 * as well as the target state number of x's shift/goto
 * actions if any.
 *
 * The number of such target states numbers is unitProdCount.
 * Note that it's bounded by non_terminal_count + 1, where 1
 * is for the leaf itself. This is because in the extreme
 * case all non terminals y => x.
 *
 * BASICALLY, unitProdCount == 0 or unitProdCount >= 2!
 * It is NOT possible that unitProdCount == 1.
 * This is because if there is a y successor of state s,
 * then y => x means there will also be x successors for s.
 *
 * unitProdCount >= 2. This also suggests that any
 * new state is not one from 0 to total states count - 1.
 *
 * These target states are to be combined in the next step.
 */
void
get_unit_prod_shift(int state,
                    SymbolTblNode* leaf,
                    const MRParents& parents,
                    std::vector<int>& unit_prod_dest_states)
{
    // printf("getUnitProdShift for '%s'\n", leaf);
    unit_prod_dest_states.clear();

    for (const auto& parent : parents) {
        SymbolTblNode* n = parent->snode;
        char action = 0;
        int state_dest = 0;
        get_action(n->type, get_col(*n), state, &action, &state_dest);
        // printf("%c, %d\n", action, state_dest);
        if (action == 'g') {
            unit_prod_dest_states.push_back(state_dest);
        }
    }

    // Note: leaf itself can be a non-terminal.
    // so action can be g too.
    if (!unit_prod_dest_states.empty()) {
        char action = 0;
        int state_dest = 0;
        get_action(leaf->type, get_col(*leaf), state, &action, &state_dest);
        if (action == 's' || action == 'g') {
            unit_prod_dest_states.push_back(state_dest);
        }
    }

    std::sort(unit_prod_dest_states.begin(), unit_prod_dest_states.end());
}

void
write_unit_prod_shift(int state,
                      SymbolTblNode* leaf,
                      const std::vector<int>& unit_prod_dest_states,
                      int new_ups_state)
{
    yyprintf("state %d, leaf '%s' -", state, leaf->symbol);
    yyprintf(" combine these states to new state %d:\n", new_ups_state);
    for (int i = 0; i < unit_prod_dest_states.size(); i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%d", unit_prod_dest_states[i]);
    }
    yyprintf("\n");
}

void
write_ups_state(const UnitProdState& ups)
{
    yyprintf("State no: %d. Is combination of these states:\n", ups.state_no);
    print_int_array(ups.combined_states);
}

void
write_ups_states(const std::vector<UnitProdState>& ups)
{
    yyprintf("==New states for unit production removal");
    yyprintf(" (total %d):\n", ups.size());
    for (const auto& up : ups) {
        write_ups_state(up);
    }
}

void
create_new_ups_state(std::vector<UnitProdState>& ups,
                     int new_state,
                     const std::vector<int>& old_states)
{
    UnitProdState new_ups;
    new_ups.state_no = new_state;
    new_ups.combined_states = old_states;
    ups.push_back(new_ups);

    // printf("ups_count = %d\n", ups_count);
    // printf("new UPS state %d, is combination of states: \n", ups_count - 1);
    // printIntArray(old_states, old_states_count);
}

/*
 * find the new state which combines states in a[].
 * Return:
 *   state no. if found, or -1 if not found.
 */
auto
get_ups_state(const std::vector<UnitProdState>& ups, const std::vector<int>& a)
  -> int
{
    if (ups.empty())
        return -1;
    for (const auto& up : ups) {
        if (a == up.combined_states) {
            return up.state_no;
        }
    }
    return -1;
}

auto
is_unit_production(int rule_no) -> bool
{
    if (rule_no >= grammar.rules.size()) {
        throw std::runtime_error(
          std::string("isUnitProduction error: array index (") +
          std::to_string(rule_no) + ") out of bound");
    }
    if (grammar.rules[rule_no]->RHS_count == 1 &&
        strlen(grammar.rules[rule_no]->nRHS_head->snode->symbol) > 0)
        return true;

    return false;
}

/*
 * Called by function insert_actionsOfCombinedStates().
 */
void
insert_action_of_symbol(SymbolTblNode* symbol,
                        int new_state,
                        int old_state_index,
                        std::vector<int>& old_states)
{

    char action = 0;
    int state_dest = 0;

    get_action(symbol->type,
               get_col(*symbol),
               old_states[old_state_index],
               &action,
               &state_dest);

    if (action == 0)
        return;

    //printf("insert action %c to dest_state %d for new state \
  //%d on symbol %s\n", action, state_dest, new_state, symbol->symbol);

    if (action == 'a') {
        insert_action(symbol, new_state, CONST_ACC);
    } else if (action == 's' || action == 'g') {
        insert_action(symbol, new_state, state_dest);
    } else if (action == 'r') {
        if (is_unit_production(state_dest) == false) {
            insert_action(symbol, new_state, (-1) * state_dest);
        }
    }
}

/*
 * For a combined state, get actions from each of the states
 * from which the combined state is made of, and copy these
 * actions to the combined state.
 *
 * Called by remove_unit_production_step1and2() only.
 */
void
insert_actions_of_combined_states(int new_state,
                                  int src_state,
                                  std::vector<int>& old_states)
{
    // printf("Source state: %d. ", src_state);
    // printf("Combine these states into state %d:\n", new_state);
    // print_int_array(old_states, old_states_count);

    // copy actions of old_states to new_state.

    for (int i = 0; i < old_states.size(); i++) {
        // Copy action of end marker STR_END.
        insert_action_of_symbol(
          hash_tbl_find(STR_END), new_state, i, old_states);

        // copy actions of terminals.
        for (const SymbolNode* a = grammar.terminal_list; a != nullptr;
             a = a->next) {
            insert_action_of_symbol(a->snode, new_state, i, old_states);
        }

        // copy actions of non_terminals.
        for (const SymbolNode* a = grammar.non_terminal_list; a != nullptr;
             a = a->next) {
            insert_action_of_symbol(a->snode, new_state, i, old_states);
        }
    } // end for
}

/*
 * step 3. Delete transitions wrt. LHS of unit productions.
 * equivalent to remove all non-terminal goto actions.
 * new states don't have these, so just remove those
 * of the old state.
 *
 * Actually, THIS IS NOT EVEN NECESSARY if all is needed
 * is stdout output. This is because after getting all
 * parent symbols we can ignore to output them in the
 * step writeFinalParsingTable!
 */
void
remove_unit_production_step3()
{
    for (int i = 0; i < ParsingTblRows; i++) {
        for (const SymbolNode* a = grammar.non_terminal_list; a != nullptr;
             a = a->next) {
            // use "" as action and 0 as dest state clears it.
            // Only those non-terminals y => x are cleared.
            if (is_parent_symbol(a->snode)) {
                update_action(get_col(*a->snode), i, 0);
            }
        }
    }
}

/*
 * Check if n is in integer array a.
 * Returns true if exists, false if not exists.
 */
auto
in_int_array(int n, const std::vector<int>& states_reachable) -> bool
{
    for (const int elem : states_reachable) {
        if (n == elem) {
            return true;
        }
    }
    return false;
}

/*
 * Note that action 'g' applies to non-terminals only.
 * But include it does not affect the cases for STR_END
 * and terminals, which have only action 's'. So just
 * include checking for 'g' for all three situations.
 */
void
get_reachable_states_for_symbol(const char* symbol,
                                int cur_state,
                                std::vector<int>& states_reachable)
{
    char action = 0;
    int state_dest = 0;

    const SymbolTblNode* n = hash_tbl_find(symbol);

    get_action(n->type, get_col(*n), cur_state, &action, &state_dest);
    if ((action == 's' || action == 'g') &&
        !in_int_array(state_dest, states_reachable)) {
        states_reachable.push_back(state_dest);
        get_reachable_states(state_dest, states_reachable);
    }
}

/*
 * In the parsing table, get states that are reachable
 * from cur_state, and stores the result in array
 * states_reachable[].
 */
void
get_reachable_states(int cur_state, std::vector<int>& states_reachable)
{
    get_reachable_states_for_symbol(STR_END, cur_state, states_reachable);

    for (SymbolNode* a = grammar.terminal_list; a != nullptr; a = a->next) {
        get_reachable_states_for_symbol(
          a->snode->symbol, cur_state, states_reachable);
    }

    for (SymbolNode* a = grammar.non_terminal_list; a != nullptr; a = a->next) {
        if (is_parent_symbol(a->snode) == false) {
            get_reachable_states_for_symbol(
              a->snode->symbol, cur_state, states_reachable);
        }
    }
}

void
write_parsing_table_col_header()
{
    for (int i = 0; i < ParsingTblCols; i++) {
        if (is_goal_symbol(ParsingTblColHdr[i]) == false) {
            yyprintf("%s\t", ParsingTblColHdr[i]->symbol);
        }
    }
    yyprintf("\n");
}

/*
 * get final parsing table column headers.
 * + 1 for end marker STR_END.
 */
void
get_f_parsing_tbl_col_hdr()
{
    SymbolNode* tail = F_ParsingTblColHdr =
      SymbolNode::create(hash_tbl_find(STR_END));
    F_ParsingTblCols = 1;

    for (SymbolNode* a = grammar.terminal_list; a != nullptr; a = a->next) {
        tail->next = SymbolNode::create(a->snode);
        tail = tail->next;
        F_ParsingTblCols++;
    }

    for (SymbolNode* a = grammar.non_terminal_list; a != nullptr; a = a->next) {
        if (is_parent_symbol(a->snode) == false &&
            is_goal_symbol(a->snode) == false) {
            tail->next = SymbolNode::create(a->snode);
            tail = tail->next;
            F_ParsingTblCols++;
        }
    }
}

void
write_final_parsing_table_col_header()
{
    for (SymbolNode* a = F_ParsingTblColHdr; a != nullptr; a = a->next) {
        yyprintf("%s\t", a->snode->symbol);
    }
    yyprintf("\n");
}

/*
 * step 4. delete all states at this stage
 * not reachable from state 0.
 * Does a recursive traveral starting from
 * state 0 to determine reachable state.
 *
 * int * states_reachable and int states_reachable_count
 * are defined in y.h.
 */
void
remove_unit_production_step4()
{
    states_reachable.clear();
    states_reachable.reserve(ParsingTblRows);
    get_reachable_states(0, states_reachable);
    std::sort(states_reachable.begin(), states_reachable.end());

    if (Options::get().debug_remove_up_step_4) {
        yyprintf("\n--remove_unit_production_step4--\n");
        yyprintf("states reachable from state 0:\n");
        print_int_array(states_reachable);
    }

    get_f_parsing_tbl_col_hdr();
}

auto
is_reachable_state(int state) -> bool
{
    if (state == 0 || in_int_array(state, states_reachable)) {
        return true;
    }
    return false;
}

/*
 * The parsing table array does not change,
 * only change the output entries.
 */
void
print_final_parsing_table()
{
    int col_size = ParsingTblCols;
    int row_size = ParsingTblRows;
    char action = 0;
    int state = 0;

    yyprintf("\n--Parsing Table--\n");
    yyprintf("State\t");
    write_final_parsing_table_col_header();

    for (int row = 0; row < row_size; row++) {
        if (is_reachable_state(row)) {
            yyprintf("%d\t", row);
            for (int col = 0; col < ParsingTblCols; col++) {
                SymbolTblNode* n = ParsingTblColHdr[col];
                if (is_goal_symbol(n) == false &&
                    is_parent_symbol(n) == false) {
                    get_action(n->type, col, row, &action, &state);
                    yyprintf("%c%d\t", action, state);
                }
            }

            yyprintf("\n");
        } // end if
          // else { printf("state %d: not reachable.\n", row); }
    }     // end for
    print_parsing_table_note();
}

/*
 * There are some holes in the parsing table.
 * Now do a first pass to record the "virtual" state number
 * and the "actual" state number correspondence relationship,
 * then when print the parsing table, replace those
 * "virtual" state numbers with corresponding "actual"
 * state numbers.
 *
 * Store the correspondence relationship in array
 * actual_state_no[].
 * actual_state_no[2 * i] is virtual state,
 * actual_state_no[2 * i + 1} is actual state.
 *
 * The following two are defined in y.h:
 * int actual_state_no[2 * STATE_COLLECTION_SIZE];
 * int actual_state_no_ct;
 */
void
get_actual_state_no()
{
    size_t row_size = ParsingTblRows;
    actual_state_no.clear();
    actual_state_no.reserve(2 * row_size);

    int i = 0;
    for (int row = 0; row < row_size; row++) {
        if (row == 0 || in_int_array(row, states_reachable)) {
            actual_state_no.push_back(row);
            actual_state_no.push_back(i);
            i++;
        }
    }

    // printIntArray(actual_state_no, actual_state_no_ct);
}

auto
get_actual_state(int virtual_state) -> int
{
    for (int i = 0; i < actual_state_no.size(); i += 2) {
        if (virtual_state == actual_state_no[i])
            return actual_state_no[i + 1];
    }
    return -1; // this should not happen.
}

void
write_actual_state_array()
{
    constexpr int LINE_LENGTH = 5;
    if (!Options::get().use_remove_unit_production)
        return;

    yyprintf("\n\n--actual state array [actual, pseudo]--\n");
    for (int i = 0; i < actual_state_no.size(); i += 2) {
        if (i > 0 && i % LINE_LENGTH == 0)
            yyprintf("\n");
        yyprintf("[%d, %d] ", actual_state_no[i], actual_state_no[i + 1]);
    }
    yyprintf("\n\n");
}

/*
 * If an action is 's' or 'g', change its target state number
 * from virtual to actual.
 */
void
print_condensed_final_parsing_table()
{
    char action = 0;
    int state_no = 0;
    int col_size = ParsingTblCols;
    // value assigned at the end of generate_parsing_table().
    int row_size = ParsingTblRows;

    yyprintf("\n--Final Parsing Table--\n");
    yyprintf("State\t");
    write_final_parsing_table_col_header();

    int i = 0;
    for (int row = 0; row < row_size; row++) {
        if (is_reachable_state(row)) {
            yyprintf("%d\t", i);
            for (int col = 0; col < ParsingTblCols; col++) {
                SymbolTblNode* n = ParsingTblColHdr[col];
                if (is_goal_symbol(n) == false &&
                    is_parent_symbol(n) == false) {
                    get_action(n->type, col, row, &action, &state_no);
                    if (action == 's' || action == 'g')
                        state_no = get_actual_state(state_no);
                    yyprintf("%c%d\t", action, state_no);
                }
            }

            i++;
            yyprintf("\n");
        } // end if
    }     // end for

    print_parsing_table_note();
}

/*
 * This actually is not needed too (see step 3).
 * Because all we care in x -> a b c
 * is how many symbols we have on the RHS.
 */
void
remove_unit_production_step5(const MRLeaves& mr_leaves)
{
    for (const auto& rule : grammar.rules) {
        int index = get_index_in_mr_parents(rule->nLHS->snode, *all_parents);
        if (index >= 0) {
            free_symbol_node(rule->nLHS);
            rule->nLHS = SymbolNode::create(
              mr_leaves[leaf_index_for_parent[index]]->symbol->snode);
        }
    }
}

void
remove_unit_production_step1and2(const MRLeaves& mr_leaves)
{
    bool debug_remove_up_step_1_2 = Options::get().debug_remove_up_step_1_2;
    // as discussed in the function comments of getUnitProdShift(),
    // unitProdDestStates array is bounded by number of non_terminals + 1.
    std::vector<int> unit_prod_dest_states;
    std::vector<UnitProdState> ups;
    ups.reserve(UPS_SIZE);
    std::vector<std::shared_ptr<MRParents>> leaf_parents;

    // pre-calculate all parents for each leaf.
    for (size_t i = 0; i < mr_leaves.size(); i++) {
        leaf_parents.push_back(create_mr_parents());
        get_parents_for_mr_leaf(mr_leaves, i, leaf_parents[i].get());
        // writeMRParents(MRLeaves[i], leaf_parents[i]);
    }

    if (debug_remove_up_step_1_2) {
        yyprintf("\n--remove_unit_production_step1and2--\n");
        yyprintf("--writeUnitProdShift--\n");
    }

    // now, steps 1 and 2.
    for (int state = 0; state < ParsingTblRows; state++) {
        for (int i = 0; i < mr_leaves.size(); i++) {
            SymbolTblNode* leaf = mr_leaves[i]->symbol->snode;
            // printf("state %d, checking leaf %s\n", state, leaf->symbol);
            const auto& parents = leaf_parents[i];

            get_unit_prod_shift(state, leaf, *parents, unit_prod_dest_states);

            if (!unit_prod_dest_states.empty()) { // unitProdCount >= 2
                int ups_state = get_ups_state(ups, unit_prod_dest_states);
                if (ups_state == -1) {
                    ups_state = ParsingTblRows;
                    ParsingTblRows++;
                    if (ParsingTblRows >= PARSING_TABLE_SIZE) {
                        // yyprintf("remove_unit_production message: ");
                        // yyprintf("Parsing Table size reached\n");
                        expand_parsing_table();
                    }
                    create_new_ups_state(ups, ups_state, unit_prod_dest_states);

                    // Combine actions of states into state ups_state.
                    // Do this only if this combined state does not exist yet.
                    insert_actions_of_combined_states(
                      ups_state, state, unit_prod_dest_states);

                } // end if (ups_state != -1)

                // Update the link from src_state to leaf transition state
                update_action(get_col(*leaf), state, ups_state); // shift.

                if (debug_remove_up_step_1_2) {
                    write_unit_prod_shift(
                      state, leaf, unit_prod_dest_states, ups_state);
                    // yyprintf(" => new ups_state: %d\n", ups_state);
                }
            } // end if (unitProdCount > 0)
        }
    }
    if (debug_remove_up_step_1_2) {
        yyprintf("--after remove_unit_production_step1and2(), ");
        yyprintf("total states: %d--\n", ParsingTblRows);
    }
}

////////////////////////////////////////////////////////
// Dr. Pager, Acta Informatica 9, 31-59 (1977), page 38.
////////////////////////////////////////////////////////
void
remove_unit_production()
{
    MRLeaves mr_leaves = build_multirooted_tree();

    remove_unit_production_step1and2(mr_leaves);
    remove_unit_production_step3();
    remove_unit_production_step4();
    remove_unit_production_step5(mr_leaves);

    n_state_opt12 = states_reachable.size() + 1;
}

/////////////////////////////////////////////////////
// Functions for removing unit productions. End.
/////////////////////////////////////////////////////

/////////////////////////////////////////////////////
// Functions for further optimization. Start.
/////////////////////////////////////////////////////

/*
 * Determine if rows i and j in the parsing table are the same.
 */
auto
is_equal_row(int i, int j) -> bool
{
    char action_i = 0, action_j = 0;
    int state_dest_i = 0, state_dest_j = 0;

    for (int col = 0; col < ParsingTblCols; col++) {
        SymbolTblNode* n = ParsingTblColHdr[col];
        get_action(n->type, get_col(*n), i, &action_i, &state_dest_i);
        get_action(n->type, get_col(*n), j, &action_j, &state_dest_j);
        if (action_i != action_j || state_dest_i != state_dest_j)
            return false;
    }

    return true;
}

/*
 * In parsing table row, replace entries whose
 * target state is old_state by new_state.
 */
void
update_repeated_row(int new_state, int old_state, int row)
{
    char action = 0;
    int state_dest = 0;
    Grammar* g = &grammar;
    // printf("In row %d, replace %d by %d\n", row, old_state, new_state);

    // for end marker column STR_END
    SymbolTblNode* n = hash_tbl_find(STR_END);
    get_action(n->type, get_col(*n), row, &action, &state_dest);
    if (state_dest == old_state)
        update_action(get_col(*hash_tbl_find(STR_END)), row, new_state);

    // for terminal columns
    for (SymbolNode* a = g->terminal_list; a != nullptr; a = a->next) {
        n = a->snode;
        get_action(n->type, get_col(*n), row, &action, &state_dest);
        if (state_dest == old_state)
            update_action(get_col(*a->snode), row, new_state);
    }

    // for non-terminal columns
    for (SymbolNode* a = g->non_terminal_list; a != nullptr; a = a->next) {
        n = a->snode;
        get_action(n->type, get_col(*n), row, &action, &state_dest);
        if (state_dest == old_state)
            update_action(get_col(*a->snode), row, new_state);
    }
}

/*
 * Go through the entire parsing table (only those reachable
 * states, to save time), replace those entries
 * whose target state is old_state by new_state.
 */
void
update_repeated_rows(int new_state, int old_state)
{
    update_repeated_row(new_state, old_state, 0); // row 0.

    for (const auto& state_reachable : states_reachable)
        update_repeated_row(new_state, old_state, state_reachable);
}

/*
 * Remove state i from the array of reachable states.
 */
void
remove_reachable_state(int i)
{
    states_reachable.erase(states_reachable.begin() + i);
}

/*
 * Remove the repeated states in the parsing table.
 * It seems that repeated states are always adjacent to each other.
 * The algorithm is:
 *   for each state,
 *     find all successive states that are the same as it,
 *     go through the entire parsing table to update link to
 *     those repeated states to the first such state.
 *     remove the repeated states from the reachable states.
 *
 * Note:
 * It seems that all equal rows are adjacent to each other,
 * Using this observation, it's faster to go through the cycle.
 * But this is just an observation though.
 * Maybe should use the safer way, to use a double loop.
 * It's O(n^2) anyway.
 */
void
further_optimization()
{
    // n_state_opt12 = states_reachable_count + 1;

    for (int k = 0; k < states_reachable.size() - 1; k++) {
        int i = states_reachable[k];
        int j = states_reachable[k + 1];
        // printf("furtherOpt: i = %d, j = %d\n", i, j);
        do {
            if (is_equal_row(i, j) == false)
                break;

            update_repeated_rows(i, j);
            // printf("state %d removed\n", states_reachable[k + 1]);
            states_reachable.erase(states_reachable.begin() + k + 1);
            if ((k + 1) == states_reachable.size())
                break;
            j = states_reachable[k + 1];
            // printf("- furtherOpt: i = %d, j = %d\n", i, j);
        } while (true);
    }

    n_state_opt123 = states_reachable.size() + 1;

    if (Options::get().show_parsing_tbl && (n_state_opt12 > n_state_opt123)) {
        yyprintf("After further optimization, ");
        yyprintf("total states reduced from %d to %d\n",
                 n_state_opt12,
                 n_state_opt123);
    }
}

/////////////////////////////////////////////////////
// Functions for further optimization. End.
/////////////////////////////////////////////////////
