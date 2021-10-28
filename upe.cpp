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
#include <cstddef>
#include <iostream>
#include <stdexcept>
#include <string>

/*
 * State numbers are added to the dynamic array cmbined_states
 * in function createNewUPSState(), where the size of
 * combined_states is given by old_states_count.
 * combined_states is dynamically allocated there and does not
 * need expansion later.
 */
struct UnitProdState
{
    int* combined_states;
    int combined_states_count;
    int state_no; // state_no of this new state.
};

/* assume 500 new states maximal. */
static int UPS_SIZE = 64;
constexpr size_t UPS_MAX_SIZE = 65536;
static UnitProdState* ups;
static int ups_count = 0;

/*
 * sort an integer array increasingly.
 * Uses insertion sort.
 */
void
sort_int(int array[], int array_len)
{
    for (int i = 1; i < array_len; i++) {
        int tmp = array[i];
        int j = i;
        for (; j > 0 && array[j - 1] > tmp; j--) {
            array[j] = array[j - 1];
        }
        array[j] = tmp;
    }
}

void
print_int_array(int a[], int count)
{
    yyprintf("count = %d\n", count);
    for (int i = 0; i < count; i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%d", a[i]);
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
                    MRParents* parents,
                    int unit_prod_dest_states[],
                    int* unit_prod_count)
{

    // printf("getUnitProdShift for '%s'\n", leaf);
    (*unit_prod_count) = 0;

    for (int i = 0; i < parents->count; i++) {
        SymbolTblNode* n = parents->parents[i]->snode;
        char action = 0;
        int state_dest = 0;
        get_action(n->type, get_col(n), state, &action, &state_dest);
        // printf("%c, %d\n", action, state_dest);
        if (action == 'g') {
            unit_prod_dest_states[*unit_prod_count] = state_dest;
            (*unit_prod_count)++;
        }
    }

    // Note: leaf itself can be a non-terminal.
    // so action can be g too.
    if ((*unit_prod_count) > 0) {
        char action = 0;
        int state_dest = 0;
        get_action(leaf->type, get_col(leaf), state, &action, &state_dest);
        if (action == 's' || action == 'g') {
            unit_prod_dest_states[*unit_prod_count] = state_dest;
            (*unit_prod_count)++;
        }
    }

    sort_int(unit_prod_dest_states, *unit_prod_count);
}

void
write_unit_prod_shift(int state,
                      SymbolTblNode* leaf,
                      int unit_prod_dest_states[],
                      int unit_prod_count,
                      int new_ups_state)
{
    int i;
    yyprintf("state %d, leaf '%s' -", state, leaf->symbol);
    yyprintf(" combine these states to new state %d:\n", new_ups_state);
    for (i = 0; i < unit_prod_count; i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%d", unit_prod_dest_states[i]);
    }
    yyprintf("\n");
}

void
check_ups_size()
{
    if (ups_count < UPS_SIZE)
        return;

    // yyprintf("checkUPSSize: max size %d reached\n", UPS_SIZE);
    // writeUPSStates(); exit(0);

    if (2 * UPS_SIZE >= UPS_MAX_SIZE) {
        std::cout << "checkUPSSize: max size " << UPS_MAX_SIZE << " reached"
                  << std::endl;
        std::cout << "Too many UPS states. " << std::endl;
        // writeUPSStates();
        exit(0);
    }

    UPS_SIZE *= 2;
    HYY_EXPAND(&ups, UPS_SIZE);

    // printf("expand ups size to %d\n", UPS_SIZE);
    // yyprintf("checkUPSSize: expand ups size to %d\n", UPS_SIZE);
}

void
write_ups_state(UnitProdState* ups)
{
    yyprintf("State no: %d. Is combination of these states:\n", ups->state_no);
    print_int_array(ups->combined_states, ups->combined_states_count);
}

void
write_ups_states()
{
    yyprintf("==New states for unit production removal");
    yyprintf(" (total %d):\n", ups_count);
    for (int i = 0; i < ups_count; i++) {
        write_ups_state(&ups[i]);
    }
}

void
create_new_ups_state(int new_state, int old_states[], int old_states_count)
{
    check_ups_size(); // expand if necessary.

    UnitProdState* new_ups = &ups[ups_count];
    new_ups->state_no = new_state;
    new_ups->combined_states_count = old_states_count;

    // allocate dynamic array int * combined_states.
    new_ups->combined_states = new int[new_ups->combined_states_count];

    // copy state numbers.
    for (int i = 0; i < old_states_count; i++)
        new_ups->combined_states[i] = old_states[i];

    ups_count++;

    // printf("ups_count = %d\n", ups_count);
    // printf("new UPS state %d, is combination of states: \n", ups_count - 1);
    // printIntArray(old_states, old_states_count);
}

auto
is_same_ups_state(int a[], int b[], int count) -> bool
{
    for (int i = 0; i < count; i++) {
        if (a[i] != b[i])
            return false;
    }
    return true;
}

/*
 * find the new state which combines states in a[].
 * Return:
 *   state no. if found, or -1 if not found.
 */
auto
get_ups_state(int a[], int count) -> int
{
    if (ups_count == 0)
        return -1;

    for (int i = 0; i < ups_count; i++) {
        if (count == ups[i].combined_states_count) {
            if (is_same_ups_state(ups[i].combined_states, a, count) == true)
                return ups[i].state_no;
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
                        int old_states[])
{

    char action = 0;
    int state_dest = 0;

    get_action(symbol->type,
               get_col(symbol),
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
                                  int old_states[],
                                  int old_states_count)
{
    // printf("Source state: %d. ", src_state);
    // printf("Combine these states into state %d:\n", new_state);
    // print_int_array(old_states, old_states_count);

    // copy actions of old_states to new_state.

    for (int i = 0; i < old_states_count; i++) {
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
            if (is_parent_symbol(a->snode) == true) {
                update_action(get_col(a->snode), i, 0);
            }
        }
    }
}

/*
 * Check if n is in integer array a.
 * Returns true if exists, false if not exists.
 */
auto
in_int_array(int n, int a[], int a_size) -> bool
{
    for (int i = 0; i < a_size; i++) {
        if (n == a[i])
            return true;
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
                                int states_reachable[],
                                int* states_count)
{
    char action = 0;
    int state_dest = 0;

    const SymbolTblNode* n = hash_tbl_find(symbol);

    get_action(n->type, get_col(n), cur_state, &action, &state_dest);
    if ((action == 's' || action == 'g') &&
        in_int_array(state_dest, states_reachable, *states_count) == false) {
        states_reachable[*states_count] = state_dest;
        (*states_count)++;
        get_reachable_states(state_dest, states_reachable, states_count);
    }
}

/*
 * In the parsing table, get states that are reachable
 * from cur_state, and stores the result in array
 * states_reachable[].
 */
void
get_reachable_states(int cur_state, int states_reachable[], int* states_count)
{
    get_reachable_states_for_symbol(
      STR_END, cur_state, states_reachable, states_count);

    for (SymbolNode* a = grammar.terminal_list; a != nullptr; a = a->next) {
        get_reachable_states_for_symbol(
          a->snode->symbol, cur_state, states_reachable, states_count);
    }

    for (SymbolNode* a = grammar.non_terminal_list; a != nullptr; a = a->next) {
        if (is_parent_symbol(a->snode) == false) {
            get_reachable_states_for_symbol(
              a->snode->symbol, cur_state, states_reachable, states_count);
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
      create_symbol_node(hash_tbl_find(STR_END));
    F_ParsingTblCols = 1;

    for (SymbolNode* a = grammar.terminal_list; a != nullptr; a = a->next) {
        tail->next = create_symbol_node(a->snode);
        tail = tail->next;
        F_ParsingTblCols++;
    }

    for (SymbolNode* a = grammar.non_terminal_list; a != nullptr; a = a->next) {
        if (is_parent_symbol(a->snode) == false &&
            is_goal_symbol(a->snode) == false) {
            tail->next = create_symbol_node(a->snode);
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
    states_reachable = new int[ParsingTblRows];
    if (states_reachable == nullptr) {
        YYERR_EXIT("remove_unit_productino_step4 error: out of memory\n");
    }
    states_reachable_count = 0;
    get_reachable_states(0, states_reachable, &states_reachable_count);
    sort_int(states_reachable, states_reachable_count);

    if (Options::get().debug_remove_up_step_4) {
        yyprintf("\n--remove_unit_production_step4--\n");
        yyprintf("states reachable from state 0:\n");
        print_int_array(states_reachable, states_reachable_count);
    }

    get_f_parsing_tbl_col_hdr();
}

auto
is_reachable_state(int state) -> bool
{
    if (state == 0 ||
        in_int_array(state, states_reachable, states_reachable_count) == true) {
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
        if (is_reachable_state(row) == true) {
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
    int row_size = ParsingTblRows;
    actual_state_no = new int[2 * row_size];
    if (actual_state_no == nullptr) {
        YYERR_EXIT("get_actual_state_no error: out of memory\n");
    }

    int i = 0;
    actual_state_no_ct = 0;
    for (int row = 0; row < row_size; row++) {
        if (row == 0 ||
            in_int_array(row, states_reachable, states_reachable_count) ==
              true) {
            actual_state_no[actual_state_no_ct++] = row;
            actual_state_no[actual_state_no_ct++] = i;
            i++;
        }
    }

    // printIntArray(actual_state_no, actual_state_no_ct);
}

auto
get_actual_state(int virtual_state) -> int
{
    for (int i = 0; i < actual_state_no_ct; i += 2) {
        if (virtual_state == actual_state_no[i])
            return actual_state_no[i + 1];
    }
    return -1; // this should not happen.
}

void
write_actual_state_array()
{
    if (!Options::get().use_remove_unit_production)
        return;

    yyprintf("\n\n--actual state array [actual, pseudo]--\n");
    for (int i = 0; i < actual_state_no_ct; i += 2) {
        if (i > 0 && i % 5 == 0)
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
        if (is_reachable_state(row) == true) {
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
remove_unit_production_step5()
{
    int ct = get_grammar_rule_count();
    for (int i = 0; i < ct; i++) {
        int index =
          get_index_in_mr_parents(grammar.rules[i]->nLHS->snode, all_parents);
        if (index >= 0) {
            free_symbol_node(grammar.rules[i]->nLHS);
            grammar.rules[i]->nLHS = create_symbol_node(
              MRLeaves[leafIndexForParent[index]]->symbol->snode);
        }
    }
}

void
remove_unit_production_step1and2()
{
    bool debug_remove_up_step_1_2 = Options::get().debug_remove_up_step_1_2;
    int state = 0, i = 0, unit_prod_count = 0, ups_state = 0;
    SymbolTblNode* leaf = nullptr;
    MRParents* parents = nullptr;

    // as discussed in the function comments of getUnitProdShift(),
    // unitProdDestStates array is bounded by number of non_terminals + 1.
    auto* unit_prod_dest_states = new int[grammar.non_terminal_count + 1];
    ups = new UnitProdState[UPS_SIZE];
    ups_count = 0;
    auto* leaf_parents = new MRParents*[MRLeaves_count];

    // pre-calculate all parents for each leaf.
    for (i = 0; i < MRLeaves_count; i++) {
        leaf_parents[i] = create_mr_parents();
        get_parents_for_mr_leaf(i, leaf_parents[i]);
        // writeMRParents(MRLeaves[i], leaf_parents[i]);
    }

    if (debug_remove_up_step_1_2) {
        yyprintf("\n--remove_unit_production_step1and2--\n");
        yyprintf("--writeUnitProdShift--\n");
    }

    // now, steps 1 and 2.
    for (state = 0; state < ParsingTblRows; state++) {
        for (i = 0; i < MRLeaves_count; i++) {
            leaf = MRLeaves[i]->symbol->snode;
            // printf("state %d, checking leaf %s\n", state, leaf->symbol);
            parents = leaf_parents[i];

            get_unit_prod_shift(
              state, leaf, parents, unit_prod_dest_states, &unit_prod_count);

            if (unit_prod_count > 0) { // unitProdCount >= 2
                ups_state =
                  get_ups_state(unit_prod_dest_states, unit_prod_count);
                if (ups_state == -1) {
                    ups_state = ParsingTblRows;
                    ParsingTblRows++;
                    if (ParsingTblRows == PARSING_TABLE_SIZE) {
                        // yyprintf("remove_unit_production message: ");
                        // yyprintf("Parsing Table size reached\n");
                        expand_parsing_table();
                    }
                    create_new_ups_state(
                      ups_state, unit_prod_dest_states, unit_prod_count);

                    // Combine actions of states into state ups_state.
                    // Do this only if this combined state does not exist yet.
                    insert_actions_of_combined_states(
                      ups_state, state, unit_prod_dest_states, unit_prod_count);

                } // end if (ups_state != -1)

                // Update the link from src_state to leaf transition state
                update_action(get_col(leaf), state, ups_state); // shift.

                if (debug_remove_up_step_1_2) {
                    write_unit_prod_shift(state,
                                          leaf,
                                          unit_prod_dest_states,
                                          unit_prod_count,
                                          ups_state);
                    // yyprintf(" => new ups_state: %d\n", ups_state);
                }
            } // end if (unitProdCount > 0)
        }
    }
    if (debug_remove_up_step_1_2) {
        yyprintf("--after remove_unit_production_step1and2(), ");
        yyprintf("total states: %d--\n", ParsingTblRows);
    }

    for (i = 0; i < MRLeaves_count; i++)
        destroy_mr_parents(leaf_parents[i]);

    delete[] unit_prod_dest_states;
}

////////////////////////////////////////////////////////
// Dr. Pager, Acta Informatica 9, 31-59 (1977), page 38.
////////////////////////////////////////////////////////
void
remove_unit_production()
{
    build_multirooted_tree();

    remove_unit_production_step1and2();
    remove_unit_production_step3();
    remove_unit_production_step4();
    remove_unit_production_step5();

    n_state_opt12 = states_reachable_count + 1;
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
        get_action(n->type, get_col(n), i, &action_i, &state_dest_i);
        get_action(n->type, get_col(n), j, &action_j, &state_dest_j);
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
    get_action(n->type, get_col(n), row, &action, &state_dest);
    if (state_dest == old_state)
        update_action(get_col(hash_tbl_find(STR_END)), row, new_state);

    // for terminal columns
    for (SymbolNode* a = g->terminal_list; a != nullptr; a = a->next) {
        n = a->snode;
        get_action(n->type, get_col(n), row, &action, &state_dest);
        if (state_dest == old_state)
            update_action(get_col(a->snode), row, new_state);
    }

    // for non-terminal columns
    for (SymbolNode* a = g->non_terminal_list; a != nullptr; a = a->next) {
        n = a->snode;
        get_action(n->type, get_col(n), row, &action, &state_dest);
        if (state_dest == old_state)
            update_action(get_col(a->snode), row, new_state);
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

    for (int i = 0; i < states_reachable_count; i++)
        update_repeated_row(new_state, old_state, states_reachable[i]);
}

/*
 * Remove state i from the array of reachable states.
 */
void
remove_reachable_state(int i)
{
    for (; i < states_reachable_count - 1; i++) {
        states_reachable[i] = states_reachable[i + 1];
    }
    states_reachable_count--;
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

    for (int k = 0; k < states_reachable_count - 1; k++) {
        int i = states_reachable[k];
        int j = states_reachable[k + 1];
        // printf("furtherOpt: i = %d, j = %d\n", i, j);
        do {
            if (is_equal_row(i, j) == false)
                break;

            update_repeated_rows(i, j);
            // printf("state %d removed\n", states_reachable[k + 1]);
            remove_reachable_state(k + 1);
            if ((k + 1) == states_reachable_count)
                break;
            j = states_reachable[k + 1];
            // printf("- furtherOpt: i = %d, j = %d\n", i, j);
        } while (true);
    }

    n_state_opt123 = states_reachable_count + 1;

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
