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
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
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
    std::vector<StateHandle> combined_states{};
    StateHandle state_no{ 0 }; // state_no of this new state.
};

/* assume 500 new states maximal. */
constexpr size_t UPS_SIZE = 64;
constexpr size_t UPS_MAX_SIZE = 65536;

static void
get_reachable_states(const Grammar& grammar,
                     const StateHandle cur_state,
                     std::vector<StateHandle>& states_reachable);

void
print_int_array(std::ostream& os, const std::vector<StateHandle>& a)
{
    os << "count = " << a.size() << std::endl;
    bool first = true;
    for (const StateHandle elem : a) {
        if (!first)
            os << ", ";
        first = false;
        os << elem;
    }
    os << std::endl;
}

/// Called by function remove_unit_production_step1and2() only.
///
/// This function does this:
///
/// For given state s and leaf x, we have
/// non-terminal symbols y such that y => x.
/// if goto actions exist for some y's, then
/// get target state numbers of all y's goto actions,
/// as well as the target state number of x's shift/goto
/// actions if any.
///
/// The number of such target states numbers is unitProdCount.
/// Note that it's bounded by non_terminal_count + 1, where 1
/// is for the leaf itself. This is because in the extreme
/// case all non terminals y => x.
///
/// BASICALLY, unitProdCount == 0 or unitProdCount >= 2!
/// It is NOT possible that unitProdCount == 1.
/// This is because if there is a y successor of state s,
/// then y => x means there will also be x successors for s.
///
/// unitProdCount >= 2. This also suggests that any
/// new state is not one from 0 to total states count - 1.
///
/// These target states are to be combined in the next step.
static void
get_unit_prod_shift(StateHandle state,
                    std::shared_ptr<SymbolTableNode> leaf,
                    const MRParents& parents,
                    std::vector<StateHandle>& unit_prod_dest_states)
{
    // std::cout << "getUnitProdShift for '" <<  leaf<< "'" << std::endl;
    unit_prod_dest_states.clear();

    for (const auto& parent : parents) {
        std::shared_ptr<SymbolTableNode> n = parent->snode;
        auto [action, state_dest] = get_action(n->type, get_col(*n), state);
        // std::cout << action << ", " << state_dest << std::endl;
        if (action == 'g') {
            unit_prod_dest_states.push_back(state_dest);
        }
    }

    // Note: leaf itself can be a non-terminal.
    // so action can be g too.
    if (!unit_prod_dest_states.empty()) {
        auto [action, state_dest] =
          get_action(leaf->type, get_col(*leaf), state);
        if (action == 's' || action == 'g') {
            unit_prod_dest_states.push_back(state_dest);
        }
    }

    std::sort(unit_prod_dest_states.begin(), unit_prod_dest_states.end());
}

static void
write_unit_prod_shift(std::ostream& os,
                      StateHandle state,
                      std::shared_ptr<SymbolTableNode> leaf,
                      const std::vector<StateHandle>& unit_prod_dest_states,
                      StateHandle new_ups_state)
{
    os << "state " << state << ", leaf '" << leaf->symbol << "' -";
    os << " combine these states to new state " << new_ups_state << ":"
       << std::endl;
    for (size_t i = 0; i < unit_prod_dest_states.size(); i++) {
        if (i > 0)
            os << ", ";
        os << unit_prod_dest_states[i];
    }
    os << std::endl;
}

static void
write_ups_state(std::ostream& os, const UnitProdState& ups)
{
    os << "State no: " << ups.state_no
       << ". Is combination of these states:" << std::endl;
    print_int_array(os, ups.combined_states);
}

static void
write_ups_states(std::ostream& os, const std::vector<UnitProdState>& ups)
{
    os << "==New states for unit production removal";
    os << " (total " << ups.size() << "):" << std::endl;
    for (const auto& up : ups) {
        write_ups_state(os, up);
    }
}

static void
create_new_ups_state(std::vector<UnitProdState>& ups,
                     StateHandle new_state,
                     const std::vector<StateHandle>& old_states)
{
    UnitProdState new_ups;
    new_ups.state_no = new_state;
    new_ups.combined_states = old_states;
    ups.push_back(new_ups);
}

/// find the new state which combines states in a[].
/// Return:
///   state no. if found, or -1 if not found.
static auto
get_ups_state(const std::vector<UnitProdState>& ups,
              const std::vector<StateHandle>& a) -> std::optional<StateHandle>
{
    if (ups.empty())
        return std::nullopt;
    for (const auto& up : ups) {
        if (a == up.combined_states) {
            return up.state_no;
        }
    }
    return std::nullopt;
}

auto
Grammar::is_unit_production(StateHandle rule_no) const -> bool
{
    if (rule_no >= this->rules.size()) {
        throw std::runtime_error(
          std::string("is_unit_production error: array index (") +
          std::to_string(rule_no) + ") out of bound");
    }
    if (this->rules[rule_no]->nRHS.size() == 1 &&
        !this->rules[rule_no]->nRHS.front().snode->symbol->empty())
        return true;
    return false;
}

/*
 * Called by function insert_actionsOfCombinedStates().
 */
void
YAlgorithm::insert_action_of_symbol(std::shared_ptr<SymbolTableNode> symbol,
                                    const StateHandle new_state,
                                    const size_t old_state_index,
                                    const std::vector<StateHandle>& old_states)
{
    auto [action, state_dest] =
      get_action(symbol->type, get_col(*symbol), old_states[old_state_index]);

    if (action == 0)
        return;

    //printf("insert action %c to dest_state %d for new state \
  //%d on symbol %s\n", action, state_dest, new_state, symbol->symbol);

    if (action == 'a') {
        this->insert_action(symbol, new_state, ParsingAction::new_accept());
    } else if (action == 's' || action == 'g') {
        this->insert_action(
          symbol, new_state, ParsingAction::new_shift(state_dest));
    } else if (action == 'r') {
        if (!grammar.is_unit_production(state_dest)) {
            this->insert_action(
              symbol, new_state, ParsingAction::new_reduce(state_dest));
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
YAlgorithm::insert_actions_of_combined_states(
  const StateHandle new_state,
  const std::vector<StateHandle>& old_states)
{
    // copy actions of old_states to new_state.
    for (size_t i = 0; i < old_states.size(); i++) {
        // Copy action of end marker STR_END.
        this->insert_action_of_symbol(
          hash_tbl_find(STR_END), new_state, i, old_states);

        // copy actions of terminals.
        for (const auto& a : this->grammar.terminal_list) {
            this->insert_action_of_symbol(a.snode, new_state, i, old_states);
        }

        // copy actions of non_terminals.
        for (const auto& a : this->grammar.non_terminal_list) {
            this->insert_action_of_symbol(a.snode, new_state, i, old_states);
        }
    }
}

/// step 3. Delete transitions wrt. LHS of unit productions.
/// equivalent to remove all non-terminal goto actions.
/// new states don't have these, so just remove those
/// of the old state.
///
/// Actually, THIS IS NOT EVEN NECESSARY if all is needed
/// is stdout output. This is because after getting all
/// parent symbols we can ignore to output them in the
/// step writeFinalParsingTable!
static void
remove_unit_production_step3(const Grammar& grammar)
{
    for (size_t i = 0; i < ParsingTblRows; i++) {
        for (const auto& a : grammar.non_terminal_list) {
            // use "" as action and Error as dest state clears it.
            // Only those non-terminals y => x are cleared.
            if (is_parent_symbol(a.snode)) {
                update_action(get_col(*a.snode), i, ParsingAction::new_error());
            }
        }
    }
}

/// Check if elem is in vector v.
template<std::integral T>
static auto
vector_contains(const std::vector<T>& v, const T elem) -> bool
{
    for (const auto& x : v) {
        if (x == elem) {
            return true;
        }
    }
    return false;
}

/// Note that action 'g' applies to non-terminals only.
/// But include it does not affect the cases for STR_END
/// and terminals, which have only action 's'. So just
/// include checking for 'g' for all three situations.
static void
get_reachable_states_for_symbol(const Grammar& grammar,
                                const std::string_view symbol,
                                const size_t cur_state,
                                std::vector<size_t>& states_reachable)
{
    const std::shared_ptr<const SymbolTableNode> n = hash_tbl_find(symbol);

    auto [action, state_dest] = get_action(n->type, get_col(*n), cur_state);
    if ((action == 's' || action == 'g') &&
        !vector_contains(states_reachable, state_dest)) {
        states_reachable.push_back(state_dest);
        get_reachable_states(grammar, state_dest, states_reachable);
    }
}

/// In the parsing table, get states that are reachable
/// from cur_state, and stores the result in array
/// states_reachable[].
void
get_reachable_states(const Grammar& grammar,
                     const size_t cur_state,
                     std::vector<size_t>& states_reachable)
{
    get_reachable_states_for_symbol(
      grammar, STR_END, cur_state, states_reachable);

    for (const auto& a : grammar.terminal_list) {
        get_reachable_states_for_symbol(
          grammar, *a.snode->symbol, cur_state, states_reachable);
    }

    for (const auto& a : grammar.non_terminal_list) {
        if (!is_parent_symbol(a.snode)) {
            get_reachable_states_for_symbol(
              grammar, *a.snode->symbol, cur_state, states_reachable);
        }
    }
}

void
write_parsing_table_col_header(std::ostream& os, const Grammar& grammar)
{
    for (const auto& node : ParsingTblColHdr) {
        if (!is_goal_symbol(grammar, node)) {
            os << node->symbol << "\t";
        }
    }
    os << std::endl;
}

/*
 * get final parsing table column headers.
 * + 1 for end marker STR_END.
 */
static void
get_f_parsing_tbl_col_hdr(const Grammar& grammar)
{
    F_ParsingTblColHdr.clear();
    F_ParsingTblColHdr.emplace_back(hash_tbl_find(STR_END));

    for (const auto& a : grammar.terminal_list) {
        F_ParsingTblColHdr.emplace_back(a.snode);
    }

    for (const auto& a : grammar.non_terminal_list) {
        if (!is_parent_symbol(a.snode) && !is_goal_symbol(grammar, a.snode)) {
            F_ParsingTblColHdr.emplace_back(a.snode);
        }
    }
}

static void
write_final_parsing_table_col_header(std::ostream& os)
{
    for (const auto& a : F_ParsingTblColHdr) {
        os << a.snode->symbol << "\t";
    }
    os << std::endl;
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
static void
remove_unit_production_step4(const Grammar& grammar)
{
    states_reachable.clear();
    states_reachable.reserve(ParsingTblRows);
    get_reachable_states(grammar, 0, states_reachable);
    std::sort(states_reachable.begin(), states_reachable.end());

    if (Options::get().debug_remove_up_step_4) {
        grammar.fp_v << std::endl
                     << "--remove_" << std::endl
                     << "it_producti" << std::endl
                     << "_step4--\n";
        grammar.fp_v << "states reachable from state 0:" << std::endl;
        print_int_array(grammar.fp_v, states_reachable);
    }

    get_f_parsing_tbl_col_hdr(grammar);
}

auto
is_reachable_state(StateHandle state) -> bool
{
    if (state == 0 || vector_contains(states_reachable, state)) {
        return true;
    }
    return false;
}

/*
 * The parsing table array does not change,
 * only change the output entries.
 */
void
print_final_parsing_table(const Grammar& grammar)
{
    grammar.fp_v << std::endl << "--Pars" << std::endl << "g Table--\n";
    grammar.fp_v << "State\t";
    write_final_parsing_table_col_header(grammar.fp_v);

    for (size_t row = 0; row < ParsingTblRows; row++) {
        if (is_reachable_state(row)) {
            grammar.fp_v << row << "\t";
            for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
                std::shared_ptr<SymbolTableNode> n = ParsingTblColHdr[col];
                if (is_goal_symbol(grammar, n) == false &&
                    is_parent_symbol(n) == false) {
                    auto [action, state] = get_action(n->type, col, row);
                    grammar.fp_v << action << state << "\t";
                }
            }

            grammar.fp_v << std::endl;
        }
    }
    print_parsing_table_note(grammar.fp_v);
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
    for (size_t row = 0; row < row_size; row++) {
        if (row == 0 || vector_contains(states_reachable, row)) {
            actual_state_no.push_back(row);
            actual_state_no.push_back(i);
            i++;
        }
    }

    // printIntArray(actual_state_no, actual_state_no_ct);
}

auto
get_actual_state(StateHandle virtual_state) -> std::optional<StateHandle>
{
    for (size_t i = 0; i < actual_state_no.size(); i += 2) {
        if (virtual_state == actual_state_no[i])
            return actual_state_no[i + 1];
    }
    return std::nullopt; // this should not happen.
}

void
write_actual_state_array(std::ostream& os)
{
    constexpr int LINE_LENGTH = 5;
    if (!Options::get().use_remove_unit_production)
        return;

    os << std::endl << "\n--actual state array [actual, pseudo]--" << std::endl;
    for (size_t i = 0; i < actual_state_no.size(); i += 2) {
        if (i > 0 && i % LINE_LENGTH == 0)
            os << std::endl;
        os << "[" << actual_state_no[i] << ", " << actual_state_no[i + 1]
           << "] ";
    }
    os << std::endl << "\n";
}

/*
 * If an action is 's' or 'g', change its target state number
 * from virtual to actual.
 */
void
print_condensed_final_parsing_table(const Grammar& grammar)
{
    // value assigned at the end of generate_parsing_table().

    grammar.fp_v << std::endl
                 << "--F" << std::endl
                 << "al Pars" << std::endl
                 << "g Table--\n";
    grammar.fp_v << "State\t";
    write_final_parsing_table_col_header(grammar.fp_v);

    int i = 0;
    for (size_t row = 0; row < ParsingTblRows; row++) {
        if (is_reachable_state(row)) {
            grammar.fp_v << i << "\t";
            for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
                std::shared_ptr<SymbolTableNode> n = ParsingTblColHdr[col];
                if (is_goal_symbol(grammar, n) == false &&
                    is_parent_symbol(n) == false) {
                    auto [action, state_no] = get_action(n->type, col, row);
                    if (action == 's' || action == 'g')
                        state_no = *get_actual_state(state_no);
                    grammar.fp_v << action << state_no << "\t";
                }
            }

            i++;
            grammar.fp_v << std::endl;
        }
    }

    print_parsing_table_note(grammar.fp_v);
}

/*
 * This actually is not needed too (see step 3).
 * Because all we care in x -> a b c
 * is how many symbols we have on the RHS.
 */
static void
remove_unit_production_step5(const Grammar& grammar, const MRLeaves& mr_leaves)
{
    for (const auto& rule : grammar.rules) {
        int index = get_index_in_mr_parents(rule->nLHS->snode, *all_parents);
        if (index >= 0) {
            rule->nLHS = std::make_shared<SymbolNode>(
              mr_leaves[leaf_index_for_parent[index]]->symbol->snode);
        }
    }
}

void
YAlgorithm::remove_unit_production_step1and2(const MRLeaves& mr_leaves)
{
    bool debug_remove_up_step_1_2 = this->options.debug_remove_up_step_1_2;
    // as discussed in the function comments of getUnitProdShift(),
    // unitProdDestStates array is bounded by number of non_terminals + 1.
    std::vector<StateHandle> unit_prod_dest_states;
    std::vector<UnitProdState> ups;
    ups.reserve(UPS_SIZE);
    std::vector<std::shared_ptr<MRParents>> leaf_parents;

    // pre-calculate all parents for each leaf.
    for (size_t i = 0; i < mr_leaves.size(); i++) {
        leaf_parents.push_back(create_mr_parents());
        get_parents_for_mr_leaf(mr_leaves, i, leaf_parents[i].get());
    }

    if (debug_remove_up_step_1_2) {
        grammar.fp_v << std::endl
                     << "--remove_" << std::endl
                     << "it_producti" << std::endl
                     << "_step1" << std::endl
                     << "d2--\n";
        grammar.fp_v << "--writeUnitProdShift--" << std::endl;
    }

    // now, steps 1 and 2.
    for (size_t state = 0; state < ParsingTblRows; state++) {
        for (size_t i = 0; i < mr_leaves.size(); i++) {
            std::shared_ptr<SymbolTableNode> leaf = mr_leaves[i]->symbol->snode;
            const auto& parents = leaf_parents[i];

            get_unit_prod_shift(state, leaf, *parents, unit_prod_dest_states);

            if (!unit_prod_dest_states.empty()) { // unitProdCount >= 2
                std::optional<StateHandle> ups_state =
                  get_ups_state(ups, unit_prod_dest_states);
                if (!ups_state.has_value()) {
                    ups_state = ParsingTblRows;
                    ParsingTblRows++;
                    if (ParsingTblRows >= PARSING_TABLE_SIZE) {
                        // TODO
                        // expand_parsing_table(this->new_states.states_new_array);
                    }
                    create_new_ups_state(
                      ups, *ups_state, unit_prod_dest_states);

                    // Combine actions of states into state ups_state.
                    // Do this only if this combined state does not exist yet.
                    this->insert_actions_of_combined_states(
                      *ups_state, unit_prod_dest_states);
                }

                // Update the link from src_state to leaf transition state
                update_action(get_col(*leaf),
                              state,
                              ParsingAction::new_shift(*ups_state)); // shift.

                if (debug_remove_up_step_1_2) {
                    write_unit_prod_shift(grammar.fp_v,
                                          state,
                                          leaf,
                                          unit_prod_dest_states,
                                          *ups_state);
                }
            }
        }
    }
    if (debug_remove_up_step_1_2) {
        grammar.fp_v << "--after remove_unit_production_step1and2(), ";
        grammar.fp_v << "total states: " << ParsingTblRows << "--" << std::endl;
    }
}

////////////////////////////////////////////////////////
// Dr. Pager, Acta Informatica 9, 31-59 (1977), page 38.
////////////////////////////////////////////////////////
void
YAlgorithm::remove_unit_production()
{
    MRLeaves mr_leaves = build_multirooted_tree(this->grammar);

    remove_unit_production_step1and2(mr_leaves);
    remove_unit_production_step3(this->grammar);
    remove_unit_production_step4(this->grammar);
    remove_unit_production_step5(this->grammar, mr_leaves);

    this->n_state_opt12 = states_reachable.size() + 1;
}

/////////////////////////////////////////////////////
// Functions for removing unit productions. End.
/////////////////////////////////////////////////////

/////////////////////////////////////////////////////
// Functions for further optimization. Start.
/////////////////////////////////////////////////////

/// Determine if rows i and j in the parsing table are the same.
static auto
is_equal_row(const StateHandle i, const StateHandle j) -> bool
{
    for (const auto& n : ParsingTblColHdr) {
        auto [action_i, state_dest_i] = get_action(n->type, get_col(*n), i);
        auto [action_j, state_dest_j] = get_action(n->type, get_col(*n), j);
        if (action_i != action_j || state_dest_i != state_dest_j)
            return false;
    }

    return true;
}

/// In parsing table row, replace entries whose
/// target state is old_state by new_state.
static void
update_repeated_row(const Grammar& grammar,
                    const StateHandle new_state,
                    const StateHandle old_state,
                    const StateHandle row)
{
    // std::cout << "In row " << row << ", replace " << old_state << " by "
    //           << new_state << std::endl;

    // for end marker column STR_END
    std::shared_ptr<SymbolTableNode> n = hash_tbl_find(STR_END);
    auto [action, state_dest] = get_action(n->type, get_col(*n), row);
    if (state_dest == old_state)
        update_action(get_col(*hash_tbl_find(STR_END)),
                      row,
                      ParsingAction::new_shift(new_state));

    // for terminal columns
    for (const auto& a : grammar.terminal_list) {
        n = a.snode;
        auto [action, state_dest] = get_action(n->type, get_col(*n), row);
        if (state_dest == old_state)
            update_action(
              get_col(*a.snode), row, ParsingAction::new_shift(new_state));
    }

    // for non-terminal columns
    for (const auto& a : grammar.non_terminal_list) {
        n = a.snode;
        auto [action, state_dest] = get_action(n->type, get_col(*n), row);
        if (state_dest == old_state)
            update_action(
              get_col(*a.snode), row, ParsingAction::new_shift(new_state));
    }
}

/*
 * Go through the entire parsing table (only those reachable
 * states, to save time), replace those entries
 * whose target state is old_state by new_state.
 */
static void
update_repeated_rows(const Grammar& grammar,
                     const StateHandle new_state,
                     const StateHandle old_state)
{
    update_repeated_row(grammar, new_state, old_state, 0); // row 0.

    for (const auto& state_reachable : states_reachable)
        update_repeated_row(grammar, new_state, old_state, state_reachable);
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
YAlgorithm::further_optimization()
{
    for (size_t k = 0; k < states_reachable.size() - 1; k++) {
        StateHandle i = states_reachable[k];
        StateHandle j = states_reachable[k + 1];
        // std::cout << "furtherOpt: i = " <<  i<< ", j = " <<  j << std::endl;
        do {
            if (is_equal_row(i, j) == false)
                break;

            update_repeated_rows(grammar, i, j);
            // std::cout << "state " <<  states_reachable[k + 1]<< " removed" <<
            // std::endl;
            states_reachable.erase(states_reachable.begin() +
                                   static_cast<ptrdiff_t>(k) + 1);
            if ((k + 1) == states_reachable.size())
                break;
            j = states_reachable[k + 1];
            // std::cout << "- furtherOpt: i = " <<  i<< ", j = " <<  j <<
            // std::endl;
        } while (true);
    }

    this->n_state_opt123 = states_reachable.size() + 1;

    if (this->options.show_parsing_tbl &&
        (this->n_state_opt12 > this->n_state_opt123)) {
        this->grammar.fp_v << "After further optimization, ";
        this->grammar.fp_v << "total states reduced from "
                           << this->n_state_opt12 << " to "
                           << this->n_state_opt123 << std::endl;
    }
}

/////////////////////////////////////////////////////
// Functions for further optimization. End.
/////////////////////////////////////////////////////
