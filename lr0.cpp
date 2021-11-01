/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2008, 2009 Xin Chen. chenx@hawaii.edu

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
 * LR(0) functions.
 *
 * @Author: Xin Chen
 * @Created on: 2/24/2008
 * @Last modified: 3/24/2009
 * @Copyright (C) 2008, 2009
 */

#include "y.hpp"
#include <cstddef>
#include <cstring>
#include <fstream>

static void
propagate_originator_change(State* s);

static void
add_successor_config_to_state_lr0(const Grammar& grammar, State* s, int rule_id)
{
    // marker = 0, isCoreConfig = 0.
    Configuration* c = create_config(grammar, rule_id, 0, 0);
    c->owner = s;
    s->config.push_back(c);
}

/*
 * Assumption: public variable config_queue contains
 * the configurations to be processed.
 */
static void
get_config_successors_lr0(const Grammar& grammar, State* s)
{
    while (config_queue->count() > 0) {
        Configuration* config = s->config[config_queue->pop()];

        if (config->marker >= 0 &&
            config->marker < grammar.rules[config->ruleID]->RHS_count) {
            SymbolTblNode* scanned_symbol = get_scanned_symbol(config);

            if (scanned_symbol->is_non_terminal()) {

                for (RuleIDNode* r = scanned_symbol->ruleIDList; r != nullptr;
                     r = r->next) {
                    // Grammar.rules[r->ruleID] starts with this scanned symbol.

                    // If not an existing config, add to state s.
                    int index = is_compatible_successor_config(s, r->ruleID);

                    if (index == -1) { // new config.
                        add_successor_config_to_state_lr0(
                          grammar, s, r->ruleID);
                        config_queue->push(static_cast<int>(s->config.size()) -
                                           1);
                        index = static_cast<int>(s->config.size()) - 1;
                    } // else is an existing old config, do nothing.

                } // end for
            }     // else, is a terminal, stop.
        }         // end if config-marker >= 0 ...
    }             // end of while
}

static void
get_closure_lr0(const Grammar& grammar, State* s)
{
    // config_queue->clear();
    for (int i = 0; i < s->config.size(); i++) {
        config_queue->push(i);
    }
    get_config_successors_lr0(grammar, s);
}

/*
 * For LR(0). Insert a/r actions to the ENTIRE row.
 */
static void
insert_reduction_to_parsing_table_lr0(const Grammar& grammar,
                                      Configuration* c,
                                      int state_no)
{
    int max_col = grammar.terminal_count + 1;

    if (grammar.rules[c->ruleID]->nLHS->snode ==
        grammar.goal_symbol->snode) { // accept, action = "a";
        // note this should happen only if the end is $end.
        insert_action(grammar, hash_tbl_find(STR_END), state_no, CONST_ACC);
    } else { // reduct, action = "r";
        for (int col = 0; col < max_col; col++) {
            SymbolTblNode* n = ParsingTblColHdr[col];
            insert_action(grammar, n, state_no, (-1) * c->ruleID);
        }
    }
}

/*
 * if context set is empty, fill all cells in the ENTIRE row;
 * otherwise, fill cells with lookaheads in the context set.
 */
static void
insert_reduction_to_parsing_table_lalr(const Grammar& grammar,
                                       Configuration* c,
                                       int state_no)
{
    int max_col = grammar.terminal_count + 1;

    if (grammar.rules[c->ruleID]->nLHS->snode ==
        grammar.goal_symbol->snode) { // accept, action = "a";
        // note this should happen only if the end is $end.
        insert_action(grammar, hash_tbl_find(STR_END), state_no, CONST_ACC);
    } else { // reduct, action = "r";
        SymbolNode* a = c->context->nContext;
        if (a != nullptr) {
            for (a = c->context->nContext; a != nullptr; a = a->next) {
                insert_action(grammar, a->snode, state_no, (-1) * c->ruleID);
            }
        } else {
            for (int col = 0; col < max_col; col++) {
                SymbolTblNode* n = ParsingTblColHdr[col];
                insert_action(grammar, n, state_no, (-1) * c->ruleID);
            }
        }
    }
}

/*
 * For each of the new temp states,
 *   If it is not one of the existing states in
 *     states_new, then add it to states_new.
 *   Also always add the transition to parsing table.
 *
 * Note:
 * The "is_compatible" variable is of NO use here,
 * since configurations don't have contexts. So such
 * states are always the "same", but not compatible.
 */
void
add_transition_states2_new_lr0(StateCollection* coll, State* src_state)
{
    State *os = nullptr, *next = nullptr, *s = nullptr;
    s = coll->states_head;

    while (s != nullptr) {
        next = s->next;

        // searchSameStateHashTbl() checks for SAME (not compatible) states.
        if ((os = search_same_state_hash_tbl(s)) == nullptr) {
            insert_state_to_pm(s);

            // Add this new state as successor to src_state.
            add_successor(src_state, s);

        } else { // same with an existing state.
            add_successor(src_state, os);
            State::destroy_state(s); // existing or compatible. No use.
        }

        s = next;
    } // end of while.
}

/*
 * Perform transition operation on a state to get successors.
 *
 * For each config c in the state s,
 *   If c is a final config, stop
 *   Else, get the scanned symbol x,
 *     If a new temp state for x does not exist yet, create it.
 *     add x to the temp state.
 * (Now get several new temp states)
 * Add these new temp states to states_new if not existed,
 * and add transition to parsing table as well.
 */
static void
transition_lr0(const Grammar& grammar, State* s)
{
    StateCollection* coll = create_state_collection();

    for (const auto& c : s->config) {
        if (is_final_configuration(grammar, c)) {
            // do nothing.
        } else { // do transit operation.
            SymbolTblNode* scanned_symbol = get_scanned_symbol(c);
            if (scanned_symbol->symbol->empty()) { // empty reduction.
                continue;
            }
            State* new_state =
              find_state_for_scanned_symbol(coll, scanned_symbol);
            if (new_state == nullptr) {
                new_state = create_state();
                // record which symbol this state is a successor by.
                new_state->trans_symbol = SymbolNode::create(scanned_symbol);
                coll->add_state2(new_state);
            }
            // create a new core config for new_state.
            Configuration* new_config = create_config(grammar, -1, 0, 1);

            new_config->owner = new_state;
            copy_config(new_config, c);
            new_config->isCoreConfig = 1;
            new_config->marker++;
            if (new_config->nMarker != nullptr)
                new_config->nMarker = new_config->nMarker->next;

            add_core_config2_state(grammar, new_state, new_config);
        }
    } // end for

    if (coll->state_count > 0) {
        add_transition_states2_new_lr0(coll, s);
    }
}

/*
 * First fill acc, s, g.
 * Then fill r. r is filled to terminal symbols only. And if
 * a cell is already a/s/g, don't fill this r.
 */
static void
output_parsing_table_row_lr0(const Grammar& grammar, State* s)
{
    // std::cout  << std::endl<< "state " <<  s->state_no<< ". config count: "
    // <<  ct << std::endl;

    // insert a/r actions.
    for (const auto& c : s->config) {
        // std::cout  <<  s->state_no<< "." <<  c->ruleID << std::endl;

        if (is_final_configuration(grammar, c) ||
            is_empty_production(grammar, c)) {
            insert_reduction_to_parsing_table_lr0(grammar, c, s->state_no);
        }
    }

    // insert s/g actions.
    for (State* t : s->successor_list) {
        insert_action(
          grammar, t->trans_symbol->snode, s->state_no, t->state_no);
    }
}

static void
output_parsing_table_row_lalr(const Grammar& grammar, State* s)
{
    // insert a/r actions.
    for (const auto& c : s->config) {
        // std::cout  <<  s->state_no<< "." <<  c->ruleID << std::endl;

        if (is_final_configuration(grammar, c) ||
            is_empty_production(grammar, c)) {
            insert_reduction_to_parsing_table_lalr(grammar, c, s->state_no);
        }
    }

    // insert s/g actions.
    for (State* t : s->successor_list) {
        insert_action(
          grammar, t->trans_symbol->snode, s->state_no, t->state_no);
    }
}

/*
 * Output parsing table from the parsing machine.
 * This is different from before. The previous implementation
 * for LR(1) is this is done when transitioning states. Now for
 * LR(0) since all default actions are reduce, the previous
 * method does not work very well.
 */
static void
output_parsing_table_lr0(const Grammar& grammar)
{
    size_t rows = states_new_array->state_list.size();
    int cols = n_symbol + 1;

    // expand size of parsing table array if needed.
    if (rows * ParsingTblCols >= PARSING_TABLE_SIZE) {
        expand_parsing_table();
    }

    for (size_t i = 0; i < cols * rows; ++i) {
        ParsingTable.at(i) = 0;
    }

    for (int i = 0; i < rows; i++) {
        State* s = states_new_array->state_list[i];
        output_parsing_table_row_lr0(grammar, s);
    }
}

/*
 *
 */
void
output_parsing_table_lalr(const Grammar& grammar)
{
    size_t rows = states_new_array->state_list.size();
    int cols = n_symbol + 1;

    // expand size of parsing table array if needed.
    while (rows >= PARSING_TABLE_SIZE) {
        expand_parsing_table();
    }

    for (size_t i = 0; i < cols * rows; ++i) {
        ParsingTable.at(i) = 0;
    }

    for (int i = 0; i < rows; i++) {
        State* s = states_new_array->state_list[i];
        output_parsing_table_row_lalr(grammar, s);
    }
}

void
update_parsing_table_lr0(const Grammar& grammar)
{
    ParsingTblRows = states_new->state_count;
    n_state_opt1 = states_new->state_count;

    // this fills the conflict list, so is need for lalr processing.
    output_parsing_table_lr0(grammar);
}

void
generate_lr0_parsing_machine(const Grammar& grammar)
{
    bool debug_gen_parsing_machine = Options::get().debug_gen_parsing_machine;
    State* new_state = states_new->states_head;

    if (debug_gen_parsing_machine) {
        *fp_v << std::endl
              << std::endl
              << "--generate parsing machine--" << std::endl;
    }

    while (new_state != nullptr) {
        if (debug_gen_parsing_machine) {
            *fp_v << states_new->state_count << " states, current state is "
                  << new_state->state_no << std::endl;
        }

        get_closure_lr0(grammar, new_state); // get closure of this state.

        // get successor states and add them to states_new.
        transition_lr0(grammar, new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    update_parsing_table_lr0(grammar);
}
