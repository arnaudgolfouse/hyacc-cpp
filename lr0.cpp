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
#include <fstream>
#include <memory>
#include <optional>

/// Add a new `Configuration` to `s.config`.
static void
add_successor_config_to_state_lr0(const Grammar& grammar,
                                  std::shared_ptr<State> s,
                                  size_t rule_id)
{
    // marker = 0, isCoreConfig = 0.
    Configuration* c = create_config(grammar, rule_id, 0, 0);
    c->owner = s;
    s->config.push_back(c);
}

/// Assumption: config_queue contains the configurations to be processed.
static void
get_config_successors_lr0(const Grammar& grammar,
                          Queue& config_queue,
                          std::shared_ptr<State> s)
{
    while (config_queue.size() > 0) {
        const Configuration& config = *s->config[*config_queue.pop()];

        if (config.marker >= 0 &&
            config.marker < grammar.rules[config.ruleID]->nRHS.size()) {
            std::shared_ptr<SymbolTableNode> scanned_symbol =
              get_scanned_symbol(config);

            if (scanned_symbol->is_non_terminal()) {

                for (RuleIDNode* r = scanned_symbol->ruleIDList; r != nullptr;
                     r = r->next) {
                    // Grammar.rules[r->ruleID] starts with this scanned symbol.

                    // If not an existing config, add to state s.
                    std::optional<size_t> index =
                      is_compatible_successor_config(s, r->rule_id);

                    if (!index.has_value()) { // new config.
                        add_successor_config_to_state_lr0(
                          grammar, s, r->rule_id);
                        // s->config is non-empty here (see beginning of the
                        // loop)
                        config_queue.push(s->config.size() - 1);
                    } // else is an existing old config, do nothing.
                }
            } // else, is a terminal, stop.
        }     // end if config-marker >= 0 ...
    }
}

static void
get_closure_lr0(const Grammar& grammar,
                Queue& config_queue,
                std::shared_ptr<State> s)
{
    // config_queue->clear();
    for (size_t i = 0; i < s->config.size(); i++) {
        config_queue.push(i);
    }
    get_config_successors_lr0(grammar, config_queue, s);
}

/// For LR(0). Insert a/r actions to the ENTIRE row.
void
LR0::insert_reduction_to_parsing_table_lr0(const Configuration& c,
                                           const StateHandle state_no)
{
    size_t max_col = this->grammar.terminal_list.size() + 1;

    if (grammar.rules.at(c.ruleID)->nLHS->snode ==
        grammar.goal_symbol->snode) { // accept, action = "a";
        // note this should happen only if the end is $end.
        this->insert_action(
          hash_tbl_find(STR_END), state_no, ParsingAction::new_accept());
    } else { // reduct, action = "r";
        for (size_t col = 0; col < max_col; col++) {
            std::shared_ptr<SymbolTableNode> n = this->ParsingTblColHdr.at(col);
            this->insert_action(
              n, state_no, ParsingAction::new_reduce(c.ruleID));
        }
    }
}

/// if context set is empty, fill all cells in the ENTIRE row;
/// otherwise, fill cells with lookaheads in the context set.
void
LR0::insert_reduction_to_parsing_table_lalr(const Configuration& c,
                                            const StateHandle state_no)
{
    size_t max_col = this->grammar.terminal_list.size() + 1;

    if (this->grammar.rules[c.ruleID]->nLHS->snode ==
        this->grammar.goal_symbol->snode) { // accept, action = "a";
        // note this should happen only if the end is $end.
        this->insert_action(
          hash_tbl_find(STR_END), state_no, ParsingAction::new_accept());
    } else { // reduct, action = "r";
        if (!c.context.context.empty()) {
            for (const auto& a : c.context.context) {
                this->insert_action(
                  a.snode, state_no, ParsingAction::new_reduce(c.ruleID));
            }
        } else {
            for (size_t col = 0; col < max_col; col++) {
                std::shared_ptr<SymbolTableNode> n =
                  this->ParsingTblColHdr[col];
                this->insert_action(
                  n, state_no, ParsingAction::new_reduce(c.ruleID));
            }
        }
    }
}

/// For each of the new temp states,
///   If it is not one of the existing states in
///     states_new, then add it to states_new.
///   Also always add the transition to parsing table.
///
/// Note:
/// The "is_compatible" variable is of NO use here,
/// since configurations don't have contexts. So such
/// states are always the "same", but not compatible.
void
LR0::add_transition_states2_new_lr0(const StateCollection& coll,
                                    std::shared_ptr<State> src_state)
{
    std::shared_ptr<State> next = nullptr;
    std::shared_ptr<State> s = coll.states_head;

    while (s != nullptr) {
        next = s->next;
        // search_same_state_hash_tbl() checks for SAME (not compatible) states.
        std::shared_ptr<State> os = this->state_hash_table.search_same_state(s);
        if (os == nullptr) {
            new_states.insert_state_to_pm(s);

            // Add this new state as successor to src_state.
            add_successor(src_state, s);
        } else { // same with an existing state.
            add_successor(src_state, os);
        }

        s = next;
    } // end of while.
}

/// Perform transition operation on a state to get successors.
///
/// For each config c in the state s,
///   If c is a final config, stop
///   Else, get the scanned symbol x,
///     If a new temp state for x does not exist yet, create it.
///     add x to the temp state.
/// (Now get several new temp states)
/// Add these new temp states to states_new if not existed,
/// and add transition to parsing table as well.
void
LR0::transition_lr0(std::shared_ptr<State> s) noexcept
{
    StateCollection& coll = *create_state_collection();

    for (const auto& c : s->config) {
        if (is_final_configuration(this->grammar, c)) {
            // do nothing.
        } else { // do transit operation.
            std::shared_ptr<SymbolTableNode> scanned_symbol =
              get_scanned_symbol(*c);
            if (scanned_symbol->symbol->empty()) { // empty reduction.
                continue;
            }
            std::shared_ptr<State> new_state =
              find_state_for_scanned_symbol(&coll, scanned_symbol);
            if (new_state == nullptr) {
                new_state = std::make_shared<State>();
                // record which symbol this state is a successor by.
                new_state->trans_symbol =
                  std::make_shared<SymbolNode>(scanned_symbol);
                coll.add_state2(new_state);
            }
            // create a new core config for new_state.
            Configuration& new_config =
              *create_config(this->grammar, c->ruleID, 0, 1);

            new_config.owner = new_state;
            copy_config(new_config, *c);
            new_config.isCoreConfig = 1;
            new_config.marker++;
            if (!new_config.nMarker.empty())
                new_config.nMarker.pop_front();

            add_core_config2_state(this->grammar, new_state, &new_config);
        }
    }

    if (coll.state_count > 0) {
        this->add_transition_states2_new_lr0(coll, s);
    }
}

/// First fill acc, s, g.
/// Then fill r. r is filled to terminal symbols only. And if
/// a cell is already a/s/g, don't fill this r.
void
LR0::output_parsing_table_row(const std::shared_ptr<const State> s)
{
    // insert a/r actions.
    for (const auto& c : s->config) {
        if (is_final_configuration(this->grammar, c) ||
            is_empty_production(this->grammar, c)) {
            this->insert_reduction_to_parsing_table_lr0(*c, s->state_no);
        }
    }

    // insert s/g actions.
    for (const auto& t : s->successor_list) {
        this->insert_action(t->trans_symbol->snode,
                            s->state_no,
                            ParsingAction::new_shift(t->state_no));
    }
}

void
LR0::output_parsing_table_row_lalr(const std::shared_ptr<const State> s)
{
    // insert a/r actions.
    for (const auto& c : s->config) {
        // std::cout  <<  s->state_no<< "." <<  c->ruleID << std::endl;

        if (is_final_configuration(this->grammar, c) ||
            is_empty_production(this->grammar, c)) {
            this->insert_reduction_to_parsing_table_lalr(*c, s->state_no);
        }
    }

    // insert s/g actions.
    for (const auto& t : s->successor_list) {
        this->insert_action(t->trans_symbol->snode,
                            s->state_no,
                            ParsingAction::new_shift(t->state_no));
    }
}

/*
 * Output parsing table from the parsing machine.
 * This is different from before. The previous implementation
 * for LR(1) is this is done when transitioning states. Now for
 * LR(0) since all default actions are reduce, the previous
 * method does not work very well.
 */
void
LR0::output_parsing_table() noexcept
{
    size_t rows = this->new_states.states_new_array.size();
    int cols = n_symbol + 1;

    // expand size of parsing table array if needed.
    if (rows * this->ParsingTblColHdr.size() >= PARSING_TABLE_SIZE) {
        // TODO
        // expand_parsing_table(this->new_states.states_new_array);
    }

    for (size_t i = 0; i < cols * rows; ++i) {
        this->ParsingTable.at(i) = std::nullopt;
    }

    for (size_t i = 0; i < rows; i++) {
        const std::shared_ptr<State> s =
          this->new_states.states_new_array[i].state;
        this->output_parsing_table_row(s);
    }
}

void
LR0::output_parsing_table_lalr()
{
    size_t rows = this->new_states.states_new_array.size();
    int cols = n_symbol + 1;

    // expand size of parsing table array if needed.
    while (rows >= PARSING_TABLE_SIZE) {
        // TODO
        // expand_parsing_table(*this->new_states.states_new_array);
    }

    for (size_t i = 0; i < cols * rows; ++i) {
        this->ParsingTable.at(i) = std::nullopt;
    }

    for (size_t i = 0; i < rows; i++) {
        const std::shared_ptr<State> s =
          this->new_states.states_new_array[i].state;
        this->output_parsing_table_row_lalr(s);
    }
}

void
LR0::update_parsing_table() noexcept
{
    ParsingTblRows = this->new_states.states_new->state_count;
    this->n_state_opt1 = this->new_states.states_new->state_count;

    // this fills the conflict list, so is need for lalr processing.
    this->output_parsing_table();
}

void
LR0::generate_lr0_parsing_machine(Queue& config_queue)
{
    std::shared_ptr<State> new_state = this->new_states.states_new->states_head;

    if (this->options.debug_gen_parsing_machine) {
        this->grammar.fp_v << std::endl
                           << std::endl
                           << "--generate parsing machine--" << std::endl;
    }

    while (new_state != nullptr) {
        if (this->options.debug_gen_parsing_machine) {
            this->grammar.fp_v << this->new_states.states_new->state_count
                               << " states, current state is "
                               << new_state->state_no << std::endl;
        }

        get_closure_lr0(
          this->grammar, config_queue, new_state); // get closure of this state.

        // get successor states and add them to states_new.
        this->transition_lr0(new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    this->update_parsing_table();
}
