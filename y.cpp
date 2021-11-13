/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007, 2008, 2009 Xin Chen. chenx@hawaii.edu

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

/****************************************************************
 * y.c
 *
 * Generates a LR(1) parsing machine given a grammar.
 *
 * @author: Xin Chen
 * @Date Started: 8/30/2006
 * @Last modified: 3/24/2009
 ****************************************************************/

#include "y.hpp"
#include "lane_tracing.hpp"
#include <array>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <ranges>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

Options Options::
  inner{}; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
std::atomic_int MAX_K;

std::array<HashTblNode, HT_SIZE> HashTbl;
// Grammar grammar;
std::atomic_size_t PARSING_TABLE_SIZE;
std::vector<std::optional<ParsingAction>> ParsingTable;
size_t ParsingTblRows;
std::vector<std::shared_ptr<SymbolTableNode>> ParsingTblColHdr;
SymbolList F_ParsingTblColHdr;
std::vector<StateHandle> states_reachable;
std::vector<StateHandle> actual_state_no;
int n_symbol;
size_t n_rule;
size_t n_rule_opt;
std::vector<StateHandle> final_state_list;
StateNoArray* states_inadequate;

/* Declaration of functions. */
static void
init_parsing_table();
static void
free_config(Configuration* c);
static auto
is_compatible_config(const Configuration* c1, const Configuration* c2) -> bool;
static void
write_state_transition_list(std::ostream& os,
                            const Grammar& grammar,
                            const NewStates& new_states);

std::string hyacc_filename;

auto
Grammar::get_rule_count() const noexcept -> size_t
{
    return this->rules.size();
}

/*
 * For StateList. Start.
 */

auto
StateList::create() -> std::shared_ptr<StateList>
{
    constexpr size_t INITIAL_SIZE = 5;
    auto l = std::make_shared<StateList>();
    l->state_list.reserve(INITIAL_SIZE);
    return l;
}

/*
 * Add if not exist yet.
 * @Return: true is added, false if not added.
 */
auto
StateList::add(std::shared_ptr<State> s) -> bool
{
    if (s == nullptr)
        return false;
    for (const auto& elem : this->state_list) {
        if (elem->state_no == s->state_no)
            return false;
    }
    this->state_list.push_back(s);
    return true;
}

auto
StateList::clone() -> std::shared_ptr<StateList>
{
    return std::make_shared<StateList>(*this);
}

auto
operator<<(std::ostream& os, const StateList& state_list) -> std::ostream&
{
    os << std::endl
       << "  parents_list(" << state_list.state_list.size() << "): ";
    for (size_t i = 0; i < state_list.state_list.size(); i++) {
        if (i > 0)
            os << ", ";
        os << state_list.state_list.at(i)->state_no;
    }
    os << std::endl;
    return os;
}

/*
 * For StateList. End.
 */

void
Production::write(std::ostream& os, std::optional<size_t> marker) const noexcept
{
    os << *this->nLHS->snode->symbol << " ";
    os << "-> ";

    size_t i = 0;
    for (const auto& n : this->nRHS) {
        if (i == marker)
            os << ". ";
        os << *n.snode->symbol << " ";
        i++;
    }
    if (i == marker)
        os << ". ";

    // print this only when marker = nullopt.
    // i.e. called from write_grammar().
    if (!marker.has_value() && this->isUnitProduction)
        os << "(unit production)";

    if (!marker.has_value() && this->lastTerminal != nullptr)
        os << " (Precedence Terminal: " << *this->lastTerminal->symbol << ")";

    // if write configuration, then don't go to new line.
    // since the context has not been written.
    if (!marker.has_value())
        os << std::endl;
}

void
Grammar::write_rules(std::ostream& os) const
{
    int count = 0;
    os << "Rules: " << std::endl;
    for (const auto& rule : this->rules) {
        os << "(" << count << ") ";
        rule->write(os, -1);
        count++;
    }
    os << "Number of Rules: " << count << std::endl;
}

void
Grammar::write_rules_no_unit_prod(std::ostream& os) const
{
    int count = 0;
    int i = 0;
    os << "Rules: " << std::endl;
    for (const auto& rule : this->rules) {
        if (!this->is_unit_production(i) || i == 0) {
            os << "(" << i << ") ";
            rule->write(os, -1);
            count++;
        }
        i++;
    }
    os << "Number of Rules: " << count << std::endl;
}

auto
Grammar::get_opt_rule_count() const noexcept -> size_t
{
    size_t count = 0;
    for (size_t i = 0; i < this->rules.size(); i++) {
        if (!this->is_unit_production(i) || i == 0)
            count++;
    }
    return count;
}

void
Grammar::write_terminals(std::ostream& os) const
{
    os << "Terminals (" << this->terminal_list.size() << "): " << std::endl;

    for (const auto& a : this->terminal_list) {
        os << *a.snode->symbol << std::endl;
    }
    os << std::endl;
}

void
Grammar::write_non_terminals(std::ostream& os) const
{
    os << "Non-terminals (" << this->non_terminal_list.size()
       << "): " << std::endl;
    for (const auto& a : this->non_terminal_list) {
        os << *a.snode->symbol << std::endl;
    }
    os << std::endl;
}

void
Grammar::write_vanish_symbols(std::ostream& os) const
{
    os << "Vanish symbols (" << this->vanish_symbol_list.size()
       << "): " << std::endl;
    for (const auto& a : this->vanish_symbol_list) {
        os << *a.snode->symbol << std::endl;
    }
    os << std::endl;
}

void
Grammar::write(std::ostream& os,
               const bool before_rm_unit_prod,
               const bool use_remove_unit_production) const noexcept
{
    os << std::endl << "--Grammar--\n";
    this->write_terminals(os);
    this->write_non_terminals(os);
    this->write_vanish_symbols(os);
    os << "Goal symbol: " << *this->goal_symbol->snode->symbol << std::endl;

    if (before_rm_unit_prod || !use_remove_unit_production) {
        this->write_rules(os);
    } else { // after remove unit production.
        this->write_rules_no_unit_prod(os);
    }
    os << std::endl;
}

void
NewStates::inc_conflict_count(const ParsingAction s,
                              const size_t state) noexcept
{
    if (s.is_shift()) {
        this->conflicts_count.rs++;
        this->states_new_array.at(state).rs_count++;
    } else if (s.is_reduce()) {
        this->conflicts_count.rr++;
        this->states_new_array.at(state).rr_count++;
    }
}

auto
NewStates::add_to_conflict_array(
  const StateHandle state,
  const std::shared_ptr<SymbolTableNode> lookahead,
  const ParsingAction action1,
  const ParsingAction action2) -> std::shared_ptr<ConflictNode>
{
    ParsingAction r = ParsingAction::new_accept(),
                  s = ParsingAction::new_accept(); // r < s
    Conflict* b_prev = nullptr;

    if ((action1.is_reduce() && action2.is_shift()) ||
        (action1.is_reduce() && action2.is_reduce() &&
         action1.reduce_value() < action2.reduce_value()) ||
        (action1.is_shift() && action2.is_shift() &&
         action1.shift_value() < action2.shift_value())) {
        r = action1;
        s = action2;
    } else {
        r = action2;
        s = action1;
    }

    if (this->states_new_array.at(state).rr_count == 0 &&
        this->states_new_array.at(state).rs_count == 0) {
        std::shared_ptr<ConflictNode> c =
          std::make_shared<ConflictNode>(state, lookahead, r, s);
        this->states_new_array.at(state).conflict = c;
        this->inc_conflict_count(s, state);
        return c;
    }

    for (std::shared_ptr<Conflict> b =
           this->states_new_array.at(state).conflict;
         b != nullptr;
         b_prev = b.get(), b = b->next) {
        if (state == b->state && lookahead == b->lookahead && r == b->r &&
            s == b->s)
            return nullptr; // exits already.
        if (state < b->state) {
            std::shared_ptr<ConflictNode> c =
              std::make_shared<ConflictNode>(state, lookahead, r, s);

            if (b_prev == nullptr) { // insert at the head.
                c->next = this->states_new_array.at(state).conflict;
                this->states_new_array.at(state).conflict = c;
            } else { // insert in the middle.
                c->next = b;
                b_prev->next = c;
            }
            this->inc_conflict_count(s, state);
            return c;
        }
    }

    std::shared_ptr<ConflictNode> c =
      std::make_shared<ConflictNode>(state, lookahead, r, s);
    b_prev->next = c; // insert at the tail.
    this->inc_conflict_count(s, state);

    return c;
}

/*
 * Initialize variables when the program starts.
 * Called by function main().
 */
void
YAlgorithm::init()
{
    this->new_states.states_new = create_state_collection();
    this->new_states.states_new_array.clear(); // size == PARSING_TABLE_SIZE

    if (this->options.use_lalr) {
        states_inadequate = create_state_no_array();
    }

    if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
        this->config_queue->clear(); // for getClosure().
    }

    // for finding same/compatible states fast.
    this->state_hash_table.init();
    this->init_start_state();
    init_parsing_table();
}

auto
operator<<(std::ostream& os, const Context& context) -> std::ostream&
{
    os << " {";

    bool first_s = true;
    for (const auto& s : context.context) {
        if (first_s) {
            first_s = false;
        } else {
            os << ", ";
        }
        os << *s.snode->symbol;
    }
    os << "} ";
    return os;
}

static void
write_configuration(std::ostream& os,
                    const Grammar& grammar,
                    const Configuration& c)
{
    auto& options = Options::get();
    grammar.rules.at(c.ruleID)->write(os, c.marker);

    if (options.use_lr0 && !options.use_lalr) { // LR(0), no context.
        // do nothing unless is goal production.
        if (c.ruleID == 0)
            os << STR_END;
    } else {
        os << c.context;
    }

    if (c.isCoreConfig == 1u)
        os << " (core) ";

    if (options.use_lalr && options.show_originators) {
        if (c.LANE_END == 1u) {
            os << " [LANE_END]";
        }
        if (c.LANE_CON == 1u) {
            os << " [LANE_CON]";
        }
        if (c.COMPLETE == 1u) {
            os << " [COMPLETE]";
        }
        os << std::endl;
        // os << " [owner: " <<  c->owner->state_no<< "]" ;
        c.write_originators(os); /* in lane_tracing.c */
        c.write_transitors(os);  /* in lane_tracing.c */
    } else {
        os << std::endl;
    }
}

static void
write_successor_list(std::ostream& os, State& s)
{
    if (!s.successor_list.empty())
        os << std::endl;
    // os << std::endl << "-successor list-\n" ;
    for (const auto& successor : s.successor_list) {
        os << *successor->trans_symbol->snode->symbol << " : "
           << successor->state_no << std::endl;
    }
}

static void
write_state_conflict_list(std::ostream& os,
                          std::optional<StateHandle> state,
                          const StateArray& states_array)
{
    if (Options::get().use_remove_unit_production && state.has_value()) {
        state = get_actual_state(*state);
    }

    if (!state.has_value())
        return;
    StateHandle state_unwrapped = *state;

    if (states_array.at(state_unwrapped).rr_count == 0 &&
        states_array.at(state_unwrapped).rs_count == 0)
        return;

    auto c = states_array.at(state_unwrapped).conflict;
    for (; c != nullptr; c = c->next) {
        os << c->state << ": ";
        if (c->s.is_shift()) {
            os << "shift/reduce conflict ";
            os << "( " << c->s << ", red'n " << c->r << ")";
        } else {
            os << "reduce/reduce conflict ";
            os << "red'n " << c->s << ", red'n " << c->r << "]";
        } // end if
        os << " on '" << *c->lookahead->symbol << "'" << std::endl;
    } // end for
}

void
NewStates::write_grammar_conflict_list(std::ostream& os) const noexcept
{
    if (this->conflicts_count.rs == 0 && this->conflicts_count.rr == 0)
        return;
    os << "Conflicts:";
    os << "  " << this->conflicts_count.rs << " shift/reduce, "
       << this->conflicts_count.rr << " reduce/reduce" << std::endl
       << "\n";

    for (size_t i = 0; i < ParsingTblRows; i++) {
        const auto& state = this->states_new_array.at(i);
        if (state.rs_count > 0) {
            os << "  state " << i << ": " << state.rs_count
               << " shift/reduce conflict"
               << ((state.rs_count == 1) ? "" : "s");
            if (state.rr_count > 0) {
                os << ", " << state.rr_count << " reduce/reduce conflict"
                   << ((state.rr_count == 1) ? "" : "s");
            }
            os << std::endl;
        } else if (state.rr_count > 0) {
            os << "  state " << i << ": " << state.rr_count
               << " reduce/reduce conflict"
               << ((state.rr_count == 1) ? "" : "s") << std::endl;
        }
    }
    os << std::endl;
}

void
NewStates::write_grammar_conflict_list2(std::ostream& os) const noexcept
{
    uint32_t final_rs_count = 0;
    uint32_t final_rr_count = 0;
    const auto& a = this->states_new_array;

    if (this->conflicts_count.rs == 0 && this->conflicts_count.rr == 0)
        return;
    os << "Conflicts:";
    os << "  " << this->conflicts_count.rs << " shift/reduce, "
       << this->conflicts_count.rr << " reduce/reduce]" << std::endl
       << "\n";

    for (size_t i = 0; i < ParsingTblRows; i++) {
        if (!is_reachable_state(i))
            continue;

        StateHandle state = *get_actual_state(i);
        const auto& a_i = a.at(i);
        if (a_i.rs_count > 0) {
            os << "  state " << state << ": " << a_i.rs_count
               << " shift/reduce conflict" << ((a_i.rs_count == 1) ? "" : "s");
            final_rs_count += a_i.rs_count;
            if (a_i.rr_count > 0) {
                os << ", " << a_i.rr_count << " reduce/reduce conflict"
                   << ((a_i.rr_count == 1) ? "" : "s");
                final_rr_count += a_i.rr_count;
            }
            os << std::endl;
        } else if (a_i.rr_count > 0) {
            os << "  state " << state << ": " << a_i.rr_count
               << " reduce/reduce conflict" << ((a_i.rr_count == 1) ? "" : "s")
               << std::endl;
            final_rr_count += a_i.rr_count;
        }
    }

    int diff = static_cast<int>(this->conflicts_count.rs) -
               static_cast<int>(final_rs_count);
    if (diff > 0)
        os << "  [" << diff << " shift/reduce conflict"
           << ((diff > 1) ? "s" : "") << " in removed states]" << std::endl;
    diff = static_cast<int>(this->conflicts_count.rr) -
           static_cast<int>(final_rr_count);
    if (diff > 0)
        os << "  [" << diff << " reduce/reduce conflict"
           << ((diff > 1) ? "s" : "") << " in removed states]" << std::endl;

    os << std::endl;
}

static void
write_state(std::ostream& os,
            const Grammar& grammar,
            const StateArray& states_array,
            State& s)
{
    auto& options = Options::get();
    write_state_conflict_list(os, s.state_no, states_array);

    os << "--state " << s.state_no << "-- config count:" << s.config.size()
       << ", core_config count:" << s.core_config_count << std::endl;
    for (const auto& config : s.config) {
        write_configuration(os, grammar, *config);
    }

    write_successor_list(os, s);

    if (options.use_lalr && options.show_originators) {
        os << *s.parents_list;
    }
    os << std::endl;
}

static void
write_state_collection(std::ostream& os,
                       const Grammar& grammar,
                       const NewStates& new_states)
{
    os << "==State Collection: (count=" << new_states.states_new->state_count
       << ")" << std::endl;

    std::shared_ptr<State> s = new_states.states_new->states_head;
    while (s != nullptr) {
        write_state(os, grammar, new_states.states_new_array, *s);
        s = s->next;
        os << std::endl;
    }

    if (new_states.states_new->state_count == 0)
        os << "(empty)" << std::endl;
    os << std::endl;
}

/// Add the given symbol to the context in increasing order.
/// There is no need to sort context later.
auto
add_symbol2_context(std::shared_ptr<SymbolTableNode> snode, Context& c) -> bool
{
    for (auto it = c.context.begin(); it != c.context.end(); it++) {
        int cmp_val = (*it).snode->symbol->compare(*snode->symbol);
        if (cmp_val == 0)
            return false;  // already in context.
        if (cmp_val > 0) { // s_prev < snode < s
            c.context.emplace(it, snode);
            return true; // inserted in the middle of context list.
        }
        // else: cmp_val < 0, go to next context node.
    }

    // insert at the end of list.
    c.context.emplace_back(snode);
    return true;
}

auto
insert_symbol_list_unique_inc(SymbolList& list,
                              std::shared_ptr<SymbolTableNode> snode) -> bool
{
    for (auto it = list.begin(); it != list.end(); it++) {
        if (it->snode == snode) {
            return true; // existing node.
        }
        if (*it->snode->symbol > *snode->symbol) {
            list.emplace(it, snode);
            return false;
        }
    }

    // insert as the last node.
    list.emplace_back(snode);
    return false;
}

/// Insert symbol to list tail if not exist, un-ordered.
static auto
insert_unique_symbol_list(SymbolList& list,
                          std::shared_ptr<SymbolTableNode> snode) -> bool
{
    for (const auto& it : list) {
        if (it.snode == snode) {
            return true; // existing node.
        }
    }

    // insert as the last node.
    list.emplace_back(snode);
    return false;
}

static void
write_symbol_node_array(std::ostream& os,
                        const SymbolList& str,
                        SymbolList::const_iterator str_it)
{
    bool first_a = true;
    for (; str_it != str.end(); str_it++) {
        if (first_a) {
            first_a = false;
        } else {
            os << ", ";
        }
        os << *str_it->snode->symbol;
    }
    os << std::endl;
}

static void
show_t_heads(std::ostream& os,
             const SymbolList& alpha,
             const SymbolList& theads)
{
    os << "string '";

    for (const auto& a : alpha)
        os << *a.snode->symbol << " ";

    os << "' has theads: ";

    for (const auto& a : theads)
        os << *a.snode->symbol << " ";

    os << std::endl;
}

static void
insert_alpha_to_heads(const SymbolList& s,
                      SymbolList::const_iterator s_it,
                      SymbolList& heads,
                      SymbolList& theads)
{
    for (; s_it != s.end(); ++s_it) {
        const auto& a = *s_it;
        if (a.snode->is_vanish_symbol()) {
            // note: a vanishable symbol must be a non-terminal.
            insert_unique_symbol_list(heads, a.snode);
        } else { // not vanish symbol. break after insertion.
            if (a.snode->is_terminal()) { // here actually can insert to tail.
                insert_symbol_list_unique_inc(theads, a.snode);
            } else { // non_terminal.
                insert_unique_symbol_list(heads, a.snode);
            }
            return;
        }
    }

    // all symbols are vanishable, since we didn't return in the cycle.
    std::shared_ptr<SymbolTableNode> snode = hash_tbl_find("");
    insert_symbol_list_unique_inc(theads, snode);
}

/// Insert the RHS symbols to heads up to an unvanishable symbol.
static void
insert_rhs_to_heads(const SymbolList& s, SymbolList& heads, SymbolList& theads)
{
    for (const auto& a : s) {
        if (a.snode->is_vanish_symbol()) {
            // note: a vanishable symbol must be a non-terminal.
            insert_unique_symbol_list(heads, a.snode);
        } else { // not vanlish symbol. break after inserting last one.
            if (a.snode->is_terminal()) {
                insert_symbol_list_unique_inc(theads, a.snode);
            } else { // non_terminal.
                insert_unique_symbol_list(heads, a.snode);
            }
            return;
        }
    }
}

/// Algorithm:
///
/// insert each symbol S in alpha to heads until S is NOT vanishable.
/// if S is a terminal, then insert S to theads; else insert to heads.
/// if all symbols are N.T., insert empty string to theads.
///
/// for each symbol A in heads {
///   for each grammar rule r where A is the LHS {
///     for each symbol B in r's RHS until NOT vanishable {
///       if B is NT, insert B to heads's tail;
///       else B is T, insert (like insertion sort) to theads.
///     }
///   }
/// }
///
/// @added to replace the old one on: 3/9/2008
///
/// @note This will skip alpha's first element.
auto
get_theads(const Grammar& grammar,
           const SymbolList& alpha,
           SymbolList::const_iterator alpha_it) -> SymbolList
{
    SymbolList heads{};
    SymbolList theads{};

    insert_alpha_to_heads(alpha, alpha_it, heads, theads);

    for (const auto& n : heads) {
        for (const RuleIDNode* rules = n.snode->ruleIDList; rules != nullptr;
             rules = rules->next) {
            Production* p = grammar.rules.at(rules->rule_id);
            insert_rhs_to_heads(p->nRHS, heads, theads);
        }
    }

    if (Options::get().debug_gen_parsing_machine) {
        grammar.fp_v << "==getTHeads: theads for: ";
        write_symbol_node_array(grammar.fp_v, alpha, alpha_it);
        write_symbol_node_array(grammar.fp_v, theads, theads.begin());
    }

    return theads;
}

/// Obtain the context for a configuration.
static void
get_context(const Grammar& grammar, const Configuration& cfg, Context& context)
{
    const Production& production = *grammar.rules.at(cfg.ruleID);

    if (cfg.marker == production.nRHS.size() - 1) {
        // is last symbol, just copy the context.
        for (const auto& a : cfg.context->context) {
            add_symbol2_context(a.snode, context);
        }
    } else { // need to find thead(alpha)
        if (Options::get().show_theads)
            write_configuration(grammar.fp_v, grammar, cfg);

        // alpha is the string after scanned symbol.

        const SymbolList& alpha =
          cfg.nMarker; // we know cfg.nMarker is not empty.
        SymbolList theads = get_theads(grammar, alpha, alpha.begin());

        if (Options::get().show_theads) {
            show_t_heads(grammar.fp_v, alpha, theads);
        }

        // if theads is empty, just copy the context.
        if (theads.empty()) {
            for (const auto& a : cfg.context->context) {
                add_symbol2_context(a.snode, context);
            }
        } else { // theads_count > 0
            for (const auto& a : theads) {
                if (a.snode->symbol->empty()) { // empty string.
                    // Entire alpha vanishable. Copy context.
                    for (const auto& a : cfg.context->context) {
                        add_symbol2_context(a.snode, context);
                    }
                } else {
                    add_symbol2_context(a.snode, context);
                }
            }
        }
    }
}

void
Context::clear()
{
    this->context.clear();
}

void
free_context(Context* c)
{
    if (c == nullptr)
        return;
    c->clear();
    delete c;
}

void
free_config(Configuration* c)
{
    if (c == nullptr)
        return;
    c->context->clear();
    delete c;
}

/// Determine if contexts c1 and c2 are the same.
static auto
is_same_context(const Context& c1, const Context& c2) -> bool
{
    if (c1.context.size() != c2.context.size())
        return false;

    auto a = c1.context.cbegin();
    auto b = c2.context.cbegin();
    while (a != c1.context.cend()) {
        if (a->snode != b->snode)
            return false;
        a++;
        b++;
    }
    return true;
}

/// Determine if configurations con and c are the same.
static auto
is_same_config(const Configuration& config1, const Configuration& config2)
  -> bool
{
    if (config1.marker != config2.marker)
        return false;
    if (config1.ruleID != config2.ruleID)
        return false;
    if (!is_same_context(*config1.context, *config2.context))
        return false;
    return true;
}

/// Note that a successor config's marker = 0.
static auto
is_existing_successor_config(const State& s,
                             const size_t rule_id,
                             const Context& con) -> bool
{
    for (const auto& config : s.config) {
        if (config->marker == 0 && rule_id == config->ruleID &&
            is_same_context(con, *config->context))
            return true; // existing config
    }
    return false;
}

/// Add `con` to `s.config`.
static void
add_successor_config_to_state(const Grammar& grammar,
                              const std::shared_ptr<StateNode> s,
                              const size_t rule_id,
                              const Context& con)
{
    // marker = 0, is_core_config = 0.
    Configuration* c = create_config(grammar, rule_id, 0, 0);
    c->owner = s;
    copy_context(*c->context, con);
    s->config.push_back(c);
}

auto
is_final_configuration(const Grammar& grammar, const Configuration* c) -> bool
{
    if (c->marker == grammar.rules.at(c->ruleID)->nRHS.size())
        return true;
    return false;
}

auto
is_empty_production(const Grammar& grammar, const Configuration* c) -> bool
{
    if (grammar.rules.at(c->ruleID)->nRHS.size() == 0)
        return true;
    return false;
}

/*
 * Used by addCoreConfig2State() only.
 * comparison is made alphabetically on:
 * 1) production,
 * 2) marker,
 * 3) context.
 * Actually, for the same state,
 * core configurations are different only
 * by production. But for the same state,
 * different core configuration comparison
 * will be sure to end in production comparison
 * -- either > or <. So does not matter to
 * add in the extra code to compare marker and
 * context. This function thus can be used
 * for general config comparison, although in
 * this program it's used only here.
 */
static auto
config_cmp(const Grammar& grammar,
           const Configuration* c1,
           const Configuration* c2) -> int
{
    const Production* c1_production = grammar.rules.at(c1->ruleID);
    const Production* c2_production = grammar.rules.at(c2->ruleID);

    // std::cout << "compare LHS" << std::endl;
    int cmp_val = c1_production->nLHS->snode->symbol->compare(
      *c2_production->nLHS->snode->symbol);
    if (cmp_val != 0)
        return cmp_val;

    // std::cout << "compare RHS" << std::endl;
    auto a = c1_production->nRHS.cbegin();
    auto b = c2_production->nRHS.cbegin();
    while ((a != c1_production->nRHS.cend()) &&
           (b != c2_production->nRHS.cend())) {
        cmp_val = a->snode->symbol->compare(*b->snode->symbol);
        if (cmp_val != 0)
            return cmp_val;
        a++;
        b++;
    }

    cmp_val = static_cast<int>(c1_production->nRHS.size()) -
              static_cast<int>(c2_production->nRHS.size());
    if (cmp_val > 0) {
        return 1;
    } // c1 RHS is longer.
    if (cmp_val < 0) {
        return -1;
    } // c2 RHS is longer.

    // If productions are the same, go on to compare context.
    // std::cout << "compare marker" << std::endl;
    cmp_val = static_cast<int>(c1->marker) - static_cast<int>(c2->marker);
    if (cmp_val > 0) {
        return 1;
    }
    if (cmp_val < 0) {
        return -1;
    }

    // If production and marker are the same, go on to compare context.
    // std::cout << "compare context" << std::endl;
    a = c1->context->context.cbegin();
    b = c2->context->context.cbegin();
    while ((a != c1->context->context.cend()) &&
           (b != c2->context->context.cend())) {
        cmp_val = a->snode->symbol->compare(*b->snode->symbol);
        if (cmp_val != 0)
            return cmp_val;
        a++;
        b++;
    }

    if (c1->context->context.size() > c2->context->context.size()) {
        return 1;
    }
    if (c1->context->context.size() < c2->context->context.size()) {
        return -1;
    }

    return 0; // the same
}

/*
 * Add core configurations in increasing order
 * The order is by production, and marker.
 * Assumption: a core config won't be inserted twice.
 */
void
add_core_config2_state(const Grammar& grammar,
                       std::shared_ptr<State> s,
                       Configuration* new_config)
{
    size_t i = 0;
    for (; i < s->config.size(); i++) {
        const int cmp_val = config_cmp(grammar, s->config.at(i), new_config);
        if (cmp_val == 0) {
            // a core config shouldn't be added twice.
            throw std::runtime_error("add_core_config2_state: a core "
                                     "config shouldn't be added "
                                     "twice."); // should never happen.
        }
        if (cmp_val > 0)
            break; // found insertion point.
    }
    s->config.insert(s->config.begin() + static_cast<ptrdiff_t>(i), new_config);
    s->core_config_count++;
}

/////////////////////////////////////////////////////
// Use config_queue when get closure for a state.
/////////////////////////////////////////////////////

/// Note that a successor config's marker = 0.
/// Returns the index of the compatible config in the state.
auto
is_compatible_successor_config(const std::shared_ptr<const State>& s,
                               const size_t rule_id) -> std::optional<size_t>
{
    for (size_t i = 0; i < s->config.size(); i++) {
        const Configuration* c = s->config.at(i);
        if (c->marker == 0 && rule_id == c->ruleID)
            return i; // existing compatible config
    }
    return std::nullopt;
}

/// Assumption: public variable config_queue contains
/// the configurations to be processed.
static void
get_config_successors(const Grammar& grammar,
                      Queue& config_queue,
                      std::shared_ptr<State> s)
{
    while (config_queue.size() > 0) {
        Configuration& config = *s->config.at(*config_queue.pop());

        if (config.marker >= 0 &&
            config.marker < grammar.rules.at(config.ruleID)->nRHS.size()) {
            const std::shared_ptr<SymbolTableNode> scanned_symbol =
              get_scanned_symbol(config);

            if (scanned_symbol->is_non_terminal()) {
                Context tmp_context{};
                get_context(grammar, config, tmp_context);

                for (const RuleIDNode* r = scanned_symbol->ruleIDList;
                     r != nullptr;
                     r = r->next) {
                    // Grammar.rules[r->ruleID] starts with this scanned
                    // symbol.

                    // If not an existing config, add to state s.
                    const std::optional<size_t> index_opt =
                      is_compatible_successor_config(s, r->rule_id);

                    if (!index_opt.has_value()) { // new config.
                        add_successor_config_to_state(
                          grammar, s, r->rule_id, tmp_context);
                        // `s->config.size() - 1 >= 0` here (see the first line
                        // of the loop)
                        config_queue.push(s->config.size() - 1);
                    } else {
                        size_t index = index_opt.value();
                        if (combine_context(s->config.at(index)->context,
                                            &tmp_context)) {
                            // compatible config
                            // if this config has no successor, don't insert
                            // to config_queue. This saves time. marker = 0
                            // here, no need to check marker >= 0.
                            if (is_final_configuration(grammar,
                                                       s->config.at(index)))
                                continue;
                            if (get_scanned_symbol(*s->config.at(index))
                                  ->is_terminal())
                                continue;
                            if (config_queue.exist(index) == 1)
                                continue;

                            // else, insert to config_queue.
                            config_queue.push(index);
                        }
                    }
                }
            }
        }
    }
}

//////////////////////////////////////////////////////
// Not use config_queue, combine compatible
// configurations after they are all generated.
// This is much slower, and is used for testing only.
//////////////////////////////////////////////////////

static void
get_successor_for_config(const Grammar& grammar,
                         std::shared_ptr<State> s,
                         const Configuration& config)
{

    if (config.marker >= 0 &&
        config.marker < grammar.rules.at(config.ruleID)->nRHS.size()) {
        const SymbolTableNode& scanned_symbol = *get_scanned_symbol(config);

        if (scanned_symbol.is_non_terminal()) {
            Context tmp_context{};
            get_context(grammar, config, tmp_context);

            for (const RuleIDNode* r = scanned_symbol.ruleIDList; r != nullptr;
                 r = r->next) {
                // grammar.rules.at(r->ruleID) starts with this scanned
                // symbol.

                // If not an existing config, add to state s.
                if (is_existing_successor_config(*s, r->rule_id, tmp_context) ==
                    false) {
                    add_successor_config_to_state(
                      grammar, s, r->rule_id, tmp_context);
                }
            }
        } // else, is a terminal, stop.
    }     // end if config-marker >= 0 ...
}

/*
 * To combine compatible configurations of the given state.
 *
 * Compatible configurations are those that have the same
 * production and marker, but different in contexts.
 */
[[maybe_unused]] static void
combine_compatible_config(std::shared_ptr<State> s,
                          const bool debug_comb_comp_config,
                          std::ofstream& fp_v)
{
    if (s == nullptr) {
        return;
    }
    if (s->config.size() <= 1)
        return;

    if (debug_comb_comp_config) {
        fp_v << "combineCompatibleCfg (state " << s->state_no
             << "). before: " << s->config.size() << ", ";
    }

    for (size_t i = 1; i < s->config.size(); i++) {
        Configuration* c = s->config.at(i);
        if (c == nullptr)
            continue;
        for (size_t j = 0; j < i; j++) {
            if (is_compatible_config(c, s->config.at(j))) {
                combine_context(s->config.at(j)->context, c->context);
                free_config(c); // combine config i to j, then remove i.
                s->config.at(i) = nullptr;
                break;
            }
        }
    }

    // shrink state configs to remove those nullptr ones
    // after the last step.
    size_t i = 0;
    size_t j = 0;
    while (j < s->config.size()) {
        while (s->config.at(j) == nullptr) {
            j++;
        }
        if (j >= s->config.size())
            break;
        s->config.at(i) = s->config.at(j);
        j++;
        i++;
    }
    s->config.resize(i);

    if (debug_comb_comp_config) {
        fp_v << "after: " << s->config.size() << std::endl;
    }
}

struct Counts
{
    /// no. of calls to get_closure.
    size_t get_closure_calls = 0;
    /// number of config in all states before combine.
    size_t nb_config_before_combine = 0;
    /// max config count in all states before combine.
    size_t max_config_before_combine = 0;
    /// number of config in all states after combine.
    size_t nb_config_after_combine = 0;
    /// max config count in all states after combine.
    size_t max_config_after_combine = 0;
};

static Counts counts{};

/*
 * Get the configuration closure of a state s given its
 * core configuratins. Called by generate_parsing_machine().
 *
 * Algorithm:
 * For each config in the state:
 *   If it is NOT a final config, then
 *     Get scanned symbol s.
 *     If s is a Non-Terminal, then
 *       Get context c (*).
 *       For each rule r in the grammar whose RHS starts with s,
 *         Combine r and c to get the next possible config.
 *         If this new config already exists in the state, stop.
 *         Else, add this new config to the end of current state.
 *
 * NOTE *: This is found by get thead(alpha), where alpha is the
 *         string following scanned symbol s. If thead(alpha) is
 *         empty, then use the context of current config.
 */
void
YAlgorithm::get_state_closure(std::shared_ptr<State> state)
{
    if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
        // config_queue->clear();
        for (size_t i = 0; i < state->config.size(); i++) {
            this->config_queue->push(i);
        }
        get_config_successors(this->grammar, *this->config_queue, state);
    } else {
        for (const Configuration* config : state->config) {
            get_successor_for_config(grammar, state, *config);
        }

        counts.get_closure_calls++;
        counts.nb_config_before_combine += state->config.size();
        if (counts.max_config_before_combine < state->config.size())
            counts.max_config_before_combine = state->config.size();

        if (this->options.debug_comb_comp_config)
            combine_compatible_config(
              state, this->options.debug_comb_comp_config, this->fp_v);

        counts.nb_config_after_combine += state->config.size();
        if (counts.max_config_after_combine < state->config.size())
            counts.max_config_after_combine = state->config.size();
    }
}

///////////////////////////////////////////
// StateCollection functions. START.
///////////////////////////////////////////

auto
create_state_collection() -> StateCollection*
{
    auto* c = new StateCollection;
    if (c == nullptr)
        throw std::runtime_error(
          "create_state_collection error: out of memory\n");

    c->states_head = nullptr;
    c->states_tail = nullptr;
    c->state_count = 0;

    return c;
}

auto
StateCollection::add_state2(std::shared_ptr<State> new_state)
  -> std::shared_ptr<State>
{
    if (new_state == nullptr)
        return nullptr;

    new_state->next = nullptr;

    if (this->state_count == 0) {
        this->states_head = new_state;
        this->states_tail = new_state;
    } else {
        this->states_tail->next = new_state;
        this->states_tail = new_state;
    }
    this->state_count++;

    return this->states_tail;
}

///////////////////////////////////////////
// StateCollection functions. END.
///////////////////////////////////////////

auto
get_scanned_symbol(const Configuration& c) -> std::shared_ptr<SymbolTableNode>
{
    if (c.nMarker.empty())
        return nullptr;
    return c.nMarker.front().snode;
}

/// Used by function transition.
/// Returns the successor state that is the result of
/// transition following the given symbol.
auto
find_state_for_scanned_symbol(
  const StateCollection* c,
  const std::shared_ptr<const SymbolTableNode> symbol) -> std::shared_ptr<State>
{
    if (c == nullptr)
        return nullptr;

    std::shared_ptr<State> s = c->states_head;
    while (s != nullptr) {
        if (symbol == s->trans_symbol->snode)
            return s;
        s = s->next;
    }

    return nullptr;
}

auto
create_context() -> Context*
{
    auto* c = new Context;
    return c;
}

auto
create_config(const Grammar& grammar,
              // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
              const size_t rule_id,
              const size_t marker,
              const uint is_core_config) -> Configuration*
{
    auto* c = new Configuration;
    if (c == nullptr) {
        throw std::runtime_error("create_config error: out of memory");
    }

    c->context = create_context();

    c->ruleID = rule_id;
    c->marker = marker;
    c->isCoreConfig = is_core_config;

    if (rule_id >= 0)
        c->nMarker =
          grammar.rules.at(rule_id)->nRHS; // copy, not exactly what we want !

    c->owner = nullptr;
    if (Options::get().use_lalr) {
        c->ORIGINATOR_CHANGED = false;
        c->COMPLETE = 0;
        c->IN_LANE = 0;
        c->LANE_END = 0;
        c->LANE_CON = 0;
        c->CONTEXT_CHANGED = 0;
        c->originators = OriginatorList();
        c->transitors = OriginatorList();
    }

    return c;
}

void
copy_context(Context& dest, const Context& src)
{
    dest.context = src.context;
}

/// return the copy of a config.
/// used by function transition when creating new state.
void
copy_config(Configuration& c_dest, const Configuration& c_src)
{
    c_dest.marker = c_src.marker;
    c_dest.isCoreConfig = c_src.isCoreConfig;
    c_dest.ruleID = c_src.ruleID;
    c_dest.nMarker = c_src.nMarker;
    copy_context(*c_dest.context, *c_src.context);
}

/////////////////////////////////////////////////////////
// Functions for combining compatible states. START.
/////////////////////////////////////////////////////////

/// Two configurations are common core configurations if
/// they have the same production and marker, but
/// DIFFERENT contexts.
static auto
is_common_config(const Configuration& con, const Configuration& c) -> bool
{
    if (con.marker != c.marker)
        return false;
    if (con.ruleID != c.ruleID)
        return false;
    // If all same, then are same config, not common config!
    if (is_same_context(*con.context, *c.context))
        return false;
    return true;
}

/// Pre-assumption: s1, s2 have at least one core config.
/// Returns true if at least one config pair have common config.
auto
has_common_core(const State* s1, const State* s2) -> bool
{
    if (s1 == nullptr || s2 == nullptr)
        return false;

    bool result = false;
    if (s1->core_config_count != s2->core_config_count)
        return false;
    if (s1->core_config_count == 0)
        return false;
    for (size_t i = 0; i < s1->core_config_count; i++) {
        if (is_same_config(*s1->config.at(i), *s2->config.at(i))) {
            // do nothing.
        } else if (is_common_config(*s1->config.at(i), *s2->config.at(i))) {
            result = true;
        } else { // not same, and not common core.
            return false;
        }
    }
    return result;
}

/// Pre-assumption: contexts c1 and c2 are sorted in increasing order.
/// See function addSymbol2Context().
static auto
has_empty_intersection(const Context& c1, const Context& c2) -> bool
{
    auto a = c1.context.cbegin();
    auto b = c2.context.cbegin();
    // intrinsically this is O(m + n).
    while (a != c1.context.cend() && b != c2.context.cend()) {
        if (a->snode == b->snode)
            return false; // common element found
        if (*a->snode->symbol < *b->snode->symbol) {
            a++;
        } else {
            b++;
        }
    }

    return true;
}

/// condition (a).
/// if (a) is satisfied, return true, otherwise false.
// TODO: this function seems to assume that `s1.core_config_count ==
// s2.core_config_count` !
static auto
is_compatible_state_a(const State& s1, const State& s2) -> bool
{
    size_t count = s1.core_config_count;
    for (size_t i = 0; i < count; i++) {
        for (size_t j = 0; j < count; j++) {
            if (i != j) {
                const Context& c1 = *s1.config.at(i)->context;
                const Context& c2 = *s2.config.at(j)->context;
                if (!has_empty_intersection(c1, c2)) {
                    return false;
                }
            }
        }
    }
    return true;
}

/// condition (b) or (c).
/// if (b) or (c) is satisfied, return true, otherwise false.
static auto
is_compatible_state_bc(const State& s) -> bool
{
    size_t count = s.core_config_count;
    for (size_t i = 0; i < count - 1; i++) {
        for (size_t j = i + 1; j < count; j++) {
            const Context& c1 = *s.config.at(i)->context;
            const Context& c2 = *s.config.at(j)->context;
            if (has_empty_intersection(c1, c2)) {
                return false;
            } // end if
        }     // end for
    }         // end for
    return true;
}

/// Pre-assumption:
///   s1 and s2 have at least one core configuration.
auto
is_compatible_states(const State* s1, const State* s2) -> bool
{
    if (!has_common_core(s1, s2))
        return false;

    if (s1->core_config_count == 1)
        return true;

    // now check context to see if s1 and s2 are compatible.
    if (is_compatible_state_a(*s1, *s2))
        return true;
    if (is_compatible_state_bc(*s1))
        return true;
    if (is_compatible_state_bc(*s2))
        return true;

    return false;
}

/*
 * Used by combineCompatibleStates() and propagateContextChange().
 */
void
YAlgorithm::update_state_parsing_tbl_entry(const State& s)
{
    for (const auto& config : s.config) {
        const std::shared_ptr<const SymbolTableNode> scanned_symbol =
          get_scanned_symbol(*config);

        // for final config and empty reduction.
        if (is_final_configuration(grammar, config) ||
            scanned_symbol->symbol->empty()) {
            this->insert_reduction_to_parsing_table(config, s.state_no);
        }
    }
}

/*
 * Used by propagateContextChange() and push_state_context_change().
 *
 * Find a similar core config for c in State t.
 *
 * similar core config:
 * - production and marker are the same.
 * - Context does not matter.
 */
auto
find_similar_core_config(const std::shared_ptr<const State>& t,
                         const Configuration* c,
                         size_t* config_index) -> Configuration*
{
    for (size_t i = 0; i < t->core_config_count; i++) {
        Configuration* tmp = t->config.at(i);

        // don't compare context, it'll be compared in
        // combine_context().
        if (tmp->marker == c->marker && tmp->ruleID == c->ruleID) {
            (*config_index) = i;
            return tmp;
        }
    }
    return nullptr;
}

/*
 * For each successor state s, update context of core
 * configurations, then get closure, add change to
 * parsing table. Do this recursively.
 *
 * In more detail:
 * For each successor state t of s:
 *   look through each config c in the config list of s:
 *     if scanned symbol of c == trans_symbol of t {
 *       // c transits to a core config in t
 *       find core config d in t which is the transition
 *       result of c,
 *       update the context of d from c.
 *       if the context of d is changed {
 *         get closure of d again,
 *         update parsing table, and
 *         propagate context change to d's successors.
 *     }
 */
void
YAlgorithm::propagate_context_change(const State& s)
{
    if (s.successor_list.empty())
        return;

    size_t config_index = 0;
    for (std::shared_ptr<State> t : s.successor_list) {
        const std::shared_ptr<const SymbolTableNode> trans_symbol =
          t->trans_symbol->snode;
        bool is_changed = false;

        for (const auto& c : s.config) {
            if (is_final_configuration(this->grammar, c))
                continue;
            // if not a successor on symbol, next.
            if (trans_symbol != get_scanned_symbol(*c))
                continue;

            c->marker += 1;
            const Configuration* d =
              find_similar_core_config(t, c, &config_index);
            c->marker -= 1;
            if (d == nullptr)
                continue;

            if (combine_context(d->context, c->context)) {
                // std::cout << "context of state " <<  t->state_no<< " updated"
                // << std::endl;
                is_changed = true;

                if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
                    // config_queue->clear();
                    config_queue->push(config_index);
                    get_config_successors(this->grammar, *config_queue, t);
                } else {
                    get_successor_for_config(this->grammar, t, *d);
                }
            }
        } // end of for

        if (is_changed) {
            if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
            } else {
                combine_compatible_config(
                  t, this->options.debug_comb_comp_config, this->fp_v);
            }
            this->update_state_parsing_tbl_entry(*t);
            this->propagate_context_change(*t);
        }
    }
}

/*
 * Assumption: s_dest and s_src have common core,
 *   and are weakly compatible.
 * NOTE: s_dest is from states_new.
 */
auto
YAlgorithm::combine_compatible_states(std::shared_ptr<State> s_dest,
                                      const State& s_src) -> bool
{
    bool is_changed = false;
    for (size_t i = 0; i < s_dest->core_config_count; i++) {
        if (combine_context(s_dest->config.at(i)->context,
                            s_src.config.at(i)->context)) {
            is_changed = true;

            if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
                this->config_queue->push(i);
                get_config_successors(
                  this->grammar, *this->config_queue, s_dest);
            } else {

                get_successor_for_config(
                  this->grammar, s_dest, *s_dest->config.at(i));
            }
        }
    }

    // Now propagate the context change to successor states.
    if (is_changed) {
        if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
        } else {
            combine_compatible_config(
              s_dest, this->options.debug_comb_comp_config, this->fp_v);
        }
        this->update_state_parsing_tbl_entry(*s_dest);
        this->propagate_context_change(*s_dest);
    }
    return is_changed;
}

/////////////////////////////////////////////////////////
// Functions for combining compatible states. END.
/////////////////////////////////////////////////////////

/// Determine if two states are the same.
/// This is done by checking if the two states have the
/// same core configurations.
/// Since the core confiurations are both in increasing order,
/// just compare them by pairs.
auto
is_same_state(const State& s1, const State& s2) -> bool
{
    if (s1.core_config_count != s2.core_config_count) {
        return false;
    }

    for (size_t i = 0; i < s1.core_config_count; i++) {
        if (!is_same_config(*s1.config.at(i), *s2.config.at(i)))
            return false;
    }

    return true;
}

StateNode::StateNode()
  : core_config_count(0)
  , state_no(-1)
  , trans_symbol(std::make_shared<SymbolNode>(hash_tbl_find("")))
  , parents_list(StateList::create())
  , next(nullptr)
  , ON_LANE(0)
  , COMPLETE(0)
  , PASS_THRU(0)
  , REGENERATED(0)
{
    this->config.reserve(STATE_INIT_SIZE);
    this->successor_list.reserve(STATE_SUCCESSOR_INIT_MAX_COUNT);
}

void
YAlgorithm::insert_reduction_to_parsing_table(const Configuration* c,
                                              const StateHandle state_no)
{
    if (this->grammar.rules.at(c->ruleID)->nLHS->snode ==
        this->grammar.goal_symbol->snode) { // accept, action = "a";
        for (const auto& a : c->context->context) {
            this->insert_action(a.snode, state_no, ParsingAction::new_accept());
        }
    } else { // reduct, action = "r";
        for (const auto& a : c->context->context) {
            this->insert_action(
              a.snode, state_no, ParsingAction::new_reduce(c->ruleID));
        }
    }
}

/*
 * Input:
 * s - src state, n - new state.
 * Add n to the successor list of s.
 */
void
add_successor(std::shared_ptr<State>& s, std::shared_ptr<State> n)
{
    s->successor_list.push_back(n);
    // std::cout << ":: state " <<
    //     s->state_no<< ", succesor is state " <<  n->state_no<< " on symbol
    //  " <<  n->trans_symbol << std::endl;

    if (Options::get().use_lalr) {
        // add s to the parents_list of n. To get originators in
        // lane-tracing.
        n->parents_list->add(s);
    }
}

/// Insert a new state to the parsing machine.
///
/// Used in 3 places: y.c, lr0.c, lane_tracing.c.
/// This can be changed to macro later.
void
NewStates::insert_state_to_pm(std::shared_ptr<State> s) noexcept
{
    s->state_no = this->states_new->state_count;

    this->states_new->add_state2(s);
    this->states_new_array.add_state(s);
}

/*
 * For each of the new temp states,
 *   If it is not one of the existing states in
 *     states_new, then add it to states_new.
 *   Also always add the transition to parsing table.
 *
 * Note:
 * The "is_compatible" variable is for this situation:
 *
 * state s_x is a successor of state s_0.
 * in function isExistingState, s_0 is already in
 * states_new, but s_x is not.
 * now s_0 is in coll too, and when calling function
 * isExistingState, new s_0 is combined to old s_0,
 * which causes propagation of new context to successors
 * of s_0. However this does not include s_x since it's
 * inserted into states_new only after that.
 * The following code ensures that the propagation is
 * performed again over all the states.
 *
 */
auto
YAlgorithm::add_transition_states2_new(StateCollection* coll,
                                       std::shared_ptr<State> src_state) -> bool
{
    bool src_state_changed = false;
    std::shared_ptr<State> s = coll->states_head;

    while (s != nullptr) {
        std::shared_ptr<State> next = s->next;
        // is_compatible = 0;

        // Two alternatives. search_state_hash_tbl is slightly
        // faster. if ((os = isExistingState(states_new, s, &
        // is_compatible)) == nullptr) {
        auto [os, is_compatible] = this->state_hash_table.search(s, *this);
        if (os == nullptr) {
            this->new_states.insert_state_to_pm(s);

            // Add this new state as successor to src_state.
            add_successor(src_state, s);

            // insert shift.
            this->insert_action(s->trans_symbol->snode,
                                src_state->state_no,
                                ParsingAction::new_shift(s->state_no));

        } else { // same or compatible with an existing state.
                 // insert shift.
            this->insert_action(os->trans_symbol->snode,
                                src_state->state_no,
                                ParsingAction::new_shift(os->state_no));

            add_successor(src_state, os);

            // This should only happen for general practical
            // method.
            if (os == src_state && is_compatible == 1) {
                src_state_changed = true;
                // std::cout << "src state is changed" << std::endl;
            }
        }

        s = next;
    } // end of while.

    return src_state_changed;
}

/*
 * Perform transition opertaion on a state to get successors.
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
void
YAlgorithm::state_transition(std::shared_ptr<State> state)
{
    StateCollection* coll = create_state_collection();

    for (Configuration* c : state->config) {
        if (is_final_configuration(this->grammar, c)) {
            this->insert_reduction_to_parsing_table(c, state->state_no);
        } else { // do transit operation.
            auto scanned_symbol = get_scanned_symbol(*c);
            if (scanned_symbol->symbol->empty()) { // insert empty reduction.
                this->insert_reduction_to_parsing_table(c, state->state_no);
                continue;
            }
            std::shared_ptr<State> new_state =
              find_state_for_scanned_symbol(coll, scanned_symbol);
            if (new_state == nullptr) {
                new_state = std::make_shared<State>();
                // record which symbol this state is a successor
                // by.
                new_state->trans_symbol =
                  std::make_shared<SymbolNode>(scanned_symbol);
                coll->add_state2(new_state);
            }
            // create a new core config for new_state.
            Configuration& new_config =
              *create_config(this->grammar, c->ruleID, 0, 1);

            new_config.owner = new_state;
            copy_config(new_config, *c);
            new_config.isCoreConfig = 1;
            new_config.marker++;
            if (!new_config.nMarker.empty()) {
                new_config.nMarker.pop_front();
            }

            add_core_config2_state(this->grammar, new_state, &new_config);
        }
    }

    if (coll->state_count > 0) {
        bool src_state_changed = this->add_transition_states2_new(coll, state);
        if (src_state_changed) {
            this->propagate_context_change(*state);
        }
    }
}

auto
is_compatible_config(const Configuration* c1, const Configuration* c2) -> bool
{
    if (c1 == nullptr || c2 == nullptr)
        return false;
    if (c1->marker != c2->marker)
        return false;
    if (c1->ruleID != c2->ruleID)
        return false;
    return true;
}

/*
 * Combine c_src into c_dest by copying symbols that
 * occur in c_src but not in c_dest to c_dest.
 *
 * Returns true if any change is made.
 */
auto
combine_context(Context* c_dest, const Context* c_src) -> bool
{
    bool is_changed = false;
    if (c_dest == nullptr || c_src == nullptr)
        return false;

    for (const SymbolNode& a : c_src->context) {
        if (add_symbol2_context(a.snode, *c_dest)) {
            is_changed = true;
        }
    }

    return is_changed;
}

/*
 * The main function to generate parsing machine.
 */
void
YAlgorithm::generate_parsing_machine()
{
    std::shared_ptr<State> new_state = this->new_states.states_new->states_head;

    if (this->options.debug_gen_parsing_machine) {
        grammar.fp_v << std::endl
                     << "\n--generate parsing machine--" << std::endl;
    }

    while (new_state != nullptr) {
        if (this->options.debug_gen_parsing_machine) {
            grammar.fp_v << this->new_states.states_new->state_count
                         << " states, current state is " << new_state->state_no
                         << std::endl;
        }

        this->get_state_closure(new_state); // get closure of this state.

        // get successor states and add them to states_new.
        this->state_transition(new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    ParsingTblRows = this->new_states.states_new->state_count;
    this->n_state_opt1 = this->new_states.states_new->state_count;
}

/////////////////////////////////////////////////////////////////
// Parsing table functions.
/////////////////////////////////////////////////////////////////

/*
 * Three places the parsing table is directly manipulated:
 * 1) updateDestState, 2) getAction, 3) insertAction.
 */

/// Use a one-dimension array to store the matrix and
/// calculate the row and column number myself:
/// row i, col j is ParsingTable.at(col_no * i + j);
///
/// Here we have:
/// 0 <= i < total states count
/// 0 <= j < col_no
void
init_parsing_table()
{
    PARSING_TABLE_SIZE = PARSING_TABLE_INIT_SIZE;
    size_t total_cells = PARSING_TABLE_SIZE * ParsingTblColHdr.size();
    ParsingTable.clear();
    ParsingTable.reserve(4 * total_cells);
    for (size_t i = 0; i < 4 * total_cells; i++) {
        ParsingTable.emplace_back(std::nullopt);
    }
}

auto
get_action(symbol_type symbol_type, size_t col, StateHandle row)
  -> std::pair<std::optional<Action>, StateHandle>
{
    const std::optional<ParsingAction> x_opt =
      ParsingTable.at(row * ParsingTblColHdr.size() + col);

    if (!x_opt.has_value()) {
        return { std::nullopt, 0 };
    }
    auto x = *x_opt;
    if (x.is_shift()) { // 's' or 'g'
        std::optional<Action> action = std::nullopt;
        if (symbol_type == symbol_type::TERMINAL) {
            action = Action::Shift;
        } else if (symbol_type == symbol_type::NONTERMINAL) {
            action = Action::Goto;
        } else {
            using std::to_string;
            throw std::runtime_error(
              std::string("get_action error: unknown symbol type ") +
              to_string(static_cast<int>(symbol_type)) +
              ", col:" + to_string(col));
        }
        return { action, x.shift_value() };
    }
    if (x.is_accept()) { // 'a'
        return { Action::Accept, 0 };
    }                             // x < 0, 'r'
    return { Action::Reduce, 1 }; // TODO: ???
}

/*
 * Inserts an action into the parsing table.
 * Input variables include:
 *   state_src - given state.
 *   symbol - transition symbol.
 *   action - action on this symbol at this state.
 *   state_dest - destination state of the action.
 *
 * Actions can be:
 *   r - reduce, state_dest < 0
 *   s - shift,  state_dest > 0
 *   a - accept, state_dest == CONST_ACC
 *   g - goto.   state_dest > 0
 *
 * This is one of the 3 places updating the ParsingTable
 * array directly. The 2nd place is function updateAction().
 * The 3rd place is in the macro
 *   clearStateTerminalTransitions(state_no)
 * in lane_tracing.c.
 */
void
LR0::insert_action(std::shared_ptr<SymbolTableNode> lookahead,
                   StateHandle row,
                   ParsingAction action)
{
    struct TerminalProperty *tp_s = nullptr, *tp_r = nullptr;

    const size_t cell = row * ParsingTblColHdr.size() + get_col(*lookahead);

    auto& previous_action_opt = ParsingTable.at(cell);
    if (!previous_action_opt.has_value()) {
        previous_action_opt = action;
        return;
    }
    auto previous_action = *previous_action_opt;

    if (action.is_shift() && action == previous_action)
        return;

    // ParsingTable.at(cell) != 0 && ParsingTable.at(cell) !=
    // state_dest. The following code process shift/reduce and
    // reduce/reduce conflicts.

    if (previous_action.is_accept() || action.is_accept()) {
        if (options.use_lr0) {
            return;
        }
        throw std::runtime_error(
          std::string("warning: conflict between ACC and an action: ") +
          previous_action.to_string() + ", " + action.to_string());
    }

    // std::cout << "conflict (state " <<
    //         row << ", lookahead " << *lookahead->symbol << "): " <<
    //         previous_action << " v.s. " << state_dest << std::endl;

    // reduce/reduce conflict, use the rule appears first.
    // i.e., the ruleID is smaller, or when negated, is bigger.
    if (previous_action.is_reduce() && action.is_reduce()) {
        // std::cout << "r/r conflict: [" <<
        //         row << ", " << *lookahead->symbol << "] - " <<
        //         previous_action << " v.s. " << state_dest << std::endl;
        std::shared_ptr<Conflict> c = this->new_states.add_to_conflict_array(
          row, lookahead, previous_action, action);

        if (action.reduce_value() < previous_action.reduce_value())
            previous_action = action;

        if (c != nullptr) {
            c->decision = previous_action;
        }

        // include r/r conflict for inadequate states.
        if (this->options.use_lalr) {
            add_state_no_array(*states_inadequate, row);
        }

        return;
    }

    // shift/shift conflict.
    if (previous_action.is_shift() && action.is_shift()) {
        if (this->options.show_ss_conflicts) {
            std::cerr << "warning: shift/shift conflict: " << action << " v.s. "
                      << previous_action << " @ (" << row << ", "
                      << *lookahead->symbol << ")" << std::endl;
        }
        // exit(1);
        // previous_action = action; // change causes
        // infinite loop.
        this->new_states.conflicts_count.ss++;
        return;
    }

    StateHandle reduce = 0, shift = 0; // for shift/reduce conflict.
    if (previous_action.is_reduce()) {
        reduce = previous_action.reduce_value();
        shift = action.shift_value();
    } else {
        reduce = action.reduce_value();
        shift = previous_action.shift_value();
    }

    tp_s = lookahead->TP;
    if (this->grammar.rules.at(reduce)->lastTerminal != nullptr)
        tp_r = this->grammar.rules.at(reduce)->lastTerminal->TP;

    if (tp_s == nullptr || tp_r == nullptr || tp_s->precedence == 0 ||
        tp_r->precedence == 0) {
        // std::cout << "s/r conflict: [" <<
        //         row << ", " << *lookahead->symbol << "] - " <<
        //         previous_action << " v.s. " << action << std::endl;
        std::shared_ptr<Conflict> c = new_states.add_to_conflict_array(
          row, lookahead, previous_action, action);

        previous_action =
          ParsingAction::new_shift(shift); // use shift over reduce by default.
        // std::cout << "default using " << previous_action << std::endl;

        if (c != nullptr) {
            c->decision = previous_action;
        }

        // include s/r conflicts not handled by
        // precedence/associativity.
        if (this->options.use_lalr) {
            add_state_no_array(*states_inadequate, row);
        }

        return;
    }

    if (tp_r->precedence > tp_s->precedence ||
        (tp_r->precedence == tp_s->precedence &&
         tp_r->assoc == associativity::LEFT)) {
        previous_action = ParsingAction::new_reduce(reduce);
        // std::cout << "resolved by using " <<
        //  previous_action << std::endl;
        return;
    }

    // include s/r conflicts not handled by
    // precedence/associativity.
    if (this->options.use_lalr) {
        add_state_no_array(*states_inadequate, row);
    }

    previous_action = ParsingAction::new_shift(shift);
    // std::cout << "resolved by using " <<  previous_action << std::endl;
}

auto
is_goal_symbol(const Grammar& grammar,
               const std::shared_ptr<const SymbolTableNode> snode) -> bool
{
    if (snode == grammar.goal_symbol->snode)
        return true;
    return false;
}

void
print_parsing_table_note(std::ostream& os)
{
    os << "Note: " << std::endl;
    os << "1. si means shift and stack state i" << std::endl;
    os << "2. ri means reduce by production numbered i" << std::endl;
    os << "3. a0 means accept" << std::endl;
    os << "4. gi means go to state i" << std::endl;
    os << "5. 0 means error" << std::endl;
}

/// Print the parsing table after LR(1) parsing machine
/// is generated, by before removing unit productions.
///
/// Note: The value of variable ParsingTblRows is
/// assigned at the end of function generate_parsing_table().
///
/// Parsing table: Ref. Aho&Ullman p219.
void
print_parsing_table(std::ostream& os, const Grammar& grammar)
{
    os << std::endl << "--Pars" << std::endl << "g Table--\n";
    os << "State\t";
    write_parsing_table_col_header(os, grammar);

    for (size_t row = 0; row < ParsingTblRows; row++) {
        os << row << "\t";
        for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
            const std::shared_ptr<const SymbolTableNode> n =
              ParsingTblColHdr.at(col);
            if (!is_goal_symbol(grammar, n)) {
                auto [action, state] = get_action(n->type, col, row);
                os << action << state << "\t";
            }
        }
        os << std::endl;
    }

    print_parsing_table_note(os);
}

/// Create a state based on the goal production (first
/// production of grammmar) and insert it to states_new.
///
/// Called by function init() only.
///
/// Assumption: grammar.rules[0] is the goal production.
void
YAlgorithm::init_start_state()
{
    std::shared_ptr<State> state0 = std::make_shared<State>();
    state0->state_no = 0;

    // ruleID = 0, marker = 0, isCoreConfig = 1.
    state0->config.push_back(create_config(this->grammar, 0, 0, 1));
    state0->core_config_count = 1;

    state0->config.at(0)->owner = state0;
    hash_tbl_insert(STR_END);
    state0->config.at(0)->context->context.emplace_back(hash_tbl_find(STR_END));

    this->new_states.states_new->add_state2(state0);
    this->new_states.states_new_array.add_state(state0);

    // insert to state hash table as the side effect of search.
    auto ignore = this->state_hash_table.search(state0, *this);
}

/// Get a list of those states in the ParsingTable whose only
/// actions are a single reduction. Such states are called
/// final states.
///
/// Use final state default reduction in the hyaccpar parse
/// engine. This significantly decreases the size of the
/// generated parser and overcomes the problem of always need to
/// get the new lookahead token to proceed parsing. Array
/// final_state_list is used in gen_compiler.c, and function
/// writeParsingTblRow() of y.c.
static void
get_final_state_list(const Grammar& grammar)
{
    std::shared_ptr<SymbolTableNode> n = nullptr;

    final_state_list.clear();
    final_state_list.reserve(ParsingTblRows);
    for (size_t i = 0; i < ParsingTblRows; i++) {
        final_state_list.push_back(0);
    }

    if constexpr (USE_REM_FINAL_STATE) {
        if (Options::get().use_remove_unit_production) {
            for (size_t i = 0; i < ParsingTblRows; i++) {
                if (!is_reachable_state(i))
                    continue;

                size_t row_start = i * ParsingTblColHdr.size();
                std::optional<StateHandle> action = std::nullopt;
                size_t j = 0;
                for (; j < ParsingTblColHdr.size(); j++) {
                    n = ParsingTblColHdr.at(j);

                    if (is_goal_symbol(grammar, n) || is_parent_symbol(n))
                        continue;

                    auto new_action_opt = ParsingTable.at(row_start + j);
                    if (!new_action_opt.has_value()) {
                        continue;
                    }
                    auto new_action = *new_action_opt;
                    if (new_action.is_shift() || new_action.is_accept())
                        break;
                    if (!action.has_value())
                        action = new_action.reduce_value();
                    if (action != new_action.reduce_value())
                        break;
                }
                if (j == ParsingTblColHdr.size())
                    final_state_list.at(i) = *action;
            }

        } else {
            for (size_t i = 0; i < ParsingTblRows; i++) {
                size_t row_start = i * ParsingTblColHdr.size();
                std::optional<StateHandle> action = std::nullopt;
                size_t j = 0;
                for (; j < ParsingTblColHdr.size(); j++) {
                    auto new_action_opt = ParsingTable.at(row_start + j);
                    if (!new_action_opt.has_value()) {
                        continue;
                    }
                    auto new_action = *new_action_opt;
                    if (new_action.is_shift() || new_action.is_accept())
                        break;
                    if (!action.has_value())
                        action = new_action.reduce_value();
                    if (action != new_action.reduce_value())
                        break;
                }
                if (j == ParsingTblColHdr.size())
                    final_state_list.at(i) = *action;
            }
        }
    }
}

static void
get_avg_config_count(std::ostream& os, const StateCollection& states_new)
{
    constexpr size_t LINE_LENGTH = 20;
    size_t i = 0, sum = 0;
    // USe raw pointer to avoid overhead of reference counting.
    const State* a = states_new.states_head.get();
    size_t max = a->config.size();
    size_t min = a->config.size();
    os << std::endl << "--No. of configurations for each state--" << std::endl;
    for (; a != nullptr; a = a->next.get()) {
        if ((++i) % LINE_LENGTH == 1)
            os << std::endl << i << ": ";
        os << a->config.size() << " ";
        sum += a->config.size();
        if (min > a->config.size())
            min = a->config.size();
        if (max < a->config.size())
            max = a->config.size();
    }
    os << std::endl;
    os << "Average configurations per state: " << std::setprecision(2)
       << static_cast<double>(sum) / static_cast<double>(states_new.state_count)
       << " (min: " << min << ", max: " << max << ')' << std::endl;
}

void
YAlgorithm::show_state_config_info(std::ostream& os) const noexcept
{
    if constexpr (USE_CONFIG_QUEUE_FOR_GET_CLOSURE) {
        this->config_queue->info();
    } else {
        os << counts.get_closure_calls << " states in total." << std::endl;
        os << "before combine: total cfg: " << counts.nb_config_before_combine
           << ", max cfg: " << counts.max_config_before_combine
           << ", cfg/state: " << std::setprecision(2)
           << static_cast<double>(counts.nb_config_before_combine) /
                static_cast<double>(counts.get_closure_calls)
           << std::endl; // before combineCompatibleConfig.
        os << "after combine: total cfg: " << counts.nb_config_after_combine
           << ", max cfg: " << counts.max_config_after_combine
           << ", cfg/state: " << std::setprecision(2)
           << static_cast<double>(counts.nb_config_after_combine) /
                static_cast<double>(counts.get_closure_calls)
           << std::endl;
    }

    get_avg_config_count(os, *this->new_states.states_new);
    this->state_hash_table.dump(os);
}

static void
show_conflict_count(const ConflictsCount& conflicts_count)
{
    // no conflicts.
    if (conflicts_count.rs == 0 && conflicts_count.rr == 0 &&
        conflicts_count.ss == 0)
        return;

    // no r/r conflicts, and s/r conflicts number is
    // expected.
    if (conflicts_count.rs == conflicts_count.expected_sr_conflict &&
        conflicts_count.rr == 0 && conflicts_count.ss == 0)
        return;

    // otherwise, report conflicts.
    std::cout << hyacc_filename << ": conflicts: ";
    if (conflicts_count.rs > 0) {
        std::cout << conflicts_count.rs << " shift/reduce";
        if (conflicts_count.rr > 0)
            std::cout << ", " << conflicts_count.rr << " reduce/reduce";
    } else if (conflicts_count.rr > 0) {
        std::cout << conflicts_count.rr << " reduce/reduce";
    }
    std::cout << std::endl;
    if (conflicts_count.ss > 0)
        std::cout << "warning: " << conflicts_count.ss
                  << " shift/shift conflicts" << std::endl;
}

/// Show statistics of the grammar and it's parsing machine.
void
YAlgorithm::show_stat(std::ostream& os) const noexcept
{
    if (!this->options.use_verbose)
        return;

    write_state_transition_list(os, this->grammar, this->new_states);
    if (this->options.show_state_config_count)
        this->show_state_config_info(os);
    if (this->options.show_actual_state_array)
        write_actual_state_array(this->grammar.fp_v);

    os << std::endl;
    // *fp_v << "--statistics--" << std::endl ;
    // *fp_v << "[Note: A first rule '$accept ->
    //  start_symbol' is added]" << std::endl << "\n" ;*fp_v << "Symbols
    //  count: " <<  n_symbol<< std::endl ;
    os << this->grammar.terminal_list.size() << " terminals, "
       << this->grammar.non_terminal_list.size() << " nonterminals"
       << std::endl;
    os << n_rule << " grammar rules" << std::endl;
    if (options.use_remove_unit_production) {
        os << n_rule_opt << " grammar rules after remove unit ";
    }
    if (this->options.use_combine_compatible_states) {
        if (this->options.use_lr0) {
            os << this->n_state_opt1 << " states without optimization"
               << std::endl;
        } else {
            os << this->n_state_opt1
               << " states after combine compatible states" << std::endl;
        }
        if (options.use_remove_unit_production) {
            os << this->n_state_opt12 << " states after remove unit productions"
               << std::endl;
            if (options.use_remove_repeated_states)
                os << this->n_state_opt123 << " states after remove repeated ";
        }
    } else {
        os << this->n_state_opt1 << " states without optimization" << std::endl;
    }

    // conflicts summary.
    os << this->new_states.conflicts_count.rs << " shift/reduce conflict"
       << ((this->new_states.conflicts_count.rs > 1) ? "s" : "") << ", "
       << this->new_states.conflicts_count.rr << " reduce/reduce conflict"
       << ((this->new_states.conflicts_count.rr > 1) ? "s" : "") << std::endl;
    if (this->options.use_remove_unit_production &&
        this->new_states.conflicts_count.ss > 0) {
        os << this->new_states.conflicts_count.ss << " shift/shift conflict"
           << ((this->new_states.conflicts_count.ss > 1) ? "s" : "")
           << std::endl
           << "\n";
    }
}

/////////////////////////////////////////////
// Functions to write state transition list
/////////////////////////////////////////////

/*
 * Used when --lr0 or --lalr is used.
 * Under such situation use_lr0 or use_lalr is true.
 */
static void
write_parsing_tbl_row_lalr(std::ostream& os, StateHandle state)
{
    size_t row_start = state * ParsingTblColHdr.size();
    std::optional<StateHandle> reduction = std::nullopt;
    int only_one_reduction = true;

    auto& options = Options::get();

    // write shift/acc actions.
    // note if a state has acc action, then that's the only
    // action. so don't have to put acc in a separate loop.
    for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
        std::optional<ParsingAction> v_opt = ParsingTable.at(row_start + col);
        if (!v_opt.has_value()) {
            continue;
        }
        auto v = *v_opt;
        const std::shared_ptr<const SymbolTableNode> s =
          ParsingTblColHdr.at(col);
        if (v.is_shift()) {
            if (options.use_remove_unit_production)
                v =
                  ParsingAction::new_shift(*get_actual_state(v.shift_value()));

            if (ParsingTblColHdr.at(col)->type == symbol_type::TERMINAL) {
                os << "  " << *s->symbol << " [" << s->value << "] " << v
                   << std::endl;
            }
        } else if (v.is_accept()) {
            os << "  " << *s->symbol << " [" << s->value << "] Accept"
               << std::endl;
        } else if (v.is_reduce()) {
            if (!reduction.has_value()) {
                reduction = 1;
            } // first reduction.
            else if (reduction != v.reduce_value()) {
                only_one_reduction = false;
            }
            // else, is the same as first reduction. do
            // nothing.
        }
    }

    // write reduce actions.
    if (only_one_reduction) {
        if (reduction.has_value()) {
            os << "  . reduce (" << *reduction << ")" << std::endl;
        } else {
            os << "  . error " << std::endl;
        } // no reduction.
    } else {
        for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
            std::optional<ParsingAction> v_opt =
              ParsingTable.at(row_start + col);
            if (!v_opt.has_value()) {
                continue;
            }
            auto v = *v_opt;
            const std::shared_ptr<const SymbolTableNode> s =
              ParsingTblColHdr.at(col);
            if (v.is_reduce()) {
                os << "  " << *s->symbol << " [" << s->value << "] " << v
                   << std::endl;
            }
        }
    }

    // write goto action.
    os << std::endl;
    for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
        std::optional<ParsingAction> v_opt = ParsingTable.at(row_start + col);
        if (!v_opt.has_value()) {
            continue;
        }
        auto v = *v_opt;
        const std::shared_ptr<const SymbolTableNode> s =
          ParsingTblColHdr.at(col);
        if (v.is_shift()) {
            if (options.use_remove_unit_production)
                v =
                  ParsingAction::new_shift(*get_actual_state(v.shift_value()));

            if (ParsingTblColHdr.at(col)->type == symbol_type::NONTERMINAL) {
                os << "  " << *s->symbol << " [" << s->value << "] goto "
                   << v.shift_value() << std::endl;
            }
        }
    }
}

static void
write_parsing_tbl_row(std::ostream& os, StateHandle state)
{
    const auto& options = Options::get();
    size_t row_start = state * ParsingTblColHdr.size();

    os << std::endl;

    if (final_state_list.at(state) < 0) {
        os << "  . reduce (" << (-1) * final_state_list.at(state) << ")"
           << std::endl;
        return;
    }

    if (options.use_lr0 || options.use_lalr) {
        write_parsing_tbl_row_lalr(os, state);
        return;
    }

    for (size_t col = 0; col < ParsingTblColHdr.size(); col++) {
        std::optional<ParsingAction> v_opt = ParsingTable.at(row_start + col);
        if (!v_opt.has_value()) {
            continue;
        }
        auto v = *v_opt;
        const std::shared_ptr<const SymbolTableNode> s =
          ParsingTblColHdr.at(col);
        if (v.is_shift()) {
            if (options.use_remove_unit_production)
                v =
                  ParsingAction::new_shift(*get_actual_state(v.shift_value()));

            if (ParsingTblColHdr.at(col)->type == symbol_type::NONTERMINAL) {
                os << "  " << *s->symbol << " [" << s->value << "] goto "
                   << v.shift_value() << std::endl;
            } else {
                os << "  " << *s->symbol << " [" << s->value << "] shift "
                   << v.shift_value() << std::endl;
            }
        } else if (v.is_accept()) {
            os << "  " << *s->symbol << " [" << s->value << "] Accept"
               << std::endl;
        } else if (v.is_reduce()) {
            os << "  " << *s->symbol << " [" << s->value << "] " << v
               << std::endl;
        }
    }
}

static void
write_state_info(std::ostream& os,
                 const Grammar& grammar,
                 const StateArray& states_array,
                 const State& s)
{
    const auto& options = Options::get();
    write_state_conflict_list(os, s.state_no, states_array);

    if (s.PASS_THRU == 1u) {
        os << "[PASS_THRU]" << std::endl;
    }

    os << "state " << s.state_no << std::endl << "\n";
    os << "  [config: " << s.config.size()
       << ", core config: " << s.core_config_count << "]" << std::endl
       << "\n";

    for (const auto& i : s.config) {
        os << "  (" << i->ruleID << ") ";
        write_configuration(os, grammar, *i);
    }

    // writeSuccessorList(s);
    if (options.use_lalr && options.show_originators) {
        os << *s.parents_list;
    }

    write_parsing_tbl_row(os, s.state_no);

    // writeCoreConfiguration(s);
    os << std::endl << "\n";
}

static void
write_state_collection_info(std::ostream& os,
                            const Grammar& grammar,
                            const NewStates& new_states)
{
    os << std::endl << "\n";
    const State* s = new_states.states_new->states_head.get();
    while (s != nullptr) {
        write_state_info(os, grammar, new_states.states_new_array, *s);
        s = s->next.get();
    }

    if (new_states.states_new->state_count == 0)
        os << "(empty)" << std::endl;
    os << std::endl;
}

static void
write_state_info_from_parsing_tbl(std::ostream& os)
{
    for (size_t row = 0; row < ParsingTblRows; row++) {
        if (is_reachable_state(row)) {
            os << std::endl
               << "\nstate " << *get_actual_state(row) << std::endl;
            write_parsing_tbl_row(os, row);
        }
    }
    os << std::endl << "\n";
}

/*
 * A list like the list in AT&T yacc and Bison's y.output
 * file.
 */
void
write_state_transition_list(std::ostream& os,
                            const Grammar& grammar,
                            const NewStates& new_states)
{
    if (!Options::get().show_state_transition_list)
        return;

    if (Options::get().use_remove_unit_production) {
        // write from the parsing table.
        write_state_info_from_parsing_tbl(os);
        new_states.write_grammar_conflict_list2(os);
    } else {
        // write from the state objects.
        write_state_collection_info(os, grammar, new_states);
        new_states.write_grammar_conflict_list(os);
    }
}

/*
 * LR1 function.
 */
auto
lr1(const FileNames& files, const Options& options, NewStates& new_states)
  -> int
{
    std::optional<LRkPTArray> lrk_pt_array = std::nullopt;
    std::optional<Queue> config_queue = std::nullopt;
    hash_tbl_init();
    std::ofstream fp_v;

    if (options.use_verbose) {
        fp_v.open(files.y_output); // for y.output
        if (!fp_v.is_open()) {
            throw std::runtime_error(std::string("cannot open file ") +
                                     files.y_output);
        }
        fp_v << "/* y.output. Generated by HYACC. */" << std::endl;
        fp_v << "/* Input file: " << hyacc_filename << " */" << std::endl;
    }

    GetYaccGrammarOutput yacc_grammar_output =
      GetYaccGrammarOutput::get_yacc_grammar(
        hyacc_filename, fp_v, new_states.conflicts_count.expected_sr_conflict);

    YAlgorithm y_algorithm(
      yacc_grammar_output.grammar, options, fp_v, new_states, config_queue);

    if (options.debug_hash_tbl) {
        hash_tbl_dump(fp_v);
    }

    y_algorithm.init();
    if (options.show_grammar) {
        y_algorithm.grammar.write(
          fp_v, true, options.use_remove_unit_production);
    }

    y_algorithm.generate_parsing_machine();

    if (options.show_parsing_tbl)
        print_parsing_table(fp_v, y_algorithm.grammar);

    if (options.use_graphviz && !options.use_remove_unit_production) {
        gen_graphviz_input(y_algorithm.grammar, files.y_gviz, options);
    } /*O0,O1*/

    if (options.use_remove_unit_production) {
        y_algorithm.remove_unit_production();
        if (options.show_parsing_tbl)
            fp_v << std::endl << "AFTER REMOVING UNIT PRODUCTION:\n";
        if (options.show_total_parsing_tbl_after_rm_up) {
            fp_v << std::endl
                 << "--" << std::endl
                 << "tire pars" << std::endl
                 << "g table ";
            fp_v << "after removing unit productions--" << std::endl;
            print_parsing_table(fp_v, y_algorithm.grammar);
        }

        if (options.show_parsing_tbl)
            print_final_parsing_table(y_algorithm.grammar);
        if (options.use_remove_repeated_states) {
            y_algorithm.further_optimization();
            if (options.show_parsing_tbl) {
                fp_v << std::endl << "AFTER REMOVING REPEATED STATES:\n";
                print_final_parsing_table(y_algorithm.grammar);
            }
        }
        get_actual_state_no(); /* update actual_state_no[].
                                */
        if (options.show_parsing_tbl)
            print_condensed_final_parsing_table(y_algorithm.grammar);
        if (options.show_grammar) {
            fp_v << std::endl
                 << "--Grammar after remov" << std::endl
                 << "g " << std::endl
                 << "it ";
            y_algorithm.grammar.write(
              fp_v, false, options.use_remove_unit_production);
        }
        if (options.use_graphviz) {
            gen_graphviz_input2(y_algorithm.grammar, files.y_gviz, options);
        } /*O2,O3*/
    }
    get_final_state_list(y_algorithm.grammar);

    if (options.use_generate_compiler)
        generate_compiler(
          yacc_grammar_output, lrk_pt_array, hyacc_filename, files);

    y_algorithm.show_stat(fp_v);
    show_conflict_count(new_states.conflicts_count);

    if (options.use_verbose)
        fp_v.close();
    return 0;
}

static auto
lr0(const FileNames& files, const Options& options, NewStates& new_states)
  -> int
{
    std::optional<Queue> config_queue = std::nullopt;
    hash_tbl_init();

    std::ofstream fp_v;
    if (options.use_verbose) {
        fp_v.open(files.y_output);
        if (!fp_v.is_open()) { // for y.output
            throw std::runtime_error(std::string("cannot open file ") +
                                     files.y_output);
        }
        fp_v << "/* y.output. Generated by HYACC. */" << std::endl;
        fp_v << "/* Input file: " << hyacc_filename << " */" << std::endl;
    }

    GetYaccGrammarOutput yacc_grammar_output =
      GetYaccGrammarOutput::get_yacc_grammar(
        hyacc_filename, fp_v, new_states.conflicts_count.expected_sr_conflict);

    if (options.debug_hash_tbl) {
        hash_tbl_dump(fp_v);
    }

    LaneTracing lane_tracing_algorithm(
      yacc_grammar_output.grammar, options, fp_v, new_states, config_queue);
    lane_tracing_algorithm.init();
    if (options.show_grammar) {
        lane_tracing_algorithm.grammar.write(
          fp_v, true, options.use_remove_unit_production);
    }

    lane_tracing_algorithm.generate_lr0_parsing_machine(*config_queue);

    std::optional<LRkPTArray> lrk_pt_array = std::nullopt;
    if (options.use_lalr) {
        lrk_pt_array = lane_tracing_algorithm.lane_tracing();
    }

    if (options.show_parsing_tbl)
        print_parsing_table(fp_v, lane_tracing_algorithm.grammar);

    if (options.use_graphviz && !options.use_remove_unit_production) {
        gen_graphviz_input(
          lane_tracing_algorithm.grammar, files.y_gviz, options);
    } /*O0,O1*/

    if (options.use_remove_unit_production) {
        lane_tracing_algorithm.remove_unit_production();
        if (options.show_parsing_tbl)
            fp_v << std::endl << "AFTER REMOVING UNIT PRODUCTION:\n";
        if (options.show_total_parsing_tbl_after_rm_up) {
            fp_v << std::endl
                 << "--" << std::endl
                 << "tire pars" << std::endl
                 << "g table ";
            fp_v << "after removing unit productions--" << std::endl;
            print_parsing_table(fp_v, lane_tracing_algorithm.grammar);
        }

        if (options.show_parsing_tbl)
            print_final_parsing_table(lane_tracing_algorithm.grammar);
        if (options.use_remove_repeated_states) {
            lane_tracing_algorithm.further_optimization();
            if (options.show_parsing_tbl) {
                fp_v << std::endl << "AFTER REMOVING REPEATED STATES:\n";
                print_final_parsing_table(lane_tracing_algorithm.grammar);
            }
        }
        get_actual_state_no(); // update actual_state_no[].

        if (options.show_parsing_tbl)
            print_condensed_final_parsing_table(lane_tracing_algorithm.grammar);
        if (options.show_grammar) {
            fp_v << std::endl
                 << "--Grammar after remov" << std::endl
                 << "g " << std::endl
                 << "it ";
            lane_tracing_algorithm.grammar.write(
              fp_v, false, options.use_remove_unit_production);
        }
        if (options.use_graphviz) {
            gen_graphviz_input2(
              lane_tracing_algorithm.grammar, files.y_gviz, options);
        } /*O2,O3*/
    }
    get_final_state_list(lane_tracing_algorithm.grammar);

    if (options.use_generate_compiler)
        generate_compiler(
          yacc_grammar_output, lrk_pt_array, hyacc_filename, files);

    lane_tracing_algorithm.show_stat(fp_v);
    show_conflict_count(new_states.conflicts_count);

    if (options.use_lr_k) {
        std::cout << "Max K in LR(k): " << MAX_K << std::endl;
    }

    if (options.use_verbose)
        fp_v.close();
    return 0;
}

/// main function.
auto
main(int argc, const char* argv[]) -> int
{
    try {
        auto& options = Options::get();
        FileNames files{};
        std::span<const char* const> args_ptrs = std::span(argv, argc);
        std::vector<std::string_view> args;
        for (const char* const arg : args_ptrs) {
            args.emplace_back(arg);
        }
        options.debug_expand_array = false;

        // test_x();

        size_t infile_index = get_options(args, options, &files);
        hyacc_filename = args.at(infile_index);

        NewStates new_states{};
        if (options.use_lr0) {
            lr0(files, options, new_states);
        } else {
            lr1(files, options, new_states);
        }

        if (false) { // for reading memory.
            std::cout << "press ENTER to end... ";
            std::cin.get(); // stop here for reading memory.
        }
    } catch (std::runtime_error error) {
        std::cerr << error.what() << std::endl;
        return 1;
    }
    return 0;
}
