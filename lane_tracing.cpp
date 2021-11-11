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
 * Lane tracing functions.
 *
 * @Author: Xin Chen
 * @Created on: 2/28/2008
 * @Last modified: 3/24/2009
 */

#include "lane_tracing.hpp"
#include "stack_config.hpp"
#include "y.hpp"
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

constexpr bool DEBUG_PHASE_1 = false;
constexpr bool DEBUG_RESOLVE_CONFLICT = false;
constexpr bool DEBUG_PHASE_2 = false;
constexpr bool DEBUG_GET_LANEHEAD = false;
constexpr bool DEBUG_PHASE_2_GET_TBL = false;
constexpr bool DEBUG_PHASE_2_REGENERATE2 = false;
constexpr bool DEBUG_PHASE_2_REGENERATE = false;
constexpr bool DEBUG_GET_ORIGINATOR = false;

constexpr unsigned int FLAG_ON = 1u;
constexpr unsigned int FLAG_OFF = 0u;

static void
get_originators(const Grammar& grammar, Configuration* c0, Configuration& c);
static auto
inherit_parent_context(const Grammar& grammar,
                       std::shared_ptr<State>,
                       const State* parent) -> bool;
static void
clear_state_context(State* s);

void
LlistContextSet::dump() const noexcept
{
    if (this->empty())
        return;

    for (const auto& c : *this) {
        // first write the config.
        std::cout << c.config->owner->state_no << '.' << c.config->ruleID
                  << ':';
        // then write the context symbol list.
        std::cout << "{ ";
        for (const auto& n : c.ctxt) {
            std::cout << n.snode->symbol << " ";
        }
        std::cout << "} ";
    }
}

/// Add (merge) contxt_set to c->ctxt.
void
LlistContextSet::add_context(const LlistContextSet::iterator c,
                             const SymbolList& context_set) const
{
    if (c == this->end()) {
        return;
    }
    for (const auto& n : context_set) {
        // if (strlen(n->snode->symbol) == 0) continue; // ignore empty string.
        insert_symbol_list_unique_inc(c->ctxt, n.snode);
    }
}

/// Return a clone of `this`.
auto
LlistContextSet::clone() const -> LlistContextSet
{
    if (this->empty())
        return LlistContextSet();

    auto d = LlistContextSet();
    for (const auto& item : *this) {
        LlistContextSetItem new_item(item.config);
        new_item.ctxt = clone_symbol_list(item.ctxt);
        d.push_back(new_item);
    }

    return d;
}

// LlistInt functions. START.

void
LlistState::add_inc(const StateHandle n)
{
    auto it = this->begin();
    for (; it != this->end(); it++) {
        if (*it == n)
            return; // exists already.
        if (*it > n) {
            this->insert(it, n); // insert before t.
            return;
        }
        // else, go on to the next state in list.
    }
    this->push_back(n);
}

void
LlistState::dump() const
{
    for (const auto& n : *this) {
        std::cout << n << ' ';
    }
}

// LlistInt functions. END.

// LlistInt2 functions. START.

void
LlistState2::add_inc(const StateHandle n1, const StateHandle n2)
{
    auto it = this->begin();
    for (; it != this->end(); it++) {
        if (it->first == n1)
            return; // exists already.
        if (it->first > n1) {
            this->insert(it, { n1, n2 });
            return;
        }
        // else, go on to the next state in list.
    }
    this->push_back({ n1, n2 });
}

void
LlistState2::add_head(const StateHandle n1, const StateHandle n2)
{
    this->push_front({ n1, n2 });
}

auto
LlistState2::find_n1(const StateHandle n1) const noexcept
  -> LlistState2::const_iterator
{
    auto it = this->begin();
    for (; it != this->end(); it++) {
        if (it->first == n1)
            return it;
    }
    return it;
}

auto
LlistState2::find_n2(const StateHandle n2) const noexcept
  -> LlistState2::const_iterator
{
    auto it = this->begin();
    for (; it != this->end(); it++) {
        if (it->second == n2)
            return it;
    }
    return it;
}

void
LlistState2::dump() const noexcept
{
    if (this->empty())
        return;

    for (const auto& [n1, n2] : *this) {
        std::cout << '(' << n1 << ", " << n2 << ") ";
    }
}

// LlistInt2 functions, END.

LtTblEntry::LtTblEntry(StateHandle from_state,
                       const std::shared_ptr<const State>& to,
                       const StateArray& states_new_array)
  : from_state(from_state)
{
    if (from_state != states_new_array[from_state].state->state_no) {
        throw std::runtime_error(
          "ERROR (std::make_shared<LtTblEntry>): state_no not equal");
    }
    this->to_states = (to == nullptr) ? LlistState() : LlistState(to->state_no);
}

/// A LtTblEntry can have more than one to_state.
/// add to_state in increasing order of the state_no.
void
LtTblEntry::add_to_state(const std::shared_ptr<const State>& to)
{
    if (to == nullptr)
        return;
    this->to_states.add_inc(to->state_no);
}

/// Add an entry (from_state, to_state) to the LT_tbl,
/// don't add the (config, context) information here.
void
LaneTracing::lt_tbl_entry_add(const StateHandle from_state,
                              const std::shared_ptr<const State>& to)
{
    if (this->LT_tbl == nullptr) {
        this->LT_tbl = std::make_shared<LtTblEntry>(
          from_state, to, this->new_states.states_new_array);
        return;
    }
    std::shared_ptr<LtTblEntry> e = this->LT_tbl;
    std::shared_ptr<LtTblEntry> e_prev = nullptr;
    // search if the from state already exists.
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from_state) {
            e->add_to_state(to); // add to state if not on list.
            return;
        }
        if (e->from_state > from_state) { // insert before e.
            if (e_prev == nullptr) {      // insert as the head.
                this->LT_tbl = std::make_shared<LtTblEntry>(
                  from_state, to, this->new_states.states_new_array);
                this->LT_tbl->next = e;
            } else { // insert between e_prev and e
                e_prev->next = std::make_shared<LtTblEntry>(
                  from_state, to, this->new_states.states_new_array);
                e_prev->next->next = e;
            }
            return;
        }
    }
    // now is at the end of the table LT_tbl, add to list tail.
    e_prev->next = std::make_shared<LtTblEntry>(
      from_state, to, this->new_states.states_new_array);
    return;
}

/// Find from state in the LT_tbl.
/// If not found, insert it.
auto
LaneTracing::lt_tbl_entry_find_insert(const StateHandle from_state)
  -> std::shared_ptr<LtTblEntry>
{
    if (this->LT_tbl == nullptr) { // insert as the first
        this->LT_tbl = std::make_shared<LtTblEntry>(
          from_state, nullptr, this->new_states.states_new_array);
        return this->LT_tbl;
    }

    std::shared_ptr<LtTblEntry> e = this->LT_tbl;
    std::shared_ptr<LtTblEntry> e_prev = nullptr;
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from_state)
            return e;
        if (e->from_state > from_state) { // insert here.
            if (e_prev == nullptr) {      // insert as the first.
                this->LT_tbl = std::make_shared<LtTblEntry>(
                  from_state, nullptr, this->new_states.states_new_array);
                this->LT_tbl->next = e;
                return this->LT_tbl;
            } // insert in the middle.
            e_prev->next = std::make_shared<LtTblEntry>(
              from_state, nullptr, this->new_states.states_new_array);
            e_prev->next->next = e;
            return e_prev->next;
        }
        // else, go on to check the next entry.
    }

    // otherwise, insert at the end.
    e_prev->next = std::make_shared<LtTblEntry>(
      from_state, nullptr, this->new_states.states_new_array);
    return e_prev->next;
}

/// Find from state in the LT_tbl.
/// Same as LtTblEntry_find_insert() except that this has no insert.
///
/// There can be at most one entry found.
auto
LaneTracing::lt_tbl_entry_find(const StateHandle from_state)
  -> std::shared_ptr<LtTblEntry>
{
    if (this->LT_tbl == nullptr) { // insert as the first
        this->LT_tbl = std::make_shared<LtTblEntry>(
          from_state, nullptr, this->new_states.states_new_array);
        return this->LT_tbl;
    }

    std::shared_ptr<LtTblEntry> e = this->LT_tbl;
    std::shared_ptr<LtTblEntry> e_prev = nullptr;
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from_state)
            return e;
        if (e->from_state > from_state) { // insert here.
            if (e_prev == nullptr) {      // insert as the first.
                this->LT_tbl = std::make_shared<LtTblEntry>(
                  from_state, nullptr, this->new_states.states_new_array);
                this->LT_tbl->next = e;
                return this->LT_tbl;
            } // insert in the middle.
            e_prev->next = std::make_shared<LtTblEntry>(
              from_state, nullptr, this->new_states.states_new_array);
            e_prev->next->next = e;
            return e_prev->next;
        }
        // else, go on to check the next entry.
    }

    return nullptr;
}

/// Find the cur_red_config in the LtTblEntry e.
/// If not found, then insert it.
static auto
llist_context_set_get(Configuration* cur_red_config,
                      LtTblEntry* e) noexcept(false)
  -> LlistContextSet::iterator
{
    if (e == nullptr) {
        throw std::runtime_error("llist_context_set_get error: e is nullptr");
    }

    auto c = e->ctxt_set.begin();
    // else, try to find it in the list.
    for (; c != e->ctxt_set.end(); c++) {
        if (c->config == cur_red_config)
            return c;                                     // found.
        if (c->config->ruleID > cur_red_config->ruleID) { // insert here
            e->ctxt_set.insert(c, LlistContextSetItem(cur_red_config));
            return c;
        }
        // else, go on to check the next.
    }
    e->ctxt_set.insert(c, LlistContextSetItem(cur_red_config));
    return c;
}

/// Add the (from_state, config, context)
/// information to an entry in the LT_tbl.
///
/// Note:
/// 1) The from_state is unique for each entry.
/// 2) The LtTblEntry_add function must have been called on the
///    from_state before calling this function, so "from" state
///    always exists.
///
/// The current config is "cur_red_config" defined at the top.
void
LaneTracing::lt_tbl_entry_add_context(const StateHandle from_state,
                                      const SymbolList& context) noexcept(false)
{
    if (context.empty())
        return;

    // 1) locate the LtTblEntry for "from" state.
    const std::shared_ptr<LtTblEntry> e =
      this->lt_tbl_entry_find_insert(from_state);
    if (nullptr == e) {
        throw std::runtime_error(
          std::string("lt_tbl_entry_add_context error: state ") +
          std::to_string(from_state) + " NOT found\n");
    }

    // 2) locate the LlistContextSet from the current config.
    auto c = llist_context_set_get(this->cur_red_config, e.get());

    // 3) add/merge the context.
    e->ctxt_set.add_context(c, context);
}

/** END */

/************************************************************
 *  phase2_regeneration2 START.
 */

/// Return true if a and b are disjoint, false otherwise.
/// Similar to the function has_empty_intersection() in y.cpp.
/// This can be put into symbol_table.cpp.
static auto
symbol_list_disjoint(const SymbolList& a, const SymbolList& b) -> bool
{
    auto a_it = a.begin();
    auto b_it = b.begin();
    while (a_it != a.end() && b_it != b.end()) {
        if (a_it->snode == b_it->snode)
            return false;
        if (a_it->snode->symbol < b_it->snode->symbol) {
            a_it++;
        } else {
            b_it++;
        }
    }
    return true;
}

/// Check that the given context sets are pair_wise disjoint.
auto
LlistContextSet::pairwise_disjoint() const noexcept -> bool
{
    // if ctxt_set contains less than 2 nodes, return true.
    if (this->size() <= 1)
        return true;

    auto it1 = this->begin();
    for (; it1 != this->begin(); it1++) {
        auto it2 = it1;
        it2++;
        for (; it2 != this->end(); it2++) {
            if (!symbol_list_disjoint(it1->ctxt, it2->ctxt)) {
                return false;
            }
        }
    }

    return true;
}

/// Create a new cluster and add it to the all_clusters set.
/// e - the start state/LtTblEntry of this cluster.
///
/// By default, the states element's n1 and n2 are the same.
LtClusterEntry::LtClusterEntry(LtTblEntry* e, bool& all_pairwise_disjoint)
  : pairwise_disjoint(e->ctxt_set.pairwise_disjoint())
{
    // by default, n1 and n2 are the same.
    this->states = LlistState2(e->from_state, e->from_state);
    this->ctxt_set = e->ctxt_set.clone();

    if (!this->pairwise_disjoint)
        all_pairwise_disjoint = false;
}

void
LtClusterEntry::dump(LtTblEntry& lane_tracing_table) const noexcept
{
    LtTblEntry* e = nullptr;

    std::cout << "states: " << std::endl;
    for (const auto& [n1, n2] : this->states) {
        std::cout << n1 << "/" << n2 << " [to: ";
        e = lane_tracing_table.find_entry(n1);
        if (e != nullptr) {
            for (const auto& n : e->to_states) {
                auto m = this->states.find_n1(n);
                std::cout << n << "/"
                          << ((m == this->states.end()) ? -1 : m->second)
                          << " ";
            }
        }
        std::cout << "]" << std::endl;
    }
    std::cout << "context sets: ";
    this->ctxt_set.dump();

    std::cout << std::endl;
}

void
LaneTracing::all_clusters_dump()
{
    std::cout << "--all_clusters.START--" << std::endl;
    for (const auto& c : this->all_clusters) {
        c.dump(*this->LT_tbl);
    }
    std::cout << "--END--" << std::endl;
}

auto
LtClusterEntry::contain_state(std::optional<StateHandle> state_no)
  const noexcept -> std::optional<StateHandle>
{

    if (!state_no.has_value())
        return std::nullopt;
    if (this->states.empty())
        return std::nullopt;

    for (const auto& [n1, n2] : this->states) {
        if (n1 == state_no) {
            return n2;
        }
    }

    return std::nullopt;
}

auto
LtClusterEntry::contain_actual_state(std::optional<StateHandle> state_no)
  const noexcept -> std::optional<StateHandle>
{
    if (!state_no.has_value())
        return std::nullopt;

    for (const auto& [n1, n2] : this->states) {
        if (n2 == state_no) {
            return n1;
        }
    }

    return std::nullopt;
}

/// Combine the two chains dst and src:
///   if src contains a LlistContextSet node whose config is NOT in dst,
///     add it in INC order.
///   if src contains a LlistContextSet node whose config is in dst,
///     combine the context.
static void
llist_context_set_merge_chain(LlistContextSet& dst, const LlistContextSet& src)

{
    if (src.empty())
        return;
    if (dst.empty()) {
        dst = src.clone();
        return;
    }

    auto b = src.cbegin();
    auto a = dst.begin();
    std::optional<LlistContextSet::iterator> a_prev = std::nullopt;
    for (; a != dst.end(); a_prev = a, a++) {
        while (b != src.end()) {
            if (a->config->ruleID ==
                b->config->ruleID) { // same config, combine contexts.
                dst.add_context(a, b->ctxt);
                a_prev = a;
                a++;
                b++;
            } else if (a->config->ruleID < b->config->ruleID) {
                a_prev = a;
                a++;
            } else { // cmp > 0, insert b to dst before a.
                dst.insert(a, LlistContextSetItem(b->config));
                dst.add_context(a, b->ctxt);
                b++;
            }
            if (a == dst.end()) {
                break;
            }
        }

        if (b == src.end() || a == dst.end())
            break;
    }

    // clone b and its tail to the end of a_prev.
    for (; b != src.end(); b++) {
        dst.insert(a, LlistContextSetItem(b->config));
        dst.back().ctxt = clone_symbol_list(b->ctxt);
    }
}

static void
copy_config_lalr(Configuration* dst, Configuration* src)
{
    if (dst == nullptr || src == nullptr)
        return;

    dst->ruleID = src->ruleID;
    dst->nMarker = src->nMarker;
    dst->marker = src->marker;
    copy_context(*dst->context, *src->context);
    dst->owner = src->owner;

    dst->isCoreConfig = src->isCoreConfig;
    dst->COMPLETE = src->COMPLETE;
    dst->IN_LANE = src->IN_LANE;
    dst->ORIGINATOR_CHANGED = src->ORIGINATOR_CHANGED;
    dst->LANE_END = src->LANE_END;
    dst->LANE_CON = src->LANE_CON;

    dst->originators = src->originators;
    dst->transitors = src->transitors;
}

/// Clone `s` into a new `shared_ptr`.
static auto
clone_state(const Grammar& grammar, const State& s) -> std::shared_ptr<State>
{
    auto t = std::make_shared<State>();

    t->next = s.next;
    t->config = s.config;
    t->core_config_count = s.core_config_count;

    for (size_t i = 0; i < t->config.size(); i++) {
        t->config[i] = create_config(grammar, s.config[i]->ruleID, 0, 1);
        copy_config_lalr(t->config[i], s.config[i]);
        t->config[i]->owner = t;
    }

    t->state_no = s.state_no;
    t->trans_symbol = std::make_shared<SymbolNode>(s.trans_symbol->snode);

    t->successor_list = s.successor_list;

    t->parents_list = s.parents_list->clone();

    t->ON_LANE = s.ON_LANE;
    t->COMPLETE = s.COMPLETE;
    t->PASS_THRU = s.PASS_THRU;
    t->REGENERATED = s.REGENERATED;

    return t;
}

/*
 * In the successor list of src_state, replace s_old with s_new.
 */
static void
replace_successor(std::shared_ptr<State> src_state,
                  std::shared_ptr<State> s_new,
                  const State* s_old)
{
    if (s_new.get() == s_old)
        return;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "replace successor of state " << src_state->state_no
                  << ": " << s_old->state_no << " replaced by "
                  << s_new->state_no << ": ";
    }

    for (auto& successor : src_state->successor_list) {
        if (successor.get() == s_old) {
            successor = s_new;
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "done" << std::endl;
            }
            return;
        }
    }

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "NOT done" << std::endl;
    }
}

/// Used for LR(k) purpose only.
/// In the lane_head_tail_pairs list, replace
/// those entries whose tail config is s to s_copy
/// according to cur_red_config.
///
/// This solves this question: end_config is not a cur_red_config but
/// belongs to a state in the middle of a lane? Using this method
/// this is not a problem any more.
///
/// Here it's called when a new state is splitted, but
/// we replace ALL the pairs related to end_config->owner state.
///
/// Algorithm:
/// if (end_config->owner == s) {
///   for each ConfigPairNode n {
///     if (n->end_owner == s &&
///         n->start is in the same cluster as s_copy) {
///       replace n->end (the one in s) by the one in s_copy;
///     }
///   }
/// }
static void
lane_head_tail_pairs_replace(ConfigPairList& lane_head_tail_pairs,
                             const LtClusterEntry& c,
                             const Configuration& end_config,
                             const State* s,
                             State* s_copy)
{
    if (end_config.owner.get() != s)
        return;

    // std::cout << "LHTPR: end_config: " <<
    //        end_config->owner->state_no<< "." <<  end_config->ruleID <<
    //        std::endl;

    for (ConfigPairNode& n : lane_head_tail_pairs) {
        // n->end->owner == end_config->owner.
        // n->start->owner is a state in cluster c -- should I
        // search as n1 or n2??? I think should be n2, so it
        // covers the situation where n->start->owner is a
        // splitted state.
        if (n.end->owner.get() == s &&
            c.states.find_n2(n.start->owner->state_no) != c.states.end()) {
            // do replacement for n->end: from that in s to s_copy.
            for (size_t i = 0; i < s->config.size(); i++) {
                if (s->config[i] == n.end) {
                    n.end = s_copy->config[i];
                    break;
                }
            }
        }
    }
}

/// Add the LtTblEntry e->from_state to c.
///
/// Return: the parent state_no:
///   if a splitted state is created, return it's state_no.
///   else, return the old state_no.
auto
LaneTracing::cluster_add_lt_tbl_entry(LtClusterEntry& c,
                                      const StateHandle from_state,
                                      const LlistContextSet& e_ctxt,
                                      const size_t e_parent_state_no,
                                      const bool copy) -> StateHandle
{
    StateHandle state_no = from_state;

    if (copy) {
        // make a new state by copying e->from_state,
        // add it to the end of states_new array, and add here.
        std::shared_ptr<State> s_parent =
          this->new_states.states_new_array[e_parent_state_no].state;
        std::shared_ptr<State> s =
          this->new_states.states_new_array[state_no].state;
        std::shared_ptr<State> s_copy = clone_state(grammar, *s);
        // insert a state to the parsing machine. Defined in y.c.
        this->new_states.insert_state_to_pm(s_copy);

        // Replace src_state's previous succcessor with s_copy.
        replace_successor(s_parent, s_copy, s.get());

        state_no = s_copy->state_no;

        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "clone state " << s->state_no << " to " << state_no
                      << std::endl;
        }

        if (this->options.use_lr_k) {
            // For LR(k), replace entry in lane_head_tail_pairs!
            // std::cout << "parrent state: " << e_parent_state_no << std::endl;
            lane_head_tail_pairs_replace(this->lane_head_tail_pairs,
                                         c,
                                         *cur_red_config,
                                         s.get(),
                                         s_copy.get());
        }
    }

    // add state_no.
    // n1: original state, n2: splitted state.
    c.states.add_inc(from_state, state_no);
    // combine context sets.
    llist_context_set_merge_chain(c.ctxt_set, e_ctxt);

    return state_no;
}

/// From all_clusters, find the one(s) that contains
/// the state (whose state number is state_no).
///
/// Note there could be more than one such cluster if the
/// given state was splitted before.
///
/// This function is like a iterator. To use this function,
/// initialized c = nullptr, then call in a loop:
///   c = nullptr;
///   while ((c = find_containing_cluster(c, state_no) != nullptr) {
///     ...
///   }
static auto
find_containing_cluster(LtCluster& all_clusters,
                        LtCluster::iterator c,
                        StateHandle state_no) -> LtCluster::iterator
{
    if (c == all_clusters.end()) {
        c = all_clusters.begin();
    } else {
        c++;
    }

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        if (c != all_clusters.end())
            std::cout << "c: current state: " << c->states.front().first << "/"
                      << c->states.front().second << std::endl;
    }

    for (; c != all_clusters.end(); c++) {
        if (c->contain_state(state_no).has_value()) {
            return c;
        }
    }
    return c; // c is all_clusters.end()
}

/// Different from find_containing_cluster in that:
///
/// Find the cluster than contains the ACTUAL state_no.
/// There can be at most one such cluster.
auto
LtCluster::find_actual_containing_cluster(StateHandle state_no) noexcept
  -> LtCluster::iterator
{
    for (auto c = this->begin(); c != this->end(); c++) {
        if (c->contain_actual_state(state_no).has_value()) {
            return c;
        }
    }
    return this->end();
}

auto
LtTblEntry::find_entry(StateHandle from_state) noexcept -> LtTblEntry*
{
    for (LtTblEntry* e = this; e != nullptr; e = e->next.get()) {
        if (e->from_state == from_state)
            return e;
        if (e->from_state > from_state)
            break;
    }

    return nullptr; // not found.
}

void
LtClusterEntry::combine(const LtClusterEntry& other)
{
    // merge states.
    for (const auto& [n1, n2] : other.states) {
        this->states.add_inc(n1, n2);
    }
    // merge context sets.
    llist_context_set_merge_chain(this->ctxt_set, other.ctxt_set);
}

/// functions for updating context in phase 2 regeneration. START.

/// Called by cluster_trace_new_chain() only.
void
LaneTracing::inherit_propagate(const StateHandle state_no,
                               const StateHandle parent_state_no,
                               const LtClusterEntry& container,
                               const LtTblEntry* e)
{
    std::shared_ptr<State> s =
      this->new_states.states_new_array[state_no].state;
    std::shared_ptr<State> s_p =
      this->new_states.states_new_array[parent_state_no].state;
    if (inherit_parent_context(grammar, s, s_p.get())) {
        this->get_state_closure(s); /* needed only if context changed.*/
        this->lt_phase2_propagate_context_change(state_no, container, e);
    }
}

void
LaneTracing::clear_inherit_regenerate(const StateHandle state_no,
                                      const StateHandle parent_state_no)
{
    std::shared_ptr<State> s =
      this->new_states.states_new_array[state_no].state;
    std::shared_ptr<State> s_p =
      this->new_states.states_new_array[parent_state_no].state;
    clear_state_context(s.get());
    if (inherit_parent_context(this->grammar, s, s_p.get())) {
        this->get_state_closure(s); /* needed only if context changed.*/
    }
}

/// Called by phase2_regeneration2() only.
///
/// Note that if it is state 0, then should add $end
/// to the goal rule before get_closure() !
void
LaneTracing::clear_regenerate(const StateHandle state_no)
{
    std::shared_ptr<State> s =
      this->new_states.states_new_array[state_no].state;
    clear_state_context(s.get());
    if (state_no == 0) {
        hash_tbl_insert(STR_END);
        s->config[0]->context->context.clear();
        s->config[0]->context->context.emplace_back(hash_tbl_find(STR_END));
    }
    this->get_state_closure(s);
}

/// Function to propagate context change until a state where
/// there is no more context change.
///
/// Here the children states are those defined in the
/// LtTblEntry's to_states list.
void
LaneTracing::lt_phase2_propagate_context_change(const StateHandle state_no,
                                                const LtClusterEntry& c,
                                                const LtTblEntry* e)
{
    if (e == nullptr) {
        return;
    }
    // find all the to_states, do recursively until no context change.
    // in cluster c, state_no has a corresponding true state_no,
    // so are its to_states which can be found from e.

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "lt_p2_propagateContextChange from state " << state_no
                  << std::endl;
    }

    for (const auto& n : e->to_states) {
        const LtTblEntry* f = this->LT_tbl->find_entry(n);
        if (f != nullptr) {
            // need to replace t->n with the true state_no in cluster c.
            auto t2 = c.states.find_n1(n);
            if (t2 == c.states.end()) {
                if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                    std::cout << "--to state " << n << ", not found in cluster."
                              << std::endl;
                    // cluster_dump(c);
                }
                continue; // if not found, just ignore this to state.
            }
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "--to state: " << n << ", actual: " << t2->second
                          << std::endl;
            }
            this->inherit_propagate(t2->second, state_no, c, f);
        }
    }
}

auto
inherit_parent_context(const Grammar& grammar,
                       const std::shared_ptr<State> s,
                       const State* parent) -> bool
{
    if (s == nullptr || parent == nullptr)
        return false;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "state " << s->state_no
                  << ": to inherit context from parent state "
                  << parent->state_no << std::endl;
    }

    bool is_changed = false;
    const std::shared_ptr<const SymbolTableNode> trans_symbol =
      s->trans_symbol->snode;
    for (const auto& c_p : parent->config) {
        if (is_final_configuration(grammar, c_p))
            continue;
        if (trans_symbol != get_scanned_symbol(*c_p))
            continue;

        c_p->marker++;
        size_t config_index = 0;
        const Configuration* c =
          find_similar_core_config(s, c_p, &config_index);
        c_p->marker--;
        if (nullptr == c)
            continue; // should NOT happen.

        if (combine_context(c->context, c_p->context)) {
            is_changed = true;
        }
    }

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        if (false && s->state_no == 39) {
            std::cout << "after: " << std::endl;
            my_write_state(grammar, *s);
        }
    }

    return is_changed;
}

/// Set a state's all configs' context to empty.
void
clear_state_context(State* s)
{
    if (s == nullptr)
        return;

    for (auto& i : s->config) {
        i->context->clear(); // defined in y.c.
    }
}

// functions for updating context in phase 2 regeneration. END.

/// Recursively add states to LtCluster c along the chain in LT_tbl.
///
/// @param c - The new cluster,
/// @param parent_state_no - the parent state no.
/// @param state_no - the state no of the from_state LT_tbl.
/// @return
///  false - combined with a cluster in all_clusters..
///  true - NOT combined with another cluster in all_clusters.
auto
LaneTracing::cluster_trace_new_chain(StateHandle parent_state_no,
                                     StateHandle state_no) -> bool
{
    bool is_new_chain = true;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        // TODO: how to display `this->new_cluster` ?
        // std::cout << "cluster: " << this->new_cluster << ". next state on
        // chain: " << state_no << std::endl;
    }

    // e will be used no matter what happen.
    LtTblEntry* e = this->LT_tbl->find_entry(state_no);
    if (e == nullptr) { // Is this possible? YES IT IS.
        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "END of chain - state_no: " << state_no << std::endl;
        }
    }
    const LlistContextSet* e_ctxt = nullptr;
    if (e != nullptr) {
        e_ctxt = &e->ctxt_set;
    }

    // state in in cluster c.
    std::optional<StateHandle> ret_state_opt =
      this->new_cluster.contain_state(state_no);
    if (ret_state_opt.has_value()) {
        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "=>2. state " << *ret_state_opt
                      << ": inherit context from state " << parent_state_no
                      << " & propagate" << std::endl;
        }
        this->inherit_propagate(
          *ret_state_opt, parent_state_no, this->new_cluster, e);

        std::shared_ptr<State> old_state =
          this->new_states.states_new_array[state_no].state;
        replace_successor(
          this->new_states.states_new_array[parent_state_no].state,
          this->new_states.states_new_array[*ret_state_opt].state,
          old_state.get());
        return true; // NOTE here it returns true.
    }

    // otherwise, state is NOT in c yet.

    // There could be more than one of this because of
    // copies made. Find the first one that has no conflict.
    // If all have conflict, use the first one.
    bool container_not_found = true;
    auto first_container = this->all_clusters.end();
    auto container = this->all_clusters.end();

    // note: this while loop can ignore the current cluster since
    // it has already been searched once above.
    while ((container = find_containing_cluster(
              this->all_clusters, container, state_no)) !=
           this->all_clusters.end()) {
        if (first_container == this->all_clusters.end())
            first_container = container;

        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            // TODO: find a way to display `container`
            // std::cout << "2. state " << state_no << " is in a cluster "
            //           << container << std::endl;
        }

        if (container->pairwise_disjoint) {
            LlistContextSet x = this->new_cluster.ctxt_set.clone();
            llist_context_set_merge_chain(x, container->ctxt_set);
            bool is_pairwise_disjoint = x.pairwise_disjoint();

            if (is_pairwise_disjoint) {
                if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                    std::cout
                      << "3. combine 2 clusters result is pairwise disjoint"
                      << std::endl;
                }
                this->new_cluster.combine(
                  *container);        // container is the result.
                is_new_chain = false; // not a new chain.

                // This is used so cluster_contain_state() at the beginning
                // of this function can return correct value once c is changed.
                this->new_cluster = *container;
                // c = this->new_cluster;

                if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                    std::cout << "=>3. state " << state_no
                              << ": inherit context from state "
                              << parent_state_no << ", ";
                }
                this->inherit_propagate(
                  state_no, parent_state_no, *container, e);

                container_not_found = false;
                break;
            }
        }
        // else, NOT pairwise disjoint, continue to find the next match.
    }

    StateHandle ret_state{};
    if (container_not_found) {
        if (first_container == this->all_clusters.end()) { // always add.
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "4. state " << state_no
                          << " is NOT in any cluster yet" << std::endl;
            }
            ret_state = this->cluster_add_lt_tbl_entry(
              this->new_cluster, state_no, *e_ctxt, parent_state_no, false);

            if (this->new_cluster.pairwise_disjoint &&
                !this->new_cluster.ctxt_set.pairwise_disjoint()) {
                this->new_cluster.pairwise_disjoint = false;
                all_pairwise_disjoint = false;
            }
            if (e != nullptr)
                e->processed = true;

            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "=>4. state " << state_no
                          << ": clear, inherit context from state "
                          << parent_state_no << ", ";
            }
            this->clear_inherit_regenerate(state_no, parent_state_no);

        } else {
            // e is already in another cluster, e.g., first_container;
            // but combined context are NOT pairwise-disjoint.
            // so make a copy e' of e and add it to c.
            ret_state = this->cluster_add_lt_tbl_entry(
              this->new_cluster, state_no, *e_ctxt, parent_state_no, true);
            this->new_cluster.pairwise_disjoint =
              this->new_cluster.ctxt_set.pairwise_disjoint();
            if (!this->new_cluster.pairwise_disjoint) {
                all_pairwise_disjoint = false;
            }
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "=>5. state " << ret_state
                          << ": clear, inherit context from state "
                          << parent_state_no << ", ";
            }
            this->clear_inherit_regenerate(ret_state, parent_state_no);
        }

        if (e != nullptr) { // recursively trace the chain.
            is_new_chain = this->cluster_trace_new_chain_all(ret_state, *e);
        }
    }

    return is_new_chain;
}

/// parent_state: state_no of the parent state.
auto
LaneTracing::cluster_trace_new_chain_all(const StateHandle parent_state,
                                         const LtTblEntry& e) -> bool
{
    bool is_new_chain = true;

    // recursively trace the chain.
    for (const auto& n : e.to_states) {
        if (!this->cluster_trace_new_chain(parent_state, n)) {
            is_new_chain = false;
        }
    }

    return is_new_chain;
}

void
LaneTracing::phase2_regeneration2()
{
    bool is_new_chain = true;

    all_clusters.clear(); // initialize all_clusters.
    all_pairwise_disjoint = true;

    for (std::shared_ptr<LtTblEntry> e_ptr = LT_tbl; e_ptr != nullptr;
         e_ptr = e_ptr->next) {
        LtTblEntry& e = *e_ptr;
        if (e.processed)
            continue;

        this->new_cluster = LtClusterEntry(&e, this->all_pairwise_disjoint);

        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "== chain head state: " << e.from_state << std::endl;
            std::cout << "=>1. clear and regenerate context for state "
                      << e.from_state << std::endl;
        }
        this->clear_regenerate(e.from_state);

        e.processed = true;

        is_new_chain = this->cluster_trace_new_chain_all(e.from_state, e);

        // add new_cluster to the all_clusters list.
        if (is_new_chain) {
            this->all_clusters.push_back(this->new_cluster);
        }
    }

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        this->all_clusters_dump(); // dump if is new chain.
    }

    // if the parsing machine is expanded, update the parsing table.
    if (this->new_states.states_new->state_count > ParsingTblRows) {
        this->update_parsing_table();
    }

    std::cout << "LT-LTT end: states count: "
              << this->new_states.states_new->state_count << std::endl;
}

/************************************************************
 * phase2_regeneration2() END
 */

/* for laneHeads list */

LaneHeadState::LaneHeadState(const std::shared_ptr<State> s,
                             const std::shared_ptr<SymbolTableNode> n) noexcept
  : s(std::move(s))
{
    this->contexts.emplace_back(n);
}

/// Add another pair of s to h.
/// lh_list is in INC order of state's state_no;
static void
add_lane_head_list(LaneHead& lh_list, std::shared_ptr<State> s) noexcept(false)
{
    if (s == nullptr) {
        throw std::runtime_error(
          "[lane_tracing.cpp add_lane_head_list] error: s is nullptr");
    }

    // else, search if s is in list.
    auto h = lh_list.begin();
    for (; h != lh_list.end(); h++) {
        if (h->s == s) {
            return;
        } // s exists already.
        if (h->s->state_no > s->state_no)
            break; // guarantee INC order.
    }

    // otherwise, s is NOT in list lh_list. Add another LaneHeadState node.
    lh_list.insert(h, LaneHeadState(s, nullptr));
}

/* for originator list */

/// Add originator to c's originator list if it does not exist yet.
///
/// @parameter:
///  cycle: 1 - called from combineConfigOriginators().
///             Is a cycle, should insert to originator list.
///         0 - called from getConfigSuccessors_LR0().
///             NOT a cycle, should NOT insert to originator list.
static auto
insert_originator_list(Configuration& c, Configuration* originator, int cycle)
  -> bool
{
    if (&c == originator && cycle == 0) {
        return false;
    }
    for (const auto& item : c.originators.list) {
        if (originator == item)
            return false; // already exists.
    }

    c.originators.list.push_back(originator);
    c.ORIGINATOR_CHANGED = true;
    return true;
}

void
write_originator_list(OriginatorList* o)
{
    if (o == nullptr)
        return;

    for (const ConfigurationNode* c : o->list) {
        std::cout << c->owner->state_no << "." << c->ruleID << " ";
    }
}

/* For transitor list */

/// Add transitor to c's transitor list if it does not exist yet.
static auto
insert_transitor_list(Configuration& c, Configuration* transitor) -> bool
{
    if (&c == transitor)
        return false;

    for (const ConfigurationNode* item : c.transitors.list) {
        if (transitor == item)
            return false; // already exists.
    }

    c.transitors.list.push_back(transitor);
    return true;
}

/* for inadequate states */

auto
create_state_no_array() -> StateNoArray*
{
    auto* sa = new StateNoArray;
    sa->states.reserve(2);
    return sa;
}

/// If state_no is not in the inadequate states list, add it.
void
add_state_no_array(StateNoArray& sa, StateHandle state_no)
{
    for (StateHandle s : sa.states) {
        if (s == state_no) {
            return;
        } // exists.
    }
    sa.states.push_back(state_no);
}

void
dump_state_no_array(const StateNoArray* sa)
{
    if (sa == nullptr)
        return;

    for (const StateHandle state : sa->states) {
        std::cout << state << " ";
    }
    std::cout << std::endl;
}

static void
my_write_context(const Context& c)
{
    std::cout << " {";

    auto s = c.context.begin();
    if (s != c.context.end()) {
        std::cout << s->snode->symbol;
        for (; s != c.context.end(); s++) {
            std::cout << ", " << s->snode->symbol;
        }
    }

    std::cout << "} ";
}

static void
my_write_production(const Production* p, size_t marker)
{
    if (p == nullptr) {
        // std::cout << "writeProduction warning: p is nullptr" << std::endl;
        return;
    }

    std::cout << p->nLHS->snode->symbol << " -> ";

    size_t i = 0;
    for (const auto& n : p->nRHS) {
        if (i == marker)
            std::cout << ". ";
        std::cout << n.snode->symbol << " ";
        i++;
    }
    if (i == marker)
        std::cout << ". ";
}

void
stdout_write_config(const Grammar& grammar, const Configuration* c)
{
    if (c == nullptr) {
        return;
    }
    std::cout << "config (" << c->owner->state_no << "." << c->ruleID << ") : ";
    my_write_production(grammar.rules[c->ruleID], c->marker);
    my_write_context(*c->context);
    std::cout << "[COMPLETE: " << c->COMPLETE << "]"
              << "[IN_LANE: " << c->IN_LANE << "]"
              << "[LANE_END: " << c->LANE_END << "]"
              << "[LANE_CON: " << c->LANE_CON << "]" << std::endl;
}

void
my_write_state(const Grammar& grammar, const State& s)
{
    std::cout << "state_no: " << s.state_no << " (core: " << s.core_config_count
              << ")" << std::endl;

    for (const auto& config : s.config) {
        stdout_write_config(grammar, config);
    }
}

/// For debug use.
void
Configuration::write_originators(std::ostream& os) const noexcept
{
    const size_t ct = this->originators.list.size();
    if (ct == 0)
        return;

    os << "      " << ct << " originator" << ((ct > 1) ? "s" : "") << ": "
       << std::endl;
    for (const Configuration* o : this->originators.list) {
        os << "      originator (" << o->owner->state_no << "." << o->ruleID
           << "." << o->marker << ") " << std::endl;
    }
}

void
Configuration::write_transitors(std::ostream& os) const noexcept
{
    const size_t ct = this->transitors.list.size();
    if (ct == 0)
        return;

    os << "      " << ct << " transitor" << ((ct > 1) ? "s" : "") << ": "
       << std::endl;
    for (const Configuration* o : this->transitors.list) {
        os << "      transitor (" << o->owner->state_no << "." << o->ruleID
           << "." << o->marker << ") " << std::endl;
    }
}

/// Get context for reduce productions in conflict states.
void
LaneTracing::get_inadequate_state_reduce_config_context(const State* s)
{
    if constexpr (DEBUG_PHASE_1) {
        std::cout << "state " << s->state_no << " [" << s->config.size()
                  << " configurations, trans_symbol: "
                  << s->trans_symbol->snode->symbol << "]: " << std::endl;
    }

    for (const auto& config : s->config) {
        Configuration* c = config;
        if (is_final_configuration(grammar, c)) {
            this->lane_tracing_reduction(c);
            if constexpr (DEBUG_PHASE_1) {
                std::cout << ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                          << std::endl;
            }
        }
    }
}

void
LaneTracing::phase1()
{
    if constexpr (DEBUG_PHASE_1) {
        std::cout << states_inadequate->states.size()
                  << " inadequate states: " << std::endl;
    }

    for (const StateHandle state_no : states_inadequate->states) {
        const State* s =
          this->new_states.states_new_array[state_no].state.get();
        this->get_inadequate_state_reduce_config_context(s);
    }

    // next will check resolved conflicts.
}

/*
 * called by update_action_table().
 */
static auto
find_successor_state_no(const StateHandle state_no,
                        const StateArray& states_new_array,
                        const std::shared_ptr<const SymbolTableNode> snode)
  -> StateHandle
{
    const State& state = *states_new_array[state_no].state;
    auto it = state.successor_list.rbegin();
    while (it != state.successor_list.rend()) {
        const State& successor = *it->get();
        if (snode == successor.trans_symbol->snode) {
            return successor.state_no;
        }
    }

    // this should NEVER happen.
    std::cerr << "find_successor_state_no(" << state_no << ", " << snode->symbol
              << "): ERROR" << std::endl;
    return 0;
}

/*
 * Clear a parsing table row where lookahead is terminal.
 */
inline void
clear_state_terminal_transitions(const Grammar& grammar, StateHandle state_no)
{
    size_t start = state_no * ParsingTblColHdr.size();
    size_t end =
      (state_no * ParsingTblColHdr.size()) + (grammar.terminal_list.size() + 1);
    for (size_t i = start; i < end; ++i) {
        ParsingTable.at(i) = std::nullopt;
    }
}

/*
 * clear all the conflicts from states_new_array->conflict_list.
 */
static void
clear_state_conflicts(StateHandle state_no, StateArray& states_new_array)
{
    states_new_array[state_no].conflict = nullptr;
}

/*
 * Algorithm:
 * resolve_conflict_2(State S) {
 *  clear the parsing table row for S where lookahead is terminal.
 *  clear all the conflicts associated with S.
 *
 *  foreach config c in S which is an x-transition or reduce {
 *    insert_action(c) to parsing table. Any conflicts will be recorded.
 *  }
 * }
 */
void
LaneTracing::resolve_lalr1_conflicts()
{
    if constexpr (DEBUG_RESOLVE_CONFLICT) {
        print_parsing_table(this->grammar.fp_v, this->grammar);
    }

    states_inadequate->count_unresolved = states_inadequate->states.size();

    const size_t ct = states_inadequate->states.size();
    for (size_t i = 0; i < ct; i++) {
        const StateHandle state_no = states_inadequate->states[i];
        if (state_no < 0)
            continue; // should never happen.

        std::shared_ptr<const State> state =
          new_states.states_new_array[state_no].state;

        // clear the parsing table row for S where lookahead is terminal.
        if constexpr (DEBUG_RESOLVE_CONFLICT) {
            std::cout << "-----clear state[" << i << "] = " << state_no
                      << ". len=" << ct << std::endl;
        }
        clear_state_terminal_transitions(this->grammar, state_no);

        // clear all the conflicts associated with S.
        clear_state_conflicts(state_no, this->new_states.states_new_array);

        // re-insert actions into parsing table for this state.
        for (auto it = state->config.rbegin(); it < state->config.rend();
             it++) {
            const Configuration* config = *it;
            if (is_final_configuration(grammar, config) &&
                config->context != nullptr) {
                // insert reduce
                for (const auto& contxt : config->context->context) {
                    this->insert_action(
                      contxt.snode,
                      state_no,
                      ParsingAction::new_reduce(config->ruleID));
                }
            } else if (!config->nMarker.empty() &&
                       config->nMarker.front().snode->is_terminal()) {
                // insert shift.
                this->insert_action(
                  config->nMarker.front().snode,
                  state_no,
                  ParsingAction::new_shift(
                    find_successor_state_no(state_no,
                                            new_states.states_new_array,
                                            config->nMarker.front().snode)));
            }
        }
        std::shared_ptr<Conflict>& c =
          this->new_states.states_new_array[state_no].conflict;
        if (c == nullptr) {
            states_inadequate->states[i] = -1;
            states_inadequate->count_unresolved--;
        } else {
            // do nothing.
        }
    }

    if constexpr (DEBUG_RESOLVE_CONFLICT) {
        print_parsing_table(this->grammar.fp_v, this->grammar);
    }
}

/// Remove all states marked with `PASS_THRU` in `lh_list`.
static void
remove_pass_through_states(LaneHead& lh_list)
{
    auto h = lh_list.begin();
    while (h != lh_list.end()) {
        if constexpr (DEBUG_PHASE_2) {
            std::cout << "state " << h->s->state_no
                      << ", PASS_THRU: " << h->s->PASS_THRU << std::endl;
        }
        if (h->s->PASS_THRU == 1u) {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "Is pass thru state! Remove it from laneHead list"
                          << std::endl;
            }
            // now remove this state from lh_list.
            lh_list.erase(h);
        } else {
            h++;
        }
    }
}

void
LaneTracing::gpm(std::shared_ptr<State> new_state)
{
    if (this->options.debug_gen_parsing_machine) {
        std::cout << std::endl
                  << std::endl
                  << "--generate parsing machine--" << std::endl
                  << "states_new count: "
                  << this->new_states.states_new->state_count << std::endl;
    }

    while (new_state != nullptr) {
        if (this->options.debug_gen_parsing_machine) {
            this->grammar.fp_v << this->new_states.states_new->state_count
                               << " states, current state is "
                               << new_state->state_no << std::endl;
        }

        this->get_state_closure(new_state); // get closure of this state.

        // get successor states and add them to states_new.
        this->state_transition(new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    ParsingTblRows = this->new_states.states_new->state_count;
    this->n_state_opt1 = this->new_states.states_new->state_count;

    std::cout << "LT-PGM end: states count: "
              << this->new_states.states_new->state_count << std::endl;
}

/*
 * Adapted from transition() in y.c.
 */
static auto
get_state_successors(const Grammar& grammar, const State& s) -> StateCollection*
{
    StateCollection* coll = create_state_collection();

    for (const auto& c : s.config) {
        if (is_final_configuration(grammar, c)) {
            // do nothing.
        } else { // do transit operation.
            std::shared_ptr<SymbolTableNode> scanned_symbol =
              get_scanned_symbol(*c);
            if (scanned_symbol->symbol->empty()) { // empty reduction.
                continue;
            }
            std::shared_ptr<State> new_state =
              find_state_for_scanned_symbol(coll, scanned_symbol);
            if (new_state == nullptr) {
                new_state = std::make_shared<State>();
                // record which symbol this state is a successor by.
                new_state->trans_symbol =
                  std::make_shared<SymbolNode>(scanned_symbol);
                coll->add_state2(new_state);
            }
            // create a new core config for new_state.
            Configuration& new_config =
              *create_config(grammar, c->ruleID, 0, 1);

            new_config.owner = new_state;
            copy_config(new_config, *c);
            new_config.isCoreConfig = 1;
            new_config.marker++;
            if (!new_config.nMarker.empty())
                new_config.nMarker.pop_front();

            add_core_config2_state(grammar, new_state, &new_config);
        }
    } // end for

    return coll;
}

/*
 * Given the trans_symbol, find the index of the
 * successor of s that has this trans_symbol.
 */
static auto
get_successor_index(const State& s,
                    const std::shared_ptr<const SymbolTableNode> trans_symbol)
  -> std::optional<size_t>
{
    for (size_t i = 0; i < s.successor_list.size(); i++) {
        if (s.successor_list[i]->trans_symbol->snode == trans_symbol) {
            return i; // s->successor_list[i];
        }
    }
    // the following code should never be reached.
    std::cout << "[lane_tracing.cpp get_successor_index] error:" << std::endl
              << " nullptr found on state " << s.state_no << ", trans_symbol "
              << trans_symbol->symbol << std::endl;
    return std::nullopt; // this should NEVER happen
}

/*
 * Update S's successor Y0 to be Y:
 *   add Y as a new state,
 *   update the parsing table, and the successor link of S
 *   from Y0 to Y.
 * @Input: S is the source state of Y.
 * @Return - true if a new state is really added.
 *          false if an existing state is found.
 */
auto
LaneTracing::add_split_state(std::shared_ptr<State> y,
                             State& s,
                             size_t successor_index) -> bool
{
    auto [os, is_compatible] =
      this->state_hash_table.search(y, *this); // same or compatible.

    if (os == nullptr) { // No existing state found. Add Y as a new state.
        y->state_no = this->new_states.states_new->state_count;
        if constexpr (DEBUG_PHASE_2_REGENERATE) {
            // std::cout << "split - new state" << std::endl;
            std::cout << "split - add new state " << y->state_no << std::endl;
        }
        this->new_states.states_new->add_state2(y);
        this->new_states.states_new_array.add_state(y);
        // update shift action.
        update_action(get_col(*y->trans_symbol->snode),
                      s.state_no,
                      ParsingAction::new_shift(y->state_no));
        // update the Y0 successor link of S to Y.
        s.successor_list[successor_index] = y;
        while (this->new_states.states_new->state_count >= PARSING_TABLE_SIZE) {
            // TODO
            // expand_parsing_table(this->new_states.states_new_array);
        }
        return true;
    } // same or compatible with an existing state.
    // std::cout << "split - old state" << std::endl;
    update_action(get_col(*os->trans_symbol->snode),
                  s.state_no,
                  ParsingAction::new_shift(os->state_no));
    s.successor_list[successor_index] = os;
    return false;
}

static void
add_unique_queue(std::shared_ptr<State> s, LaneHead& lh_list)
{
    for (const auto& lane_head : lh_list) {
        if (lane_head.s == s) { // exists already.
            if constexpr (DEBUG_PHASE_2_REGENERATE) {
                std::cout << "state " << s->state_no
                          << " already on laneHead queue" << std::endl;
            }
            return;
        }
    }
    lh_list.emplace_back(s, nullptr);
}

/// Replace the context of S by those of T.
///
/// Assumption: S and T are as in the phase2_regeneration()
/// function: they contains the same number of configurations.
/// T will be destroyed, so its context lists can be moved to S.
static void
regenerate_state_context(State& s, const State& t) noexcept(false)
{
    if (t.core_config_count != s.core_config_count) {
        throw std::runtime_error("regenerate error: inequal config_count");
    }

    // clear the context of S.
    for (const auto& config :
         s.config) { // -> if final config, remove p.t. entry.
        config->context->context.clear();
    }

    // copy the context from T to S.
    const size_t ct = t.core_config_count;
    for (size_t i = 0; i < ct; i++) {
        copy_context(*s.config[i]->context, *t.config[i]->context);
    }
}

/*
 * Combine the contexts from T to S. No propagation here.
 */
static auto
combine_state_context(State& s_dest, const State& s_src) -> bool
{
    bool is_changed = false;
    for (size_t i = 0; i < s_dest.core_config_count; i++) {
        if (combine_context(s_dest.config[i]->context,
                            s_src.config[i]->context)) {
            is_changed = true;
        }
    }
    return is_changed;
}

/*
 * update reduce actions in parsing table for S.
 * note that transition actions are handled by addSplitState()
 * or keep unchanged.
 */
void
LaneTracing::update_state_reduce_action(State& s)
{
    for (const auto& c : s.config) {
        // update reduce action for final/empty production.
        if (is_final_configuration(this->grammar, c) ||
            get_scanned_symbol(*c)->symbol->empty()) {
            for (const auto& lookahead : c->context->context) {
                auto [action, state_dest] = get_action(
                  lookahead.snode->type, get_col(*lookahead.snode), s.state_no);
                if (state_dest != c->ruleID) {
                    if (!action.has_value() || *action == Action::Reduce) {
                        update_action(get_col(*lookahead.snode),
                                      s.state_no,
                                      ParsingAction::new_reduce(c->ruleID));
                    } else { // else, is "s" or "acc".
                        this->insert_action(
                          lookahead.snode,
                          s.state_no,
                          ParsingAction::new_reduce(c->ruleID));
                    }
                }
            }
        }
    }
}

/// Algorithm is:
/// ==START==
/// Let Q be a queue containing the start states.
/// Associate these start state's contexts to be empty sets.
///
/// while (Q is not empty) {
///  s <- next state in Q;
///  coll <- regenerate successors of s;
///  foreach state t in coll {
///    if the corresponding t0 is on conflicting lane {
///       if (t0 is original) {
///         replace contexts in t0 with those in t;
///         // now t0 is not original
///       } else {
///         if (t is compatible with t0) {
///           combine t to to t0;
///           if (t0 is not in Q) { add t0 to Q; }
///         } else {
///           add t as new state;
///         }
///       }
///     }
///   }
/// }
///
/// // now Q is empty.
/// GPM() on the new states.
/// ==END==
void
LaneTracing::phase2_regeneration(LaneHead& lh_list)
{
    std::shared_ptr<State> new_state = nullptr;
    bool exists = false;

    // 1) handle the head states and PASS_THRU states.
    for (auto& lane_head : lh_list) {
        std::shared_ptr<State>& s = lane_head.s;
        this->get_state_closure(s);
        this->update_state_reduce_action(*s);

        if constexpr (DEBUG_PHASE_2_REGENERATE) {
            std::cout << std::endl
                      << "==reg" << std::endl
                      << "erate state " << s->state_no << std::endl;
            my_write_state(grammar, *s);
        }
        const StateCollection* coll = get_state_successors(grammar, *s);

        for (std::shared_ptr<State> y = coll->states_head; y != nullptr;
             y = y->next) {
            std::optional<size_t> successor_index_opt =
              get_successor_index(*s, y->trans_symbol->snode);

            if (!successor_index_opt.has_value())
                continue; // should NEVER happen.
            size_t successor_index = *successor_index_opt;
            std::shared_ptr<State> y0 = s->successor_list[successor_index];

            if (y0->PASS_THRU == 0u) {
                if constexpr (DEBUG_PHASE_2_REGENERATE) {
                    std::cout << "state " << y0->state_no
                              << " PASS_THRU == 0 - NOT on lane" << std::endl;
                }
                continue; // NOT on 'conflicting' lane.
            }
            if constexpr (DEBUG_PHASE_2_REGENERATE) {
                std::cout << "state " << y0->state_no
                          << " PASS_THRU == 1 - on lane" << std::endl;
                std::cout << y->trans_symbol->snode->symbol
                          << " successor isOnConflictingLane" << std::endl;
            }

            if (y0->REGENERATED == 0u) { // is original.
                if constexpr (DEBUG_PHASE_2_REGENERATE) {
                    std::cout << "replace - replace old state " << y0->state_no
                              << std::endl;
                }
                // replace the context of Y0 with those of Y.
                regenerate_state_context(*y0, *y);
                y0->REGENERATED = 1;
                add_unique_queue(y0, lh_list);
            } else { // is regenerated state.
                exists = is_compatible_states(y0.get(), y.get());

                if (exists) {
                    if constexpr (DEBUG_PHASE_2_REGENERATE) {
                        std::cout << "combine to compatible state "
                                  << y0->state_no << std::endl;
                    }
                    combine_state_context(*y0, *y);
                    add_unique_queue(y0, lh_list);
                } else {
                    if (this->add_split_state(y, *s, successor_index)) {
                        if (new_state == nullptr) {
                            new_state = y;
                        }
                        if constexpr (DEBUG_PHASE_2_REGENERATE) {
                            std::cout << "split - new state added" << std::endl;
                        }
                    }
                }
            }
        }
    }

    // 2) handle the new added states.
    //    If there are any new split states, do GPM on them.
    if (new_state != nullptr) {
        if constexpr (DEBUG_PHASE_2_REGENERATE) {
            std::cout << "GRM(new_state) now." << std::endl;
        }
        this->gpm(new_state);
    }
}

auto
LaneTracing::get_the_context(const Configuration* o) noexcept(false)
  -> SymbolList
{
    if (o == nullptr)
        return {};

    if (o->nMarker.empty())
        return {};

    // TODO: shoud be o->nMarker.next
    SymbolList gamma_theads = get_theads(
      this->grammar, o->nMarker); // Note nullptr is a valid terminal.

    // note that "" will be the first node in the INC list,
    // so it's not so inefficient.
    remove_from_symbol_list(gamma_theads, hash_tbl_find(""));

    if constexpr (DEBUG_PHASE_2_GET_TBL) {
        if (!gamma_theads.empty()) {
            std::cout << "C: Add context to entry state " << o->owner->state_no
                      << ": ";
            std::cout << "[" << cur_red_config->owner->state_no << "."
                      << cur_red_config->ruleID << "] ";
            auto b = gamma_theads.begin();
            std::cout << "{";
            if (!gamma_theads.empty()) {
                std::cout << b->snode->symbol;
                b++;
                for (; b != gamma_theads.end(); b++) {
                    std::cout << ", " << b->snode->symbol;
                }
            } else
                std::cout << "EMPTY";
            std::cout << "}";
            std::cout << std::endl;
        }
    }

    this->lt_tbl_entry_add_context(o->owner->state_no,
                                   gamma_theads); // add context.

    return gamma_theads;
}

/// Recursively trace back the lane along originators until
/// a config labeld as LANE_END is found.
///
/// Note that the originator does NOT include those transition
/// config, so only shift config are included. This guarantees
/// that we won't end at those intermediate config that are
/// labeled as "LANE_END" by other lanes, since such intermediate
/// config are always transition configs and are not included as
/// originator by this lane.
void
LaneTracing::trace_back(Configuration& c, LaneHead& lh_list) noexcept(false)
{
    c.LANE_CON = 1; // set as config on conflicting lane.

    if (c.LANE_END == 1u) {
        if constexpr (DEBUG_PHASE_2) {
            std::cout << "END config FOUND: " << c.owner->state_no << "."
                      << c.ruleID << std::endl
                      << "\n";
            std::cout << "=ADD another head state: " << c.owner->state_no
                      << std::endl;
        }

        if constexpr (DEBUG_PHASE_2_GET_TBL) {
            std::cout << "D: END of config lane FOUND: " << c.owner->state_no
                      << "." << c.ruleID << " " << std::endl;
        }

        if (this->options.use_lr_k) { // for LR(k) use only.
            if constexpr (DEBUG_PHASE_2_GET_TBL) {
                std::cout << "config_red_config: "
                          << cur_red_config->owner->state_no << "."
                          << cur_red_config->ruleID
                          << ", LANE_END: " << c.owner->state_no << "."
                          << c.ruleID << std::endl;
            }
            // Don't use goal production. As it is the augmented rule, and
            // it generates no context at all.
            if (!(c.owner->state_no == 0 && c.ruleID == 0))
                lane_head_tail_pairs.insert(cur_red_config, &c);
        }

        add_lane_head_list(lh_list, c.owner);
        return;
    }

    for (Configuration* o_ptr : c.originators.list) {
        Configuration& o = *o_ptr;
        this->set_transitors_pass_thru_on(c, o); // set PASS_THRU ON.
        if (o.LANE_CON == 0u) {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "config on lane: " << o.owner->state_no << "."
                          << o.ruleID << std::endl;
            }
            if (c.owner != o.owner) {
                c.owner->PASS_THRU = 1;
                if constexpr (DEBUG_PHASE_2) {
                    std::cout << "set state " << c.owner->state_no
                              << " PASS_THRU ON" << std::endl;
                }
            }

            this->trace_back(o, lh_list);
        } else {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "already traced: " << o.owner->state_no << "."
                          << o.ruleID << std::endl;
            }
        }
    }
}

/// For use by LR(k) only.
/// Purpose: get LANE_END configurations and add to
/// lane_head_tail_pairs list.
void
LaneTracing::trace_back_lrk(Configuration* c)
{
    c->LANE_CON = 1; // set as config on conflicting lane.

    if (c->LANE_END == 1u) {
        if constexpr (DEBUG_PHASE_2) {
            std::cout << "END config FOUND: " << c->owner->state_no << "."
                      << c->ruleID << std::endl
                      << "\n";
            std::cout << "=ADD another head state: " << c->owner->state_no
                      << std::endl;
        }

        if constexpr (DEBUG_PHASE_2_GET_TBL) {
            std::cout << "config_red_config: "
                      << this->cur_red_config->owner->state_no << "."
                      << this->cur_red_config->ruleID
                      << ", LANE_END: " << c->owner->state_no << "."
                      << c->ruleID << std::endl;
        }
        // Don't use goal production. As it is the augmented rule, and
        // it generates no context at all.
        if (!(c->owner->state_no == 0 && c->ruleID == 0))
            this->lane_head_tail_pairs.insert(this->cur_red_config, c);

        return;
    }

    for (Configuration* o : c->originators.list) {
        if (o->LANE_CON == 0u) {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "config on lane: " << o->owner->state_no << "."
                          << o->ruleID << std::endl;
            }

            this->trace_back_lrk(o);
        } else {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "already traced: " << o->owner->state_no << "."
                          << o->ruleID << std::endl;
            }
        }
    }
}

/// For use by LR(k) only.
/// Purpose: clear the LANE_CON flag so as not to
///          interfere with the lane-tracing of other
///          LANE_END configurations.
void
trace_back_lrk_clear(Configuration& c)
{
    c.LANE_CON = 0; // set as config on conflicting lane.

    if (c.LANE_END == 1u) {
        return;
    }

    for (Configuration* o : c.originators.list) {
        if (o->LANE_CON == 1u) {
            trace_back_lrk_clear(*o);
        }
    }
}

/// Get those states from which conflicting lanes start from, and
/// the associated conflicting contexts for those states.
///
/// Do this by tracing back each final config from this state.
void
LaneTracing::get_state_conflict_lane_head(const StateHandle state_no,
                                          LaneHead& lh_list) noexcept(false)
{
    std::shared_ptr<const State> s =
      this->new_states.states_new_array[state_no].state;
    for (const auto& con : s->config) {
        if (is_final_configuration(this->grammar, con)) {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << std::endl
                          << "final config: " << state_no << '.' << con->ruleID
                          << std::endl;
            }

            if constexpr (DEBUG_PHASE_2_GET_TBL) {
                std::cout << std::endl
                          << "A: final config: " << state_no << '.'
                          << con->ruleID << std::endl;
            }
            cur_red_config = con;
            this->trace_back(*con, lh_list);
        }
    }
}

/// Get those states from which conflicting lanes start from,
/// and their associated conflicting contexts.
auto
LaneTracing::get_conflict_lane_head() noexcept(false) -> LaneHead
{
    LaneHead lane_head_list{};

    for (const StateHandle state_no : states_inadequate->states) {
        if (state_no >= 0) {
            if constexpr (DEBUG_GET_LANEHEAD) {
                std::cout << "inadequate state: " << state_no << ". ";
                const Conflict* c =
                  this->new_states.states_new_array[state_no].conflict.get();
                if (c != nullptr) {
                    std::cout << "conflicting contexts: "
                              << c->lookahead->symbol;

                    for (c = c->next.get(); c != nullptr; c = c->next.get()) {
                        std::cout << ", " << c->lookahead->symbol;
                    }
                }
            }

            if (this->new_states.states_new_array[state_no].rr_count > 0) {
                if constexpr (DEBUG_GET_LANEHEAD) {
                    std::cout
                      << " ["
                      << this->new_states.states_new_array[state_no].rr_count
                      << " r/r conflicts]";
                }
                this->get_state_conflict_lane_head(state_no, lane_head_list);
            }

            if constexpr (DEBUG_GET_LANEHEAD) {
                std::cout << std::endl;
            }
        }
    }

    if constexpr (DEBUG_PHASE_2) {
        std::cout << "dumpLaneHeadList: " << std::endl;
        for (const auto& lane_head : lane_head_list) {
            std::cout << lane_head.s->state_no << " " << std::endl;
        }
    }

    remove_pass_through_states(lane_head_list);
    return lane_head_list;
}

void
LaneTracing::phase2()
{
    lane_head_tail_pairs.clear(); // for LR(k) use only.
    LT_tbl = nullptr;             // initialize the LT_tbl.

    if constexpr (DEBUG_PHASE_2) {
        std::cout << "phase 2. unresolved inadequate states: "
                  << states_inadequate->count_unresolved << std::endl;
    }

    LaneHead lane_head_list = this->get_conflict_lane_head();
    if (lane_head_list.empty()) {
        std::cout << "lane_head_list is empty. return" << std::endl;
        return;
    }

    if constexpr (DEBUG_PHASE_2_GET_TBL) {
        const auto* lane_tracing_table = this->LT_tbl.get();
        if (lane_tracing_table != nullptr) {
            std::cout << "FROM \t| CONFIG:{CONTEXT} | TO" << std::endl;
            for (const LtTblEntry* e = lane_tracing_table; e != nullptr;
                 e = e->next.get()) {
                if (e != nullptr) {
                    std::cout << e->from_state << " \t| ";
                    // dump_config_context.
                    e->ctxt_set.dump();
                    std::cout << "\t| ";
                    e->to_states.dump();
                    std::cout << std::endl;
                }
            }
        }
    }

    if constexpr (DEBUG_PHASE_2_REGENERATE) {
        std::cout << "Now do regeneration" << std::endl;
    }

    if (!this->options.use_combine_compatible_states) {
        this->phase2_regeneration2(); // using all_clusters.
    } else {
        this->phase2_regeneration(lane_head_list);
    }
}

auto
LaneTracing::lane_tracing() -> std::optional<LRkPTArray>
{
    std::optional<LRkPTArray> lrk_pt_array = std::nullopt;
    this->in_edge_pushing_lane_tracing = false;
    MAX_K = 1; // max K used in LR(k).

    this->phase1();
    this->resolve_lalr1_conflicts();

    if (this->options.use_lane_tracing &&
        states_inadequate->count_unresolved > 0) {
        this->phase2();
        this->resolve_lalr1_conflicts();
        this->output_parsing_table_lalr();

        if (this->options.use_lr_k && new_states.conflicts_count.rr > 0) {
            // do LR(k) if there are still r/r conflicts.
            lrk_pt_array = this->lane_tracing_lrk();
        }
    } else {
        this->output_parsing_table_lalr(); // is this needed?
    }

    if (this->grammar_ambiguous) {
        std::cout << "Grammar is ambiguous" << std::endl;
    }
    return lrk_pt_array;
}

/////////////////////////////////////////////////////////////

/* Functions for lane tracing */

void
LaneTracing::dump_stacks() const
{
    std::cout << "__STACK__" << std::endl;
    this->stack.dump(grammar);
    std::cout << "__LANE__" << std::endl;
    this->lane.dump(grammar);
}

/// Does gamma have a non-null terminal descendent?
/// Input: n - gamma_theads.
///
/// A null terminal is "", which is an empty string.
auto
test_a(const SymbolList& n) -> bool
{
    for (const auto& elem : n) {
        if (!elem.snode->symbol->empty())
            return true;
    }
    return false;
}

/// Is the COMPLETE flag for c on?
constexpr inline auto
test_b(const Configuration& c) -> bool
{
    return c.COMPLETE == FLAG_ON;
}

/// Is the IN_LANE flag for c on?
constexpr inline auto
test_c(const Configuration& c) -> bool
{
    return c.IN_LANE == FLAG_ON;
}

/// Is the IN_LANE flag for c on?
/// Actually is the same as testC.
constexpr inline auto
test_d(const Configuration& c) -> bool
{
    return c.IN_LANE == FLAG_ON;
}

/// Insert snode to the list, no repetition allowed, increasing order.
/// Do it like insertion sort.
///
/// @parameters:
///  exist - label whether snode already existed.
static auto
insert_symbol_list_unique(SymbolList& list,
                          const std::shared_ptr<SymbolTableNode> snode) -> bool
{
    auto n = list.begin();
    for (; n != list.end(); n++) {
        if (n->snode == snode) {
            return true; // existing node.
        }
        if (n->snode->symbol > snode->symbol) {
            // insert new_snode before n.
            list.emplace(n, snode);
            return false;
        }
    } // end of for.

    // insert as the last node.
    list.emplace_back(snode);
    return false;
}

/// Assumption: list != nullptr, c != nullptr.
static void
combine_context_list(SymbolList& list, const SymbolList& new_list)
{

    if (new_list.empty())
        return;
    for (const auto& new_list_elem : new_list) {
        insert_symbol_list_unique(list, new_list_elem.snode);
    }
}

/// Copy from list to a new symbol list.
/// NOT including nodes that contain empty string.
static auto
get_contexts_generated(SymbolList& list) -> std::pair<bool, SymbolList>
{
    SymbolList sn{};
    bool null_possible = false;

    for (const auto& list_elem : list) {
        if (list_elem.snode->symbol->empty()) {
            null_possible = true;
        } else {
            insert_symbol_list_unique(sn, list_elem.snode);
        }
    }
    return { null_possible, sn };
}

void
LaneTracing::stack_operation(int* fail_ct, Configuration* o)
{
    std::optional<Configuration*> tmp = std::nullopt;

    (*fail_ct)++;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "---------------fail_ct = " << *fail_ct << std::endl;
    }

    switch (*fail_ct) {
        case 1:
            this->lane.push(o);
            o->IN_LANE = FLAG_ON;
            this->test_failed = true;
            break;
        case 2:
            tmp = this->lane.pop();
            this->lane.push(std::nullopt);
            this->lane.push(tmp);

            this->stack.push(std::nullopt);
            this->stack.push(o);
            break;
        default: // fail_ct >= 3
            this->stack.push(o);
            break;
    }

    if constexpr (DEBUG_PHASE_1) {
        this->dump_stacks();
    }
}

void
LaneTracing::move_markers(const Configuration* o) noexcept
{
    int r = 0;
    int ct = static_cast<int>(this->lane.count()) - 1;

    for (; ct >= 0; ct--) {
        const auto c = this->lane.array[ct];
        if (c == o)
            break;

        if (!c.has_value()) {
            this->lane.array[ct] = nullptr;
            r++;
        }
    }

    if (!this->test_failed) {
        for (; r > 0; r--)
            this->lane.push(std::nullopt);
    } else {
        auto c = this->lane.pop();
        for (; r > 0; r--)
            this->lane.push(std::nullopt);
        this->lane.push(c);
    }
}

/// For each symbol s in CONTEXT_GENERATED, do this:
///   remove s from CONTEXT_GENERATED;
///   for each config c in the this->lane stack top-down
///     if (s is in c) { break; }
///     else { add s to c; }
///
/// NOTE: here it accesses the this->lane stack internal member
///       array directly.
void
LaneTracing::context_adding(SymbolList context_generated,
                            const size_t cur_config_index) const
{
    if constexpr (DEBUG_PHASE_1) {
        std::cout << "CONTEXT ADDING ROUTINE: " << std::endl;
        write_symbol_list(context_generated, "CONTEXT_GENERATED");
    }

    while (!context_generated.empty()) {
        auto it = this->lane.array.rbegin();
        for (size_t i = 0; i < (this->lane.array.size() - 1 - cur_config_index);
             i++) {
            it++;
        }
        for (; it != this->lane.array.rend(); it++) {
            auto c = *it;
            if (c.has_value() && c.value() != nullptr) {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "add context to " << (*c)->owner->state_no
                              << "." << (*c)->ruleID << std::endl;
                }
                bool exist = insert_symbol_list_unique(
                  (*c)->context->context, context_generated.front().snode);
                if (exist)
                    break;
                // else, NOT exist, insert was sucessful.
            }
        }
        context_generated.pop_front();
    }
}

void
LaneTracing::context_adding_routine(const SymbolList context_generated,
                                    Configuration* o,
                                    const size_t cur_config_index,
                                    int* fail_ct)
{
    this->context_adding(context_generated, cur_config_index);

    if (this->trace_further) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "TRACE_FURTHER is ON" << std::endl;
        }

        this->trace_further = false;
        this->stack_operation(fail_ct, o);
    }

    if constexpr (DEBUG_PHASE_1) {
        if (!this->trace_further) {
            std::cout << "TRACE_FURTHER is OFF" << std::endl;
        }
    }
}

/// Do lane_tracing Phase 1 on a configuration.
/// Pre-assumption: c is a reduction.
void
LaneTracing::lane_tracing_reduction(Configuration* c) noexcept(false)
{
    if (c == nullptr)
        return;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "work on reduce config: ";
        stdout_write_config(this->grammar, c);
    }

    if (c->COMPLETE == 1u) {
        if constexpr (DEBUG_EDGE_PUSHING) {
            std::cout << "c->COMPLETE == 1" << std::endl;
        }
        return; // already evaluated.
    }

    this->lane = Stack();
    this->stack = Stack();

    this->lane.push(c);
    c->IN_LANE = FLAG_ON;
    this->trace_further = false;
    this->test_failed = false;

    if constexpr (DEBUG_EDGE_PUSHING) {
        std::cout << "DO_LOOP:" << std::endl;
    }

    this->do_loop();
}

/// Used if `DEBUG_PHASE_1` is `true`.
[[maybe_unused]] static void
my_show_t_heads(
  const SymbolList& alpha, // NOLINT(bugprone-easily-swappable-parameters)
  const SymbolList& theads)
{
    std::cout << "string '";

    bool first = true;
    for (const auto& a : alpha) {
        if (first)
            first = false;
        else
            std::cout << ' ';
        std::cout << a.snode->symbol;
    }

    std::cout << "' has theads: ";

    first = true;
    for (const auto& a : theads) {
        if (first)
            first = false;
        else
            std::cout << ", ";
        std::cout << a.snode->symbol;
    }
    std::cout << std::endl;
}

/// Returns true is one of c's originators is o.
static auto
is_on_transitor_chain(const Configuration& c, const Configuration* o) -> bool
{
    for (const Configuration* orig : c.originators.list) {
        if (orig == o)
            return true;
    }
    return false;
}

/// o - the originator. o does not change in the call stack.
void
LaneTracing::set_transitors_pass_thru_on(const Configuration& cur_config,
                                         const Configuration& o) noexcept(false)
{
    // find the next transitor for originator o.
    for (Configuration* c : cur_config.transitors.list) {
        get_originators(this->grammar, c, *c);

        if (is_on_transitor_chain(*c, &o)) {

            if (!this->options.use_combine_compatible_states) {
                if constexpr (DEBUG_PHASE_2_GET_TBL) {
                    std::cout << "B: next entry in entry_table: ("
                              << c->owner->state_no << "." << c->ruleID << ", "
                              << cur_config.owner->state_no << "."
                              << cur_config.ruleID << ")" << std::endl;
                }
                // add another entry to LT_tbl.
                this->lt_tbl_entry_add(c->owner->state_no, cur_config.owner);
            }

            if (c->owner != cur_config.owner && c->owner != o.owner) {
                // use this criteria because: cur_config does not need to be
                // handled here since it's already handled in trace_back().
                // you also don't want to set o->owner to be PASS_THRU, since
                // it may be the end state.
                if constexpr (DEBUG_PHASE_2_GET_TBL) {
                    std::cout << "==state " << c->owner->state_no
                              << ": set pass_thru ON. 2." << std::endl;
                }
                c->owner->PASS_THRU = 1;
            }
            this->set_transitors_pass_thru_on(*c, o);
        } // end is_on_transitor_chain.
    }

    if (o.owner == cur_config.owner) {
        this->get_the_context(&o);
    }
}

///!!!!!!!!!!!!!!!!!!!!!!!!!!! START.

/// let s = c->owner, find transitors for c in parent states of s.
static void
get_transitors(const Grammar& grammar, Configuration* c0, Configuration& c)
{
    const auto& l = c.owner->parents_list;
    if (l == nullptr) {
        std::cout << "Error: get_transitors() - L is nullptr" << std::endl;
        return;
    }
    if (l->state_list.size() == 0)
        return;

    if constexpr (DEBUG_GET_ORIGINATOR) {
        std::cout << "get transitor for " << c.owner->state_no << "."
                  << c.ruleID << "." << c.marker << std::endl;
    }

    for (const auto& p : l->state_list) {
        // now get transitor for c.
        for (const auto& t_ptr : p->config) {
            Configuration& t = *t_ptr;
            if (t.ruleID == c.ruleID && t.marker == c.marker - 1) {
                // is a transitor.
                if constexpr (DEBUG_GET_ORIGINATOR) {
                    std::cout << "++ ++ transitor found for ["
                              << c.owner->state_no << "." << c.ruleID << "."
                              << c.marker << "]: [" << p->state_no << "."
                              << t.ruleID << "." << t.marker << "]"
                              << std::endl;
                }
                insert_transitor_list(c, &t);
                get_originators(grammar, c0, t);
            }
        }
    }
}

/// Get originators and transitors for c.
///
/// @input:
///   - c0 : the original config to find originators for,
///   - c  : the current config on path.
///
/// Transitor of c: a config d in parent state,
///   c.ruleID = d.ruleID, c.marker = d.marker + 1
/// Originator: a config d in current state,
///   d.scanned_symbol = c.LHS_symbol
void
get_originators(const Grammar& grammar, Configuration* c0, Configuration& c)
{
    if constexpr (DEBUG_GET_ORIGINATOR) {
        std::cout << "-- current config: " << c.owner->state_no << "."
                  << c.ruleID << "." << c.marker << std::endl;
    }

    if (c.isCoreConfig == 1u) { // core config, search parent states.
        get_transitors(grammar, c0, c);
    } else { // not core config. find originators in current state.
        for (const auto& d : c.owner->config) {
            if (&c == d) {
                continue;
            } // ignore c.
            if (d->nMarker.empty()) {
                continue;
            }
            if (d->nMarker.front().snode ==
                grammar.rules[c.ruleID]->nLHS->snode) {
                if constexpr (DEBUG_GET_ORIGINATOR) {
                    std::cout << "-- -- originator found for ["
                              << c0->owner->state_no << "." << c0->ruleID << "."
                              << c0->marker << "]: [" << d->owner->state_no
                              << "." << d->ruleID << "." << d->marker << "]"
                              << std::endl;
                }
                insert_originator_list(*c0, d, 1);
            }
        }
    }
}

///!!!!!!!!!!!!!!!!!!!!!!!!!!! END.

void
LaneTracing::do_loop() noexcept(false)
{
    // CAREFUL : whether this is nullopt or not does **not** depend on whether
    // this->stack is empty !
    auto cur_config_opt = this->lane.top();
    const size_t cur_config_index = this->lane.count() - 1;

    if (!cur_config_opt.has_value() ||
        cur_config_opt.value() == nullptr) { // should never happen.
        throw std::runtime_error("do_loop cur_config error");
    }
    Configuration& cur_config = **cur_config_opt;

    if constexpr (DEBUG_GET_ORIGINATOR) {
        std::cout << "==call get_originators(cur_config) in do_loop()=="
                  << std::endl;
    }
    get_originators(this->grammar, &cur_config, cur_config);

    int fail_ct = 0;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "++++++++++TOP of this->lane is: ";
        stdout_write_config(this->grammar, &cur_config);
    }

    for (ConfigurationNode* o_ptr : cur_config.originators.list) {
        Configuration& o = *o_ptr;
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "________NEXT ORIGINATOR___________________"
                      << std::endl;
        }

        SymbolList contexts_generated{};

        const SymbolList& gamma = o.nMarker;

        if constexpr (DEBUG_PHASE_1) {
            stdout_write_config(this->grammar, &o);
            std::cout << "gamma: "
                      << (gamma.empty() ? "nullptr"
                                        : gamma.front().snode->symbol->c_str())
                      << std::endl;
        }

        SymbolList gamma_theads{};
        if (!gamma.empty()) { // if not nullptr, get theads.
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "gamma not nullptr, get theads." << std::endl;
            }
            gamma_theads = get_theads(this->grammar, gamma); // get Heads.
            if constexpr (DEBUG_PHASE_1) {
                my_show_t_heads(gamma, gamma_theads);
            }
        } else {
            // gamma is nullptr, check if this is goal production.
            // NOTE that there are only TWO goal production in all states.
            // one is in state 0, the other is in the state where the
            // goal production is a final production. The second case
            // won't be traced in lane-tracing at all.
            // if (o->owner->state_no == 0 && o->ruleID == 0) {
            if (is_goal(o)) {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "GOAL PRODUCTION - generate context: $end"
                              << std::endl;
                }
                gamma_theads.emplace_back(hash_tbl_find(STR_END));
            }
        }

        if constexpr (DEBUG_PHASE_1) {
            // Print those configurations where a conflicting lane starts.
            std::cout << "START " << (*this->lane.array[0])->owner->state_no
                      << "." << (*this->lane.array[0])->ruleID << ": "
                      << o.owner->state_no << "." << o.ruleID
                      << " generates contexts";
            write_symbol_list(gamma_theads, "");
        }

        if (test_a(gamma_theads)) {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "testA true, get CONTEXTS_GENERATED" << std::endl;
            }
            auto [null_possible, contexts] =
              get_contexts_generated(gamma_theads);
            contexts_generated = contexts;

            if (this->in_edge_pushing_lane_tracing) { /// 12-19-2008.
                this->edge_pushing_context_generated = contexts_generated;
            }

            if (null_possible) {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "null possible true" << std::endl;
                }
                if (test_b(o)) { // if (o->COMPLETE == FLAG_ON) {
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "COMPLETE ON" << std::endl;
                    }
                    combine_context_list(contexts_generated,
                                         o.context->context);
                } else {
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "COMPLETE OFF" << std::endl;
                    }

                    if (test_d(o)) { // if (o->IN_LANE == FLAG_ON) {
                        if constexpr (DEBUG_PHASE_1) {
                            std::cout << "IN_LANE ON" << std::endl;
                            std::cout << "GRAMMAR is AMBIGUOUS" << std::endl;
                        }
                        this->grammar_ambiguous = true;
                        /// exit(1); //////////////// exit prematurely.
                        this->move_markers(&o);
                    } else {
                        if constexpr (DEBUG_PHASE_1) {
                            std::cout << "IN_LANE OFF. set TRACE_FURTHER ON"
                                      << std::endl;
                        }

                        this->trace_further = true;
                    }
                }
            } else {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "possible null is: false" << std::endl;
                }
                o.LANE_END = 1; // set LANE_END to be true.

                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "Found lane_end: " << o.owner->state_no << "."
                              << o.ruleID << std::endl;
                    std::cout << "conflict config: "
                              << (*this->lane.array.at(0))->owner->state_no
                              << "." << (*this->lane.array.at(0))->ruleID
                              << ", lane head state " << o.owner->state_no
                              << ", contexts: ";
                    write_symbol_list(gamma_theads, "contexts");
                }
                // if is in edge_pushing, ignore the context adding routine.
                if (this->in_edge_pushing_lane_tracing) {
                    continue;
                }
            }
            // CONTEXT adding routine.
            this->context_adding_routine(
              contexts_generated, &o, cur_config_index, &fail_ct);

        } else {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "testA false" << std::endl;
            }

            if (test_b(o)) { // testB
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "testB true" << std::endl;
                }

                combine_context_list(contexts_generated, o.context->context);
                this->context_adding_routine(
                  contexts_generated, &o, cur_config_index, &fail_ct);
            } else {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "testB false" << std::endl;
                }

                if (test_c(o)) { // test_c
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "test_c true" << std::endl;
                    }

                    this->move_markers(&o);

                    combine_context_list(contexts_generated,
                                         o.context->context);
                    this->context_adding_routine(
                      contexts_generated, &o, cur_config_index, &fail_ct);
                } else {
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "test_c false" << std::endl;
                    }

                    this->stack_operation(&fail_ct, &o);
                }
            }
        } // end of else (test_a false).

    } // end of for.

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "________END OF DO_LOOP____________________" << std::endl;
    }

    if (this->test_failed) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "TEST_FAILED is ON__" << std::endl;
        }

        this->test_failed = false;
        this->do_loop();
    } else {
        this->check_lane_top();
    }
}

void
LaneTracing::pop_lane()
{
    if constexpr (DEBUG_PHASE_1) {
        std::cout << "POP_LANE" << std::endl;
    }

    this->lane.pop();
    this->check_lane_top();
}

void
LaneTracing::check_stack_top()
{
    // CAREFUL : whether this is nullopt or not does **not** depend on whether
    // this->stack is empty !
    auto top_opt = this->stack.top();

    if (!top_opt.has_value()) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "__check_stack_top true" << std::endl;
        }

        this->stack.pop();
        this->pop_lane();

    } else {
        Configuration* top = *top_opt;
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "__check_stack_top false" << std::endl;
        }

        if (top->COMPLETE == FLAG_ON) {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "__top COMPLETE ON" << std::endl;
            }

            this->stack.pop();
            this->check_stack_top();
        } else {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "__top COMPLETE OFF" << std::endl;
            }

            this->stack.pop();
            this->lane.push(top);
            top->IN_LANE = FLAG_ON;
            this->do_loop();
        }
    }
}

static void
propogate_context_sets(const Grammar& grammar, Configuration* c)
{
    if (c == nullptr || c->COMPLETE == 0u || c->isCoreConfig == 1u)
        return;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "===config " << c->owner->state_no << "." << c->ruleID
                  << " heuristic propogate context sets===" << std::endl;
        stdout_write_config(grammar, c);
    }

    std::shared_ptr<const State> s = c->owner;
    for (const auto& config : s->config) {
        Configuration* f = config;
        if (f == c)
            continue;
        if (grammar.rules[f->ruleID]->nLHS->snode !=
            grammar.rules[c->ruleID]->nLHS->snode)
            continue;
        if (f->marker > 0)
            continue;
        if (f->COMPLETE == 1u)
            continue; // x-transition of conflict states.

        // otherwise, heuristically propagate context sets.
        free_context(f->context);
        f->context = c->context;
        f->COMPLETE = 1;
    }
}

void
LaneTracing::check_lane_top()
{
    // CAREFUL : whether this is nullopt or not does **not** depend on whether
    // this->stack is empty !
    auto lane_top_opt = this->lane.top();

    if (!lane_top_opt.has_value()) {
        throw std::runtime_error("[In LaneTracing::check_lane_top] the lane "
                                 "top should have a value !");
    }
    Configuration* lane_top = *lane_top_opt;

    if (lane_top == nullptr) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "check lane top true" << std::endl;
        }

        this->check_stack_top();
    } else {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "check lane top false" << std::endl;
        }

        if (lane_top == nullptr) {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "lane top is ZERO." << std::endl;
            }

            this->pop_lane();
        } else {
            lane_top->IN_LANE = FLAG_OFF;
            lane_top->COMPLETE = FLAG_ON;

            /// 12-19-2008.
            if (!this->in_edge_pushing_lane_tracing) {
                propogate_context_sets(this->grammar, lane_top);
            }
            if (this->lane.count() == 1) { // the starting reduction
                /////////// END PROGRAMING ///////////////
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "=====REDUCTION LANE TRACING ENDS====="
                              << std::endl;
                }
            } else {
                this->pop_lane();
            }
        }
    }
}
