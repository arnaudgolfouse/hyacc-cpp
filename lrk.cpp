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
 * lrk.c
 *
 * Functions for LR(k).
 *
 * @Author: Xin Chen
 * @Created on: 11/25/2008
 * @Last modified: 11/25/2008
 * @Copyright: 2008, 2009
 */

#include "lane_tracing.hpp"
#include "y.hpp"
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <optional>

/** For (conflict_config, lane_end_config) pairs. */
ConfigPairList lane_head_tail_pairs;

inline void
print_int(const int& object)
{
    std::cout << object << std::endl;
}
inline void
print_string(char const* const& object)
{
    std::cout << object << std::endl;
}
inline void
print_symbol_list(const SymbolList& object)
{
    write_symbol_list(object, "");
}

static auto
get_start_config_from_tail(Configuration* c) -> Configuration*
{
    ConfigPairNode* n = ConfigPairNode::list_find(lane_head_tail_pairs, c);
    if (nullptr == n)
        return nullptr;
    return n->start;
}

/// If configuration cc->c is not in set, insert cc to set.
/// Otherwise, compare cc->ctxt symbol list and add in new symbols
/// to the object in set.
static void
insert_cfg_ctxt_to_set(const std::shared_ptr<CfgCtxt> cc,
                       Set<std::shared_ptr<CfgCtxt>>& st)
{
    for (auto& s : st.inner) {
        if (s->c == cc->c) {
            combine_inc_symbol_list(s->ctxt, cc->ctxt);
        }
    }
    // now o is nullptr
    st.insert(cc);
}

/// Note that dummy is set_c2. Use it as global variable instead of passed in
/// to overcome the lack of head pointer problem.
///
/// Insert c->ruleID to entry col at LR(MAX_K) table row (state_no, token).
/// for each token in token_list (context list).
/// @param lrk_pt_array LR(k) parsing table array.
static void
insert_lrk_pt(LRkPTArray& lrk_pt_array,
              const StateHandle state_no,
              const SymbolList token_list,
              const std::shared_ptr<SymbolTableNode> col,
              Configuration* c,
              Configuration* c_tail,
              Set<std::shared_ptr<CfgCtxt>>& set_c2)
{
    LRkPT* pt = lrk_pt_array.get(MAX_K); // LR(k) parsing table where k = MAX_K.
    bool exist = false;

    // create LR(MAX_K) parsing table if it does not exist yet.
    if (nullptr == pt) {
        pt = LRkPT::create(MAX_K);
        lrk_pt_array.add(pt);
    }

    for (const SymbolNode& token : token_list) {
        ConfigPairNode* c0 = pt->get_entry(state_no, token.snode, col, &exist);
        if (exist == false || c0 == nullptr) { // no conflict.
            pt->add_reduction(state_no, token.snode, col, c, c_tail);
        } else { // exist is true, and c0 != nullptr.
            SymbolList list;
            list.emplace_back(col);
            auto cc = std::make_shared<CfgCtxt>(c, list, c_tail);
            insert_cfg_ctxt_to_set(cc, set_c2);
            if (reinterpret_cast<uintptr_t>(c0->end) == CONST_CONFLICT_SYMBOL) {
                // do nothing.
            } else {
                cc = std::make_shared<CfgCtxt>(c0->start, list, c0->end);
                insert_cfg_ctxt_to_set(cc, set_c2);
                // set this entry to CONST_CONFLICT_SYMBOL.
                pt->add_reduction(state_no, token.snode, col, c, c_tail);
            }
        }
    }
}

void
LaneTracing::lrk_config_lane_tracing(Configuration* c) noexcept
{
    this->edge_pushing_context_generated.clear();
    this->in_edge_pushing_lane_tracing = true;
    this->lane_tracing_reduction(c);

    // pretend that c is a reduce configuration.
    c->LANE_END = 0;
    this->cur_red_config = c;

    // get LANE_END configs and add to lane_head_tail_pairs list.
    this->trace_back_lrk(c);

    // clear the LANE_CON flag of configurations on conflicting lanes.
    trace_back_lrk_clear(c);

    c->LANE_END = 1; // recover the value of c->LANE_END.
    // note that the value of cur_red_config does not need recovery.
}

/*
 * @Return: the conflict symbol list of configuration c in INC order.
 */
static auto
get_config_conflict_context(Configuration* c,
                            const StateArray& states_new_array) -> SymbolList
{
    SymbolList ret_list{};

    if (c == nullptr)
        return ret_list;

    std::shared_ptr<const Conflict> n =
      states_new_array[c->owner->state_no].conflict;
    for (; n != nullptr; n = n->next) {
        if (n->r.is_reduce() && n->s.is_reduce()) { // is r/r conflict.
            for (const auto& contxt : c->context->context) {
                if (n->lookahead == contxt.snode) {
                    insert_inc_symbol_list(ret_list, n->lookahead);
                }
            }
        }
    }

    std::cout << "conflict symbol for config " << c->owner->state_no << "."
              << c->ruleID;
    write_symbol_list(ret_list, "");
    return ret_list;
}

static void
fill_set_c2(SymbolList edge_pushing_context_generated,
            LRkPTArray& lrk_pt_array,
            ConfigPairNode* n,
            const Configuration* c,
            Configuration* c_tail,
            const int k1,
            Set<std::shared_ptr<CfgCtxt>>& set_c2,
            const StateHandle state_no,
            const CfgCtxt* cc)
{
    // n->start is the next level lane head.
    n->start->z = c->z + k1 - 1;
    std::cout << "next level lane head: " << n->start->owner->state_no << "."
              << n->start->ruleID << ". z = " << c->z << " + " << k1
              << " - 1 = " << n->start->z << std::endl;
    for (const auto& sn : edge_pushing_context_generated) {
        insert_lrk_pt(
          lrk_pt_array, state_no, cc->ctxt, sn.snode, n->start, c_tail, set_c2);
    }
}

/*
 * @Input: inadequate state no.: state_no.
 */
void
LaneTracing::edge_pushing(LRkPTArray& lrk_pt_array, StateHandle state_no)
{
    std::shared_ptr<CfgCtxt> cc = nullptr;
    std::shared_ptr<const State> s =
      this->new_states.states_new_array[state_no].state;
    if (s == nullptr)
        return;

    std::cout << "\nedge_pushing on state " << state_no << std::endl;

    Set<std::shared_ptr<CfgCtxt>> set_c{};
    Set<std::shared_ptr<CfgCtxt>> set_c2{};
    int k = 1;
    // note, need to get conflict symbols for each final config also.

    for (const auto& i : s->config) {
        Configuration* c = i;
        if (is_final_configuration(this->grammar, c)) {
            // check if this final config's context contains conflict symbol,
            // if so add it to set_c, together with the conflict symbol(s).
            cc = std::make_shared<CfgCtxt>(
              get_start_config_from_tail(c),
              get_config_conflict_context(c, this->new_states.states_new_array),
              c);
            c->z = 0;
            set_c.insert(cc);
        }
    }

    while (!set_c.inner.empty()) {
        k++;
        MAX_K = k; // K for LR(K).
        std::cout << "while loop: k = " << k << std::endl;

        for (const auto& cc : set_c.inner) {
            Configuration* c = cc->c;
            if (nullptr == c) {
                std::cout << "? c is nullptr";
                continue;
            }
            auto* c_tail = cc->tail;

            int k1 = k - c->z; ///////////!!!!!!!!

            if (c->nMarker.empty()) {
                std::cout << "Error: c->nMarker is empty. " << std::endl;
                continue;
            }
            // TODO: lrk_theads takes c->nMarker.next...
            std::shared_ptr<List> phi =
              lrk_theads(this->grammar, c->nMarker, k1);
            if (phi == nullptr) {
                // std::cout << "phi is nullptr. should not!" << std::endl;
                continue;
            }

            for (auto& x_str : phi->inner) {
                int x_len = static_cast<int>(x_str.size());
                if (x_len == k1) {
                    insert_lrk_pt(lrk_pt_array,
                                  state_no,
                                  cc->ctxt,
                                  x_str.back().snode,
                                  c,
                                  c_tail,
                                  set_c2);
                } else if (x_len == k1 - 1) {
                    ConfigPairNode* n =
                      ConfigPairNode::list_find(lane_head_tail_pairs, c);
                    if (n != nullptr) { // found in cache.
                        std::cout << "found in cache" << std::endl;
                        // this list is in INC order of c.
                        for (; n != nullptr; n = n->next) {
                            if (c != n->end)
                                break;
                            fill_set_c2(this->edge_pushing_context_generated,
                                        lrk_pt_array,
                                        n,
                                        c,
                                        c_tail,
                                        k1,
                                        set_c2,
                                        state_no,
                                        cc.get());
                        }
                    } else {
                        ConfigPairNode* tmp =
                          lane_head_tail_pairs; // store the old list.
                        lane_head_tail_pairs = nullptr;
                        this->lrk_config_lane_tracing(c);
                        for (ConfigPairNode* n = lane_head_tail_pairs;
                             n != nullptr;
                             n = n->next) {
                            fill_set_c2(this->edge_pushing_context_generated,
                                        lrk_pt_array,
                                        n,
                                        c,
                                        c_tail,
                                        k1,
                                        set_c2,
                                        state_no,
                                        cc.get());
                        }
                        // now combine the two list.
                        if (nullptr == lane_head_tail_pairs) {
                            lane_head_tail_pairs = tmp;
                        } else {
                            lane_head_tail_pairs = ConfigPairNode::list_combine(
                              lane_head_tail_pairs, tmp);
                            ConfigPairNode::list_destroy(tmp);
                        }
                    }
                }
            }
        }

        set_c = set_c2;
        set_c2.inner.clear();
    }
}

/// Remove r/r conflict nodes from list.
static void
remove_rr_conflict_from_list(size_t state_no, NewStates& new_states)
{
    std::shared_ptr<Conflict> c_prev = nullptr;
    std::shared_ptr<Conflict>& c =
      new_states.states_new_array[state_no].conflict;
    while (nullptr != c) {
        if (c->r.is_reduce() && c->s.is_reduce()) { // remove this node.
            if (c_prev == nullptr) {                // remove at head.
                new_states.states_new_array[state_no].conflict = c->next;
                c = new_states.states_new_array[state_no].conflict;
            } else { // remove in the middle.
                c_prev->next = c->next;
                c = c_prev->next;
            }
            new_states.states_new_array[state_no].rr_count--;
            new_states.conflicts_count.rr--;
            continue;
        }

        c = c->next;
    }

    // if list is empty, remove this conflict.
    if (new_states.states_new_array[state_no].conflict == nullptr) {
        for (StateHandle& state : states_inadequate->states) {
            if (state_no == state) {
                state = -1;
                states_inadequate->count_unresolved--;
            }
        }
    }
}

/// Update LR(1) parsing table.
/// 1) set entry to -10000010.
/// 2) remove this conflict from conflict list.
static void
update_lr1_parsing_table(size_t state_no, NewStates& new_states)
{
    for (std::shared_ptr<Conflict> c =
           new_states.states_new_array[state_no].conflict;
         c != nullptr;
         c = c->next) {
        update_action(
          get_col(*c->lookahead), state_no, ParsingAction::new_error());
    }

    // remove r/r conflict node from list.
    remove_rr_conflict_from_list(state_no, new_states);
}

auto
LaneTracing::lane_tracing_lrk() -> std::optional<LRkPTArray>
{
    if (0 == states_inadequate->count_unresolved)
        return std::nullopt;
    MAX_K++; // increment K for LR(k).
    LRkPTArray lrk_pt_array{};

    std::cout << "\n------------- lane_tracing_LR_k ------------- "
              << "lane head/tail pairs:" << std::endl;
    ConfigPairNode::list_dump(lane_head_tail_pairs);

    for (const StateHandle state_no : states_inadequate->states) {
        size_t ct_rr = this->new_states.states_new_array[state_no].rr_count;

        if (state_no >= 0 && ct_rr > 0) {
            this->edge_pushing(lrk_pt_array, state_no);
            // update corresonding entry in LR(1) parsing table.
            update_lr1_parsing_table(state_no, new_states);
        }
    }
    return lrk_pt_array;
}
