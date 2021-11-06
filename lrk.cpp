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

static void
print_int(void* object)
{
    std::cout << *static_cast<int*>(object) << std::endl;
}
static void
print_string(void* object)
{
    std::cout << static_cast<const char*>(object) << std::endl;
}
void
print_symbol_list(void* object)
{
    auto* n = static_cast<SymbolNode*>(object);
    write_symbol_list(n, "");
}
static void
print_cfg_ctxt(void* object)
{
    if (object == nullptr)
        return;

    auto* cc = static_cast<CfgCtxt*>(object);
    auto* c = cc->c;
    auto* n = cc->ctxt;

    if (c == nullptr) {
        std::cout << "c is nullptr" << std::endl;
        return;
    }
    if (n == nullptr) {
        std::cout << "n is nullptr" << std::endl;
        return;
    }

    std::cout << c->owner->state_no << "." << c->ruleID << ": ";
    write_symbol_list(n, "");
}

static void
test_int()
{
    Set* s = nullptr;
    int i = 1, j = 2, k = 3;

    std::cout << "test_int" << std::endl;
    s = set_insert(s, (void*)&i);

    set_dump(s, &print_int);
    s = set_insert(s, (void*)&j);
    s = set_insert(s, (void*)&k);
    s = set_delete(s, (void*)&i);
    s = set_delete(s, (void*)&j);
    set_dump(s, &print_int);
}

static void
test_str()
{
    Set* s = nullptr;
    const char* s1 = "s1";
    const char* s2 = "s2";
    const char* s3 = "s3";
    std::cout << "test_str" << std::endl;

    s = set_insert(s, (void*)s1);
    s = set_insert(s, (void*)s2);
    s = set_insert(s, (void*)s3);
    s = set_delete(s, (void*)s3);
    set_dump(s, &print_string);
}

static void
test_lrk_theads(const Grammar& grammar)
{
    std::cout << "test_lrk_theads(), on G_thead." << std::endl;
    SymbolList alpha = SymbolNode::create(hash_tbl_find("X"));
    SymbolNode* t = alpha;
    t->next = SymbolNode::create(hash_tbl_find("Y"));
    t = t->next;
    t->next = SymbolNode::create(hash_tbl_find("Z"));
    t = t->next;
    t->next = SymbolNode::create(hash_tbl_find("U"));
    t = t->next;

    std::shared_ptr<List> th = lrk_theads(grammar, alpha, 2);
    th->dump(&print_symbol_list);
    exit(0);
}

static void
dump_conflict_list(const Conflict* c)
{
    for (; c != nullptr; c = c->next.get()) {
        std::cout << "state " << c->state << ", token " << c->lookahead->symbol
                  << ", actions: [" << c->r << ", " << c->s
                  << "], decision: " << c->decision << std::endl;
    }
}

// return the length of the symbollist.
auto
get_lrk_theads_len(SymbolList s) -> int
{
    int len = 0;
    for (SymbolNode* t = s; t != nullptr; t = t->next) {
        len++;
    }
    return len;
}

// return the last symbol node.
auto
get_last_symbol(SymbolList s) -> SymbolNode*
{
    SymbolNode* t = nullptr;
    if (nullptr == s)
        return nullptr;
    for (t = s; t->next != nullptr; t = t->next) {
    }
    return t;
}

static auto
get_start_config_from_tail(Configuration* c) -> Configuration*
{
    ConfigPairNode* n = ConfigPairNode::list_find(lane_head_tail_pairs, c);
    if (nullptr == n)
        return nullptr;
    return n->start;
}

//
// If configuration cc->c is not in set, insert cc to set.
// Otherwise, compare cc->ctxt symbol list and add in new symbols
// to the object in set.
//
static auto
insert_cfg_ctxt_to_set(CfgCtxt* cc, Set* st) -> Set*
{
    if (st == nullptr) {
        st = set_insert(st, (void*)cc);
        return st;
    }

    for (ObjectItem* o = st; o != nullptr; o = o->next) {
        auto* s = static_cast<CfgCtxt*>(o->object);
        if (s->c == cc->c) {
            s->ctxt = combine_inc_symbol_list(s->ctxt, cc->ctxt);
            return st;
        }
    }

    // now o is nullptr
    st = set_insert(st, (void*)cc);
    return st;
}

/// Note that dummy is set_c2. Use it as global variable instead of passed in
/// to overcome the lack of head pointer problem.
///
/// Insert c->ruleID to entry col at LR(MAX_K) table row (state_no, token).
/// for each token in token_list (context list).
/// @param lrk_pt_array LR(k) parsing table array.
static auto
insert_lrk_pt(LRkPTArray& lrk_pt_array,
              int state_no,
              SymbolNode* token_list,
              std::shared_ptr<SymbolTableNode> col,
              Configuration* c,
              Configuration* c_tail,
              Set* set_c2) -> Set*
{
    LRkPT* pt = lrk_pt_array.get(MAX_K); // LR(k) parsing table where k = MAX_K.
    bool exist = false;

    // create LR(MAX_K) parsing table if it does not exist yet.
    if (nullptr == pt) {
        pt = LRkPT::create(MAX_K);
        lrk_pt_array.add(pt);
    }

    for (SymbolNode* token = token_list; token != nullptr;
         token = token->next) {
        ConfigPairNode* c0 = pt->get_entry(state_no, token->snode, col, &exist);
        if (exist == false || c0 == nullptr) { // no conflict.
            pt->add_reduction(state_no, token->snode, col, c, c_tail);
        } else { // exist is true, and c0 != nullptr.
            CfgCtxt* cc = CfgCtxt::create(c, SymbolNode::create(col), c_tail);
            set_c2 = insert_cfg_ctxt_to_set(cc, set_c2);
            if (reinterpret_cast<uintptr_t>(c0->end) == CONST_CONFLICT_SYMBOL) {
                // do nothing.
            } else {
                cc =
                  CfgCtxt::create(c0->start, SymbolNode::create(col), c0->end);
                set_c2 = insert_cfg_ctxt_to_set(cc, set_c2);
                // set this entry to CONST_CONFLICT_SYMBOL.
                pt->add_reduction(state_no, token->snode, col, c, c_tail);
            }
        }
    }

    return set_c2;
}

void
LaneTracing::lrk_config_lane_tracing(Configuration* c) noexcept
{
    EDGE_PUSHING_CONTEXT_GENERATED = nullptr;
    IN_EDGE_PUSHING_LANE_TRACING = true;
    this->lane_tracing_reduction(c);

    // pretend that c is a reduce configuration.
    c->LANE_END = 0;
    cur_red_config = c;

    // get LANE_END configs and add to lane_head_tail_pairs list.
    trace_back_lrk(c);

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
    SymbolNode* ret_list = nullptr;

    if (nullptr == c)
        return ret_list;

    std::shared_ptr<Conflict> n =
      states_new_array.conflict_list[c->owner->state_no];
    for (; n != nullptr; n = n->next) {
        if (n->r < 0 && n->s < 0) { // is r/r conflict.
            for (SymbolList contxt = c->context->nContext; contxt != nullptr;
                 contxt = contxt->next) {
                if (n->lookahead == contxt->snode) {
                    ret_list = insert_inc_symbol_list(ret_list, n->lookahead);
                }
            }
        }
    }

    std::cout << "conflict symbol for config " << c->owner->state_no << "."
              << c->ruleID;
    write_symbol_list(ret_list, "");
    return ret_list;
}

static auto
fill_set_c2(LRkPTArray& lrk_pt_array,
            ConfigPairNode* n,
            const Configuration* c,
            Configuration* c_tail,
            int k1,
            Set* set_c2,
            int state_no,
            CfgCtxt* cc) -> Set*
{
    // n->start is the next level lane head.
    n->start->z = c->z + k1 - 1;
    std::cout << "next level lane head: " << n->start->owner->state_no << "."
              << n->start->ruleID << ". z = " << c->z << " + " << k1
              << " - 1 = " << n->start->z << std::endl;
    SymbolNode* sn = EDGE_PUSHING_CONTEXT_GENERATED;
    for (; sn != nullptr; sn = sn->next) {
        set_c2 = insert_lrk_pt(lrk_pt_array,
                               state_no,
                               cc->ctxt,
                               sn->snode,
                               n->start,
                               c_tail,
                               set_c2);
    }

    return set_c2;
}

/*
 * @Input: inadequate state no.: state_no.
 */
void
LaneTracing::edge_pushing(LRkPTArray& lrk_pt_array, int state_no)
{
    CfgCtxt* cc = nullptr;
    std::shared_ptr<const State> s =
      this->new_states.states_new_array->state_list[state_no];
    if (s == nullptr)
        return;

    std::cout << "\nedge_pushing on state " << state_no << std::endl;

    Set* set_c = nullptr;
    Set* set_c2 = nullptr;
    int k = 1;
    // note, need to get conflict symbols for each final config also.

    for (const auto& i : s->config) {
        Configuration* c = i;
        if (is_final_configuration(this->grammar, c)) {
            // check if this final config's context contains conflict symbol,
            // if so add it to set_c, together with the conflict symbol(s).
            cc = CfgCtxt::create(get_start_config_from_tail(c),
                                 get_config_conflict_context(
                                   c, *this->new_states.states_new_array),
                                 c);
            c->z = 0;
            set_c = set_insert(set_c, (void*)cc);
        }
    }

    while (set_c != nullptr) {
        k++;
        MAX_K = k; // K for LR(K).
        std::cout << "while loop: k = " << k << std::endl;

        for (ObjectItem* si = set_c; si != nullptr; si = si->next) {
            cc = static_cast<CfgCtxt*>(si->object);
            Configuration* c = cc->c;
            if (nullptr == c) {
                std::cout << "? c is nullptr";
                continue;
            }
            auto* c_tail = cc->tail;

            int k1 = k - c->z; ///////////!!!!!!!!

            if (c->nMarker == nullptr) {
                std::cout << "Error: c->nMarker is nullptr. " << std::endl;
                continue;
            }
            std::shared_ptr<List> phi =
              lrk_theads(this->grammar, c->nMarker->next, k1);
            if (phi == nullptr) {
                // std::cout << "phi is nullptr. should not!" << std::endl;
                continue;
            }

            for (ObjectItem* x = phi->head; x != nullptr; x = x->next) {
                auto* x_str = static_cast<SymbolList>(x->object);
                int x_len = get_lrk_theads_len(x_str);
                if (x_len == k1) {
                    set_c2 = insert_lrk_pt(lrk_pt_array,
                                           state_no,
                                           cc->ctxt,
                                           get_last_symbol(x_str)->snode,
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
                            set_c2 = fill_set_c2(lrk_pt_array,
                                                 n,
                                                 c,
                                                 c_tail,
                                                 k1,
                                                 set_c2,
                                                 state_no,
                                                 cc);
                        }
                    } else {
                        ConfigPairNode* tmp =
                          lane_head_tail_pairs; // store the old list.
                        lane_head_tail_pairs = nullptr;
                        this->lrk_config_lane_tracing(c);
                        for (ConfigPairNode* n = lane_head_tail_pairs;
                             n != nullptr;
                             n = n->next) {
                            set_c2 = fill_set_c2(lrk_pt_array,
                                                 n,
                                                 c,
                                                 c_tail,
                                                 k1,
                                                 set_c2,
                                                 state_no,
                                                 cc);
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
        set_c2 = nullptr;
    }
}

/*
 * Remove r/r conflict nodes from list.
 */
static void
remove_rr_conflict_from_list(size_t state_no, NewStates& new_states)
{
    std::shared_ptr<Conflict> c_prev = nullptr;
    std::shared_ptr<Conflict>& c =
      new_states.states_new_array->conflict_list[state_no];
    while (nullptr != c) {
        if (c->r < 0 && c->s < 0) {  // remove this node.
            if (c_prev == nullptr) { // remove at head.
                new_states.states_new_array->conflict_list[state_no] = c->next;
                c = new_states.states_new_array->conflict_list[state_no];
            } else { // remove in the middle.
                c_prev->next = c->next;
                c = c_prev->next;
            }
            new_states.states_new_array->rr_count[state_no]--;
            new_states.conflicts_count.rr--;
            continue;
        }

        c = c->next;
    }

    // if list is empty, remove this conflict.
    if (new_states.states_new_array->conflict_list[state_no] == nullptr) {
        for (int& state : states_inadequate->states) {
            if (static_cast<int>(state_no) == state) {
                state = -1;
                states_inadequate->count_unresolved--;
            }
        }
    }
}

/*
 * Update LR(1) parsing table.
 * 1) set entry to -10000010.
 * 2) remove this conflict from conflict list.
 */
static void
update_lr1_parsing_table(size_t state_no, NewStates& new_states)
{
    for (std::shared_ptr<Conflict> c =
           new_states.states_new_array->conflict_list[state_no];
         c != nullptr;
         c = c->next) {
        update_action(get_col(*c->lookahead),
                      state_no,
                      static_cast<int>(CONST_CONFLICT_SYMBOL));
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

    for (const int state_no : states_inadequate->states) {
        int ct_rr = this->new_states.states_new_array->rr_count[state_no];

        if (state_no >= 0 && ct_rr > 0) {
            this->edge_pushing(lrk_pt_array, state_no);
            // update corresonding entry in LR(1) parsing table.
            update_lr1_parsing_table(state_no, new_states);
        }
    }
    return lrk_pt_array;
}
