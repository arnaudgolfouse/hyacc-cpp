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
#include <fstream>
#include <iostream>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>

constexpr bool DEBUG_PHASE_1 = false;
constexpr bool DEBUG_RESOLVE_CONFLICT = false;
constexpr bool DEBUG_PHASE_2 = false;
constexpr bool DEBUG_GET_LANEHEAD = false;
constexpr bool DEBUG_PHASE_2_GET_TBL = false;
constexpr bool DEBUG_PHASE_2_REGENERATE2 = false;
constexpr bool DEBUG_PHASE_2_REGENERATE = false;
constexpr bool DEBUG_GET_ORIGINATOR = false;

constexpr bool DEBUG_ORIGIN = false;

LRkPtEntry* LRk_PT = nullptr;

/*
 * Variables used for lane tracing. Start.
 */

static Stack* LANE;
static Stack* STACK;
static int TRACE_FURTHER;
static int TEST_FAILED;

/*
 * Current final config that is traced in trace_back() function.
 * Used for phase 2 state combining.
 */
Configuration* cur_red_config;
static bool GRAMMAR_AMBIGUOUS = false;

constexpr unsigned int FLAG_ON = 1u;
constexpr unsigned int FLAG_OFF = 0u;

static Configuration* LT_MARKER = (Configuration*)-1;
static Configuration* LT_ZERO = 0;

extern bool in_lanetracing;
bool IN_EDGE_PUSHING_LANE_TRACING;
SymbolList EDGE_PUSHING_CONTEXT_GENERATED;

/*
 * Variables used for lane tracing. End.
 */

static void
get_originators(const Grammar& grammar, Configuration* c0, Configuration* c);
static void
do_loop(const Grammar& grammar);
static void
check_lane_top(const Grammar& grammar);
static auto
find_successor_state_no(int state_no,
                        const StateArray& states_new_array,
                        const SymbolTblNode* snode) -> int;
static auto
inherit_parent_context(const Grammar& grammar, State* s, State* parent) -> bool;
static void
clear_state_context(State* s);

/*
 * Initialize this to nullptr at the beginning of phase2_regeneration2().
 * This list is in the order of cluster insertion.
 */
LtCluster* all_clusters;
LtCluster* all_clusters_tail;
LtCluster* new_cluster;
/*
 * Initialized to true. If in regeneration context conflicts occur,
 * set this to false, which means the grammar is NOT LR(1).
 */
bool all_pairwise_disjoint;

/*
 * Initialize this to nullptr at the beginning of lane_tracing_phase2().
 * This list is in INC order on from_state->state_no.
 */
LtTblEntry* LT_tbl;

/* declaration of functions */
static void
dump_lt_tbl();
static auto
lt_tbl_find_entry(int from_state) -> LtTblEntry*;
static auto
cluster_trace_new_chain_all(const Grammar& grammar,
                            const StateArray& states_new_array,
                            std::optional<Queue>& config_queue,
                            int parent_state,
                            const LtTblEntry* e) -> bool;

void
dump_llist_context_set(LlistContextSet* contxt_set)
{
    LlistContextSet* c = contxt_set;
    if (c == nullptr)
        return;

    for (; c != nullptr; c = c->next) {
        // first write the config.
        std::cout << c->config->owner->state_no << '.' << c->config->ruleID
                  << ':';
        // then write the context symbol list.
        std::cout << "{ ";
        for (SymbolNode* n = c->ctxt; n != nullptr; n = n->next) {
            std::cout << n->snode->symbol << " ";
        }
        std::cout << "} ";
    }
}

/*
 * Assumption: contxt_set is in INC order.
 */
auto
llist_context_set_create(Configuration* config)
  -> LlistContextSet* //, SymbolList contxt_set)
{
    auto* s = new LlistContextSet;

    s->config = config;
    s->ctxt = nullptr; // clone_symbol_list(contxt_set);
    s->next = nullptr;
    return s;
}

void
llist_context_set_destroy(LlistContextSet* s)
{
    free_symbol_node_list(s->ctxt);
    delete s;
}

/*
 * Add (merge) contxt_set to c->ctxt.
 */
void
llist_context_set_add_context(LlistContextSet* c, SymbolList contxt_set)
{
    if (nullptr == c)
        return;

    for (SymbolNode* n = contxt_set; n != nullptr; n = n->next) {
        bool exist = false;
        // if (strlen(n->snode->symbol) == 0) continue; // ignore empty string.
        c->ctxt = insert_symbol_list_unique_inc(c->ctxt, n->snode, &exist);
    }
}

/*
 * Return a clone of c.
 */
auto
llist_context_set_clone(LlistContextSet* c) -> LlistContextSet*
{

    if (nullptr == c)
        return nullptr;
    // std::cout << "clone c: "; dump_LlistContextSet(c); std::cout <<
    // std::endl;

    LlistContextSet* d = llist_context_set_create(c->config);
    LlistContextSet* d_next = d;
    d->ctxt = clone_symbol_list(c->ctxt);

    for (LlistContextSet* c_next = c->next; c_next != nullptr;
         c_next = c_next->next) {
        d_next->next = llist_context_set_create(c_next->config);
        d_next->next->ctxt = clone_symbol_list(c_next->ctxt);
        d_next = d_next->next;
    }

    // std::cout << "result d: "; dump_LlistContextSet(d); std::cout <<
    // std::endl;
    return d;
}

// LlistInt functions. START.

auto
llist_int_create(int n) -> LlistInt*
{
    auto* item = new LlistInt;
    item->n = n;
    item->next = nullptr;
    return item;
}

void
llist_int_destroy(LlistInt* list)
{
    while (list != nullptr) {
        LlistInt* tmp = list;
        list = list->next;
        delete tmp;
    }
}

/*
 * Add n to the list in INC order.
 */
auto
LlistInt::add_inc(int n) noexcept -> LlistInt*
{
    LlistInt* list = this;
    // else, list is not empty. insert in INC order.

    int cmp = 0;
    LlistInt *t = list, *t_prev = nullptr;
    for (; t != nullptr; t_prev = t, t = t->next) {
        cmp = t->n - n;
        if (cmp == 0)
            return list;             // exists already.
        if (cmp > 0) {               // insert before t.
            if (t_prev == nullptr) { // insert as the head.
                list = llist_int_create(n);
                list->next = t;
            } else { // insert in the middle
                t_prev->next = llist_int_create(n);
                t_prev->next->next = t;
            }
            return list;
        }
        // else, go on to the next state in list.
    }
    // now is at the end of the list. Add to list tail.
    t_prev->next = llist_int_create(n);

    return list;
}

/*
 * Add n to the head of list.
 * Return: pointer to the head of the list.
 */
auto
llist_int_add_head(LlistInt* list, int n) -> LlistInt*
{
    LlistInt* t = llist_int_create(n);
    t->next = list;
    return t;
}

void
LlistInt::dump() const noexcept
{
    for (const LlistInt* x = this; x != nullptr; x = x->next) {
        std::cout << x->n << ' ';
    }
}

// LlistInt functions. END.

// LlistInt2 functions. START.

auto
llist_int2_create(int n1, int n2) -> LlistInt2*
{
    auto* item = new LlistInt2;
    item->n1 = n1;
    item->n2 = n2;
    item->next = nullptr;
    return item;
}

void
llist_int2_destroy(LlistInt2* list)
{
    while (list != nullptr) {
        LlistInt2* tmp = list;
        list = list->next;
        delete tmp;
    }
}

/*
 * Add n1, n2 to the list in INC order of n1.
 */
auto
llist_int2_add_inc(LlistInt2* list, int n1, int n2) -> LlistInt2*
{
    if (list == nullptr) {
        return llist_int2_create(n1, n2);
    }
    // else, list is not empty. insert in INC order.

    int cmp = 0;
    LlistInt2 *t = list, *t_prev = nullptr;
    for (; t != nullptr; t_prev = t, t = t->next) {
        cmp = t->n1 - n1;
        if (cmp == 0)
            return list;             // exists already.
        if (cmp > 0) {               // insert before t.
            if (t_prev == nullptr) { // insert as the head.
                t_prev = list = llist_int2_create(n1, n2);
                list->next = t;
            } else { // insert in the middle
                t_prev->next = llist_int2_create(n1, n2);
                t_prev->next->next = t;
            }
            return list;
        }
        // else, go on to the next state in list.
    }
    // now is at the end of the list. Add to list tail.
    t_prev->next = llist_int2_create(n1, n2);

    return list;
}

/*
 * Add n to the head of list.
 * Return: pointer to teh head of the list.
 */
auto
llist_int2_add_head(LlistInt2* list, int n1, int n2) -> LlistInt2*
{
    LlistInt2* t = llist_int2_create(n1, n2);
    t->next = list;
    return t;
}

/*
 * Find the node in a LlistInt list whose first entry is n1.
 */
auto
llist_int2_find_n1(LlistInt2* list, int n1) -> LlistInt2*
{
    for (LlistInt2* t = list; t != nullptr; t = t->next) {
        if (t->n1 == n1)
            return t;
    }
    return nullptr;
}

auto
LlistInt2::find_n2(int n2) noexcept -> LlistInt2*
{
    for (LlistInt2* t = this; t != nullptr; t = t->next) {
        if (t->n2 == n2)
            return t;
    }
    return nullptr;
}

void
llist_int2_dump(LlistInt2* list)
{
    if (list == nullptr)
        return;

    for (LlistInt2* x = list; x != nullptr; x = x->next) {
        std::cout << '(' << x->n1 << ", " << x->n2 << ") ";
    }
}

// LlistInt2 functions, END.

static auto
lt_tbl_entry_create(State* from, State* to, const StateArray& states_new_array)
  -> LtTblEntry*
{
    auto* e = new LtTblEntry;
    e->from_state = from->state_no;

    if (e->from_state != states_new_array.state_list[e->from_state]->state_no) {
        throw std::runtime_error(
          "ERROR (lt_tbl_entry_create): state_no not equal");
    }

    e->processed = false;
    e->ctxt_set = nullptr;
    e->to_states = (nullptr == to) ? nullptr : llist_int_create(to->state_no);
    e->next = nullptr;
    return e;
}

/*
 * A LtTblEntry can have more than one to_state.
 * add to_state in increasing order of the state_no.
 */
void
lt_tbl_entry_add_to_state(LtTblEntry* e, State* to)
{
    if (to == nullptr || e == nullptr)
        return;
    e->to_states = e->to_states->add_inc(to->state_no);
}

/*
 * Add an entry (from_state, to_state) to the LT_tbl,
 * don't add the (config, context) information here.
 */
static void
lt_tbl_entry_add(State* from, State* to, const StateArray& states_new_array)
{
    if (LT_tbl == nullptr) {
        LT_tbl = lt_tbl_entry_create(from, to, states_new_array);
        return;
    }
    LtTblEntry *e = LT_tbl, *e_prev = nullptr;
    // search if the from state already exists.
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from->state_no) {
            lt_tbl_entry_add_to_state(e, to); // add to state if not on list.
            return;
        }
        if (e->from_state > from->state_no) { // insert before e.
            if (e_prev == nullptr) {          // insert as the head.
                LT_tbl = lt_tbl_entry_create(from, to, states_new_array);
                LT_tbl->next = e;
            } else { // insert between e_prev and e
                e_prev->next = lt_tbl_entry_create(from, to, states_new_array);
                e_prev->next->next = e;
            }
            return;
        }
    }
    // now is at the end of the table LT_tbl, add to list tail.
    e_prev->next = lt_tbl_entry_create(from, to, states_new_array);
    return;
}

/*
 * Find from state in the LT_tbl.
 * If not found, insert it.
 */
static auto
lt_tbl_entry_find_insert(State* from, const StateArray& states_new_array)
  -> LtTblEntry*
{
    if (nullptr == LT_tbl) { // insert as the first
        LT_tbl = lt_tbl_entry_create(from, nullptr, states_new_array);
        return LT_tbl;
    }

    LtTblEntry *e = LT_tbl, *e_prev = nullptr;
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from->state_no)
            return e;
        if (e->from_state > from->state_no) { // insert here.
            if (e_prev == nullptr) {          // insert as the first.
                LT_tbl = lt_tbl_entry_create(from, nullptr, states_new_array);
                LT_tbl->next = e;
                return LT_tbl;
            } // insert in the middle.
            e_prev->next = lt_tbl_entry_create(from, nullptr, states_new_array);
            e_prev->next->next = e;
            return e_prev->next;
        }
        // else, go on to check the next entry.
    }

    // otherwise, insert at the end.
    e_prev->next = lt_tbl_entry_create(from, nullptr, states_new_array);
    return e_prev->next;
}

/*
 * Find from state in the LT_tbl.
 * Same as LtTblEntry_find_insert() except that this has no insert.
 *
 * There can be at most one entry found.
 */
auto
lt_tbl_entry_find(State* from, const StateArray& states_new_array)
  -> LtTblEntry*
{
    if (nullptr == LT_tbl) { // insert as the first
        LT_tbl = lt_tbl_entry_create(from, nullptr, states_new_array);
        return LT_tbl;
    }

    LtTblEntry *e = LT_tbl, *e_prev = nullptr;
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from->state_no)
            return e;
        if (e->from_state > from->state_no) { // insert here.
            if (e_prev == nullptr) {          // insert as the first.
                LT_tbl = lt_tbl_entry_create(from, nullptr, states_new_array);
                LT_tbl->next = e;
                return LT_tbl;
            } // insert in the middle.
            e_prev->next = lt_tbl_entry_create(from, nullptr, states_new_array);
            e_prev->next->next = e;
            return e_prev->next;
        }
        // else, go on to check the next entry.
    }

    return nullptr;
}

/*
 * Find the cur_red_config in the LtTblEntry e.
 * If not found, then insert it.
 */
static auto
llist_context_set_get(LtTblEntry* e) -> LlistContextSet*
{

    if (e == nullptr) {
        throw std::runtime_error("llist_context_set_get error: e is nullptr");
    }

    if (e->ctxt_set == nullptr) { // empty, insert as the first one.
        e->ctxt_set = llist_context_set_create(cur_red_config);
        return e->ctxt_set;
    }

    LlistContextSet* c = e->ctxt_set;
    LlistContextSet* c_prev = nullptr;
    // else, try to find it in the list.
    for (; c != nullptr; c_prev = c, c = c->next) {
        if (c->config == cur_red_config)
            return c;                                     // found.
        if (c->config->ruleID > cur_red_config->ruleID) { // insert here
            if (c_prev == nullptr) {                      // insert as the first
                e->ctxt_set = llist_context_set_create(cur_red_config);
                e->ctxt_set->next = c;
                return e->ctxt_set;
            } // insert in the middle.
            c_prev->next = llist_context_set_create(cur_red_config);
            c_prev->next->next = c;
            return c_prev->next;
        }
        // else, go on to check the next.
    }
    // else, insert at the list tail.
    c_prev->next = llist_context_set_create(cur_red_config);
    return c_prev->next;
}

/*
 * Add the (from_state, config, context)
 * information to an entry in the LT_tbl.
 *
 * Note:
 * 1) The from_state is unique for each entry.
 * 2) The LtTblEntry_add function must have been called on the
 *    from_state before calling this function, so "from" state
 *    always exists.
 *
 * The current config is "cur_red_config" defined at the top.
 */
static void
lt_tbl_entry_add_context(State* from,
                         SymbolList ctxt,
                         const StateArray& states_new_array)
{
    if (ctxt == nullptr)
        return;

    // 1) locate the LtTblEntry for "from" state.
    LtTblEntry* e = lt_tbl_entry_find_insert(from, states_new_array);
    if (nullptr == e) {
        throw std::runtime_error(
          std::string("lt_tbl_entry_add_context error: state ") +
          std::to_string(from->state_no) + " NOT found\n");
    }

    // 2) locate the LlistContextSet from the current config.
    LlistContextSet* c = llist_context_set_get(e);

    // 3) add/merge the context.
    llist_context_set_add_context(c, ctxt);
}

static void
dump_lt_tbl_entry(LtTblEntry* e)
{
    if (nullptr == e)
        return;

    std::cout << e->from_state << " \t| ";
    // dump_config_context.
    dump_llist_context_set(e->ctxt_set);
    std::cout << "\t| ";
    e->to_states->dump();
    std::cout << std::endl;
}

void
dump_lt_tbl()
{
    if (LT_tbl == nullptr)
        return;

    std::cout << "FROM \t| CONFIG:{CONTEXT} | TO" << std::endl;
    for (LtTblEntry* e = LT_tbl; e != nullptr; e = e->next) {
        dump_lt_tbl_entry(e);
    }
}

/** END */

/************************************************************
 *  phase2_regeneration2 START.
 */

/*
 * Return true if a and b are disjoint, false otherwise.
 * Similar to the function hasEmptyIntersection() in y.c.
 * This can be put into symbol_table.c.
 */
static auto
symbol_list_disjoint(SymbolList a, SymbolList b) -> bool
{
    while (a != nullptr && b != nullptr) {
        if (a->snode == b->snode)
            return false;
        if (a->snode->symbol < b->snode->symbol) {
            a = a->next;
        } else {
            b = b->next;
        }
    }
    return true;
}

/*
 * Check the given context sets are pair_wise disjoint.
 */
static auto
pairwise_disjoint(LlistContextSet* ctxt_set) -> bool
{
    // if ctxt_set contains less than 2 nodes, return true.
    if (ctxt_set == nullptr || ctxt_set->next == nullptr)
        return true;

    for (LlistContextSet* a = ctxt_set; a->next != nullptr; a = a->next) {
        for (LlistContextSet* b = a->next; b != nullptr; b = b->next) {
            if (symbol_list_disjoint(a->ctxt, b->ctxt) == false) {
                return false;
            }
        }
    }

    return true;
}

/*
 * Create a new cluster and add it to the all_clusters set.
 * e - the start state/LtTblEntry of this cluster.
 *
 * By default, the states element's n1 and n2 are the same.
 */
static auto
cluster_create(LtTblEntry* e) -> LtCluster*
{
    auto* c = new LtCluster;

    // by default, n1 and n2 are the same.
    c->states = llist_int2_create(e->from_state, e->from_state);
    c->ctxt_set = llist_context_set_clone(e->ctxt_set);
    c->pairwise_disjoint = pairwise_disjoint(e->ctxt_set);
    c->next = nullptr;

    if (c->pairwise_disjoint == false)
        all_pairwise_disjoint = false;

    return c;
}

/*
 * Not really neccesssary, but can be used to save running time space.
 */
static void
cluster_destroy(LtCluster* c)
{
    llist_int2_destroy(c->states);
    llist_context_set_destroy(c->ctxt_set);
    delete c;
}

void
LtCluster::dump() const noexcept
{
    LtTblEntry* e = nullptr;

    std::cout << "states: " << std::endl;
    for (LlistInt2* n = this->states; n != nullptr; n = n->next) {
        std::cout << n->n1 << "/" << n->n2 << " [to: ";
        if ((e = lt_tbl_find_entry(n->n1)) != nullptr) {
            for (LlistInt* s = e->to_states; s != nullptr; s = s->next) {
                LlistInt2* m = llist_int2_find_n1(this->states, s->n);
                std::cout << s->n << "/" << ((m == nullptr) ? -1 : m->n2)
                          << " ";
            }
        }
        std::cout << "]" << std::endl;
    }
    std::cout << "context sets: ";
    dump_llist_context_set(this->ctxt_set);

    std::cout << std::endl;
}

static void
all_clusters_dump()
{
    std::cout << "--all_clusters.START--" << std::endl;
    for (const LtCluster* c = all_clusters; c != nullptr; c = c->next) {
        c->dump();
    }
    std::cout << "--END--" << std::endl;
}

auto
LtCluster::contain_state(int state_no) const noexcept -> int
{

    if (state_no < 0)
        return -1;
    // std::cout << "cluster_cotain_state(state_no: " <<  state_no<< ")" <<
    // std::endl;
    if (this->states == nullptr)
        return -1;

    for (const LlistInt2* s = this->states; s != nullptr; s = s->next) {
        if (s->n1 == state_no) {
            // std::cout << "found" << std::endl;
            return s->n2;
        }
    }
    // std::cout << "NOT found" << std::endl;

    return -1;
}

/*
 * Return:
 *   the splitted state's no if state_no is in c->states list
 *   -1 otherwise.
 *
 * Note state_no here is the actual state_no.
 * There could be only one cluster contains it.
 */
auto
cluster_contain_actual_state(LtCluster* c, int state_no) -> int
{
    if (state_no < 0)
        return -1;
    // std::cout << "cluster_cotain_state(state_no: " <<  state_no<< ")" <<
    // std::endl;
    if (c == nullptr || c->states == nullptr)
        return -1;

    for (LlistInt2* s = c->states; s != nullptr; s = s->next) {
        if (s->n2 == state_no) {
            // std::cout << "found" << std::endl;
            return s->n1;
        }
    }
    // std::cout << "NOT found" << std::endl;

    return -1;
}

/*
 * Combine the two chains dst and src:
 *   if src contains a LlistContextSet node whose config is NOT in dst,
 *     add it in INC order.
 *   if src contains a LlistContextSet node whose config is in dst,
 *     combine the context.
 */
auto
llist_context_set_merge_chain(LlistContextSet* dst, LlistContextSet* src)
  -> LlistContextSet*
{

    if (src == nullptr)
        return dst;
    if (dst == nullptr)
        return llist_context_set_clone(src);

    LlistContextSet* b = src;
    LlistContextSet *a_prev = nullptr, *a = dst;
    for (; a != nullptr; a_prev = a, a = a->next) {
        while (b != nullptr) {
            int cmp = a->config->ruleID - b->config->ruleID;
            if (cmp == 0) { // same config, combine contexts.
                llist_context_set_add_context(a, b->ctxt);
                a_prev = a;
                a = a->next;
                b = b->next;
            } else if (cmp < 0) {
                a_prev = a;
                a = a->next;
            } else {                     // cmp > 0, insert b to dst before a.
                if (a_prev == nullptr) { // add to the head
                    a_prev = dst = llist_context_set_create(b->config);
                    llist_context_set_add_context(dst, b->ctxt);
                    dst->next = a;
                } else { // add to the middle.
                    a_prev->next = llist_context_set_create(b->config);
                    llist_context_set_add_context(a_prev->next, b->ctxt);
                    a_prev->next->next = a;
                }
                b = b->next;
            }
            if (a == nullptr) {
                break;
            }
        } // end of while(b != nullptr).

        if (b == nullptr || a == nullptr)
            break;
    }

    if (b != nullptr) { // clone b and its tail to the end of a_prev.
        a_prev->next = llist_context_set_clone(b);
    }

    return dst;
}

auto
clone_originator_list(OriginatorList* o) -> OriginatorList*
{
    auto* s = new OriginatorList;
    s->list = o->list;
    return s;
}

void
copy_config_lalr(Configuration* dst, Configuration* src)
{
    if (dst == nullptr || src == nullptr)
        return;

    dst->ruleID = src->ruleID;
    dst->nMarker = src->nMarker;
    dst->marker = src->marker;
    copy_context(dst->context, src->context);
    dst->owner = src->owner;

    dst->isCoreConfig = src->isCoreConfig;
    dst->COMPLETE = src->COMPLETE;
    dst->IN_LANE = src->IN_LANE;
    dst->ORIGINATOR_CHANGED = src->ORIGINATOR_CHANGED;
    dst->LANE_END = src->LANE_END;
    dst->LANE_CON = src->LANE_CON;

    dst->originators = clone_originator_list(src->originators);
    dst->transitors = clone_originator_list(src->transitors);
}

static auto
clone_state(const Grammar& grammar, const State* s) -> State*
{
    auto* t = new State;

    t->next = s->next;
    t->config = s->config;
    t->core_config_count = s->core_config_count;

    for (size_t i = 0; i < t->config.size(); i++) {
        t->config[i] = create_config(grammar, -1, 0, 1);
        copy_config_lalr(t->config[i], s->config[i]);
        t->config[i]->owner = t;
    }

    t->state_no = s->state_no;
    t->trans_symbol = SymbolNode::create(s->trans_symbol->snode);

    t->successor_list = s->successor_list;

    t->parents_list = s->parents_list->clone();

    t->ON_LANE = s->ON_LANE;
    t->COMPLETE = s->COMPLETE;
    t->PASS_THRU = s->PASS_THRU;
    t->REGENERATED = s->REGENERATED;

    return t;
}

/*
 * In the successor list of src_state, replace s_old with s_new.
 */
void
replace_successor(State* src_state, State* s_new, State* s_old)
{
    if (s_new == s_old)
        return;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "replace successor of state " << src_state->state_no
                  << ": " << s_old->state_no << " replaced by "
                  << s_new->state_no << ": ";
    }

    for (auto& successor : src_state->successor_list) {
        if (successor == s_old) {
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

/*
 * Used for LR(k) purpose only.
 * In the lane_head_tail_pairs list, replace
 * those entries whose tail config is s to s_copy
 * according to cur_red_config.
 *
 * This solves this question: end_config is not a cur_red_config but
 * belongs to a state in the middle of a lane? Using this method
 * this is not a problem any more.
 *
 * Here it's called when a new state is splitted, but
 * we replace ALL the pairs related to end_config->owner state.
 *
 * Algorithm:
 * if (end_config->owner == s) {
 *   for each ConfigPairNode n {
 *     if (n->end_owner == s &&
 *         n->start is in the same cluster as s_copy) {
 *       replace n->end (the one in s) by the one in s_copy;
 *     }
 *   }
 * }
 */
static void
lane_head_tail_pairs_replace(LtCluster* c,
                             Configuration* end_config,
                             State* s,
                             State* s_copy)
{
    if (end_config->owner != s)
        return;

    // std::cout << "LHTPR: end_config: " <<
    //        end_config->owner->state_no<< "." <<  end_config->ruleID <<
    //        std::endl;

    for (ConfigPairNode* n = lane_head_tail_pairs; n != nullptr; n = n->next) {
        // n->end->owner == end_config->owner.
        // n->start->owner is a state in cluster c -- should I
        // search as n1 or n2??? I think should be n2, so it
        // covers the situation where n->start->owner is a
        // splitted state.
        if (n->end->owner == s &&
            c->states->find_n2(n->start->owner->state_no) != nullptr) {
            // do replacement for n->end: from that in s to s_copy.
            for (int i = 0; i < s->config.size(); i++) {
                if (s->config[i] == n->end) {
                    n->end = s_copy->config[i];
                    break;
                }
            }
        }
    }
}

/*
 * Add the LtTblEntry e->from_state to c.
 *
 * Return: the parent state_no:
 *   if a splitted state is created, return it's state_no.
 *   else, return the old state_no.
 */
auto
LaneTracing::cluster_add_lt_tbl_entry(LtCluster* c,
                                      const int from_state,
                                      LlistContextSet* e_ctxt,
                                      const size_t e_parent_state_no,
                                      bool copy) const -> int
{
    int state_no = from_state;

    if (copy) {
        // make a new state by copying e->from_state,
        // add it to the end of states_new array, and add here.
        State* s_parent =
          this->new_states.states_new_array->state_list[e_parent_state_no];
        State* s = this->new_states.states_new_array->state_list[state_no];
        State* s_copy = clone_state(grammar, s);
        // insert a state to the parsing machine. Defined in y.c.
        this->new_states.insert_state_to_pm(s_copy);

        // Replace src_state's previous succcessor with s_copy.
        replace_successor(s_parent, s_copy, s);

        state_no = s_copy->state_no;

        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "clone state " << s->state_no << " to " << state_no
                      << std::endl;
        }

        if (this->options.use_lr_k) {
            // For LR(k), replace entry in lane_head_tail_pairs!
            // std::cout << "parrent state: " <<  e_parent_state_no <<
            // std::endl;
            lane_head_tail_pairs_replace(c, cur_red_config, s, s_copy);
        }
    }

    // add state_no.
    // n1: original state, n2: splitted state.
    c->states = llist_int2_add_inc(c->states, from_state, state_no);
    // combine context sets.
    c->ctxt_set = llist_context_set_merge_chain(c->ctxt_set, e_ctxt);

    return state_no;
}

/*
 * From all_clusters, find the one(s) that contains
 * the state (whose state number is state_no).
 *
 * Note there could be more than one such cluster if the
 * given state was splitted before.
 *
 * This function is like a iterator. To use this function,
 * initialized c = nullptr, then call in a loop:
 *   c = nullptr;
 *   while ((c = find_containing_cluster(c, state_no) != nullptr) {
 *     ...
 *   }
 */
auto
find_containing_cluster(LtCluster* c, int state_no) -> LtCluster*
{
    c = (nullptr == c) ? all_clusters : c->next;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        if (nullptr != c)
            std::cout << "c: current state: " << c->states->n1 << "/"
                      << c->states->n2 << std::endl;
    }

    for (; c != nullptr; c = c->next) {
        if (c->contain_state(state_no) >= 0) {
            return c;
        }
    }
    return nullptr;
}

/*
 * Different from find_containing_cluster in that:
 *
 * Find the cluster than contains the ACTUAL state_no.
 * There can be at most one such cluster.
 */
auto
LtCluster::find_actual_containing_cluster(int state_no) -> LtCluster*
{
    for (LtCluster* c = all_clusters; c != nullptr; c = c->next) {
        if (cluster_contain_actual_state(c, state_no) >= 0) {
            return c;
        }
    }
    return nullptr;
}

/*
 * In LT_tbl, find the entry where from_state is state_no.
 *
 * Note that in LT_tbl, the entries are in INC order of state_no.
 */
auto
lt_tbl_find_entry(int from_state) -> LtTblEntry*
{
    for (LtTblEntry* e = LT_tbl; e != nullptr; e = e->next) {
        int cmp = e->from_state - from_state;
        if (cmp == 0)
            return e;
        if (cmp > 0)
            break;
    }

    return nullptr; // not found.
}

/*
 * Combine new_part into old_part which is already in all_clusters list.
 *
 * Add each state in new_part to old_part, also merge context sets.
 */
void
combine_cluster(LtCluster* c_new, LtCluster* c_old)
{
    // merge states.
    for (LlistInt2* a = c_new->states; a != nullptr; a = a->next) {
        c_old->states = llist_int2_add_inc(c_old->states, a->n1, a->n2);
    }
    // merge context sets.
    c_old->ctxt_set =
      llist_context_set_merge_chain(c_old->ctxt_set, c_new->ctxt_set);
}

/// functions for updating context in phase 2 regeneration. START.

/// Called by cluster_trace_new_chain() only.
void
LaneTracing::inherit_propagate(int state_no,
                               int parent_state_no,
                               LtCluster* container,
                               LtTblEntry* e)
{
    State* s = this->new_states.states_new_array->state_list[state_no];
    State* s_p = this->new_states.states_new_array->state_list[parent_state_no];
    if (inherit_parent_context(grammar, s, s_p)) {
        s->get_closure(grammar,
                       config_queue); /* needed only if context changed.*/
        this->lt_phase2_propagate_context_change(state_no, container, e);
    }
}

void
LaneTracing::clear_inherit_regenerate(int state_no, int parent_state_no)
{
    State* s = this->new_states.states_new_array->state_list[state_no];
    State* s_p = this->new_states.states_new_array->state_list[parent_state_no];
    clear_state_context(s);
    if (inherit_parent_context(this->grammar, s, s_p)) {
        s->get_closure(this->grammar,
                       this->config_queue); /* needed only if context changed.*/
    }
}

/*
 * Called by phase2_regeneration2() only.
 *
 * Note that if it is state 0, then should add $end
 * to the goal rule before get_closure() !
 */
void
LaneTracing::clear_regenerate(int state_no)
{
    State* s = this->new_states.states_new_array->state_list[state_no];
    clear_state_context(s);
    if (0 == (state_no)) {
        hash_tbl_insert(STR_END);
        s->config[0]->context->nContext =
          SymbolNode::create(hash_tbl_find(STR_END));
        s->config[0]->context->context_count = 1;
    }
    s->get_closure(this->grammar, this->config_queue);
}

/*
 * Function to propagate context change until a state where
 * there is no more context change.
 *
 * Here the children states are those defined in the
 * LtTblEntry's to_states list.
 */
void
LaneTracing::lt_phase2_propagate_context_change(int state_no,
                                                LtCluster* c,
                                                LtTblEntry* e)
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

    for (const LlistInt* t = e->to_states; t != nullptr; t = t->next) {
        LtTblEntry* f = lt_tbl_find_entry(t->n);
        if (f != nullptr) {
            // need to replace t->n with the true state_no in cluster c.
            const LlistInt2* t2 = llist_int2_find_n1(c->states, t->n);
            if (t2 == nullptr) {
                if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                    std::cout << "--to state " << t->n
                              << ", not found in cluster." << std::endl;
                    // cluster_dump(c);
                }
                continue; // if not found, just ignore this to state.
            }
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "--to state: " << t->n << ", actual: " << t2->n2
                          << std::endl;
            }
            this->inherit_propagate(t2->n2, state_no, c, f);
        }
    }
}

auto
inherit_parent_context(const Grammar& grammar, State* s, State* parent) -> bool
{
    if (s == nullptr || parent == nullptr)
        return false;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "state " << s->state_no
                  << ": to inherit context from parent state "
                  << parent->state_no << std::endl;
    }

    bool is_changed = false;
    const SymbolTblNode* trans_symbol = s->trans_symbol->snode;
    for (const auto& c_p : parent->config) {
        if (is_final_configuration(grammar, c_p))
            continue;
        if (trans_symbol != get_scanned_symbol(c_p))
            continue;

        c_p->marker++;
        int config_index = 0;
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
        if (0 && s->state_no == 39) {
            std::cout << "after: " << std::endl;
            my_write_state(grammar, *s);
        }
    }

    return is_changed;
}

/*
 * Set a state's all configs' context to empty.
 */
void
clear_state_context(State* s)
{
    if (nullptr == s)
        return;

    for (const auto& i : s->config) {
        i->context->clear(); // defined in y.c.
    }
}

/// functions for updating context in phase 2 regeneration. END.

/*
 * Recursively add states to LtCluster c along the chain in LT_tbl.
 *
 * Parameters:
 *  @ c - The new cluster,
 *  @ parent_state_no - the parent state no.
 *  @ state_no - the state no of the from_state LT_tbl.
 * Return:
 *  false - combined with a cluster in all_clusters..
 *  true - NOT combined with another cluster in all_clusters.
 */
auto
LaneTracing::cluster_trace_new_chain(int parent_state_no, int state_no) -> bool
{
    bool is_new_chain = true;
    LtCluster* c = new_cluster;

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        std::cout << "cluster: " << c << ". next state on chain: " << state_no
                  << std::endl;
    }

    // e will be used no matter what happen.
    LtTblEntry* e = lt_tbl_find_entry(state_no);
    if (nullptr == e) { // Is this possible? YES IT IS.
        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "END of chain - state_no: " << state_no << std::endl;
        }
    }
    LlistContextSet* e_ctxt = (e == nullptr) ? nullptr : e->ctxt_set;

    // state in in cluster c.
    int ret_state = c->contain_state(state_no);
    if (ret_state >= 0) {
        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "=>2. state " << ret_state
                      << ": inherit context from state " << parent_state_no
                      << " & propagate" << std::endl;
        }
        this->inherit_propagate(ret_state, parent_state_no, c, e);

        const auto& state_list = this->new_states.states_new_array->state_list;
        replace_successor(state_list[parent_state_no],
                          state_list[ret_state],
                          state_list[state_no]);
        return true; // NOTE here it returns true.
    }

    // otherwise, state is NOT in c yet.

    // There could be more than one of this because of
    // copies made. Find the first one that has no conflict.
    // If all have conflict, use the first one.
    bool container_not_found = true;
    LtCluster *first_container = nullptr, *container = nullptr;

    // note: this while loop can ignore the current cluster since
    // it has already been searched once above.
    while ((container = find_containing_cluster(container, state_no)) !=
           nullptr) {
        if (first_container == nullptr)
            first_container = container;

        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "2. state " << state_no << " is in a cluster "
                      << container << std::endl;
        }

        if (container->pairwise_disjoint) {
            LlistContextSet* x = llist_context_set_clone(c->ctxt_set);
            x = llist_context_set_merge_chain(x, container->ctxt_set);
            bool is_pairwise_disjoint = pairwise_disjoint(x);
            llist_context_set_destroy(x);

            if (is_pairwise_disjoint) {
                if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                    std::cout
                      << "3. combine 2 clusters result is pairwise disjoint"
                      << std::endl;
                }
                combine_cluster(c, container); // container is the result.
                is_new_chain = false;          // not a new chain.

                // This is used so cluster_contain_state() at the beginning
                // of this function can return correct value once c is changed.
                c = new_cluster = container;

                if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                    std::cout << "=>3. state " << state_no
                              << ": inherit context from state "
                              << parent_state_no << ", ";
                }
                this->inherit_propagate(
                  state_no, parent_state_no, container, e);

                container_not_found = false;
                break;
            }
        }
        // else, NOT pairwise disjoint, continue to find the next match.
    } // end of while.

    if (container_not_found) {
        if (first_container == nullptr) { // always add.
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "4. state " << state_no
                          << " is NOT in any cluster yet" << std::endl;
            }
            ret_state = this->cluster_add_lt_tbl_entry(
              c, state_no, e_ctxt, parent_state_no, false);

            if (c->pairwise_disjoint &&
                pairwise_disjoint(c->ctxt_set) == false) {
                c->pairwise_disjoint = false;
                all_pairwise_disjoint = false;
            }
            if (nullptr != e)
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
              c, state_no, e_ctxt, parent_state_no, true);
            if ((c->pairwise_disjoint = pairwise_disjoint(c->ctxt_set)) ==
                false) {
                all_pairwise_disjoint = false;
            }
            if constexpr (DEBUG_PHASE_2_REGENERATE2) {
                std::cout << "=>5. state " << ret_state
                          << ": clear, inherit context from state "
                          << parent_state_no << ", ";
            }
            this->clear_inherit_regenerate(ret_state, parent_state_no);
        }

        if (nullptr != e) { // recursively trace the chain.
            is_new_chain = this->cluster_trace_new_chain_all(ret_state, e);
        }
    }

    return is_new_chain;
}

/*
 * parent_state: state_no of the parent state.
 */
auto
LaneTracing::cluster_trace_new_chain_all(int parent_state, const LtTblEntry* e)
  -> bool
{
    bool is_new_chain = true;

    // recursively trace the chain.
    for (const LlistInt* s = e->to_states; s != nullptr; s = s->next) {
        if (false == this->cluster_trace_new_chain(parent_state, s->n)) {
            is_new_chain = false;
        }
    }

    return is_new_chain;
}

/*
 * Add c to the all_clusters list.
 * c is always added to the tail of all_clusters.
 */
void
all_clusters_add(LtCluster* c)
{
    if (all_clusters == nullptr) {
        all_clusters_tail = all_clusters = c;
    } else { // add to the tail.
        all_clusters_tail->next = c;
        all_clusters_tail = all_clusters_tail->next;
    }
}

void
LaneTracing::phase2_regeneration2()
{
    bool is_new_chain = true;

    all_clusters = all_clusters_tail = nullptr; // initialize all_clusters.
    all_pairwise_disjoint = true;

    for (LtTblEntry* e = LT_tbl; e != nullptr; e = e->next) {
        if (e->processed)
            continue;

        LtTblEntry* x = e; // start state of another chain/cluster of states.
        new_cluster = cluster_create(x);

        if constexpr (DEBUG_PHASE_2_REGENERATE2) {
            std::cout << "== chain head state: " << e->from_state << std::endl;
            std::cout << "=>1. clear and regenerate context for state "
                      << x->from_state << std::endl;
        }
        this->clear_regenerate(x->from_state);

        x->processed = true;

        is_new_chain = this->cluster_trace_new_chain_all(x->from_state, x);

        // add new_cluster to the all_clusters list.
        if (is_new_chain) {
            all_clusters_add(new_cluster);
        }
    }

    if constexpr (DEBUG_PHASE_2_REGENERATE2) {
        all_clusters_dump(); // dump if is new chain.
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

static auto
create_lane_head(State* s, SymbolTblNode* n) -> laneHead*
{
    auto* h = new laneHead;
    h->s = s;
    h->contexts = new SymbolNode;
    h->contexts->snode = n;
    h->contexts->next = nullptr;
    h->next = nullptr;
    return h;
}

static void
destroy_lane_head_node(laneHead* h)
{
    if (nullptr == h)
        return;

    free_symbol_node_list(h->contexts);
    delete h;
}

/*
 * Add n to the contexts list of h in INC order.
 *
 * Called by addLaneHeadList() only, h and n are Not nullptr.
 */
static void
add_context_to_lane_head(laneHead* h, SymbolTblNode* n)
{
    SymbolNode *sn = h->contexts, *sn_prev = nullptr;
    for (; sn != nullptr; sn_prev = sn, sn = sn->next) {
        if (sn->snode == n)
            return; // already in list, don't add.
        if (sn->snode->symbol > n->symbol) {
            // add n before sn, after sn_prev.
            break;
        }
    }
    auto* tmp = new SymbolNode;
    tmp->snode = n;
    tmp->next = sn;
    if (sn_prev == nullptr) { // add as the first node.
        h->contexts = tmp;
    } else { // add in the middle, after sn_prev, before sn.
        sn_prev->next = tmp;
    }
}

/*
 * Add another pair of s to h.
 * lh_list is in INC order of state's state_no;
 */
static auto
add_lane_head_list(laneHead* lh_list, State* s) -> laneHead*
{
    if (s == nullptr) {
        throw std::runtime_error(
          "lane_tracing.cpp add_lane_head_list error: s is nullptr");
    }

    if (nullptr == lh_list) {
        return create_lane_head(s, nullptr);
    }
    // else, search if s is in list.
    laneHead *h_prev = nullptr, *h = lh_list;
    for (; h != nullptr; h_prev = h, h = h->next) {
        if (h->s == s) {
            return lh_list;
        } // s exists already.
        if (h->s->state_no > s->state_no)
            break; // guarantee INC order.
    }

    // otherwise, s is NOT in list lh_list. Add another laneHead node.
    if (h == lh_list) { // insert as the first node.
        h = create_lane_head(s, nullptr);
        h->next = lh_list;
        return h;
    } // insert in the middle after h_prev and before h.
    laneHead* tmp = create_lane_head(s, nullptr);
    h_prev->next = tmp;
    tmp->next = h;
    return lh_list;
}

static void
dump_lane_head_list(const laneHead* lh_list)
{
    std::cout << "dumpLaneHeadList: " << std::endl;
    for (const laneHead* h = lh_list; h != nullptr; h = h->next) {
        std::cout << h->s->state_no << " " << std::endl;
    }
}

/* for originator list */

/*
 * Used when use_lane_tracing  only.
 * Called in y.c function create_config().
 */
auto
create_originator_list() -> OriginatorList*
{
    auto* o = new OriginatorList;
    o->list.reserve(ORIGINATOR_LIST_LEN_INIT);
    return o;
}

/*
 * Add originator to c's originator list if it does not exist yet.
 *
 * @parameter:
 *  cycle: 1 - called from combineConfigOriginators().
 *             Is a cycle, should insert to originator list.
 *         0 - called from getConfigSuccessors_LR0().
 *             NOT a cycle, should NOT insert to originator list.
 */
auto
insert_originator_list(Configuration* c, Configuration* originator, int cycle)
  -> bool
{
    OriginatorList* o = c->originators;

    if (c == originator && cycle == 0) {
        return false;
    }
    for (const auto& item : o->list) {
        if (originator == item)
            return false; // already exists.
    }

    o->list.push_back(originator);
    c->ORIGINATOR_CHANGED = true;
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

/*
 * Add transitor to c's transitor list if it does not exist yet.
 */
static auto
insert_transitor_list(Configuration* c, Configuration* transitor) -> bool
{
    OriginatorList* o = c->transitors;

    if (c == transitor)
        return false;

    for (const ConfigurationNode* item : o->list) {
        if (transitor == item)
            return false; // already exists.
    }

    o->list.push_back(transitor);
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

/*
 * If state_no is not in the inadequate states list, add it.
 */
void
add_state_no_array(StateNoArray* sa, int state_no)
{
    for (int s : sa->states) {
        if (s == state_no) {
            return;
        } // exists.
    }
    // std::cout << "state " <<  state_no<< " is added to list. " << std::endl;
    sa->states.push_back(state_no);
}

void
dump_state_no_array(const StateNoArray* sa)
{
    if (sa == nullptr)
        return;

    for (const int state : sa->states) {
        std::cout << state << " ";
    }
    std::cout << std::endl;
}

/* for debug use */
void
my_write_symbol_node_array(const SymbolNode* str)
{
    for (const SymbolNode* a = str; a != nullptr; a = a->next) {
        if (a != str)
            std::cout << ", ";
        std::cout << a->snode->symbol;
    }
}

static void
my_write_context(const Context* c, const bool use_lr_k)
{
    std::cout << " {";

    const SymbolNode* s = c->nContext;
    if (s != nullptr) {
        std::cout << s->snode->symbol;
        while ((s = s->next) != nullptr) {
            std::cout << ", " << s->snode->symbol;
        }
    }

    if (use_lr_k) {
        // specifically for LR(k). This can be combined with the
        // above if block. Single this part out here is to keep
        // the code easier to maintain for LR(1) and LR(k) separately.
        for (c = c->next; c != nullptr; c = c->next) {
            std::cout << "; ";
            if ((s = c->nContext) != nullptr) {
                std::cout << s->snode->symbol;
                while ((s = s->next) != nullptr) {
                    std::cout << ", " << s->snode->symbol;
                }
            }
        }
    }

    std::cout << "} ";
}
static void
my_write_production(const Production* p, int marker)
{
    if (p == nullptr) {
        // std::cout << "writeProduction warning: p is nullptr" << std::endl;
        return;
    }

    std::cout << p->nLHS->snode->symbol << " -> ";

    int i = 0;
    for (const SymbolNode* n = p->nRHS_head; n != nullptr; n = n->next) {
        if (i == marker)
            std::cout << ". ";
        std::cout << n->snode->symbol << " ";
        i++;
    }
    if (i == marker)
        std::cout << ". ";

    // print this only when marker = -1.
    // i.e. called from writeGrammar().
    if (marker == -1 && p->isUnitProduction)
        std::cout << "(unit production)";

    if (marker == -1 && p->lastTerminal != nullptr)
        std::cout << "(Precedence Terminal: " << p->lastTerminal->symbol << ")";

    // if write configration, then don't go to new line.
    // since the context has not been written.
    if (marker < 0)
        std::cout << std::endl;
}

static void
my_write_config_originators(const Configuration& c);

void
stdout_write_config(const Grammar& grammar, const Configuration* c)
{
    if (c == nullptr) {
        return;
    }
    std::cout << "config (" << c->owner->state_no << "." << c->ruleID << ") : ";
    my_write_production(grammar.rules[c->ruleID], c->marker);
    my_write_context(c->context, Options::get().use_lr_k);
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

/*
 * For debug use.
 */
void
Configuration::write_originators(std::ostream& os) const noexcept
{
    const size_t ct = this->originators->list.size();
    if (ct == 0)
        return;

    os << "      " << ct << " originator" << ((ct > 1) ? "s" : "") << ": "
       << std::endl;
    for (const Configuration* o : this->originators->list) {
        os << "      originator (" << o->owner->state_no << "." << o->ruleID
           << "." << o->marker << ") " << std::endl;
    }
}

void
Configuration::write_transitors(std::ostream& os) const noexcept
{
    const size_t ct = this->transitors->list.size();
    if (ct == 0)
        return;

    os << "      " << ct << " transitor" << ((ct > 1) ? "s" : "") << ": "
       << std::endl;
    for (const Configuration* o : this->transitors->list) {
        os << "      transitor (" << o->owner->state_no << "." << o->ruleID
           << "." << o->marker << ") " << std::endl;
    }
}

void
my_write_config_originators(const Configuration& c)
{
    const size_t ct = c.originators->list.size();
    std::cout << "config has " << ct << " originators: " << std::endl;
    for (const Configuration* o : c.originators->list) {
        std::cout << "      originator (" << o->owner->state_no << "."
                  << o->ruleID << ") " << std::endl;
    }
}

/*
 * get context for reduce productions in conflict states.
 */
void
LaneTracing::get_inadequate_state_reduce_config_context(const State* s) const
{
    if constexpr (DEBUG_PHASE_1) {
        std::cout << "state " << s->state_no << " [" << s->config.size()
                  << " configurations, trans_symbol: "
                  << s->trans_symbol->snode->symbol << "]: " << std::endl;
    }

    for (const auto& config : s->config) {
        Configuration* c = config;
        if (is_final_configuration(grammar, c)) {
            lane_tracing_reduction(grammar, c);
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

    for (const int state_no : states_inadequate->states) {
        const State* s =
          this->new_states.states_new_array->state_list[state_no];
        this->get_inadequate_state_reduce_config_context(s);
    }

    // next will check resolved conflicts.
}

/*
 * called by update_action_table().
 */
static auto
find_successor_state_no(int state_no,
                        const StateArray& states_new_array,
                        const SymbolTblNode* snode) -> int
{
    const State* state = states_new_array.state_list[state_no];
    auto it = state->successor_list.rbegin();
    while (it != state->successor_list.rend()) {
        const State* successor = *it;
        if (snode == successor->trans_symbol->snode) {
            return successor->state_no;
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
clear_state_terminal_transitions(const Grammar& grammar, int state_no)
{
    size_t start = (static_cast<size_t>(state_no) * ParsingTblCols);
    size_t end = (state_no * ParsingTblCols) + (grammar.terminal_count + 1);
    for (size_t i = start; i < end; ++i) {
        ParsingTable.at(i) = 0;
    }
}

/*
 * clear all the conflicts from states_new_array->conflict_list.
 */
static void
clear_state_conflicts(int state_no, StateArray& states_new_array)
{
    states_new_array.conflict_list[state_no] = nullptr;
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
        const int state_no = states_inadequate->states[i];
        if (state_no < 0)
            continue; // should never happen.

        const State* state = new_states.states_new_array->state_list[state_no];

        // clear the parsing table row for S where lookahead is terminal.
        if constexpr (DEBUG_RESOLVE_CONFLICT) {
            std::cout << "-----clear state[" << i << "] = " << state_no
                      << ". len=" << ct << std::endl;
        }
        clear_state_terminal_transitions(this->grammar, state_no);

        // clear all the conflicts associated with S.
        clear_state_conflicts(state_no, *this->new_states.states_new_array);

        // re-insert actions into parsing table for this state.
        for (int j = static_cast<int>(state->config.size() - 1); j >= 0; j--) {
            const Configuration* config = state->config[j];
            if (is_final_configuration(grammar, config) &&
                config->context != nullptr) {
                // insert reduce
                for (const SymbolNode* contxt = config->context->nContext;
                     contxt != nullptr;
                     contxt = contxt->next) {
                    this->insert_action(
                      contxt->snode, state_no, (-1) * config->ruleID);
                }
            } else if (config->nMarker != nullptr &&
                       config->nMarker->snode->is_terminal()) {
                // insert shift.
                this->insert_action(
                  config->nMarker->snode,
                  state_no,
                  find_successor_state_no(state_no,
                                          *new_states.states_new_array,
                                          config->nMarker->snode));
            }
        }
        std::shared_ptr<Conflict>& c =
          this->new_states.states_new_array->conflict_list[state_no];
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

static void
write_conflicting_context(int state_no, const StateArray& states_new_array)
{
    const Conflict* c = states_new_array.conflict_list[state_no].get();
    if (c == nullptr)
        return;

    std::cout << "conflicting contexts: " << c->lookahead->symbol;

    for (c = c->next.get(); c != nullptr; c = c->next.get()) {
        std::cout << ", " << c->lookahead->symbol;
    }
}

static auto
remove_pass_through_states(laneHead* lh_list) -> laneHead*
{
    laneHead *h = lh_list, *h_prev = nullptr;
    for (; h != nullptr;) {
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
            laneHead* tmp = h;
            if (h_prev == nullptr) { // first node
                lh_list = h->next;
                h = lh_list;
            } else { // node in the middle of list.
                h_prev->next = tmp->next;
                h = tmp->next;
            }
            destroy_lane_head_node(tmp);
        } else {
            h_prev = h;
            h = h->next;
        }
    }

    return lh_list;
}

void
LaneTracing::gpm(State* new_state)
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

        new_state->get_closure(
          this->grammar,
          this->config_queue); // get closure of this state.

        // get successor states and add them to states_new.
        this->state_transition(*new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    ParsingTblRows = this->new_states.states_new->state_count;
    n_state_opt1 = this->new_states.states_new->state_count;

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

    return coll;
}

/*
 * Given the trans_symbol, find the index of the
 * successor of s that has this trans_symbol.
 */
static auto
get_successor_index(const State& s, const SymbolTblNode* trans_symbol)
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
LaneTracing::add_split_state(State& y, State& s, size_t successor_index) -> bool
{
    bool is_compatible = false;
    State* os =
      this->search_state_hash_tbl(y,
                                  &is_compatible); // same or compatible.

    if (os == nullptr) { // No existing state found. Add Y as a new state.
        y.state_no = this->new_states.states_new->state_count;
        if constexpr (DEBUG_PHASE_2_REGENERATE) {
            // std::cout << "split - new state" << std::endl;
            std::cout << "split - add new state " << y.state_no << std::endl;
        }
        this->new_states.states_new->add_state2(&y);
        this->new_states.states_new_array->add_state(&y);
        // update shift action.
        update_action(get_col(*y.trans_symbol->snode), s.state_no, y.state_no);
        // update the Y0 successor link of S to Y.
        s.successor_list[successor_index] = &y;
        while (this->new_states.states_new->state_count >= PARSING_TABLE_SIZE) {
            expand_parsing_table(*this->new_states.states_new_array);
        }
        return true;
    } // same or compatible with an existing state.
    // std::cout << "split - old state" << std::endl;
    update_action(get_col(*os->trans_symbol->snode), s.state_no, os->state_no);
    s.successor_list[successor_index] = os;
    State::destroy_state(&y);
    return false;
}

static auto
add_unique_queue(State* s, laneHead* lh_list) -> laneHead*
{
    laneHead* h = lh_list;
    if (h == nullptr)
        return create_lane_head(s, nullptr);

    if (h->s == s) { // exists, is the first node.
        if constexpr (DEBUG_PHASE_2_REGENERATE) {
            std::cout << "state " << s->state_no << " already on laneHead queue"
                      << std::endl;
        }
        return lh_list;
    }

    for (; h->next != nullptr; h = h->next) {
        if (h->next->s == s) { // exists already.
            if constexpr (DEBUG_PHASE_2_REGENERATE) {
                std::cout << "state " << s->state_no
                          << " already on laneHead queue" << std::endl;
            }
            return lh_list;
        }
    }
    // now h->next is nullptr. insert s to the end.
    h->next = create_lane_head(s, nullptr);
    return lh_list;
}

/*
 * Replace the context of S by those of T.
 *
 * Assumption: S and T are as in the phase2_regeneration()
 * function: they contains the same number of configurations.
 * T will be destroyed, so its context lists can be moved to S.
 */
static void
regenerate_state_context(State& s, const State& t)
{
    if (t.core_config_count != s.core_config_count) {
        throw std::runtime_error("regenerate error: inequal config_count");
    }

    // clear the context of S.
    for (const auto& config :
         s.config) { // -> if final config, remove p.t. entry.
        Context* c = config->context;
        free_symbol_node_list(c->nContext);
        c->nContext = nullptr;
        c->context_count = 0;
    }

    // copy the context from T to S.
    const size_t ct = t.core_config_count;
    for (size_t i = 0; i < ct; i++) {
        copy_context(s.config[i]->context, t.config[i]->context);
    }
}

/*
 * Combine the contexts from T to S. No propagation here.
 */
static auto
combine_state_context(State& s_dest, const State& s_src) -> bool
{
    bool is_changed = false;
    for (int i = 0; i < s_dest.core_config_count; i++) {
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
            get_scanned_symbol(c)->symbol->empty()) {
            const SymbolNode* lookahead = c->context->nContext;
            for (; lookahead != nullptr; lookahead = lookahead->next) {
                auto [action, state_dest] =
                  get_action(lookahead->snode->type,
                             get_col(*lookahead->snode),
                             s.state_no);
                if (state_dest != c->ruleID) {
                    if (action == '\0' || action == 'r') {
                        update_action(get_col(*lookahead->snode),
                                      s.state_no,
                                      (-1) * c->ruleID);
                    } else { // else, is "s" or "acc".
                        this->insert_action(
                          lookahead->snode, s.state_no, (-1) * c->ruleID);
                    }
                }
            }
        }
    }
}

/*
 * Algorithm is:
 * ==START==
 * Let Q be a queue containing the start states.
 * Associate these start state's contexts to be empty sets.
 *
 * while (Q is not empty) {
 *  s <- next state in Q;
 *  coll <- regenerate successors of s;
 *  foreach state t in coll {
 *    if the corresponding t0 is on conflicting lane {
 *       if (t0 is original) {
 *         replace contexts in t0 with those in t;
 *         // now t0 is not original
 *       } else {
 *         if (t is compatible with t0) {
 *           combine t to to t0;
 *           if (t0 is not in Q) { add t0 to Q; }
 *         } else {
 *           add t as new state;
 *         }
 *       }
 *     }
 *   }
 * }
 *
 * // now Q is empty.
 * GPM() on the new states.
 * ==END==
 */
void
LaneTracing::phase2_regeneration(laneHead* lh_list)
{
    laneHead* h = lh_list;
    State* new_state = nullptr;
    bool exists = false;

    // 1) handle the head states and PASS_THRU states.
    for (; h != nullptr; h = h->next) {
        State& s = *h->s;
        s.get_closure(grammar,
                      config_queue); // get_closure() is defined in y.c
        this->update_state_reduce_action(s);

        if constexpr (DEBUG_PHASE_2_REGENERATE) {
            std::cout << std::endl
                      << "==reg" << std::endl
                      << "erate state " << s.state_no << std::endl;
            my_write_state(grammar, s);
        }
        const StateCollection* coll = get_state_successors(grammar, s);

        for (State* y_ptr = coll->states_head; y_ptr != nullptr;
             y_ptr = y_ptr->next) {
            State& y = *y_ptr;
            std::optional<size_t> successor_index_opt =
              get_successor_index(s, y.trans_symbol->snode);

            if (!successor_index_opt.has_value())
                continue; // should NEVER happen.
            size_t successor_index = *successor_index_opt;
            State& y0 = *s.successor_list[successor_index];

            if (y0.PASS_THRU == 0u) {
                if constexpr (DEBUG_PHASE_2_REGENERATE) {
                    std::cout << "state " << y0.state_no
                              << " PASS_THRU == 0 - NOT on lane" << std::endl;
                }
                continue; // NOT on 'conflicting' lane.
            }
            if constexpr (DEBUG_PHASE_2_REGENERATE) {
                std::cout << "state " << y0.state_no
                          << " PASS_THRU == 1 - on lane" << std::endl;
                std::cout << y.trans_symbol->snode->symbol
                          << " successor isOnConflictingLane" << std::endl;
            }

            if (y0.REGENERATED == 0u) { // is original.
                if constexpr (DEBUG_PHASE_2_REGENERATE) {
                    std::cout << "replace - replace old state " << y0.state_no
                              << std::endl;
                }
                // replace the context of Y0 with those of Y.
                regenerate_state_context(y0, y);
                y0.REGENERATED = 1;
                lh_list = add_unique_queue(&y0, lh_list);
            } else { // is regenerated state.
                exists = is_compatible_states(&y0, &y);

                if (exists) {
                    if constexpr (DEBUG_PHASE_2_REGENERATE) {
                        std::cout << "combine to compatible state "
                                  << y0.state_no << std::endl;
                    }
                    combine_state_context(y0, y);
                    lh_list = add_unique_queue(&y0, lh_list);
                } else {
                    if (this->add_split_state(y, s, successor_index)) {
                        if (new_state == nullptr) {
                            new_state = &y;
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

static void
write_the_symbol_list(SymbolList a)
{
    SymbolNode* b = a;
    std::cout << "{";
    if (b != nullptr) {
        std::cout << b->snode->symbol;
        for (b = b->next; b != nullptr; b = b->next) {
            std::cout << ", " << b->snode->symbol;
        }
    } else
        std::cout << "EMPTY";
    std::cout << "}";
}

auto
LaneTracing::get_the_context(const Configuration* o) const -> SymbolNode*
{
    if (o == nullptr)
        return nullptr;

    const SymbolNode* scanned_symbol = o->nMarker;
    if (scanned_symbol == nullptr)
        return nullptr;

    SymbolNode* gamma = scanned_symbol->next;
    SymbolNode* gamma_theads =
      get_theads(this->grammar, gamma); // Note nullptr is a valid terminal.

    // note that "" will be the first node in the INC list,
    // so it's not so inefficient.
    bool exist = false;
    gamma_theads =
      remove_from_symbol_list(gamma_theads, hash_tbl_find(""), &exist);

    if constexpr (DEBUG_PHASE_2_GET_TBL) {
        if (gamma_theads != nullptr) {
            std::cout << "C: Add context to entry state " << o->owner->state_no
                      << ": ";
            std::cout << "[" << cur_red_config->owner->state_no << "."
                      << cur_red_config->ruleID << "] ";
            write_the_symbol_list(gamma_theads);
            std::cout << std::endl;
        }
    }

    lt_tbl_entry_add_context(
      o->owner,
      gamma_theads,
      *this->new_states.states_new_array); // add context.

    return gamma_theads;
}

/*
 * Recursively trace back the lane along originators until
 * a config labeld as LANE_END is found.
 *
 * Note that the originator does NOT include those transition
 * config, so only shift config are included. This guarantees
 * that we won't end at those intermediate config that are
 * labeled as "LANE_END" by other lanes, since such intermediate
 * config are always transition configs and are not included as
 * originator by this lane.
 *
 * c is the originator of c0.
 */
auto
LaneTracing::trace_back(const Configuration* c0,
                        Configuration* c,
                        laneHead* lh_list) const -> laneHead*
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
            std::cout << "D: END of config lane FOUND: " << c->owner->state_no
                      << "." << c->ruleID << " " << std::endl;
        }

        if (this->options.use_lr_k) { // for LR(k) use only.
            if constexpr (DEBUG_PHASE_2_GET_TBL) {
                std::cout << "config_red_config: "
                          << cur_red_config->owner->state_no << "."
                          << cur_red_config->ruleID
                          << ", LANE_END: " << c->owner->state_no << "."
                          << c->ruleID << std::endl;
            }
            // Don't use goal production. As it is the augmented rule, and
            // it generates no context at all.
            if (!(c->owner->state_no == 0 && c->ruleID == 0))
                lane_head_tail_pairs = ConfigPairNode::list_insert(
                  lane_head_tail_pairs, cur_red_config, c);
        }

        lh_list = add_lane_head_list(lh_list, c->owner);
        return lh_list;
    }

    if (c->originators == nullptr) {
        std::cout << "trace_back: c->originators is nullptr. error? report bug"
                  << std::endl;
        return lh_list; // should NEVER happen.
    }

    for (Configuration* o : c->originators->list) {
        this->set_transitors_pass_thru_on(*c,
                                          o); // set PASS_THRU ON.
        if (o->LANE_CON == 0u) {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "config on lane: " << o->owner->state_no << "."
                          << o->ruleID << std::endl;
            }
            if (c->owner != o->owner) {
                c->owner->PASS_THRU = 1;
                if constexpr (DEBUG_PHASE_2) {
                    std::cout << "set state " << c->owner->state_no
                              << " PASS_THRU ON" << std::endl;
                }
            }

            lh_list = this->trace_back(c, o, lh_list);
        } else {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "already traced: " << o->owner->state_no << "."
                          << o->ruleID << std::endl;
            }
        }
    }

    return lh_list;
}

/*
 * For use by LR(k) only.
 * Purpose: get LANE_END configurations and add to
 * lane_head_tail_pairs list.
 */
void
trace_back_lrk(const Configuration* c0, Configuration* c)
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
                      << cur_red_config->owner->state_no << "."
                      << cur_red_config->ruleID
                      << ", LANE_END: " << c->owner->state_no << "."
                      << c->ruleID << std::endl;
        }
        // Don't use goal production. As it is the augmented rule, and
        // it generates no context at all.
        if (!(c->owner->state_no == 0 && c->ruleID == 0))
            lane_head_tail_pairs = ConfigPairNode::list_insert(
              lane_head_tail_pairs, cur_red_config, c);

        return;
    }

    // get_originators(c, c);

    if (c->originators == nullptr) {
        std::cout << "trace_back: c->originators is nullptr. error? report bug"
                  << std::endl;
        return; // should NEVER happen.
    }

    for (Configuration* o : c->originators->list) {
        if (o->LANE_CON == 0u) {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "config on lane: " << o->owner->state_no << "."
                          << o->ruleID << std::endl;
            }

            trace_back_lrk(c, o);
        } else {
            if constexpr (DEBUG_PHASE_2) {
                std::cout << "already traced: " << o->owner->state_no << "."
                          << o->ruleID << std::endl;
            }
        }
    }
}

/*
 * For use by LR(k) only.
 * Purpose: clear the LANE_CON flag so as not to
 *          interfere with the lane-tracing of other
 *          LANE_END configurations.
 */
void
trace_back_lrk_clear(const Configuration* c0, Configuration* c)
{
    c->LANE_CON = 0; // set as config on conflicting lane.

    if (c->LANE_END == 1u) {
        return;
    }
    if (c->originators == nullptr) {
        return;
    } // should NEVER happen.

    for (Configuration* o : c->originators->list) {
        if (o->LANE_CON == 1u) {
            trace_back_lrk_clear(c, o);
        }
    }
}

/*
 * Get those states from which conflicting lanes start from, and
 * the associated conflicting contexts for those states.
 *
 * Do this by tracing back each final config from this state.
 */
auto
LaneTracing::get_state_conflict_lane_head(int state_no, laneHead* lh_list) const
  -> laneHead*
{
    const State* s = this->new_states.states_new_array->state_list[state_no];
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
            lh_list = this->trace_back(nullptr, con, lh_list);
        }
    }

    return lh_list;
}

/*
 * Get those states from which conflicting lanes start from,
 * and their associated conflicting contexts.
 */
auto
LaneTracing::get_conflict_lane_head() const -> laneHead*
{
    laneHead* lane_head_list = nullptr;

    for (const int state_no : states_inadequate->states) {
        if (state_no >= 0) {
            if constexpr (DEBUG_GET_LANEHEAD) {
                std::cout << "inadequate state: " << state_no << ". ";
                write_conflicting_context(state_no,
                                          *this->new_states.states_new_array);
            }

            if (this->new_states.states_new_array->rr_count[state_no] > 0) {
                if constexpr (DEBUG_GET_LANEHEAD) {
                    std::cout
                      << " ["
                      << this->new_states.states_new_array->rr_count[state_no]
                      << " r/r conflicts]";
                }
                lane_head_list =
                  this->get_state_conflict_lane_head(state_no, lane_head_list);
            }

            if constexpr (DEBUG_GET_LANEHEAD) {
                std::cout << std::endl;
            }
        }
    }

    if constexpr (DEBUG_PHASE_2) {
        dump_lane_head_list(lane_head_list);
    }

    lane_head_list = remove_pass_through_states(lane_head_list);

    return lane_head_list;
}

void
LaneTracing::phase2()
{
    lane_head_tail_pairs = nullptr; // for LR(k) use only.
    LT_tbl = nullptr;               // initialize the LT_tbl.

    const size_t ct = states_inadequate->count_unresolved;
    if constexpr (DEBUG_PHASE_2) {
        std::cout << "phase 2. unresolved inadequate states: " << ct
                  << std::endl;
    }

    laneHead* lane_head_list = this->get_conflict_lane_head();
    if (lane_head_list == nullptr) {
        std::cout << "laneHeadList is nullptr. return" << std::endl;
        return;
    }

    if constexpr (DEBUG_PHASE_2_GET_TBL) {
        dump_lt_tbl();
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
    IN_EDGE_PUSHING_LANE_TRACING = false;
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

    if (GRAMMAR_AMBIGUOUS) {
        std::cout << "Grammar is ambiguous" << std::endl;
    }
    return lrk_pt_array;
}

/////////////////////////////////////////////////////////////

/* Functions for lane tracing */

static void
dump_stacks(const Grammar& grammar)
{
    std::cout << "__STACK__" << std::endl;
    STACK->dump(grammar);
    std::cout << "__LANE__" << std::endl;
    LANE->dump(grammar);
}

/*
 * Does gamma have a non-null terminal descendent?
 * Input: n - gamma_theads.
 *
 * A null terminal is "", which is an empty string.
 */
auto
test_a(const SymbolNode* n) -> bool
{
    for (; n != nullptr; n = n->next) {
        if (!n->snode->symbol->empty())
            return true;
    }
    return false;
}

/*
 * Is the COMPLETE flag for c on?
 */
constexpr inline auto
test_b(const Configuration* c) -> bool
{
    return c->COMPLETE == FLAG_ON;
}

/*
 * Is the IN_LANE flag for c on?
 */
constexpr inline auto
test_c(const Configuration* c) -> bool
{
    return c->IN_LANE == FLAG_ON;
}

/*
 * Is the IN_LANE flag for c on?
 * Actually is the same as testC.
 */
constexpr inline auto
test_d(const Configuration* c) -> bool
{
    return c->IN_LANE == FLAG_ON;
}

/*
 * Insert snode to the list, no repetition allowed, increasing order.
 * Do it like insertion sort.
 *
 * @parameters:
 *  exist - label whether snode already existed.
 */
static auto
insert_symbol_list_unique(SymbolList list, SymbolTblNode* snode, bool* exist)
  -> SymbolNode*
{
    *exist = false;

    if (list == nullptr)
        return SymbolNode::create(snode);

    SymbolNode *n = list, *n_prev = nullptr;
    for (; n != nullptr; n_prev = n, n = n->next) {
        if (n->snode == snode) {
            *exist = true;
            return list; // existing node.
        }
        if (n->snode->symbol > snode->symbol) {
            SymbolNode* new_node = SymbolNode::create(snode);
            // insert new_snode before n.

            if (n_prev == nullptr) {
                new_node->next = list;
                return new_node;
            }
            new_node->next = n;
            n_prev->next = new_node;
            return list;
        }
    } // end of for.

    // insert as the last node.
    n_prev->next = SymbolNode::create(snode);
    return list;
}

/*
 * Assumption: list != nullptr, c != nullptr.
 */
auto
combine_context_list(SymbolList list, SymbolList new_list) -> SymbolNode*
{

    if (new_list == nullptr)
        return list;
    for (; new_list != nullptr; new_list = new_list->next) {
        bool exist = false;
        list = insert_symbol_list_unique(list, new_list->snode, &exist);
    }
    return list;
}

/*
 * Copy from list to a new symbol list.
 * NOT including nodes that contain empty string.
 */
auto
get_contexts_generated(SymbolList list, bool* null_possible) -> SymbolList
{
    SymbolNode* sn = nullptr;
    *null_possible = false;

    for (; list != nullptr; list = list->next) {
        if (list->snode->symbol->empty()) {
            *null_possible = true;
        } else {
            bool exist = false;
            sn = insert_symbol_list_unique(sn, list->snode, &exist);
        }
    }
    return sn;
}

static void
stack_operation(const Grammar& grammar, int* fail_ct, Configuration* o)
{
    Configuration* tmp = nullptr;

    (*fail_ct)++;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "---------------fail_ct = " << *fail_ct << std::endl;
    }

    switch (*fail_ct) {
        case 1:
            LANE->push(o);
            o->IN_LANE = FLAG_ON;
            TEST_FAILED = FLAG_ON;
            break;
        case 2:
            tmp = LANE->pop();
            LANE->push(LT_MARKER);
            LANE->push(tmp);

            STACK->push(LT_MARKER);
            STACK->push(o);
            break;
        default: // fail_ct >= 3
            STACK->push(o);
            break;
    }

    if constexpr (DEBUG_PHASE_1) {
        dump_stacks(grammar);
    }
}

static void
move_markers(Configuration* o)
{
    int r = 0;
    int ct = static_cast<int>(LANE->count()) - 1;

    for (; ct >= 0; ct--) {
        const Configuration* c = LANE->array[ct];
        if (c == o)
            break;

        if (c == LT_MARKER) {
            LANE->array[ct] = LT_ZERO;
            r++;
        }
    }

    if (TEST_FAILED == FLAG_OFF) {
        for (; r > 0; r--)
            LANE->push(LT_MARKER);
    } else {
        Configuration* c = LANE->pop();
        for (; r > 0; r--)
            LANE->push(LT_MARKER);
        LANE->push(c);
    }
}

/*
 * For each symbol s in CONTEXT_GENERATED, do this:
 *   remove s from CONTEXT_GENERATED;
 *   for each config c in the LANE stack top-down
 *     if (s is in c) { break; }
 *     else { add s to c; }
 *
 * NOTE: here it accesses the LANE stack internal member
 *       array directly.
 */
static void
context_adding(SymbolList context_generated, int cur_config_index)
{
    if constexpr (DEBUG_PHASE_1) {
        std::cout << "CONTEXT ADDING ROUTINE: " << std::endl;
        write_symbol_list(context_generated, "CONTEXT_GENERATED");
    }

    SymbolNode* n = context_generated;

    while (n != nullptr) {
        for (int ct = cur_config_index; ct >= 0; ct--) {
            Configuration* c = LANE->array[ct];
            if (c != LT_ZERO && c != LT_MARKER) {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "add context to " << c->owner->state_no << "."
                              << c->ruleID << std::endl;
                }
                bool exist = false;
                c->context->nContext = insert_symbol_list_unique(
                  c->context->nContext, n->snode, &exist);
                if (exist)
                    break;
                // else, NOT exist, insert was sucessful.
                c->context->context_count++;
            }
        }
        SymbolNode* tmp = n;
        n = n->next;
        free_symbol_node(tmp);
    }
}

static void
context_adding_routine(const Grammar& grammar,
                       SymbolList context_generated,
                       Configuration* o,
                       int cur_config_index,
                       int* fail_ct)
{
    context_adding(context_generated, cur_config_index);

    if (TRACE_FURTHER == FLAG_ON) {

        if constexpr (DEBUG_PHASE_1) {
            std::cout << "__TRACE_FURTHER is ON" << std::endl;
        }

        TRACE_FURTHER = FLAG_OFF;
        stack_operation(grammar, fail_ct, o);
    }

    if constexpr (DEBUG_PHASE_1) {
        if (TRACE_FURTHER != FLAG_ON) {
            std::cout << "__TRACE_FURTHER is OFF" << std::endl;
        }
    }
}

/*
 * Do lane_tracing Phase 1 on a configuration.
 * Pre-assumption: c is a reduction.
 */
void
lane_tracing_reduction(const Grammar& grammar, Configuration* c)
{
    if (c == nullptr)
        return;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "work on reduce config: ";
        stdout_write_config(grammar, c);
    }

    if (c->COMPLETE == 1u) {
        if constexpr (DEBUG_EDGE_PUSHING) {
            std::cout << "c->COMPLETE == 1" << std::endl;
        }
        return; // already evaluated.
    }

    LANE = Stack::create();
    STACK = Stack::create();

    LANE->push(c);
    c->IN_LANE = FLAG_ON;
    TRACE_FURTHER = FLAG_OFF;
    TEST_FAILED = FLAG_OFF;

    if constexpr (DEBUG_EDGE_PUSHING) {
        std::cout << "DO_LOOP:" << std::endl;
    }

    do_loop(grammar);
}

/*
 * Print those configurations where a conflicting lane starts.
 * For debug use only.
 */
static void
dump_lane_start_states(Configuration* o, SymbolList gamma_theads)
{
    if (nullptr == gamma_theads)
        return;

    std::cout << "START " << LANE->array[0]->owner->state_no << "."
              << LANE->array[0]->ruleID << ": " << o->owner->state_no << "."
              << o->ruleID << " generates contexts";
    if (gamma_theads == nullptr) {
        std::cout << ":" << std::endl;
    } else {
        write_symbol_list(gamma_theads, "");
    }
}

void
my_show_t_heads(const SymbolList alpha, const SymbolList theads)
{
    std::cout << "string '";

    const SymbolNode* a = alpha;
    for (a = alpha; a != nullptr; a = a->next) {
        if (a != alpha)
            std::cout << ' ';
        std::cout << a->snode->symbol;
    }

    std::cout << "' has theads: ";

    for (a = theads; a != nullptr; a = a->next) {
        if (a != theads)
            std::cout << ", ";
        std::cout << a->snode->symbol;
    }
    std::cout << std::endl;
}

static void
write_config(Configuration* c)
{
    if (nullptr == c)
        std::cout << "nullptr ";
    else
        std::cout << c->owner->state_no << '.' << c->ruleID;
}

/*
 * Returns true is one of c's originators is o.
 */
static auto
is_on_transitor_chain(Configuration* c, Configuration* o) -> bool
{
    for (const Configuration* orig : c->originators->list) {
        if (orig == o)
            return true;
    }
    return false;
}

/*
 * o - the originator. o does not change in the call stack.
 */
void
LaneTracing::set_transitors_pass_thru_on(const Configuration& cur_config,
                                         Configuration* o) const
{
    // find the next transitor for originator o.
    for (Configuration* c : cur_config.transitors->list) {
        get_originators(this->grammar, c, c);

        if (is_on_transitor_chain(c, o)) {

            if (!this->options.use_combine_compatible_states) {
                if constexpr (DEBUG_PHASE_2_GET_TBL) {
                    std::cout << "B: next entry in entry_table: ("
                              << c->owner->state_no << "." << c->ruleID << ", "
                              << cur_config.owner->state_no << "."
                              << cur_config.ruleID << ")" << std::endl;
                }
                // add another entry to LT_tbl.
                lt_tbl_entry_add(c->owner,
                                 cur_config.owner,
                                 *this->new_states.states_new_array);
            }

            if (c->owner != cur_config.owner && c->owner != o->owner) {
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

    if (o->owner == cur_config.owner) {
        this->get_the_context(o);
    }
}

///!!!!!!!!!!!!!!!!!!!!!!!!!!! START.

/*
 * let s = c->owner, find transitors for c in parent states of s.
 */
static void
get_transitors(const Grammar& grammar, Configuration* c0, Configuration* c)
{
    const auto& l = c->owner->parents_list;
    if (l == nullptr) {
        std::cout << "Error: get_transitors() - L is nullptr" << std::endl;
        return;
    }
    if (l->state_list.size() == 0)
        return;

    if constexpr (DEBUG_GET_ORIGINATOR) {
        std::cout << "get transitor for " << c->owner->state_no << "."
                  << c->ruleID << "." << c->marker << std::endl;
    }

    for (const auto& p : l->state_list) {
        // now get transitor for c.
        for (const auto& t : p->config) {
            if (t->ruleID == c->ruleID && t->marker == c->marker - 1) {
                // is a transitor.
                if constexpr (DEBUG_GET_ORIGINATOR) {
                    std::cout << "++ ++ transitor found for ["
                              << c->owner->state_no << "." << c->ruleID << "."
                              << c->marker << "]: [" << p->state_no << "."
                              << t->ruleID << "." << t->marker << "]"
                              << std::endl;
                }
                insert_transitor_list(c, t);
                get_originators(grammar, c0, t);
            }
        }
    }
}

/*
 * Get originators and transitors for c.
 *
 * @input:
 *   - c0 : the original config to find originators for,
 *   - c  : the current config on path.
 *
 * Transitor of c: a config d in parent state,
 *   c.ruleID = d.ruleID, c.marker = d.marker + 1
 * Originator: a config d in current state,
 *   d.scanned_symbol = c.LHS_symbol
 */
void
get_originators(const Grammar& grammar, Configuration* c0, Configuration* c)
{
    if constexpr (DEBUG_GET_ORIGINATOR) {
        std::cout << "-- current config: " << c->owner->state_no << "."
                  << c->ruleID << "." << c->marker << std::endl;
    }

    if (c->isCoreConfig == 1u) { // core config, search parent states.
        get_transitors(grammar, c0, c);
    } else { // not core config. find originators in current state.
        for (const auto& d : c->owner->config) {
            if (c == d) {
                continue;
            } // ignore c.
            if (d->nMarker == nullptr) {
                continue;
            }
            if (d->nMarker->snode == grammar.rules[c->ruleID]->nLHS->snode) {
                if constexpr (DEBUG_GET_ORIGINATOR) {
                    std::cout << "-- -- originator found for ["
                              << c0->owner->state_no << "." << c0->ruleID << "."
                              << c0->marker << "]: [" << d->owner->state_no
                              << "." << d->ruleID << "." << d->marker << "]"
                              << std::endl;
                }
                insert_originator_list(c0, d, 1);
            }
        }
    }
}

///!!!!!!!!!!!!!!!!!!!!!!!!!!! END.

static void
do_loop(const Grammar& grammar)
{
    Configuration* cur_config = LANE->top();
    const size_t cur_config_index = LANE->count() - 1;

    if (cur_config == LT_MARKER || cur_config == LT_ZERO ||
        cur_config == nullptr) { // should never happen.
        throw std::runtime_error("do_loop cur_config error");
    }

    if constexpr (DEBUG_GET_ORIGINATOR) {
        std::cout << "==call get_originators(cur_config) in do_loop()=="
                  << std::endl;
    }
    get_originators(grammar, cur_config, cur_config);

    int fail_ct = 0;

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "++++++++++TOP of LANE is: ";
        stdout_write_config(grammar, cur_config);
    }

    for (ConfigurationNode* o : cur_config->originators->list) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "________NEXT ORIGINATOR___________________"
                      << std::endl;
        }

        SymbolNode* contexts_generated = nullptr;
        SymbolNode* scanned_symbol = o->nMarker;
        SymbolNode* gamma = nullptr;
        if (scanned_symbol == nullptr)
            gamma = nullptr; // shouldn't happen
        else
            gamma = scanned_symbol->next;

        if constexpr (DEBUG_PHASE_1) {
            stdout_write_config(grammar, o);
            std::cout << "gamma: "
                      << (gamma == nullptr ? "nullptr"
                                           : gamma->snode->symbol->c_str())
                      << std::endl;
        }

        SymbolNode* gamma_theads = nullptr;
        if (gamma != nullptr) { // if not nullptr, get theads.
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "gamma not nullptr, get theads." << std::endl;
            }
            gamma_theads = get_theads(grammar, gamma); // get Heads.
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
            if (is_goal(*o)) {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "GOAL PRODUCTION - generate context: $end"
                              << std::endl;
                }
                gamma_theads = SymbolNode::create(hash_tbl_find(STR_END));
            }
        }

        if constexpr (DEBUG_PHASE_1) {
            dump_lane_start_states(o, gamma_theads);
        }

        if (test_a(gamma_theads)) {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "testA true, get CONTEXTS_GENERATED" << std::endl;
            }
            bool null_possible = false;
            contexts_generated =
              get_contexts_generated(gamma_theads, &null_possible);

            if (IN_EDGE_PUSHING_LANE_TRACING) { /// 12-19-2008.
                EDGE_PUSHING_CONTEXT_GENERATED = contexts_generated;
            }

            if (null_possible) {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "null possible true" << std::endl;
                }
                if (test_b(o)) { // if (o->COMPLETE == FLAG_ON) {
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "COMPLETE ON" << std::endl;
                    }

                    contexts_generated = combine_context_list(
                      contexts_generated, o->context->nContext);
                } else {
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "COMPLETE OFF" << std::endl;
                    }

                    if (test_d(o)) { // if (o->IN_LANE == FLAG_ON) {
                        if constexpr (DEBUG_PHASE_1) {
                            std::cout << "IN_LANE ON" << std::endl;
                            std::cout << "GRAMMAR is AMBIGUOUS" << std::endl;
                        }
                        GRAMMAR_AMBIGUOUS = true;
                        /// exit(1); //////////////// exit prematurely.
                        move_markers(o);
                    } else {
                        if constexpr (DEBUG_PHASE_1) {
                            std::cout << "IN_LANE OFF. set TRACE_FURTHER ON"
                                      << std::endl;
                        }

                        TRACE_FURTHER = FLAG_ON;
                    }
                }
            } else {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "possible null is: false" << std::endl;
                }
                o->LANE_END = 1; // set LANE_END to be true.

                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "Found lane_end: " << o->owner->state_no << "."
                              << o->ruleID << std::endl;
                    std::cout
                      << "conflict config: " << LANE->array[0]->owner->state_no
                      << "." << LANE->array[0]->ruleID << ", lane head state "
                      << o->owner->state_no << ", contexts: ";
                    write_symbol_list(gamma_theads, "contexts");
                }
                // if is in edge_pushing, ignore the context adding routine.
                if (IN_EDGE_PUSHING_LANE_TRACING) {
                    continue;
                }
            }
            // CONTEXT adding routine.
            context_adding_routine(
              grammar, contexts_generated, o, cur_config_index, &fail_ct);

        } else {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "testA false" << std::endl;
            }

            if (test_b(o)) { // testB
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "testB true" << std::endl;
                }

                contexts_generated = combine_context_list(contexts_generated,
                                                          o->context->nContext);
                context_adding_routine(
                  grammar, contexts_generated, o, cur_config_index, &fail_ct);
            } else {
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "testB false" << std::endl;
                }

                if (test_c(o)) { // test_c
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "test_c true" << std::endl;
                    }

                    move_markers(o);

                    contexts_generated = combine_context_list(
                      contexts_generated, o->context->nContext);
                    context_adding_routine(grammar,
                                           contexts_generated,
                                           o,
                                           cur_config_index,
                                           &fail_ct);
                } else {
                    if constexpr (DEBUG_PHASE_1) {
                        std::cout << "test_c false" << std::endl;
                    }

                    stack_operation(grammar, &fail_ct, o);
                }
            }
        } // end of else (test_a false).

    } // end of for.

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "________END OF DO_LOOP____________________" << std::endl;
    }

    if (TEST_FAILED == FLAG_ON) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "__TEST_FAILED is ON__" << std::endl;
        }

        TEST_FAILED = FLAG_OFF;
        do_loop(grammar);
    } else {
        check_lane_top(grammar);
    }
}

static void
pop_lane(const Grammar& grammar)
{

    if constexpr (DEBUG_PHASE_1) {
        std::cout << "POP_LANE" << std::endl;
    }

    LANE->pop();
    check_lane_top(grammar);
}

static void
check_stack_top(const Grammar& grammar)
{
    Configuration* top = STACK->top();

    if (top == LT_MARKER) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "__check_stack_top true" << std::endl;
        }

        STACK->pop();
        pop_lane(grammar);

    } else {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "__check_stack_top false" << std::endl;
        }

        if (top->COMPLETE == FLAG_ON) {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "__top COMPLETE ON" << std::endl;
            }

            STACK->pop();
            check_stack_top(grammar);
        } else {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "__top COMPLETE OFF" << std::endl;
            }

            STACK->pop();
            LANE->push(top);
            top->IN_LANE = FLAG_ON;
            do_loop(grammar);
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

    State* s = c->owner;
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
check_lane_top(const Grammar& grammar)
{
    Configuration* lane_top = LANE->top();

    if (lane_top == LT_MARKER) {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "check lane top true" << std::endl;
        }

        check_stack_top(grammar);
    } else {
        if constexpr (DEBUG_PHASE_1) {
            std::cout << "check lane top false" << std::endl;
        }

        if (lane_top == LT_ZERO) {
            if constexpr (DEBUG_PHASE_1) {
                std::cout << "lane top is ZERO." << std::endl;
            }

            pop_lane(grammar);
        } else {
            lane_top->IN_LANE = FLAG_OFF;
            lane_top->COMPLETE = FLAG_ON;

            /// 12-19-2008.
            if (IN_EDGE_PUSHING_LANE_TRACING == false) {
                propogate_context_sets(grammar, lane_top);
            }
            if (LANE->count() == 1) { // the starting reduction
                                      /////////// END PROGRAMING ///////////////
                if constexpr (DEBUG_PHASE_1) {
                    std::cout << "=====REDUCTION LANE TRACING ENDS====="
                              << std::endl;
                }
            } else {
                pop_lane(grammar);
            }
        }
    }
}
