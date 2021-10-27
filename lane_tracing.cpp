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
#include <iostream>
#include <stdexcept>
#include <string>

#define DEBUG_PHASE_1 0
#define DEBUG_RESOLVE_CONFLICT 0
#define DEBUG_PHASE_2 0
#define DEBUG_GET_LANEHEAD 0
#define DEBUG_PHASE_2_GET_TBL 0
#define DEBUG_PHASE_2_REGENERATE2 0
#define DEBUG_PHASE_2_REGENERATE 0
#define DEBUG_GET_ORIGINATOR 0

#define DEBUG_ORIGIN 0

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
get_originators(Configuration* c0, Configuration* c);
static void
get_transitors(Configuration* c0, Configuration* c);

static void
do_loop();
static void
check_lane_top();
static auto
find_successor_state_no(int state_no, const SymbolTblNode* snode) -> int;
void
my_write_state(State* s);
static void
set_transitors_pass_thru_on(const Configuration* cur_config, Configuration* o);
static auto
inherit_parent_context(State* s, State* parent) -> bool;

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
void
dump_lt_tbl();
auto
lt_tbl_find_entry(int from_state) -> LtTblEntry*;
auto
cluster_trace_new_chain_all(int parent_state, const LtTblEntry* e) -> bool;

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
    // printf("clone c: "); dump_LlistContextSet(c); puts("");

    LlistContextSet* d = llist_context_set_create(c->config);
    LlistContextSet* d_next = d;
    d->ctxt = clone_symbol_list(c->ctxt);

    for (LlistContextSet* c_next = c->next; c_next != nullptr;
         c_next = c_next->next) {
        d_next->next = llist_context_set_create(c_next->config);
        d_next->next->ctxt = clone_symbol_list(c_next->ctxt);
        d_next = d_next->next;
    }

    // printf("result d: "); dump_LlistContextSet(d); puts("");
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
llist_int_add_inc(LlistInt* list, int n) -> LlistInt*
{
    if (list == nullptr) {
        return llist_int_create(n);
    }
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
llist_int_dump(LlistInt* list)
{
    if (list == nullptr)
        return;

    for (LlistInt* x = list; x != nullptr; x = x->next) {
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

/*
 * Find the node in a LlistInt list whose first entry is n2.
 */
auto
llist_int2_find_n2(LlistInt2* list, int n2) -> LlistInt2*
{
    for (LlistInt2* t = list; t != nullptr; t = t->next) {
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

auto
lt_tbl_entry_create(State* from, State* to) -> LtTblEntry*
{
    auto* e = new LtTblEntry;
    e->from_state = from->state_no;

    if (e->from_state !=
        states_new_array->state_list[e->from_state]->state_no) {
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
    e->to_states = llist_int_add_inc(e->to_states, to->state_no);
}

/*
 * Add an entry (from_state, to_state) to the LT_tbl,
 * don't add the (config, context) information here.
 */
static void
lt_tbl_entry_add(State* from, State* to)
{
    if (LT_tbl == nullptr) {
        LT_tbl = lt_tbl_entry_create(from, to);
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
                LT_tbl = lt_tbl_entry_create(from, to);
                LT_tbl->next = e;
            } else { // insert between e_prev and e
                e_prev->next = lt_tbl_entry_create(from, to);
                e_prev->next->next = e;
            }
            return;
        }
    }
    // now is at the end of the table LT_tbl, add to list tail.
    e_prev->next = lt_tbl_entry_create(from, to);
    return;
}

/*
 * Find from state in the LT_tbl.
 * If not found, insert it.
 */
auto
lt_tbl_entry_find_insert(State* from) -> LtTblEntry*
{
    if (nullptr == LT_tbl) { // insert as the first
        LT_tbl = lt_tbl_entry_create(from, nullptr);
        return LT_tbl;
    }

    LtTblEntry *e = LT_tbl, *e_prev = nullptr;
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from->state_no)
            return e;
        if (e->from_state > from->state_no) { // insert here.
            if (e_prev == nullptr) {          // insert as the first.
                LT_tbl = lt_tbl_entry_create(from, nullptr);
                LT_tbl->next = e;
                return LT_tbl;
            } // insert in the middle.
            e_prev->next = lt_tbl_entry_create(from, nullptr);
            e_prev->next->next = e;
            return e_prev->next;
        }
        // else, go on to check the next entry.
    }

    // otherwise, insert at the end.
    e_prev->next = lt_tbl_entry_create(from, nullptr);
    return e_prev->next;
}

/*
 * Find from state in the LT_tbl.
 * Same as LtTblEntry_find_insert() except that this has no insert.
 *
 * There can be at most one entry found.
 */
auto
lt_tbl_entry_find(State* from) -> LtTblEntry*
{
    if (nullptr == LT_tbl) { // insert as the first
        LT_tbl = lt_tbl_entry_create(from, nullptr);
        return LT_tbl;
    }

    LtTblEntry *e = LT_tbl, *e_prev = nullptr;
    for (; e != nullptr; e_prev = e, e = e->next) {
        if (e->from_state == from->state_no)
            return e;
        if (e->from_state > from->state_no) { // insert here.
            if (e_prev == nullptr) {          // insert as the first.
                LT_tbl = lt_tbl_entry_create(from, nullptr);
                LT_tbl->next = e;
                return LT_tbl;
            } // insert in the middle.
            e_prev->next = lt_tbl_entry_create(from, nullptr);
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
        throw std::runtime_error("LlistContextSet_get error: e is nullptr");
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
lt_tbl_entry_add_context(State* from, SymbolList ctxt)
{
    if (ctxt == nullptr)
        return;

    // 1) locate the LtTblEntry for "from" state.
    LtTblEntry* e = lt_tbl_entry_find_insert(from);
    if (nullptr == e) {
        throw std::runtime_error(
          std::string("LtTblEntry_addContext error: state ") +
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
    llist_int_dump(e->to_states);
    puts("");
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
        if (strcmp(a->snode->symbol, b->snode->symbol) < 0) {
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
cluster_dump(LtCluster* c)
{
    LtTblEntry* e = nullptr;

    std::cout << "states: " << std::endl;
    for (LlistInt2* n = c->states; n != nullptr; n = n->next) {
        std::cout << n->n1 << "/" << n->n2 << " [to: ";
        if ((e = lt_tbl_find_entry(n->n1)) != nullptr) {
            for (LlistInt* s = e->to_states; s != nullptr; s = s->next) {
                LlistInt2* m = llist_int2_find_n1(c->states, s->n);
                std::cout << s->n << "/" << ((m == nullptr) ? -1 : m->n2)
                          << " ";
            }
        }
        std::cout << "]" << std::endl;
    }
    std::cout << "context sets: ";
    dump_llist_context_set(c->ctxt_set);

    std::cout << std::endl;
}

static void
all_clusters_dump()
{
    std::cout << "--all_clusters.START--" << std::endl;
    for (LtCluster* c = all_clusters; c != nullptr; c = c->next) {
        cluster_dump(c);
    }
    std::cout << "--END--" << std::endl;
}

/*
 * Return:
 *   the splitted state's no if state_no is in c->states list
 *   -1 otherwise.
 *
 * Note state_no here is the virtual state_no: the one
 * splitted from. So there could be more than one cluster
 * contain it.
 */
auto
cluster_contain_state(const LtCluster* c, int state_no) -> int
{

    if (state_no < 0)
        return -1;
    // printf("cluster_cotain_state(state_no: %d)\n", state_no);
    if (c == nullptr || c->states == nullptr)
        return -1;

    for (const LlistInt2* s = c->states; s != nullptr; s = s->next) {
        if (s->n1 == state_no) {
            // puts("found");
            return s->n2;
        }
    }
    // puts("NOT found");

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
    // printf("cluster_cotain_state(state_no: %d)\n", state_no);
    if (c == nullptr || c->states == nullptr)
        return -1;

    for (LlistInt2* s = c->states; s != nullptr; s = s->next) {
        if (s->n2 == state_no) {
            // puts("found");
            return s->n1;
        }
    }
    // puts("NOT found");

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
    s->size = o->size;
    s->count = o->count;
    s->list = new Configuration*[s->size];

    int ct = s->count;
    for (int i = 0; i < ct; i++) {
        s->list[i] = o->list[i];
    }

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

auto
clone_state(const State* s) -> State*
{
    auto* t = new State;

    t->next = s->next;
    t->config_max_count = s->config_max_count;
    t->config = new Configuration*[t->config_max_count];
    t->config_count = s->config_count;
    t->core_config_count = s->core_config_count;

    int ct = t->config_count;
    for (int i = 0; i < ct; i++) {
        t->config[i] = create_config(-1, 0, 1);
        copy_config_lalr(t->config[i], s->config[i]);
        t->config[i]->owner = t;
    }

    t->state_no = s->state_no;
    t->trans_symbol = create_symbol_node(s->trans_symbol->snode);

    t->successor_max_count = s->successor_max_count;
    t->successor_list = new State*[t->successor_max_count];
    t->successor_count = s->successor_count;
    ct = t->successor_count;
    for (int i = 0; i < ct; i++) {
        t->successor_list[i] = s->successor_list[i];
    }

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

#if DEBUG_PHASE_2_REGENERATE2
    printf("replace successor of state %d: %d replaced by %d: ",
           src_state->state_no,
           s_old->state_no,
           s_new->state_no);
#endif

    for (int i = 0, ct = src_state->successor_count; i < ct; i++) {
        if (src_state->successor_list[i] == s_old) {
            src_state->successor_list[i] = s_new;
#if DEBUG_PHASE_2_REGENERATE2
            puts("done");
#endif
            return;
        }
    }

#if DEBUG_PHASE_2_REGENERATE2
    puts("NOT done");
#endif
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

    // printf("LHTPR: end_config: %d.%d\n",
    //       end_config->owner->state_no, end_config->ruleID);

    for (ConfigPairNode* n = lane_head_tail_pairs; n != nullptr; n = n->next) {
        // n->end->owner == end_config->owner.
        // n->start->owner is a state in cluster c -- should I
        // search as n1 or n2??? I think should be n2, so it
        // covers the situation where n->start->owner is a
        // splitted state.
        if (n->end->owner == s &&
            llist_int2_find_n2(c->states, n->start->owner->state_no) !=
              nullptr) {
            // do replacement for n->end: from that in s to s_copy.
            for (int i = 0; i < s->config_count; i++) {
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
cluster_add_lt_tbl_entry(LtCluster* c,
                         int from_state,
                         LlistContextSet* e_ctxt,
                         int e_parent_state_no,
                         bool copy) -> int
{
    int state_no = from_state;

    if (copy) {
        // make a new state by copying e->from_state,
        // add it to the end of states_new array, and add here.
        State* s_parent = states_new_array->state_list[e_parent_state_no];
        State* s = states_new_array->state_list[state_no];
        State* s_copy = clone_state(s);
        // insert a state to the parsing machine. Defined in y.c.
        insert_state_to_pm(s_copy);

        // Replace src_state's previous succcessor with s_copy.
        replace_successor(s_parent, s_copy, s);

        state_no = s_copy->state_no;

#if DEBUG_PHASE_2_REGENERATE2
        printf("clone state %d to %d\n", s->state_no, state_no);
#endif

        if (USE_LR_K) {
            // For LR(k), replace entry in lane_head_tail_pairs!
            // printf("parrent state: %d\n", e_parent_state_no);
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

#if DEBUG_PHASE_2_REGENERATE2
    if (nullptr != c)
        printf("c: current state: %d/%d\n", c->states->n1, c->states->n2);
#endif

    for (; c != nullptr; c = c->next) {
        if (cluster_contain_state(c, state_no) >= 0) {
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

/*
 * Use macro since it's faster.
 * Called by cluster_trace_new_chain() only.
 */
#define inherit_propagate(state_no, parent_state_no, container, e)             \
    {                                                                          \
        State *s, *s_p;                                                        \
        s = states_new_array->state_list[state_no];                            \
        s_p = states_new_array->state_list[parent_state_no];                   \
        if (true == inherit_parent_context(s, s_p)) {                          \
            get_closure(s); /* needed only if context changed.*/               \
            lt_phase2_propagate_context_change(state_no, container, e);        \
        }                                                                      \
        /*else { printf("::no propagation for state %d\n", state_no); }*/      \
    }

#define clear_inherit_regenerate(state_no, parent_state_no)                    \
    {                                                                          \
        State *s, *s_p;                                                        \
        s = states_new_array->state_list[state_no];                            \
        s_p = states_new_array->state_list[parent_state_no];                   \
        clear_state_context(s);                                                \
        if (true == inherit_parent_context(s, s_p)) {                          \
            get_closure(s); /* needed only if context changed.*/               \
        }                                                                      \
    }

/*
 * Called by phase2_regeneration2() only.
 *
 * Note that if it is state 0, then should add $end
 * to the goal rule before get_closure() !
 */
#define clear_regenerate(state_no)                                             \
    {                                                                          \
        State* s;                                                              \
        s = states_new_array->state_list[state_no];                            \
        clear_state_context(s);                                                \
        if (0 == (state_no)) {                                                 \
            hash_tbl_insert(strEnd);                                           \
            s->config[0]->context->nContext =                                  \
              create_symbol_node(hash_tbl_find(strEnd));                       \
            s->config[0]->context->context_count = 1;                          \
        }                                                                      \
        get_closure(s);                                                        \
    }

/*
 * Function to propagate context change until a state where
 * there is no more context change.
 *
 * Here the children states are those defined in the
 * LtTblEntry's to_states list.
 */
void
lt_phase2_propagate_context_change(int state_no, LtCluster* c, LtTblEntry* e)
{
    if (e == nullptr) {
        return;
    }
    // find all the to_states, do recursively until no context change.
    // in cluster c, state_no has a corresponding true state_no,
    // so are its to_states which can be found from e.

#if DEBUG_PHASE_2_REGENERATE2
    printf("lt_p2_propagateContextChange from state %d\n", state_no);
#endif

    for (const LlistInt* t = e->to_states; t != nullptr; t = t->next) {
        LtTblEntry* f = lt_tbl_find_entry(t->n);
        if (f != nullptr) {
            // need to replace t->n with the true state_no in cluster c.
            const LlistInt2* t2 = llist_int2_find_n1(c->states, t->n);
            if (t2 == nullptr) {
#if DEBUG_PHASE_2_REGENERATE2
                printf("--to state %d, not found in cluster.\n", t->n);
                // cluster_dump(c);
#endif
                continue; // if not found, just ignore this to state.
            }
#if DEBUG_PHASE_2_REGENERATE2
            printf("--to state: %d, actual: %d\n", t->n, t2->n2);
#endif
            inherit_propagate(t2->n2, state_no, c, f);
        }
    }
}

auto
inherit_parent_context(State* s, State* parent) -> bool
{
    if (s == nullptr || parent == nullptr)
        return false;

#if DEBUG_PHASE_2_REGENERATE2
    printf("state %d: to inherit context from parent state %d\n",
           s->state_no,
           parent->state_no);

    if (0 && s->state_no == 39) {
        printf("before: \n");
        my_writeState(s);
        my_writeState(parent);
    }
#endif

    bool is_changed = false;
    const SymbolTblNode* trans_symbol = s->trans_symbol->snode;
    const int ct = parent->config_count;
    for (int i = 0; i < ct; i++) {
        Configuration* c_p = parent->config[i];
        if (is_final_configuration(c_p))
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

        if (true == combine_context(c->context, c_p->context)) {
            is_changed = true;
        }
    }

#if DEBUG_PHASE_2_REGENERATE2
    if (0 && s->state_no == 39) {
        printf("after: \n");
        my_writeState(s);
    }
#endif

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

    const int ct = s->config_count;
    for (int i = 0; i < ct; i++) {
        clear_context(s->config[i]->context); // defined in y.c.
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
cluster_trace_new_chain(int parent_state_no, int state_no) -> bool
{
    bool is_new_chain = true;
    LtCluster* c = new_cluster;

#if DEBUG_PHASE_2_REGENERATE2
    printf("cluster: %d. next state on chain: %d\n", c, state_no);
#endif

    // e will be used no matter what happen.
    LtTblEntry* e = lt_tbl_find_entry(state_no);
    if (nullptr == e) { // Is this possible? YES IT IS.
#if DEBUG_PHASE_2_REGENERATE2
        printf("END of chain - state_no: %d\n", state_no);
#endif
    }
    LlistContextSet* e_ctxt = (e == nullptr) ? nullptr : e->ctxt_set;

    // state in in cluster c.
    int ret_state = cluster_contain_state(c, state_no);
    if (ret_state >= 0) {
#if DEBUG_PHASE_2_REGENERATE2
        printf("=>2. state %d: inherit context from state %d & propagate\n",
               ret_state,
               parent_state_no);
#endif
        inherit_propagate(ret_state, parent_state_no, c, e);

        replace_successor(states_new_array->state_list[parent_state_no],
                          states_new_array->state_list[ret_state],
                          states_new_array->state_list[state_no]);
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

#if DEBUG_PHASE_2_REGENERATE2
        printf("2. state %d is in a cluster %d\n", state_no, container);
#endif

        if (container->pairwise_disjoint) {
            LlistContextSet* x = llist_context_set_clone(c->ctxt_set);
            x = llist_context_set_merge_chain(x, container->ctxt_set);
            bool is_pairwise_disjoint = pairwise_disjoint(x);
            llist_context_set_destroy(x);

            if (is_pairwise_disjoint) {
#if DEBUG_PHASE_2_REGENERATE2
                puts("3. combine 2 clusters result is pairwise disjoint");
#endif
                combine_cluster(c, container); // container is the result.
                is_new_chain = false;          // not a new chain.

                // This is used so cluster_contain_state() at the beginning
                // of this function can return correct value once c is changed.
                c = new_cluster = container;

#if DEBUG_PHASE_2_REGENERATE2
                printf(
                  "=>3. state %d: inherit context from state %d, propagate\n",
                  state_no,
                  parent_state_no);
#endif
                inherit_propagate(state_no, parent_state_no, container, e);

                container_not_found = false;
                break;
            }
        }
        // else, NOT pairwise disjoint, continue to find the next match.
    } // end of while.

    if (container_not_found) {
        if (first_container == nullptr) { // always add.
#if DEBUG_PHASE_2_REGENERATE2
            printf("4. state %d is NOT in any cluster yet\n", state_no);
#endif
            ret_state = cluster_add_lt_tbl_entry(
              c, state_no, e_ctxt, parent_state_no, false);

            if (c->pairwise_disjoint &&
                pairwise_disjoint(c->ctxt_set) == false) {
                c->pairwise_disjoint = false;
                all_pairwise_disjoint = false;
            }
            if (nullptr != e)
                e->processed = true;

#if DEBUG_PHASE_2_REGENERATE2
            printf("=>4. state %d: clear, inherit context from state %d, "
                   "regenerate\n",
                   state_no,
                   parent_state_no);
#endif
            clear_inherit_regenerate(state_no, parent_state_no);

        } else {
            // e is already in another cluster, e.g., first_container;
            // but combined context are NOT pairwise-disjoint.
            // so make a copy e' of e and add it to c.
            ret_state = cluster_add_lt_tbl_entry(
              c, state_no, e_ctxt, parent_state_no, true);
            if ((c->pairwise_disjoint = pairwise_disjoint(c->ctxt_set)) ==
                false) {
                all_pairwise_disjoint = false;
            }
#if DEBUG_PHASE_2_REGENERATE2
            printf("=>5. state %d: clear, inherit context from state %d, "
                   "regenerate\n",
                   ret_state,
                   parent_state_no);
#endif
            clear_inherit_regenerate(ret_state, parent_state_no);
        }

        if (nullptr != e) { // recursively trace the chain.
            is_new_chain = cluster_trace_new_chain_all(ret_state, e);
        }
    }

    return is_new_chain;
}

/*
 * parent_state: state_no of the parent state.
 */
auto
cluster_trace_new_chain_all(int parent_state, const LtTblEntry* e) -> bool
{
    bool is_new_chain = true;

    // recursively trace the chain.
    for (const LlistInt* s = e->to_states; s != nullptr; s = s->next) {
        if (false == cluster_trace_new_chain(parent_state, s->n)) {
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
phase2_regeneration2()
{
    bool is_new_chain = true;

    all_clusters = all_clusters_tail = nullptr; // initialize all_clusters.
    all_pairwise_disjoint = true;

    for (LtTblEntry* e = LT_tbl; e != nullptr; e = e->next) {
        if (e->processed)
            continue;

        LtTblEntry* x = e; // start state of another chain/cluster of states.
        new_cluster = cluster_create(x);

#if DEBUG_PHASE_2_REGENERATE2
        printf("== chain head state: %d\n", e->from_state);
        printf("=>1. clear and regenerate context for state %d\n",
               x->from_state);
#endif
        clear_regenerate(x->from_state);

        x->processed = true;

        is_new_chain = cluster_trace_new_chain_all(x->from_state, x);

        // add new_cluster to the all_clusters list.
        if (true == is_new_chain) {
            all_clusters_add(new_cluster);
        }
    }

#if DEBUG_PHASE_2_REGENERATE2
    all_clusters_dump(); // dump if is new chain.
#endif

    // if the parsing machine is expanded, update the parsing table.
    if (states_new->state_count > ParsingTblRows) {
        update_parsing_table_lr0();
    }

    std::cout << "LT-LTT end: states count: " << states_new->state_count
              << std::endl;
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
        if (strcmp(sn->snode->symbol, n->symbol) > 0) {
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
        YYERR_EXIT("lane_tracing.c addLaneHead error: s is nullptr");
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
 * Used when USE_LANE_TRACING  only.
 * Called in y.c function create_config().
 */
auto
create_originator_list() -> OriginatorList*
{
    auto* o = new OriginatorList;
    o->size = OriginatorList_Len_Init;
    o->count = 0;
    o->list = new Configuration*[o->size];
    return o;
}

void
expand_originator_list(OriginatorList* o)
{
    if (nullptr == o || o->count < o->size)
        return;

    o->size *= 2;
    HYY_EXPAND(&o->list, o->size);
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

    if (o->count == o->size) {
        expand_originator_list(o);
    }

    for (int i = 0, ct = o->count; i < ct; i++) {
        if (originator == o->list[i])
            return false; // already exists.
    }

    o->list[o->count++] = originator;
    c->ORIGINATOR_CHANGED = true;
    return true;
}

void
write_originator_list(OriginatorList* o)
{
    if (o == nullptr)
        return;

    for (int i = 0; i < o->count; i++) {
        const Configuration* c = o->list[i];
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

    if (o->count == o->size) {
        expand_originator_list(o);
    }

    const int ct = o->count;
    for (int i = 0; i < ct; i++) {
        if (transitor == o->list[i])
            return false; // already exists.
    }

    o->list[o->count++] = transitor;
    return true;
}

/* for inadequate states */

auto
create_state_no_array() -> StateNoArray*
{
    auto* sa = new StateNoArray;
    sa->size = 2; // start value.
    sa->count = 0;
    sa->states = new int[sa->size];

    return sa;
}

static void
expand_state_no_array(StateNoArray* sa)
{
    sa->size *= 2;
    HYY_EXPAND(&sa->states, sa->size);
}

/*
 * If state_no is not in the inadequate states list, add it.
 */
void
add_state_no_array(StateNoArray* sa, int state_no)
{
    if (sa->count == sa->size) {
        expand_state_no_array(sa);
    }

    const int ct = sa->count;
    for (int i = 0; i < ct; i++) {
        if (sa->states[i] == state_no) {
            return;
        } // exists.
    }
    // printf("state %d is added to list. \n", state_no);
    sa->states[sa->count++] = state_no;
}

void
dump_state_no_array(const StateNoArray* sa)
{
    if (sa == nullptr)
        return;

    const int ct = sa->count;
    for (int i = 0; i < ct; i++) {
        std::cout << sa->states[i] << " ";
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

void
my_write_context(const Context* c)
{
    std::cout << " {";

    const SymbolNode* s = c->nContext;
    if (s != nullptr) {
        std::cout << s->snode->symbol;
        while ((s = s->next) != nullptr) {
            std::cout << ", " << s->snode->symbol;
        }
    }

    if (USE_LR_K) {
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
        // printf("writeProduction warning: p is nullptr\n");
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

void
my_write_config_originators(const Configuration* c);

void
stdout_write_config(const Configuration* c)
{
    if (c == nullptr) {
        return;
    }
    std::cout << "config (" << c->owner->state_no << "." << c->ruleID << ") : ";
    my_write_production(grammar.rules[c->ruleID], c->marker);
    my_write_context(c->context);
    std::cout << "[COMPLETE: " << c->COMPLETE << "]"
              << "[IN_LANE: " << c->IN_LANE << "]"
              << "[LANE_END: " << c->LANE_END << "]"
              << "[LANE_CON: " << c->LANE_CON << "]" << std::endl;
}
void
my_write_state(const State* s)
{
    std::cout << "state_no: " << s->state_no
              << " (core: " << s->core_config_count << ")" << std::endl;

    const int ct = s->config_count;
    for (int i = 0; i < ct; i++) {
        stdout_write_config(s->config[i]);
    }
}

/*
 * For debug use.
 */
void
write_config_originators(const Configuration& c)
{
    const int ct = c.originators->count;
    if (ct == 0)
        return;

    yyprintf("      %d originator%s: \n", ct, (ct > 1) ? "s" : "");
    for (int i = 0; i < ct; i++) {
        const Configuration* o = c.originators->list[i];
        yyprintf("      originator (%d.%d.%d) \n",
                 o->owner->state_no,
                 o->ruleID,
                 o->marker);
    }
}

void
write_config_transitors(const Configuration& c)
{
    const int ct = c.transitors->count;
    if (ct == 0)
        return;

    yyprintf("      %d transitor%s: \n", ct, (ct > 1) ? "s" : "");
    for (int i = 0; i < ct; i++) {
        const Configuration* o = c.transitors->list[i];
        yyprintf("      transitor (%d.%d.%d) \n",
                 o->owner->state_no,
                 o->ruleID,
                 o->marker);
    }
}

void
my_write_config_originators(const Configuration* c)
{
    const int ct = c->originators->count;
    std::cout << "config has " << ct << " originators: " << std::endl;
    for (int i = 0; i < ct; i++) {
        const Configuration* o = c->originators->list[i];
        std::cout << "      originator (" << o->owner->state_no << "."
                  << o->ruleID << ") " << std::endl;
    }
}

/*
 * get context for reduce productions in conflict states.
 */
static void
get_inadequate_state_reduce_config_context(const State* s)
{
    const int ct = s->config_count;
#if DEBUG_PHASE_1
    printf("state %d [%d configurations, trans_symbol: %s]: \n",
           s->state_no,
           ct,
           s->trans_symbol->snode->symbol);
#endif

    for (int i = 0; i < ct; i++) {
        Configuration* c = s->config[i];
        if (is_final_configuration(c)) {
            lane_tracing_reduction(c);
#if DEBUG_PHASE_1
            puts(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
#endif
        }
    }
}

static void
lane_tracing_phase1()
{
    const int ct = states_inadequate->count;

#if DEBUG_PHASE_1
    printf("%d inadequate states: \n", ct);
#endif

    for (int i = 0; i < ct; i++) {
        const int state_no = states_inadequate->states[i];
        const State* s = states_new_array->state_list[state_no];
        get_inadequate_state_reduce_config_context(s);
    }

    // next will check resolved conflicts.
}

/*
 * called by update_action_table().
 */
static auto
find_successor_state_no(int state_no, const SymbolTblNode* snode) -> int
{
    const State* state = states_new_array->state_list[state_no];

    for (int i = state->successor_count - 1; i >= 0; i--) {
        const State* successor = state->successor_list[i];
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
#define clearStateTerminalTransitions(state_no)                                \
    {                                                                          \
        memset((void*)(ParsingTable + state_no * ParsingTblCols),              \
               0,                                                              \
               (grammar.terminal_count + 1) * 4);                              \
    }

/*
 * clear all the conflicts from states_new_array->conflict_list.
 */
static void
clear_state_conflicts(int state_no)
{
    Conflict* list = states_new_array->conflict_list[state_no];

    while (list != nullptr) {
        Conflict* tmp = list->next;
        if (list->s > 0) {
            states_new_array->rs_count[state_no]--;
            rs_count--;
        } else {
            states_new_array->rr_count[state_no]--;
            rr_count--;
        }
        Conflict::destroy_list(list);
        list = tmp;
    }

    states_new_array->conflict_list[state_no] = nullptr;
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
static void
resolve_lalr1_conflicts()
{
#if DEBUG_RESOLVE_CONFLICT
    printParsingTable();
#endif

    states_inadequate->count_unresolved = states_inadequate->count;

    const int ct = states_inadequate->count;
    for (int i = 0; i < ct; i++) {
        const int state_no = states_inadequate->states[i];
        if (state_no < 0)
            continue; // should never happen.

        const State* state = states_new_array->state_list[state_no];

        // clear the parsing table row for S where lookahead is terminal.
#if DEBUG_RESOLVE_CONFLICT
        printf("-----clear state[%d] = %d. len=%d\n", i, state_no, ct);
#endif
        clearStateTerminalTransitions(state_no);

        // clear all the conflicts associated with S.
        clear_state_conflicts(state_no);

        // re-insert actions into parsing table for this state.
        for (int j = state->config_count - 1; j >= 0; j--) {
            const Configuration* config = state->config[j];
            if (is_final_configuration(config) && config->context != nullptr) {
                // insert reduce
                for (const SymbolNode* contxt = config->context->nContext;
                     contxt != nullptr;
                     contxt = contxt->next) {
                    insert_action(
                      contxt->snode, state_no, (-1) * config->ruleID);
                }
            } else if (config->nMarker != nullptr &&
                       is_terminal(config->nMarker->snode)) {
                // insert shift.
                insert_action(
                  config->nMarker->snode,
                  state_no,
                  find_successor_state_no(state_no, config->nMarker->snode));
            }
        }
        const Conflict* c = states_new_array->conflict_list[state_no];
        if (c == nullptr) {
            states_inadequate->states[i] = -1;
            states_inadequate->count_unresolved--;
        } else {
            // do nothing.
        }
    }

#if DEBUG_RESOLVE_CONFLICT
    printParsingTable();
#endif
}

void
write_conflicting_context(int state_no)
{
    const Conflict* c = states_new_array->conflict_list[state_no];
    if (c == nullptr)
        return;

    std::cout << "conflicting contexts: " << c->lookahead->symbol;

    for (c = c->next; c != nullptr; c = c->next) {
        std::cout << ", " << c->lookahead->symbol;
    }
}

static auto
remove_pass_through_states(laneHead* lh_list) -> laneHead*
{
    laneHead *h = lh_list, *h_prev = nullptr;
    for (; h != nullptr;) {
#if DEBUG_PHASE_2
        printf("state %d, PASS_THRU: %d\n", h->s->state_no, h->s->PASS_THRU);
#endif
        if (h->s->PASS_THRU == 1) {
#if DEBUG_PHASE_2
            puts("Is pass thru state! Remove it from laneHead list");
#endif
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

static void
gpm(State* new_state)
{
    if (DEBUG_GEN_PARSING_MACHINE) {
        std::cout << std::endl
                  << std::endl
                  << "--generate parsing machine--" << std::endl
                  << "states_new count: " << states_new->state_count
                  << std::endl;
    }

    while (new_state != nullptr) {
        if (DEBUG_GEN_PARSING_MACHINE) {
            yyprintf("%d states, current state is %d\n",
                     states_new->state_count,
                     new_state->state_no);
        }

        get_closure(new_state); // get closure of this state.

        // get successor states and add them to states_new.
        transition(new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    ParsingTblRows = states_new->state_count;
    n_state_opt1 = states_new->state_count;

    std::cout << "LT-PGM end: states count: " << states_new->state_count
              << std::endl;
}

static void
write_state_no_array(const StateNoArray* a, char* name)
{
    if (name != nullptr) {
        std::cout << name << ": ";
    }
    const int ct = a->count;
    for (int i = 0; i < ct; i++) {
        std::cout << a->states[i] << " ";
    }
    std::cout << std::endl;
}

/*
 * Adapted from transition() in y.c.
 */
static auto
get_state_successors(const State* s) -> StateCollection*
{
    StateCollection* coll = create_state_collection();

    for (int i = 0; i < s->config_count; i++) {
        Configuration* c = s->config[i];
        if (is_final_configuration(c)) {
            // do nothing.
        } else { // do transit operation.
            SymbolTblNode* scanned_symbol = get_scanned_symbol(c);
            if (strlen(scanned_symbol->symbol) == 0) { // empty reduction.
                continue;
            }
            State* new_state =
              find_state_for_scanned_symbol(coll, scanned_symbol);
            if (new_state == nullptr) {
                new_state = create_state();
                // record which symbol this state is a successor by.
                new_state->trans_symbol = create_symbol_node(scanned_symbol);
                coll->add_state2(new_state);
            }
            // create a new core config for new_state.
            Configuration* new_config = create_config(-1, 0, 1);

            new_config->owner = new_state;
            copy_config(new_config, c);
            new_config->isCoreConfig = 1;
            new_config->marker++;
            if (new_config->nMarker != nullptr)
                new_config->nMarker = new_config->nMarker->next;

            add_core_config2_state(new_state, new_config);
        }
    } // end for

    return coll;
}

/*
 * Given the trans_symbol, find the index of the
 * successor of s that has this trans_symbol.
 */
static auto
get_successor_index(const State* s, const SymbolTblNode* trans_symbol) -> int
{
    const int len = s->successor_count;
    for (int i = 0; i < len; i++) {
        if (s->successor_list[i]->trans_symbol->snode == trans_symbol) {
            return i; // s->successor_list[i];
        }
    }
    // the following code should never be reached.
    std::cout << "lane_tracing.c getCorrespondingSuccessor() error:"
              << std::endl
              << " nullptr found on state " << s->state_no << ", trans_symbol "
              << trans_symbol->symbol << std::endl;
    return -1; // this should NEVER happen
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
static auto
add_split_state(State* y, const State* s, int successor_index) -> bool
{
    int is_compatible = 0;
    State* os = search_state_hash_tbl(y, &is_compatible); // same or compatible.

    if (os == nullptr) { // No existing state found. Add Y as a new state.
        y->state_no = states_new->state_count;
#if DEBUG_PHASE_2_REGENERATE
        // puts("split - new state");
        printf("split - add new state %d\n", Y->state_no);
#endif
        states_new->add_state2(y);
        add_state_to_state_array(*states_new_array, y);
        // update shift action.
        update_action(
          get_col(y->trans_symbol->snode), s->state_no, y->state_no);
        // update the Y0 successor link of S to Y.
        s->successor_list[successor_index] = y;
        if (states_new->state_count >= PARSING_TABLE_SIZE) {
            expand_parsing_table();
        }
        return true;
    } // same or compatible with an existing state.
    // puts("split - old state");
    update_action(get_col(os->trans_symbol->snode), s->state_no, os->state_no);
    s->successor_list[successor_index] = os;
    State::destroy_state(y);
    return false;
}

static auto
add_unique_queue(State* s, laneHead* lh_list) -> laneHead*
{
    laneHead* h = lh_list;
    if (h == nullptr)
        return create_lane_head(s, nullptr);

    if (h->s == s) { // exists, is the first node.
#if DEBUG_PHASE_2_REGENERATE
        printf("state %d already on laneHead queue\n", s->state_no);
#endif
        return lh_list;
    }

    for (; h->next != nullptr; h = h->next) {
        if (h->next->s == s) { // exists already.
#if DEBUG_PHASE_2_REGENERATE
            printf("state %d already on laneHead queue\n", s->state_no);
#endif
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
regenerate_state_context(State* s, State* t)
{
    if (t->core_config_count != s->core_config_count) {
        YYERR_EXIT("regenerate error: inequal config_count");
    }

    // clear the context of S.
    int ct = s->config_count;
    for (int i = 0; i < ct; i++) { // -> if final config, remove p.t. entry.
        Context* c = s->config[i]->context;
        free_symbol_node_list(c->nContext);
        c->nContext = nullptr;
        c->context_count = 0;
    }

    // copy the context from T to S.
    ct = t->core_config_count;
    for (int i = 0; i < ct; i++) {
        copy_context(s->config[i]->context, t->config[i]->context);
    }
}

/*
 * Combine the contexts from T to S. No propagation here.
 */
static auto
combine_state_context(State* s_dest, State* s_src) -> bool
{
    bool is_changed = false;
    for (int i = 0; i < s_dest->core_config_count; i++) {
        if (combine_context(s_dest->config[i]->context,
                            s_src->config[i]->context)) {
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
static void
update_state_reduce_action(State* s)
{
    int state_dest = 0;
    char action = 0;

    const int ct = s->config_count;
    for (int i = 0; i < ct; i++) {
        // update reduce action for final/empty production.
        Configuration* c = s->config[i];
        if (is_final_configuration(c) ||
            strlen(get_scanned_symbol(c)->symbol) == 0) {
            const SymbolNode* lookahead = c->context->nContext;
            for (; lookahead != nullptr; lookahead = lookahead->next) {
                get_action(lookahead->snode->type,
                           get_col(lookahead->snode),
                           s->state_no,
                           &action,
                           &state_dest);
                if (state_dest != c->ruleID) {
                    if (action == 0 || action == 'r') {
                        update_action(get_col(lookahead->snode),
                                      s->state_no,
                                      (-1) * c->ruleID);
                    } else { // else, is "s" or "acc".
                        insert_action(
                          lookahead->snode, s->state_no, (-1) * c->ruleID);
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
static void
phase2_regeneration(laneHead* lh_list)
{
    laneHead* h = lh_list;
    State* new_state = nullptr;
    bool exists = false;

    // 1) handle the head states and PASS_THRU states.
    for (; h != nullptr; h = h->next) {
        State* s = h->s;
        get_closure(s); // get_closure() is defined in y.c
        update_state_reduce_action(s);

#if DEBUG_PHASE_2_REGENERATE
        printf("\n==regenerate state %d\n", S->state_no);
        my_writeState(S);
#endif
        const StateCollection* coll = get_state_successors(s);

        for (State* y = coll->states_head; y != nullptr; y = y->next) {
            int successor_index =
              get_successor_index(s, y->trans_symbol->snode);
            if (successor_index == -1)
                continue; // should NEVER happen.
            State* y0 = s->successor_list[successor_index];

            if (y0->PASS_THRU == 0u) {
#if DEBUG_PHASE_2_REGENERATE
                printf("state %d PASS_THRU == 0 - NOT on lane\n", Y0->state_no);
#endif
                continue; // NOT on 'conflicting' lane.
            }
#if DEBUG_PHASE_2_REGENERATE
            printf("state %d PASS_THRU == 1 - on lane\n", Y0->state_no);
            printf("%s successor isOnConflictingLane\n",
                   Y->trans_symbol->snode->symbol);
#endif

            if (y0->REGENERATED == 0u) { // is original.
#if DEBUG_PHASE_2_REGENERATE
                printf("replace - replace old state %d\n", Y0->state_no);
#endif
                // replace the context of Y0 with those of Y.
                regenerate_state_context(y0, y);
                y0->REGENERATED = 1;
                lh_list = add_unique_queue(y0, lh_list);
            } else { // is regenerated state.
                exists = is_compatible_states(y0, y);

                if (exists) {
#if DEBUG_PHASE_2_REGENERATE
                    printf("combine to compatible state %d\n", Y0->state_no);
#endif
                    combine_state_context(y0, y);
                    lh_list = add_unique_queue(y0, lh_list);
                } else {
                    if (add_split_state(y, s, successor_index)) {
                        if (new_state == nullptr) {
                            new_state = y;
                        }
#if DEBUG_PHASE_2_REGENERATE
                        puts("split - new state added");
#endif
                    }
                }
            }
        } // end of for.
    }     // end of for.

    // 2) handle the new added states.
    //    If there are any new split states, do GPM on them.
    if (new_state != nullptr) {
#if DEBUG_PHASE_2_REGENERATE
        puts("GRM(new_state) now.");
#endif
        gpm(new_state);
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

static auto
get_the_context(const Configuration* o) -> SymbolNode*
{
    if (o == nullptr)
        return nullptr;

    const SymbolNode* scanned_symbol = o->nMarker;
    if (scanned_symbol == nullptr)
        return nullptr;

    SymbolNode* gamma = scanned_symbol->next;
    SymbolNode* gamma_theads =
      get_theads(gamma); // Note nullptr is a valid terminal.

    // note that "" will be the first node in the INC list,
    // so it's not so inefficient.
    int exist = 0;
    gamma_theads =
      remove_from_symbol_list(gamma_theads, hash_tbl_find(""), &exist);

#if DEBUG_PHASE_2_GET_TBL
    if (gamma_theads != nullptr) {
        printf("C: Add context to entry state %d: ", o->owner->state_no);
        printf(
          "[%d.%d] ", cur_red_config->owner->state_no, cur_red_config->ruleID);
        writeTheSymbolList(gamma_theads);
        printf("\n");
    }
#endif

    lt_tbl_entry_add_context(o->owner, gamma_theads); // add context.

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
trace_back(const Configuration* c0, Configuration* c, laneHead* lh_list)
  -> laneHead*
{
    c->LANE_CON = 1; // set as config on conflicting lane.

    if (c->LANE_END == 1) {
#if DEBUG_PHASE_2
        printf("END config FOUND: %d.%d\n\n", c->owner->state_no, c->ruleID);
        printf("=ADD another head state: %d\n", c->owner->state_no);
#endif

#if DEBUG_PHASE_2_GET_TBL
        printf("D: END of config lane FOUND: %d.%d \n",
               c->owner->state_no,
               c->ruleID);
#endif

        if (USE_LR_K) { // for LR(k) use only.
#if DEBUG_PHASE_2_GET_TBL
            printf("config_red_config: %d.%d, LANE_END: %d.%d\n",
                   cur_red_config->owner->state_no,
                   cur_red_config->ruleID,
                   c->owner->state_no,
                   c->ruleID);
#endif
            // Don't use goal production. As it is the augmented rule, and
            // it generates no context at all.
            if (!(c->owner->state_no == 0 && c->ruleID == 0))
                lane_head_tail_pairs = config_pair_list_insert(
                  lane_head_tail_pairs, cur_red_config, c);
        }

        lh_list = add_lane_head_list(lh_list, c->owner);
        return lh_list;
    }

    if (c->originators == nullptr) {
        puts("trace_back: c->originators is nullptr. error? report bug");
        return lh_list; // should NEVER happen.
    }

    const int len = c->originators->count;
    for (int i = 0; i < len; i++) {
        Configuration* o = c->originators->list[i];

        set_transitors_pass_thru_on(c, o); // set PASS_THRU ON.
        if (o->LANE_CON == 0) {
#if DEBUG_PHASE_2
            printf("config on lane: %d.%d\n", o->owner->state_no, o->ruleID);
#endif
            if (c->owner != o->owner) {
                c->owner->PASS_THRU = 1;
#if DEBUG_PHASE_2
                printf("set state %d PASS_THRU ON\n", c->owner->state_no);
#endif
            }

            lh_list = trace_back(c, o, lh_list);
        } else {
#if DEBUG_PHASE_2
            printf("already traced: %d.%d\n", o->owner->state_no, o->ruleID);
#endif
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

    if (c->LANE_END == 1) {
#if DEBUG_PHASE_2
        printf("END config FOUND: %d.%d\n\n", c->owner->state_no, c->ruleID);
        printf("=ADD another head state: %d\n", c->owner->state_no);
#endif

#if DEBUG_PHASE_2_GET_TBL
        printf("config_red_config: %d.%d, LANE_END: %d.%d\n",
               cur_red_config->owner->state_no,
               cur_red_config->ruleID,
               c->owner->state_no,
               c->ruleID);
#endif
        // Don't use goal production. As it is the augmented rule, and
        // it generates no context at all.
        if (!(c->owner->state_no == 0 && c->ruleID == 0))
            lane_head_tail_pairs =
              config_pair_list_insert(lane_head_tail_pairs, cur_red_config, c);

        return;
    }

    // get_originators(c, c);

    if (c->originators == nullptr) {
        puts("trace_back: c->originators is nullptr. error? report bug");
        return; // should NEVER happen.
    }

    const int len = c->originators->count;
    for (int i = 0; i < len; i++) {
        Configuration* o = c->originators->list[i];

        if (o->LANE_CON == 0) {
#if DEBUG_PHASE_2
            printf("config on lane: %d.%d\n", o->owner->state_no, o->ruleID);
#endif

            trace_back_lrk(c, o);
        } else {
#if DEBUG_PHASE_2
            printf("already traced: %d.%d\n", o->owner->state_no, o->ruleID);
#endif
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

    if (c->LANE_END == 1) {
        return;
    }
    if (c->originators == nullptr) {
        return;
    } // should NEVER happen.

    const int len = c->originators->count;
    for (int i = 0; i < len; i++) {
        Configuration* o = c->originators->list[i];

        if (o->LANE_CON == 1) {
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
static auto
get_state_conflict_lane_head(int state_no, laneHead* lh_list) -> laneHead*
{
    const State* s = states_new_array->state_list[state_no];
    const int len = s->config_count;
    for (int i = 0; i < len; i++) {
        Configuration* con = s->config[i];
        if (is_final_configuration(con)) {
#if DEBUG_PHASE_2
            printf("\nfinal config: %d.%d\n", state_no, con->ruleID);
#endif

#if DEBUG_PHASE_2_GET_TBL
            printf("\nA: final config: %d.%d\n", state_no, con->ruleID);
#endif
            cur_red_config = con;
            lh_list = trace_back(nullptr, con, lh_list);
        }
    }

    return lh_list;
}

/*
 * Get those states from which conflicting lanes start from,
 * and their associated conflicting contexts.
 */
static auto
get_conflict_lane_head() -> laneHead*
{
    laneHead* lane_head_list = nullptr;

    for (int i = 0; i < states_inadequate->count; i++) {
        const int state_no = states_inadequate->states[i];
        if (state_no >= 0) {
#if DEBUG_GET_LANEHEAD
            printf("inadequate state: %d. ", state_no);
            writeConflictingContext(state_no);
#endif

            if (states_new_array->rr_count[state_no] > 0) {
#if DEBUG_GET_LANEHEAD
                printf(" [%d r/r conflicts]",
                       states_new_array->rr_count[state_no]);
#endif
                lane_head_list =
                  get_state_conflict_lane_head(state_no, lane_head_list);
            }

#if DEBUG_GET_LANEHEAD
            puts("");
#endif
        }
    }

#if DEBUG_PHASE_2
    dumpLaneHeadList(laneHeadList);
#endif

    lane_head_list = remove_pass_through_states(lane_head_list);

    return lane_head_list;
}

static void
lane_tracing_phase2()
{
    lane_head_tail_pairs = nullptr; // for LR(k) use only.
    LT_tbl = nullptr;               // initialize the LT_tbl.

    const int ct = states_inadequate->count_unresolved;
#if DEBUG_PHASE_2
    printf("phase 2. unresolved inadequate states: %d\n", ct);
#endif

    laneHead* lane_head_list = get_conflict_lane_head();
    if (lane_head_list == nullptr) {
        puts("laneHeadList is nullptr. return");
        return;
    }

#if DEBUG_PHASE_2_GET_TBL
    dump_LT_tbl();
#endif

#if DEBUG_PHASE_2_REGENERATE
    puts("Now do regeneration");
#endif

    if (!USE_COMBINE_COMPATIBLE_STATES) {
        phase2_regeneration2(); // using all_clusters.
    } else {
        phase2_regeneration(lane_head_list);
    }
}

void
lane_tracing()
{
    IN_EDGE_PUSHING_LANE_TRACING = false;
    MAX_K = 1; // max K used in LR(k).

    lane_tracing_phase1();
    resolve_lalr1_conflicts();

    if (USE_LANE_TRACING && states_inadequate->count_unresolved > 0) {
        lane_tracing_phase2();
        resolve_lalr1_conflicts();   ///
        output_parsing_table_lalr(); ///

        if (USE_LR_K && rr_count > 0) {
            lrk_pt_array = nullptr; // initialize for use in gen_compiler.
            // do LR(k) if there are still r/r conflicts.
            lane_tracing_lrk();
        }
    } else {
        output_parsing_table_lalr(); // is this needed?
    }

    if (GRAMMAR_AMBIGUOUS) {
        puts("Grammar is ambiguous");
    }
}

/////////////////////////////////////////////////////////////

/* Functions for lane tracing */

static void
dump_stacks()
{
    std::cout << "__STACK__" << std::endl;
    STACK->dump();
    std::cout << "__LANE__" << std::endl;
    LANE->dump();
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
        if (strlen(n->snode->symbol) != 0)
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
        return create_symbol_node(snode);

    SymbolNode *n = list, *n_prev = nullptr;
    for (; n != nullptr; n_prev = n, n = n->next) {
        if (n->snode == snode) {
            *exist = true;
            return list; // existing node.
        }
        if (strcmp(n->snode->symbol, snode->symbol) > 0) {
            SymbolNode* new_node = create_symbol_node(snode);
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
    n_prev->next = create_symbol_node(snode);
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
        if (strlen(list->snode->symbol) == 0) {
            *null_possible = true;
        } else {
            bool exist = false;
            sn = insert_symbol_list_unique(sn, list->snode, &exist);
        }
    }
    return sn;
}

static void
stack_operation(int* fail_ct, Configuration* o)
{
    Configuration* tmp = nullptr;

    (*fail_ct)++;

#if DEBUG_PHASE_1
    printf("---------------fail_ct = %d\n", *fail_ct);
#endif

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

#if DEBUG_PHASE_1
    dump_stacks();
#endif
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
#if DEBUG_PHASE_1
    puts("CONTEXT ADDING ROUTINE: ");
    writeSymbolList(CONTEXT_GENERATED, "CONTEXT_GENERATED");
#endif

    SymbolNode* n = context_generated;

    while (n != nullptr) {
        for (int ct = cur_config_index; ct >= 0; ct--) {
            Configuration* c = LANE->array[ct];
            if (c != LT_ZERO && c != LT_MARKER) {
#if DEBUG_PHASE_1
                printf("add context to %d.%d\n", c->owner->state_no, c->ruleID);
#endif
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
context_adding_routine(SymbolList context_generated,
                       Configuration* o,
                       int cur_config_index,
                       int* fail_ct)
{
    context_adding(context_generated, cur_config_index);

    if (TRACE_FURTHER == FLAG_ON) {

#if DEBUG_PHASE_1
        puts("__TRACE_FURTHER is ON");
#endif

        TRACE_FURTHER = FLAG_OFF;
        stack_operation(fail_ct, o);
    }

#if DEBUG_PHASE_1
    else {
        puts("__TRACE_FURTHER is OFF");
    }
#endif
}

/*
 * Do lane_tracing Phase 1 on a configuration.
 * Pre-assumption: c is a reduction.
 */
void
lane_tracing_reduction(Configuration* c)
{
    if (nullptr == c)
        return;

#if DEBUG_PHASE_1
    printf("work on reduce config: ");
    stdout_writeConfig(c);
#endif

    if (c->COMPLETE == 1) {
#if DEBUG_EdgePushing
        puts("c->COMPLETE == 1");
#endif
        return; // already evaluated.
    }

    LANE = Stack::create();
    STACK = Stack::create();

    LANE->push(c);
    c->IN_LANE = FLAG_ON;
    TRACE_FURTHER = FLAG_OFF;
    TEST_FAILED = FLAG_OFF;

#if DEBUG_EdgePushing
    puts("DO_LOOP:");
#endif

    do_loop();
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
    const int ct = c->originators->count;
    for (int i = 0; i < ct; i++) {
        if (c->originators->list[i] == o)
            return true;
    }
    return false;
}

/*
 * o - the originator. o does not change in the call stack.
 */
static void
set_transitors_pass_thru_on(const Configuration* cur_config, Configuration* o)
{
    // find the next transitor for originator o.
    const int ct = cur_config->transitors->count;

    for (int i = 0; i < ct; i++) {
        Configuration* c = cur_config->transitors->list[i];

        get_originators(c, c);

        if (is_on_transitor_chain(c, o)) {

            if (false == USE_COMBINE_COMPATIBLE_STATES) {
#if DEBUG_PHASE_2_GET_TBL
                printf("B: next entry in entry_table: (%d.%d, %d.%d)\n",
                       c->owner->state_no,
                       c->ruleID,
                       cur_config->owner->state_no,
                       cur_config->ruleID);
#endif
                // add another entry to LT_tbl.
                lt_tbl_entry_add(c->owner, cur_config->owner);
            }

            if (c->owner != cur_config->owner && c->owner != o->owner) {
                // use this criteria because: cur_config does not need to be
                // handled here since it's already handled in trace_back().
                // you also don't want to set o->owner to be PASS_THRU, since
                // it may be the end state.
#if DEBUG_PHASE_2_GET_TBL
                printf("==state %d: set pass_thru ON. 2.\n",
                       c->owner->state_no);
#endif
                c->owner->PASS_THRU = 1;
            }
            set_transitors_pass_thru_on(c, o);
        } // end is_on_transitor_chain.
    }

    if (o->owner == cur_config->owner) {
        get_the_context(o);
    }
}

///!!!!!!!!!!!!!!!!!!!!!!!!!!! START.

/*
 * let s = c->owner, find transitors for c in parent states of s.
 */
static void
get_transitors(Configuration* c0, Configuration* c)
{
    StateList* l = c->owner->parents_list;
    if (l == nullptr) {
        std::cout << "Error: get_transitors() - L is nullptr" << std::endl;
        return;
    }
    if (l->state_list.size() == 0)
        return;

#if DEBUG_GET_ORIGINATOR
    printf(
      "get transitor for %d.%d.%d\n", c->owner->state_no, c->ruleID, c->marker);
#endif

    for (int i = 0; i < l->state_list.size(); i++) {
        State* p = l->state_list[i];
        // now get transitor for c.
        for (int j = 0; j < p->config_count; j++) {
            Configuration* t = p->config[j];
            if (t->ruleID == c->ruleID && t->marker == c->marker - 1) {
                // is a transitor.
#if DEBUG_GET_ORIGINATOR
                printf("++ ++ transitor found for [%d.%d.%d]: [%d.%d.%d]\n",
                       c->owner->state_no,
                       c->ruleID,
                       c->marker,
                       p->state_no,
                       t->ruleID,
                       t->marker);
#endif
                insert_transitor_list(c, t);
                get_originators(c0, t);
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
static void
get_originators(Configuration* c0, Configuration* c)
{
#if DEBUG_GET_ORIGINATOR
    printf("-- current config: %d.%d.%d\n",
           c->owner->state_no,
           c->ruleID,
           c->marker);
#endif

    if (c->isCoreConfig == 1) { // core config, search parent states.
        get_transitors(c0, c);
    } else { // not core config. find originators in current state.
        for (int i = 0; i < c->owner->config_count; i++) {
            Configuration* d = c->owner->config[i];
            if (c == d) {
                continue;
            } // ignore c.
            if (d->nMarker == nullptr) {
                continue;
            }
            if (d->nMarker->snode == grammar.rules[c->ruleID]->nLHS->snode) {
#if DEBUG_GET_ORIGINATOR
                printf("-- -- originator found for [%d.%d.%d]: [%d.%d.%d]\n",
                       c0->owner->state_no,
                       c0->ruleID,
                       c0->marker,
                       d->owner->state_no,
                       d->ruleID,
                       d->marker);
#endif
                insert_originator_list(c0, d, 1);
            }
        }
    }
}

///!!!!!!!!!!!!!!!!!!!!!!!!!!! END.

static void
do_loop()
{
    SymbolNode* CONTEXTS_GENERATED;
    int fail_ct;
    int cur_config_index;

    Configuration* cur_config = LANE->top();
    cur_config_index = LANE->count() - 1;

    if (cur_config == LT_MARKER || cur_config == LT_ZERO ||
        cur_config == nullptr) { // should never happen.
        puts("DO_LOOP cur_config error");
        exit(1);
    }

#if DEBUG_GET_ORIGINATOR
    printf("==call get_originators(cur_config) in DO_LOOP()==\n");
#endif
    get_originators(cur_config, cur_config);

    const int ct = cur_config->originators->count;
    fail_ct = 0;

#if DEBUG_PHASE_1
    printf("++++++++++TOP of LANE is: ");
    stdout_writeConfig(cur_config);
#endif

    for (int i = 0; i < ct; i++) {

#if DEBUG_PHASE_1
        puts("________NEXT ORIGINATOR___________________");
#endif

        CONTEXTS_GENERATED = nullptr;
        Configuration* o = cur_config->originators->list[i];

        SymbolNode* scanned_symbol = o->nMarker;
        SymbolNode* gamma = nullptr;
        if (scanned_symbol == nullptr)
            gamma = nullptr; // shouldn't happen
        else
            gamma = scanned_symbol->next;

#if DEBUG_PHASE_1
        stdout_writeConfig(o);
        printf("gamma: %s\n",
               gamma == nullptr ? "nullptr" : gamma->snode->symbol);
#endif

        SymbolNode* gamma_theads = nullptr;
        if (gamma != nullptr) { // if not nullptr, get theads.
#if DEBUG_PHASE_1
            puts("gamma not nullptr, get theads.");
#endif
            gamma_theads = get_theads(gamma); // get Heads.
#if DEBUG_PHASE_1
            my_showTHeads(gamma, gamma_theads);
#endif
        } else {
            // gamma is nullptr, check if this is goal production.
            // NOTE that there are only TWO goal production in all states.
            // one is in state 0, the other is in the state where the
            // goal production is a final production. The second case
            // won't be traced in lane-tracing at all.
            // if (o->owner->state_no == 0 && o->ruleID == 0) {
            if (is_goal(*o)) {
#if DEBUG_PHASE_1
                puts("GOAL PRODUCTION - generate context: $end");
#endif
                gamma_theads = create_symbol_node(hash_tbl_find(strEnd));
            }
        }

#if DEBUG_PHASE_1
        dumpLaneStartStates(o, gamma_theads);
#endif

        if (test_a(gamma_theads)) {
#if DEBUG_PHASE_1
            puts("testA true, get CONTEXTS_GENERATED");
#endif
            bool null_possible = false;
            CONTEXTS_GENERATED =
              get_contexts_generated(gamma_theads, &null_possible);

            if (IN_EDGE_PUSHING_LANE_TRACING) { /// 12-19-2008.
                EDGE_PUSHING_CONTEXT_GENERATED = CONTEXTS_GENERATED;
            }

            if (null_possible) {
#if DEBUG_PHASE_1
                puts("null possible true");
#endif
                if (test_b(o)) { // if (o->COMPLETE == FLAG_ON) {
#if DEBUG_PHASE_1
                    puts("COMPLETE ON");
#endif

                    CONTEXTS_GENERATED = combine_context_list(
                      CONTEXTS_GENERATED, o->context->nContext);
                } else {
#if DEBUG_PHASE_1
                    puts("COMPLETE OFF");
#endif

                    if (test_d(o)) { // if (o->IN_LANE == FLAG_ON) {
#if DEBUG_PHASE_1
                        puts("IN_LANE ON");
                        puts("GRAMMAR is AMBIGUOUS");
#endif
                        GRAMMAR_AMBIGUOUS = true;
                        /// exit(1); //////////////// exit prematurely.
                        move_markers(o);
                    } else {
#if DEBUG_PHASE_1
                        puts("IN_LANE OFF. set TRACE_FURTHER ON");
#endif

                        TRACE_FURTHER = FLAG_ON;
                    }
                }
            } else {
#if DEBUG_PHASE_1
                puts("possible null is: false");
#endif
                o->LANE_END = 1; // set LANE_END to be true.

#if DEBUG_PHASE_1
                printf(
                  "Found lane_end: %d.%d\n", o->owner->state_no, o->ruleID);
                printf("conflict config: %d.%d, lane head state %d, contexts: ",
                       LANE->array[0]->owner->state_no,
                       LANE->array[0]->ruleID,
                       o->owner->state_no);
                writeSymbolList(gamma_theads, "contexts");
#endif
                // if is in edge_pushing, ignore the context adding routine.
                if (IN_EDGE_PUSHING_LANE_TRACING) {
                    continue;
                }
            }
            // CONTEXT adding routine.
            context_adding_routine(
              CONTEXTS_GENERATED, o, cur_config_index, &fail_ct);

        } else {
#if DEBUG_PHASE_1
            puts("testA false");
#endif

            if (test_b(o)) { // testB
#if DEBUG_PHASE_1
                puts("testB true");
#endif

                CONTEXTS_GENERATED = combine_context_list(CONTEXTS_GENERATED,
                                                          o->context->nContext);
                context_adding_routine(
                  CONTEXTS_GENERATED, o, cur_config_index, &fail_ct);
            } else {
#if DEBUG_PHASE_1
                puts("testB false");
#endif

                if (test_c(o)) { // test_c
#if DEBUG_PHASE_1
                    puts("test_c true");
#endif

                    move_markers(o);

                    CONTEXTS_GENERATED = combine_context_list(
                      CONTEXTS_GENERATED, o->context->nContext);
                    context_adding_routine(
                      CONTEXTS_GENERATED, o, cur_config_index, &fail_ct);
                } else {
#if DEBUG_PHASE_1
                    puts("test_c false");
#endif

                    stack_operation(&fail_ct, o);
                }
            }
        } // end of else (test_a false).

    } // end of for.

#if DEBUG_PHASE_1
    puts("________END OF DO_LOOP____________________");
#endif

    if (TEST_FAILED == FLAG_ON) {
#if DEBUG_PHASE_1
        puts("__TEST_FAILED is ON__");
#endif

        TEST_FAILED = FLAG_OFF;
        do_loop();
    } else {
        check_lane_top();
    }
}

static void
pop_lane()
{

#if DEBUG_PHASE_1
    puts("POP_LANE");
#endif

    LANE->pop();
    check_lane_top();
}

static void
check_stack_top()
{
    Configuration* top = STACK->top();

    if (top == LT_MARKER) {
#if DEBUG_PHASE_1
        puts("__check_stack_top true");
#endif

        STACK->pop();
        pop_lane();

    } else {
#if DEBUG_PHASE_1
        puts("__check_stack_top false");
#endif

        if (top->COMPLETE == FLAG_ON) {
#if DEBUG_PHASE_1
            puts("__top COMPLETE ON");
#endif

            STACK->pop();
            check_stack_top();
        } else {
#if DEBUG_PHASE_1
            puts("__top COMPLETE OFF");
#endif

            STACK->pop();
            LANE->push(top);
            top->IN_LANE = FLAG_ON;
            do_loop();
        }
    }
}

static void
propogate_context_sets(Configuration* c)
{
    State* s;
    Configuration* f;
    int i, ct;
    if (c == nullptr || c->COMPLETE == 0 || c->isCoreConfig == 1)
        return;

#if DEBUG_PHASE_1
    printf("===config %d.%d heuristic propogate context sets===\n",
           c->owner->state_no,
           c->ruleID);
    stdout_writeConfig(c);
#endif

    s = c->owner;
    ct = s->config_count;
    for (i = 0; i < ct; i++) {
        f = s->config[i];
        if (f == c)
            continue;
        if (grammar.rules[f->ruleID]->nLHS->snode !=
            grammar.rules[c->ruleID]->nLHS->snode)
            continue;
        if (f->marker > 0)
            continue;
        if (f->COMPLETE == 1)
            continue; // x-transition of conflict states.

        // otherwise, heuristically propagate context sets.
        free_context(f->context);
        f->context = c->context;
        f->COMPLETE = 1;
    }
}

static void
check_lane_top()
{
    Configuration* lane_top = LANE->top();

    if (lane_top == LT_MARKER) {
#if DEBUG_PHASE_1
        puts("check lane top true");
#endif

        check_stack_top();
    } else {
#if DEBUG_PHASE_1
        puts("check lane top false");
#endif

        if (lane_top == LT_ZERO) {
#if DEBUG_PHASE_1
            puts("lane top is ZERO.");
#endif

            pop_lane();
        } else {
            lane_top->IN_LANE = FLAG_OFF;
            lane_top->COMPLETE = FLAG_ON;

            /// 12-19-2008.
            if (IN_EDGE_PUSHING_LANE_TRACING == false) {
                propogate_context_sets(lane_top);
            }
            if (LANE->count() == 1) { // the starting reduction
                                      /////////// END PROGRAMING ///////////////
#if DEBUG_PHASE_1
                puts("=====REDUCTION LANE TRACING ENDS=====");
#endif
            } else {
                pop_lane();
            }
        }
    }
}
