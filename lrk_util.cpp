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
 * lrk_util.c
 *
 * Utility functions for LR(k).
 *
 * @Author: Xin Chen
 * @Created on: 11/25/2008
 * @Last modified: 11/25/2008
 * @Copyright: 2008
 */

#include "lane_tracing.hpp"
#include "y.hpp"
#include <fstream>
#include <iostream>
#include <ostream>
#include <stdexcept>

//////////////////////////////////////////////////////////////////
// Functions for ConfigPairNode. START.
//////////////////////////////////////////////////////////////////

static auto
config_pair_node_create(Configuration* conflict_config,
                        Configuration* lane_start_config) -> ConfigPairNode*
{
    auto* n = new ConfigPairNode;
    n->end = conflict_config;
    n->start = lane_start_config;
    n->next = nullptr;
    return n;
}

static void
config_pair_node_destroy(ConfigPairNode* n)
{
    delete n;
}

/*
 * compare function: 1 - greater than, 0 - equal, -1 - less than.
 */
static auto
config_pair_cmp(Configuration* end1,
                Configuration* start1,
                Configuration* end2,
                Configuration* start2) -> int
{
    int cmp = start1->owner->state_no - start2->owner->state_no;
    if (cmp > 0)
        return 1;
    if (cmp < 0)
        return -1;
    // else, == 0.

    cmp = start1->ruleID - start2->ruleID;
    if (cmp > 0)
        return 1;
    if (cmp < 0)
        return -1;
    // else, == 0.

    cmp = end1->owner->state_no - end2->owner->state_no;
    if (cmp > 0)
        return 1;
    if (cmp < 0)
        return -1;
    // else, == 0.

    cmp = end1->ruleID - end2->ruleID;
    if (cmp > 0)
        return 1;
    if (cmp < 0)
        return -1;
    // else, == 0.

    return 0;
}

/*
 * Combine list s(ource) to list t(arget).
 */
auto
config_pair_list_combine(ConfigPairList t, ConfigPairList s) -> ConfigPairList
{
    if (s == nullptr)
        return t;
    if (t == nullptr)
        return s;

    for (ConfigPairNode* n = s; n != nullptr; n = n->next) {
        t = config_pair_list_insert(t, n->end, n->start);
    }

    return t;
}

/*
 * Insert in INC order of:
 *   conflict_config's state_no and ruleID,
 *   lane_start_config's state_no and ruleID.
 */
auto
config_pair_list_insert(ConfigPairList list,
                        Configuration* conflict_config,
                        Configuration* lane_start_config) -> ConfigPairList
{
    ConfigPairNode *n = nullptr, *n_prev = nullptr;

    if (list == nullptr) {
        return config_pair_node_create(conflict_config, lane_start_config);
    }

    for (n_prev = nullptr, n = list; n != nullptr; n_prev = n, n = n->next) {
        int cmp =
          config_pair_cmp(n->end, n->start, conflict_config, lane_start_config);
        if (cmp < 0) {
            continue;
        }
        if (cmp == 0) {
            // existing config pair.
            return list;
        } // cmp > 0, insert to list between n and n_prev.
        ConfigPairNode* m =
          config_pair_node_create(conflict_config, lane_start_config);
        if (n_prev == nullptr) { // insert at start
            m->next = list;
            list = m;
        } else { // insert in the middle
            n_prev->next = m;
            m->next = n;
        }
        return list;
    }

    // n is nullptr. insert at the end.
    n_prev->next = config_pair_node_create(conflict_config, lane_start_config);
    return list;
}

static void
config_pair_node_dump(ConfigPairNode* n)
{
    std::cout << "(" << n->start->owner->state_no << "." << n->start->ruleID
              << " -> " << n->end->owner->state_no << "." << n->end->ruleID
              << ")";

    if (n->start->owner->PASS_THRU == 1) {
        std::cout << " PASS_THRU. ";
    }
}

void
config_pair_list_dump(ConfigPairList list)
{
    std::cout << "--ConfigPairList--" << std::endl;
    for (ConfigPairNode* n = list; n != nullptr; n = n->next) {
        config_pair_node_dump(n);
        puts("");
    }
}

/*
 * Note that more than one LANE_END configurations could be found.
 */
auto
config_pair_list_find(ConfigPairList list, Configuration* conflict_config)
  -> ConfigPairNode*
{
    for (ConfigPairNode* n = list; n != nullptr; n = n->next) {
        if (n->end == conflict_config) {
            return n;
        }
    }
#if DEBUG_EdgePushing
    puts("not found");
#endif
    return nullptr;
}

void
config_pair_list_destroy(ConfigPairList list)
{
    ConfigPairNode* n = list;
    while (n != nullptr) {
        ConfigPairNode* tmp = n;
        n = n->next;
        config_pair_node_destroy(tmp);
    }
}

//////////////////////////////////////////////////////////////////
// Functions for ConfigPairNode. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for Set. START.
//////////////////////////////////////////////////////////////////

/*
 * Create new set object.
 */
static auto
object_item_new(void* object) -> ObjectItem*
{
    auto* s = new ObjectItem;
    s->object = object;
    s->next = nullptr;
    return s;
}

static void
object_item_destroy(ObjectItem* s)
{
    delete s;
}

/*
 * Insert if not exist.
 * NOTE: "not exist" means the object, but if "not exist" means
 * the contents of the object, then a separate function should
 * be written for this.
 */
auto
set_insert(Set* set, void* object) -> Set*
{
    if (set == nullptr) {
        return object_item_new(object);
    }

    Set* s = nullptr;
    // else, set is not nullptr.
    for (s = set; s->next != nullptr; s = s->next) {
        if (s->object == object)
            return set; // exists already.
    }

    // now s->next is nullptr.
    if (s->object == object) {
        return set;
    }
    s->next = object_item_new(object);

    return set;
}

auto
set_find(Set* set, void* object) -> ObjectItem*
{
    for (Set* s = set; s != nullptr; s = s->next) {
        if (s->object == object)
            return s;
    }

    return nullptr;
}

auto
set_delete(Set* set, void* object) -> Set*
{

    if (set == nullptr) {
        return nullptr;
    }

    Set *s = nullptr, *s_prev = set;
    // else, set is not nullptr.
    for (; s != nullptr; s_prev = s, s = s->next) {
        if (s->object == object) {
            if (s_prev == nullptr) {
                s_prev = s;
                s = s->next;
                object_item_destroy(s_prev);
                return s;
            }
            s_prev->next = s->next;
            object_item_destroy(s);
            return set;
        }
    }

    return set;
}

/*
 * A function pointer is passed in. This function dumps the set item.
 */
void
set_dump(const Set* set, void (*set_item_dump)(void*))
{
    if (set == nullptr) {
        puts("(set is empty)");
        return;
    }

    for (const Set* s = set; s != nullptr; s = s->next) {
        (*set_item_dump)(s->object);
    }
}

//////////////////////////////////////////////////////////////////
// Functions for Set. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for List. START.
//////////////////////////////////////////////////////////////////

// create an empty list.
auto
list_create() -> List*
{
    auto* t = new List;
    t->head = nullptr;
    t->tail = nullptr;
    t->count = 0;
    return t;
}

// insert new object at tail of list t,
// without checking if the object already exists.
void
list_insert_tail(List* t, void* object)
{
    if (nullptr == object || nullptr == t)
        return;

    if (t->head == nullptr) {
        t->head = t->tail = object_item_new(object);
    } else {
        t->tail->next = object_item_new(object);
        t->tail = t->tail->next;
    }

    t->count++;
}

void
list_destroy(List* t)
{
    ObjectItem* o = nullptr;

    if (t == nullptr)
        return;
    if ((o = t->head) != nullptr) {
        while (o != nullptr) {
            ObjectItem* tmp = o;
            o = o->next;
            object_item_destroy(tmp);
        }
    }

    delete t;
}

void
list_dump(List* t, void (*list_item_dump)(void*))
{
    if (t == nullptr || t->head == nullptr) {
        std::cout << "(list is empty)" << std::endl;
        return;
    }

    std::cout << "list count: " << t->count << std::endl;

    int i = 0;
    for (ObjectItem* s = t->head; s != nullptr; s = s->next) {
        std::cout << ++i << " ";
        (*list_item_dump)(s->object);
    }
}

//////////////////////////////////////////////////////////////////
// Functions for List. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for LR(k) table. START.
//////////////////////////////////////////////////////////////////

// create a parsing table.
auto
lrk_pt_create(int k) -> LRkPT*
{
    auto* t = new LRkPT;
    t->k = k;
    t->row_count = 0;
    t->rows = nullptr;
    return t;
}

void
lrk_pt_dump(const LRkPT* t)
{
    if (t == nullptr)
        return;
    if (t->rows == nullptr) {
        std::cout << "(empty LR(" << t->k << ") parsing table)" << std::endl;
        return;
    }

    for (LRkPTRow* r = t->rows; r != nullptr; r = r->next) {
        std::cout << "[" << r->state << ", " << r->token->snode->symbol << "] ";
        for (int i = 0; i < ParsingTblCols; i++) {
            ConfigPairNode* n = r->row[i];
            if (n == nullptr) {
                std::cout << "0 ";
            } else {
                Configuration* c = n->end; /// start;
                if (reinterpret_cast<uintptr_t>(c) == CONST_CONFLICT_SYMBOL) {
                    std::cout << "X ";
                } else {
                    std::cout << n->end->ruleID << " ";
                }
            }
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

/*
 * Same as lrk_pt_dump() but write a file.
 */
void
lrk_pt_dump_file(const LRkPT* t, std::ofstream& fp)
{
    if (!fp.is_open()) {
        std::cout << "lrk_pt_dump_FILE error: fp is closed." << std::endl;
        return;
    }
    if (t == nullptr)
        return;
    if (t->rows == nullptr) {
        fp << "(empty LR(" << t->k << ") parsing table)" << std::endl;
        return;
    }

    for (const LRkPTRow* r = t->rows; r != nullptr; r = r->next) {
        fp << "[" << r->state << ", " << r->token->snode->symbol << "] ";
        for (int i = 0; i < ParsingTblCols; i++) {
            const ConfigPairNode* n = r->row[i];
            if (n == nullptr) {
                fp << "0 ";
            } else {
                const Configuration* c = n->end; /// start;
                if (reinterpret_cast<uintptr_t>(c) == CONST_CONFLICT_SYMBOL) {
                    fp << "X ";
                } else {
                    fp << n->end->ruleID << " ";
                }
            }
        }
        fp << std::endl;
    }
    fp << std::endl;
}

/*
 * @Return: found - true if found, false if not.
 *          If found is true, return the row.
 *          otherwise, return the row before the insertion point.
 */
auto
lrk_pt_find(const LRkPT* t, int state, SymbolTblNode* token, bool* found)
  -> LRkPTRow*
{

    *found = false;
    if (nullptr == t)
        return nullptr;

    LRkPTRow* r = t->rows;
    if (nullptr == r)
        return nullptr;

    LRkPTRow* r_prev = nullptr;
    for (; r != nullptr; r_prev = r, r = r->next) {
        if (r->state < state)
            continue;
        if (r->state > state)
            break; // not found
                   // found same state.
        int cmp = strcmp(r->token->snode->symbol, token->symbol);
        if (cmp < 0)
            continue;
        if (cmp > 0)
            break;
        *found = true;
        return r;
    }

    return r_prev;
}

/*
 * Pre-assumption: t is not nullptr.
 * Insert the new row after r.
 * @Return: the inserted new row.
 */
static auto
lrk_pt_add_row(LRkPT* t, LRkPTRow* r_prev, int state, SymbolTblNode* token)
  -> LRkPTRow*
{
    auto* r = new LRkPTRow;
    r->state = state;
    r->token = create_symbol_node(token);

    r->row = new ConfigPairNode*[ParsingTblCols];
    for (int i = 0; i < ParsingTblCols; i++) {
        r->row[i] = nullptr; // initialize to nullptr.
    }

    r->next = nullptr;
    if (r_prev != nullptr) {
        r->next = r_prev->next;
        r_prev->next = r;
    } else {
        t->rows = r;
    }

    t->row_count++;
    return r;
}

/*
 * Get the entry [(state, token), col_token] in t.
 *
 * Assumptions: t != nullptr.
 */
auto
lrk_pt_get_entry(LRkPT* t,
                 int state,
                 SymbolTblNode* token,
                 SymbolTblNode* col_token,
                 bool* exist) -> ConfigPairNode*
{
    bool found = false;

    *exist = false;
    if (t == nullptr)
        return nullptr; // parsing table not exist.

    LRkPTRow* r = lrk_pt_find(t, state, token, &found);
    if (found == false)
        return nullptr; // row not exist in t.

    *exist = true; // this entry exists.
    int index = get_col(col_token);

    return r->row[index];
}

/*
 * For row on (state, token), there is a reduce action for symbol s.
 * New entries are inserted in INC order of state, then token.
 *
 * ruleID can be accessed as c->ruleID.
 *
 * Return: true is confilct occurs, false otherwise.
 */
auto
lrk_pt_add_reduction(LRkPT* t,
                     int state,
                     SymbolTblNode* token,
                     SymbolTblNode* s,
                     Configuration* c,
                     Configuration* c_tail) -> bool
{
    if (t == nullptr)
        return false;

    bool found = false;
    LRkPTRow* r = lrk_pt_find(t, state, token, &found);

    if (found == false) { // insert new entry
        r = lrk_pt_add_row(t, r, state, token);
    }

    // now add the reduce action on token s.
    int index = get_col(s);
    ConfigPairNode* n = r->row[index];
    if (n == nullptr) {
        r->row[index] = config_pair_node_create(c_tail, c);
        return false;
    }
    Configuration* prev_entry = n->end; /// start;
    if (reinterpret_cast<uintptr_t>(prev_entry) == CONST_CONFLICT_SYMBOL) {
        std::cout << "row [" << r->state << ", " << r->token->snode->symbol
                  << "] r/r conflict: CONFLICT_LABEL:" << c->ruleID
                  << std::endl;
    } else if (prev_entry == c_tail) {
        // same config, do nothing.
    } else {
        std::cout << "row [" << r->state << ", " << r->token->snode->symbol
                  << "] r/r conflict: " << prev_entry->ruleID << ":"
                  << c->ruleID << std::endl;
        r->row[index]->end =
          reinterpret_cast<Configuration*>(CONST_CONFLICT_SYMBOL);
    }
    return true;
}

//////////////////////////////////////////////////////////////////
// Functions for LR(k) table. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for LR(k) table array. START.
//////////////////////////////////////////////////////////////////

auto
lrk_pt_array_create() -> LRkPTArray*
{

    auto* a = new LRkPTArray;
    a->size = 10; // initial max number of entries: 10.
    a->array = new LRkPT*[a->size];
    for (size_t i = 0; i < a->size; i++) { // initialize to nullptr.
        a->array[i] = nullptr;
    }
    a->max_k = 1;
    return a;
}

/*
 * Add t to a.
 */
void
lrk_pt_array_add(LRkPTArray* a, LRkPT* t)
{
    if (nullptr == a || nullptr == t)
        return;

    if (a->max_k >= a->size + 1) { // expand a->array.
        HYY_EXPAND(&a->array, a->size * 2);
        a->size = a->size * 2;
    }
    if (a->array[t->k - 2] == nullptr) {
        a->array[t->k - 2] = t;
        if (a->max_k < t->k) {
            a->max_k = t->k;
        }
    } else {
        std::cerr << "Error: LR(" << t->k << ") table already exists"
                  << std::endl;
    }
}

/*
 * Get the LR(k) parsing table for k.
 */
auto
lrk_pt_array_get(LRkPTArray* a, int k) -> LRkPT*
{
    if (k < 2 || k > a->max_k)
        return nullptr;
    if (a->array[k - 2] == nullptr) {
        std::cerr << "Warning: LR(" << k << ") table is empty" << std::endl;
        return nullptr;
    }
    return a->array[k - 2];
}

static void
write_parsing_tbl_col_hdr()
{
    std::cout << "--Parsing Table Column Header [Total: " << ParsingTblCols
              << "]--" << std::endl;
    for (int i = 0; i < ParsingTblCols; i++) {
        std::cout << ParsingTblColHdr[i]->symbol << " ";
    }
    std::cout << std::endl;
}

static void
write_parsing_tbl_col_hdr_file(std::ostream& fp)
{
    fp << "--Parsing Table Column Header [Total: " << ParsingTblCols << "]--"
       << std::endl;
    for (int i = 0; i < ParsingTblCols; i++) {
        fp << ParsingTblColHdr[i]->symbol << " ";
    }
    fp << std::endl;
}

void
lrk_pt_array_dump(LRkPTArray* a)
{
    if (nullptr == a)
        return;

    write_parsing_tbl_col_hdr();
    std::cout << "===LRkPTArray_dump [max_k = " << a->max_k
              << "]===" << std::endl;

    for (int i = 2; i <= a->max_k; i++) {
        std::cout << "LR(" << i << ") p.t." << std::endl;
        lrk_pt_dump(a->array[i - 2]);
    }

    std::cout << "====================================" << std::endl;
}

/*
 * Dump to disk.
 */
void
lrk_pt_array_dump_file(LRkPTArray* a)
{
    if (nullptr == a)
        return;

    std::ofstream fp2;
    fp2.open("y.lrk");
    if (!fp2.is_open()) {
        std::cerr << "cannot open file y.lrk to write" << std::endl;
        return;
    }

    write_parsing_tbl_col_hdr_file(fp2);
    fp2 << "===LRkPTArray_dump [max_k = " << a->max_k << "]===" << std::endl;

    for (int i = 2; i <= a->max_k; i++) {
        fp2 << "=LR(" << i << ") p.t." << std::endl;
        lrk_pt_dump_file(a->array[i - 2], fp2);
    }

    fp2.close();
}

//////////////////////////////////////////////////////////////////
// Functions for LR(k) table array. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for struct CfgCtxt. START.
//////////////////////////////////////////////////////////////////

auto
cfg_ctxt_create(Configuration* c, SymbolList s, Configuration* tail) -> CfgCtxt*
{
    auto* cc = new CfgCtxt;
    cc->c = c;
    cc->ctxt = s;
    cc->tail = tail;
    return cc;
}

void
cfg_ctxt_destroy(CfgCtxt* cc)
{
    delete cc;
}

void
cfg_ctxt_dump(const CfgCtxt* cc)
{
    if (nullptr == cc)
        return;
    std::cout << "CfgCtxt: " << cc->c->owner->state_no << "." << cc->c->ruleID
              << " { ";
    for (SymbolList a = cc->ctxt; a != nullptr; a = a->next) {
        std::cout << a->snode->symbol << " ";
    }
    std::cout << "}[tail: ";
    if (cc->tail != nullptr) {
        std::cout << cc->tail->owner->state_no << "." << cc->tail->ruleID;
    }
    std::cout << "]" << std::endl;
}

//////////////////////////////////////////////////////////////////
// Functions for struct CfgCtxt. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. START.
//////////////////////////////////////////////////////////////////

#define DEBUG_LRK_THEADS_CYCLE 1
#define DEBUG_LRK_THEADS 0

//
// Get the head of string alpha up to the k-th symbol that
// does not vanish, or the entire alpha if less than k symbols
// do not vanish.
// Note: k >= 2.
// @Return: a COPY of the head of the original string is returned.
//
auto
get_string_with_k_non_vanish_symbol(SymbolList alpha, int k) -> SymbolList
{
    if (nullptr == alpha || k <= 0)
        return nullptr;

    SymbolNode* pt = alpha;
    SymbolNode* cpy_tail = create_symbol_node(pt->snode);
    SymbolNode* cpy = cpy_tail;
    int i = (is_vanish_symbol(*pt->snode) == false) ? 1 : 0;
    if (i == k)
        return cpy;

    for (pt = alpha->next; pt != nullptr; pt = pt->next) {
        cpy_tail->next = create_symbol_node(pt->snode);
        cpy_tail = cpy_tail->next;
        if (is_vanish_symbol(*pt->snode) == false)
            i++;
        if (i == k)
            break;
    }

    return cpy;
}

//
// n is a node in list new_list,
// replace symbol n->next with the RHS of rule numbered ruleID.
// return the new list.
//
auto
replace_with_rhs(SymbolList new_list, SymbolNode* n_prev, int rule_id)
  -> SymbolNode*
{
    SymbolNode *rhs_tail = nullptr, *tmp = nullptr;

    SymbolList rhs = clone_symbol_list(grammar.rules[rule_id]->nRHS_head);
    if (rhs == nullptr) {
        rhs_tail = nullptr;
        // in this case, rhs is empty list,
        // just remove n_prev->next from new_list.
        if (nullptr == n_prev) {
            tmp = new_list;
            new_list = new_list->next;
        } else {
            tmp = n_prev->next;
            n_prev->next = tmp->next;
        }
        free_symbol_node(tmp);
        return new_list;
    }

    // else, rhs is NOT nullptr.
    for (rhs_tail = rhs; rhs_tail->next != nullptr; rhs_tail = rhs_tail->next)
        ;

    if (n_prev == nullptr) { // replace the first symbol with rhs list.
        tmp = new_list;
        rhs_tail->next = tmp->next;
        free_symbol_node(tmp);
        new_list = rhs;
    } else { // replace symbol n_prev->next in the middle.
        tmp = n_prev->next;
        rhs_tail->next = tmp->next;
        free_symbol_node(tmp);
        n_prev->next = rhs;
    }

    return new_list;
}

auto
is_same_symbol_list(SymbolList a, SymbolList b) -> bool
{
    for (; (a != nullptr) && (b != nullptr); a = a->next, b = b->next) {
        if (a->snode != b->snode)
            return false;
    }
    if ((nullptr == a) && (nullptr == b))
        return true;
    return false;
}

// assumption: new_list != nullptr, t != nullptr.
auto
lrk_thead_in_list(List* t, SymbolList new_list) -> bool
{
    if (nullptr == t || new_list == nullptr) {
        throw std::runtime_error(
          "lrk_thead_in_list ERROR: t or new_list is nullptr");
    }

    for (ObjectItem* o = t->head; o != nullptr; o = o->next) {
        if (true ==
            is_same_symbol_list(new_list, static_cast<SymbolList>(o->object))) {
            return true;
        }
    }

    return false;
}

//
// Assumption: k >= 2.
// Truncate list s so it contains up to k non-vanishable symbols.
//
static auto
lrk_theads_truncate_list_by_k(SymbolList s, int k) -> SymbolList
{
    if (nullptr == s)
        return nullptr;

    int i = 0;
    for (SymbolNode* t = s; t != nullptr; t = t->next) {
        if (t->snode->vanishable == false)
            i++;
        if (i >= k) { // truncate from after this point.
                      // puts("--Yes truncate");
            SymbolNode* tmp = t->next;
            t->next = nullptr;
            if (nullptr != tmp) {
                free_symbol_node_list(tmp);
            }
            break;
        }
    }

    return s;
}

//
// Add to the end of list the result of applying all possible
// productions to the j-th symbol, omitting existing strings,
// and truncate until it contains no more than k non-vanishable
// symbols.
//
void
add_derivatives(List* t, ObjectItem* o, int j, int k)
{
    // get the (j)-th symbol.
    auto* m = static_cast<SymbolNode*>(o->object);
    for (int i = 0; i < j; i++) {
        if (nullptr == m) {
            return;
        }
        m = m->next;
    }
    if (nullptr == m) {
        return;
    }

    for (RuleIDNode* r = m->snode->ruleIDList; r != nullptr; r = r->next) {
        SymbolList new_list =
          clone_symbol_list(static_cast<SymbolNode*>(o->object));
        // get the (j-1)-th symbol and store as n_prev.
        SymbolNode* n_prev = nullptr;
        SymbolNode* n = new_list;
        for (int i = 0; i < j; i++) {
            n_prev = n;
            n = n->next;
        }

        new_list = replace_with_rhs(new_list, n_prev, r->ruleID);
        // assumption: new_list != nullptr, t != nullptr.
        if (nullptr != new_list) {
            new_list = lrk_theads_truncate_list_by_k(new_list, k);
            if (lrk_thead_in_list(t, new_list) == false) {
                list_insert_tail(t, (void*)new_list);
            }
        } // end if
    }     // end for
}

//
// Assumption: s.length >= j.
//
// Return true is the j-th symbol of s exists and is Non-terminal.
// otherwise return false.
//
auto
j_th_symbol_is_nt(SymbolList s, int j) -> bool
{
    for (int i = 0; i < j; i++) {
        if (s == nullptr)
            return false;
        s = s->next;
    }
    if (s == nullptr)
        return false;

    return (s->snode->type == symbol_type::NONTERMINAL);
}

//
// Remove from list t all strings whose j-th symbol is non-terminal.
//
static void
lrk_theads_rm_nt(List* t, int j)
{
#if DEBUG_LRK_THEADS
    puts("\nlrk_theads_rm_nt:");
#endif

    if (nullptr == t || nullptr == t->head)
        return;

    ObjectItem* o_prev = nullptr;
    ObjectItem* o = t->head;
    while (o != nullptr) {
        if (true == j_th_symbol_is_nt(static_cast<SymbolList>(o->object), j)) {
            t->count--;
            // remove o.
            if (o_prev == nullptr) {
                t->head = o->next;
                if (t->head == nullptr) {
                    t->tail = nullptr;
                }
                object_item_destroy(o);
                o = t->head;
            } else {
                o_prev->next = o->next;
                if (o_prev->next == nullptr) {
                    t->tail = o_prev;
                }
                object_item_destroy(o);
                o = o_prev->next;
            }
        } else {
            o_prev = o;
            o = o->next;
        }
    }
}

//
// Assumption: k >= 2.
//
// Returns true if the first k symbols are s are all terminals.
// Otherwise returns false.
//
static auto
k_heads_are_t(SymbolList s, int k, int* len) -> bool
{
    int i;
    SymbolList s0 = s;
    *len = -1;

    if (nullptr == s)
        return false;

    for (i = 0; (i < k) && (nullptr != s); i++) {
        if (s->snode->type == symbol_type::NONTERMINAL)
            return false;
        s = s->next;
    }

    *len = i;

    // s contains less than k symbols, and these are all terminals.
    if ((i < k) && (nullptr == s)) {
        return false;
    }

    return true;
}

//
// Remove from t all strings whose k-heads consist entirely
// of terminals, and add the k-heads to set t_heads;
//
static void
lrk_theads_rm_theads(List* t, int k, List* t_heads)
{
    int len = 0;

    if (nullptr == t || nullptr == t->head)
        return;

    ObjectItem* o_prev = nullptr;
    ObjectItem* o = t->head;
    while (o != nullptr) {
        if (true ==
              k_heads_are_t(static_cast<SymbolList>(o->object), k, &len) ||
            (len > 0 && len < k)) {
            t->count--;
            if (len < k) {
                // k'-thead, where k' < k. do nothing
                if (len == k - 1) {
                    list_insert_tail(t_heads, o->object);
                }
            } else if (false ==
                       lrk_thead_in_list(t_heads, (SymbolList)o->object)) {
                // k-thead.
                list_insert_tail(t_heads, o->object);
            }
            // remove o.
            if (o_prev == nullptr) {
                t->head = o->next;
                if (t->head == nullptr) {
                    t->tail = nullptr;
                }
                object_item_destroy(o);
                o = t->head;
            } else {
                o_prev->next = o->next;
                if (o_prev->next == nullptr) {
                    t->tail = o_prev;
                }
                object_item_destroy(o);
                o = o_prev->next;
            }
        } else {
            o_prev = o;
            o = o->next;
        }
    }
}

/*
 * Find theads of length k for string alpha.
 * This is a set of strings.
 */
auto
lrk_theads(SymbolList alpha, int k) -> List*
{
    if (alpha == nullptr)
        return nullptr;

    SymbolList s = get_string_with_k_non_vanish_symbol(alpha, k);

    List* t_heads = list_create(); // set of LR(k) theads.
    List* t = list_create();
    list_insert_tail(t, (void*)s);

    for (int j = 0; j < k; j++) {
        ObjectItem* o = t->head;
        while (o != nullptr) {
            add_derivatives(t, o, j, k);
            o = o->next;
        }

        lrk_theads_rm_nt(t, j);
        lrk_theads_rm_theads(t, k, t_heads);
    }

    return t_heads;
}

//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. END.
//////////////////////////////////////////////////////////////////
