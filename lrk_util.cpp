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
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <vector>

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
ConfigPairNode::list_combine(ConfigPairList t, ConfigPairList s) noexcept
  -> ConfigPairList
{
    if (s == nullptr)
        return t;
    if (t == nullptr)
        return s;

    for (ConfigPairNode* n = s; n != nullptr; n = n->next) {
        t = ConfigPairNode::list_insert(t, n->end, n->start);
    }

    return t;
}

/*
 * Insert in INC order of:
 *   conflict_config's state_no and ruleID,
 *   lane_start_config's state_no and ruleID.
 */
auto
ConfigPairNode::list_insert(ConfigPairList list,
                            Configuration* conflict_config,
                            Configuration* lane_start_config) noexcept
  -> ConfigPairList
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
ConfigPairNode::list_dump(ConfigPairList list)
{
    std::cout << "--ConfigPairList--" << std::endl;
    for (ConfigPairNode* n = list; n != nullptr; n = n->next) {
        config_pair_node_dump(n);
        std::cout << std::endl;
    }
}

/*
 * Note that more than one LANE_END configurations could be found.
 */
auto
ConfigPairNode::list_find(ConfigPairList list,
                          Configuration* conflict_config) noexcept
  -> ConfigPairNode*
{
    for (ConfigPairNode* n = list; n != nullptr; n = n->next) {
        if (n->end == conflict_config) {
            return n;
        }
    }
    if constexpr (DEBUG_EDGE_PUSHING) {
        std::cout << "not found" << std::endl;
    }
    return nullptr;
}

void
ConfigPairNode::list_destroy(ConfigPairList list) noexcept
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
        std::cout << "(set is empty)" << std::endl;
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
List::create() -> std::shared_ptr<List>
{
    auto t = std::make_shared<List>();
    t->head = nullptr;
    t->tail = nullptr;
    t->count = 0;
    return t;
}

// insert new object at tail of list t,
// without checking if the object already exists.
void
List::insert_tail(void* object)
{
    if (object == nullptr)
        return;

    if (this->head == nullptr) {
        this->head = this->tail = object_item_new(object);
    } else {
        this->tail->next = object_item_new(object);
        this->tail = this->tail->next;
    }

    this->count++;
}

void
List::dump(void (*list_item_dump)(void*)) const
{
    if (this->head == nullptr) {
        std::cout << "(list is empty)" << std::endl;
        return;
    }

    std::cout << "list count: " << this->count << std::endl;

    int i = 0;
    for (ObjectItem* s = this->head; s != nullptr; s = s->next) {
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
LRkPT::create(int k) noexcept -> LRkPT*
{
    auto* t = new LRkPT;
    t->k = k;
    t->row_count = 0;
    t->rows = nullptr;
    return t;
}

void
LRkPT::dump() const noexcept
{
    if (this->rows == nullptr) {
        std::cout << "(empty LR(" << this->k << ") parsing table)" << std::endl;
        return;
    }

    for (const LRkPTRow* r = this->rows; r != nullptr; r = r->next) {
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
LRkPT::find(int state, SymbolTblNode* token, bool* found) const noexcept
  -> LRkPTRow*
{

    *found = false;
    LRkPTRow* r = this->rows;
    if (r == nullptr)
        return nullptr;

    LRkPTRow* r_prev = nullptr;
    for (; r != nullptr; r_prev = r, r = r->next) {
        if (r->state < state)
            continue;
        if (r->state > state)
            break; // not found
                   // found same state.
        const std::string& symb1 = *r->token->snode->symbol;
        const std::string& symb2 = *token->symbol;
        if (symb1 < symb2)
            continue;
        if (symb1 > symb2)
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
    r->token = SymbolNode::create(token);

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
LRkPT::get_entry(int state,
                 SymbolTblNode* token,
                 const SymbolTblNode* col_token,
                 bool* exist) noexcept -> ConfigPairNode*
{
    bool found = false;
    *exist = false;
    LRkPTRow* r = this->find(state, token, &found);
    if (found == false)
        return nullptr; // row not exist in t.

    *exist = true; // this entry exists.
    int index = get_col(*col_token);

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
LRkPT::add_reduction(int state,
                     SymbolTblNode* token,
                     const SymbolTblNode* s,
                     Configuration* c,
                     Configuration* c_tail) noexcept -> bool
{
    bool found = false;
    LRkPTRow* r = this->find(state, token, &found);

    if (found == false) { // insert new entry
        r = lrk_pt_add_row(this, r, state, token);
    }

    // now add the reduce action on token s.
    int index = get_col(*s);
    const ConfigPairNode* n = r->row[index];
    if (n == nullptr) {
        r->row[index] = config_pair_node_create(c_tail, c);
        return false;
    }
    const Configuration* prev_entry = n->end; /// start;
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
LRkPTArray::create() noexcept -> LRkPTArray*
{

    auto* a = new LRkPTArray;
    a->array = std::vector<LRkPT*>(10, nullptr);
    return a;
}

/*
 * Add t to a.
 */
void
LRkPTArray::add(LRkPT* t) noexcept
{
    if (nullptr == t)
        return;

    while (t->k - 2 >= this->array.size()) {
        this->array.push_back(nullptr);
    }
    if (this->array[t->k - 2] == nullptr)
        this->array[t->k - 2] = t;
    else
        std::cerr << "Error: LR(" << t->k << ") table already exists"
                  << std::endl;
}

/*
 * Get the LR(k) parsing table for k.
 */
auto
LRkPTArray::get(int k) const noexcept -> LRkPT*
{
    if (k < 2 || k > this->max_k())
        return nullptr;
    if (this->array[k - 2] == nullptr) {
        std::cerr << "Warning: LR(" << k << ") table is empty" << std::endl;
        return nullptr;
    }
    return this->array[k - 2];
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
LRkPTArray::dump() const noexcept
{
    write_parsing_tbl_col_hdr();
    std::cout << "===LRkPTArray_dump [max_k = " << this->max_k()
              << "]===" << std::endl;

    size_t i = 2;
    for (const LRkPT* elem : this->array) {
        std::cout << "LR(" << i << ") p.t." << std::endl;
        // TODO: check for nullptr ???
        elem->dump();
        i++;
    }

    std::cout << "====================================" << std::endl;
}

/*
 * Dump to disk.
 */
void
LRkPTArray::dump_file() const noexcept
{
    std::ofstream fp2;
    fp2.open("y.lrk");
    if (!fp2.is_open()) {
        std::cerr << "cannot open file y.lrk to write" << std::endl;
        return;
    }

    write_parsing_tbl_col_hdr_file(fp2);
    fp2 << "===LRkPTArray_dump [max_k = " << this->max_k()
        << "]===" << std::endl;

    size_t i = 2;
    for (const LRkPT* elem : this->array) {
        fp2 << "=LR(" << i << ") p.t." << std::endl;
        lrk_pt_dump_file(elem, fp2);
        i++;
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
CfgCtxt::create(Configuration* c, SymbolList s, Configuration* tail) noexcept
  -> CfgCtxt*
{
    auto* cc = new CfgCtxt;
    cc->c = c;
    cc->ctxt = s;
    cc->tail = tail;
    return cc;
}

void
CfgCtxt::destroy(CfgCtxt* cc) noexcept
{
    delete cc;
}

void
CfgCtxt::dump() const noexcept
{
    std::cout << "CfgCtxt: " << this->c->owner->state_no << "."
              << this->c->ruleID << " { ";
    for (SymbolList a = this->ctxt; a != nullptr; a = a->next) {
        std::cout << a->snode->symbol << " ";
    }
    std::cout << "}[tail: ";
    if (this->tail != nullptr) {
        std::cout << this->tail->owner->state_no << "." << this->tail->ruleID;
    }
    std::cout << "]" << std::endl;
}

//////////////////////////////////////////////////////////////////
// Functions for struct CfgCtxt. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. START.
//////////////////////////////////////////////////////////////////

constexpr bool DEBUG_LRK_THEADS_CYCLE = true;
constexpr bool DEBUG_LRK_THEADS = false;

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
    SymbolNode* cpy_tail = SymbolNode::create(pt->snode);
    SymbolNode* cpy = cpy_tail;
    int i = (is_vanish_symbol(*pt->snode) == false) ? 1 : 0;
    if (i == k)
        return cpy;

    for (pt = alpha->next; pt != nullptr; pt = pt->next) {
        cpy_tail->next = SymbolNode::create(pt->snode);
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
                      // std::cout << "--Yes truncate" << std::endl;
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

void
List::add_derivatives(ObjectItem* o, int j, int k)
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
            if (lrk_thead_in_list(this, new_list) == false) {
                this->insert_tail((void*)new_list);
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

void
List::lrk_theads_rm_nt(int j)
{
    if constexpr (DEBUG_LRK_THEADS) {
        std::cout << std::endl << "lrk_theads_rm_nt:" << std::endl;
    }

    if (this->head == nullptr)
        return;

    ObjectItem* o_prev = nullptr;
    ObjectItem* o = this->head;
    while (o != nullptr) {
        if (true == j_th_symbol_is_nt(static_cast<SymbolList>(o->object), j)) {
            this->count--;
            // remove o.
            if (o_prev == nullptr) {
                this->head = o->next;
                if (this->head == nullptr) {
                    this->tail = nullptr;
                }
                object_item_destroy(o);
                o = this->head;
            } else {
                o_prev->next = o->next;
                if (o_prev->next == nullptr) {
                    this->tail = o_prev;
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
    *len = -1;

    if (s == nullptr)
        return false;

    int i = 0;
    for (; (i < k) && (nullptr != s); i++) {
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

void
List::lrk_theads_rm_theads(int k, List* t_heads)
{
    int len = 0;

    if (this->head == nullptr)
        return;

    ObjectItem* o_prev = nullptr;
    ObjectItem* o = this->head;
    while (o != nullptr) {
        if (true ==
              k_heads_are_t(static_cast<SymbolList>(o->object), k, &len) ||
            (len > 0 && len < k)) {
            this->count--;
            if (len < k) {
                // k'-thead, where k' < k. do nothing
                if (len == k - 1) {
                    t_heads->insert_tail(o->object);
                }
            } else if (false ==
                       lrk_thead_in_list(t_heads, (SymbolList)o->object)) {
                // k-thead.
                t_heads->insert_tail(o->object);
            }
            // remove o.
            if (o_prev == nullptr) {
                this->head = o->next;
                if (this->head == nullptr) {
                    this->tail = nullptr;
                }
                object_item_destroy(o);
                o = this->head;
            } else {
                o_prev->next = o->next;
                if (o_prev->next == nullptr) {
                    this->tail = o_prev;
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
lrk_theads(SymbolList alpha, int k) -> std::shared_ptr<List>
{
    if (alpha == nullptr)
        return nullptr;

    SymbolList s = get_string_with_k_non_vanish_symbol(alpha, k);

    std::shared_ptr<List> t_heads = List::create(); // set of LR(k) theads.
    std::shared_ptr<List> t = List::create();
    t->insert_tail((void*)s);

    for (int j = 0; j < k; j++) {
        ObjectItem* o = t->head;
        while (o != nullptr) {
            t->add_derivatives(o, j, k);
            o = o->next;
        }

        t->lrk_theads_rm_nt(j);
        t->lrk_theads_rm_theads(k, t_heads.get());
    }

    return t_heads;
}

//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. END.
//////////////////////////////////////////////////////////////////
