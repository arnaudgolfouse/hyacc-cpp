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
#include <any>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <utility>
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

    if (n->start->owner->PASS_THRU == 1u) {
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

//////////////////////////////////////////////////////////////////
// Functions for Set. END.
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// Functions for List. START.
//////////////////////////////////////////////////////////////////

// create an empty list.
// auto
// List::create() -> std::shared_ptr<List>
// {
//     auto t = std::make_shared<List>();
//     t->head = nullptr;
//     t->tail = nullptr;
//     t->count = 0;
//     return t;
// }

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
        for (size_t i = 0; i < ParsingTblColHdr.size(); i++) {
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
        for (size_t i = 0; i < ParsingTblColHdr.size(); i++) {
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

/// @Return: found - true if found, false if not.
///          If found is true, return the row.
///          otherwise, return the row before the insertion point.
auto
LRkPT::find(int state,
            std::shared_ptr<SymbolTableNode> token,
            bool* found) const noexcept -> LRkPTRow*
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

/// Pre-assumption: t is not nullptr.
/// Insert the new row after r.
/// @Return: the inserted new row.
static auto
lrk_pt_add_row(LRkPT* t,
               LRkPTRow* r_prev,
               int state,
               std::shared_ptr<SymbolTableNode> token) -> LRkPTRow*
{
    auto* r = new LRkPTRow;
    r->state = state;
    r->token = std::make_shared<SymbolNode>(token);

    r->row = new ConfigPairNode*[ParsingTblColHdr.size()];
    for (size_t i = 0; i < ParsingTblColHdr.size(); i++) {
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

/// Get the entry [(state, token), col_token] in t.
///
/// Assumptions: t != nullptr.
auto
LRkPT::get_entry(int state,
                 std::shared_ptr<SymbolTableNode> token,
                 const std::shared_ptr<const SymbolTableNode> col_token,
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
                     std::shared_ptr<SymbolTableNode> token,
                     const std::shared_ptr<const SymbolTableNode> s,
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

LRkPTArray::LRkPTArray() noexcept
{
    this->array = std::vector<LRkPT*>(10, nullptr);
}

/*
 * Add t to a.
 */
void
LRkPTArray::add(LRkPT* t) noexcept
{
    if (nullptr == t)
        return;

    while (t->k >= this->array.size() + 2) {
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
LRkPTArray::get(size_t k) const noexcept -> LRkPT*
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
    std::cout << "--Parsing Table Column Header [Total: "
              << ParsingTblColHdr.size() << "]--" << std::endl;
    for (const auto& i : ParsingTblColHdr) {
        std::cout << i->symbol << " ";
    }
    std::cout << std::endl;
}

static void
write_parsing_tbl_col_hdr_file(std::ostream& fp)
{
    fp << "--Parsing Table Column Header [Total: " << ParsingTblColHdr.size()
       << "]--" << std::endl;
    for (const auto& i : ParsingTblColHdr) {
        fp << i->symbol << " ";
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
    auto* cc = new CfgCtxt(c, s, tail);
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
    for (const auto& a : this->ctxt) {
        std::cout << a.snode->symbol << " ";
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

constexpr bool DEBUG_LRK_THEADS = false;

/// Get the head of string alpha up to the k-th symbol that
/// does not vanish, or the entire alpha if less than k symbols
/// do not vanish.
/// @note k >= 2.
/// @return a COPY of the head of the original string is returned.
static auto
get_string_with_k_non_vanish_symbol(SymbolList& alpha, size_t k) -> SymbolList
{
    if (alpha.empty() || k <= 0)
        return SymbolList{};

    auto it = alpha.begin();
    SymbolList cpy_tail{};
    cpy_tail.emplace_back(it->snode);
    size_t i = (it->snode->is_vanish_symbol()) ? 0 : 1;
    if (i == k)
        return cpy_tail;

    ++it;
    for (; it != alpha.end(); ++it) {
        cpy_tail.emplace_back(it->snode);
        if (!it->snode->is_vanish_symbol())
            i++;
        if (i == k)
            break;
    }

    return cpy_tail;
}

/// n is a node in list new_list,
/// replace symbol n+1 with the RHS of rule numbered ruleID.
/// return the new list.
static void
replace_with_rhs(const Grammar& grammar,
                 SymbolList& new_list,
                 SymbolList::iterator& n,
                 int rule_id)
{
    SymbolList rhs = clone_symbol_list(grammar.rules[rule_id]->nRHS);
    if (rhs.empty()) {
        // in this case, rhs is empty list,
        // just remove n_prev->next from new_list.
        if (n == new_list.end()) {
            new_list.pop_front();
        } else {
            ++n;
            new_list.erase(n);
        }
        return;
    }

    if (n == new_list.end()) { // replace the first symbol with rhs list.
        SymbolList tmp = new_list;
        rhs.splice(rhs.end(), new_list);
        new_list = rhs;
    } else { // replace symbol n_prev->next in the middle.
        n++;
        new_list.splice(n, rhs);
    }
}

static auto
is_same_symbol_list(const SymbolList& a, const SymbolList& b) -> bool
{
    if (a.size() != b.size()) {
        return false;
    }
    auto ita = a.begin();
    auto itb = b.begin();
    for (; (ita != a.end()) && (itb != b.end()); ++ita, ++itb) {
        if (ita->snode != itb->snode)
            return false;
    }
    return false;
}

/// assumption: new_list != nullptr, t != nullptr.
static auto
lrk_thead_in_list(const List* t, const SymbolList& new_list) -> bool
{
    if (t == nullptr || new_list.empty()) {
        throw std::runtime_error(
          "lrk_thead_in_list ERROR: t is nullptr or new_list is empty");
    }

    for (const auto& o : t->inner) {
        if (is_same_symbol_list(new_list, o)) {
            return true;
        }
    }

    return false;
}

/// Assumption: k >= 2.
/// Truncate list s so it contains up to k non-vanishable symbols.
static void
lrk_theads_truncate_list_by_k(SymbolList& s, int k)
{
    int i = 0;
    size_t total = 0;
    for (const auto& t : s) {
        if (!t.snode->vanishable)
            i++;
        if (i >= k) {
            break;
        }
        total += 1;
    }
    while (s.size() > total) {
        s.pop_back();
    }
}

void
List::add_derivatives(const Grammar& grammar,
                      const SymbolList& m,
                      const size_t j,
                      const size_t k)
{
    // get the (j)-th symbol.
    auto m_it = m.begin();
    for (size_t i = 0; i < j; i++) {
        if (m_it == m.end()) {
            return;
        }
        ++m_it;
    }
    if (m_it == m.end()) {
        return;
    }

    for (RuleIDNode* r = m_it->snode->ruleIDList; r != nullptr; r = r->next) {
        SymbolList new_list = clone_symbol_list(m);
        // get the (j-1)-th symbol and store as n_prev.
        auto n_prev = new_list.begin();
        if (j != 0) {
            for (size_t i = 0; i < j - 1; i++) {
                n_prev++;
            }
        } else {
            n_prev = new_list.end(); // really ?
        }
        // TODO: this logic might be simpler :)
        replace_with_rhs(grammar, new_list, n_prev, r->rule_id);
        // assumption: !new_list.empty(), t != nullptr.
        if (!new_list.empty()) {
            lrk_theads_truncate_list_by_k(new_list, k);
            if (!lrk_thead_in_list(this, new_list)) {
                this->insert_tail(new_list);
            }
        }
    }
}

/// Assumption: s.length >= j.
///
/// Return true is the j-th symbol of s exists and is Non-terminal.
/// otherwise return false.
static auto
j_th_symbol_is_nt(const SymbolList& s, const size_t j) -> bool
{
    auto it = s.begin();
    for (size_t i = 0; i < j; i++) {
        if (it == s.end())
            return false;
        it++;
    }
    if (it == s.end())
        return false;

    return (it->snode->type == symbol_type::NONTERMINAL);
}

void
List::lrk_theads_rm_nt(const size_t j)
{
    if constexpr (DEBUG_LRK_THEADS) {
        std::cout << std::endl << "lrk_theads_rm_nt:" << std::endl;
    }

    if (this->inner.empty())
        return;

    for (auto it = this->inner.begin(); it != this->inner.end();) {
        if (j_th_symbol_is_nt(*it, j)) {
            this->inner.erase(it);
        } else {
            it++;
        }
    }
}

/// Assumption: k >= 2.
///
/// Returns true if the first k symbols are s are all terminals.
/// Otherwise returns false.
static auto
k_heads_are_t(const SymbolList& s, size_t k)
  -> std::pair<bool, std::optional<size_t>>
{
    if (s.empty())
        return { false, std::nullopt };

    size_t i = 0;
    auto it = s.cbegin();
    for (; it != s.cend(); it++) {
        if (i == k) {
            break;
        }
        if (it->snode->type == symbol_type::NONTERMINAL) {
            return { false, std::nullopt };
        }
        i += 1;
    }

    // s contains less than k symbols, and these are all terminals.
    if ((i < k) && (it == s.cend())) {
        return { false, i };
    }

    return { true, i };
}

void
List::lrk_theads_rm_theads(size_t k, List* t_heads)
{
    if (this->inner.empty())
        return;

    auto o = this->inner.begin();
    for (; o != this->inner.end();) {
        auto [k_heads_are_t_res, len] = k_heads_are_t(*o, k);
        if (k_heads_are_t_res || (len > 0 && len < k)) {
            this->count--; // TODO: what is this ???
            if (len < k) {
                // k'-thead, where k' < k. do nothing
                if (len == k - 1) {
                    t_heads->insert_tail(*o);
                }
            } else if (false == lrk_thead_in_list(t_heads, *o)) {
                // k-thead.
                t_heads->insert_tail(*o);
            }
            // remove o.
            this->inner.erase(o);
        } else {
            o++;
        }
    }
}

/// Find theads of length k for string alpha.
/// This is a set of strings.
auto
lrk_theads(const Grammar& grammar, SymbolList& alpha, const size_t k)
  -> std::shared_ptr<List>
{
    if (alpha.empty())
        return nullptr;

    SymbolList s = get_string_with_k_non_vanish_symbol(alpha, k);

    std::shared_ptr<List> t_heads =
      std::make_shared<List>(); // set of LR(k) theads.
    std::shared_ptr<List> t = std::make_shared<List>();
    t->insert_tail(s);

    for (size_t j = 0; j < k; j++) {
        auto o = t->inner.begin();
        while (o != t->inner.end()) {
            t->add_derivatives(grammar, *o, j, k);
            o++;
        }

        t->lrk_theads_rm_nt(j);
        t->lrk_theads_rm_theads(k, t_heads.get());
    }

    return t_heads;
}

//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. END.
//////////////////////////////////////////////////////////////////
