/*
   This file is part of Hyacc, a LR(1) parser generator.
   Copyright (C) 2007 Xin Chen. chenx@hawaii.edu

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
 * symbol_table.c
 *
 * Stores all symbols of the grammar and relevant information
 * of these symbols.
 *
 * @Author: Xin Chen
 * @Created on: 2/16/2007
 * @Last modified: 3/21/2007
 * @Copyright (C) 2007
 */

#include "y.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <string_view>

constexpr bool DEBUG_HASHTBL = false;

/*******************************************
 * Functions for RuleIDNode.
 *******************************************/

auto
create_rule_id_node(size_t rule_id) -> RuleIDNode*
{
    auto* r = new RuleIDNode;
    if (r == nullptr)
        throw std::runtime_error("create_rule_id_node: out of memory\n");

    r->rule_id = rule_id;
    r->next = nullptr;
    return r;
}

static void
write_rule_id_list(const SymbolTableNode& n)
{
    RuleIDNode* a = n.ruleIDList;
    std::cout << *n.symbol << ": ";
    if (a != nullptr) {
        std::cout << a->rule_id;

        for (a = a->next; a != nullptr; a = a->next) {
            std::cout << ", " << a->rule_id;
        }
    }
    std::cout << std::endl;
}

static void
write_non_terminal_rule_id_list(const Grammar& grammar)
{
    int count = 0;
    std::cout << "--Nonterminal symbol rule index list--" << std::endl;
    for (const SymbolNode& a : grammar.non_terminal_list) {
        std::cout << count++ << ": ";
        write_rule_id_list(*a.snode);
    }
}

void
destroy_rule_id_list(RuleIDNode* r)
{
    if (r == nullptr)
        return;

    RuleIDNode* a = r;
    while (a != nullptr) {
        RuleIDNode* b = a->next;
        delete a;
        a = b;
    }
}

/*******************************************
 * Functions for SymbolNode.
 *******************************************/

void
free_symbol_node(SymbolNode* n)
{
    if (n == nullptr)
        return;
    delete n;
}

auto
find_in_symbol_list(SymbolList& a,
                    const std::shared_ptr<const SymbolTableNode> s)
  -> SymbolNode*
{
    for (auto& b : a) {
        if (b.snode == s) {
            return &b;
        }
    }

    return nullptr;
}

/// Given a symbol list a, returns a clone of it.
auto
clone_symbol_list(const SymbolList& a) -> SymbolList
{
    return a;
}

/// Remove s from list a.
/// @return `true` if `s` was found in `a`.
auto
remove_from_symbol_list(SymbolList& a, std::shared_ptr<SymbolTableNode> s)
  -> bool
{
    for (auto it = a.begin(); it != a.end(); it++) {
        if (it->snode == s) {
            a.erase(it);
            return true;
        }
    }
    return false;
}

/// Find in a sorted (INCREMENTAL) list.
auto
find_in_inc_symbol_list(SymbolList& a, std::shared_ptr<SymbolTableNode> s)
  -> SymbolNode*
{
    for (SymbolNode& b : a) {
        if (b.snode == s)
            return &b;
        if (*s->symbol > *b.snode->symbol) {
            return nullptr;
        }
    }

    return nullptr;
}

/// Insert symbol n into INC list a.
void
insert_inc_symbol_list(SymbolList& a, std::shared_ptr<SymbolTableNode> n)
{
    if (n == nullptr) {
        return;
    }

    for (auto it = a.begin(); it != a.end(); it++) {
        int cmp = n->symbol->compare(*it->snode->symbol);
        if (cmp < 0) {
            a.emplace(it, n);
            return;
        }
        if (cmp > 0) { // go on.
            continue;
        }
        return; // equals. already exists.
    }

    a.emplace_back(n);
}

/*
 * Combine list b into a. Both lists are in INC order.
 * Returns the head of the combined list.
 * This can be used when combining contexts.
 */
void
combine_inc_symbol_list(SymbolList& a, const SymbolList& b)
{
    if (a.empty())
        a = b;
    if (b.empty())
        return;

    auto na = a.begin();
    auto nb = b.begin();

    while (true) {
        int cmp = na->snode->symbol->compare(*nb->snode->symbol);
        if (cmp == 0) {
            na++;
            nb++;
        } else if (cmp > 0) { // insert b before na.
            a.insert(na, *nb);
            nb++;
        } else { // cmp <= 0.
            na++;
        }

        if (na == a.end()) {
            for (; nb != b.end(); nb++) {
                a.push_back(*nb);
            }
            break;
        }
        if (nb == b.end()) {
            break;
        }
    }
}

void
write_symbol_list(const SymbolList& a, const std::string_view name)
{
    std::cout << name << ": ";
    bool first = true;
    for (const auto& b : a) {
        if (first) {
            first = false;
        } else {
            std::cout << ", ";
        }
        std::cout << *b.snode->symbol;
    }
    if (!a.empty()) {
        std::cout << std::endl;
    }
}

/*******************************************
 * Function for SymbolTableNode.
 *******************************************/

/*
 * Empty string and EOF '$' are _NEITHER type.
 */
static auto
get_symbol_type(const std::shared_ptr<const SymbolTableNode> n)
  -> const std::string_view
{
    if (n->type == symbol_type::TERMINAL)
        return "T";
    if (n->type == symbol_type::NONTERMINAL)
        return "NT";
    return "-";
}

static auto
get_assoc_name(associativity a) -> const std::string_view
{
    if (a == associativity::LEFT)
        return "left";
    if (a == associativity::RIGHT)
        return "right";
    if (a == associativity::NONASSOC)
        return "none";
    return "--unknown--";
}

/*******************************************
 * Hash table functions.
 *******************************************/

void
hash_tbl_init()
{
    for (auto& elem : HashTbl) {
        elem.count = 0;
        elem.next = nullptr;
    }

    if constexpr (DEBUG_HASHTBL) {
        std::cout << "size of hash table = " << sizeof(HashTbl) << std::endl;
    }
    // testHashTbl();
}

/// @note Empty string is allowed.
static auto
hash_value(const std::string_view symbol) -> size_t
{
    size_t len = symbol.size();

    size_t sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum = (sum + static_cast<size_t>(symbol[i])) % HT_SIZE;
    }

    if constexpr (DEBUG_HASHTBL) {
        std::cout << "hash value for " << symbol << " is " << sum << std::endl;
    }

    return sum;
}

/// If the symbol exists, return the node,
/// otherwise create a node and return the node.
///
/// So this contains the function of hash_tbl_find().
/// If it's unknown whether a symbol exists, use
/// this function to ensure getting a node contains
/// this symbol.
auto
hash_tbl_insert(const std::string_view symbol)
  -> std::shared_ptr<SymbolTableNode>
{
    const size_t v = hash_value(symbol);
    // no lint since we know that `v` is in bounds.
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-constant-array-index)
    auto& cell = HashTbl[v];
    if constexpr (DEBUG_HASHTBL) {
        std::cout << "hash insert " << symbol << " at " << v << std::endl;
    }
    if (cell.next == nullptr) {
        cell.next = std::make_shared<SymbolTableNode>(symbol);
        cell.count++;
        return cell.next;
    }
    std::shared_ptr<SymbolTableNode> n = cell.next;
    for (; n->next != nullptr; n = n->next) {
        if (*n->symbol == symbol) {
            if constexpr (DEBUG_HASHTBL) {
                std::cout << "node for string " << symbol << " exists"
                          << std::endl;
            }
            return n;
        }
    }
    // the last node on this linked list.
    if (*n->symbol == symbol) {
        if constexpr (DEBUG_HASHTBL) {
            std::cout << "node for string " << symbol << " exists" << std::endl;
        }
        return n;
    }
    n->next = std::make_shared<SymbolTableNode>(symbol);
    cell.count++;
    return n->next;
}

/*
 * Return the node for symbol.
 * If symbol does not exist, return nullptr.
 */
auto
hash_tbl_find(const std::string_view symbol) -> std::shared_ptr<SymbolTableNode>
{
    const size_t v = hash_value(symbol);

    for (std::shared_ptr<SymbolTableNode> n = HashTbl.at(v).next; n != nullptr;
         n = n->next) {
        if (*n->symbol == symbol) {
            if constexpr (DEBUG_HASHTBL) {
                std::cout << "node for " << symbol << " is found" << std::endl;
            }
            return n;
        }
    }

    if constexpr (DEBUG_HASHTBL) {
        std::cout << "node for " << symbol << " is NOT found" << std::endl;
    }

    return nullptr;
}

void
hash_tbl_destroy()
{
    std::shared_ptr<SymbolTableNode> nnext = nullptr;

    // std::cout << "--destroy hash table--" << std::endl;
    for (auto& elem : HashTbl) {
        if (elem.count > 0) {
            for (std::shared_ptr<SymbolTableNode> n = elem.next; n != nullptr;
                 n = nnext) {
                nnext = n->next;
                // std::cout << "freeing node for " << *n->symbol << std::endl;
                destroy_rule_id_list(n->ruleIDList);
            }
        }
    }
}

static void
symbol_tbl_node_dump(std::ostream& os,
                     const std::shared_ptr<const SymbolTableNode> n)
{
    os << *n->symbol << " [type=" << get_symbol_type(n)
       << ",vanish=" << (n->vanishable ? "true" : "false")
       << ", seq=" << *n->seq << ", val=" << n->value;
    if (n->type == symbol_type::TERMINAL && n->TP != nullptr) {
        os << ", prec=" << n->TP->precedence
           << ", assoc=" << get_assoc_name(n->TP->assoc);
    }
    os << "]";
}

void
hash_tbl_dump(std::ostream& os)
{
    int count = 0, list_count = 0;

    os << std::endl << "\n--Hash table--" << std::endl;
    size_t i = 0;
    for (const auto& elem : HashTbl) {
        if (elem.count > 0) {
            list_count++;
            os << "HashTbl[" << i << "] (count=" << elem.count << "): ";
            std::shared_ptr<const SymbolTableNode> n = elem.next;
            for (; n->next != nullptr; n = n->next) {
                symbol_tbl_node_dump(os, n);
                os << ", ";
                count++;
            }
            symbol_tbl_node_dump(os, n);
            os << std::endl;
            count++;
        }
        i++;
    }
    os << "--hash table size: " << HT_SIZE << "--" << std::endl;
    os << "--symbol count: " << count << ", load factor lamda (" << count << '/'
       << HT_SIZE << ") = " << std::setprecision(3) << ((double)count) / HT_SIZE
       << "--" << std::endl;
    os << "--list count: " << list_count << ". Hash Table cell usage ("
       << list_count << '/' << HT_SIZE << ") = " << std::setprecision(3)
       << ((double)list_count) / HT_SIZE << "--" << std::endl;
    os << "--symbols per list: " << std::setprecision(3)
       << ((double)count) / list_count << "--" << std::endl;
    // hash_tbl_destroy();
}
