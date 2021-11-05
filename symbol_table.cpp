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
    for (SymbolNode* a = grammar.non_terminal_list; a != nullptr; a = a->next) {
        std::cout << count++ << ": ";
        write_rule_id_list(*a->snode);
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

/*
 * Create a symbol node, used by Production, Context etc.
 */
auto
SymbolNode::create(std::shared_ptr<SymbolTableNode> sn) -> SymbolNode*
{
    if (sn == nullptr)
        throw std::runtime_error("SymbolNode::create error: sn is nullptr\n");

    SymbolNode* n = new SymbolNode;
    if (n == nullptr)
        throw std::runtime_error("SymbolNode::create error: out of memory\n");
    n->snode = sn;
    n->next = nullptr;
    return n;
}

void
free_symbol_node(SymbolNode* n)
{
    if (n == nullptr)
        return;
    delete n;
}

void
free_symbol_node_list(SymbolNode* a)
{
    while (a != nullptr) {
        SymbolNode* b = a->next;
        delete a;
        a = b;
    }
}

auto
find_in_symbol_list(SymbolList a,
                    const std::shared_ptr<const SymbolTableNode> s)
  -> SymbolNode*
{
    for (SymbolNode* b = a; b != nullptr; b = b->next) {
        if (b->snode == s)
            return b;
    }

    return nullptr;
}

auto
get_symbol_list_len(SymbolList a) -> int
{
    int len = 0;
    for (SymbolNode* b = a; b != nullptr; b = b->next) {
        len++;
    }
    return len;
}

/*
 * Given a symbol list a, returns a clone of it.
 */
auto
clone_symbol_list(const SymbolList a) -> SymbolList
{
    if (a == nullptr)
        return nullptr;
    SymbolNode* c = SymbolNode::create(a->snode);
    SymbolNode* clone = c;
    for (SymbolNode* b = a->next; b != nullptr; b = b->next) {
        c->next = SymbolNode::create(b->snode);
        c = c->next;
    }

    return clone;
}

/*
 * Remove s from list a.
 * @return: the new list.
 */
auto
remove_from_symbol_list(SymbolList a,
                        std::shared_ptr<SymbolTableNode> s,
                        bool* exist) -> SymbolList
{
    SymbolNode* b = nullptr;
    *exist = true;

    if (a->snode == s) { // is the first node.
        b = a;
        a = a->next;
        free_symbol_node(b);
        return a;
    }

    for (b = a; b->next != nullptr; b = b->next) {
        if (b->next->snode == s) {
            SymbolNode* tmp = b->next;
            b->next = tmp->next;
            free_symbol_node(tmp);
            return a;
        }
    }

    // b->next is nullptr. s is NOT in list a.
    *exist = false;
    return a;
}

/*
 * Find in a sorted (INCREMENTAL) list.
 */
auto
find_in_inc_symbol_list(SymbolList a, std::shared_ptr<SymbolTableNode> s)
  -> SymbolNode*
{
    for (SymbolNode* b = a; b != nullptr; b = b->next) {
        if (b->snode == s)
            return b;
        if (*s->symbol > *b->snode->symbol) {
            return nullptr;
        }
    }

    return nullptr;
}

/*
 * Insert symbol n into INC list a.
 * @Return: the result list.
 */
auto
insert_inc_symbol_list(SymbolList a, std::shared_ptr<SymbolTableNode> n)
  -> SymbolNode*
{
    SymbolNode *b = nullptr, *b_prev = nullptr;
    if (n == nullptr)
        return a;
    if (a == nullptr)
        return SymbolNode::create(n);

    for (b_prev = nullptr, b = a; b != nullptr; b_prev = b, b = b->next) {
        int cmp = n->symbol->compare(*b->snode->symbol);
        if (cmp < 0) {               // insert after b_prev, before b.
            if (b_prev == nullptr) { // insert at the head.
                b_prev = SymbolNode::create(n);
                b_prev->next = b;
                return b_prev;
            } // insert in the middle.
            b_prev->next = SymbolNode::create(n);
            b_prev->next->next = b;
            return a;
        }
        if (cmp > 0) { // go on.
            continue;
        } // equals. already exists.
        return a;
    }

    // b is nullptr. insert at the end.
    b_prev->next = SymbolNode::create(n);

    return a;
}

/*
 * Combine list b into a. Both lists are in INC order.
 * Returns the head of the combined list.
 * This can be used when combining contexts.
 */
auto
combine_inc_symbol_list(SymbolList a, SymbolList b) -> SymbolNode*
{
    if (a == nullptr)
        return clone_symbol_list(b);
    if (b == nullptr)
        return a;

    SymbolNode* na_prev = nullptr;
    SymbolNode* na = a;
    SymbolNode* nb = b;

    while (true) {
        int cmp = na->snode->symbol->compare(*nb->snode->symbol);
        // std::cout << "strcmp(" << *na->snode->symbol << ", "
        //           << *nb->snode->symbol << ") = " << cmp << std::endl;
        if (cmp == 0) {
            na_prev = na;
            na = na->next;
            nb = nb->next;
        } else if (cmp > 0) {         // insert b before na.
            if (na_prev == nullptr) { // insert at the head of a.
                na_prev = SymbolNode::create(nb->snode);
                na_prev->next = a;
                a = na_prev;
            } else { // insert in the middle of list a before na.
                na_prev->next = SymbolNode::create(nb->snode);
                na_prev->next->next = na;
            }
            nb = nb->next;
        } else { // cmp < 0.
            na_prev = na;
            na = na->next;
        }

        if (na == nullptr) {
            na_prev->next = clone_symbol_list(nb); // attach the rest of nb.
            break;
        }
        if (nb == nullptr) {
            break;
        }
        // writeSymbolList(a, "a");
    }

    return a;
}

void
write_symbol_list(SymbolList a, const std::string_view name)
{
    SymbolNode* b = a;
    std::cout << name << ": ";
    if (b != nullptr) {
        std::cout << *b->snode->symbol;
        for (b = b->next; b != nullptr; b = b->next) {
            std::cout << ", " << *b->snode->symbol;
        }
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
hash_value(const std::string_view symbol) -> int
{
    size_t len = symbol.size();

    int sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum = (sum + symbol[i]) % static_cast<int>(HT_SIZE);
    }

    if constexpr (DEBUG_HASHTBL) {
        std::cout << "hash value for " << symbol << " is " << sum << std::endl;
    }

    return sum;
}

/*
 * If the symbol exists, return the node,
 * otherwise create a node and return the node.
 *
 * So this contains the function of hash_tbl_find().
 * If it's unknown whether a symbol exists, use
 * this function to ensure getting a node contains
 * this symbol.
 */
auto
hash_tbl_insert(const std::string_view symbol)
  -> std::shared_ptr<SymbolTableNode>
{
    const int v = hash_value(symbol);
    if constexpr (DEBUG_HASHTBL) {
        std::cout << "hash insert " << symbol << " at " << v << std::endl;
    }

    if (HashTbl[v].next == nullptr) {
        HashTbl[v].next = std::make_shared<SymbolTableNode>(symbol);
        HashTbl[v].count++;
        return HashTbl[v].next;
    }
    std::shared_ptr<SymbolTableNode> n = HashTbl[v].next;
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
    HashTbl[v].count++;
    return n->next;
}

/*
 * Return the node for symbol.
 * If symbol does not exist, return nullptr.
 */
auto
hash_tbl_find(const std::string_view symbol) -> std::shared_ptr<SymbolTableNode>
{
    int v = hash_value(symbol);

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
       << ",vanish=" << (n->vanishable ? "true" : "false") << ", seq=" << n->seq
       << ", val=" << n->value;
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
