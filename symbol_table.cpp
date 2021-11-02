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
#include <string>
#include <string_view>

constexpr bool DEBUG_HASHTBL = false;

/*******************************************
 * Functions for RuleIDNode.
 *******************************************/

auto
create_rule_id_node(int rule_id) -> RuleIDNode*
{
    auto* r = new RuleIDNode;
    if (r == nullptr)
        throw std::runtime_error("create_rule_id_node: out of memory\n");

    r->ruleID = rule_id;
    r->next = nullptr;
    return r;
}

static void
write_rule_id_list(const SymbolTblNode& n)
{
    RuleIDNode* a = n.ruleIDList;
    std::cout << *n.symbol << ": ";
    if (a != nullptr) {
        std::cout << a->ruleID;

        for (a = a->next; a != nullptr; a = a->next) {
            std::cout << ", " << a->ruleID;
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
SymbolNode::create(SymbolTblNode* sn) -> SymbolNode*
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
    if (a == nullptr)
        return;

    while (a != nullptr) {
        SymbolNode* b = a->next;
        delete a;
        a = b;
    }
}

auto
find_in_symbol_list(SymbolList a, const SymbolTblNode* s) -> SymbolNode*
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
remove_from_symbol_list(SymbolList a, SymbolTblNode* s, bool* exist)
  -> SymbolList
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
find_in_inc_symbol_list(SymbolList a, SymbolTblNode* s) -> SymbolNode*
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
insert_inc_symbol_list(SymbolList a, SymbolTblNode* n) -> SymbolNode*
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
 * Function for SymbolTblNode.
 *******************************************/

/*
 * create a symbol table node, used by hash table HashTbl.
 */
auto
create_symbol_tbl_node(const std::string_view symbol) -> SymbolTblNode*
{
    auto* n = new SymbolTblNode;
    if (n == nullptr)
        throw std::runtime_error(
          "create_symbol_tbl_node error: out of memory\n");
    n->symbol = std::make_shared<std::string>(symbol);
    n->next = nullptr;
    n->type = symbol_type::NEITHER;
    n->vanishable = false; // default value: FALSE
    n->seq = -1;
    n->ruleIDList = nullptr;
    n->TP = nullptr; // terminal property.
    n->value = 0;
    return n;
}

/*
 * Empty string and EOF '$' are _NEITHER type.
 */
static auto
get_symbol_type(SymbolTblNode* n) -> const std::string_view
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
test_hash_tbl()
{
    hash_tbl_insert("xin");
    hash_tbl_insert("abe");
    hash_tbl_insert("xin");
    hash_tbl_insert("");
    hash_tbl_insert("");

    hash_tbl_find("");
    hash_tbl_find("abe");
    hash_tbl_find("ooo");

    hash_tbl_dump();
    hash_tbl_destroy();
    std::cout << "---------------" << std::endl;
}

void
hash_tbl_init()
{
    for (int i = 0; i < HT_SIZE; i++) {
        HashTbl[i].count = 0;
        HashTbl[i].next = nullptr;
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
hash_tbl_insert(const std::string_view symbol) -> SymbolTblNode*
{
    if constexpr (DEBUG_HASHTBL) {
        // std::cout << "hash insert " << symbol << " at " << where <<
        // std::endl;
    }
    const int v = hash_value(symbol);

    if (HashTbl[v].next == nullptr) {
        HashTbl[v].next = create_symbol_tbl_node(symbol);
        HashTbl[v].count++;
        return HashTbl[v].next;
    }
    SymbolTblNode* n = HashTbl[v].next;
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
    n->next = create_symbol_tbl_node(symbol);
    HashTbl[v].count++;
    return n->next;
}

/*
 * Return the node for symbol.
 * If symbol does not exist, return nullptr.
 */
auto
hash_tbl_find(const std::string_view symbol) -> SymbolTblNode*
{
    int v = hash_value(symbol);

    for (SymbolTblNode* n = HashTbl.at(v).next; n != nullptr; n = n->next) {
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
    SymbolTblNode* nnext = nullptr;

    // std::cout << "--destroy hash table--" << std::endl;
    for (int i = 0; i < HT_SIZE; i++) {
        if (HashTbl[i].count > 0) {
            for (SymbolTblNode* n = HashTbl[i].next; n != nullptr; n = nnext) {
                nnext = n->next;
                // std::cout << "freeing node for " << *n->symbol << std::endl;
                destroy_rule_id_list(n->ruleIDList);
                delete n;
            }
        }
    }
}

void
symbol_tbl_node_dump(SymbolTblNode* n)
{
    *fp_v << *n->symbol << " [type=" << get_symbol_type(n)
          << ",vanish=" << (n->vanishable ? "T" : "F") << ",seq=" << n->seq
          << ",val=" << n->value;
    if (n->type == symbol_type::TERMINAL && n->TP != nullptr) {
        *fp_v << ",prec=" << n->TP->precedence
              << ",assoc=" << get_assoc_name(n->TP->assoc);
    }
    *fp_v << "]";
}

void
hash_tbl_dump()
{
    int count = 0, list_count = 0;
    SymbolTblNode* n = nullptr;

    *fp_v << std::endl << "\n--Hash table--" << std::endl;
    for (int i = 0; i < HT_SIZE; i++) {
        if (HashTbl[i].count > 0) {
            list_count++;
            *fp_v << "HashTbl[" << i << "] (count=" << HashTbl[i].count
                  << "): ";
            for (n = HashTbl[i].next; n->next != nullptr; n = n->next) {
                symbol_tbl_node_dump(n);
                *fp_v << ", ";
                count++;
            }
            symbol_tbl_node_dump(n);
            *fp_v << std::endl;
            count++;
        }
    }
    *fp_v << "--hash table size: " << HT_SIZE << "--" << std::endl;
    *fp_v << "--symbol count: " << count << ", load factor lamda (" << count
          << '/' << HT_SIZE << ") = " << std::setprecision(3)
          << ((double)count) / HT_SIZE << "--" << std::endl;
    *fp_v << "--list count: " << list_count << ". Hash Table cell usage ("
          << list_count << '/' << HT_SIZE << ") = " << std::setprecision(3)
          << ((double)list_count) / HT_SIZE << "--" << std::endl;
    *fp_v << "--symbols per list: " << std::setprecision(3)
          << ((double)count) / list_count << "--" << std::endl;
    // hash_tbl_destroy();
}
