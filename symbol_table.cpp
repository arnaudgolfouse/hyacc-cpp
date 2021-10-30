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
#include <iostream>

#define DEBUG_HASHTBL 0

/*******************************************
 * Functions for RuleIDNode.
 *******************************************/

auto
create_rule_id_node(int rule_id) -> RuleIDNode*
{
    auto* r = new RuleIDNode;
    if (r == nullptr)
        YYERR_EXIT("createRuleIDNode: out of memory\n");

    r->ruleID = rule_id;
    r->next = nullptr;
    return r;
}

void
write_rule_id_list(SymbolTblNode* n)
{
    RuleIDNode* a = nullptr;
    std::cout << n->symbol << ": ";
    if ((a = n->ruleIDList) != nullptr) {
        std::cout << a->ruleID;

        for (a = a->next; a != nullptr; a = a->next) {
            std::cout << ", " << a->ruleID;
        }
    }
    std::cout << std::endl;
}

void
write_non_terminal_rule_id_list()
{
    int count = 0;
    std::cout << "--Nonterminal symbol rule index list--" << std::endl;
    for (SymbolNode* a = grammar.non_terminal_list; a != nullptr; a = a->next) {
        std::cout << count++ << ": ";
        write_rule_id_list(a->snode);
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
        YYERR_EXIT("createSymbolNode error: snode is nullptr\n");

    SymbolNode* n = new SymbolNode;
    if (n == nullptr)
        YYERR_EXIT("createSymbolNode error: out of memory\n");
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
find_in_symbol_list(SymbolList a, SymbolTblNode* s) -> SymbolNode*
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
remove_from_symbol_list(SymbolList a, SymbolTblNode* s, int* exist)
  -> SymbolList
{
    SymbolNode* b = nullptr;
    *exist = 1;

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
    *exist = 0;
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
        if (strcmp(s->symbol, b->snode->symbol) > 0) {
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
    if (nullptr == n)
        return a;
    if (nullptr == a)
        return SymbolNode::create(n);

    for (b_prev = nullptr, b = a; b != nullptr; b_prev = b, b = b->next) {
        int cmp = strcmp(n->symbol, b->snode->symbol);
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
        int cmp = strcmp(na->snode->symbol, nb->snode->symbol);
        // printf("strcmp(%s, %s) = %d\n",
        //        na->snode->symbol, nb->snode->symbol, cmp);
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
write_symbol_list(SymbolList a, const char* name)
{
    SymbolNode* b = a;
    if (name != nullptr)
        std::cout << name << ": ";
    if (b != nullptr) {
        std::cout << b->snode->symbol;
        for (b = b->next; b != nullptr; b = b->next) {
            std::cout << ", " << b->snode->symbol;
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
create_symbol_tbl_node(const char* symbol) -> SymbolTblNode*
{
    if (symbol == nullptr)
        YYERR_EXIT("createSymbolTblNode error: symbol is nullptr\n");

    auto* n = new SymbolTblNode;
    if (n == nullptr)
        YYERR_EXIT("createSymbolTblNode error: out of memory\n");
    n->symbol = new char[strlen(symbol) + 1];
    strcpy(n->symbol, symbol);
    n->next = nullptr;
    n->type = symbol_type::NEITHER;
    n->vanishable = 0; // default value: FALSE
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
get_symbol_type(SymbolTblNode* n) -> const char*
{
    if (n->type == symbol_type::TERMINAL)
        return "T";
    if (n->type == symbol_type::NONTERMINAL)
        return "NT";
    return "-";
}

static auto
get_assoc_name(associativity a) -> const char*
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

#if DEBUG_HASHTBL
    printf("size of hash table = %ld\n", sizeof(HashTbl));
#endif
    // testHashTbl();
}

/*
 * Assumption: symbol != nullptr.
 * empty string is allowed.
 */
static auto
hash_value(const char* symbol) -> int
{
    size_t len = strlen(symbol);

    int sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum = (sum + symbol[i]) % static_cast<int>(HT_SIZE);
    }

#if DEBUG_HASHTBL
    printf("hash value for %s is %d\n", symbol, sum);
#endif

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
hash_tbl_insert(const char* symbol) -> SymbolTblNode*
{
    int v = 0;
    SymbolTblNode* n = nullptr;

    if (symbol == nullptr)
        return nullptr;
#if DEBUG_HASHTBL
    printf("hash insert %s at %d\n", symbol, where);
#endif
    v = hash_value(symbol);

    if (HashTbl[v].next == nullptr) {
        HashTbl[v].next = create_symbol_tbl_node(symbol);
        HashTbl[v].count++;
        return HashTbl[v].next;
    }
    for (n = HashTbl[v].next; n->next != nullptr; n = n->next) {
        if (strcmp(n->symbol, symbol) == 0) {
#if DEBUG_HASHTBL
            printf("node for string %s exists\n", symbol);
#endif
            return n;
        }
    }
    // the last node on this linked list.
    if (strcmp(n->symbol, symbol) == 0) {
#if DEBUG_HASHTBL
        printf("node for string %s exists\n", symbol);
#endif
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
hash_tbl_find(const char* symbol) -> SymbolTblNode*
{
    if (symbol == nullptr)
        return nullptr;
    int v = hash_value(symbol);

    for (SymbolTblNode* n = HashTbl.at(v).next; n != nullptr; n = n->next) {
        if (strcmp(n->symbol, symbol) == 0) {
#if DEBUG_HASHTBL
            printf("node for %s is found\n", symbol);
#endif
            return n;
        }
    }

#if DEBUG_HASHTBL
    printf("node for %s is NOT found\n", symbol);
#endif

    return nullptr;
}

void
hash_tbl_destroy()
{
    SymbolTblNode* nnext = nullptr;

    // printf("--destroy hash table--\n");
    for (int i = 0; i < HT_SIZE; i++) {
        if (HashTbl[i].count > 0) {
            for (SymbolTblNode* n = HashTbl[i].next; n != nullptr; n = nnext) {
                nnext = n->next;
                // printf("freeing node for %s\n", n->symbol);
                delete[] n->symbol;
                destroy_rule_id_list(n->ruleIDList);
                delete n;
            }
        }
    }
}

void
symbol_tbl_node_dump(SymbolTblNode* n)
{
    yyprintf("%s [type=%s,vanish=%s,seq=%d,val=%d",
             n->symbol,
             get_symbol_type(n),
             n->vanishable ? "T" : "F",
             n->seq,
             n->value);
    if (n->type == symbol_type::TERMINAL && n->TP != nullptr) {
        yyprintf(
          ",prec=%d,assoc=%s", n->TP->precedence, get_assoc_name(n->TP->assoc));
    }
    yyprintf("]");
}

void
hash_tbl_dump()
{
    int count = 0, list_count = 0;
    SymbolTblNode* n = nullptr;

    yyprintf("\n\n--Hash table--\n");
    for (int i = 0; i < HT_SIZE; i++) {
        if (HashTbl[i].count > 0) {
            list_count++;
            yyprintf("HashTbl[%d] (count=%d): ", i, HashTbl[i].count);
            for (n = HashTbl[i].next; n->next != nullptr; n = n->next) {
                symbol_tbl_node_dump(n);
                yyprintf(", ");
                count++;
            }
            symbol_tbl_node_dump(n);
            yyprintf("\n");
            count++;
        }
    }
    yyprintf("--hash table size: %d--\n", HT_SIZE);
    yyprintf("--symbol count: %d, load factor lamda (%d/%d) = %.3f--\n",
             count,
             count,
             HT_SIZE,
             ((double)count) / HT_SIZE);
    yyprintf("--list count: %d. Hash Table cell usage (%d/%d) = %.3f--\n",
             list_count,
             list_count,
             HT_SIZE,
             ((double)list_count) / HT_SIZE);
    yyprintf("--symbols per list: %.3f--\n", ((double)count) / list_count);

    // hash_tbl_destroy();
}
