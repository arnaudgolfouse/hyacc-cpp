/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
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
 * state_hash_table.c
 *
 * This hash table stores state numbers. It hashes a state by
 * a function of the rule index  and  marker  position of the
 * state's core configurations.
 *
 * This hash table is used to expidite the the process of fi-
 * nding a same or compatible state in the function addTrans-
 * itionStates2New().
 *
 * This is an alternative of the function  isExistingState(),
 * which uses linear search in all the states, and is slower.
 *
 * For example, for C.y, in -O0 (no optimization),  there are
 * 1605 states in the parsing machine,  so from the first  to
 * the last state as they are added, in average there will be
 * sum_{k=1_to_n}(k/2) = n(n+1)/4 searches,  where  n = 1605.
 * This is 644,407.
 *
 * But using this state hash table, in average there are 5.02
 * states per list (from StateHashTbl_dump() output),  so the
 * cost is (1 + 5.02) * 1605 = 9,662, where 1 is the  cost of
 * getting the hash value for a state.
 *
 * This is 644,407/9,662 = 67 times faster.
 *
 * @Author: Xin Chen
 * @Created on: 3/3/2006
 * @Last modified: 3/21/2007
 */

#include "y.hpp"
#include <array>

struct StateTableNode
{
    State* state;
    struct StateTableNode* next;
};
using StateTblNode = struct StateTableNode;

struct StateHashTblNode
{
    int count;
    StateTblNode* next;
};

constexpr size_t SHT_SIZE = 997; /* State hash table size */
std::array<StateHashTblNode, SHT_SIZE> StateHashTbl;

auto
create_state_node(State* s) -> StateTblNode*
{
    auto* n = new StateTblNode;
    n->state = s;
    n->next = nullptr;
    return n;
}

void
destroy_state_node(StateTblNode* n)
{
    delete n;
}

void
init_state_hash_tbl()
{
    for (int i = 0; i < SHT_SIZE; i++) {
        StateHashTbl[i].count = 0;
        StateHashTbl[i].next = nullptr;
    }
}

static auto
get_state_hash_val(State* s) -> int
{
    int sum = 0;
    for (int i = 0; i < s->core_config_count; i++) {
        sum = (sum + s->config[i]->ruleID * 97 + s->config[i]->marker * 7 + i) %
              static_cast<int>(SHT_SIZE);
    }
    return sum;
}

extern bool in_lanetracing;

/*
 * Search the state hash table for state s.
 * If not found, insert it and return nullptr.
 * else, return the found state.
 */
auto
search_state_hash_tbl(State* s, int* is_compatible) -> State*
{
    int v = get_state_hash_val(s);
    StateTblNode* n = StateHashTbl[v].next;
    StateTblNode* n_prev = nullptr;

    (*is_compatible) = 0; // default to 0 - false.

    if (n == nullptr) {
        StateHashTbl[v].next = create_state_node(s);
        StateHashTbl[v].count = 1;
        return nullptr;
    }

    while (n != nullptr) {
        n_prev = n;
        if (is_same_state(n->state, s) == true) {
            return n->state;
        }
        if (Options::get().use_combine_compatible_states) {
            if (is_compatible_states(n->state, s) == true) {
                combine_compatible_states(n->state, s);
                (*is_compatible) = 1;
                return n->state;
            }
        }
        n = n->next;
    }
    // n == nullptr, s does not exist. insert at end.
    n_prev->next = create_state_node(s);
    StateHashTbl[v].count++;

    return nullptr;
}

/*
 * Search the state hash table for state s.
 * If not found, insert it and return nullptr.
 * else, return the found state.
 *
 * Is similar to searchStateHashTbl, but does not use
 * weak compatibility.
 */
auto
search_same_state_hash_tbl(State* s) -> State*
{
    int v = get_state_hash_val(s);
    StateTblNode* n = StateHashTbl.at(v).next;
    StateTblNode* n_prev = nullptr;

    if (n == nullptr) {
        StateHashTbl.at(v).next = create_state_node(s);
        StateHashTbl.at(v).count = 1;
        return nullptr;
    }

    while (n != nullptr) {
        n_prev = n;
        if (is_same_state(n->state, s) == true) {
            return n->state;
        }
        n = n->next;
    }
    // n == nullptr, s does not exist. insert at end.
    n_prev->next = create_state_node(s);
    StateHashTbl.at(v).count++;

    return nullptr;
}

/*
 * load factor: number of entry / hash table size.
 * hash table cell usage:
 *   number of used hash table cells / hash table size.
 */
void
state_hash_tbl_dump()
{
    int states_count = 0, list_count = 0;
    StateTblNode* n = nullptr;

    yyprintf("\n--state hash table--\n");
    yyprintf("-----------------------\n");
    yyprintf("cell |   count  | state\n");
    yyprintf("-----------------------\n");
    for (int i = 0; i < SHT_SIZE; i++) {
        if (StateHashTbl.at(i).count == 0)
            continue;

        list_count++;
        states_count += StateHashTbl.at(i).count;

        yyprintf("[%3d] (count=%d) : ", i, StateHashTbl.at(i).count);
        if ((n = StateHashTbl.at(i).next) != nullptr) {
            yyprintf("%d", n->state->state_no);

            n = n->next;
            while (n != nullptr) {
                yyprintf(", %d", n->state->state_no);
                n = n->next;
            }
        }
        yyprintf("\n");
    }

    yyprintf("%d states, %d lists, in average %.2f states/list.\n",
             states_count,
             list_count,
             ((double)states_count) / list_count);
    yyprintf("load factor: %.2f, hash table cell usage: %.2f\n",
             ((double)states_count) / SHT_SIZE,
             ((double)list_count) / SHT_SIZE);
}
