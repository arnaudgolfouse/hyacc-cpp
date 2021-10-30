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

#ifndef _LANE_TRACING_H_
#define _LANE_TRACING_H_

#include "y.hpp"
#include <cstdint>

/*
 * lane_tracing.h
 *
 * Used by lane_tracing.c and lrk.c only.
 *
 * @Author: Xin Chen
 * @Created on: 7/26/2008
 * @Last modified: 3/24/2009
 */

constexpr bool DEBUG_EDGE_PUSHING = false;

/*
 * A macro to determine if a config is the goal production
 * of state 0. Used for getting the "$end" context
 * before test_a() in lane-tracing.
 * Single it out and put here, so it's easier to understand.
 */
constexpr inline auto
is_goal(Configuration& o) -> bool
{
    return o.owner->state_no == 0 && o.ruleID == 0;
}

/** Data structure for the state-combination in phase 2 table. START */

struct LlistInt
{
    int n;
    LlistInt* next;
};

/*
 * Similar to llist_int, but has two int fields.
 * Used by LT_cluster only.
 */
struct LlistInt2
{
    int n1;
    int n2;
    LlistInt2* next;
};

struct LlistContextSet
{
    Configuration* config; // in INC order of config->ruleID.
    SymbolList ctxt;       // in INC order of symbol.
    LlistContextSet* next;
};

/*
 * to get a State pointer from the state_no, use
 * states_new_array->state_list[state_no].
 */
struct LtTblEntry
{
    bool processed; // whether this entry was processed during regeration.
    int from_state;
    LlistContextSet* ctxt_set; // in INC order of config->ruleID.
    LlistInt* to_states;       // in INC order of state_no.
    LtTblEntry* next;
};

/*
 * For state combining purpose.
 */
struct LtCluster
{
    bool pairwise_disjoint;
    LlistInt2* states;         // in INC order of state_no.
    LlistContextSet* ctxt_set; // in INC order of config->ruleID.
    LtCluster* next;

    static auto find_actual_containing_cluster(int state_no) -> LtCluster*;
};

extern LtCluster* all_clusters;

/* Functions */

extern auto
lt_tbl_entry_find(State* from) -> LtTblEntry*;
extern void
cluster_dump(LtCluster* c);
extern auto
llist_int_add_inc(LlistInt* list, int n) -> LlistInt*;
extern auto
cluster_contain_state(const LtCluster* c, int state_no) -> int;
extern auto
llist_int2_find_n2(LlistInt2* list, int n2) -> LlistInt2*;
extern void
llist_int_dump(LlistInt* list);

/*
 * Data structures in lrk.c
 */

/*
 * For conflicting lanes' head states and associated conflicting contexts.
 */
using laneHead = struct laneHeadState;
struct laneHeadState
{
    State* s;            // conflicting lane head state.
    SymbolList contexts; // list of conflicting contexts.
    laneHead* next;
};

/*
 * For (conflict_config, lane_end_config) pairs.
 */
struct ConfigPairNode
{
    Configuration* end;   // conflict_config
    Configuration* start; // lane_start_config
    ConfigPairNode* next;
};
using ConfigPairList = ConfigPairNode*;

extern ConfigPairList lane_head_tail_pairs;

/*
 * For parsing table extention on LR(k).
 */

struct LRkContextListNode
{
    int k;                    // level of k in LR(k).
    SymbolList context;       // context symbols.
    int context_count;        // basically this is useless but keep it here.
    LRkContextListNode* next; // to next context list of higher k in LR(k)
};

struct LRkConfigListNode
{
    Configuration* config;
    LRkContextListNode* ctxt_list;
    LRkConfigListNode* next;
};

struct LRkPtEntry
{
    int state;
    SymbolTblNode* token;
    LRkConfigListNode* cfg_list;
    int conflict_count; // 0 if no conflict. can be > 1, but counted as 1
                        // when report in statistics.
    LRkPtEntry* next;
};

extern LRkPtEntry* LRk_PT; // extension parsing table for LR(k).

/*
 * Functions in lane_tracing.c
 */
extern auto
trace_back(const Configuration* c0, Configuration* c, laneHead* lh_list)
  -> laneHead*;
extern void
trace_back_lrk(const Configuration* c0, Configuration* c);
extern void
trace_back_lrk_clear(const Configuration* c0, Configuration* c);

/*
 * Functions in lrk.cpp
 */
extern void
lane_tracing_lrk();

/*
 * Functions in lrk_util.cpp
 */
extern auto
config_pair_list_combine(ConfigPairList t, ConfigPairList s) -> ConfigPairList;
extern auto
config_pair_list_insert(ConfigPairList list,
                        Configuration* conflict_config,
                        Configuration* lane_start_config) -> ConfigPairList;
extern void
config_pair_list_dump(ConfigPairList list);
extern auto
config_pair_list_find(ConfigPairList list, Configuration* conflict_config)
  -> ConfigPairNode*;
extern void
config_pair_list_destroy(ConfigPairList list);

// Set - a linked list of objects.
struct ObjectItem
{
    void* object;
    ObjectItem* next;
};
using Set = ObjectItem;
extern auto
set_insert(Set* set, void* object) -> Set*;
extern auto
set_find(Set* set, void* object) -> ObjectItem*;
extern auto
set_delete(Set* set, void* object) -> Set*;
extern void
set_dump(const Set* set, void (*set_item_dump)(void*));

// List - a single linked list.
struct List
{
    int count;
    ObjectItem* head;
    ObjectItem* tail;

    static auto create() -> std::shared_ptr<List>;
    // insert new object at tail of list t,
    // without checking if the object already exists.
    void insert_tail(void* object);
    // Remove from list t all strings whose j-th symbol is non-terminal.
    void lrk_theads_rm_nt(int j);
    // Remove from t all strings whose k-heads consist entirely
    // of terminals, and add the k-heads to set t_heads;
    void lrk_theads_rm_theads(int k, List* t_heads);
    // Add to the end of list the result of applying all possible
    // productions to the j-th symbol, omitting existing strings,
    // and truncate until it contains no more than k non-vanishable
    // symbols.
    void add_derivatives(ObjectItem* o, int j, int k);
    void dump(void (*list_item_dump)(void*)) const;
};

extern void
print_symbol_list(void* object);

//
// LRk_P_T - LR(k) parsing table.
//
constexpr uintptr_t CONST_CONFLICT_SYMBOL = -10000010;
struct LRkPTRow
{
    int state;
    SymbolNode* token;
    ConfigPairNode** row;
    LRkPTRow* next;
};

constexpr size_t LRK_P_T_INIT_SIZE = 10;
constexpr size_t LRK_P_T_INC = 10;

// LR(k) parsing table.
struct LRkPT
{
    int k;         // k in LR(k).
    int row_count; // number of rows.
    LRkPTRow* rows;
};

extern auto
lrk_pt_create(int k) -> LRkPT*;
extern void
lrk_pt_dump(const LRkPT* t);
extern auto
lrk_pt_find(const LRkPT* t, int state, SymbolTblNode* token, bool* found)
  -> LRkPTRow*;
extern auto
lrk_pt_get_entry(LRkPT* t,
                 int state,
                 SymbolTblNode* token,
                 SymbolTblNode* col_token,
                 bool* exist) -> ConfigPairNode*;
extern auto
lrk_pt_add_reduction(LRkPT* t,
                     int state,
                     SymbolTblNode* token,
                     SymbolTblNode* s,
                     Configuration* c,
                     Configuration* c_tail) -> bool;

//
// LR(k) parsing table array.
//
struct LRkPTArray
{
    LRkPT** array;
    int max_k; // number of entries in array is max_k - 1.
    int size;  // 0 <= max_k - 2 <= size - 1
};

extern LRkPTArray* lrk_pt_array; // defined in lrk.c

extern auto
lrk_pt_array_create() -> LRkPTArray*;
extern void
lrk_pt_array_add(LRkPTArray* a, LRkPT* t);
extern auto
lrk_pt_array_get(LRkPTArray* a, int k) -> LRkPT*;
extern void
lrk_pt_array_dump(LRkPTArray* a);
extern void
lrk_pt_array_dump_file(LRkPTArray* a);

//
// for (configuration, conflict context) pair
//
struct CfgCtxt
{
    Configuration* c;
    SymbolList ctxt; // conflict context symbols
    Configuration* tail;
};

extern auto
cfg_ctxt_create(Configuration* c, SymbolList s, Configuration* tail)
  -> CfgCtxt*;
extern void
cfg_ctxt_destroy(CfgCtxt* cc);
extern void
cfg_ctxt_dump(const CfgCtxt* cc);

// for LR(k) theads.
extern auto
lrk_theads(SymbolList alpha, int k) -> std::shared_ptr<List>;

// in the lane_tracing of edge_pushing.
extern bool IN_EDGE_PUSHING_LANE_TRACING;
extern Configuration* cur_red_config;
extern SymbolList EDGE_PUSHING_CONTEXT_GENERATED;

#endif
