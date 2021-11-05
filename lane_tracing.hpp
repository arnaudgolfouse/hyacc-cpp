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

#pragma once

#include "stack_config.hpp"
#include "y.hpp"
#include <cstddef>
#include <cstdint>
#include <vector>

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

    auto add_inc(int n) noexcept -> LlistInt*;
    void dump() const noexcept;
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

    /// Find the node in a LlistInt list whose first entry is n2.
    auto find_n2(int n2) noexcept -> LlistInt2*;
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
    void dump() const noexcept;
    /// Return:
    ///   the splitted state's no if state_no is in c->states list
    ///   -1 otherwise.
    ///
    /// Note state_no here is the virtual state_no: the one
    /// splitted from. So there could be more than one cluster
    /// contain it.
    [[nodiscard]] auto contain_state(int state_no) const noexcept -> int;
};

extern LtCluster* all_clusters;

/* Functions */

extern auto
lt_tbl_entry_find(State* from, const StateArray& states_new_array)
  -> LtTblEntry*;

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
using ConfigPairList = struct ConfigPairNode*;
struct ConfigPairNode
{
    Configuration* end;   // conflict_config
    Configuration* start; // lane_start_config
    ConfigPairNode* next;

    /*
     * Functions in lrk_util.cpp
     */
    static auto list_combine(ConfigPairList s, ConfigPairList t) noexcept
      -> ConfigPairList;

    static auto list_insert(ConfigPairList list,
                            Configuration* conflict_config,
                            Configuration* lane_start_config) noexcept
      -> ConfigPairList;
    static void list_dump(ConfigPairList list);
    static auto list_find(ConfigPairList list,
                          Configuration* conflict_config) noexcept
      -> ConfigPairNode*;
    static void list_destroy(ConfigPairList list) noexcept;
};

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

/*
 * Functions in lane_tracing.c
 */
extern void
trace_back_lrk(const Configuration* c0, Configuration* c);
extern void
trace_back_lrk_clear(const Configuration* c0, Configuration* c);

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
    void add_derivatives(const Grammar& grammar, ObjectItem* o, int j, int k);
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

    static auto create(int k) noexcept -> LRkPT*;
    void dump() const noexcept;
    auto find(int state, SymbolTblNode* token, bool* found) const noexcept
      -> LRkPTRow*;
    auto get_entry(int state,
                   SymbolTblNode* token,
                   const SymbolTblNode* col_token,
                   bool* exist) noexcept -> ConfigPairNode*;
    auto add_reduction(int state,
                       SymbolTblNode* token,
                       const SymbolTblNode* s,
                       Configuration* c,
                       Configuration* c_tail) noexcept -> bool;
};

//
// LR(k) parsing table array.
//
struct LRkPTArray
{
    std::vector<LRkPT*> array;

    explicit LRkPTArray() noexcept;

    /// `max_k` is `array.size() + 1`
    [[nodiscard]] constexpr inline auto max_k() const noexcept -> size_t
    {
        return this->array.size() + 1;
    }
    void add(LRkPT* t) noexcept;
    [[nodiscard]] auto get(int k) const noexcept -> LRkPT*;
    void dump() const noexcept;
    void dump_file() const noexcept;
};

//
// for (configuration, conflict context) pair
//
struct CfgCtxt
{
    Configuration* c;
    SymbolList ctxt; // conflict context symbols
    Configuration* tail;

    static auto create(Configuration* c,
                       SymbolList s,
                       Configuration* tail) noexcept -> CfgCtxt*;
    static void destroy(CfgCtxt* cc) noexcept;
    void dump() const noexcept;
};

// for LR(k) theads.
extern auto
lrk_theads(const Grammar& grammar, SymbolList alpha, int k)
  -> std::shared_ptr<List>;

/// @brief Holds the information passed to the lane-tracing algorithm.
class LaneTracing : public YAlgorithm
{
  public:
    explicit LaneTracing(const Grammar& grammar,
                         const Options& options,
                         NewStates& new_states,
                         std::optional<Queue>& config_queue)
      : YAlgorithm(grammar, options, new_states, config_queue)
    {}
    [[nodiscard]] auto lane_tracing() -> std::optional<LRkPTArray>;

  private:
    Stack lane;
    Stack stack;

    void phase1();
    void phase2();
    void gpm(State* new_state);
    auto add_split_state(State& y, State& s, size_t successor_index) -> bool;
    void update_state_reduce_action(State& s);
    void phase2_regeneration(laneHead* lh_list);
    void phase2_regeneration2();
    void set_transitors_pass_thru_on(const Configuration& cur_config,
                                     Configuration* o) const noexcept(false);
    void inherit_propagate(int state_no,
                           int parent_state_no,
                           LtCluster* container,
                           LtTblEntry* e);
    void lt_phase2_propagate_context_change(int state_no,
                                            LtCluster* c,
                                            LtTblEntry* e);
    void clear_regenerate(int state_no);
    void clear_inherit_regenerate(int state_no, int parent_state_no);
    auto cluster_trace_new_chain_all(int parent_state, const LtTblEntry* e)
      -> bool;
    auto cluster_trace_new_chain(int parent_state_no, int state_no) -> bool;
    auto cluster_add_lt_tbl_entry(LtCluster* c,
                                  int from_state,
                                  LlistContextSet* e_ctxt,
                                  size_t e_parent_state_no,
                                  bool copy) const -> int;
    auto get_the_context(const Configuration* o) const noexcept(false)
      -> SymbolNode*;
    auto trace_back(const Configuration* c0,
                    Configuration* c,
                    laneHead* lh_list) const noexcept(false) -> laneHead*;
    [[nodiscard]] auto get_state_conflict_lane_head(int state_no,
                                                    laneHead* lh_list) const
      noexcept(false) -> laneHead*;
    [[nodiscard]] auto get_conflict_lane_head() const noexcept(false)
      -> laneHead*;
    void get_inadequate_state_reduce_config_context(const State* s);
    void resolve_lalr1_conflicts();
    void do_loop() noexcept(false);
    void check_lane_top();
    void pop_lane();
    void check_stack_top();
    void dump_stacks() const;
    void stack_operation(int* fail_ct, Configuration* o);
    void context_adding_routine(SymbolList context_generated,
                                Configuration* o,
                                int cur_config_index,
                                int* fail_ct);
    /* used by both originator list and transitor list */
    void lane_tracing_reduction(Configuration* c) noexcept(false);

    // In `lrk.cpp`
    [[nodiscard]] auto lane_tracing_lrk() -> std::optional<LRkPTArray>;
    void edge_pushing(LRkPTArray& lrk_pt_array, int state_no);
    void lrk_config_lane_tracing(Configuration* c) noexcept;
};

// in the lane_tracing of edge_pushing.
extern bool IN_EDGE_PUSHING_LANE_TRACING;
extern Configuration* cur_red_config;
extern SymbolList EDGE_PUSHING_CONTEXT_GENERATED;
