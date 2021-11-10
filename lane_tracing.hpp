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
#include <any>
#include <cstddef>
#include <cstdint>
#include <forward_list>
#include <fstream>
#include <utility>
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

/// Determine if a config is the goal production of state 0. Used for getting
/// the "$end" context before test_a() in lane-tracing.
///
/// Single it out and put here, so it's easier to understand.
constexpr inline auto
is_goal(Configuration& o) -> bool
{
    return o.owner->state_no == 0 && o.ruleID == 0;
}

/** Data structure for the state-combination in phase 2 table. START */

class LlistState : public std::list<StateHandle>
{
  public:
    explicit LlistState() noexcept = default;
    explicit LlistState(StateHandle n) { this->push_back(n); }
    /// Add n to the list in INC order.
    void add_inc(StateHandle n);
    void dump() const;
};

/// Similar to llist_int, but has two int fields.
/// Used by LT_cluster only.

class LlistState2 : public std::list<std::pair<StateHandle, StateHandle>>
{
  public:
    explicit LlistState2() noexcept = default;
    explicit LlistState2(StateHandle n1, StateHandle n2)
    {
        this->push_back({ n1, n2 });
    }
    /// Add n to the head of list.
    void add_head(StateHandle n1, StateHandle n2);
    /// Add n1, n2 to the list in INC order of n1.
    void add_inc(StateHandle n1, StateHandle n2);
    /// Find the node in a LlistInt list whose first entry is n1.
    [[nodiscard]] auto find_n1(StateHandle n1) const noexcept
      -> LlistState2::const_iterator;
    /// Find the node in a LlistInt list whose second entry is n2.
    [[nodiscard]] auto find_n2(StateHandle n2) const noexcept
      -> LlistState2::const_iterator;
    void dump() const noexcept;
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
    StateHandle from_state;
    LlistContextSet* ctxt_set; // in INC order of config->ruleID.
    LlistState to_states{};    // in INC order of state_no.
    LtTblEntry* next;

    /*
     * In LT_tbl, find the entry where from_state is state_no.
     *
     * Note that in LT_tbl, the entries are in INC order of state_no.
     */
    [[nodiscard]] auto find_entry(StateHandle from_state) noexcept
      -> LtTblEntry*;
};

/// For state combining purpose.
struct LtCluster
{
    bool pairwise_disjoint = false;
    LlistState2 states{};                // in INC order of state_no.
    LlistContextSet* ctxt_set = nullptr; // in INC order of config->ruleID.
    LtCluster* next = nullptr;

    static auto find_actual_containing_cluster(int state_no) -> LtCluster*;
    void dump(LtTblEntry& lane_tracing_table) const noexcept;
    /// Return:
    ///   the splitted state's no if state_no is in c->states list
    ///   -1 otherwise.
    ///
    /// Note state_no here is the virtual state_no: the one
    /// splitted from. So there could be more than one cluster
    /// contain it.
    [[nodiscard]] auto contain_state(std::optional<StateHandle> state_no)
      const noexcept -> std::optional<StateHandle>;
};

extern LtCluster* all_clusters;

/*
 * Data structures in lrk.c
 */

/*
 * For conflicting lanes' head states and associated conflicting contexts.
 */
using laneHead = struct laneHeadState;
struct laneHeadState
{
    /// conflicting lane head state.
    ///
    /// Should never contain `nullptr`.
    std::shared_ptr<State> s;
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
    std::shared_ptr<SymbolTableNode> token;
    LRkConfigListNode* cfg_list;
    int conflict_count; // 0 if no conflict. can be > 1, but counted as 1
                        // when report in statistics.
    LRkPtEntry* next;
};

/*
 * Functions in lane_tracing.c
 */
extern void
trace_back_lrk(Configuration* c);
extern void
trace_back_lrk_clear(Configuration* c);

/// Set - a linked list of objects.
template<typename T>
struct Set
{
    using DumpFunction = void (*)(const T&);
    using iterator = typename std::forward_list<T>::iterator;
    using const_iterator = typename std::forward_list<T>::const_iterator;

    /// Insert if not exist.
    /// NOTE: "not exist" means the object, but if "not exist" means
    /// the contents of the object, then a separate function should
    /// be written for this.
    void insert(T object)
    {
        auto it = this->inner.begin();
        for (; it != this->inner.end(); it++) {
            if (*it == object) {
                return;
            }
        }
    }
    auto find(const T& object) const noexcept -> iterator
    {
        auto it = this->cbegin();
        for (; it != this->cend();) {
            if (*it == object) {
                return it;
            }
        }
        return it;
    }
    void remove(const T& object) { this->inner.remove(object); }
    /// A function pointer is passed in. This function dumps the set item.
    void dump(DumpFunction set_item_dump) const
    {
        if (this->inner.empty()) {
            std::cout << "(set is empty)" << std::endl;
            return;
        }
        for (const auto& s : this->inner) {
            (*set_item_dump)(s);
        }
    }

    auto begin() noexcept -> iterator { return this->inner.begin(); }
    auto cbegin() noexcept -> const_iterator { return this->inner.cbegin(); }
    auto end() noexcept -> iterator { return this->inner.end(); }
    auto end() const noexcept -> const_iterator { return this->inner.end(); }
    auto cend() const noexcept -> const_iterator { return this->inner.cend(); }

    std::forward_list<T> inner;
};

/// List - a single linked list.
struct List
{
    using DumpFunction = void (*)(const SymbolList&);

    explicit constexpr List() noexcept = default;
    // insert new object at tail of list t,
    // without checking if the object already exists.
    inline void insert_tail(SymbolList object)
    {
        this->inner.push_back(object);
        this->count++;
    }
    // Remove from list t all strings whose j-th symbol is non-terminal.
    void lrk_theads_rm_nt(size_t j);
    // Remove from t all strings whose k-heads consist entirely
    // of terminals, and add the k-heads to set t_heads;
    void lrk_theads_rm_theads(size_t k, List* t_heads);
    // Add to the end of list the result of applying all possible
    // productions to the j-th symbol, omitting existing strings,
    // and truncate until it contains no more than k non-vanishable
    // symbols.
    void add_derivatives(const Grammar& grammar,
                         const SymbolList& o,
                         size_t j,
                         size_t k);
    void dump(DumpFunction list_item_dump) const noexcept
    {
        if (this->inner.empty()) {
            std::cout << "(list is empty)" << std::endl;
            return;
        }

        std::cout << "list count: " << this->count << std::endl;

        size_t i = 0;
        for (const auto& s : this->inner) {
            std::cout << ++i << ' ';
            (*list_item_dump)(s);
        }
    }

    std::list<SymbolList> inner{};
    size_t count = 0;
};

//
// LRk_P_T - LR(k) parsing table.
//
constexpr uintptr_t CONST_CONFLICT_SYMBOL = -10000010;
struct LRkPTRow
{
    StateHandle state;
    std::shared_ptr<SymbolNode> token;
    ConfigPairNode** row;
    LRkPTRow* next;
};

constexpr size_t LRK_P_T_INIT_SIZE = 10;
constexpr size_t LRK_P_T_INC = 10;

// LR(k) parsing table.
struct LRkPT
{
    size_t k;      // k in LR(k).
    int row_count; // number of rows.
    LRkPTRow* rows;

    static auto create(int k) noexcept -> LRkPT*;
    void dump() const noexcept;
    auto find(StateHandle state,
              std::shared_ptr<SymbolTableNode> token,
              bool* found) const noexcept -> LRkPTRow*;
    auto get_entry(StateHandle state,
                   std::shared_ptr<SymbolTableNode> token,
                   std::shared_ptr<const SymbolTableNode> col_token,
                   bool* exist) noexcept -> ConfigPairNode*;
    auto add_reduction(StateHandle state,
                       std::shared_ptr<SymbolTableNode> token,
                       std::shared_ptr<const SymbolTableNode> s,
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
    [[nodiscard]] auto get(size_t k) const noexcept -> LRkPT*;
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

    explicit CfgCtxt(Configuration* c, SymbolList ctxt, Configuration* tail)
      : c(c)
      , ctxt(std::move(ctxt))
      , tail(tail)
    {}

    static auto create(Configuration* c,
                       SymbolList s,
                       Configuration* tail) noexcept -> CfgCtxt*;
    static void destroy(CfgCtxt* cc) noexcept;
    void dump() const noexcept;
};

// for LR(k) theads.
extern auto
lrk_theads(const Grammar& grammar, SymbolList& alpha, size_t k)
  -> std::shared_ptr<List>;

/// @brief Holds the information passed to the lane-tracing algorithm.
class LaneTracing : public YAlgorithm
{
  public:
    explicit LaneTracing(const Grammar& grammar,
                         const Options& options,
                         std::ofstream& fp_v,
                         NewStates& new_states,
                         std::optional<Queue>& config_queue)
      : YAlgorithm(grammar, options, fp_v, new_states, config_queue)
    {}
    [[nodiscard]] auto lane_tracing() -> std::optional<LRkPTArray>;

  private:
    Stack lane;
    Stack stack;
    bool trace_further = false;
    bool test_failed = false;
    bool grammar_ambiguous = false;
    /// Initialize this to nullptr at the beginning of lane_tracing_phase2().
    /// This list is in INC order on from_state->state_no.
    LtTblEntry* LT_tbl = nullptr;

    void phase1();
    void phase2();
    void gpm(std::shared_ptr<State> new_state);
    void lt_tbl_entry_add(StateHandle from_state,
                          const std::shared_ptr<const State>& to);
    auto lt_tbl_entry_find_insert(StateHandle from_state) -> LtTblEntry*;
    auto lt_tbl_entry_find(StateHandle from_state) -> LtTblEntry*;
    void lt_tbl_entry_add_context(StateHandle from_state,
                                  SymbolList& ctxt) noexcept(false);
    auto add_split_state(std::shared_ptr<State> y,
                         State& s,
                         size_t successor_index) -> bool;
    void update_state_reduce_action(State& s);
    void phase2_regeneration(laneHead* lh_list);
    void phase2_regeneration2();
    void set_transitors_pass_thru_on(const Configuration& cur_config,
                                     const Configuration& o) noexcept(false);
    void inherit_propagate(StateHandle state_no,
                           StateHandle parent_state_no,
                           LtCluster* container,
                           const LtTblEntry* e);
    void lt_phase2_propagate_context_change(StateHandle state_no,
                                            LtCluster* c,
                                            const LtTblEntry* e);
    void clear_regenerate(StateHandle state_no);
    void clear_inherit_regenerate(StateHandle state_no,
                                  StateHandle parent_state_no);
    auto cluster_trace_new_chain_all(StateHandle parent_state,
                                     const LtTblEntry& e) -> bool;
    auto cluster_trace_new_chain(StateHandle parent_state_no,
                                 StateHandle state_no) -> bool;
    auto cluster_add_lt_tbl_entry(LtCluster* c,
                                  StateHandle from_state,
                                  LlistContextSet* e_ctxt,
                                  size_t e_parent_state_no,
                                  bool copy) const -> StateHandle;
    auto get_the_context(const Configuration* o) noexcept(false) -> SymbolList;
    auto trace_back(Configuration* c, laneHead* lh_list) noexcept(false)
      -> laneHead*;
    [[nodiscard]] auto get_state_conflict_lane_head(
      int state_no,
      laneHead* lh_list) noexcept(false) -> laneHead*;
    [[nodiscard]] auto get_conflict_lane_head() noexcept(false) -> laneHead*;
    void get_inadequate_state_reduce_config_context(const State* s);
    void resolve_lalr1_conflicts();
    void do_loop() noexcept(false);
    void check_lane_top();
    void pop_lane();
    void check_stack_top();
    void dump_stacks() const;
    void move_markers(const Configuration* o) noexcept;
    void context_adding(SymbolList context_generated,
                        size_t cur_config_index) const;
    void stack_operation(int* fail_ct, Configuration* o);
    void context_adding_routine(SymbolList context_generated,
                                Configuration* o,
                                size_t cur_config_index,
                                int* fail_ct);
    /* used by both originator list and transitor list */
    void lane_tracing_reduction(Configuration* c) noexcept(false);

    // In `lrk.cpp`
    [[nodiscard]] auto lane_tracing_lrk() -> std::optional<LRkPTArray>;
    void edge_pushing(LRkPTArray& lrk_pt_array, StateHandle state_no);
    void lrk_config_lane_tracing(Configuration* c) noexcept;
};

// in the lane_tracing of edge_pushing.
extern bool IN_EDGE_PUSHING_LANE_TRACING;
extern Configuration* cur_red_config;
extern SymbolList EDGE_PUSHING_CONTEXT_GENERATED;
