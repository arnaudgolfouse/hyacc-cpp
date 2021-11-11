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
#include <list>
#include <utility>
#include <vector>

/**
 * lane_tracing.hpp
 *
 * Used by lane_tracing.cpp and lrk.cpp only.
 *
 * @author: Xin Chen
 * @created_on: 7/26/2008
 * @last_modified: 3/24/2009
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

struct LlistContextSetItem
{
    Configuration* config; // in INC order of config->ruleID.
    SymbolList ctxt{};     // in INC order of symbol.

    explicit LlistContextSetItem(Configuration* config)
      : config(config)
    {}
};

class LlistContextSet : public std::list<LlistContextSetItem>
{
  public:
    explicit LlistContextSet() noexcept = default;

    void dump() const noexcept;
    void add_context(LlistContextSet::iterator c,
                     const SymbolList& context_set) const;
    [[nodiscard]] auto clone() const -> LlistContextSet;
    [[nodiscard]] auto pairwise_disjoint() const noexcept -> bool;
};

/// to get a State pointer from the state_no, use
/// states_new_array->state_list[state_no].
struct LtTblEntry
{
    bool processed =
      false; // whether this entry was processed during regeration.
    StateHandle from_state;
    LlistContextSet ctxt_set{}; // in INC order of config->ruleID.
    LlistState to_states{};     // in INC order of state_no.
    std::shared_ptr<LtTblEntry> next = nullptr;

    explicit LtTblEntry(StateHandle from_state,
                        const std::shared_ptr<const State>& to,
                        const StateArray& states_new_array);

    /// In LT_tbl, find the entry where from_state is state_no.
    ///
    /// Note that in LT_tbl, the entries are in INC order of state_no.
    [[nodiscard]] auto find_entry(StateHandle from_state) noexcept
      -> LtTblEntry*;
    void add_to_state(const std::shared_ptr<const State>& to);
};

/// For state combining purpose.
struct LtClusterEntry
{
    bool pairwise_disjoint = false;
    LlistState2 states{};       // in INC order of state_no.
    LlistContextSet ctxt_set{}; // in INC order of config->ruleID.

    explicit LtClusterEntry() noexcept =
      default; // TODO: remove this, and properly initialize
               // `LaneTracing::new_cluster` ?
    explicit LtClusterEntry(LtTblEntry* e, bool& all_pairwise_disjoint);
    void dump(LtTblEntry& lane_tracing_table) const noexcept;
    /// Return:
    ///   the splitted state's no if state_no is in c->states list
    ///   nullopt otherwise.
    ///
    /// Note state_no here is the virtual state_no: the one
    /// splitted from. So there could be more than one cluster
    /// contain it.
    [[nodiscard]] auto contain_state(std::optional<StateHandle> state_no)
      const noexcept -> std::optional<StateHandle>;
    /// Return:
    ///   the splitted state's no if state_no is in c->states list
    ///   -1 otherwise.
    ///
    /// Note state_no here is the actual state_no.
    /// There could be only one cluster contains it.
    [[nodiscard]] auto contain_actual_state(std::optional<StateHandle> state_no)
      const noexcept -> std::optional<StateHandle>;
    /// Combine new_part into old_part which is already in all_clusters list.
    ///
    /// Add each state in new_part to old_part, also merge context sets.
    void combine(const LtClusterEntry& other);
};

class LtCluster : public std::list<LtClusterEntry>
{
  public:
    auto find_actual_containing_cluster(StateHandle state_no) noexcept
      -> LtCluster::iterator;
};

/*
 * Data structures in lrk.c
 */

/// For conflicting lanes' head states and associated conflicting contexts.
struct LaneHeadState
{
    /// Conflicting lane head state.
    ///
    /// Should never contain `nullptr`.
    std::shared_ptr<State> s;
    /// list of conflicting contexts.
    SymbolList contexts;

    explicit LaneHeadState(std::shared_ptr<State> s,
                           std::shared_ptr<SymbolTableNode> n) noexcept;
};

using LaneHead = std::list<LaneHeadState>;

/// For (conflict_config, lane_end_config) pairs.
class ConfigPairList : public std::list<struct ConfigPairNode>
{
  public:
    /// Combine `other` into `this`.
    void combine(const ConfigPairList& other) noexcept;
    void insert(Configuration* conflict_config,
                Configuration* lane_start_config) noexcept;
    void dump() const noexcept;
    auto find(Configuration* conflict_config) noexcept
      -> ConfigPairList::iterator;
};

struct ConfigPairNode
{
    Configuration* end;   // conflict_config
    Configuration* start; // lane_start_config

    /*
     * Functions in lrk_util.cpp
     */

    explicit ConfigPairNode(Configuration* conflict_config,
                            Configuration* lane_start_config) noexcept
      : end(conflict_config)
      , start(lane_start_config)
    {}

    void dump() const noexcept;
};

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
trace_back_lrk_clear(Configuration& c);

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
    void lrk_theads_rm_theads(size_t k, List& t_heads);
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
    /// Size is ParsingTblColHdr
    std::vector<std::optional<ConfigPairNode>> row;
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
                   bool* exist) noexcept -> std::optional<ConfigPairNode>;
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

  private:
    constexpr static size_t INIT_SIZE = 10;
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
    Stack lane{};
    Stack stack{};
    bool trace_further = false;
    bool test_failed = false;
    bool grammar_ambiguous = false;
    /// Initialize this to nullptr at the beginning of lane_tracing_phase2().
    /// This list is in INC order on from_state->state_no.
    std::shared_ptr<LtTblEntry> LT_tbl = nullptr;
    /// Current final config that is traced in trace_back() function.
    /// Used for phase 2 state combining.
    Configuration* cur_red_config = nullptr;
    /// Initialize this to nullptr at the beginning of phase2_regeneration2().
    /// This list is in the order of cluster insertion.
    LtCluster all_clusters{};
    LtClusterEntry new_cluster{}; // TODO: use a free local variable instead ?
    /// Initialized to true. If in regeneration context conflicts occur,
    /// set this to false, which means the grammar is NOT LR(1).
    bool all_pairwise_disjoint = true;
    bool in_edge_pushing_lane_tracing = false;
    SymbolList edge_pushing_context_generated{};
    ConfigPairList lane_head_tail_pairs{};

    auto add_split_state(std::shared_ptr<State> y,
                         State& s,
                         size_t successor_index) -> bool;
    void all_clusters_dump();
    void check_lane_top();
    void check_stack_top();
    void clear_inherit_regenerate(StateHandle state_no,
                                  StateHandle parent_state_no);
    void clear_regenerate(StateHandle state_no);
    auto cluster_add_lt_tbl_entry(LtClusterEntry& c,
                                  StateHandle from_state,
                                  const LlistContextSet& e_ctxt,
                                  size_t e_parent_state_no,
                                  bool copy) -> StateHandle;
    auto cluster_trace_new_chain(StateHandle parent_state_no,
                                 StateHandle state_no) -> bool;
    auto cluster_trace_new_chain_all(StateHandle parent_state,
                                     const LtTblEntry& e) -> bool;
    void context_adding(SymbolList context_generated,
                        size_t cur_config_index) const;
    void context_adding_routine(SymbolList context_generated,
                                Configuration* o,
                                size_t cur_config_index,
                                int* fail_ct);
    void do_loop() noexcept(false);
    void dump_stacks() const;
    void edge_pushing(LRkPTArray& lrk_pt_array, StateHandle state_no);
    [[nodiscard]] auto get_conflict_lane_head() noexcept(false) -> LaneHead;
    void get_inadequate_state_reduce_config_context(const State* s);
    void get_state_conflict_lane_head(StateHandle state_no,
                                      LaneHead& lh_list) noexcept(false);
    auto get_the_context(const Configuration* o) noexcept(false) -> SymbolList;
    void gpm(std::shared_ptr<State> new_state);
    void inherit_propagate(StateHandle state_no,
                           StateHandle parent_state_no,
                           const LtClusterEntry& container,
                           const LtTblEntry* e);
    /// used by both originator list and transitor list
    void lane_tracing_reduction(Configuration* c) noexcept(false);
    void lt_phase2_propagate_context_change(StateHandle state_no,
                                            const LtClusterEntry& c,
                                            const LtTblEntry* e);
    void lt_tbl_entry_add(StateHandle from_state,
                          const std::shared_ptr<const State>& to);
    void lt_tbl_entry_add_context(StateHandle from_state,
                                  const SymbolList& ctxt) noexcept(false);
    auto lt_tbl_entry_find(StateHandle from_state)
      -> std::shared_ptr<LtTblEntry>;
    auto lt_tbl_entry_find_insert(StateHandle from_state)
      -> std::shared_ptr<LtTblEntry>;
    void move_markers(const Configuration* o) noexcept;
    void phase1();
    void phase2();
    void phase2_regeneration(LaneHead& lh_list);
    void phase2_regeneration2();
    void pop_lane();
    void resolve_lalr1_conflicts();
    void set_transitors_pass_thru_on(const Configuration& cur_config,
                                     const Configuration& o) noexcept(false);
    void stack_operation(int* fail_ct, Configuration* o);
    void trace_back(Configuration& c, LaneHead& lh_list) noexcept(false);
    /// For use by LR(k) only.
    /// Purpose: get LANE_END configurations and add to
    /// lane_head_tail_pairs list.
    void trace_back_lrk(Configuration* c);
    void update_state_reduce_action(State& s);

    // In `lrk.cpp`
    [[nodiscard]] auto lane_tracing_lrk() -> std::optional<LRkPTArray>;
    void lrk_config_lane_tracing(Configuration& c) noexcept;
};
