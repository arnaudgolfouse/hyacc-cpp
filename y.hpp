/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007, 2008, 2009 Xin Chen. chenx@hawaii.edu

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

/****************************************************************
 * y.h
 *
 * Header file for y.c
 *
 * @Author: Xin Chen
 * @Created on: 8/30/2006
 * @last modified: 3/24/2009
 ****************************************************************/

#include <atomic>
#include <cstddef>
#include <memory>
#include <queue>
#include <span>
#include <string>
#include <vector>

constexpr size_t SYMBOL_INIT_SIZE = 128; /* Init size of a symbol string. */
constexpr size_t SYMBOL_MAX_SIZE = 512;
constexpr size_t GRAMMAR_RULE_INIT_MAX_COUNT =
  256; /* Number of rules allowed. */
constexpr size_t PARSING_TABLE_INIT_SIZE = 1024;
constexpr size_t STATE_INIT_SIZE = 1024; /* Number of configs in a state. */
constexpr size_t STATE_SUCCESSOR_INIT_MAX_COUNT = 8;
/*
 * Max line length in yacc input file declaration section
 * starting with "%start" or "%token".
 * Used in gen_compiler.c and get_yacc_grammar.c.
 */
constexpr size_t LINE_INIT_SIZE = 128;
/*
 * Whether add the $accept = ... first goal rule.
 * This should generally always be set to 1.
 */
constexpr bool ADD_GOAL_RULE = true;

/* store output of y.output. */
extern std::unique_ptr<std::ofstream> fp_v;

template<typename T>
constexpr auto
HYY_EXPAND(T** name, size_t new_size)
{
    delete[] * name;
    *name = new T[new_size];
    if (name == nullptr) {
        throw std::runtime_error("HYY_EXPAND error: out of memory");
    }
}

inline void
YYERR_EXIT(const char* msg)
{
    throw std::runtime_error(msg);
}

//////////////////////////////////////////////////////////////////
// Options that can be turned on/off in get_options.c
//////////////////////////////////////////////////////////////////

class Options
{
  private:
    static Options
      inner; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

  public:
    static auto get() -> Options& { return Options::inner; }

    std::atomic_bool use_combine_compatible_config;
    std::atomic_bool use_combine_compatible_states;
    std::atomic_bool use_remove_unit_production;
    std::atomic_bool use_remove_repeated_states;
    std::atomic_bool show_grammar;
    std::atomic_bool show_parsing_tbl;
    std::atomic_bool debug_gen_parsing_machine;
    std::atomic_bool debug_comb_comp_config;
    std::atomic_bool debug_build_multirooted_tree;
    std::atomic_bool debug_remove_up_step_1_2;
    std::atomic_bool debug_remove_up_step_4;
    std::atomic_bool show_total_parsing_tbl_after_rm_up;
    std::atomic_bool show_theads;
    std::atomic_bool debug_expand_array;
    std::atomic_bool debug_hash_tbl;
    std::atomic_bool show_ss_conflicts;
    std::atomic_bool show_state_transition_list;
    std::atomic_bool show_state_config_count;
    std::atomic_bool show_actual_state_array;
    /* switch for YYDEBUG in hyaccpar */
    std::atomic_bool use_yydebug;
    std::atomic_bool use_lines;
    std::atomic_bool use_verbose;
    std::atomic_bool use_output_filename;
    std::atomic_bool use_filename_prefix;
    std::atomic_bool use_header_file;
    std::atomic_bool use_generate_compiler;
    std::atomic_bool preserve_unit_prod_with_code;
    std::atomic_bool use_graphviz;
    /* lane-tracing algorithm */
    std::atomic_bool use_lr0;
    std::atomic_bool use_lalr;
    std::atomic_bool use_lane_tracing;
    std::atomic_bool use_lr_k;
    std::atomic_bool show_originators;
};

/* used by gen_compiler.c, value obtained in get_options.c */
extern std::string y_tab_c;
extern std::string y_tab_h;
/* used by y.c, value obtained in get_options.c */
extern std::string y_output;
/* used by gen_graphviz.c, value obtained in get_options.c */
extern std::string y_gviz;
/* Max K used for LR(k) */
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables,readability-identifier-naming)
extern std::atomic_int MAX_K;

////////////////////////////////////////////
// For symbol table
////////////////////////////////////////////

struct RuleIndexNode
{
    int ruleID;
    RuleIndexNode* next;
};
using RuleIDNode = struct RuleIndexNode;

/* _NEITHER: like empty string is neither. */
enum class symbol_type
{
    TERMINAL,
    NONTERMINAL,
    NEITHER
};

enum class associativity
{
    LEFT,
    RIGHT,
    NONASSOC
};
struct TerminalProperty
{
    bool is_quoted;
    int precedence;
    associativity assoc;
};

/* contains a symbol. */
struct SymbolTableNode
{
    std::shared_ptr<std::string> symbol;
    int value; /* symbol value, for parsing table col header. */
    symbol_type type;
    bool vanishable;
    TerminalProperty* TP;
    int seq; /* sequence (column) number in parsing table. */
    RuleIDNode* ruleIDList;
    SymbolTableNode* next;
    const char* token_type;

    [[nodiscard]] constexpr inline auto is_terminal() const noexcept -> bool
    {
        return this->type == symbol_type::TERMINAL;
    }
    /// Determine if string s is a non-terminal of grammar g.
    ///
    /// Instead of searching the entire list of non_terminal_list,
    /// use hash table node's type. In O(1) time.
    /// The type was obtained when calling get_terminals() and
    /// get_nonterminals().
    [[nodiscard]] constexpr inline auto is_non_terminal() const noexcept -> bool
    {
        return this->type == symbol_type::NONTERMINAL;
    }
};
using SymbolTblNode = struct SymbolTableNode;

/* entry for hash table array. */
struct HashTblNode
{
    int count;
    SymbolTblNode* next;
};

constexpr size_t HT_SIZE = 997; /* should be a prime. */
extern std::array<HashTblNode, HT_SIZE> HashTbl;

/* used in other data structures. */
using SymbolNode = struct SymNode;
struct SymNode
{
    SymbolTblNode* snode;
    SymNode* next;

    static auto create(SymbolTblNode* sn) -> SymbolNode*;
};

using SymbolList = SymbolNode*;

//////////////////////////////////
// For queue used by getClosure()
// For queue.c
//////////////////////////////////

/*
 * This uses config_queue when getting closure for a state,
 * and combines compatible configurations along the way.
 * It's much faster than combining configurations after
 * they are all generated.
 *
 * Set USE_CONFIG_QUEUE_FOR_GET_CLOSURE to 0 to test the
 * other way. See result in y.output.
 */
constexpr bool USE_CONFIG_QUEUE_FOR_GET_CLOSURE = true;

constexpr int QUEUE_ERR_CODE = -10000000;

struct Queue
{
    static auto create() -> Queue*;
    static void destroy(Queue* q) noexcept;
    void clear() noexcept;
    void clear_all() noexcept;
    /// Push at tail.
    void push(int n);
    /// Pop at front
    auto pop() noexcept -> int;
    [[nodiscard]] auto peek() const noexcept -> int;
    /// Check if number n exists in the queue.
    [[nodiscard]] auto exist(int n) const noexcept -> bool;
    [[nodiscard]] constexpr inline auto count() const noexcept -> size_t
    {
        return this->size;
    }
    void dump() const noexcept;
    /// Print the usage information of the queue.
    void info() const noexcept;

  private:
    int* array;
    size_t capacity;
    size_t size;
    size_t start;

    /* These three are for collecting information purpose. */
    size_t max_count;
    size_t sum_count;
    size_t call_count;

    void expand();
    [[nodiscard]] inline auto get_no_check(size_t i) const noexcept -> int
    {
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        return this->array[(i + this->start) % this->capacity];
    }
    [[nodiscard]] inline auto get_no_check(size_t i) noexcept -> int&
    {
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        return this->array[(i + this->start) % this->capacity];
    }
};

extern Queue* config_queue;

/*
 * Use final state default reduction in the hyaccpar parse engine.
 * This significantly decreases the size of the generated parser
 * and overcomes the problem of always need to get the new lookahead
 * token to proceed parsing.
 * Only when debugging or for other development reason should this
 * be set to 0.
 */
constexpr bool USE_REM_FINAL_STATE = true;

////////////////////////////////////
// Data structures for HYACC.
////////////////////////////////////

/* Context of a configuration. */
struct Context
{
    SymbolList nContext;
    int context_count;
    Context* next; // Used for LR(k) only.

    /*
     * Empty a context.
     */
    void clear();
};

/* Production of a configuration, or rule of a grammar. */
struct Production
{
    SymbolNode* nLHS;
    SymbolList nRHS_head;
    SymbolNode* nRHS_tail;
    int RHS_count;
    unsigned int isUnitProduction : 1; // 1 - true, 0 - false
    unsigned int hasCode : 1; // has associated code: 1 - true, 0 -false
    unsigned int padding : 30;
    const SymbolTblNode* lastTerminal; // for precedence information.

    /*
     * marker: mark the position in configuration.
     * 0 <= marker <= RHS_count
     * (If marker = -1, don't print it.)
     *
     * In general, marker == -1 only when print grammar rules.
     * marker >= 0 when print configuration production.
     */
    void write(int marker) const;
};

/*
 * Used for LANE_TRACING.
 */
struct OriginatorList
{
    struct ConfigurationNode** list;
    int count;
    int size;
};
extern int OriginatorList_Len_Init;

/*
 * This has the same structure as OriginatorList.
 */
using TransitorList = OriginatorList;

/*
 * configuration's production is grammar.rules[ruleID].
 */
using Configuration = struct ConfigurationNode;
struct ConfigurationNode
{
    int ruleID;          // index of rule in grammar.rules[].
    SymbolNode* nMarker; // point to scanned symbol.
    int marker;          // redundant to nMarker, but make processing easier.
    Context* context;
    struct StateNode* owner;

    unsigned int isCoreConfig : 1; /* 1 - true, 0 - false. */
    /* flags for lane_tracing. */
    unsigned int COMPLETE : 1;
    unsigned int IN_LANE : 1;
    unsigned int ORIGINATOR_CHANGED : 1;
    unsigned int LANE_END : 1;        /* last config in lane. */
    unsigned int LANE_CON : 1;        /* label for config on conflict lane. */
    unsigned int CONTEXT_CHANGED : 1; /* for LR(k) use only. */
    unsigned int padding : 25;
    OriginatorList* originators; /* for LANE_TRACING */
    OriginatorList* transitors;  /* for phase 2 of LANE_TRACING */

    int z; // used by LR(k) edge_pushing only...11/26/2008
};
// typedef struct ConfigurationNode Configuration;

using Conflict = struct ConflictNode;
struct ConflictNode
{
    int state;
    SymbolTblNode* lookahead;
    int r;        // reduce, neg, and is always the smaller one.
    int s;        // reduce or shift, neg or pos.
    int decision; // final decision.
    std::shared_ptr<ConflictNode> next;

    static auto create_node(int state, SymbolTblNode* lookahead, int r, int s)
      -> std::shared_ptr<ConflictNode>;
    // static void destroy_node(Conflict* c);
    // static void destroy_list(Conflict* a);
};

extern int ss_count; // total count of shift/shift conflicts.
extern int rr_count; // total count of reduce/reduce conflicts.
extern int rs_count; // total count of shift/shift conflicts.

extern int expected_sr_conflict; // expected shift/reduce conflicts.

using State = struct StateNode;
struct StateList
{
    std::vector<StateNode*> state_list;

    static auto create() -> std::shared_ptr<StateList>;
    static void expand(StateList* l);
    /*
     * Called by cloneState() in lane_tracing.c.
     */
    auto clone() -> std::shared_ptr<StateList>;
    // static void destroy(StateList* l);

    /*
     * Add if not exist yet.
     * @Return: true is added, false if not added.
     */
    auto add(State* s) -> bool;
    void write() const;
};

struct StateNode
{
    std::vector<Configuration*> config;
    size_t core_config_count;

    int state_no;
    SymbolNode* trans_symbol;

    std::shared_ptr<StateList> parents_list;

    std::vector<StateNode*> successor_list;

    StateNode* next;

    /* for lane-tracing */
    unsigned int ON_LANE : 1;
    unsigned int COMPLETE : 1;
    unsigned int PASS_THRU : 1;   /* for phase 2 */
    unsigned int REGENERATED : 1; /* for phase 2 regeneration */
    unsigned int padding : 28;

    static void destroy_state(State* s);
};

/*
 * Note: a state collection is implemented as a linked list.
 */
struct StateCollection
{
    State* states_head;
    State* states_tail;
    int state_count;

    auto add_state2(State* new_state) -> State*;
};

/*
 * Note: a vanish symbol must occur on the
 * LHS of a production, so must be a non-terminal.
 */
struct Grammar
{
    std::vector<Production*> rules;
    SymbolNode* goal_symbol;
    SymbolList terminal_list;
    int terminal_count;
    SymbolList non_terminal_list;
    int non_terminal_count;
    SymbolList vanish_symbol_list;
    int vanish_symbol_count;

    /*
     * Write rules of the given grammar.
     */
    void write_rules() const;
    /*
     * Write rules of the given grammar which are not
     * unit productions.
     * The goal production is always printed no matter
     * it is a unit production or not.
     */
    void write_rules_no_unit_prod() const;
    /*
     * Returns number of rules excluding unit productions.
     */
    auto get_opt_rule_count() -> size_t const;
    /*
     * Write terminals of the given grammar.
     */
    void write_terminals() const;
    /*
     * Write non-terminals of the given grammar.
     *
     * Note: the part "if constexpr (ADD_GOAL_RULE) ..."
     * is just to keep consistent with yacc.
     * Leave this out for now.
     */
    void write_non_terminals() const;
    void write_vanish_symbols() const;
    /*
     * Write the given grammar, including its terminals,
     * non-terminals, goal symbol and rules.
     */
    void write(bool before_rm_unit_prod) const;
    [[nodiscard]] auto is_unit_production(size_t rule_no) const -> bool;
};

extern Grammar grammar; /* Used by the entire program. */

extern StateCollection* states_new;

/* for indexed access of states_new states */
struct StateArray
{
  public:
    std::vector<State*> state_list = {};

    // each cell is for a state, index is state_no.
    std::vector<std::shared_ptr<Conflict>> conflict_list = {};
    std::vector<int> rs_count = {}; // shift/reduce conflicts count.
    std::vector<int> rr_count = {}; // reduce/reduce conflicts count.

    explicit inline StateArray()
    {
        this->state_list.reserve(PARSING_TABLE_INIT_SIZE);
        this->conflict_list.reserve(PARSING_TABLE_INIT_SIZE);
    }

    static auto create() -> std::shared_ptr<StateArray>;
    static void expand(StateArray* array, size_t new_size);
};

extern std::shared_ptr<StateArray> states_new_array;

/* Variables for parsing table. */

constexpr int CONST_ACC = -10000000; /* for ACC in parsing table */

extern std::atomic_size_t PARSING_TABLE_SIZE;
extern std::vector<int> ParsingTable;
extern int ParsingTblCols;
extern int ParsingTblRows;
/*
 * For parsing table column header names.
 * Value = terminal_count + non_terminal_count + 1 columns.
 */
extern std::vector<SymbolTblNode*> ParsingTblColHdr;
/*
 * For final parsing table.
 */
extern SymbolList F_ParsingTblColHdr;
extern int F_ParsingTblCols;

/*
 * Used by step 4 of remove unit production in y.c,
 * and by get_yy_arrays() in gen_compiler.c.
 */
extern std::vector<int> states_reachable;

/*
 * For condensed parsing table after removing unit productions.
 * Used by function getActualState() in y.c and gen_compiler.c.
 */
extern std::vector<int> actual_state_no;

/* Statistical values. */
extern int n_symbol;
extern size_t n_rule;
extern size_t n_rule_opt;
extern size_t n_state_opt1;
extern size_t n_state_opt12;
extern size_t n_state_opt123;

/* defined in hyacc_path.c, used in gen_compiler.c */
extern std::string HYACC_PATH;

/*************************
 * Function headers.
 *************************/

/* Functions from grammars.c */
extern void
use_grammar(int grammar_index);

/* functions in y.c */
extern auto
create_context() -> Context*;
extern auto
create_production(char* lhs, char* rhs[], int rhs_count) -> Production*;
// extern Configuration * createConfig();
extern auto
create_config(int rule_id, int marker, int is_core_config) -> Configuration*;
extern auto
create_state() -> State*;
extern auto
is_goal_symbol(const SymbolTblNode* snode) -> bool;
extern auto
get_actual_state(int virtual_state) -> int;
extern void
get_action(symbol_type symbol_type,
           int col,
           int row,
           char* action,
           int* state_dest);
// extern bool isVanishSymbol(SymbolTblNode * n);
extern auto
is_parent_symbol(const SymbolTblNode* s) -> bool;
extern auto
is_reachable_state(int state) -> bool;
extern auto
is_same_state(const State* s1, const State* s2) -> bool;
extern auto
is_compatible_states(const State* s1, const State* s2) -> bool;
extern auto
combine_compatible_states(State* s_dest, const State* s_src) -> bool;

extern auto
is_final_configuration(const Configuration* c) -> bool;
extern auto
is_empty_production(const Configuration* c) -> bool;
extern auto
add_symbol2_context(SymbolTblNode* snode, Context* c) -> bool;
extern auto
is_compatible_successor_config(const State* s, int rule_id) -> int;
extern void
insert_action(SymbolTblNode* lookahead, int row, int state_dest);
extern void
insert_reduction_to_parsing_table(const Configuration* c, int state_no);
extern void
copy_config(Configuration* c_dest, const Configuration* c_src);
extern void
add_core_config2_state(State* s, Configuration* new_config);
extern auto
add_transition_states2_new(StateCollection* coll, State* src_state) -> bool;
extern void
add_state_to_state_array(StateArray& a, State* s);
extern void
add_successor(State* s, State* n);
extern void
expand_parsing_table();
extern auto
get_theads(SymbolNode* str) -> SymbolNode*;
extern void
show_theads(SymbolList alpha, SymbolList theads);
extern auto
find_similar_core_config(const State* t,
                         const Configuration* c,
                         int* config_index) -> Configuration*;
extern void
copy_context(Context* dest, const Context* src);
extern void
mandatory_update_action(SymbolTblNode* lookahead, int row, int state_dest);
extern void
free_context(Context* c);
extern void
get_reachable_states(int cur_state, std::vector<int>& states_reachable);
extern void
print_parsing_table_note();
extern auto
get_grammar_rule_count() -> size_t;
extern auto
insert_symbol_list_unique_inc(SymbolList list,
                              SymbolTblNode* snode,
                              bool* exist) -> SymbolNode*;
extern void
print_parsing_table(); // for DEBUG use.

extern void
get_closure(State* s);
extern void
transition(State* s);
extern auto
has_common_core(State* s1, State* s2) -> bool;
extern auto
combine_context(Context* c_dest, Context* c_src) -> bool;
extern void
get_context(const Configuration* cfg, Context* context);
extern void
update_state_parsing_tbl_entry(const State* s);
extern void
propagate_context_change(const State* s);

extern void
insert_state_to_pm(State* s);

/*
 * Given a symbol, returns which column it locates in
 * the parsing table.
 *
 * The column is arranged this way:
 * STR_END, terminals, non-terminals.
 *
 * Used in y.c and upe.c.
 */
inline auto
get_col(const SymbolTableNode& n) -> int
{
    return n.seq;
}

/*
 * Given a SymbolTblNode, returns whether it is vanishable.
 */
constexpr inline auto
is_vanish_symbol(const SymbolTblNode& n) -> bool
{
    return n.vanishable;
}

/*
 * Update the destination state of a entry in the parsing table.
 * Used by updateRepeatedRow(), remove_unit_production_step3() and
 * remove_unit_production_step1and2().
 * Type of symbol is SymbolTblNode *.
 * Just copy the value of state_dest.
 *
 * Used in y.c and upe.c.
 */
inline void
update_action(size_t col, size_t row, int state_dest)
{
    ParsingTable.at(row * ParsingTblCols + col) = state_dest;
}

/* Defined in upe.c */
extern void
write_actual_state_array();
extern void
remove_unit_production();
extern void
print_final_parsing_table();
extern void
further_optimization();
extern void
get_actual_state_no();
extern void
print_condensed_final_parsing_table();

/* Defined in get_yacc_grammar.c */
extern auto
get_yacc_grammar(const std::string& infile) -> Grammar;

/* Defined in version.c */
extern void
print_version();

/* Defined in hyacc_path.c */
extern void
show_manpage();

/* Defined in get_options.c */
extern auto
get_options(std::span<const char* const> args, Options& options) -> int;

/* Defined in gen_compiler.cpp */
// Length is ParsingTblRows (?)
extern std::vector<int> final_state_list; // for final states.
extern void
generate_compiler(const std::string& infile);

/* functions in symbol_table.c */
extern void
hash_tbl_init();
extern auto
hash_tbl_insert(const std::string& symbol) -> SymbolTblNode*;
extern auto
hash_tbl_find(const std::string& symbol) -> SymbolTblNode*;
extern void
hash_tbl_dump();
extern void
hash_tbl_destroy();
extern auto
find_in_symbol_list(SymbolList a, const SymbolTblNode* s) -> SymbolNode*;
extern auto
find_in_inc_symbol_list(SymbolList a, SymbolTblNode* s) -> SymbolNode*;
extern auto
clone_symbol_list(const SymbolList a) -> SymbolList;
extern void
free_symbol_node(SymbolNode* n);
extern void
free_symbol_node_list(SymbolNode* a);
extern auto
create_rule_id_node(int rule_id) -> RuleIDNode*;
extern void
write_symbol_list(SymbolList a, const char* name);
extern auto
remove_from_symbol_list(SymbolList a, SymbolTblNode* s, int* exist)
  -> SymbolList;
extern auto
get_symbol_list_len(SymbolList a) -> int;
extern auto
combine_inc_symbol_list(SymbolList a, SymbolList b) -> SymbolNode*;
extern auto
insert_inc_symbol_list(SymbolList a, SymbolTblNode* n) -> SymbolNode*;

/* functions in state_hash_table.c */
extern void
init_state_hash_tbl();
extern auto
search_state_hash_tbl(State* s, int* is_compatible) -> State*;
extern auto
search_same_state_hash_tbl(State* s) -> State*;
extern void
state_hash_tbl_dump();

/*
 * For use by get_yacc_grammar.c and gen_compiler.c
 */
extern SymbolList tokens;
extern int tokens_ct;

extern void
write_tokens();

/*
 * Define the state of yacc input file section 2.
 */
enum YACC_STATE
{
    LHS,
    LHS_COMMENT,
    COLON,
    COLON_COMMENT,
    RHS,
    TERMINAL,
    CODE,
    CODE_SINGLE_QUOTE,
    CODE_DOUBLE_QUOTE,
    CODE_COMMENT,
    CODE_COMMENT2,
    COMMENT,
    COMMENT2
};

extern const char* const STR_ACCEPT;
extern const char* const STR_PLACE_HOLDER;
extern const char* const STR_END;
extern const char* const STR_EMPTY;
extern const char* const STR_ERROR;

/* function in gen_graphviz.c */
extern void
gen_graphviz_input(); /* For O0, O1 */
extern void
gen_graphviz_input2(); /* For O2, O3 */

/* functions in lr0.c */
extern void
generate_lr0_parsing_machine();
extern auto
get_scanned_symbol(const Configuration* c) -> SymbolTblNode*; // in y.c
extern auto
create_state_collection() -> StateCollection*;
extern auto
find_state_for_scanned_symbol(const StateCollection* c,
                              const SymbolTblNode* symbol) -> State*;
extern void
update_parsing_table_lr0();
extern void
output_parsing_table_lalr();

/* list of inadequate states for lane-tracing. */

struct StateNoArray
{
    // list of state_no
    std::vector<int> states;
    // SymbolNode ** conflictSymbolList; // conflict symbols for each state
    size_t count_unresolved; /* unresolved after lane tracing phase 1. */
};

extern StateNoArray* states_inadequate;

/* functions in lane_tracing.c */
extern auto
create_originator_list() -> OriginatorList*;
extern auto
insert_originator_list(Configuration* c, Configuration* originator, int cycle)
  -> bool;
extern auto
create_state_no_array() -> StateNoArray*;
extern void
add_state_no_array(StateNoArray* sa, int state_no);
extern void
write_config_originators(const Configuration& c);
extern void
write_config_transitors(const Configuration& c);
extern void
lane_tracing();
extern void
stdout_write_config(const Configuration* c);
extern auto
is_inadequate_state(int state_no) -> bool;
/* used by both originator list and transitor list */
extern void
expand_originator_list(OriginatorList* o);
extern void
lane_tracing_reduction(Configuration* c);

// these are used in lane_tracing.c and lrk.c only.
extern void
my_write_state(State* s);
extern void
my_show_t_heads(SymbolList alpha, SymbolList theads);
extern auto
test_a(const SymbolNode* n) -> bool;
extern auto
get_contexts_generated(SymbolList list, bool* null_possible) -> SymbolList;
extern auto
combine_context_list(SymbolList list, SymbolList new_list) -> SymbolNode*;
extern void
write_conflicting_context(int state_no); // for debug.

/*
 * When this is true, transition occurs only on conflicting contexts.
 */
extern bool in_lanetracing;
