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
#include <cstdint>
#include <fstream>
#include <iostream>
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <queue>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

class YAlgorithm; // forward-declaration

constexpr size_t SYMBOL_INIT_SIZE = 128; /* Init size of a symbol string. */
constexpr size_t GRAMMAR_RULE_INIT_MAX_COUNT =
  256; /* Number of rules allowed. */
constexpr size_t PARSING_TABLE_INIT_SIZE = 1024;
constexpr size_t STATE_INIT_SIZE = 1024; /* Number of configs in a state. */
constexpr size_t STATE_SUCCESSOR_INIT_MAX_COUNT = 8;
/*
 * Max line length in yacc input file declaration section
 * starting with "%start" or "%token".
 * Used in gen_compiler.cpp and get_yacc_grammar.cpp.
 */
constexpr size_t LINE_INIT_SIZE = 128;
/*
 * Whether add the $accept = ... first goal rule.
 * This should generally always be set to 1.
 */
constexpr bool ADD_GOAL_RULE = true;

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

using StateHandle = size_t;

/// Names of various files.
struct FileNames
{
    /// Used by gen_compiler.c, value obtained in get_options.cpp.
    std::string y_tab_c{};
    /// Used by gen_compiler.c, value obtained in get_options.cpp.
    std::string y_tab_h{};
    /// used by y.c, value obtained in get_options.cpp.
    std::string y_output{};
    /// used by gen_graphviz.c, value obtained in get_options.cpp.
    std::string y_gviz{};
};

/* Max K used for LR(k) */
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables,readability-identifier-naming)
extern std::atomic_int MAX_K;

////////////////////////////////////////////
// For symbol table
////////////////////////////////////////////

struct RuleIndexNode
{
    size_t rule_id;
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

enum class Action
{
    Shift,
    Goto,
    Reduce,
    Accept,
};

inline auto
operator<<(std::ostream& os, Action action) -> std::ostream&
{
    switch (action) {
        case Action::Accept:
            os << 'a';
            return os;
        case Action::Reduce:
            os << 'r';
            return os;
        case Action::Shift:
            os << 's';
            return os;
        case Action::Goto:
            os << 'g';
            return os;
    }
}

inline auto
operator<<(std::ostream& os, std::optional<Action> action) -> std::ostream&
{
    if (action.has_value()) {
        os << *action;
    } else {
        os << '\0';
    }
    return os;
}

class ParsingAction
{
  public:
    ~ParsingAction() = default;
    ParsingAction(const ParsingAction&) = default;
    ParsingAction(ParsingAction&&) = default;
    auto operator=(const ParsingAction&) -> ParsingAction& = default;
    auto operator=(ParsingAction&&) -> ParsingAction& = default;
    auto operator==(const ParsingAction&) const noexcept -> bool = default;
    auto operator!=(const ParsingAction&) const noexcept -> bool = default;

    enum class Kind
    {
        Accept,
        Reduce,
        Shift,
        Error,
    };

    constexpr static auto new_shift(const StateHandle state) noexcept
      -> ParsingAction
    {
        auto new_action = ParsingAction();
        new_action.data = state;
        new_action.m_kind = Kind::Shift;
        return new_action;
    }
    constexpr static auto new_reduce(const StateHandle state) noexcept
      -> ParsingAction
    {
        auto new_action = ParsingAction();
        new_action.data = state;
        new_action.m_kind = Kind::Reduce;
        return new_action;
    }
    constexpr static auto new_accept() noexcept -> ParsingAction
    {
        auto new_action = ParsingAction();
        new_action.m_kind = Kind::Accept;
        return new_action;
    }
    constexpr static auto new_error() noexcept -> ParsingAction
    {
        auto new_action = ParsingAction();
        new_action.m_kind = Kind::Error;
        return new_action;
    }
    [[nodiscard]] constexpr auto kind() const noexcept -> Kind
    {
        return this->m_kind;
    }
    [[nodiscard]] auto is_accept() const noexcept -> bool
    {
        return this->m_kind == Kind::Accept;
    }
    [[nodiscard]] constexpr auto is_reduce() const noexcept -> bool
    {
        return this->m_kind == Kind::Reduce;
    }
    [[nodiscard]] constexpr auto is_shift() const noexcept -> bool
    {
        return this->m_kind == Kind::Shift;
    }
    [[nodiscard]] constexpr auto shift_value() const -> StateHandle
    {
        if (this->m_kind != Kind::Shift) {
            throw std::runtime_error(
              std::string("Invalid call to shift_value: action is ") +
              this->to_string());
        }
        return this->data;
    }
    [[nodiscard]] constexpr auto reduce_value() const -> StateHandle
    {
        if (this->m_kind != Kind::Reduce) {
            throw std::runtime_error(
              std::string("Invalid call to reduce_value: action is ") +
              this->to_string());
        }
        return this->data;
    }
    [[nodiscard]] auto to_string() const -> std::string
    {
        switch (this->m_kind) {
            case Kind::Accept:
                return "Accept";
            case Kind::Reduce:
                return "Reduce";
            case Kind::Shift:
                return std::string("Shift(") + std::to_string(this->data) + ')';
            case Kind::Error:
                return "Error";
        };
    }

  private:
    ParsingAction() noexcept = default;
    /// Only used if `m_kind == Shift` or `m_kind == Reduce`.
    StateHandle data = 0;
    Kind m_kind = Kind::Accept;
};

inline auto
operator<<(std::ostream& os, ParsingAction action) -> std::ostream&
{
    switch (action.kind()) {
        case ParsingAction::Kind::Accept:
            os << "Accept";
            return os;
        case ParsingAction::Kind::Reduce:
            os << "Reduce(" << action.reduce_value() << ")";
            return os;
        case ParsingAction::Kind::Shift:
            os << "Shift(" << action.shift_value() << ")";
            return os;
        case ParsingAction::Kind::Error:
            os << "Error";
            return os;
    }
}

/* contains a symbol. */
struct SymbolTableNode
{
    std::shared_ptr<std::string> symbol;
    /// symbol value, for parsing table col header.
    int value = 0;
    symbol_type type = symbol_type::NEITHER;
    bool vanishable = false;
    TerminalProperty* TP = nullptr;
    /// sequence (column) number in parsing table.
    std::optional<size_t> seq = std::nullopt;
    RuleIDNode* ruleIDList = nullptr;
    std::shared_ptr<SymbolTableNode> next;
    std::optional<std::string> token_type;

    explicit SymbolTableNode(const std::string_view symbol)
      : symbol(std::make_shared<std::string>(symbol))
    {}

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
    /// Given a SymbolTableNode, returns whether it is vanishable.
    [[nodiscard]] constexpr inline auto is_vanish_symbol() const noexcept
      -> bool
    {
        return this->vanishable;
    }
    void init_terminal_property()
    {
        this->TP = new TerminalProperty;
        this->TP->precedence = 0;
        this->TP->assoc = associativity::NONASSOC;
        this->TP->is_quoted = false;
    }
};

/* entry for hash table array. */
struct HashTblNode
{
    int count;
    std::shared_ptr<SymbolTableNode> next;
};

constexpr size_t HT_SIZE = 997; /* should be a prime. */
extern std::array<HashTblNode, HT_SIZE> HashTbl;

/// used in other data structures.
struct SymbolNode
{
    std::shared_ptr<SymbolTableNode> snode;

    explicit SymbolNode(std::shared_ptr<SymbolTableNode> snode)
      : snode(std::move(snode))
    {}
};

using SymbolList = std::list<SymbolNode>;

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

class Queue
{
  public:
    explicit Queue()
    {
        // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
        this->array.reserve(Queue::QUEUE_INIT_SIZE);
    }
    ~Queue() = default;
    Queue(const Queue& other) = delete;
    Queue(Queue&& other) = delete;
    auto operator=(const Queue& other) -> Queue& = delete;
    auto operator=(Queue&& other) -> Queue& = delete;

    void clear() noexcept;
    void clear_all() noexcept;
    /// Push at back.
    void push(size_t n);
    /// Pop at front
    auto pop() noexcept -> std::optional<size_t>;
    ///
    [[nodiscard]] auto peek() const noexcept -> std::optional<size_t>;
    /// Check if element n exists in the queue.
    [[nodiscard]] auto exist(size_t elem) const noexcept -> bool;
    [[nodiscard]] constexpr inline auto size() const noexcept -> size_t
    {
        return this->array.size() - this->start;
    }
    void dump() const noexcept;
    /// Print the usage information of the queue.
    void info() const noexcept;

  private:
    std::vector<size_t> array;
    size_t start = 0;

    /* These three are for collecting information purpose. */
    size_t max_count = 0;
    size_t sum_count = 0;
    size_t call_count = 0;

    constexpr static size_t QUEUE_INIT_SIZE = 256; // hidden from outside
    constexpr static bool DEBUG_QUEUE = false;

    void set_start_to_0();
    [[nodiscard]] inline auto get_no_check(size_t i) const noexcept -> size_t
    {
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        return this->array[i + this->start];
    }
    [[nodiscard]] inline auto get_no_check(size_t i) noexcept -> size_t&
    {
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        return this->array[i + this->start];
    }
};

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
    SymbolList context{};

    /// Empty a context.
    void clear();
    friend auto operator<<(std::ostream& os, const Context& dt)
      -> std::ostream&;
};

/* Production of a configuration, or rule of a grammar. */
struct Production
{
    std::shared_ptr<SymbolNode> nLHS;
    // TODO: use a vector instead.
    SymbolList nRHS;
    unsigned int isUnitProduction : 1; // 1 - true, 0 - false
    unsigned int hasCode : 1; // has associated code: 1 - true, 0 -false
    unsigned int padding : 30;
    std::shared_ptr<const SymbolTableNode>
      lastTerminal; // for precedence information.

    /*
     * marker: mark the position in configuration.
     * 0 <= marker <= RHS_count
     * (If marker = -1, don't print it.)
     *
     * In general, marker == -1 only when print grammar rules.
     * marker >= 0 when print configuration production.
     */
    void write(std::ostream& os, std::optional<size_t> marker) const noexcept;
};

/*
 * Used for LANE_TRACING.
 */
struct OriginatorList
{
    std::vector<struct ConfigurationNode*> list;

    explicit OriginatorList() noexcept
    {
        this->list.reserve(ORIGINATOR_LIST_LEN_INIT);
    }

  private:
    constexpr static size_t ORIGINATOR_LIST_LEN_INIT = 2;
};

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
    size_t ruleID;      // index of rule in grammar.rules[].
    SymbolList nMarker; // point to scanned symbol.
    size_t marker;      // redundant to nMarker, but make processing easier.
    Context context{};
    std::shared_ptr<struct StateNode> owner;

    unsigned int isCoreConfig : 1; /* 1 - true, 0 - false. */
    /* flags for lane_tracing. */
    unsigned int COMPLETE : 1;
    unsigned int IN_LANE : 1;
    unsigned int ORIGINATOR_CHANGED : 1;
    unsigned int LANE_END : 1;        /* last config in lane. */
    unsigned int LANE_CON : 1;        /* label for config on conflict lane. */
    unsigned int CONTEXT_CHANGED : 1; /* for LR(k) use only. */
    unsigned int padding : 25;
    OriginatorList originators; /* for LANE_TRACING */
    OriginatorList transitors;  /* for phase 2 of LANE_TRACING */

    int z; // used by LR(k) edge_pushing only...11/26/2008

    void write_originators(std::ostream& os) const noexcept;
    void write_transitors(std::ostream& os) const noexcept;
};
// typedef struct ConfigurationNode Configuration;

using Conflict = struct ConflictNode;
struct ConflictNode
{
    StateHandle state;
    std::shared_ptr<SymbolTableNode> lookahead;
    ParsingAction r; // reduce, and is always the smaller one.
    ParsingAction s; // reduce or shift.
    ParsingAction decision = ParsingAction::new_accept(); // final decision.
    std::shared_ptr<ConflictNode> next;

    explicit ConflictNode(const StateHandle state,
                          const std::shared_ptr<SymbolTableNode> lookahead,
                          const ParsingAction r,
                          const ParsingAction s) noexcept
      : state(state)
      , lookahead(std::move(lookahead))
      , r(r)
      , s(s)
      , next(nullptr)
    {}
};

struct ConflictsCount
{
    /// total count of shift/shift conflicts.
    uint32_t ss{};
    /// total count of reduce/reduce conflicts.
    uint32_t rr{};
    /// total count of shift/shift conflicts.
    uint32_t rs{};
    /// expected shift/reduce conflicts.
    uint32_t expected_sr_conflict{};
};

using State = struct StateNode;
struct StateList
{
    std::vector<std::shared_ptr<StateNode>> state_list;

    static auto create() -> std::shared_ptr<StateList>;
    static void expand(StateList* l);
    /*
     * Called by cloneState() in lane_tracing.c.
     */
    auto clone() -> std::shared_ptr<StateList>;
    // static void destroy(StateList* l);

    /// Add if not exist yet.
    /// @return: true is added, false if not added.
    auto add(std::shared_ptr<State>) -> bool;
    friend auto operator<<(std::ostream& os, const StateList& dt)
      -> std::ostream&;
};

struct StateNode
{
    std::vector<Configuration*> config{};
    size_t core_config_count;

    StateHandle state_no;
    std::shared_ptr<SymbolNode> trans_symbol;

    std::shared_ptr<StateList> parents_list;

    std::vector<std::shared_ptr<StateNode>> successor_list;

    std::shared_ptr<StateNode> next;

    /* for lane-tracing */
    unsigned int ON_LANE : 1;
    unsigned int COMPLETE : 1;
    unsigned int PASS_THRU : 1;   /* for phase 2 */
    unsigned int REGENERATED : 1; /* for phase 2 regeneration */

    explicit StateNode();
};

/*
 * Note: a state collection is implemented as a linked list.
 */
struct StateCollection
{
    std::shared_ptr<State> states_head;
    std::shared_ptr<State> states_tail;
    size_t state_count;

    auto add_state2(std::shared_ptr<State> new_state) -> std::shared_ptr<State>;
};

/*
 * Note: a vanish symbol must occur on the
 * LHS of a production, so must be a non-terminal.
 */
struct Grammar
{
    explicit Grammar() = default;
    ~Grammar() noexcept = default;
    Grammar(const Grammar&) = delete;
    Grammar(Grammar&&) = default;
    auto operator=(const Grammar&) -> Grammar& = delete;
    auto operator=(Grammar&&) -> Grammar& = delete;

    std::vector<Production*> rules{};
    std::shared_ptr<SymbolNode> goal_symbol{ nullptr };
    SymbolList terminal_list{};
    SymbolList non_terminal_list{};
    SymbolList vanish_symbol_list{};
    SymbolList tokens{};

    /*
     * Write rules of the given grammar.
     */
    void write_rules(std::ostream& os) const;
    /*
     * Write rules of the given grammar which are not
     * unit productions.
     * The goal production is always printed no matter
     * it is a unit production or not.
     */
    void write_rules_no_unit_prod(std::ostream& os) const;
    /*
     * Returns number of rules excluding unit productions.
     */
    [[nodiscard]] auto get_opt_rule_count() const noexcept -> size_t;
    [[nodiscard]] auto get_rule_count() const noexcept -> size_t;
    /*
     * Write terminals of the given grammar.
     */
    void write_terminals(std::ostream& os) const;
    /*
     * Write non-terminals of the given grammar.
     *
     * Note: the part "if constexpr (ADD_GOAL_RULE) ..."
     * is just to keep consistent with yacc.
     * Leave this out for now.
     */
    void write_non_terminals(std::ostream& os) const;
    void write_vanish_symbols(std::ostream& os) const;
    /*
     * Write the given grammar, including its terminals,
     * non-terminals, goal symbol and rules.
     */
    void write(std::ostream& os,
               bool before_rm_unit_prod,
               bool use_remove_unit_production) const noexcept;
    [[nodiscard]] auto is_unit_production(StateHandle rule_no) const -> bool;
};

// extern Grammar grammar; /* Used by the entire program. */

/// Position in an input file.
struct Position
{
    uint32_t line{ 0 };
    uint32_t col{ 0 };
};

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

class YSymbol : std::string
{
  private:
    constexpr static size_t SYMBOL_MAX_SIZE = 512;
    Position position = { 0, 0 };

  public:
    YSymbol()
    {
        this->reserve(SYMBOL_INIT_SIZE);
        // for (size_t i = 0; i < SYMBOL_INIT_SIZE; i++) {
        //     this->push_back(-1); // TODO: is this still needed...
        // }
    }
    /// @brief Push `c` into `this`.
    ///
    /// This will write `c` at position `ysymbol_pt` in `ysymbol`, expanding
    /// ysymbol` if necessary.
    ///
    /// @throw This will throw a `std::runtime_error` if `ysymbol_pt` is greater
    /// than `SYMBOL_MAX_SIZE`.
    ///
    /// @note At this time, don't worry about whether the first char of a
    /// symbol should be a letter.
    void add_char_to_symbol(char c);
    [[nodiscard]] auto get_symbol() const noexcept -> std::string_view;
    /// Clear `this`.
    void reset_symbol() noexcept;
};

/// Output of `get_yacc_grammar`.
class GetYaccGrammarOutput
{
  public:
    /// Final position.
    Position position{};
    /// Parsed grammar.
    Grammar grammar;
    std::string yystype_definition;
    std::vector<std::shared_ptr<SymbolTableNode>> parsing_tbl_col_hdr;
    /// token symbol.
    YSymbol ysymbol{};

    constexpr static size_t SYMBOL_MAX_SIZE = 512;

    explicit GetYaccGrammarOutput();
    ~GetYaccGrammarOutput() = default;
    GetYaccGrammarOutput(const GetYaccGrammarOutput&) = delete;
    GetYaccGrammarOutput(GetYaccGrammarOutput&&) = default;
    auto operator=(const GetYaccGrammarOutput&)
      -> GetYaccGrammarOutput& = delete;
    auto operator=(GetYaccGrammarOutput&&) -> GetYaccGrammarOutput& = delete;

    static auto get_yacc_grammar(const std::string& infile,
                                 uint32_t& expected_sr_conflict)
      -> GetYaccGrammarOutput;

  private:
    /// Output of `process_yacc_file_input_section1`.
    struct Section1Output
    {
        std::shared_ptr<SymbolTableNode> start_symbol;
    };

    /// Processes section 1 (declaration section) of yacc input file.
    ///
    /// Currently this only gets the "%start" line if there is one.
    auto process_yacc_file_input_section1(std::ifstream& fp,
                                          const uint32_t expected_sr_conflict)
      -> Section1Output;
    /// Processes section 2 (grammar section) of a yacc input file.
    void process_yacc_file_input_section2(std::ifstream& fp);

    /// Outputs a new symbol and inserts it to the LHS or RHS
    /// of a rule of the grammar.
    ///
    /// Note that a construct like "%prec UNARY" indicates the
    /// precedence of this rule. Both "%prec" and "UNARY" are
    /// not true terminals, and should not be inserted into the
    /// terminal symbol list. The detail  of handling precedence
    /// is to be implemented.
    ///
    /// If a new symbol is on the LHS, it's a non-terminal.
    /// If it is on the RHS and not found in the symbol table,
    /// and not quoted by '', then it's a non-terminal.
    /// Note that all terminals not quoted by '' should already
    /// be delcared in section 1.
    ///
    /// Empty string is not allowed.
    auto output_nonterminal(YACC_STATE state,
                            bool& is_prec,
                            const YACC_STATE yacc_sec2_state)
      -> std::shared_ptr<SymbolTableNode>;
};

struct StateArrayElement
{
    std::shared_ptr<State> state = nullptr;
    std::shared_ptr<Conflict> conflict = nullptr;
    size_t rs_count = 0;
    size_t rr_count = 0;
};

/// for indexed access of states_new states
struct StateArray : std::vector<StateArrayElement>
{
  public:
    explicit inline StateArray() { this->reserve(PARSING_TABLE_INIT_SIZE); }

    inline void add_state(std::shared_ptr<State> s) noexcept
    {
        this->push_back(StateArrayElement{ s, nullptr, 0, 0 });
    }
};

struct NewStates
{
    StateArray states_new_array{};
    StateCollection* states_new{ nullptr };
    ConflictsCount conflicts_count{};

    void inc_conflict_count(ParsingAction s, size_t state) noexcept;
    /// insert in increasing order by conflict's state no.
    ///
    /// @note no need to check size of conflict arrays,
    /// which is handled in expand_parsing_table().
    /// TODO: this is no longer true :)
    auto add_to_conflict_array(StateHandle state,
                               std::shared_ptr<SymbolTableNode> lookahead,
                               ParsingAction action1,
                               ParsingAction action2)
      -> std::shared_ptr<ConflictNode>;
    void write_grammar_conflict_list(std::ostream& os) const noexcept;
    /// Used when USE_REMOVE_UNIT_PROD is used.
    void write_grammar_conflict_list2(std::ostream& os) const noexcept;
    void insert_state_to_pm(std::shared_ptr<State> s) noexcept;
};

class StateHashTable
{
  private:
    constexpr static size_t SHT_SIZE = 997;
    using Bucket = std::list<std::shared_ptr<State>>;

    std::array<Bucket, SHT_SIZE> data;

    [[nodiscard]] static auto get_value(const State& s) noexcept -> size_t;

  public:
    explicit StateHashTable() = default;
    ~StateHashTable() = default;
    StateHashTable(const StateHashTable&) = delete;
    StateHashTable(StateHashTable&&) = delete;
    auto operator=(const StateHashTable&) -> StateHashTable& = delete;
    auto operator=(StateHashTable&&) -> StateHashTable& = delete;

    void init() noexcept;
    [[nodiscard]] auto search_same_state(std::shared_ptr<State> s)
      -> std::shared_ptr<State>;
    [[nodiscard]] auto search(const std::shared_ptr<State>& s,
                              YAlgorithm& y_algorithm) noexcept
      -> std::pair<std::shared_ptr<State>, bool>;
    void dump(std::ostream& os) const noexcept;
};

/// Runs the LR(0) algorithm.
class LR0
{
  public:
    explicit LR0(GetYaccGrammarOutput yacc_grammar_output,
                 const Options& options,
                 std::ofstream fp_v,
                 NewStates& new_states) noexcept
      : grammar(std::move(yacc_grammar_output.grammar))
      , options(options)
      , fp_v(std::move(fp_v))
      , new_states(new_states)
      , ParsingTblColHdr(std::move(yacc_grammar_output.parsing_tbl_col_hdr))
    {
        // Use a one-dimension array to store the matrix and
        // calculate the row and column number myself:
        // row i, col j is ParsingTable.at(col_no * i + j);
        //
        // Here we have:
        // 0 <= i < total states count
        // 0 <= j < col_no
        size_t total_cells = PARSING_TABLE_INIT_SIZE * ParsingTblColHdr.size();
        this->ParsingTable.reserve(4 * total_cells);
        for (size_t i = 0; i < 4 * total_cells; i++) {
            this->ParsingTable.emplace_back(std::nullopt);
        }
    }
    void update_parsing_table() noexcept;
    void output_parsing_table_lalr();
    void generate_lr0_parsing_machine(Queue& config_queue);
    void insert_action(std::shared_ptr<SymbolTableNode> lookahead,
                       StateHandle row,
                       ParsingAction action);

    /// Update the destination state of a entry in the parsing table.
    /// Used by updateRepeatedRow(), remove_unit_production_step3() and
    /// remove_unit_production_step1and2().
    /// Type of symbol is SymbolTableNode *.
    /// Just copy the value of state_dest.
    ///
    /// Used in y.c and upe.c.
    inline void update_action(const size_t col,
                              const size_t row,
                              const ParsingAction state_dest)
    {
        while ((row * ParsingTblColHdr.size() + col) >=
               this->ParsingTable.size()) {
            this->ParsingTable.emplace_back(std::nullopt);
        }
        ParsingTable[row * ParsingTblColHdr.size() + col] = state_dest;
    }
    /// Given a state and a transition symbol, find the
    /// action and the destination state.
    ///
    /// row - source state.
    ///
    /// Results are stored in variable state_dest.
    ///
    /// @return The found action, and the destination state.
    ///
    /// @example
    /// ```cpp
    /// auto [action, state_dest] = get_action(SymbolType::TERMINAL, 1, 1);
    /// ```
    [[nodiscard]] auto get_action(symbol_type symbol_type,
                                  size_t col,
                                  StateHandle row) const
      -> std::pair<std::optional<Action>, StateHandle>;

    const Grammar grammar;
    const Options& options;
    std::ofstream fp_v;
    NewStates& new_states;
    std::vector<std::optional<ParsingAction>> ParsingTable{};
    /// For parsing table column header names.
    /// Value = terminal_count + non_terminal_count + 1 columns.
    std::vector<std::shared_ptr<SymbolTableNode>> ParsingTblColHdr{};
    size_t n_state_opt1{};

  protected:
    StateHashTable state_hash_table{};

    void add_transition_states2_new_lr0(const StateCollection& coll,
                                        std::shared_ptr<State> src_state);
    void transition_lr0(std::shared_ptr<State> s) noexcept;
    void output_parsing_table() noexcept;
    void insert_reduction_to_parsing_table_lr0(const Configuration& c,
                                               StateHandle state_no);
    void output_parsing_table_row(std::shared_ptr<const State> s);
    void insert_reduction_to_parsing_table_lalr(const Configuration& c,
                                                StateHandle state_no);
    void output_parsing_table_row_lalr(std::shared_ptr<const State>);
};

class YAlgorithm : public LR0
{
  public:
    YAlgorithm(GetYaccGrammarOutput yacc_grammar_output,
               const Options& options,
               std::ofstream fp_v,
               NewStates& new_states,
               std::optional<Queue>& config_queue) noexcept
      : LR0(std::move(yacc_grammar_output),
            options,
            std::move(fp_v),
            new_states)
      , config_queue(config_queue)
    {}
    /// in `y.cpp`
    void init();
    void generate_parsing_machine();
    /// for DEBUG use.
    void print_parsing_table();
    void get_final_state_list() const noexcept;
    /// Algorithm:
    ///
    /// insert each symbol S in alpha to heads until S is NOT vanishable.
    /// if S is a terminal, then insert S to theads; else insert to heads.
    /// if all symbols are N.T., insert empty string to theads.
    ///
    /// for each symbol A in heads {
    ///   for each grammar rule r where A is the LHS {
    ///     for each symbol B in r's RHS until NOT vanishable {
    ///       if B is NT, insert B to heads's tail;
    ///       else B is T, insert (like insertion sort) to theads.
    ///     }
    ///   }
    /// }
    ///
    /// @added to replace the old one on: 3/9/2008
    ///
    /// @note This will skip alpha's first element.
    auto get_theads(const SymbolList& alpha,
                    SymbolList::const_iterator alpha_it) -> SymbolList;

    /// in `upe.cpp`

    void get_actual_state_no();
    void remove_unit_production();
    void further_optimization();
    void show_stat() noexcept;
    auto combine_compatible_states(std::shared_ptr<State> s_dest,
                                   const State& s_src) -> bool;
    void print_final_parsing_table() noexcept;
    void print_condensed_final_parsing_table() noexcept;

    // in `gen_compiler.cpp`

    void generate_compiler(YSymbol& y_symbol,
                           std::string yystype_definition,
                           const std::optional<struct LRkPTArray>& lrk_pt_array,
                           const std::string& infile,
                           const FileNames& files);

    // in `gen_graphviz.cpp`
    void gen_graphviz_input(
      const std::string& y_gviz) const noexcept; /* For O0, O1 */
    void gen_graphviz_input2(
      const std::string& y_gviz) const noexcept; /* For O2, O3 */

  protected:
    std::optional<Queue>& config_queue;
    size_t n_state_opt12 = 0;
    size_t n_state_opt123 = 0;

    void get_state_closure(std::shared_ptr<State> state);
    void state_transition(std::shared_ptr<State> state);

  private:
    // in `y.cpp`
    void init_start_state();
    void update_state_parsing_tbl_entry(const State& s);
    void insert_reduction_to_parsing_table(const Configuration* c,
                                           StateHandle state_no);
    void propagate_context_change(const State& s);
    auto add_transition_states2_new(StateCollection* coll,
                                    std::shared_ptr<State> rc_state) -> bool;
    void write_parsing_tbl_row_lalr(std::ostream& os,
                                    const StateHandle state) const noexcept;
    void write_parsing_tbl_row(std::ostream& os,
                               const StateHandle state) const noexcept;
    void write_state_info(std::ostream& os, const State& s) const noexcept;
    void write_state_collection_info(std::ostream& os) const noexcept;
    void write_state_info_from_parsing_tbl(std::ostream& os) const noexcept;
    void write_state_transition_list(std::ostream& os) const noexcept;
    void get_context(const Configuration& cfg, Context& context);
    void get_config_successors(std::shared_ptr<State> s);
    void get_successor_for_config(std::shared_ptr<State> s,
                                  const Configuration& config);

    // in `mrt.cpp`

    auto build_multirooted_tree()
      -> std::vector<std::shared_ptr<struct MRTreeNode>>;

    // in `upe.cpp`

    void get_unit_prod_shift(
      StateHandle state,
      std::shared_ptr<SymbolTableNode> leaf,
      const std::vector<std::shared_ptr<SymbolNode>>& parents,
      std::vector<StateHandle>& unit_prod_dest_states) const;
    void insert_action_of_symbol(std::shared_ptr<SymbolTableNode> symbol,
                                 StateHandle new_state,
                                 size_t old_state_index,
                                 const std::vector<StateHandle>& old_states);
    void insert_actions_of_combined_states(
      StateHandle new_state,
      const std::vector<StateHandle>& old_states);
    void show_state_config_info(std::ostream& os) const noexcept;
    void update_repeated_row(const StateHandle new_state,
                             const StateHandle old_state,
                             const StateHandle row) noexcept;
    void update_repeated_rows(const StateHandle new_state,
                              const StateHandle old_state) noexcept;
    void remove_unit_production_step1and2(
      const std::vector<std::shared_ptr<struct MRTreeNode>>& mr_leaves);
    void remove_unit_production_step3();
    void remove_unit_production_step4();
    void write_parsing_table_col_header(std::ostream& os) const noexcept;
    [[nodiscard]] auto is_equal_row(const StateHandle i,
                                    const StateHandle j) const -> bool;
    void get_reachable_states_for_symbol(
      const std::string_view symbol,
      const size_t cur_state,
      std::vector<size_t>& states_reachable) const;
    void get_reachable_states(const size_t cur_state,
                              std::vector<size_t>& states_reachable) const;
};

/* Variables for parsing table. */

constexpr int CONST_ACC = -10000000; /* for ACC in parsing table */

extern std::atomic_size_t PARSING_TABLE_SIZE;
extern size_t ParsingTblRows;
/*
 * For final parsing table.
 */
extern SymbolList F_ParsingTblColHdr;

/// Used by step 4 of remove unit production in y.c,
/// and by get_yy_arrays() in gen_compiler.c.
extern std::vector<size_t> states_reachable;

/*
 * For condensed parsing table after removing unit productions.
 * Used by function getActualState() in y.c and gen_compiler.c.
 */
extern std::vector<StateHandle> actual_state_no;

/* Statistical values. */
extern size_t n_symbol;
extern size_t n_rule;
extern size_t n_rule_opt;

/* defined in hyacc_path.c, used in gen_compiler.c */
extern std::string HYACC_PATH;

/*************************
 * Function headers.
 *************************/

/* functions in y.c */
// extern Configuration * createConfig();
extern void
expand_parsing_table(StateArray& states_new_array,
                     const size_t parsing_table_size);
extern auto
create_config(const Grammar& grammar,
              size_t rule_id,
              size_t marker,
              uint is_core_config) -> Configuration*;
extern auto
is_goal_symbol(const Grammar& grammar,
               std::shared_ptr<const SymbolTableNode> snode) -> bool;
extern auto
get_actual_state(StateHandle virtual_state) -> std::optional<StateHandle>;
// extern bool isVanishSymbol(SymbolTableNode * n);
extern auto
is_parent_symbol(std::shared_ptr<const SymbolTableNode> s) -> bool;
extern auto
is_reachable_state(StateHandle state) -> bool;
extern auto
is_same_state(const State& s1, const State& s2) -> bool;
extern auto
is_compatible_states(const State* s1, const State* s2) -> bool;

extern auto
is_final_configuration(const Grammar& grammar, const Configuration* c) -> bool;
extern auto
is_empty_production(const Grammar& grammar, const Configuration* c) -> bool;
extern auto
add_symbol2_context(std::shared_ptr<SymbolTableNode> snode, Context& c) -> bool;
extern auto
is_compatible_successor_config(const std::shared_ptr<const State>& s,
                               size_t rule_id) -> std::optional<size_t>;
extern void
copy_config(Configuration& c_dest, const Configuration& c_src);
extern void
add_core_config2_state(const Grammar& grammar,
                       std::shared_ptr<State>,
                       Configuration* new_config);
extern void
add_successor(std::shared_ptr<State>& s, std::shared_ptr<State> n);
extern auto
get_theads(const Grammar& grammar,
           const SymbolList& alpha,
           SymbolList::const_iterator alpha_it) -> SymbolList;
extern void
show_theads(SymbolList alpha, SymbolList theads);
extern auto
find_similar_core_config(const std::shared_ptr<const State>& t,
                         const Configuration* c,
                         size_t* config_index) -> Configuration*;
extern void
copy_context(Context& dest, const Context& src);
extern void
mandatory_update_action(std::shared_ptr<SymbolTableNode> lookahead,
                        int row,
                        int state_dest);
extern void
print_parsing_table_note(std::ostream& os);
/// Insert snode to the list, no repetition allowed, increasing order.
/// Do it like insertion sort.
///
/// @return whether snode already existed.
extern auto
insert_symbol_list_unique_inc(SymbolList& list,
                              std::shared_ptr<SymbolTableNode> snode) -> bool;

extern auto
has_common_core(std::shared_ptr<State> s1, std::shared_ptr<State> s2) -> bool;
extern auto
combine_context(Context& c_dest, const Context& c_src) -> bool;

/// Given a symbol, returns which column it locates in
/// the parsing table.
///
/// The column is arranged this way:
/// STR_END, terminals, non-terminals.
///
/// Used in y.cpp and upe.cpp.
inline auto
get_col(const SymbolTableNode& n) -> size_t
{
    if (!n.seq.has_value()) {
        throw std::runtime_error("[in get_col] `seq` should not be nullopt !");
    }
    return n.seq.value();
}

/* Defined in upe.c */
extern void
write_actual_state_array(std::ostream& os);

/* Defined in version.cpp */
extern void
print_version();

/* Defined in hyacc_path.cpp */
extern void
show_manpage();

/* Defined in get_options.cpp */
extern auto
get_options(std::span<const std::string_view> args,
            Options& options,
            FileNames* files) -> size_t;

/* Defined in gen_compiler.cpp */
// Length is ParsingTblRows (?)
extern std::vector<StateHandle> final_state_list; // for final states.

/* functions in symbol_table.cpp */
extern void
hash_tbl_init();
extern auto
hash_tbl_insert(std::string_view symbol) -> std::shared_ptr<SymbolTableNode>;
extern auto
hash_tbl_find(std::string_view symbol) -> std::shared_ptr<SymbolTableNode>;
extern void
hash_tbl_dump(std::ostream& os);
extern void
hash_tbl_destroy();
extern auto
find_in_symbol_list(SymbolList& a, std::shared_ptr<const SymbolTableNode> s)
  -> SymbolNode*;
extern auto
find_in_inc_symbol_list(SymbolList& a, std::shared_ptr<SymbolTableNode> s)
  -> SymbolNode*;
extern auto
clone_symbol_list(const SymbolList& a) -> SymbolList;
extern void
free_symbol_node(SymbolNode* n);
extern auto
create_rule_id_node(size_t rule_id) -> RuleIDNode*;
extern void
write_symbol_list(const SymbolList& a, const std::string_view name);
extern auto
remove_from_symbol_list(SymbolList& a, std::shared_ptr<SymbolTableNode> s)
  -> bool;
extern auto
get_symbol_list_len(SymbolList a) -> int;
extern void
combine_inc_symbol_list(SymbolList& a, const SymbolList& b);
extern void
insert_inc_symbol_list(SymbolList& a, std::shared_ptr<SymbolTableNode> n);

/*
 * For use by get_yacc_grammar.cpp and gen_compiler.cpp
 */

extern const std::string_view STR_ACCEPT;
extern const std::string_view STR_PLACE_HOLDER;
extern const std::string_view STR_END;
extern const std::string_view STR_EMPTY;
extern const std::string_view STR_ERROR;

/* functions in lr0.cpp */

/// Get the scanned symbol of configuration c.
/// The scanned symbol can be obtained by nMarker pointer
/// as here, or by marker which needs more calculation.
extern auto
get_scanned_symbol(const Configuration& c)
  -> std::shared_ptr<SymbolTableNode>; // in y.c
extern auto
create_state_collection() -> StateCollection*;
extern auto
find_state_for_scanned_symbol(const StateCollection* c,
                              std::shared_ptr<const SymbolTableNode> symbol)
  -> std::shared_ptr<State>;

/* list of inadequate states for lane-tracing. */

struct StateNoArray
{
    // list of state_no
    std::vector<StateHandle> states;
    // SymbolNode ** conflictSymbolList; // conflict symbols for each state
    size_t count_unresolved; /* unresolved after lane tracing phase 1. */

    inline void clear() noexcept
    {
        this->states.clear();
        this->count_unresolved = 0;
    }
};

extern StateNoArray states_inadequate;

/* functions in lane_tracing.c */
extern void
add_state_no_array(StateNoArray& sa, StateHandle state_no);
extern void
stdout_write_config(const Grammar& grammar, const Configuration* c);
extern auto
is_inadequate_state(int state_no) -> bool;

// these are used in lane_tracing.c and lrk.c only.
extern void
my_write_state(const Grammar& grammar, const State& s);
extern auto
test_a(const SymbolList& n) -> bool;
