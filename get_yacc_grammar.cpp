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
 * get_yacc_grammar.cpp
 *
 * Parse Yacc input file, extract the grammar and feed to y.c.
 *
 * @Author: Xin Chen
 * @Created on: 9/28/2006
 * @Last modified: 3/21/2007
 */

#include "y.hpp"
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

constexpr bool DEBUG_YACC_INPUT_PARSER = false;

//////////////////////////////////////////////////////////////////
// Basically, there are 3 sections in a yacc input file.
// Call these 3 states. State 1 is definition section,
// state 2 is grammar section, state 3 is for code section.
//////////////////////////////////////////////////////////////////

/* Special symbols used by the entire program */
const std::string_view STR_ACCEPT = "$accept";
const std::string_view STR_PLACE_HOLDER = "$placeholder";
const std::string_view STR_END = "$end";
const std::string_view STR_EMPTY = "";
const std::string_view STR_ERROR = "error"; // reserved word.

/// %prec is in section2. See output_nonterminal() function
/// in gen_compiler.c. To be implemented.
///
/// In section 1, %left, %right, %union, %nonassoc,
/// %type, %expect, %pure_passer are to be implemented.
enum yacc_section1_state
{
    IS_NONE,
    IS_CODE,
    IS_TOKEN,
    IS_DIRECTIVE,
    IS_START,
    IS_LEFT,
    IS_RIGHT,
    IS_UNION,
    IS_TOKEN_TYPE,
    IS_NONASSOC,
    IS_TYPE,
    IS_EXPECT,
    IS_QUOTED_TERMINAL,
    IS_PURE_PARSER,
    IS_COMMENT,
    IS_UNKNOWN
};

static void
add_rhs_symbol(Grammar& grammar, std::shared_ptr<SymbolTableNode> symbol)
{
    // std::cout  << std::endl<< "==add RHS symbol: " <<  symbol << std::endl;
    Production& p = *grammar.rules.back();

    auto s = SymbolNode(symbol);
    // if s is a terminal, it's the last terminal of p's RHS.
    if (s.snode->type == symbol_type::TERMINAL && s.snode->TP != nullptr)
        p.lastTerminal = s.snode;
    p.nRHS.push_back(s);
}

/// case-insensitive string compare.
/// Return:
///   - 1 if a > b
///   - -1 if a < b
///   - 0 if a == b
///
/// Note: strcasecmp() is not an ANSI C function.
/// So use my own to be ANSI C compliant.
static auto
y_strcasecmp(const std::string_view a, const std::string_view b) -> int
{
    int cmp_val = 0;
    size_t len_a = a.size();
    size_t len_b = b.size();

    size_t len = len_a;
    if (len > len_b)
        len = len_b;

    for (size_t i = 0; i < len; i++) {
        cmp_val = tolower(a[i]) - tolower(b[i]);
        if (cmp_val > 0)
            return 1;
        if (cmp_val < 0)
            return -1;
    }

    if (len_a > len_b)
        return 1;
    if (len_a < len_b)
        return -1;
    return 0;
}

///  A valid identifier: [alph|_](alph|digit|_)*.
///  where alph is [a-zA-Z], digit is [0-9].
static auto
validate_identifier(const std::string_view s) -> bool
{
    size_t len = s.size();
    if (len == 0)
        return false;

    if (!(isalpha(s[0]) || s[0] == '_'))
        return false;

    for (size_t i = 1; i < len; i++) {
        if (!(isalnum(s[i]) || s[i] == '_'))
            return false;
    }
    return true;
}

/// Assumption: n is a terminal, and n->TP != nullptr.
static void
get_terminal_precedence(SymbolTableNode& n,
                        yacc_section1_state state,
                        int precedence)
{
    if (n.type != symbol_type::TERMINAL || n.TP == nullptr)
        return;

    if (state == IS_LEFT) {
        n.TP->assoc = associativity::LEFT;
        n.TP->precedence = precedence;
    } else if (state == IS_RIGHT) {
        n.TP->assoc = associativity::RIGHT;
        n.TP->precedence = precedence;
    } else if (state == IS_NONASSOC) {
        n.TP->assoc = associativity::NONASSOC;
        n.TP->precedence = precedence;
    }
}

static auto
get_symbol(const symbol_type t, GetYaccGrammarOutput& output)
  -> std::shared_ptr<SymbolTableNode>
{
    if (output.ysymbol.get_symbol().empty())
        return nullptr;

    std::shared_ptr<SymbolTableNode> n =
      hash_tbl_insert(output.ysymbol.get_symbol());
    output.ysymbol.reset_symbol();

    if (t != symbol_type::NEITHER)
        n->type = t;

    return n;
}

/// Used when process section 1 of the grammar file.
///
/// The token added here is a terminal.
/// These are declared in the first section of yacc input file.
/// empty string is not allowed.
static void
output_terminal(yacc_section1_state state,
                yacc_section1_state prev_state,
                const std::optional<std::string>& token_type,
                GetYaccGrammarOutput& output,
                int precedence)
{
    bool must_add_token = false;

    if (output.ysymbol.get_symbol().empty()) {
        if (state == IS_QUOTED_TERMINAL) {
            throw std::runtime_error(
              std::string("[output_terminal]: error [line ") +
              std::to_string(output.position.line) + ", col " +
              std::to_string(output.position.col) +
              "]: empty token is not allowed");
        }
        return;
    }

    auto n = get_symbol(symbol_type::NEITHER, output);

    switch (n->type) {
        case symbol_type::TERMINAL: // already entered
            must_add_token = false;
            if (!n->token_type.has_value() && token_type.has_value())
                n->token_type = token_type;
            break;

        case symbol_type::NEITHER: // new symbol
            n->type = symbol_type::TERMINAL;
            n->token_type = token_type;
            must_add_token = true;
            break;

        default:
            throw std::runtime_error(std::string("[output_terminal]: error ") +
                                     *n->symbol + " used a terminal");
    }

    // get property of this terminal.
    // Quote informatin is used to decide to print it to y.tab.h.
    n->init_terminal_property();

    if (state == IS_QUOTED_TERMINAL) {
        n->TP->is_quoted = true;
        get_terminal_precedence(*n, prev_state, precedence);
    } else {
        if (!validate_identifier(*n->symbol)) {
            std::cout << "[output_terminal]: error [line "
                      << output.position.line << ", col " << output.position.col
                      << "]: invalid identifier: '" << *n->symbol << '\''
                      << std::endl;
            n->TP->is_quoted = false;
            get_terminal_precedence(*n, state, precedence);
            return;
        }
        n->TP->is_quoted = false;
        get_terminal_precedence(*n, state, precedence);
    }

    if (must_add_token)
        output.grammar.tokens.emplace_back(n);
}

static void
get_type_symbol(const std::optional<std::string> token_type,
                GetYaccGrammarOutput& output)
{
    auto n = get_symbol(symbol_type::NONTERMINAL, output);
    n->token_type = token_type;
}

[[nodiscard]] static auto
get_start_symbol(const std::optional<std::string> token_type,
                 GetYaccGrammarOutput& output)
  -> std::shared_ptr<SymbolTableNode>
{
    std::shared_ptr<SymbolTableNode> start_symbol =
      get_symbol(symbol_type::NONTERMINAL, output);
    start_symbol->token_type = token_type;
    return start_symbol;
}

/// If more than one %expect value, use the first one.
///
/// Note that atoi() function returns 0 if error occurs,
/// like when the ysymbol is a string "abc" and not a number.
static void
get_expect_sr_conflict(GetYaccGrammarOutput& output,
                       uint32_t expected_sr_conflict)
{
    if (output.ysymbol.get_symbol().empty())
        return;

    if (expected_sr_conflict > 0) { // already got it.
        std::cout << "warning [" << output.position.line << ", "
                  << output.position.col << "]: more than one %expect value: "
                  << std::string(output.ysymbol.get_symbol()) << std::endl;
        output.ysymbol.reset_symbol();
        return;
    }

    auto [ptr, ec] = std::from_chars(output.ysymbol.get_symbol().data(),
                                     output.ysymbol.get_symbol().data() +
                                       output.ysymbol.get_symbol().size(),
                                     expected_sr_conflict);
    if (ec == std::errc::invalid_argument) {
        throw std::runtime_error(std::string("That isn't a number: '")
                                   .append(output.ysymbol.get_symbol()) +
                                 '\'');
    }
    if (ec == std::errc::result_out_of_range) {
        throw std::runtime_error(
          std::string("This number is larger than an uint32_t: '")
            .append(output.ysymbol.get_symbol()) +
          '\'');
    }
    if (expected_sr_conflict == 0) {
        using std::to_string;
        throw std::runtime_error(
          std::string("error [") + to_string(output.position.line) + ", " +
          to_string(output.position.col) + "]: invalid %expect value: " +
          std::string(output.ysymbol.get_symbol()));
    }

    // std::cout << "expect: " <<  expected_sr_conflict << std::endl;
    output.ysymbol.reset_symbol();
}

void
YSymbol::add_char_to_symbol(const char c)
{
    if (this->size() >= YSymbol::SYMBOL_MAX_SIZE) {
        using std::to_string;
        throw std::runtime_error(
          std::string("[line ") + to_string(this->position.line) + ", col " +
          to_string(this->position.col) +
          "] add_char_to_symbol Error: symbol max size " +
          to_string(YSymbol::SYMBOL_MAX_SIZE) +
          " reached\nsymbol is: " + *this);
    }
    this->push_back(c);
}

auto
YSymbol::get_symbol() const noexcept -> std::string_view
{
    return *this;
}

void
YSymbol::reset_symbol() noexcept
{
    this->clear();
}

/// Every occurence of "%left" or "%right" increases
/// the precedence by 1.
static auto
get_section1_state(const std::string_view ysymbol,
                   const Position position,
                   int& precedence) -> yacc_section1_state
{
    if (y_strcasecmp(ysymbol, "token") == 0) {
        return IS_TOKEN;
    }
    if (y_strcasecmp(ysymbol, "start") == 0) {
        return IS_START;
    }
    if (y_strcasecmp(ysymbol, "left") == 0) {
        precedence++;
        return IS_LEFT;
    }
    if (y_strcasecmp(ysymbol, "right") == 0) {
        precedence++;
        return IS_RIGHT;
    }
    if (y_strcasecmp(ysymbol, "nonassoc") == 0) {
        return IS_NONASSOC;
    }
    if (y_strcasecmp(ysymbol, "union") == 0) {
        return IS_UNION;
    }
    if (y_strcasecmp(ysymbol, "type") == 0) {
        return IS_TYPE;
    }
    if (y_strcasecmp(ysymbol, "expect") == 0) {
        return IS_EXPECT;
    }
    if (y_strcasecmp(ysymbol, "pure_parser") == 0) {
        return IS_PURE_PARSER;
    }
    std::cout << "[get_section1_state]: error [line " << position.line
              << ", col " << position.col << "]: unknown directive %" << ysymbol
              << std::endl;
    // exit(1);
    return IS_UNKNOWN;
}

static void
my_perror(const std::string_view msg, const char c, const Position position)
{
    using std::to_string;
    throw std::runtime_error(
      (std::string("[my_perror]: error [line ") + to_string(position.line) +
       ", col " + to_string(position.col) + "]: invalid char '" + c + "'. ")
        .append(msg));
}

auto
GetYaccGrammarOutput::process_yacc_file_input_section1(
  std::ifstream& fp,
  const uint32_t expected_sr_conflict) -> Section1Output
{
    int precedence = 0;
    yacc_section1_state state = IS_NONE,
                        prev_state = static_cast<yacc_section1_state>(-1);
    std::optional<std::string> token_type;
    int union_depth = 0;
    off_t union_start = 0;

    this->ysymbol.reset_symbol();
    this->grammar.tokens.clear();
    std::shared_ptr<SymbolTableNode> start_symbol = nullptr;
    this->position.line = 1;
    this->position.col = 1;

    char c = 0, last_c = '\n', last_last_c = 0;
    while (fp.get(c)) {
        if (state != IS_COMMENT && last_c == '\n' && c == '%') {
            // do nothing.
            state = IS_NONE;

        } else if (state != IS_COMMENT && last_last_c == '\n' &&
                   last_c == '%') {
            if (c == '%') {
                break; // end of section1.
            }
            if (c == '{') {
                state = IS_CODE;
            } else if (c == '}') {
                state = IS_NONE;
            } else if (isalpha(c)) { // %token, %left, ... etc.
                token_type.reset();
                state = IS_DIRECTIVE;
                this->ysymbol.add_char_to_symbol(c);
            } else {
                using std::to_string;
                throw std::runtime_error(
                  std::string(
                    "[process_yacc_file_input_section1]: error [line ") +
                  to_string(this->position.line) + ", col " +
                  to_string(this->position.col) + "]: wrong directive name: %" +
                  to_string(c));
            }

        } else if (state == IS_CODE) {
            // do nothing.
        } else if (state == IS_COMMENT) {
            if (last_c == '*' && c == '/') {
                state = prev_state;
            }
        } else if (last_c == '/' && c == '*' && state != IS_QUOTED_TERMINAL) {
            prev_state = state;
            state = IS_COMMENT;
        } else if (c == '/' && state != IS_QUOTED_TERMINAL) {
            // '/' is not a valid char in a unquoted token.
            if (state == IS_TOKEN && !this->ysymbol.get_symbol().empty()) {
                output_terminal(
                  state, prev_state, token_type, *this, precedence);
            }
        } else if (state == IS_DIRECTIVE) {
            if (isspace(c)) {
                if (this->ysymbol.get_symbol().empty()) {
                    my_perror("invalid char after %%", c, this->position);
                }
                state = get_section1_state(
                  this->ysymbol.get_symbol(), this->position, precedence);
                this->ysymbol.reset_symbol();
            } else {
                this->ysymbol.add_char_to_symbol(c);
            }
        } else if (state == IS_QUOTED_TERMINAL) {
            // putchar(c);
            // avoid '\'' and '\\'.
            if (c == '\'' &&
                (last_c != '\\' || (this->ysymbol.get_symbol().size() == 2 &&
                                    this->ysymbol.get_symbol()[0] == '\\'))) {
                // std::cout << "] terminal ends" << std::endl;
                output_terminal(state,
                                prev_state,
                                token_type,
                                *this,
                                precedence); // output quoted terminal.
                state = prev_state;
            } else if (!isspace(c)) {
                this->ysymbol.add_char_to_symbol(c);
            }
        } else if (state == IS_TOKEN_TYPE) {
            if (isspace(c)) {
                // do nothing, ignore space.
                // if there are spaces between the words in token type,
                // these words will be combined. However that's not valid
                // and should not happen.
            } else if (c == '>') {
                // process token type. to be implemented.
                // this->ysymbol[this->ysymbol_pt] = '\0';
                // std::cout << "token type [" <<  position.line<< ", " <<
                // position.col<< "]: " <<  ysymbol << std::endl;

                token_type = this->ysymbol.get_symbol();
                this->ysymbol.reset_symbol();
                state = prev_state;
            } else {
                this->ysymbol.add_char_to_symbol(c);
            }

        } else if (state == IS_TOKEN || state == IS_LEFT || state == IS_RIGHT ||
                   state == IS_NONASSOC) {
            if (isspace(c) && this->ysymbol.get_symbol().empty()) {
                // do nothing, ignore space
            } else if (isspace(c)) { // output another token
                output_terminal(
                  state, prev_state, token_type, *this, precedence);
            } else if (c == '\'') {
                // std::cout << "terminal starts: ['";
                prev_state = state;
                state = IS_QUOTED_TERMINAL;
            } else if (c == '<') { // start of <token_type>.
                prev_state = state;
                state = IS_TOKEN_TYPE;
            } else { // add char to token string
                this->ysymbol.add_char_to_symbol(c);
            }

        } else if (state == IS_TYPE) { // %type declares non-terminals.
            if (isspace(c) && this->ysymbol.get_symbol().empty()) {
                // do nothing, ignore space
            } else if (isspace(c)) { // output another non-terminal.
                get_type_symbol(token_type, *this);
            } else if (c == '<') { // start of <token_type>.
                prev_state = state;
                state = IS_TOKEN_TYPE;
            } else { // add char to token string
                this->ysymbol.add_char_to_symbol(c);
            }
        } else if (state == IS_START) {
            // else, do nothing, ignore this line.
            if (isspace(c) && this->ysymbol.get_symbol().empty()) {
                // do nothing, ignore space.
            } else if (isspace(c)) { // output start token
                start_symbol =
                  get_start_symbol(token_type,
                                   *this); // start symbol is a non-terminal.
            } else {                       // add char to token string.
                this->ysymbol.add_char_to_symbol(c);
            }
        } else if (state == IS_UNION) {
            if (c == '{') { // union starts.
                if (union_depth++ == 0)
                    union_start =
                      static_cast<off_t>(fp.tellg()) - 1; // to include the {
            } else if (c == '}') {                        // union ends.
                if (--union_depth == 0) {
                    constexpr std::string_view STR1 = "typedef union YYSTYPE\n";
                    constexpr std::string_view STR2 = "\n        YYSTYPE;";
                    off_t size = static_cast<off_t>(fp.tellg()) - union_start;

                    this->yystype_definition = STR1;
                    fp.seekg(union_start, std::ios_base::beg);
                    std::vector<char> fp_string(size, ' ');
                    fp.read(fp_string.data(), size);
                    this->yystype_definition += fp_string.data();
                    this->yystype_definition += STR2;

                    state = IS_NONE;
                }
            } else { // get union content.
            }

        } else if (state == IS_EXPECT) {
            // else, do nothing, ignore this line.
            if (isspace(c) && this->ysymbol.get_symbol().empty()) {
                // ignore white space.
            } else if (isspace(c)) {
                get_expect_sr_conflict(*this, expected_sr_conflict);
            } else {
                this->ysymbol.add_char_to_symbol(c);
            }

        } else if (state == IS_PURE_PARSER) {
            if (c == '\n')
                state = IS_NONE;
            // else, do nothing, ignore this line.
        } else if (state == IS_UNKNOWN) {
            // do nothing.
        }

        last_last_c = last_c;
        last_c = c;

        this->position.col++;
        if (c == '\n') {
            this->position.line++;
            this->position.col = 1;
        }
    }
    return Section1Output{ start_symbol };
}

/////////////////////////////////////////////////////////////
// Functions to add rules to grammar. Start.
/////////////////////////////////////////////////////////////

/// Used by function createNewRule().
static auto
create_empty_production() -> Production*
{
    auto* p = new Production;
    if (p == nullptr) {
        throw std::runtime_error(
          "create_empty_production error: output memory");
    }

    p->isUnitProduction = 0;
    p->hasCode = 0;
    p->lastTerminal = nullptr;
    return p;
}

static auto
create_new_rule(Grammar& grammar) -> Production*&
{
    grammar.rules.push_back(create_empty_production());
    return grammar.rules.back();
}

/// Add a rule for mid-production action.
///
/// @return: the new created non-terminal, which will
///          be inserted to the position of the mid-production action.
static void
insert_mid_prod_rule(Grammar& grammar, int ct)
{
    constexpr size_t NAME_LHS_SIZE = 20;
    std::string name_lhs;
    name_lhs.reserve(NAME_LHS_SIZE);

    create_new_rule(grammar);

    // switch the postion of the last two rules.
    size_t rule_id = grammar.rules.size() - 1; // last rule's pointer.
    Production* p = grammar.rules[rule_id];
    grammar.rules[rule_id] = grammar.rules[rule_id - 1];
    grammar.rules[rule_id - 1] = p;

    // now fill the value of the new added rule.
    name_lhs += "$$";
    name_lhs += std::to_string(rule_id - 1);
    name_lhs += "_@";
    name_lhs += std::to_string(ct);
    std::shared_ptr<SymbolTableNode> n = hash_tbl_insert(name_lhs);
    n->type = symbol_type::NONTERMINAL;
    p->nLHS = std::make_shared<SymbolNode>(n);
    p->hasCode = 1;

    add_rhs_symbol(grammar, n);
}

static void
add_lhs_symbol(Grammar& grammar, std::shared_ptr<SymbolTableNode> symbol)
{
    // std::cout  << std::endl<< "==add LHS symbol: " <<  symbol << std::endl;
    Production* p = create_new_rule(grammar);
    p->nLHS = std::make_shared<SymbolNode>(symbol);
}

/// Assumption: symbol is the one after %prec in the RHS of p.
static void
get_rhs_prec_symbol(const Grammar& grammar,
                    const std::string_view symbol,
                    const Position position)
{
    std::shared_ptr<SymbolTableNode> n = hash_tbl_find(symbol);
    if (n == nullptr) {
        using std::to_string;
        throw std::runtime_error(
          (std::string("[get_rhs_prec_symbol]: error [line ") +
           to_string(position.line) + ", col " + to_string(position.col) +
           "]: %prec symbol ")
            .append(symbol) +
          " should be declared.");
    }
    Production* p = grammar.rules.back();
    if (n->TP != nullptr && n->TP->precedence > 0)
        p->lastTerminal = n;
}

static void
set_has_code(const Grammar& grammar)
{
    grammar.rules.back()->hasCode = 1;
}

/////////////////////////////////////////////////////////////
// Functions to add rules to grammar. End.
/////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////
// Functions to get grammar parameters. START.
/////////////////////////////////////////////////////////////

static auto
is_in_vanish_symbol_list(const Grammar& grammar,
                         const std::shared_ptr<const SymbolTableNode> n) -> bool
{
    // std::cout << "isInVanishSymbolList input: " <<  symbol << std::endl;
    if (n->symbol->empty()) {
        return true;
    }

    for (const auto& a : grammar.vanish_symbol_list) {
        if (n == a.snode)
            return true;
    }

    return false;
}

/// Used by the algorithm to find vanish symbol(s) of a grammar.
static auto
flag_y(const Grammar& grammar, Production* p) -> bool
{
    for (const auto& a : p->nRHS) {
        if (!is_in_vanish_symbol_list(grammar, a.snode))
            return false;
    }
    return true; // x1,...,xn are all vanish symbols. Flag y.
}

/// Called when creating a grammar.
///
/// Define epsilon as empty string "".
/// Then epsilon production is one whose
/// RHS_count = 1 AND RHS[0] == "".
///
/// NOTE: a symbol that can vanish must occur
/// on the left side of a production, thus a
/// non-terminal symbol.
///
/// Algorithm:
///   1. Flag all symbols which occur in epsilon-productions,
///      i.e., all symbols y which occur in productions of
///      the form y -> epsilon.
///   2. Go thru the grammar and for each produciton
///      y -> x1...xn, if x1, ..., xn are all flagged,
///      then flag y.
///   3. If any new symbols were flagged in step 2, go
///      back to step 2.
static void
get_vanish_symbols(Grammar& grammar)
{
    grammar.vanish_symbol_list.clear();

    // find vanish symbols that occur in epsilon-productions.
    for (auto& rule : grammar.rules) {
        if (flag_y(grammar, rule)) {
            grammar.vanish_symbol_list.emplace_back(rule->nLHS->snode);
            rule->nLHS->snode->vanishable = true;
        }
    }

    // if so far no vanish symbol, then no epsilon-production.
    // then no more vanish symbols.
    if (grammar.vanish_symbol_list.empty())
        return;

    while (true) {
        bool new_vanish_symbol_found = false;
        for (auto& rule : grammar.rules) {
            if (is_in_vanish_symbol_list(grammar, rule->nLHS->snode) == false) {
                // y is not yet a vanish symbol, then:
                if (flag_y(grammar, rule)) {
                    grammar.vanish_symbol_list.emplace_back(rule->nLHS->snode);
                    rule->nLHS->snode->vanishable = true;
                    new_vanish_symbol_found = true;
                }
            }
        }
        if (!new_vanish_symbol_found)
            break;
    }
}

/// Given a grammar's rules, get non-terminals.
///
/// Assumption: all non-terminals are used as LHS of
/// some rules. If not, such invalid non-terminal will
/// be reported after getSymbolRuleIDList().
static void
get_non_terminals(Grammar& grammar)
{
    int index = 0;

    // First scan LHS of all rules.
    for (const auto& rule : grammar.rules) {
        if (find_in_symbol_list(grammar.non_terminal_list, rule->nLHS->snode) !=
            nullptr) {
            continue;
        }
        auto& back = grammar.non_terminal_list.emplace_back(rule->nLHS->snode);
        if (*back.snode->symbol != STR_ACCEPT) {
            back.snode->value = (-1) * (++index);
        }
    }

    // bool has_error = false;

    // Next scan RHS of all rules.
    // no extra non-terminal should appear, since otherwise
    // it's not used as the LHS of any rule.
    for (const auto& rule : grammar.rules) {
        for (const auto& symbol : rule->nRHS) {
            if (symbol.snode->type != symbol_type::NONTERMINAL)
                continue;

            if (find_in_symbol_list(grammar.non_terminal_list, symbol.snode) ==
                nullptr) {
                std::cerr << "error: non-terminal '" << *symbol.snode->symbol
                          << "' is not used as the LHS of any rule"
                          << std::endl;
                ;
                // has_error = true;
            }
        }
    }
    // if (has_error ) exit(1);
}

/// Assumption:
/// This function is called after getNonTerminals().
/// empty string is not included as terminal.
static void
get_terminals(Grammar& grammar)
{
    for (const auto& rule : grammar.rules) {
        auto s = rule->nRHS.begin();
        for (size_t j = 0; j < rule->nRHS.size(); j++) {
            if (j > 0)
                s++; // s: g->rules.at(i)->RHS[j].

            if (s->snode->type != symbol_type::TERMINAL)
                continue;

            // is an empty string.
            if (s->snode->symbol->empty())
                continue;

            if (find_in_symbol_list(grammar.terminal_list, s->snode) != nullptr)
                continue;

            grammar.terminal_list.emplace_back(s->snode);
            s->snode->type = symbol_type::TERMINAL;
        }
    }
}

/// ref: page 38. The C programming language.
static auto
get_escape_char(const char c) -> char
{
    switch (c) {
        case 'a':
            return '\a';
            break;
        case 'b':
            return '\b';
            break;
        case 'f':
            return '\f';
            break;
        case 'n':
            return '\n';
            break;
        case 'r':
            return '\r';
            break;
        case 't':
            return '\t';
            break;
        case 'v':
            return '\v';
            break;
        case '\\':
            return '\\';
            break;
        case '?':
            return '\?';
            break;
        case '\"':
            return '\"';
            break;
        case '\'':
            return '\'';
            break;
        default:
            return 0;
            break;
    }
}

/// Used by get_tokens_value() only.
static auto
get_token_value(const std::string& s, int* index) -> int
{

    if (s.size() == 1)
        return s[0]; // single letter.

    int val = 0;
    if (s.size() == 2 && s[0] == '\\') { // escaped sequence.
        val = static_cast<unsigned char>(get_escape_char(s[1]));
        if (val != 0)
            return val;
    }

    if (s == STR_ERROR)
        return 256;
    if (s == STR_END)
        return 0;

    val = 257 + (*index);
    (*index)++;
    return val;
}

/// This function gets the values of all terminals,
/// Including those after %prec in the rule section,
/// and including STR_END, STR_ERROR.
///
/// Symbol can be:
///   - a token, value is 257 + index in token list (in un-quoted ones).
///   - "error", value is 256.
///   - a char or escaped char, value is its ascii code.
///   - STR_END, value is 0.
///   - a non-terminal symbol, value is -1 * index in non-terminal list.
///
/// The values of non-terminals are calculated in
/// getNonTerminals().
///
/// STR_EMPTY has no value. STR_ACCEPT is a non-terminal,
/// whose value is default to 0. But the value of these
/// two are never used, so doesn't matter.
///
/// The values of tokens are used for y.tab.h, for yytoks[]
/// and for yytbltok[] in y.tab.c.
///
/// Note that the terminal list is a subset of tokens list.
/// The tokens list can include those after %prec in the
/// rule section, which terminal list does not include.
static void
get_tokens_value(SymbolList& tokens)
{
    int index = 0;
    for (auto& a : tokens) {
        a.snode->value = get_token_value(*a.snode->symbol, &index);
    }
}

static void
get_goal_symbol(Grammar& grammar)
{
    grammar.goal_symbol =
      std::make_shared<SymbolNode>(grammar.rules.at(0)->nLHS->snode);
}

/// Get the column index in the parsing table for
/// each symbol.
/// This makes it easy to find the address for a symbol
/// in the parsing table.
///
/// This can be done when calling getNonTerminals() and
/// getTerminals(), but doing it here separately makes
/// it easy to check, debug and change.
/// Since there are not many symbols, this will take
/// only very little time.
static void
get_symbol_parsing_tbl_col(const Grammar& grammar)
{
    std::shared_ptr<SymbolTableNode> n = hash_tbl_find(STR_END);
    n->seq = 0;

    auto a = grammar.terminal_list.begin();
    for (int i = 1; a != grammar.terminal_list.end(); a++, i++) {
        n = hash_tbl_find(*a->snode->symbol);
        n->seq = i;
    }

    a = grammar.non_terminal_list.begin();
    for (int i = 1; a != grammar.non_terminal_list.end(); a++, i++) {
        n = hash_tbl_find(*a->snode->symbol);
        n->seq = grammar.terminal_list.size() + i;
    }
}

/// Get the list of Parsing table column header symbols.
/// This plus the get_symbol_parsing_tbl_col() function make it
/// easy to refer between column number and column symbol.
[[nodiscard]] static auto
get_parsing_tbl_col_hdr(const Grammar& grammar)
  -> std::vector<std::shared_ptr<SymbolTableNode>>
{
    std::vector<std::shared_ptr<SymbolTableNode>> parsing_tbl_col_hdr{};
    parsing_tbl_col_hdr.reserve(1 + grammar.terminal_list.size() +
                                grammar.non_terminal_list.size());

    parsing_tbl_col_hdr.push_back(hash_tbl_find(STR_END));

    for (const auto& a : grammar.terminal_list) {
        parsing_tbl_col_hdr.push_back(a.snode);
    }

    for (const auto& a : grammar.non_terminal_list) {
        parsing_tbl_col_hdr.push_back(a.snode);
    }
    return parsing_tbl_col_hdr;
}

/// For each non-terminal, get a list of indexes of rules
/// for which the non-terminal is the LHS.
///
/// If a non-terminal is not the LHS of any rule,
/// it's an error and should be reported.
static void
get_symbol_rule_id_list(Grammar& grammar)
{
    for (auto& a : grammar.non_terminal_list) {
        std::shared_ptr<SymbolTableNode>& n = a.snode;
        RuleIDNode* tail = nullptr;
        for (size_t i = 0; i < grammar.rules.size(); i++) {
            if (n == grammar.rules[i]->nLHS->snode) {
                RuleIDNode* r = create_rule_id_node(i);
                if (tail == nullptr) {
                    n->ruleIDList = tail = r;
                } else {
                    tail->next = r; // insert at head.
                    tail = tail->next;
                }
            }
        }
    }
}

static void
get_grammar_unit_productions(const Grammar& grammar)
{
    for (Production* rule : grammar.rules) {
        if (rule->nRHS.size() == 1 &&
            rule->nRHS.front().snode->symbol->size() > 0) {
            rule->isUnitProduction = true;
        }
    }
}

static void
get_grammar_params(GetYaccGrammarOutput& output)
{
    get_non_terminals(output.grammar);
    get_terminals(output.grammar);

    get_tokens_value(output.grammar.tokens);

    get_goal_symbol(output.grammar);
    get_vanish_symbols(output.grammar);

    get_symbol_parsing_tbl_col(output.grammar);
    output.parsing_tbl_col_hdr = get_parsing_tbl_col_hdr(output.grammar);

    get_symbol_rule_id_list(output.grammar);
    get_grammar_unit_productions(output.grammar);

    // write_symbol_list(grammar.non_terminal_list, "NT list");
    // write_symbol_list(grammar.terminal_list, "T list");
    // write_symbol_list(tokens, "tokens");
}

/////////////////////////////////////////////////////////////
// Functions to get grammar parameters. START.
/////////////////////////////////////////////////////////////

auto
GetYaccGrammarOutput::output_nonterminal(const YACC_STATE state,
                                         bool& is_prec,
                                         const YACC_STATE yacc_sec2_state)
  -> std::shared_ptr<SymbolTableNode>
{
    if (this->ysymbol.get_symbol().empty()) {
        if (state == TERMINAL) {
            using std::to_string;
            throw std::runtime_error(
              std::string("[output_nonterminal]: error [line ") +
              to_string(this->position.line) + ", col " +
              to_string(this->position.col) + "]: empty token is not allowed");
        }
        return nullptr;
    }

    if (y_strcasecmp(this->ysymbol.get_symbol(), "%prec") == 0) {
        is_prec = true;
        this->ysymbol.reset_symbol();
        return nullptr;
    }
    if (is_prec) {
        // is a ficticious terminal, no token actually.
        get_rhs_prec_symbol(this->grammar,
                            this->ysymbol.get_symbol(),
                            this->position); // ysymbol should exist.
        // std::cout << "%prec on symbol " <<  ysymbol << std::endl;
        is_prec = false;
        this->ysymbol.reset_symbol();
        return nullptr;
    }
    // std::cout << "another symbol: " <<  ysymbol << std::endl;

    if constexpr (DEBUG_YACC_INPUT_PARSER) {
        std::cout << this->ysymbol.get_symbol() << ' ';
    }

    std::shared_ptr<SymbolTableNode> n =
      hash_tbl_find(this->ysymbol.get_symbol());

    if (yacc_sec2_state == LHS) {
        if (n == nullptr) {
            n = hash_tbl_insert(this->ysymbol.get_symbol());
            n->type = symbol_type::NONTERMINAL;
        } else if (n->type == symbol_type::TERMINAL) {
            using std::to_string;
            throw std::runtime_error(
              (std::string("[output_nonterminal]: error [line ") +
               to_string(this->position.line) + ", col " +
               to_string(this->position.col) + "]: symbol ")
                .append(this->ysymbol.get_symbol()) +
              " is declared as terminal but used as non-terminal");
        }
        add_lhs_symbol(this->grammar, n);
    } else { // RHS.
        if (n == nullptr) {
            n = hash_tbl_insert(this->ysymbol.get_symbol());

            // if quoted by '', is terminal, otherwise is non-terminal.
            // "error" is a reserved word, a terminal.
            if (state == TERMINAL || this->ysymbol.get_symbol() == STR_ERROR) {
                n->type = symbol_type::TERMINAL;
                n->init_terminal_property();
                n->TP->is_quoted = true;

                // add this to the tokens list.
                this->grammar.tokens.emplace_back(n);
            } else {
                n->type = symbol_type::NONTERMINAL;
            }
        }

        add_rhs_symbol(this->grammar, n);
    }

    this->ysymbol.reset_symbol();
    return n;
}

/// Stores LHS of current rule.
/// To be used by the next rule with the same LHS in cases like:
///   LHS : RHS1 | RHS2 ...
inline auto
get_cur_lhs(std::shared_ptr<SymbolTableNode> n, const Position position)
  -> std::shared_ptr<SymbolTableNode>
{
    if (n == nullptr) {
        using std::to_string;
        throw std::runtime_error(std::string("[get_cur_lhs]: error [line ") +
                                 to_string(position.line) + ", col " +
                                 to_string(position.col) +
                                 "]: LHS symbol is empty.");
    }
    return n;
}

void
GetYaccGrammarOutput::process_yacc_file_input_section2(std::ifstream& fp)
{
    // for mid-production actions.
    int mid_prod_code_ct = 0;
    int code_level = 0;
    bool end_of_code = false;
    // used for %prec
    bool is_prec = false;
    std::shared_ptr<SymbolTableNode> cur_lhs = nullptr;

    YACC_STATE yacc_sec2_state = LHS;
    this->ysymbol.reset_symbol();

    char c = 0, last_c = 0;
    while (fp.get(c)) {
        if (last_c == '%' && c == '%')
            return;

        switch (yacc_sec2_state) {
            case LHS:
                if (isspace(c) && this->ysymbol.get_symbol().empty()) {
                    // Ignore empty spaces before LHS symbol.
                } else if (c == ':') {
                    cur_lhs = get_cur_lhs(
                      this->output_nonterminal(LHS, is_prec, yacc_sec2_state),
                      this->position); // OUTPUT LHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << "-> ";
                    }
                    yacc_sec2_state = RHS;
                } else if (isspace(c)) {
                    cur_lhs = get_cur_lhs(
                      this->output_nonterminal(LHS, is_prec, yacc_sec2_state),
                      this->position); // OUTPUT LHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << "-> ";
                    }
                    yacc_sec2_state = COLON;
                } else if (last_c == '/' && c == '*') {
                    yacc_sec2_state = LHS_COMMENT;
                    this->ysymbol.reset_symbol();
                } else if (c == '/') {
                    // do nothing. '/' is not a valid char for a symbol.
                } else if (c == ';') {
                    // do nothing. a rule without anything.
                } else if (!isspace(c)) {
                    // when encountering the '%%' starting section 3,
                    // the first '%' will be inserted. But since it won't
                    // call output_nonterminal(), a new rule won't be created.
                    // So no problem will occur here.
                    this->ysymbol.add_char_to_symbol(c);
                }
                break;
            case LHS_COMMENT:
                if (last_c == '*' && c == '/') {
                    yacc_sec2_state = LHS;
                }
                break;
            case COLON:
                if (c == ':') {
                    yacc_sec2_state = RHS;
                } else if (c == '/') {
                    // do nothing
                } else if (last_c == '/' && c == '*') {
                    yacc_sec2_state = COLON_COMMENT;
                } else if (!isspace(c)) {
                    my_perror("in state COLON", c, this->position);
                }
                break;
            case COLON_COMMENT:
                if (last_c == '*' && c == '/') {
                    yacc_sec2_state = COLON;
                }
                break;
            case RHS:
                if (isspace(c)) {
                    if (!this->ysymbol.get_symbol().empty()) {
                        this->output_nonterminal(
                          RHS,
                          is_prec,
                          yacc_sec2_state); // OUTPUT NEXT RHS SYMBOL.
                    }
                    // else, ignore empty space.
                } else if (c == '\'') {
                    // std::cout << "terminal starts(line " <<  position.line<<
                    // "): ["
                    // <<  c;
                    if (!this->ysymbol.get_symbol().empty()) {
                        this->output_nonterminal(
                          RHS,
                          is_prec,
                          yacc_sec2_state); // OUTPUT NEXT RHS SYMBOL.
                    }
                    yacc_sec2_state = TERMINAL;
                    if (end_of_code) { // for mid-prod action.
                        mid_prod_code_ct++;
                        insert_mid_prod_rule(this->grammar, mid_prod_code_ct);
                        end_of_code = false;
                    }
                } else if (c == ';') {
                    end_of_code = false; // for mid-prod action.
                    if (this->ysymbol.get_symbol().empty() && is_prec) {
                        using std::to_string;
                        throw std::runtime_error(
                          std::string("[process_yacc_file_input_section2]: "
                                      "error [line ") +
                          to_string(this->position.line) + ", col " +
                          to_string(this->position.col) +
                          "]: forgot the symbol after %prec?");
                    }
                    if (!this->ysymbol.get_symbol().empty())
                        this->output_nonterminal(
                          RHS,
                          is_prec,
                          yacc_sec2_state); // OUTPUT NEXT RHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << std::endl;
                    }
                    yacc_sec2_state = LHS;
                } else if (c == '|') {   // another rule with same LHS.
                    end_of_code = false; // for mid-prod action.
                    if (this->ysymbol.get_symbol().empty() && is_prec) {
                        using std::to_string;
                        throw std::runtime_error(
                          std::string("[process_yacc_file_input_section2]: "
                                      "error [line ") +
                          to_string(this->position.line) + ", col " +
                          to_string(this->position.col) +
                          "]: forgot the symbol after %prec?");
                    }
                    if (!this->ysymbol.get_symbol().empty())
                        this->output_nonterminal(
                          RHS,
                          is_prec,
                          yacc_sec2_state); // OUTPUT NEXT RHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << std::endl << *cur_lhs->symbol << " -> ";
                    }
                    // std::cout  << std::endl;
                    add_lhs_symbol(this->grammar, cur_lhs);
                } else if (c == '{') {
                    set_has_code(this->grammar); // has associated code.
                    /// std::cout << "start of code at rule " <<
                    ///  grammar.rule_count<< ":" << std::endl<< "{";
                    yacc_sec2_state = CODE;
                    code_level = 1;
                } else if (last_c == '/' && c == '*') {
                    yacc_sec2_state = COMMENT; // the format "/* ... */"
                } else if (last_c == '/' && c == '/') {
                    yacc_sec2_state = COMMENT2; // the format "// ..."
                } else if (c == '/') {
                    // do nothing.
                } else if (c == ':') {
                    my_perror(
                      "A ';' is missed in the last rule?", c, this->position);
                } else {
                    if (end_of_code) { // for mid-prod action.
                        mid_prod_code_ct++;
                        insert_mid_prod_rule(this->grammar, mid_prod_code_ct);
                        end_of_code = false;
                    }
                    this->ysymbol.add_char_to_symbol(c);
                }
                break;
            case TERMINAL:
                // putchar(c);
                // avoid '\'' and '\\'.
                if (c == '\'' && (last_c != '\\' ||
                                  (this->ysymbol.get_symbol().size() == 2 &&
                                   this->ysymbol.get_symbol()[0] == '\\'))) {
                    // std::cout << "] terminal ends" << std::endl;
                    yacc_sec2_state = RHS;
                    this->output_nonterminal(
                      TERMINAL,
                      is_prec,
                      yacc_sec2_state); // OUTPUT NEXT RHS SYMBOL. is terminal.
                } else {
                    // if (isspace(c)) std::cout << "hit space here " <<
                    // position.line<< " " <<  position.col << std::endl;
                    this->ysymbol.add_char_to_symbol(c);
                }
                break;
            case CODE:
                if (c == '\"') {
                    yacc_sec2_state = CODE_DOUBLE_QUOTE;
                } else if (c == '\'') {
                    yacc_sec2_state = CODE_SINGLE_QUOTE;
                } else if (c == '*' && last_c == '/') {
                    yacc_sec2_state = CODE_COMMENT;
                } else if (c == '/' && last_c == '/') {
                    yacc_sec2_state = CODE_COMMENT2;
                } else if (c == '}' && code_level == 1) {
                    yacc_sec2_state = RHS;
                    /// std::cout << "end of code" << std::endl;
                    end_of_code = true; // for mid-prod action.
                } else if (c == '{') {
                    code_level++;
                } else if (c == '}') {
                    code_level--;
                }
                break;
            case CODE_DOUBLE_QUOTE:
                if (c == '\"' && last_c != '\\')
                    yacc_sec2_state = CODE;
                break;
            case CODE_SINGLE_QUOTE:
                if (c == '\'')
                    yacc_sec2_state = CODE;
                break;
            case CODE_COMMENT:
                if (c == '/' && last_c == '*')
                    yacc_sec2_state = CODE;
                break;
            case CODE_COMMENT2:
                if (c == '\n')
                    yacc_sec2_state = CODE;
                break;
            case COMMENT:
                if (last_c == '*' && c == '/')
                    yacc_sec2_state = RHS;
                break;
            case COMMENT2:
                if (c == '\n')
                    yacc_sec2_state = RHS;
                break;
            default:
                break;
        }

        // putchar(c);
        last_c = c;

        this->position.col++;
        if (c == '\n') {
            this->position.line++;
            this->position.col = 1;
        }
    }
}

/// If there is a "%start ..." in the declaration section,
/// copy the start symbol value to RHS of goal production.
/// Otherwise, use the LHS of the first user rule as the
/// RHS of goal production.
static void
get_goal_rule_rhs(const Grammar& grammar,
                  const std::shared_ptr<SymbolTableNode> start_symbol)
{
    if (grammar.rules.size() > 1) {
        if (start_symbol != nullptr) {
            grammar.rules[0]->nRHS.emplace_back(start_symbol);
        } else {
            grammar.rules[0]->nRHS.emplace_back(grammar.rules[1]->nLHS->snode);
        }
    } else {
        throw std::runtime_error(
          "get_goal_rule_rhs() error: there is no user rule.");
    }
}

/// This modification is to preserve those unit productions
/// that have associated code. In such case add a place holder
/// nonterminal to the end of each such unit production, to
/// convert them to non-unit productions. This place holder
/// nonterminal will reduce to empty string.
static void
post_modification(Grammar& grammar)
{
    auto& options = Options::get();
    // std::cout << "calling post_modification" << std::endl;
    if (options.use_remove_unit_production == false)
        return;
    if (options.preserve_unit_prod_with_code)
        return;

    std::shared_ptr<SymbolTableNode> n = hash_tbl_insert(STR_PLACE_HOLDER);
    n->type = symbol_type::NONTERMINAL;

    size_t count = 0;
    for (const auto& rule : grammar.rules) {
        if (rule->nRHS.size() == 1 && rule->hasCode == 1u) {
            // std::cout << "rule " << i << " is a unit production with code" <<
            // std::endl;
            count++;
            Production* p = rule;
            p->isUnitProduction = 0;
            // add one more symbol to the end of RHS:
            p->nRHS.emplace_back(n);
        }
    }

    if (count > 0) {
        Production* p = create_new_rule(grammar); // $PlaceHolder -> epsilon
        p->nLHS = std::make_shared<SymbolNode>(n);
        p->isUnitProduction = 0;
        p->hasCode = 0;
    }
}

GetYaccGrammarOutput::GetYaccGrammarOutput(std::ofstream& fp_v)
  : grammar(fp_v)
{
    // insert special symbols to hash table.
    std::shared_ptr<SymbolTableNode> n =
      hash_tbl_insert(STR_END); // end marker of production.
    n->type = symbol_type::TERMINAL;

    hash_tbl_insert(STR_ACCEPT);
    hash_tbl_insert(STR_END);
    n = hash_tbl_insert(STR_EMPTY);
    n->type = symbol_type::TERMINAL;
    n->vanishable = true;

    this->grammar.rules.reserve(GRAMMAR_RULE_INIT_MAX_COUNT);
}

/// The main function of this file.
/// Gets grammar from a yacc input file.
///
/// Called by function main() in y.cpp.
auto
GetYaccGrammarOutput::get_yacc_grammar(const std::string& infile,
                                       std::ofstream& fp_v,
                                       uint32_t& expected_sr_conflict)
  -> GetYaccGrammarOutput
{
    if constexpr (DEBUG_YACC_INPUT_PARSER) {
        std::cout << "input file: " << infile << std::endl;
    }

    std::ifstream fp{};
    fp.open(infile);
    if (!fp.is_open()) {
        throw std::runtime_error(std::string("can't open file ") + infile);
    }

    GetYaccGrammarOutput output = GetYaccGrammarOutput(fp_v);
    expected_sr_conflict = 0;

    // Add a `$accept -> ...` rule.
    if constexpr (ADD_GOAL_RULE) {
        std::shared_ptr<SymbolTableNode> n = hash_tbl_insert(STR_ACCEPT);
        auto& new_rule =
          create_new_rule(output.grammar); // goal production rule.
        new_rule->nLHS = std::make_shared<SymbolNode>(n);
        n->type = symbol_type::NONTERMINAL;
    }

    GetYaccGrammarOutput::Section1Output section1_output =
      output.process_yacc_file_input_section1(fp, expected_sr_conflict);
    std::shared_ptr<SymbolTableNode> start_symbol =
      section1_output.start_symbol;
    output.process_yacc_file_input_section2(fp);

    fp.close();

    if constexpr (ADD_GOAL_RULE) {
        get_goal_rule_rhs(output.grammar, start_symbol);
    }
    post_modification(output.grammar);

    get_grammar_params(output);

    n_rule = output.grammar.rules.size();
    n_symbol = output.grammar.terminal_list.size() +
               output.grammar.non_terminal_list.size();
    n_rule_opt = output.grammar.get_opt_rule_count();
    return output;
}
