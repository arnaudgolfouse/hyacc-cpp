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
 * get_yacc_grammar.c
 *
 * Parse Yacc input file, extract the grammar and feed to y.c.
 *
 * @Author: Xin Chen
 * @Created on: 9/28/2006
 * @Last modified: 3/21/2007
 */

#include "y.hpp"
#include <array>
#include <fstream>
#include <ios>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

constexpr bool DEBUG_YACC_INPUT_PARSER = false;

//////////////////////////////////////////////////////////////////
// Basically, there are 3 sections in a yacc input file.
// Call these 3 states. State 1 is definition section,
// state 2 is grammar section, state 3 is for code section.
//////////////////////////////////////////////////////////////////

/* Special symbols used by the entire program */
const char* const STR_ACCEPT = "$accept";
const char* const STR_PLACE_HOLDER = "$placeholder";
const char* const STR_END = "$end";
const char* const STR_EMPTY = "";
const char* const STR_ERROR = "error"; // reserved word.

char* ysymbol; // token symbol.
int ysymbol_pt;
int ysymbol_size;

static SymbolTblNode* curLHS;

static YACC_STATE yacc_sec2_state;
static int CODE_level;

static SymbolTblNode* start_symbol;

static int n_line; // count the line number of yacc input file.
static int n_col;  // count the current column number of yacc input.

static int precedence;
static int IS_PREC; // used for %prec

/*
 * %prec is in section2. See output_nonterminal() function
 * in gen_compiler.c. To be implemented.
 *
 * In section 1, %left, %right, %union, %nonassoc,
 * %type, %expect, %pure_passer are to be implemented.
 */
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

/*
 * Functions declarations.
 */
void
add_rhs_symbol(SymbolTblNode* symbol);

/*
 * case-insensitive string compare.
 * Return:
 *   - 1 if a > b
 *   - -1 if a < b
 *   - 0 if a == b
 *
 * Note: strcasecmp() is not an ANSI C function.
 * So use my own to be ANSI C compliant.
 */
int
y_strcasecmp(const char* a, const char* b)
{
    int len_a, len_b, len, i, cmp_val;
    len_a = strlen(a);
    len_b = strlen(b);

    len = len_a;
    if (len > len_b)
        len = len_b;

    for (i = 0; i < len; i++) {
        cmp_val = tolower(a[i]) - tolower(b[i]);
        if (cmp_val > 0)
            return 1;
        if (cmp_val < 0)
            return -1;
    }

    cmp_val = len_a - len_b;
    if (cmp_val > 0)
        return 1;
    if (cmp_val < 0)
        return -1;

    return 0;
}

void
init_terminal_property(SymbolTblNode* n)
{
    n->TP = new TerminalProperty;
    n->TP->precedence = 0;
    n->TP->assoc = associativity::NONASSOC;
    n->TP->is_quoted = false;
}

/*
 *  A valid identifier: [alph|_](alph|digit|_)*.
 *  where alph is [a-zA-Z], digit is [0-9].
 */
bool
validate_identifier(char* s)
{
    int i;
    int len = strlen(s);

    if (len == 0)
        return false;

    if (!(isalpha(s[0]) || s[0] == '_'))
        return false;

    for (i = 1; i < len; i++) {
        if (!(isalnum(s[i]) || s[i] == '_'))
            return false;
    }
    return true;
}

/*
 * Assumption: n is a terminal, and n->TP != nullptr.
 */
void
get_terminal_precedence(SymbolTblNode* n, yacc_section1_state state)
{
    if (n->type != symbol_type::TERMINAL || n->TP == nullptr)
        return;

    if (state == IS_LEFT) {
        n->TP->assoc = associativity::LEFT;
        n->TP->precedence = precedence;
    } else if (state == IS_RIGHT) {
        n->TP->assoc = associativity::RIGHT;
        n->TP->precedence = precedence;
    } else if (state == IS_NONASSOC) {
        n->TP->assoc = associativity::NONASSOC;
        n->TP->precedence = precedence;
    }
}

/*
 * Add a token to the tokens list.
 */
static void
add_token(SymbolNode* tokens_tail, SymbolTblNode* n)
{
    if (tokens_tail == nullptr) {
        tokens_tail = tokens = SymbolNode::create(n);
    } else {
        tokens_tail->next = SymbolNode::create(n);
        tokens_tail = tokens_tail->next;
    }

    tokens_ct++;
}

static auto
get_symbol(symbol_type t) -> SymbolTblNode*
{
    if (ysymbol_pt == 0)
        return nullptr;

    ysymbol[ysymbol_pt] = 0;
    SymbolTblNode* n = hash_tbl_insert(ysymbol);
    ysymbol_pt = 0;

    if (t != symbol_type::NEITHER)
        n->type = t;

    return n;
}

/*
 * Used when process section 1 of the grammar file.
 *
 * The token added here is a terminal.
 * These are declared in the first section of yacc input file.
 * empty string is not allowed.
 */
static void
output_terminal(SymbolNode* tokens_tail,
                yacc_section1_state state,
                yacc_section1_state prev_state,
                const char* token_type)
{
    bool must_add_token = false;

    if (ysymbol_pt == 0) {
        if (state == IS_QUOTED_TERMINAL) {
            throw std::runtime_error(
              std::string("error [line ") + std::to_string(n_line) + ", col " +
              std::to_string(n_col) + "]: empty token is not allowed");
        }
        return;
    }

    SymbolTblNode* n = get_symbol(symbol_type::NEITHER);

    switch (n->type) {
        case symbol_type::TERMINAL: /* already entered */
            must_add_token = false;
            if (n->token_type == nullptr && token_type != nullptr)
                n->token_type = token_type;
            break;

        case symbol_type::NEITHER: /* new symbol */
            n->type = symbol_type::TERMINAL;
            n->token_type = token_type;
            must_add_token = true;
            break;

        default:
            throw std::runtime_error(std::string("error ") + n->symbol +
                                     " used a terminal");
    }

    // get property of this terminal.
    // Quote informatin is used to decide to print it to y.tab.h.
    init_terminal_property(n);

    if (state == IS_QUOTED_TERMINAL) {
        n->TP->is_quoted = true;
        get_terminal_precedence(n, prev_state);
    } else {
        if (!validate_identifier(ysymbol)) {
            std::cout << "error [line " << n_line << ", col " << n_col
                      << "]: invalid identifier: " << ysymbol << std::endl;
            n->TP->is_quoted = false;
            get_terminal_precedence(n, state);
            return;
            // exit(1);
        }
        n->TP->is_quoted = false;
        get_terminal_precedence(n, state);
    }

    if (must_add_token)
        add_token(tokens_tail, n);
}

void
get_type_symbol(char* token_type)
{
    SymbolTblNode* n = get_symbol(symbol_type::NONTERMINAL);
    n->token_type = token_type;
}

void
get_start_symbol(char* token_type)
{
    start_symbol = get_symbol(symbol_type::NONTERMINAL);
    start_symbol->token_type = token_type;
}

/*
 * If more than one %expect value, use the first one.
 *
 * Note that atoi() function returns 0 if error occurs,
 * like when the ysymbol is a string "abc" and not a number.
 */
void
get_expect_sr_conflict()
{
    if (ysymbol_pt == 0)
        return;

    ysymbol[ysymbol_pt] = 0;

    if (expected_sr_conflict > 0) { // already got it.
        std::cout << "warning [" << n_line << ", " << n_col
                  << "]: more than one %expect value: " << ysymbol << std::endl;
        ysymbol_pt = 0;
        return;
    }

    expected_sr_conflict = atoi(ysymbol);
    if (expected_sr_conflict < 0) {
        using std::to_string;
        throw std::runtime_error(std::string("error [") + to_string(n_line) +
                                 ", " + to_string(n_col) + "]: %expect value " +
                                 ysymbol + " is not positive");
    }
    if (expected_sr_conflict == 0) {
        using std::to_string;
        throw std::runtime_error(std::string("error [") + to_string(n_line) +
                                 ", " + to_string(n_col) +
                                 "]: invalid %expect value: " + ysymbol);
    }

    // std::cout << "expect: " <<  expected_sr_conflict << std::endl;
    ysymbol_pt = 0;
}

/*
 * Note: At this time, don't worry about whether the first
 * char of a symbol should be a letter.
 */
void
add_char_to_symbol(char c)
{
    if (ysymbol_pt >= ysymbol_size - 1) { // one more for '\0'.
        ysymbol_size *= 2;

        if (ysymbol_size >= SYMBOL_MAX_SIZE + 1) {
            using std::to_string;
            ysymbol[ysymbol_pt - 1] = 0;
            throw std::runtime_error(
              std::string("[line ") + to_string(n_line) + ", col " +
              to_string(n_col) +
              "] add_char_to_symbol Error: symbol max size " +
              to_string(SYMBOL_MAX_SIZE) + " reached\nsymbol is: " + ysymbol);
        }

        HYY_EXPAND(&ysymbol, ysymbol_size);
        std::cout << "symbol size expanded to " << ysymbol_size << std::endl;
    }

    ysymbol[ysymbol_pt++] = c;
}

/*
 * Every occurence of "%left" or "%right" increases
 * the precedence by 1.
 */
auto
get_section1_state() -> yacc_section1_state
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
    std::cout << "error [line " << n_line << ", col " << n_col
              << "]: unknown directive %" << ysymbol << std::endl;
    // exit(1);
    return IS_UNKNOWN;
}

static void
my_perror(const char* msg, int c)
{
    using std::to_string;
    throw std::runtime_error(std::string("\nerror [line ") + to_string(n_line) +
                             ", col " + to_string(n_col) + "]: invalid char '" +
                             to_string(c) + "'. " + msg);
}

/*
 * Processes section 1 (declaration section)
 * of yacc input file.
 *
 * Currently this only gets the "%start" line if there is one.
 */
auto
process_yacc_file_input_section1(std::ifstream& fp) -> SymbolNode*
{
    yacc_section1_state state = IS_NONE,
                        prev_state = static_cast<yacc_section1_state>(-1);
    char* token_type = nullptr;
    int union_depth = 0;
    off_t union_start = 0;

    ysymbol_pt = 0;
    tokens = nullptr;
    SymbolNode* tokens_tail = nullptr;

    tokens_ct = 0;
    n_line = 1;
    n_col = 1;

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
                token_type = nullptr;
                state = IS_DIRECTIVE;
                add_char_to_symbol(c);
            } else {
                using std::to_string;
                throw std::runtime_error(
                  std::string("error [line ") + to_string(n_line) + ", col " +
                  to_string(n_col) + "]: wrong directive name: %" +
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
            if (state == IS_TOKEN && ysymbol_pt > 0) {
                output_terminal(tokens_tail, state, prev_state, token_type);
            }
        } else if (state == IS_DIRECTIVE) {
            if (isspace(c)) {
                if (ysymbol_pt == 0) {
                    my_perror("invalid char after %%", c);
                }
                add_char_to_symbol(0);
                state = get_section1_state();
                ysymbol_pt = 0;
            } else {
                add_char_to_symbol(c);
            }
        } else if (state == IS_QUOTED_TERMINAL) {
            // putchar(c);
            // avoid '\'' and '\\'.
            if (c == '\'' &&
                (last_c != '\\' || (ysymbol_pt == 2 && ysymbol[0] == '\\'))) {
                // std::cout << "] terminal ends" << std::endl;
                output_terminal(tokens_tail,
                                state,
                                prev_state,
                                token_type); // output quoted terminal.
                state = prev_state;
            } else if (!isspace(c)) {
                add_char_to_symbol(c);
            }
        } else if (state == IS_TOKEN_TYPE) {
            if (isspace(c)) {
                // do nothing, ignore space.
                // if there are spaces between the words in token type,
                // these words will be combined. However that's not valid
                // and should not happen.
            } else if (c == '>') {
                // process token type. to be implemented.
                ysymbol[ysymbol_pt] = 0;
                // std::cout << "token type [" <<  n_line<< ", " <<  n_col<< "]:
                // " <<  ysymbol << std::endl;

                token_type = new char[strlen(ysymbol) + 1];
                if (token_type != nullptr) {
                    strcpy(token_type, ysymbol);
                } else {
                    throw std::runtime_error("Out of memory");
                }
                ysymbol_pt = 0;
                state = prev_state;
            } else {
                add_char_to_symbol(c);
            }

        } else if (state == IS_TOKEN || state == IS_LEFT || state == IS_RIGHT ||
                   state == IS_NONASSOC) {
            if (isspace(c) && ysymbol_pt == 0) {
                // do nothing, ignore space
            } else if (isspace(c)) { // output another token
                output_terminal(tokens_tail, state, prev_state, token_type);
            } else if (c == '\'') {
                // std::cout << "terminal starts: ['";
                prev_state = state;
                state = IS_QUOTED_TERMINAL;
            } else if (c == '<') { // start of <token_type>.
                prev_state = state;
                state = IS_TOKEN_TYPE;
            } else { // add char to token string
                add_char_to_symbol(c);
            }

        } else if (state == IS_TYPE) { // %type declares non-terminals.
            if (isspace(c) && ysymbol_pt == 0) {
                // do nothing, ignore space
            } else if (isspace(c)) { // output another non-terminal.
                get_type_symbol(token_type);
            } else if (c == '<') { // start of <token_type>.
                prev_state = state;
                state = IS_TOKEN_TYPE;
            } else { // add char to token string
                add_char_to_symbol(c);
            }
        } else if (state == IS_START) {
            // else, do nothing, ignore this line.
            if (isspace(c) && ysymbol_pt == 0) {
                // do nothing, ignore space.
            } else if (isspace(c)) {          // output start token
                get_start_symbol(token_type); // start symbol is a non-terminal.
            } else {                          // add char to token string.
                add_char_to_symbol(c);
            }
        } else if (state == IS_UNION) {
            if (c == '{') { // union starts.
                if (union_depth++ == 0)
                    union_start =
                      static_cast<off_t>(fp.tellg()) - 1; // to include the {
            } else if (c == '}') {                        // union ends.
                if (--union_depth == 0) {
                    constexpr const char* const STR1 =
                      "typedef union YYSTYPE\n";
                    constexpr const char* const STR2 = "\n        YYSTYPE;";
                    off_t size = static_cast<off_t>(fp.tellg()) - union_start;
                    extern std::string yystype_definition;

                    yystype_definition = STR1;
                    fp.seekg(union_start, std::ios_base::beg);
                    std::vector<char> fp_string(size, ' ');
                    fp.read(fp_string.data(), size);
                    yystype_definition += fp_string.data();
                    yystype_definition += STR2;

                    state = IS_NONE;
                }
            } else { // get union content.
            }

        } else if (state == IS_EXPECT) {
            // else, do nothing, ignore this line.
            if (isspace(c) && ysymbol_pt == 0) {
                // ignore white space.
            } else if (isspace(c)) {
                get_expect_sr_conflict();
            } else {
                add_char_to_symbol(c);
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

        n_col++;
        if (c == '\n') {
            n_line++;
            n_col = 1;
        }
    }
    return tokens_tail;
    // writeTokens();
}

/////////////////////////////////////////////////////////////
// Functions to add rules to grammar. Start.
/////////////////////////////////////////////////////////////

/*
 * Used by function createNewRule().
 */
auto
create_empty_production() -> Production*
{
    auto* p = new Production;
    if (p == nullptr) {
        throw std::runtime_error(
          "create_empty_production error: output memory");
    }
    p->nLHS = SymbolNode::create(hash_tbl_find(""));

    p->RHS_count = 0;
    p->isUnitProduction = 0;
    p->hasCode = 0;
    p->nLHS = p->nRHS_head = p->nRHS_tail = nullptr;
    p->lastTerminal = nullptr;
    return p;
}

auto
create_new_rule() -> Production*
{
    grammar.rules.push_back(create_empty_production());
    return grammar.rules.back();
}

/*
 * Add a rule for mid-production action.
 *
 * @return: the new created non-terminal, which will
 *          be inserted to the position of the mid-production action.
 */
static void
insert_mid_prod_rule(int ct)
{
    constexpr size_t NAME_LHS_SIZE = 20;
    std::array<char, NAME_LHS_SIZE> name_lhs{};

    create_new_rule();

    // switch the postion of the last two rules.
    int rule_id = grammar.rules.size() - 1; // last rule's pointer.
    Production* p = grammar.rules[rule_id];
    grammar.rules[rule_id] = grammar.rules[rule_id - 1];
    grammar.rules[rule_id - 1] = p;

    // now fill the value of the new added rule.
    sprintf(name_lhs.data(), "$$%d_@%d", rule_id - 1, ct);
    SymbolTblNode* n = hash_tbl_insert(name_lhs.data());
    n->type = symbol_type::NONTERMINAL;
    p->nLHS = SymbolNode::create(n);
    p->hasCode = 1;

    add_rhs_symbol(n);
}

void
add_lhs_symbol(SymbolTblNode* symbol)
{
    // std::cout  << std::endl<< "==add LHS symbol: " <<  symbol << std::endl;
    Production* p = grammar.rules.back();
    p->nLHS = SymbolNode::create(symbol);
}

void
add_rhs_symbol(SymbolTblNode* symbol)
{
    // std::cout  << std::endl<< "==add RHS symbol: " <<  symbol << std::endl;
    Production* p = grammar.rules.back();
    p->RHS_count++;

    SymbolNode* s = SymbolNode::create(symbol);
    if (p->nRHS_head == nullptr) {
        p->nRHS_head = p->nRHS_tail = s;
    } else {
        p->nRHS_tail->next = s;
        p->nRHS_tail = s;
    }

    // if s is a terminal, it's the last terminal of p's RHS.
    if (s->snode->type == symbol_type::TERMINAL && s->snode->TP != nullptr)
        p->lastTerminal = s->snode;
}

/*
 * Assumption: symbol is the one after %prec in the RHS of p.
 */
void
get_rhs_prec_symbol(char* symbol)
{
    SymbolTblNode* n = hash_tbl_find(symbol);
    if (n == nullptr) {
        using std::to_string;
        throw std::runtime_error(std::string("error [line ") +
                                 to_string(n_line) + ", col " +
                                 to_string(n_col) + "]: %prec symbol " +
                                 symbol + " should be declared.");
    }
    Production* p = grammar.rules.back();
    if (n->TP != nullptr && n->TP->precedence > 0)
        p->lastTerminal = n;
}

void
set_has_code()
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
is_in_vanish_symbol_list(const SymbolTblNode* n) -> bool
{
    // std::cout << "isInVanishSymbolList input: " <<  symbol << std::endl;
    if (strlen(n->symbol) == 0) {
        return true;
    }

    for (SymbolNode* a = grammar.vanish_symbol_list; a != nullptr;
         a = a->next) {
        if (n == a->snode)
            return true;
    }

    return false;
}

/*
 * Used by the algorithm to find vanish symbol(s) of a grammar.
 */
auto
flag_y(Production* p) -> bool
{
    for (SymbolNode* a = p->nRHS_head; a != nullptr; a = a->next) {
        if (is_in_vanish_symbol_list(a->snode) == false)
            return false;
    }
    return true; // x1,...,xn are all vanish symbols. Flag y.
}

/*
 * Called when creating a grammar.
 *
 * Define epsilon as empty string "".
 * Then epsilon production is one whose
 * RHS_count = 1 AND RHS[0] == "".
 *
 * NOTE: a symbol that can vanish must occur
 * on the left side of a production, thus a
 * non-terminal symbol.
 *
 * Algorithm:
 *   1. Flag all symbols which occur in epsilon-productions,
 *      i.e., all symbols y which occur in productions of
 *      the form y -> epsilon.
 *   2. Go thru the grammar and for each produciton
 *      y -> x1...xn, if x1, ..., xn are all flagged,
 *      then flag y.
 *   3. If any new symbols were flagged in step 2, go
 *      back to step 2.
 */
void
get_vanish_symbols(Grammar* g)
{
    SymbolNode* tail = nullptr;
    g->vanish_symbol_count = 0;

    // find vanish symbols that occur in epsilon-productions.
    for (auto& rule : g->rules) {
        if (flag_y(rule)) {

            if (tail == nullptr) {
                tail = g->vanish_symbol_list =
                  SymbolNode::create(rule->nLHS->snode);
            } else {
                tail->next = SymbolNode::create(rule->nLHS->snode);
                tail = tail->next;
            }

            rule->nLHS->snode->vanishable = true;
            g->vanish_symbol_count++;
        }
    }

    // if so far no vanish symbol, then no epsilon-production.
    // then no more vanish symbols.
    if (g->vanish_symbol_count == 0)
        return;

    while (true) {
        bool new_vanish_symbol_found = false;
        for (auto& rule : g->rules) {
            if (is_in_vanish_symbol_list(rule->nLHS->snode) == false) {
                // y is not yet a vanish symbol, then:
                if (flag_y(rule)) {
                    // we know tail != nullptr
                    tail->next = SymbolNode::create(rule->nLHS->snode);
                    tail = tail->next;

                    rule->nLHS->snode->vanishable = true;

                    g->vanish_symbol_count++;
                    new_vanish_symbol_found = true;
                }
            }
        }
        if (!new_vanish_symbol_found)
            break;
    } // end while
}

/*
 * Given a grammar's rules, get non-terminals.
 *
 * Assumption: all non-terminals are used as LHS of
 * some rules. If not, such invalid non-terminal will
 * be reported after getSymbolRuleIDList().
 */
void
get_non_terminals(Grammar* g)
{
    int index = 0;
    SymbolNode* tail = nullptr;
    g->non_terminal_count = 0;

    // First scan LHS of all rules.
    for (const auto& rule : g->rules) {
        if (find_in_symbol_list(g->non_terminal_list, rule->nLHS->snode) !=
            nullptr) {
            continue;
        }
        if (tail == nullptr) {
            tail = g->non_terminal_list = SymbolNode::create(rule->nLHS->snode);
        } else {
            tail->next = SymbolNode::create(rule->nLHS->snode);
            tail = tail->next;
        }

        if (strcmp(tail->snode->symbol, STR_ACCEPT) != 0)
            tail->snode->value = (-1) * (++index);

        g->non_terminal_count++;
    }

    bool has_error = false;
    // Next scan RHS of all rules.
    // no extra non-terminal should appear, since otherwise
    // it's not used as the LHS of any rule.
    for (const auto& rule : g->rules) {
        for (tail = rule->nRHS_head; tail != nullptr; tail = tail->next) {
            if (tail->snode->type != symbol_type::NONTERMINAL)
                continue;

            if (find_in_symbol_list(g->non_terminal_list, tail->snode) ==
                nullptr) {
                std::cerr << "error: non-termnal '" << tail->snode->symbol
                          << "' is not used as the LHS of any rule"
                          << std::endl;
                ;
                has_error = true;
            }
        }
    }
    // if (has_error ) exit(1);
}

/*
 * Assumption:
 * This function is called after getNonTerminals().
 * empty string is not included as terminal.
 */
void
get_terminals(Grammar* g)
{
    SymbolNode* tail = nullptr;
    g->terminal_count = 0;

    for (const auto& rule : g->rules) {
        SymbolNode* s = rule->nRHS_head;
        for (int j = 0; j < rule->RHS_count; j++) {
            if (j > 0)
                s = s->next; // s: g->rules[i]->RHS[j].
            char* symbol = s->snode->symbol;

            if (s->snode->type != symbol_type::TERMINAL)
                continue;

            // is an empty string.
            if (strlen(s->snode->symbol) == 0)
                continue;

            if (find_in_symbol_list(g->terminal_list, s->snode) != nullptr)
                continue;

            if (tail == nullptr) {
                tail = g->terminal_list = SymbolNode::create(s->snode);
            } else {
                tail->next = SymbolNode::create(s->snode);
                tail = tail->next;
            }
            s->snode->type = symbol_type::TERMINAL;
            g->terminal_count++;
        } // end of for
    }     // end of for
}

/*
 * ref: page 38. The C programming language.
 */
auto
get_escape_char(char c) -> char
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

/*
 * Used by getTokensValue() only.
 */
auto
get_token_value(const char* s, int* index) -> int
{

    if (strlen(s) == 1)
        return s[0]; // single letter.

    int val = 0;
    if (strlen(s) == 2 && s[0] == '\\') { // escaped sequence.
        val = static_cast<unsigned char>(get_escape_char(s[1]));
        if (val != 0)
            return val;
    }

    if (strcmp(s, STR_ERROR) == 0)
        return 256;
    if (strcmp(s, STR_END) == 0)
        return 0;

    val = 257 + (*index);
    (*index)++;
    return val;
}

/*
 * This function gets the values of all terminals,
 * Including those after %prec in the rule section,
 * and including STR_END, STR_ERROR.
 *
 * Symbol can be:
 *   - a token, value is 257 + index in token list (in un-quoted ones).
 *   - "error", value is 256.
 *   - a char or escaped char, value is its ascii code.
 *   - STR_END, value is 0.
 *   - a non-terminal symbol, value is -1 * index in non-terminal list.
 *
 * The values of non-terminals are calculated in
 * getNonTerminals().
 *
 * STR_EMPTY has no value. STR_ACCEPT is a non-terminal,
 * whose value is default to 0. But the value of these
 * two are never used, so doesn't matter.
 *
 * The values of tokens are used for y.tab.h, for yytoks[]
 * and for yytbltok[] in y.tab.c.
 *
 * Note that the terminal list is a subset of tokens list.
 * The tokens list can include those after %prec in the
 * rule section, which terminal list does not include.
 */
void
get_tokens_value(Grammar* g)
{
    int index = 0;
    for (SymbolNode* a = tokens; a != nullptr; a = a->next) {
        a->snode->value = get_token_value(a->snode->symbol, &index);
    }
}

void
get_goal_symbol(Grammar* g)
{
    g->goal_symbol = SymbolNode::create(g->rules[0]->nLHS->snode);
}

/*
 * Get the column index in the parsing table for
 * each symbol.
 * This makes it easy to find the address for a symbol
 * in the parsing table.
 *
 * This can be done when calling getNonTerminals() and
 * getTerminals(), but doing it here separately makes
 * it easy to check, debug and change.
 * Since there are not many symbols, this will take
 * only very little time.
 */
void
get_symbol_parsing_tbl_col(Grammar* g)
{
    SymbolNode* a = g->terminal_list;
    SymbolTblNode* n = hash_tbl_find(STR_END);
    n->seq = 0;

    for (int i = 1; a != nullptr; a = a->next, i++) {
        n = hash_tbl_find(a->snode->symbol);
        n->seq = i;
    }

    for (int i = 1; a != nullptr; a = a->next, i++) {
        n = hash_tbl_find(a->snode->symbol);
        n->seq = g->terminal_count + i;
    }
}

/*
 *  Get the list of Parsing table column header symbols.
 *  This plus the getSymbolParsingTblCol() function make it
 *  easy to refer between column number and column symbol.
 */
void
get_parsing_tbl_col_hdr(Grammar* g)
{
    ParsingTblColHdr = std::vector<SymbolTblNode*>(
      1 + g->terminal_count + g->non_terminal_count, nullptr);

    ParsingTblColHdr.at(0) = hash_tbl_find(STR_END);
    ParsingTblCols = 1;

    for (SymbolNode* a = grammar.terminal_list; a != nullptr; a = a->next) {
        ParsingTblColHdr.at(ParsingTblCols++) = a->snode;
    }

    for (SymbolNode* a = g->non_terminal_list; a != nullptr; a = a->next) {
        ParsingTblColHdr.at(ParsingTblCols++) = a->snode;
    }
}

/*
 * For each non-terminal, get a list of indexes of rules
 * for which the non-terminal is the LHS.
 *
 * If a non-terminal is not the LHS of any rule,
 * it's an error and should be reported.
 */
void
get_symbol_rule_id_list(Grammar* g)
{
    for (SymbolNode* a = g->non_terminal_list; a != nullptr; a = a->next) {
        SymbolTblNode* n = a->snode;
        RuleIDNode* tail = nullptr;
        for (int i = 0; i < g->rules.size(); i++) {
            if (n == g->rules[i]->nLHS->snode) {
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
    // writeNonTerminalRuleIDList();
}

void
get_grammar_unit_productions(Grammar* g)
{
    for (int i = 0; i < g->rules.size(); i++) {
        if (grammar.rules[i]->RHS_count == 1 &&
            strlen(grammar.rules[i]->nRHS_head->snode->symbol) > 0) {
            g->rules[i]->isUnitProduction = true;
        }
    }
}

void
get_grammar_params()
{
    get_non_terminals(&grammar);
    get_terminals(&grammar);

    get_tokens_value(&grammar);

    get_goal_symbol(&grammar);
    get_vanish_symbols(&grammar);

    get_symbol_parsing_tbl_col(&grammar);
    get_parsing_tbl_col_hdr(&grammar);

    get_symbol_rule_id_list(&grammar);
    get_grammar_unit_productions(&grammar);

    // writeSymbolList(grammar.non_terminal_list, "NT list");
    // writeSymbolList(grammar.terminal_list, "T list");
    // writeSymbolList(tokens, "tokens");
}

/////////////////////////////////////////////////////////////
// Functions to get grammar parameters. START.
/////////////////////////////////////////////////////////////

/*
 * Outputs a new symbol and inserts it to the LHS or RHS
 * of a rule of the grammar.
 *
 * Note that a construct like "%prec UNARY" indicates the
 * precedence of this rule. Both "%prec" and "UNARY" are
 * not true terminals, and should not be inserted into the
 * terminal symbol list. The detail  of handling precedence
 * is to be implemented.
 *
 * If a new symbol is on the LHS, it's a non-terminal.
 * If it is on the RHS and not found in the symbol table,
 * and not quoted by '', then it's a non-terminal.
 * Note that all terminals not quoted by '' should already
 * be delcared in section 1.
 *
 * Empty string is not allowed.
 */
auto
output_nonterminal(SymbolNode* tokens_tail, YACC_STATE state) -> SymbolTblNode*
{
    if (ysymbol_pt == 0) {
        if (state == TERMINAL) {
            using std::to_string;
            throw std::runtime_error(
              std::string("error [line ") + to_string(n_line) + ", col " +
              to_string(n_col) + "]: empty token is not allowed");
        }
        return nullptr;
    }

    ysymbol[ysymbol_pt] = 0;

    if (y_strcasecmp(ysymbol, "%prec") == 0) {
        IS_PREC = 1;
        ysymbol_pt = 0;
        return nullptr;
    }
    if (IS_PREC == 1) {
        // is a ficticious terminal, no token actually.
        get_rhs_prec_symbol(ysymbol); // ysymbol should exist.
        // std::cout << "%prec on symbol " <<  ysymbol << std::endl;
        IS_PREC = 0;
        ysymbol_pt = 0;
        return nullptr;
    }
    // std::cout << "another symbol: " <<  ysymbol << std::endl;

    if constexpr (DEBUG_YACC_INPUT_PARSER) {
        std::cout << ysymbol << ' ';
    }

    SymbolTblNode* n = hash_tbl_find(ysymbol);

    if (yacc_sec2_state == LHS) {
        if (n == nullptr) {
            n = hash_tbl_insert(ysymbol);
            n->type = symbol_type::NONTERMINAL;
        } else if (n->type == symbol_type::TERMINAL) {
            using std::to_string;
            throw std::runtime_error(
              std::string("error [line ") + to_string(n_line) + ", col " +
              to_string(n_col) + "]: symbol " + ysymbol +
              " is declared as terminal but used as non-terminal");
        }
        create_new_rule(); // CREATE NEW RULE HERE.
        add_lhs_symbol(n);

    } else { // RHS.
        if (n == nullptr) {
            n = hash_tbl_insert(ysymbol);

            // if quoted by '', is terminal, otherwise is non-terminal.
            // "error" is a reserved word, a terminal.
            if (state == TERMINAL || strcmp(ysymbol, STR_ERROR) == 0) {
                n->type = symbol_type::TERMINAL;
                init_terminal_property(n);
                n->TP->is_quoted = true;

                // add this to the tokens list.
                add_token(tokens_tail, n);
            } else {
                n->type = symbol_type::NONTERMINAL;
            }
        }

        add_rhs_symbol(n);
    }

    ysymbol_pt = 0;
    return n;
}

/*
 * Stores LHS of current rule.
 * To be used by the next rule with the same LHS in cases like:
 *   LHS : RHS1 | RHS2 ...
 */
void
get_cur_lhs(SymbolTblNode* n)
{
    if (n == nullptr) {
        using std::to_string;
        throw std::runtime_error(std::string("error [line ") +
                                 to_string(n_line) + ", col " +
                                 to_string(n_col) + "]: LHS symbol is empty.");
    }
    curLHS = n;
}

/*
 * Processes section 2 (grammar section)
 * of a yacc input file.
 */
void
process_yacc_file_input_section2(SymbolNode* tokens_tail, std::ifstream& fp)
{
    // for mid-production actions.
    int mid_prod_code_ct = 0;
    bool end_of_code = false;

    yacc_sec2_state = LHS;
    ysymbol_pt = 0;

    char c = 0, last_c = 0;
    while (fp.get(c)) {
        if (last_c == '%' && c == '%')
            return;

        switch (yacc_sec2_state) {
            case LHS:
                if (isspace(c) && ysymbol_pt == 0) {
                    // Ignore empty spaces before LHS symbol.
                } else if (c == ':') {
                    get_cur_lhs(output_nonterminal(tokens_tail,
                                                   LHS)); // OUTPUT LHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << "-> ";
                    }
                    yacc_sec2_state = RHS;
                } else if (isspace(c)) {
                    get_cur_lhs(output_nonterminal(tokens_tail,
                                                   LHS)); // OUTPUT LHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << "-> ";
                    }
                    yacc_sec2_state = COLON;
                } else if (last_c == '/' && c == '*') {
                    yacc_sec2_state = LHS_COMMENT;
                    ysymbol_pt = 0;
                } else if (c == '/') {
                    // do nothing. '/' is not a valid char for a symbol.
                } else if (c == ';') {
                    // do nothing. a rule without anything.
                } else if (!isspace(c)) {
                    // when encountering the '%%' starting section 3,
                    // the first '%' will be inserted. But since it won't
                    // call output_nonterminal(), a new rule won't be created.
                    // So no problem will occur here.
                    add_char_to_symbol(c);
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
                    my_perror("in state COLON", c);
                }
                break;
            case COLON_COMMENT:
                if (last_c == '*' && c == '/') {
                    yacc_sec2_state = COLON;
                }
                break;
            case RHS:
                if (isspace(c)) {
                    if (ysymbol_pt != 0) {
                        output_nonterminal(tokens_tail,
                                           RHS); // OUTPUT NEXT RHS SYMBOL.
                    }
                    // else, ignore empty space.
                } else if (c == '\'') {
                    // std::cout << "terminal starts(line " <<  n_line<< "): ["
                    // <<  c;
                    if (ysymbol_pt != 0) {
                        output_nonterminal(tokens_tail,
                                           RHS); // OUTPUT NEXT RHS SYMBOL.
                    }
                    yacc_sec2_state = TERMINAL;
                    if (end_of_code) { // for mid-prod action.
                        mid_prod_code_ct++;
                        insert_mid_prod_rule(mid_prod_code_ct);
                        end_of_code = false;
                    }
                } else if (c == ';') {
                    end_of_code = false; // for mid-prod action.
                    if (ysymbol_pt == 0 && IS_PREC == 1) {
                        using std::to_string;
                        throw std::runtime_error(
                          std::string("error [line ") + to_string(n_line) +
                          ", col " + to_string(n_col) +
                          "]: forgot the symbol after %prec?");
                    }
                    if (ysymbol_pt != 0)
                        output_nonterminal(tokens_tail,
                                           RHS); // OUTPUT NEXT RHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << std::endl;
                    }
                    yacc_sec2_state = LHS;
                } else if (c == '|') {   // another rule with same LHS.
                    end_of_code = false; // for mid-prod action.
                    if (ysymbol_pt == 0 && IS_PREC == 1) {
                        using std::to_string;
                        throw std::runtime_error(
                          std::string("error [line ") + to_string(n_line) +
                          ", col " + to_string(n_col) +
                          "]: forgot the symbol after %prec?");
                    }
                    if (ysymbol_pt != 0)
                        output_nonterminal(tokens_tail,
                                           RHS); // OUTPUT NEXT RHS SYMBOL.
                    if constexpr (DEBUG_YACC_INPUT_PARSER) {
                        std::cout << std::endl << curLHS->symbol << " -> ";
                    }
                    // std::cout  << std::endl;
                    create_new_rule(); // CREATE NEW RULE HERE.
                    add_lhs_symbol(curLHS);
                } else if (c == '{') {
                    set_has_code(); // has associated code.
                    /// std::cout << "start of code at rule " <<
                    ///  grammar.rule_count<< ":" << std::endl<< "{";
                    yacc_sec2_state = CODE;
                    CODE_level = 1;
                } else if (last_c == '/' && c == '*') {
                    yacc_sec2_state = COMMENT; // the format "/* ... */"
                } else if (last_c == '/' && c == '/') {
                    yacc_sec2_state = COMMENT2; // the format "// ..."
                } else if (c == '/') {
                    // do nothing.
                } else if (c == ':') {
                    my_perror("A ';' is missed in the last rule?", c);
                } else {
                    if (end_of_code) { // for mid-prod action.
                        mid_prod_code_ct++;
                        insert_mid_prod_rule(mid_prod_code_ct);
                        end_of_code = false;
                    }
                    add_char_to_symbol(c);
                }
                break;
            case TERMINAL:
                // putchar(c);
                // avoid '\'' and '\\'.
                if (c == '\'' && (last_c != '\\' ||
                                  (ysymbol_pt == 2 && ysymbol[0] == '\\'))) {
                    // std::cout << "] terminal ends" << std::endl;
                    yacc_sec2_state = RHS;
                    output_nonterminal(tokens_tail,
                                       TERMINAL); // OUTPUT NEXT RHS SYMBOL. is
                                                  // terminal.
                } else {
                    /* if (isspace(c))std::cout << "hit space here " << n_line<<
                     * " " <<  n_col << std::endl; */
                    add_char_to_symbol(c);
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
                } else if (c == '}' && CODE_level == 1) {
                    yacc_sec2_state = RHS;
                    /// std::cout << "end of code" << std::endl;
                    end_of_code = true; // for mid-prod action.
                } else if (c == '{') {
                    CODE_level++;
                } else if (c == '}') {
                    CODE_level--;
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
        } // end switch

        // putchar(c);
        last_c = c;

        n_col++;
        if (c == '\n') {
            n_line++;
            n_col = 1;
        }

    } // end while
}

/*
 * If there is a "%start ..." in the declaration section,
 * copy the start symbol value to RHS of goal production.
 * Otherwise, use the LHS of the first user rule as the
 * RHS of goal production.
 */
void
get_goal_rule_rhs()
{
    if (grammar.rules.size() > 1) {
        if (start_symbol != nullptr) {
            grammar.rules[0]->nRHS_head = grammar.rules[0]->nRHS_tail =
              SymbolNode::create(start_symbol);
        } else {
            grammar.rules[0]->nRHS_head = grammar.rules[0]->nRHS_tail =
              SymbolNode::create(grammar.rules[1]->nLHS->snode);
        }
        grammar.rules[0]->RHS_count = 1;
    } else {
        throw std::runtime_error(
          "getGoalRuleRHS() error: there is no user rule.");
    }
}

void
get_goal_rule_lhs()
{
    SymbolTblNode* n = hash_tbl_insert(STR_ACCEPT);
    create_new_rule(); // goal production rule.
    grammar.rules[0]->nLHS = SymbolNode::create(n);
    n->type = symbol_type::NONTERMINAL;
}

/*
 * This modification is to preserve those unit productions
 * that have associated code. In such case add a place holder
 * nonterminal to the end of each such unit production, to
 * convert them to non-unit productions. This place holder
 * nonterminal will reduce to empty string.
 */
void
post_modification(Grammar* g)
{
    auto& options = Options::get();
    // std::cout << "calling post_modification" << std::endl;
    if (options.use_remove_unit_production == false)
        return;
    if (options.preserve_unit_prod_with_code)
        return;

    SymbolTblNode* n = hash_tbl_insert(STR_PLACE_HOLDER);
    n->type = symbol_type::NONTERMINAL;

    int count = 0;
    for (const auto& rule : g->rules) {
        if (rule->RHS_count == 1 && rule->hasCode == 1u) {
            // std::cout << "rule " <<  i<< " is a unit production with code" <<
            // std::endl;
            count++;
            Production* p = rule;
            p->RHS_count++;
            p->isUnitProduction = 0;
            // add one more symbol to the end of RHS:
            p->nRHS_tail->next = SymbolNode::create(n);
            p->nRHS_tail = p->nRHS_tail->next;
        }
    }

    if (count > 0) {
        Production* p = create_new_rule(); // $PlaceHolder -> epsilon
        p->nLHS = SymbolNode::create(n);

        p->RHS_count = 0;
        p->isUnitProduction = 0;
        p->hasCode = 0;
        p->nRHS_head = p->nRHS_tail = nullptr;
    }
}

void
get_yacc_grammar_init()
{
    // insert special symbols to hash table.
    SymbolTblNode* n = hash_tbl_insert(STR_END); // end marker of production.
    n->type = symbol_type::TERMINAL;

    hash_tbl_insert(STR_ACCEPT);
    hash_tbl_insert(STR_END);
    n = hash_tbl_insert(STR_EMPTY);
    n->type = symbol_type::TERMINAL;
    n->vanishable = 1;

    grammar.rules.reserve(GRAMMAR_RULE_INIT_MAX_COUNT);

    ysymbol_size = SYMBOL_INIT_SIZE;
    ysymbol = new char[ysymbol_size];

    start_symbol = nullptr;

    precedence = 0;
    IS_PREC = 0;
    expected_sr_conflict = 0;
}

/*
 * The main function of this file.
 * Gets grammar from a yacc input file.
 *
 * Called by function main() in y.c.
 */
void
get_yacc_grammar(const std::string& infile)
{
    if constexpr (DEBUG_YACC_INPUT_PARSER) {
        std::cout << "input file: " << infile << std::endl;
    }

    std::ifstream fp{};
    fp.open(infile);
    if (!fp.is_open()) {
        throw std::runtime_error(std::string("can't open file ") + infile);
    }

    get_yacc_grammar_init();

    if constexpr (ADD_GOAL_RULE) {
        get_goal_rule_lhs();
    }

    SymbolNode* tokens_tail = process_yacc_file_input_section1(fp);
    process_yacc_file_input_section2(tokens_tail, fp);

    fp.close();

    if constexpr (ADD_GOAL_RULE) {
        get_goal_rule_rhs();
    }
    post_modification(&grammar);

    get_grammar_params();

    n_rule = grammar.rules.size();
    n_symbol = grammar.terminal_count + grammar.non_terminal_count;
    n_rule_opt = grammar.get_opt_rule_count();
    // writeGrammar(& grammar); exit(0);
}
