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

/*
 * gen_compiler.c
 *
 * Contains functions to generate a compiler.
 *
 * @Author: Xin Chen
 * @Date started: 10/16/2006
 * @Last modified: 3/21/2007
 */

#include "lane_tracing.hpp"
#include "y.hpp"
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

constexpr size_t MAX_RULE_LENGTH = 0xfffff;
constexpr int INTEGER_PADDING = 6;
constexpr int ITEM_PER_LINE = 10;

std::string yystype_definition = "typedef int YYSTYPE;";
constexpr std::string_view YYSTYPE_FORMAT =
  "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED\n"
  "%s\n"
  "#define YYSTYPE_IS_DECLARED 1\n"
  "#endif\n";

static void
prepare_outfile(std::ofstream& fp, std::ofstream& fp_h, const FileNames& files)
{
    fp.open(files.y_tab_c);
    if (!fp.is_open()) {
        throw std::runtime_error(std::string("Cannot open output file ") +
                                 files.y_tab_c);
    }
    if (Options::get().use_header_file == false)
        return;
    fp_h.open(files.y_tab_h);
    if (!fp_h.is_open()) {
        fp.close();
        throw std::runtime_error(std::string("Cannot open output file ") +
                                 files.y_tab_h);
    }
}

static void
my_perror(const char* msg, char c, int n_line, int n_col)
{
    using std::to_string;
    throw std::runtime_error(std::string("\nerror [line ") + to_string(n_line) +
                             ", col " + to_string(n_col) + "]: invalid char '" +
                             to_string(c) + "'. " + msg);
}

inline void
print_break(std::ofstream& fp)
{
    fp << "break;" << std::endl;
}

/*
 * Token - terminal symbols.
 */
void
write_tokens()
{
    int i = 0;
    SymbolNode* a = nullptr;
    for (a = tokens, i = 0; a != nullptr; a = a->next, i++) {
        std::cout << "token " << i + 1 << ": " << a->snode->symbol << std::endl;
    }
}

/*
 *  Write all terminal tokens that are not quoted, and not "error".
 */
void
write_tokens_to_compiler_file(std::ofstream& fp, std::ofstream& fp_h)
{
    auto& options = Options::get();
    fp << std::endl << "/* tokens */" << std::endl << std::endl;
    if (options.use_header_file)
        fp_h << std::endl << "/* tokens */" << std::endl << std::endl;

    int index = 0;
    int i = 0;
    for (SymbolNode* a = tokens; a != nullptr; a = a->next, i++) {
        if (a->snode->TP->is_quoted || *a->snode->symbol == STR_ERROR)
            continue;

        fp << "#define " << a->snode->symbol << index + 257 << std::endl;
        if (options.use_header_file)
            fp_h << "#define " << a->snode->symbol << index + 257 << std::endl;
        index++;
    }

    fp << "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED" << std::endl
       << yystype_definition << std::endl
       << "#define YYSTYPE_IS_DECLARED 1" << std::endl
       << "#endif" << std::endl;
    if (options.use_header_file) {
        fp_h << "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED"
             << std::endl
             << yystype_definition << std::endl
             << "#define YYSTYPE_IS_DECLARED 1" << std::endl
             << "#endif" << std::endl;
        fp_h << std::endl << "extern YYSTYPE yylval;" << std::endl;
    }
}

/*
 *  Get code declarations from section 1, write to
 *  y.tab.c, and write token declarations too.
 */
void
process_yacc_file_section1(std::ifstream& fp_yacc,
                           std::ofstream& fp,
                           std::ofstream& fp_h,
                           int& n_line,
                           int& n_col)
{
    n_line = 1;
    n_col = 1;

    bool is_code = false;
    char c = 0, last_c = '\n', last_last_c = 0;
    while (fp_yacc.get(c)) {
        if (last_c == '\n' && c == '%') {
            is_code = false;
        } else if (last_last_c == '\n' && last_c == '%') {
            if (c == '%') {
                break; // end of section1.
            }
            if (c == '{') {
                is_code = true;
            } else if (c == '}') {
                is_code = false;
            }
        } else if (is_code) {
            fp << c;
        }

        last_last_c = last_c;
        last_c = c;

        n_col += 1;
        if (c == '\n') {
            n_line += 1;
            n_col = 1;
        }
    }

    // writeTokens();
    write_tokens_to_compiler_file(fp, fp_h);
}

/*
 * rewind to section 2.
 */
static void
goto_section2(std::ifstream& fp_yacc, int& n_line)
{
    char c = 0, last_c = 0, last_last_c = '\n';

    fp_yacc.seekg(0); // go to the beginning of file fp.
    n_line = 1;
    while (fp_yacc.get(c)) {
        if (c == '\n')
            n_line += 1;
        if (last_last_c == '\n' && last_c == '%' && c == '%')
            break; // end section 2.

        last_last_c = last_c;
        last_c = c;
    }
}

/*
 * Pass section 2, go to section 3.
 * Presumption: finished section 1, entering section 2.
 */
static void
goto_section3(std::ifstream& fp_yacc, int& n_line)
{
    char c = 0, last_c = 0, last_last_c = '\n';

    while (fp_yacc.get(c)) {
        if (c == '\n')
            n_line++;
        if (last_last_c == '\n' && last_c == '%' && c == '%') {
            break; // end section 2.
        }
        last_last_c = last_c;
        last_c = c;
    }
}

static auto
find_full_rule(const Grammar& grammar, int rule_count) -> Production*
{
    Production* rule = nullptr;
    const SymbolNode* node = nullptr;
    const SymbolTblNode* sym = nullptr;

    for (int full_rule = rule_count; full_rule < grammar.rules.size();
         ++full_rule) {

        if ((rule = grammar.rules[full_rule]) && (node = rule->nLHS) &&
            (sym = node->snode)) {
            if (sym->symbol->starts_with(
                  "$$")) /* node symbol starting with $$ we continue */
                break;
        } else {
            throw std::runtime_error(
              std::string("Malformed grammar rule at index ") +
              std::to_string(full_rule));
        }
    }

    return rule;
}

static auto
find_mid_prod_index(const Production* rule, const Production* mid_prod_rule)
  -> int
{
    const SymbolNode* lnode = mid_prod_rule->nLHS;
    const SymbolTblNode* lsym = lnode->snode;
    const std::string_view l = *lsym->symbol;
    const SymbolNode* rnode = rule->nRHS_head;
    const SymbolTblNode* rsym = nullptr;

    for (int i = 0; rnode; ++i, rnode = rnode->next) {
        const std::string_view r = *rsym->symbol;
        if (!(rsym = rnode->snode)) {
            throw std::runtime_error(
              std::string("Did not find mid production rule of ").append(l));
        }
        if (l == r)
            return i;
    }
    return -1;
}

static auto
find_sym(const Production* rule, int dollar_number) -> const SymbolTblNode*
{
    const SymbolTblNode* sym = nullptr;
    if (dollar_number == MAX_RULE_LENGTH)
        return sym;

    int i = 1;
    const SymbolNode* node = rule->nRHS_head;
    for (; i < dollar_number && node; ++i, node = node->next)
        ;
    if (i != dollar_number) {
        throw std::runtime_error(std::string("Rule terminated before ") +
                                 std::to_string(dollar_number) + " RHS");
    }
    if (!(sym = node->snode)) {
        throw std::runtime_error(std::string("Malformed grammar rule RHS ") +
                                 std::to_string(dollar_number) +
                                 " had no symbol table node");
    }
    return sym;
}

/*
 * Basically, this has the same structure as function
 *   processYaccFileInput_section2(int c)
 * in parsetYaccInput.c.
 *
 * The purpose here is to extract the code for semantic
 * actions of rules.
 */
static void
process_yacc_file_section2(GetYaccGrammarOutput& yacc_grammar_output,
                           std::ifstream& fp_yacc,
                           std::ofstream& fp,
                           const std::string_view filename,
                           int n_line,
                           int n_col)
{
    YACC_STATE state = LHS;
    int code_level = 0;
    bool reading_symbol = false;
    bool reading_number = false;
    bool reading_type = false;
    int dollar_number = 0;
    char c = 0, last_c = 0, last_last_c = 0;
    int rule_count = 0;
    bool end_of_code = false; // for mid-production action.
    std::optional<std::string> explicit_type = std::nullopt;
    static const std::string_view padding = "        ";

    while (fp_yacc.get(c)) {
        if (last_c == '%' && c == '%')
            break; // end section 2.

        switch (state) {
            case LHS:
                if (isspace(c) && reading_symbol == false) {
                    // do nothing, skip space.
                } else if (c == ':') {
                    reading_symbol = false;
                    state = RHS;
                } else if (isspace(c)) { // finish reading symbol
                    reading_symbol = false;
                    state = COLON;
                } else if (last_c == '/' && c == '*') {
                    state = LHS_COMMENT;
                    if (reading_symbol) {
                        reading_symbol = false;
                        rule_count--;
                    }
                } else if (c == '/') { // do nothing.
                } else if (c == ';') { // do nothing.
                } else if (!isspace(c)) {
                    if (reading_symbol == false) {
                        rule_count++;
                        reading_symbol = true;
                    }
                }
                break;
            case LHS_COMMENT:
                if (last_c == '*' && c == '/') {
                    state = LHS;
                }
                break;
            case COLON:
                if (c == ':') {
                    state = RHS;
                } else if (c == '/') {
                    // do nothing
                } else if (last_c == '/' && c == '*') {
                    state = COLON_COMMENT;
                } else if (!isspace(c)) {
                    my_perror("error: state COLON. ", c, n_line, n_col);
                }
                break;
            case COLON_COMMENT:
                if (last_c == '*' && c == '/') {
                    state = COLON;
                }
                break;
            case RHS:
                if (isspace(c)) {
                    // end of a symbol. do nothing.
                } else if (c == '\'') {
                    state = TERMINAL;
                    if (end_of_code) {
                        rule_count++;
                        print_break(fp);
                        end_of_code = false;
                    }

                } else if (c == ';') {
                    state = LHS; // end of a rule.
                    if (end_of_code) {
                        print_break(fp);
                    }
                    end_of_code = false;
                } else if (c == '|') { // end of a rule
                    rule_count++;
                    if (end_of_code) {
                        print_break(fp);
                    }
                    end_of_code = false;
                } else if (c == '{') { // code for rule #rule_count
                    state = CODE;
                    code_level = 1;

                    if (end_of_code == false) {
                        fp << std::endl
                           << padding << "  case " << rule_count << ':'
                           << std::endl;
                        if (Options::get().use_lines)
                            fp << "# line " << n_line << " \"" << filename
                               << '\"' << std::endl;
                    }
                    fp << '{';

                } else if (last_c == '/' && c == '*') {
                    state = COMMENT;
                } else if (last_c == '/' && c == '/') {
                    state = COMMENT2;
                } else if (c == '/') {
                    // do nothing
                } else if (c == ':') {
                    my_perror(
                      "You may miss a ';' in the last rule.", c, n_line, n_col);
                } else {
                    // reading a symbol. do nothing
                    if (end_of_code) {
                        rule_count++;
                        print_break(fp);
                        end_of_code = false;
                    }
                }
                break;
            case TERMINAL:
                // avoid '\'' and '\\'.
                if (c == '\'' && (last_c != '\\' || last_last_c == '\\')) {
                    state = RHS;
                } else if (!isspace(c)) {
                    // reading a symbol. do nothing.
                }
                break;
            case CODE: // the meat. write code to yacc output file.
                if (reading_type) {
                    if (c == '>') {
                        reading_type = false;
                        c = '$';
                        yacc_grammar_output.add_char_to_symbol('\0');
                        explicit_type = yacc_grammar_output.get_symbol();
                    } else {
                        yacc_grammar_output.add_char_to_symbol(c);
                    }
                } else if (last_c == '$' && c == '<') {
                    yacc_grammar_output.reset_symbol();
                    reading_type = true;
                } else if (last_c != '$' && c == '$') {
                    // do nothing, this may be a special character.
                } else if (last_c == '$' && c == '$') {
                    std::optional<std::string> token_type = std::nullopt;
                    if (explicit_type) {
                        token_type = explicit_type;
                        explicit_type = std::nullopt;
                    } else {
                        const Production* rule = find_full_rule(
                          yacc_grammar_output.grammar, rule_count);
                        token_type = rule->nLHS->snode->token_type;
                    }
                    if (token_type)
                        fp << "(yyval." << token_type.value() << ')';
                    else
                        fp << "yyval";

                } else if (last_c == '$' && isdigit(c)) {
                    reading_number = true;
                    dollar_number = (c - '0') + 10 * dollar_number;
                } else if (reading_number && isdigit(c)) {
                    dollar_number = (c - '0') + 10 * dollar_number;
                } else if (reading_number && !isdigit(c)) {
                    Production* start_rule =
                      yacc_grammar_output.grammar.rules[rule_count];
                    int rhs_index = 0;

                    const Production* rule =
                      find_full_rule(yacc_grammar_output.grammar, rule_count);
                    if (rule != start_rule)
                        rhs_index = find_mid_prod_index(rule, start_rule);
                    else
                        rhs_index = rule->RHS_count;
                    std::optional<std::string> token_type = std::nullopt;
                    if (explicit_type) {
                        token_type = explicit_type;
                        explicit_type = std::nullopt;
                    } else {
                        const SymbolTblNode* sym =
                          find_sym(rule, dollar_number);
                        token_type = sym->token_type;
                    }
                    if (token_type)
                        fp << "(yypvt[" << dollar_number - rhs_index << "]."
                           << token_type.value() << ")/* " << rule_count << ' '
                           << rhs_index << " */";
                    else
                        fp << "yypvt[" << dollar_number - rhs_index << "]/* "
                           << rule_count << ' ' << rhs_index << " */";

                    fp << c;
                    reading_number = false;
                    dollar_number = 0;
                } else {
                    fp << c;
                }

                if (c == '\"') {
                    state = CODE_DOUBLE_QUOTE;
                } else if (c == '\'') {
                    state = CODE_SINGLE_QUOTE;
                } else if (c == '*' && last_c == '/') {
                    state = CODE_COMMENT;
                } else if (c == '/' && last_c == '/') {
                    state = CODE_COMMENT2;
                } else if (c == '}' && code_level == 1) {
                    fp << ' '; // print_break();
                    state = RHS;
                    //!!
                    end_of_code = true; // end of a section of code.

                } else if (c == '{') {
                    code_level++;
                } else if (c == '}') {
                    code_level--;
                } else {
                    // do nothing.
                }
                break;
            case CODE_DOUBLE_QUOTE:
                fp << c;
                if (c == '\"' && last_c != '\\')
                    state = CODE;
                break;
            case CODE_SINGLE_QUOTE:
                fp << c;
                if (c == '\'')
                    state = CODE;
                break;
            case CODE_COMMENT:
                fp << c;
                if (c == '/' && last_c == '*')
                    state = CODE;
                break;
            case CODE_COMMENT2:
                fp << c;
                if (c == '\n')
                    state = CODE;
                break;
            case COMMENT:
                if (last_c == '*' && c == '/')
                    state = RHS;
                break;
            case COMMENT2:
                if (last_c == '\n')
                    state = RHS;
                break;
            default:
                break;
        } // end switch.

        // putc(c, stdout);
        last_last_c = last_c;
        last_c = c;

        n_col++;
        if (c == '\n') {
            n_line++;
            n_col = 1;
        }
    }
}

void
process_yacc_file_section3(std::ifstream& fp_yacc, std::ofstream& fp)
{
    char c = 0;
    while (fp_yacc.get(c)) {
        fp << c;
    }
}

/*
 * This function will return the position of $A
 * to the end of yaccpar.
 * This is for the purpose of inserting code
 * associated with reductions.
 */
void
copy_yaccpar_file_1(std::ofstream& fp, const std::string& filename)
{
    std::ifstream fp_src;
    fp_src.open(filename);
    if (!fp_src.is_open()) {
        throw std::runtime_error(std::string("error: can't open file ") +
                                 filename);
    }
    char c = 0, last_c = 0;
    while (fp_src.get(c)) {
        if (last_c == '$' && c == 'A') {
            break;
        }
        fp << c;
        last_c = c;
    }
    fp.seekp(-1,
             std::ios_base::cur); // write head reverse 1 byte to remove '$'.
    fp_src.close();
}

void
copy_yaccpar_file_2(std::ofstream& fp, const std::string& filename)
{
    std::ifstream fp_src;
    fp_src.open(filename);
    if (!fp_src.is_open()) {
        throw std::runtime_error(std::string("error: can't open file ") +
                                 filename);
    }
    char c = 0, last_c = 0;
    while (fp_src.get(c)) {
        if (last_c == '$' && c == 'A') {
            break;
        }
        fp << c;
        last_c = c;
    }
    while (fp_src.get(c)) {
        fp << c;
    }
    fp_src.close();
}

///////////////////////////////////////////////////////
// Functions to print parsing table arrays. START.
///////////////////////////////////////////////////////

auto
get_index_in_tokens_array(SymbolTblNode* s) -> int
{
    SymbolNode* a = tokens;
    for (int i = 0; a != nullptr; a = a->next, i++) {
        if (s == a->snode)
            return i;
    }
    return -1;
}

/*
 * Returns the index of the given symbol in the
 * non-terminal array of the given grammar.
 * Used in gen_compiler.c.
 */
static auto
get_non_terminal_index(const Grammar& grammar, const SymbolTblNode* snode)
  -> int
{
    const SymbolNode* a = grammar.non_terminal_list;
    for (int i = 0; a != nullptr; a = a->next) {
        if (snode == a->snode)
            return i;
        i++;
    }
    // std::cout << "getNonTerminalIndex warning: ";
    // std::cout  <<  symbol<< " not found!" << std::endl;
    return -1;
}

/*
 * yyr1[i] represents index of non-terminal symbol
 * on the LHS of reduction i, or a terminal symbol
 * if use unit-production-removal and in step 3
 * the LHS are replaced with leaf terminals in the
 * multi-rooted trees.
 */
void
print_yyr1(std::ofstream& fp, const Grammar& grammar)
{
    fp << "static YYCONST yytabelem yyr1[] = {" << std::endl;
    // First rule is always "$accept : ...".
    fp << std::setw(INTEGER_PADDING) << 0 << ',';

    if (Options::get().use_remove_unit_production) {
        int index = 0;
        for (int i = 1; i < grammar.rules.size(); i++) {
            // std::cout << "rule " <<  i<< " lhs: " <<  grammar.rules[i]->LHS
            // << std::endl;
            index = grammar.rules[i]->nLHS->snode->value;
            fp << std::setw(INTEGER_PADDING) << index;
            if (i < grammar.rules.size() - 1)
                fp << ',';
            if ((i - 9) % ITEM_PER_LINE == 0)
                fp << std::endl;
        }
        fp << "};" << std::endl;
        return;
    }

    for (int i = 1; i < grammar.rules.size(); i++) {
        fp << std::setw(INTEGER_PADDING)
           << (-1) *
                get_non_terminal_index(grammar, grammar.rules[i]->nLHS->snode);
        if (i < grammar.rules.size() - 1)
            fp << ',';
        if ((i - (ITEM_PER_LINE - 1)) % ITEM_PER_LINE == 0)
            fp << std::endl;
    }
    fp << "};" << std::endl;
}

/*
 * yyr2[0] is a dummy field, and is alwasy 0.
 * for i >= 1, yyr2[i] defines the following for grammar rule i:
 *   let x be the number of symbols on the RHS of rule i,
 *   let y indicate whether there is any code associated
 *     with this rule (y = 1 for yes, y = 0 for no).
 *   then yyr2[i] = (x << 1) + y;
 */
void
print_yyr2(std::ofstream& fp, const Grammar& grammar)
{
    int i = 0;
    fp << "static YYCONST yytabelem yyr2[] = {" << std::endl;
    fp << std::setw(INTEGER_PADDING) << 0 << ',';
    for (i = 1; i < grammar.rules.size(); i++) {
        fp << std::setw(INTEGER_PADDING)
           << (grammar.rules[i]->RHS_count << 1) +
                static_cast<int>(grammar.rules[i]->hasCode);
        if (i < grammar.rules.size() - 1)
            fp << ',';
        if ((i - 9) % ITEM_PER_LINE == 0)
            fp << std::endl;
    }
    fp << "};" << std::endl;
}

void
print_yynonterminals(std::ofstream& fp, const Grammar& grammar)
{
    SymbolNode* a = nullptr;
    fp << "yytoktype yynts[] = {" << std::endl;

    int i = 1; // ignore first nonterminal: $accept.
    for (a = grammar.non_terminal_list->next; a != nullptr; a = a->next) {
        fp << "\t\"" << a->snode->symbol << "\",\t-" << i << ',' << std::endl;
        i++;
    }
    fp << "\t\"-unknown-\", 1  /* ends search */" << std::endl;
    fp << "};" << std::endl;
}

/*
 * Print terminal tokens.
 */
void
print_yytoks(std::ofstream& fp)
{
    fp << "yytoktype yytoks[] = {" << std::endl;
    for (SymbolNode* a = tokens; a != nullptr; a = a->next) {
        if (*a->snode->symbol == STR_ERROR)
            continue;

        if (a->snode->symbol->size() == 2 &&
            (*a->snode->symbol)[0] == '\\') { // escape sequence
            fp << R"(	"\\)" << a->snode->symbol << R"(",	)"
               << a->snode->value << ',' << std::endl;
        } else {
            fp << "\t\"" << a->snode->symbol << "\",\t" << a->snode->value
               << ',' << std::endl;
        }
    }

    fp << "\t\"-unknown-\", -1  /* ends search */" << std::endl
       << "};" << std::endl;
}

/*
 * Print reductions.
 */
void
print_yyreds(std::ofstream& fp, const Grammar& grammar)
{
    fp << "char * yyreds[] = {" << std::endl;
    fp << "\t\"-no such reduction-\"" << std::endl;
    for (int i = 1; i < grammar.rules.size(); i++) {
        fp << "\t\"" << grammar.rules[i]->nLHS->snode->symbol << " : ";

        const SymbolNode* a = grammar.rules[i]->nRHS_head;
        for (int j = 0; j < grammar.rules[i]->RHS_count; j++) {
            if (j > 0)
                fp << ' ';

            if (j > 0)
                a = a->next;
            const std::string_view symbol = *a->snode->symbol;

            if (symbol.size() == 1 ||
                (symbol.size() == 2 && symbol[0] == '\\')) {
                fp << '\'' << symbol << '\'';
            } else {
                fp << symbol;
            }
        }
        fp << "\", " << std::endl;
    }
    fp << "};" << std::endl;
}

void
print_yytoken(std::ofstream& fp)
{
    const SymbolNode* a = tokens;

    fp << "int yytoken[] = {" << std::endl;
    for (int i = 0; a != nullptr; a = a->next, i++) {
        fp << "\t\"" << a->snode->symbol << "\",\t" << 257 + i << ','
           << std::endl;
    }

    fp << "\t\"-unknown-\", -1  /* ends search */" << std::endl;
    fp << "};" << std::endl;
}

void
print_parsing_tbl_entry(std::ofstream& fp,
                        char action,
                        int state_no,
                        int* count)
{
    bool is_entry = false;
    if (action == 's' || action == 'g') {
        fp << state_no << ", ";
        is_entry = true;
    } else if (action == 'r') {
        fp << '-' << state_no << ", ";
        is_entry = true;
    } else if (action == 'a') {
        fp << "0, ";
        is_entry = true;
    }

    if (is_entry) {
        (*count)++;
        if ((*count) % ITEM_PER_LINE == 0 && (*count) != 0)
            fp << std::endl;
    }
}

/*
 * For the actions in a parsing table.
 * if an action yyptblact[i] is positive, it's a shift/goto;
 * if it is negative, it's a reduce;
 * if it's zero, it's accept.
 */
static void
print_parsing_tbl(std::ofstream& fp, const Grammar& grammar)
{
    int col_size = ParsingTblCols;
    std::vector<int> rowoffset;
    rowoffset.reserve(ParsingTblRows);
    int count = 0;

    fp << "static YYCONST yytabelem yyptblact[] = {" << std::endl;

    if (Options::get().use_remove_unit_production) {
        for (int row = 0; row < ParsingTblRows; row++) {
            if (is_reachable_state(row)) {

                if constexpr (USE_REM_FINAL_STATE) {
                    if (final_state_list[row] < 0) {
                        print_parsing_tbl_entry(
                          fp, 's', final_state_list[row], &count);
                        rowoffset.push_back(count);
                        continue;
                    }
                }
                for (int col = 0; col < ParsingTblCols; col++) {
                    const SymbolTblNode* n = ParsingTblColHdr[col];
                    if (is_goal_symbol(grammar, n) == false &&
                        is_parent_symbol(n) == false) {
                        int state_no = 0;
                        char action = get_action(n->type, col, row, &state_no);

                        if (action == 's' || action == 'g')
                            state_no = get_actual_state(state_no);
                        // std::cout  <<  action <<  state_no<< "\t";
                        print_parsing_tbl_entry(fp, action, state_no, &count);
                    } // end of if.
                }

                rowoffset.push_back(count);
                // std::cout  << std::endl;
            } // end of if.
        }
    } else {
        for (int i = 0; i < ParsingTblRows; i++) {

            if constexpr (USE_REM_FINAL_STATE) {
                if (final_state_list[i] < 0) {
                    print_parsing_tbl_entry(
                      fp, 's', final_state_list[i], &count);
                    rowoffset.push_back(count);
                    continue;
                }
            }

            for (int j = 0; j < ParsingTblCols; j++) {
                int state_no = 0;
                char action =
                  get_action(ParsingTblColHdr[j]->type, j, i, &state_no);
                // std::cout  <<  action <<  state_no<< ", ";
                print_parsing_tbl_entry(fp, action, state_no, &count);
            }

            rowoffset.push_back(count);
            // std::cout  << std::endl;
        } // end of for.
    }

    fp << "-10000000};" << std::endl << std::endl; // -10000000 is space filler

    fp << "static YYCONST yytabelem yyrowoffset[] = {\n0, " << std::endl;
    for (size_t i = 0; i < rowoffset.size(); i++) {
        fp << rowoffset[i];
        if (i < rowoffset.size() - 1)
            fp << ", ";
        if (i % ITEM_PER_LINE == 0 && i != 0)
            fp << std::endl;
    }
    fp << "};" << std::endl
       << std::endl; // NOTE: the last entry is (yyptbl.size - 1).
}

void
print_parsing_tbl_col_entry(std::ofstream& fp,
                            char action,
                            int token_value,
                            int* count)
{
    bool is_entry = false;
    if (action == 's' || action == 'g' || action == 'r' || action == 'a') {
        fp << token_value << ", ";
        is_entry = true;
    }

    if (is_entry) {
        (*count)++;
        if ((*count) % ITEM_PER_LINE == 0 && (*count) != 0)
            fp << std::endl;
    }
}

/*
 * For the tokens upon which action are taken.
 * If it's between 0 - 255, it's an ascii char;
 * if it's 256, it's 'error';
 * if it's > 256, it's a token;
 * if it's < 0, it's a non-terminal.
 */
static void
print_parsing_tbl_col(std::ofstream& fp, const Grammar& grammar)
{
    int count = 0;
    int col_size = ParsingTblCols;

    fp << "static YYCONST yytabelem yyptbltok[] = {" << std::endl;

    if (Options::get().use_remove_unit_production) {
        int i = 0;
        for (int row = 0; row < ParsingTblRows; row++) {
            if (is_reachable_state(row)) {

                if constexpr (USE_REM_FINAL_STATE) {
                    if (final_state_list[row] < 0) {
                        print_parsing_tbl_col_entry(fp, 'r', -10000001, &count);
                        continue;
                    }
                }
                for (int col = 0; col < ParsingTblCols; col++) {
                    SymbolTblNode* n = ParsingTblColHdr[col];
                    if (is_goal_symbol(grammar, n) == false &&
                        is_parent_symbol(n) == false) {
                        int state = 0;
                        char action = get_action(n->type, col, row, &state);
                        print_parsing_tbl_col_entry(
                          fp, action, n->value, &count);
                    } // end of if.
                }     // end of for.
            }         // end of if.
        }
    } else {
        for (int i = 0; i < ParsingTblRows; i++) {

            if constexpr (USE_REM_FINAL_STATE) {
                if (final_state_list[i] < 0) { // is a final state.
                    // -10000001 labels a final state's col entry
                    print_parsing_tbl_col_entry(fp, 'r', -10000001, &count);
                    continue;
                }
            }
            for (int j = 0; j < ParsingTblCols; j++) {
                SymbolTblNode* n = ParsingTblColHdr[j];
                int state = 0;
                char action = get_action(n->type, j, i, &state);
                print_parsing_tbl_col_entry(fp, action, n->value, &count);
            }
        } // end of for.
    }

    fp << "-10000000};" << std::endl << std::endl; // -10000000 is space filler
}

/*
 * Find those states that only have a single reduce action.
 * Refer: Pager July, 72', Tech Rpt PE 259. Measure 3.
 */
void
get_final_states(std::ofstream& fp)
{
    fp << "static YYCONST yytabelem yyfs[] = {" << std::endl;
    fp << final_state_list[0];
    int j = 0;
    for (int i = 1; i < ParsingTblRows; i++) {
        if (Options::get().use_remove_unit_production) {
            if (is_reachable_state(i) == false)
                continue;
        }

        fp << ", ";
        if ((++j) % ITEM_PER_LINE == 0)
            fp << std::endl;
        fp << final_state_list[i];
    }
    fp << "};" << std::endl << std::endl;
}

auto
use_lrk() -> bool
{
    return Options::get().use_lr_k &&
           (lrk_pt_array != nullptr && lrk_pt_array->max_k() >= 2);
}

void
write_lrk_table_arrays(std::ofstream& fp)
{
    fp << std::endl
       << "/* * For LR(k) parsing tables." << std::endl
       << " */" << std::endl;

    // yy_lrk_k.
    fp << std::endl << "/* Max K in LR(k). */" << std::endl;
    fp << "static YYCONST yytabelem yy_lrk_k = " << lrk_pt_array->max_k() << ";"
       << std::endl;

    // yy_lrk_rows[].
    fp << std::endl
       << "/* Number of rows in each LR(k) parsing table. */" << std::endl;
    fp << "static YYCONST yytabelem yy_lrk_rows[] = {";
    for (size_t i = 2; i <= lrk_pt_array->max_k(); i++) {
        // std::cout << "write LRK table arrays: i = " <<  i << std::endl;
        if (i > 2)
            fp << ", ";
        fp << lrk_pt_array->array[i - 2]->row_count;
    }
    fp << "};" << std::endl;

    // yy_lrk_cols
    fp << std::endl << "/* yyPTC_count + 2 */" << std::endl;
    fp << "static YYCONST yytabelem yy_lrk_cols = " << ParsingTblCols + 2 << ';'
       << std::endl;

    // yy_lrk_r[].
    fp << std::endl << "/* Values in each LR(k) parsing table. */" << std::endl;
    fp << "static YYCONST yytabelem yy_lrk_r[] = {" << std::endl;
    size_t k = 2;
    for (const LRkPT* t : lrk_pt_array->array) {
        for (const LRkPTRow* r = t->rows; r != nullptr; r = r->next) {
            fp << "  " << r->state << ", " << r->token->snode->value << ", ";
            for (size_t j = 0; j < ParsingTblCols; j++) {
                if (r->row[j] != nullptr) {
                    if (reinterpret_cast<uintptr_t>(r->row[j]->end) ==
                        CONST_CONFLICT_SYMBOL) {
                        fp << j << ", " << -2 << ", ";
                    } else {
                        fp << j << ", " << r->row[j]->end->ruleID << ", ";
                    }
                }
            }
            if (k == lrk_pt_array->max_k() && r->next == nullptr) {
                fp << "-1";
            } else {
                fp << "-1, ";
            }
            fp << std::endl;
        }
        if (k < lrk_pt_array->max_k())
            fp << std::endl;
        k++;
    }
    fp << "};" << std::endl;

    // CONST_ACC.
    fp << std::endl << "#define CONST_ACC -10000000 ";
    fp << "/* for ACC in parsing table. */" << std::endl;

    // yyPTC[].
    fp << std::endl
       << "/* Values of parsing table column tokens. */" << std::endl;
    fp << "static YYCONST yytabelem yyPTC[] = {" << std::endl;
    for (int i = 0; i < ParsingTblCols; i++) {
        if (i > 0)
            fp << ", ";
        if (i % ITEM_PER_LINE == 0) {
            if (i > 0)
                fp << std::endl;
            fp << "  ";
        }
        if (*ParsingTblColHdr[i]->symbol == "$accept") {
            fp << "CONST_ACC";
        } else if (*ParsingTblColHdr[i]->symbol == "$end") {
            fp << 0;
        } else {
            fp << ParsingTblColHdr[i]->value;
        }
    }
    fp << std::endl << "};" << std::endl;
    fp << std::endl << std::endl;
}

/*
 * write the generated parsing table into the arrays
 * used by the driver code.
 */
static void
write_parsing_table_arrays(std::ofstream& fp, const Grammar& grammar)
{
    get_final_states(fp);

    print_parsing_tbl_col(fp, grammar); // yytbltok[]
    print_parsing_tbl(fp, grammar);     // yytblact[], yyrowoffset[]

    print_yyr1(fp, grammar); // yyr1[]
    print_yyr2(fp, grammar); // yyr2[]

    if (use_lrk() == false) {
        fp << std::endl << "#ifdef YYDEBUG" << std::endl << std::endl;
        fp << "typedef struct {char *t_name; int t_val;} yytoktype;"
           << std::endl
           << std::endl;
        print_yynonterminals(fp, grammar); // yynts[]. nonterminals.

        print_yytoks(fp);          // yytoks[]. tokens.
        print_yyreds(fp, grammar); // yyreds[]. Productions of grammar.
        fp << "#endif /* YYDEBUG */" << std::endl << std::endl;

    } else { // use LR(k).
        fp << "typedef struct {char *t_name; int t_val;} yytoktype;"
           << std::endl
           << std::endl;
        print_yynonterminals(fp, grammar); // yynts[]. nonterminals.
        print_yytoks(fp);                  // yytoks[]. tokens.

        fp << std::endl << "#ifdef YYDEBUG" << std::endl << std::endl;
        print_yyreds(fp, grammar); // yyreds[]. Productions of grammar.
        fp << "#endif /* YYDEBUG */" << std::endl << std::endl;

        write_lrk_table_arrays(fp);
    }
}

///////////////////////////////////////////////////////
// Functions to print parsing table arrays. END.
///////////////////////////////////////////////////////

void
write_special_info(std::ofstream& fp)
{
    fp << std::endl << "YYSTYPE yylval;" << std::endl;
    if (Options::get().use_yydebug) {
        fp << std::endl << "#define YYDEBUG 1" << std::endl;
    }
}

/*
 * Do this is use LR(k).
 */
void
get_lrk_hyacc_path()
{
    if (use_lrk()) {
        std::cout << "lrk used" << std::endl;
        HYACC_PATH += 'k';
        std::cout << "LR(k) HYACC_PATH: " << HYACC_PATH << std::endl;
    }
}

void
generate_compiler(GetYaccGrammarOutput& yacc_grammar_output,
                  const std::string& infile,
                  const FileNames& files)
{
    // count number of lines in yacc input file.
    int n_line = 0;
    int n_col = 0;
    auto& options = Options::get();
    std::ifstream fp_yacc{};
    std::ofstream fp{};
    std::ofstream fp_h{};

    fp_yacc.open(infile);
    if (!fp_yacc.is_open()) {
        throw std::runtime_error(std::string("error: can't open file ") +
                                 infile);
    }

    prepare_outfile(fp, fp_h, files); // open output compiler file.

    if (options.use_lines)
        fp << std::endl << "# line 1 \"" << infile << '\"' << std::endl;
    process_yacc_file_section1(
      fp_yacc, fp, fp_h, n_line, n_col); // declaration section.

    write_special_info(fp);

    goto_section3(fp_yacc, n_line);

    if (options.use_lines)
        fp << std::endl
           << "# line " << n_line << " \"" << infile << '\"' << std::endl;

    process_yacc_file_section3(fp_yacc, fp); // code section.

    fp << std::endl << "#define YYCONST const" << std::endl;
    fp << "typedef int yytabelem;" << std::endl << std::endl;
    write_parsing_table_arrays(fp, yacc_grammar_output.grammar);

    get_lrk_hyacc_path(); /* do this if LR(k) is used */

    copy_yaccpar_file_1(fp, HYACC_PATH);
    goto_section2(fp_yacc, n_line);
    process_yacc_file_section2(yacc_grammar_output,
                               fp_yacc,
                               fp,
                               infile,
                               n_line,
                               n_col); // get reduction code.
    copy_yaccpar_file_2(fp, HYACC_PATH);

    free_symbol_node_list(tokens);

    fp.close();
    if (options.use_header_file)
        fp_h.close();
    fp_yacc.close();
}
