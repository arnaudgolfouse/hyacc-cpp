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
#include <cstdio>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

extern const char* y_tab_c;
extern const char* y_tab_h;

static FILE* fp_yacc; // for yacc input file.
static FILE* fp;      // pointer to output file y.tab.c
static FILE* fp_h;    // for y.tab.h

static int n_line; // count number of lines in yacc input file.
static int n_col;

constexpr size_t MAX_RULE_LENGTH = 0xfffff;

char* yystype_definition = (char*)"typedef int YYSTYPE;";
constexpr const char* YYSTYPE_FORMAT =
  "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED\n"
  "%s\n"
  "#define YYSTYPE_IS_DECLARED 1\n"
  "#endif\n";

extern void
add_char_to_symbol(char c);
extern char* ysymbol; // token symbol.
extern int ysymbol_pt;
extern int ysymbol_size;

static void
prepare_outfile()
{
    if ((fp = fopen(y_tab_c, "w")) == nullptr) {
        throw std::runtime_error(std::string("Cannot open output file ") +
                                 y_tab_c);
    }
    if (USE_HEADER_FILE == false)
        return;
    if ((fp_h = fopen(y_tab_h, "w")) == nullptr) {
        fclose(fp);
        throw std::runtime_error(std::string("Cannot open output file ") +
                                 y_tab_h);
    }
}

static void
my_perror(const char* msg, int c)
{
    using std::to_string;
    throw std::runtime_error(std::string("\nerror [line ") + to_string(n_line) +
                             ", col " + to_string(n_col) + "]: invalid char '" +
                             to_string(c) + "'. " + msg);
}

inline void
print_break()
{
    fprintf(fp, "break;\n");
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
write_tokens_to_compiler_file()
{

    fprintf(fp, "\n/* tokens */\n\n");
    if (USE_HEADER_FILE)
        fprintf(fp_h, "\n/* tokens */\n\n");

    int index = 0;
    int i = 0;
    for (SymbolNode* a = tokens; a != nullptr; a = a->next, i++) {
        if (a->snode->TP->is_quoted || strcmp(a->snode->symbol, strError) == 0)
            continue;

        fprintf(fp, "#define %s %d\n", a->snode->symbol, index + 257);
        if (USE_HEADER_FILE)
            fprintf(fp_h, "#define %s %d\n", a->snode->symbol, index + 257);
        index++;
    }

    fprintf(fp, YYSTYPE_FORMAT, yystype_definition);
    if (USE_HEADER_FILE) {
        fprintf(fp_h, YYSTYPE_FORMAT, yystype_definition);
        fprintf(fp_h, "\nextern YYSTYPE yylval;\n");
    }
}

/*
 *  Get code declarations from section 1, write to
 *  y.tab.c, and write token declarations too.
 */
void
process_yacc_file_section1()
{
    n_line = n_col = 1;

    bool is_code = false;
    char c = 0, last_c = '\n', last_last_c = 0;
    while ((c = static_cast<char>(getc(fp_yacc))) != EOF) {

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
            putc(c, fp); // write to output file.
        }

        last_last_c = last_c;
        last_c = c;

        n_col++;
        if (c == '\n') {
            n_line++;
            n_col = 1;
        }
    }

    // writeTokens();
    write_tokens_to_compiler_file();
}

/*
 * rewind to section 2.
 */
static void
goto_section2()
{
    char c = 0, last_c = 0, last_last_c = '\n';

    fseek(fp_yacc, 0L, 0); // go to the beginning of file fp.
    n_line = 1;
    while ((c = static_cast<char>(getc(fp_yacc))) != EOF) {
        if (c == '\n')
            n_line++;
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
goto_section3()
{
    char c = 0, last_c = 0, last_last_c = '\n';

    while ((c = static_cast<char>(getc(fp_yacc))) != EOF) {
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
find_full_rule(int rule_count) -> Production*
{
    Production* rule = nullptr;
    SymbolNode* node = nullptr;
    SymbolTblNode* sym = nullptr;

    for (int full_rule = rule_count; full_rule < grammar.rules.size();
         ++full_rule) {

        if ((rule = grammar.rules[full_rule]) && (node = rule->nLHS) &&
            (sym = node->snode)) {
            // TODO : is this really `!= 0` ?
            // was simply `strncmp(...)` before.
            if (strncmp("$$", sym->symbol, 2) !=
                0) /* node symbol starting with $$ we continue */
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
find_mid_prod_index(Production* rule, Production* mid_prod_rule) -> int
{
    SymbolNode* lnode = mid_prod_rule->nLHS;
    SymbolTblNode* lsym = lnode->snode;
    char* l = lsym->symbol;
    SymbolNode* rnode = rule->nRHS_head;
    SymbolTblNode* rsym = nullptr;
    char* r = nullptr;

    for (int i = 0; rnode; ++i, rnode = rnode->next)
        if (!((rsym = rnode->snode) && (r = rsym->symbol))) {
            throw std::runtime_error(
              std::string("Did not find mid production rule of ") + l);
        } else if (strcmp(l, r) == 0)
            return i;
    return -1;
}

static auto
find_sym(Production* rule, int dollar_number) -> SymbolTblNode*
{
    SymbolTblNode* sym = nullptr;
    if (dollar_number == MAX_RULE_LENGTH)
        return sym;

    int i = 1;
    SymbolNode* node = rule->nRHS_head;
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
process_yacc_file_section2(char* filename)
{
    YACC_STATE state = LHS;
    int code_level;
    bool reading_symbol = false;
    bool reading_number = false;
    bool reading_type = false;
    int dollar_number = 0;
    char c = 0, last_c = 0, last_last_c = 0;
    int rule_count = 0;
    bool end_of_code = false; // for mid-production action.
    char* explicit_type = 0;
    Production* rule;
    SymbolTblNode* sym;
    static const char* padding = "        ";

    while ((c = static_cast<char>(getc(fp_yacc))) != EOF) {
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
                    my_perror("error: state COLON. ", c);
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
                        print_break();
                        end_of_code = false;
                    }

                } else if (c == ';') {
                    state = LHS; // end of a rule.
                    if (end_of_code) {
                        print_break();
                    }
                    end_of_code = false;
                } else if (c == '|') { // end of a rule
                    rule_count++;
                    if (end_of_code) {
                        print_break();
                    }
                    end_of_code = false;
                } else if (c == '{') { // code for rule #rule_count
                    state = CODE;
                    code_level = 1;

                    if (end_of_code == false) {
                        fprintf(fp, "\n%s  case %d:\n", padding, rule_count);
                        if (USE_LINES)
                            fprintf(fp, "# line %d \"%s\"\n", n_line, filename);
                    }
                    putc('{', fp);

                } else if (last_c == '/' && c == '*') {
                    state = COMMENT;
                } else if (last_c == '/' && c == '/') {
                    state = COMMENT2;
                } else if (c == '/') {
                    // do nothing
                } else if (c == ':') {
                    my_perror("You may miss a ';' in the last rule.", c);
                } else {
                    // reading a symbol. do nothing
                    if (end_of_code) {
                        rule_count++;
                        print_break();
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
                        add_char_to_symbol('\0');
                        explicit_type = ysymbol;
                    } else {
                        add_char_to_symbol(c);
                    }
                } else if (last_c == '$' && c == '<') {
                    ysymbol_pt = 0;
                    reading_type = true;
                } else if (last_c != '$' && c == '$') {
                    // do nothing, this may be a special character.
                } else if (last_c == '$' && c == '$') {
                    const char* token_type = nullptr;
                    if (explicit_type) {
                        token_type = explicit_type;
                        explicit_type = nullptr;
                    } else {
                        rule = find_full_rule(rule_count);
                        token_type = rule->nLHS->snode->token_type;
                    }
                    if (token_type)
                        fprintf(fp, "(yyval.%s)", token_type);
                    else
                        fprintf(fp, "yyval");

                } else if (last_c == '$' && isdigit(c)) {
                    reading_number = true;
                    dollar_number = (c - 48) + 10 * dollar_number;
                } else if (reading_number && isdigit(c)) {
                    dollar_number = (c - 48) + 10 * dollar_number;
                } else if (reading_number && !isdigit(c)) {
                    Production* start_rule = grammar.rules[rule_count];
                    int RHS_index;

                    rule = find_full_rule(rule_count);
                    if (rule != start_rule)
                        RHS_index = find_mid_prod_index(rule, start_rule);
                    else
                        RHS_index = rule->RHS_count;
                    const char* token_type = nullptr;
                    if (explicit_type) {
                        token_type = explicit_type;
                        explicit_type = 0;
                    } else {
                        sym = find_sym(rule, dollar_number);
                        token_type = sym->token_type;
                    }
                    if (token_type)
                        fprintf(fp,
                                "(yypvt[%d].%s)/* %d %d */",
                                (dollar_number - RHS_index),
                                token_type,
                                rule_count,
                                RHS_index);
                    else
                        fprintf(fp,
                                "yypvt[%d]/* %d %d */",
                                dollar_number - RHS_index,
                                rule_count,
                                RHS_index);

                    putc(c, fp);
                    reading_number = false;
                    dollar_number = 0;
                } else {
                    putc(c, fp);
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
                    fprintf(fp, " "); // print_break();
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
                putc(c, fp);
                if (c == '\"' && last_c != '\\')
                    state = CODE;
                break;
            case CODE_SINGLE_QUOTE:
                putc(c, fp);
                if (c == '\'')
                    state = CODE;
                break;
            case CODE_COMMENT:
                putc(c, fp);
                if (c == '/' && last_c == '*')
                    state = CODE;
                break;
            case CODE_COMMENT2:
                putc(c, fp);
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
process_yacc_file_section3()
{
    char c = 0;
    while ((c = static_cast<char>(getc(fp_yacc))) != EOF) {
        putc(c, fp);
    }
}

/*
 * Copy code from resource files into output file.
 */
void
copy_src_file(char* filename)
{
    std::ifstream fp_src;
    fp_src.open(filename);
    if (!fp_src.is_open()) {
        throw std::runtime_error(std::string("error: can't open file ") +
                                 filename);
    }
    char c = 0;
    while (fp_src.get(c)) {
        putc(c, fp);
    }
    fp_src.close();
}

/*
 * This function will return the position of $A
 * to the end of yaccpar.
 * This is for the purpose of inserting code
 * associated with reductions.
 */
void
copy_yaccpar_file_1(const char* filename)
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
        putc(c, fp);
        last_c = c;
    }
    fseek(fp, -1L, SEEK_CUR); // write head reverse 1 byte to remove '$'.
    fp_src.close();
}

void
copy_yaccpar_file_2(const char* filename)
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
        putc(c, fp);
        last_c = c;
    }
    while (fp_src.get(c)) {
        putc(c, fp);
    }
    fp_src.close();
}

///////////////////////////////////////////////////////
// Functions to print parsing table arrays. START.
///////////////////////////////////////////////////////

static const char* const INDENT = "    "; // indentation.

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
auto
get_non_terminal_index(SymbolTblNode* snode) -> int
{
    SymbolNode* a = grammar.non_terminal_list;
    for (int i = 0; a != nullptr; a = a->next) {
        if (snode == a->snode)
            return i;
        i++;
    }
    // printf("getNonTerminalIndex warning: ");
    // printf("%s not found!\n", symbol);
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
print_yyr1()
{
    fprintf(fp, "static YYCONST yytabelem yyr1[] = {\n");
    // First rule is always "$accept : ...".
    fprintf(fp, "%6d,", 0);

    if (USE_REMOVE_UNIT_PRODUCTION) {
        int index = 0;
        for (int i = 1; i < grammar.rules.size(); i++) {
            // printf("rule %d lhs: %s\n", i, grammar.rules[i]->LHS);
            index = grammar.rules[i]->nLHS->snode->value;
            fprintf(fp, "%6d", index);
            if (i < grammar.rules.size() - 1)
                fprintf(fp, ",");
            if ((i - 9) % 10 == 0)
                fprintf(fp, "\n");
        }
        fprintf(fp, "};\n");
        return;
    }

    for (int i = 1; i < grammar.rules.size(); i++) {
        fprintf(fp,
                "%6d",
                (-1) * get_non_terminal_index(grammar.rules[i]->nLHS->snode));
        if (i < grammar.rules.size() - 1)
            fprintf(fp, ",");
        if ((i - 9) % 10 == 0)
            fprintf(fp, "\n");
    }

    fprintf(fp, "};\n");
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
print_yyr2()
{
    int i = 0;
    fprintf(fp, "static YYCONST yytabelem yyr2[] = {\n");
    fprintf(fp, "%6d,", 0);
    for (i = 1; i < grammar.rules.size(); i++) {
        fprintf(fp,
                "%6d",
                (grammar.rules[i]->RHS_count << 1) + grammar.rules[i]->hasCode);
        if (i < grammar.rules.size() - 1)
            fprintf(fp, ",");
        if ((i - 9) % 10 == 0)
            fprintf(fp, "\n");
    }
    fprintf(fp, "};\n");
}

void
print_yynonterminals()
{
    SymbolNode* a = nullptr;
    fprintf(fp, "yytoktype yynts[] = {\n");

    int i = 1; // ignore first nonterminal: $accept.
    for (a = grammar.non_terminal_list->next; a != nullptr; a = a->next) {
        fprintf(fp, "\t\"%s\",\t-%d,\n", a->snode->symbol, i);
        i++;
    }
    fprintf(fp, "\t\"-unknown-\", 1  /* ends search */\n");
    fprintf(fp, "};\n");
}

/*
 * Print terminal tokens.
 */
void
print_yytoks()
{
    SymbolNode* a = tokens;
    int index = 0;

    fprintf(fp, "yytoktype yytoks[] = {\n");
    for (int i = 0; a != nullptr; a = a->next, i++) {
        if (strcmp(a->snode->symbol, strError) == 0)
            continue;

        if (strlen(a->snode->symbol) == 2 &&
            a->snode->symbol[0] == '\\') { // escape sequence
            fprintf(
              fp, "\t\"\\\\%s\",\t%d,\n", a->snode->symbol, a->snode->value);
        } else {
            fprintf(fp, "\t\"%s\",\t%d,\n", a->snode->symbol, a->snode->value);
        }
    }

    fprintf(fp, "\t\"-unknown-\", -1  /* ends search */\n");
    fprintf(fp, "};\n");
}

/*
 * Print reductions.
 */
void
print_yyreds()
{
    fprintf(fp, "char * yyreds[] = {\n");
    fprintf(fp, "\t\"-no such reduction-\"\n");
    for (int i = 1; i < grammar.rules.size(); i++) {
        fprintf(fp, "\t\"%s : ", grammar.rules[i]->nLHS->snode->symbol);

        const SymbolNode* a = grammar.rules[i]->nRHS_head;
        for (int j = 0; j < grammar.rules[i]->RHS_count; j++) {
            if (j > 0)
                fprintf(fp, " ");

            if (j > 0)
                a = a->next;
            const char* symbol = a->snode->symbol;

            if (strlen(symbol) == 1 ||
                (strlen(symbol) == 2 && symbol[0] == '\\')) {
                fprintf(fp, "'%s'", symbol);
            } else {
                fprintf(fp, "%s", symbol);
            }
        }
        fprintf(fp, "\", \n");
    }
    fprintf(fp, "};\n");
}

void
print_yytoken()
{
    const SymbolNode* a = tokens;

    fprintf(fp, "int yytoken[] = {\n");
    for (int i = 0; a != nullptr; a = a->next, i++) {
        fprintf(fp, "\t\"%s\",\t%d,\n", a->snode->symbol, 257 + i);
    }

    fprintf(fp, "\t\"-unknown-\", -1  /* ends search */\n");
    fprintf(fp, "};\n");
}

void
print_parsing_tbl_entry(char action, int state_no, int* count)
{
    bool is_entry = false;
    if (action == 's' || action == 'g') {
        fprintf(fp, "%d, ", state_no); // state to go.
        is_entry = true;
    } else if (action == 'r') {
        fprintf(fp, "-%d, ", state_no); // no. of reduction.
        is_entry = true;
    } else if (action == 'a') {
        fprintf(fp, "0, ");
        is_entry = true;
    }

    if (is_entry) {
        (*count)++;
        if ((*count) % 10 == 0 && (*count) != 0)
            fprintf(fp, "\n");
    }
}

/*
 * For the actions in a parsing table.
 * if an action yyptblact[i] is positive, it's a shift/goto;
 * if it is negative, it's a reduce;
 * if it's zero, it's accept.
 */
void
print_parsing_tbl()
{
    int col_size = ParsingTblCols;
    int* rowoffset = new int[ParsingTblRows];
    int rowoffset_pt = 0;
    int count = 0;

    fprintf(fp, "static YYCONST yytabelem yyptblact[] = {\n");

    if (USE_REMOVE_UNIT_PRODUCTION) {
        int i = 0;
        for (int row = 0; row < ParsingTblRows; row++) {
            if (is_reachable_state(row)) {

#if USE_REM_FINAL_STATE
                if (final_state_list[row] < 0) {
                    print_parsing_tbl_entry('s', final_state_list[row], &count);
                    *(rowoffset + rowoffset_pt) = count;
                    rowoffset_pt++;
                    continue;
                }
#endif
                for (int col = 0; col < ParsingTblCols; col++) {
                    const SymbolTblNode* n = ParsingTblColHdr[col];
                    if (is_goal_symbol(n) == false &&
                        is_parent_symbol(n) == false) {
                        char action = 0;
                        int state_no = 0;
                        get_action(n->type, col, row, &action, &state_no);

                        if (action == 's' || action == 'g')
                            state_no = get_actual_state(state_no);
                        // printf("%c%d\t", action, state_no);
                        print_parsing_tbl_entry(action, state_no, &count);
                    } // end of if.
                }

                *(rowoffset + rowoffset_pt) = count;
                rowoffset_pt++;
                // printf("\n");
            } // end of if.
        }
    } else {
        for (int i = 0; i < ParsingTblRows; i++) {

#if USE_REM_FINAL_STATE
            if (final_state_list[i] < 0) {
                print_parsing_tbl_entry('s', final_state_list[i], &count);
                *(rowoffset + rowoffset_pt) = count;
                rowoffset_pt++;
                continue;
            }
#endif

            for (int j = 0; j < ParsingTblCols; j++) {
                char action = 0;
                int state_no = 0;
                get_action(ParsingTblColHdr[j]->type, j, i, &action, &state_no);
                // printf("%c%d, ", action, state_no);
                print_parsing_tbl_entry(action, state_no, &count);
            }

            *(rowoffset + rowoffset_pt) = count;
            rowoffset_pt++;
            // printf("\n");
        } // end of for.
    }

    fprintf(fp, "-10000000};\n\n"); // -10000000 is space filler

    fprintf(fp, "static YYCONST yytabelem yyrowoffset[] = {\n0, ");
    for (int i = 0; i < rowoffset_pt; i++) {
        fprintf(fp, "%d", *(rowoffset + i));
        if (i < rowoffset_pt - 1)
            fprintf(fp, ", ");
        if (i % 10 == 0 && i != 0)
            fprintf(fp, "\n");
    }
    fprintf(fp, "};\n\n"); // NOTE: the last entry is (yyptbl.size - 1).
}

void
print_parsing_tbl_col_entry(char action, int token_value, int* count)
{
    bool isEntry = false;
    if (action == 's' || action == 'g' || action == 'r' || action == 'a') {
        fprintf(fp, "%d, ", token_value); // state to go.
        isEntry = true;
    }

    if (isEntry) {
        (*count)++;
        if ((*count) % 10 == 0 && (*count) != 0)
            fprintf(fp, "\n");
    }
}

/*
 * For the tokens upon which action are taken.
 * If it's between 0 - 255, it's an ascii char;
 * if it's 256, it's 'error';
 * if it's > 256, it's a token;
 * if it's < 0, it's a non-terminal.
 */
void
print_parsing_tbl_col()
{
    int i, j, row, col, count = 0;
    int col_size = ParsingTblCols;
    char action;
    int state;
    SymbolTblNode* n;

    fprintf(fp, "static YYCONST yytabelem yyptbltok[] = {\n");

    if (USE_REMOVE_UNIT_PRODUCTION) {
        i = 0;
        for (row = 0; row < ParsingTblRows; row++) {
            if (is_reachable_state(row)) {

#if USE_REM_FINAL_STATE
                if (final_state_list[row] < 0) {
                    print_parsing_tbl_col_entry('r', -10000001, &count);
                    continue;
                }
#endif
                for (col = 0; col < ParsingTblCols; col++) {
                    n = ParsingTblColHdr[col];
                    if (is_goal_symbol(n) == false &&
                        is_parent_symbol(n) == false) {
                        get_action(n->type, col, row, &action, &state);
                        print_parsing_tbl_col_entry(action, n->value, &count);
                    } // end of if.
                }     // end of for.
            }         // end of if.
        }
    } else {
        for (i = 0; i < ParsingTblRows; i++) {

#if USE_REM_FINAL_STATE
            if (final_state_list[i] < 0) { // is a final state.
                // -10000001 labels a final state's col entry
                print_parsing_tbl_col_entry('r', -10000001, &count);
                continue;
            }
#endif
            for (j = 0; j < ParsingTblCols; j++) {
                n = ParsingTblColHdr[j];
                get_action(n->type, j, i, &action, &state);
                print_parsing_tbl_col_entry(action, n->value, &count);
            }
        } // end of for.
    }

    fprintf(fp, "-10000000};\n\n"); // -10000000 is space filler
}

/*
 * Find those states that only have a single reduce action.
 * Refer: Pager July, 72', Tech Rpt PE 259. Measure 3.
 */
void
get_final_states()
{
    fprintf(fp, "static YYCONST yytabelem yyfs[] = {\n");
    fprintf(fp, "%d", final_state_list[0]);
    int j = 0;
    for (int i = 1; i < ParsingTblRows; i++) {
        if (USE_REMOVE_UNIT_PRODUCTION) {
            if (is_reachable_state(i) == false)
                continue;
        }

        fprintf(fp, ", ");
        if ((++j) % 10 == 0)
            fprintf(fp, "\n");
        fprintf(fp, "%d", final_state_list[i]);
    }
    fprintf(fp, "};\n\n");
}

auto
use_lrk() -> bool
{
    return USE_LR_K && (lrk_pt_array != nullptr && lrk_pt_array->max_k >= 2);
}

void
write_lrk_table_arrays()
{
    fprintf(fp, "/*\n * For LR(k) parsing tables.\n */\n");

    // yy_lrk_k.
    fprintf(fp, "\n/* Max K in LR(k). */\n");
    fprintf(
      fp, "static YYCONST yytabelem yy_lrk_k = %d;\n", lrk_pt_array->max_k);

    // yy_lrk_rows[].
    fprintf(fp, "\n/* Number of rows in each LR(k) parsing table. */\n");
    fprintf(fp, "static YYCONST yytabelem yy_lrk_rows[] = {");
    for (int i = 2; i <= lrk_pt_array->max_k; i++) {
        // printf("write LRK table arrays: i = %d\n", i);
        if (i > 2)
            fprintf(fp, ", ");
        fprintf(fp, "%d", lrk_pt_array->array[i - 2]->row_count);
    }
    fprintf(fp, "};\n");

    // yy_lrk_cols
    fprintf(fp, "\n/* yyPTC_count + 2 */\n");
    fprintf(
      fp, "static YYCONST yytabelem yy_lrk_cols = %d;\n", ParsingTblCols + 2);

    // yy_lrk_r[].
    fprintf(fp, "\n/* Values in each LR(k) parsing table. */\n");
    fprintf(fp, "static YYCONST yytabelem yy_lrk_r[] = {\n");
    for (int i = 2; i <= lrk_pt_array->max_k; i++) {
        const LRkPT* t = lrk_pt_array->array[i - 2];
        for (const LRkPTRow* r = t->rows; r != nullptr; r = r->next) {
            fprintf(fp, "  %d, %d, ", r->state, r->token->snode->value);
            for (int j = 0; j < ParsingTblCols; j++) {
                if (r->row[j] != nullptr) {
                    if (r->row[j]->end ==
                        (Configuration*)CONST_CONFLICT_SYMBOL) {
                        fprintf(fp, "%d, %d, ", j, -2);
                    } else {
                        fprintf(fp, "%d, %d, ", j, r->row[j]->end->ruleID);
                    }
                }
            }
            if (i == lrk_pt_array->max_k && r->next == nullptr) {
                fprintf(fp, "-1");
            } else {
                fprintf(fp, "-1, ");
            }
            fprintf(fp, "\n");
        }
        if (i < lrk_pt_array->max_k)
            fprintf(fp, "\n");
    }
    fprintf(fp, "};\n");

    // CONST_ACC.
    fprintf(fp, "\n#define CONST_ACC -10000000 ");
    fprintf(fp, "/* for ACC in parsing table. */\n");

    // yyPTC[].
    fprintf(fp, "\n/* Values of parsing table column tokens. */\n");
    fprintf(fp, "static YYCONST yytabelem yyPTC[] = {\n");
    for (int i = 0; i < ParsingTblCols; i++) {
        if (i > 0)
            fprintf(fp, ", ");
        if (i % 10 == 0) {
            if (i > 0)
                fprintf(fp, "\n");
            fprintf(fp, "  ");
        }
        if (strcmp("$accept", ParsingTblColHdr[i]->symbol) == 0) {
            fprintf(fp, "CONST_ACC");
        } else if (strcmp("$end", ParsingTblColHdr[i]->symbol) == 0) {
            fprintf(fp, "%d", 0);
        } else {
            fprintf(fp, "%d", ParsingTblColHdr[i]->value);
        }
    }
    fprintf(fp, "\n};\n");

    fprintf(fp, "\n\n");
}

/*
 * write the generated parsing table into the arrays
 * used by the driver code.
 */
void
write_parsing_table_arrays()
{
    get_final_states();

    print_parsing_tbl_col(); // yytbltok[]
    print_parsing_tbl();     // yytblact[], yyrowoffset[]

    print_yyr1(); // yyr1[]
    print_yyr2(); // yyr2[]

    if (use_lrk() == false) {
        fprintf(fp, "\n#ifdef YYDEBUG\n\n");
        fprintf(fp, "typedef struct {char *t_name; int t_val;} yytoktype;\n\n");
        print_yynonterminals(); // yynts[]. nonterminals.

        print_yytoks(); // yytoks[]. tokens.
        print_yyreds(); // yyreds[]. Productions of grammar.
        fprintf(fp, "#endif /* YYDEBUG */\n\n");

    } else { // use LR(k).
        fprintf(fp, "typedef struct {char *t_name; int t_val;} yytoktype;\n\n");
        print_yynonterminals(); // yynts[]. nonterminals.
        print_yytoks();         // yytoks[]. tokens.

        fprintf(fp, "\n#ifdef YYDEBUG\n\n");
        print_yyreds(); // yyreds[]. Productions of grammar.
        fprintf(fp, "#endif /* YYDEBUG */\n\n");

        write_lrk_table_arrays();
    }
}

///////////////////////////////////////////////////////
// Functions to print parsing table arrays. END.
///////////////////////////////////////////////////////

void
write_special_info()
{
    fprintf(fp, "\nYYSTYPE yylval;\n");
    if (USE_YYDEBUG) {
        fprintf(fp, "\n#define YYDEBUG 1\n");
    }
}

/*
 * Do this is use LR(k).
 */
void
get_lrk_hyacc_path()
{
    if (use_lrk()) {
        puts("lrk used");
        auto* tmp = new char[strlen(HYACC_PATH) + 2];
        strcpy(tmp, HYACC_PATH);
        strcat(tmp, "k"); // hyaccpark
        HYACC_PATH = tmp;

        printf("LR(k) HYACC_PATH: %s\n", HYACC_PATH);
    }
}

void
generate_compiler(char* infile)
{
    if ((fp_yacc = fopen(infile, "r")) == nullptr) {
        throw std::runtime_error(std::string("error: can't open file ") +
                                 infile);
    }

    prepare_outfile(); // open output compiler file.

    if (USE_LINES)
        fprintf(fp, "\n# line 1 \"%s\"\n", infile);
    process_yacc_file_section1(); // declaration section.

    write_special_info();

    goto_section3();

    if (USE_LINES)
        fprintf(fp, "\n# line %d \"%s\"\n", n_line, infile);

    process_yacc_file_section3(); // code section.

    fprintf(fp, "\n#define YYCONST const\n");
    fprintf(fp, "typedef int yytabelem;\n\n");
    write_parsing_table_arrays();

    get_lrk_hyacc_path(); /* do this if LR(k) is used */

    copy_yaccpar_file_1(HYACC_PATH);
    goto_section2();
    process_yacc_file_section2(infile); // get reduction code.
    copy_yaccpar_file_2(HYACC_PATH);

    free_symbol_node_list(tokens);

    fclose(fp);
    if (USE_HEADER_FILE)
        fclose(fp_h);
    fclose(fp_yacc);
}
