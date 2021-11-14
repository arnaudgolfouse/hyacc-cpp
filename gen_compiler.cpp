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
 * gen_compiler.cpp
 *
 * Contains functions to generate a compiler.
 *
 * @Author: Xin Chen
 * @Date started: 10/16/2006
 * @Last modified: 3/21/2007
 */

#include "lane_tracing.hpp"
#include "y.hpp"
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
constexpr size_t ITEM_PER_LINE = 10;
/// Index at which the `#define <TOKEN> <NUMBER>` begins.
constexpr uint32_t TOKEN_START_INDEX = 257;
/// Used when converting a string to a base ten number.
constexpr int BASE_TEN = 10;

struct GenCompiler
{
    std::ifstream fp_yacc{};
    std::ofstream fp{};
    std::ofstream fp_h{};
    const Options& options;
    const std::string yystype_definition;

    explicit GenCompiler(const Options& options,
                         const std::string yystype_definition)
      : options(options)
      , yystype_definition(std::move(yystype_definition))
    {}

    void prepare_outfile(const FileNames& files, const bool use_header_file);
    /// Get code declarations from section 1, write to `fp`, and write token
    /// declarations too.
    [[nodiscard]] auto process_yacc_file_section1(const SymbolList& tokens)
      -> Position;
    ///  Write all terminal tokens that are not quoted, and not "error".
    void write_tokens_to_compiler_file(const SymbolList& tokens);
    void process_yacc_file_section3();
    /// Basically, this has the same structure as function
    ///   processYaccFileInput_section2(int c)
    /// in parsetYaccInput.c.
    ///
    /// The purpose here is to extract the code for semantic
    /// actions of rules.
    void process_yacc_file_section2(YSymbol& y_symbol,
                                    const Grammar& grammar,
                                    const std::string_view filename,
                                    Position position);
    static void my_perror(const std::string_view msg,
                          const char c,
                          const Position position)
    {
        using std::to_string;
        throw std::runtime_error(std::string("\nerror [line ") +
                                 to_string(position.line) + ", col " +
                                 to_string(position.col) + "]: invalid char '" +
                                 to_string(c) + "'. " + std::string(msg));
    }

    /// Write the generated parsing table into the arrays
    /// used by the driver code.
    void write_parsing_table_arrays(
      const std::optional<LRkPTArray>& lrk_pt_array,
      const YAlgorithm& y_algorithm);

  private:
    /// Prints `"break;"` into `this->fp`.
    inline void print_break() { this->fp << "break;" << std::endl; }
    static auto find_full_rule(const Grammar& grammar, int rule_count)
      -> Production*;
    static auto find_mid_prod_index(const Production& rule,
                                    const Production& mid_prod_rule)
      -> std::optional<size_t>;
    static auto find_sym(const Production* rule, uint32_t dollar_number)
      -> std::shared_ptr<const SymbolTableNode>;
    /// Find those states that only have a single reduce action.
    /// Refer: Pager July, 72', Tech Rpt PE 259. Measure 3.
    void get_final_states();
    /// For the tokens upon which action are taken.
    /// If it's between 0 - 255, it's an ascii char;
    /// if it's 256, it's 'error';
    /// if it's > 256, it's a token;
    /// if it's < 0, it's a non-terminal.
    void print_parsing_tbl_col(const YAlgorithm& y_algorithm);
    static void print_parsing_tbl_col_entry(std::ofstream& fp,
                                            const std::optional<Action> action,
                                            const int token_value,
                                            uint32_t& count);
    /// For the actions in a parsing table.
    /// if an action yyptblact[i] is positive, it's a shift/goto;
    /// if it is negative, it's a reduce;
    /// if it's zero, it's accept.
    void print_parsing_tbl(const YAlgorithm& y_algorithm);
};

/// rewind to section 2.
static void
goto_section2(std::ifstream& fp_yacc, uint32_t& n_line)
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

/// Pass section 2, go to section 3.
/// Presumption: finished section 1, entering section 2.
static void
goto_section3(std::ifstream& fp_yacc, uint32_t& n_line)
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

/// This function will return the position of $A
/// to the end of yaccpar.
/// This is for the purpose of inserting code
/// associated with reductions.
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

/// Returns the index of the given symbol in the
/// non-terminal array of the given grammar.
/// Used in gen_compiler.c.
static auto
get_non_terminal_index(const Grammar& grammar,
                       std::shared_ptr<const SymbolTableNode> snode) -> int
{
    int i = 0;
    for (const auto& a : grammar.non_terminal_list) {
        if (snode == a.snode)
            return i;
        i++;
    }
    return -1;
}

/// yyr1[i] represents index of non-terminal symbol
/// on the LHS of reduction i, or a terminal symbol
/// if use unit-production-removal and in step 3
/// the LHS are replaced with leaf terminals in the
/// multi-rooted trees.
static void
print_yyr1(std::ofstream& fp, const Grammar& grammar)
{
    fp << "static YYCONST yytabelem yyr1[] = {" << std::endl;
    // First rule is always "$accept : ...".
    fp << std::setw(INTEGER_PADDING) << 0 << ',';

    if (Options::get().use_remove_unit_production) {
        int index = 0;
        for (size_t i = 1; i < grammar.rules.size(); i++) {
            // std::cout << "rule " <<  i<< " lhs: " << grammar.rules.at(i)->LHS
            // << std::endl;
            index = grammar.rules.at(i)->nLHS->snode->value;
            fp << std::setw(INTEGER_PADDING) << index;
            if (i < grammar.rules.size() - 1)
                fp << ',';
            if ((i + 1) % ITEM_PER_LINE == 0)
                fp << std::endl;
        }
        fp << "};" << std::endl;
        return;
    }

    for (size_t i = 1; i < grammar.rules.size(); i++) {
        fp << std::setw(INTEGER_PADDING)
           << (-1) * get_non_terminal_index(grammar,
                                            grammar.rules.at(i)->nLHS->snode);
        if (i < grammar.rules.size() - 1)
            fp << ',';
        if ((i + 1) % ITEM_PER_LINE == 0)
            fp << std::endl;
    }
    fp << "};" << std::endl;
}

/// yyr2[0] is a dummy field, and is alwasy 0.
/// for i >= 1, yyr2[i] defines the following for grammar rule i:
///   let x be the number of symbols on the RHS of rule i,
///   let y indicate whether there is any code associated
///     with this rule (y = 1 for yes, y = 0 for no).
///   then yyr2[i] = (x << 1) + y;
static void
print_yyr2(std::ofstream& fp, const Grammar& grammar)
{
    fp << "static YYCONST yytabelem yyr2[] = {" << std::endl;
    fp << std::setw(INTEGER_PADDING) << 0 << ',';
    for (size_t i = 1; i < grammar.rules.size(); i++) {
        fp << std::setw(INTEGER_PADDING)
           << (grammar.rules.at(i)->nRHS.size() << 1) +
                static_cast<int>(grammar.rules.at(i)->hasCode);
        if (i < grammar.rules.size() - 1)
            fp << ',';
        if ((i + 1) % ITEM_PER_LINE == 0)
            fp << std::endl;
    }
    fp << "};" << std::endl;
}

static void
print_yynonterminals(std::ofstream& fp, const Grammar& grammar)
{
    fp << "yytoktype yynts[] = {" << std::endl;

    size_t i = 1; // ignore first nonterminal: $accept.
    auto a = grammar.non_terminal_list.begin();
    a++;
    for (; a != grammar.non_terminal_list.end(); a++) {
        fp << "\t\"" << a->snode->symbol << "\",\t-" << i << ',' << std::endl;
        i++;
    }
    fp << "\t\"-unknown-\", 1  /* ends search */" << std::endl;
    fp << "};" << std::endl;
}

/// Print terminal tokens.
static void
print_yytoks(std::ofstream& fp, const SymbolList& tokens)
{
    fp << "yytoktype yytoks[] = {" << std::endl;
    for (const auto& a : tokens) {
        if (*a.snode->symbol == STR_ERROR)
            continue;

        if (a.snode->symbol->size() == 2 &&
            (*a.snode->symbol).at(0) == '\\') { // escape sequence
            fp << R"(	"\\)" << a.snode->symbol << R"(",	)"
               << a.snode->value << ',' << std::endl;
        } else {
            fp << "\t\"" << a.snode->symbol << "\",\t" << a.snode->value << ','
               << std::endl;
        }
    }

    fp << "\t\"-unknown-\", -1  /* ends search */" << std::endl
       << "};" << std::endl;
}

/// Print reductions.
static void
print_yyreds(std::ofstream& fp, const Grammar& grammar)
{
    fp << "char * yyreds[] = {" << std::endl;
    fp << "\t\"-no such reduction-\"" << std::endl;
    for (const auto& rule : grammar.rules) {
        fp << "\t\"" << rule->nLHS->snode->symbol << " : ";

        auto a = rule->nRHS.begin();
        for (size_t j = 0; j < rule->nRHS.size(); j++) {
            if (j > 0)
                fp << ' ';

            if (j > 0)
                a++;
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

static void
print_parsing_tbl_entry(std::ofstream& fp,
                        const std::optional<Action> action,
                        const StateHandle state_no,
                        uint32_t& count)
{
    bool is_entry = false;
    if (action == Action::Shift || action == Action::Goto) {
        fp << state_no << ", ";
        is_entry = true;
    } else if (action == Action::Reduce) {
        fp << '-' << state_no << ", ";
        is_entry = true;
    } else if (action == Action::Accept) {
        fp << "0, ";
        is_entry = true;
    }

    if (is_entry) {
        count++;
        if (count % ITEM_PER_LINE == 0 && count != 0)
            fp << std::endl;
    }
}

static auto
use_lrk(const std::optional<LRkPTArray>& lrk_pt_array) -> bool
{
    return Options::get().use_lr_k &&
           (lrk_pt_array.has_value() && lrk_pt_array->max_k() >= 2);
}

static void
write_lrk_table_arrays(
  std::ofstream& fp,
  const std::optional<LRkPTArray>& lrk_pt_array,
  const std::vector<std::shared_ptr<SymbolTableNode>>& parsing_tbl_col_hdr)
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
    fp << "static YYCONST yytabelem yy_lrk_cols = "
       << parsing_tbl_col_hdr.size() + 2 << ';' << std::endl;

    // yy_lrk_r[].
    fp << std::endl << "/* Values in each LR(k) parsing table. */" << std::endl;
    fp << "static YYCONST yytabelem yy_lrk_r[] = {" << std::endl;
    size_t k = 2;
    for (const LRkPT* t : lrk_pt_array->array) {
        for (const LRkPTRow* r = t->rows; r != nullptr; r = r->next) {
            fp << "  " << r->state << ", " << r->token->snode->value << ", ";
            for (size_t j = 0; j < parsing_tbl_col_hdr.size(); j++) {
                if (r->row.at(j).has_value()) {
                    if (r->row.at(j)->end.has_value()) {
                        fp << j << ", " << r->row.at(j)->end.value()->ruleID
                           << ", ";
                    } else {
                        fp << j << ", " << -2 << ", ";
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
    for (size_t i = 0; i < parsing_tbl_col_hdr.size(); i++) {
        if (i > 0)
            fp << ", ";
        if (i % ITEM_PER_LINE == 0) {
            if (i > 0)
                fp << std::endl;
            fp << "  ";
        }
        if (*parsing_tbl_col_hdr[i]->symbol == "$accept") {
            fp << "CONST_ACC";
        } else if (*parsing_tbl_col_hdr[i]->symbol == "$end") {
            fp << 0;
        } else {
            fp << parsing_tbl_col_hdr[i]->value;
        }
    }
    fp << std::endl << "};" << std::endl;
    fp << std::endl << std::endl;
}

void
GenCompiler::prepare_outfile(const FileNames& files, const bool use_header_file)
{
    this->fp.open(files.y_tab_c);
    if (!this->fp.is_open()) {
        throw std::runtime_error(std::string("Cannot open output file ") +
                                 files.y_tab_c);
    }
    if (!use_header_file)
        return;
    this->fp_h.open(files.y_tab_h);
    if (!fp_h.is_open()) {
        this->fp.close();
        throw std::runtime_error(std::string("Cannot open output file ") +
                                 files.y_tab_h);
    }
}

auto
GenCompiler::process_yacc_file_section1(const SymbolList& tokens) -> Position
{
    Position position{ 1, 1 };
    bool is_code = false;
    char c = '\0', last_c = '\n', last_last_c = '\0';

    while (this->fp_yacc.get(c)) {
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
            this->fp << c;
        }

        last_last_c = last_c;
        last_c = c;

        position.col += 1;
        if (c == '\n') {
            position.line += 1;
            position.col = 1;
        }
    }

    this->write_tokens_to_compiler_file(tokens);
    return position;
}

auto
GenCompiler::find_full_rule(const Grammar& grammar, int rule_count)
  -> Production*
{
    Production* rule = nullptr;
    const SymbolNode* node = nullptr;
    std::shared_ptr<const SymbolTableNode> sym = nullptr;

    for (size_t full_rule = rule_count; full_rule < grammar.rules.size();
         ++full_rule) {

        if ((rule = grammar.rules.at(full_rule)) && (node = rule->nLHS.get()) &&
            (sym = node->snode)) {
            if (sym->symbol->starts_with(
                  "$$")) // node symbol starting with $$ we continue
                break;
        } else {
            throw std::runtime_error(
              std::string("Malformed grammar rule at index ") +
              std::to_string(full_rule));
        }
    }

    return rule;
}

void
GenCompiler::write_tokens_to_compiler_file(const SymbolList& tokens)
{
    this->fp << std::endl << "/* tokens */" << std::endl << std::endl;
    if (options.use_header_file)
        this->fp_h << std::endl << "/* tokens */" << std::endl << std::endl;

    uint32_t index = 0;
    for (const auto& a : tokens) {
        if (a.snode->TP->is_quoted || *a.snode->symbol == STR_ERROR)
            continue;

        this->fp << "#define " << a.snode->symbol << index + TOKEN_START_INDEX
                 << std::endl;
        if (this->options.use_header_file)
            this->fp_h << "#define " << a.snode->symbol
                       << index + TOKEN_START_INDEX << std::endl;
        index++;
    }

    this->fp << "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED"
             << std::endl
             << this->yystype_definition << std::endl
             << "#define YYSTYPE_IS_DECLARED 1" << std::endl
             << "#endif" << std::endl;
    if (this->options.use_header_file) {
        this->fp_h << "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED"
                   << std::endl
                   << this->yystype_definition << std::endl
                   << "#define YYSTYPE_IS_DECLARED 1" << std::endl
                   << "#endif" << std::endl;
        this->fp_h << std::endl << "extern YYSTYPE yylval;" << std::endl;
    }
}

void
GenCompiler::process_yacc_file_section3()
{
    char c = '\0';
    while (this->fp_yacc.get(c)) {
        this->fp << c;
    }
}

auto
GenCompiler::find_mid_prod_index(const Production& rule,
                                 const Production& mid_prod_rule)
  -> std::optional<size_t>
{
    const SymbolNode* lnode = mid_prod_rule.nLHS.get();
    std::shared_ptr<const SymbolTableNode> lsym = lnode->snode;
    const std::string_view l = *lsym->symbol;
    std::shared_ptr<const SymbolTableNode> rsym = nullptr;

    size_t i = 0;
    for (const auto& rnode : rule.nRHS) {
        const std::string_view r = *rsym->symbol;
        if (!(rsym = rnode.snode)) {
            throw std::runtime_error(
              std::string("Did not find mid production rule of ").append(l));
        }
        if (l == r)
            return i;
        i++;
    }
    return std::nullopt;
}

auto
GenCompiler::find_sym(const Production* rule, uint32_t dollar_number)
  -> std::shared_ptr<const SymbolTableNode>
{
    std::shared_ptr<const SymbolTableNode> sym = nullptr;
    if (dollar_number == MAX_RULE_LENGTH)
        return sym;

    uint32_t i = 1;
    auto node = rule->nRHS.begin();
    for (; i < dollar_number && node != rule->nRHS.end(); ++i, node++) {
    }
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

void
GenCompiler::get_final_states()
{
    this->fp << "static YYCONST yytabelem yyfs[] = {" << std::endl;
    this->fp << final_state_list.at(0);
    int j = 0;
    for (size_t i = 1; i < ParsingTblRows; i++) {
        if (this->options.use_remove_unit_production) {
            if (is_reachable_state(i) == false)
                continue;
        }

        this->fp << ", ";
        if ((++j) % ITEM_PER_LINE == 0)
            this->fp << std::endl;
        this->fp << final_state_list.at(i);
    }
    this->fp << "};" << std::endl << std::endl;
}

void
GenCompiler::print_parsing_tbl_col(const YAlgorithm& y_algorithm)
{
    // labels a final state's col entry
    constexpr int FINAL_STATE_COL_ENTRY = -10000001;

    uint32_t count = 0;

    this->fp << "static YYCONST yytabelem yyptbltok[] = {" << std::endl;

    if (this->options.use_remove_unit_production) {
        for (size_t row = 0; row < ParsingTblRows; row++) {
            if (is_reachable_state(row)) {

                if constexpr (USE_REM_FINAL_STATE) {
                    if (final_state_list.at(row) < 0) {
                        GenCompiler::print_parsing_tbl_col_entry(
                          this->fp,
                          Action::Reduce,
                          FINAL_STATE_COL_ENTRY,
                          count);
                        continue;
                    }
                }
                for (size_t col = 0; col < y_algorithm.ParsingTblColHdr.size();
                     col++) {
                    std::shared_ptr<SymbolTableNode> n =
                      y_algorithm.ParsingTblColHdr.at(col);
                    if (is_goal_symbol(y_algorithm.grammar, n) == false &&
                        is_parent_symbol(n) == false) {
                        auto [action, state] =
                          y_algorithm.get_action(n->type, col, row);
                        GenCompiler::print_parsing_tbl_col_entry(
                          this->fp, action, n->value, count);
                    }
                }
            }
        }
    } else {
        for (size_t i = 0; i < ParsingTblRows; i++) {

            if constexpr (USE_REM_FINAL_STATE) {
                if (final_state_list.at(i) < 0) { // is a final state.
                    GenCompiler::print_parsing_tbl_col_entry(
                      this->fp, Action::Reduce, FINAL_STATE_COL_ENTRY, count);
                    continue;
                }
            }
            for (size_t j = 0; j < y_algorithm.ParsingTblColHdr.size(); j++) {
                std::shared_ptr<SymbolTableNode> n =
                  y_algorithm.ParsingTblColHdr.at(j);
                auto [action, state] = y_algorithm.get_action(n->type, j, i);
                GenCompiler::print_parsing_tbl_col_entry(
                  this->fp, action, n->value, count);
            }
        }
    }

    this->fp << "-10000000};" << std::endl
             << std::endl; // -10000000 is space filler
}

void
GenCompiler::process_yacc_file_section2(YSymbol& y_symbol,
                                        const Grammar& grammar,
                                        const std::string_view filename,
                                        Position position)
{
    YACC_STATE state = LHS;
    int code_level = 0;
    bool reading_symbol = false;
    bool reading_number = false;
    bool reading_type = false;
    uint32_t dollar_number = 0;
    char c = '\0', last_c = '\0', last_last_c = '\0';
    int rule_count = 0;
    bool end_of_code = false; // for mid-production action.
    std::optional<std::string> explicit_type = std::nullopt;
    static const std::string_view padding = "        ";

    while (this->fp_yacc.get(c)) {
        if (last_c == '%' && c == '%')
            break; // end section 2.

        switch (state) {
            case LHS:
                if (isspace(c) &&
                    !reading_symbol) { // NOLINT(bugprone-branch-clone)
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
                    this->my_perror("error: state COLON. ", c, position);
                }
                break;
            case COLON_COMMENT:
                if (last_c == '*' && c == '/') {
                    state = COLON;
                }
                break;
            case RHS:
                if (isspace(c)) { // NOLINT(bugprone-branch-clone)
                    // end of a symbol. do nothing.
                } else if (c == '\'') {
                    state = TERMINAL;
                    if (end_of_code) {
                        rule_count++;
                        this->print_break();
                        end_of_code = false;
                    }

                } else if (c == ';') {
                    state = LHS; // end of a rule.
                    if (end_of_code) {
                        this->print_break();
                    }
                    end_of_code = false;
                } else if (c == '|') { // end of a rule
                    rule_count++;
                    if (end_of_code) {
                        this->print_break();
                    }
                    end_of_code = false;
                } else if (c == '{') { // code for rule #rule_count
                    state = CODE;
                    code_level = 1;

                    if (end_of_code == false) {
                        this->fp << std::endl
                                 << padding << "  case " << rule_count << ':'
                                 << std::endl;
                        if (this->options.use_lines)
                            this->fp << "# line " << position.line << " \""
                                     << filename << '\"' << std::endl;
                    }
                    this->fp << '{';

                } else if (last_c == '/' && c == '*') {
                    state = COMMENT;
                } else if (last_c == '/' && c == '/') {
                    state = COMMENT2;
                } else if (c == '/') {
                    // do nothing
                } else if (c == ':') {
                    this->my_perror(
                      "You may miss a ';' in the last rule.", c, position);
                } else {
                    // reading a symbol. do nothing
                    if (end_of_code) {
                        rule_count++;
                        this->print_break();
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
                        y_symbol.add_char_to_symbol('\0');
                        explicit_type = y_symbol.get_symbol();
                    } else {
                        y_symbol.add_char_to_symbol(c);
                    }
                } else if (last_c == '$' && c == '<') {
                    y_symbol.reset_symbol();
                    reading_type = true;
                } else if (last_c != '$' && c == '$') {
                    // do nothing, this may be a special character.
                } else if (last_c == '$' && c == '$') {
                    std::optional<std::string> token_type = std::nullopt;
                    if (explicit_type) {
                        token_type = explicit_type;
                        explicit_type = std::nullopt;
                    } else {
                        const Production* rule =
                          GenCompiler::find_full_rule(grammar, rule_count);
                        token_type = rule->nLHS->snode->token_type;
                    }
                    if (token_type)
                        this->fp << "(yyval." << token_type.value() << ')';
                    else
                        this->fp << "yyval";

                } else if (last_c == '$' && isdigit(c)) {
                    reading_number = true;
                    dollar_number = (c - '0') + BASE_TEN * dollar_number;
                } else if (reading_number && isdigit(c)) {
                    dollar_number = (c - '0') + BASE_TEN * dollar_number;
                } else if (reading_number && !isdigit(c)) {
                    Production* start_rule = grammar.rules.at(rule_count);
                    std::optional<size_t> rhs_index = 0;

                    const Production* rule =
                      GenCompiler::find_full_rule(grammar, rule_count);
                    if (rule != start_rule) {
                        rhs_index =
                          GenCompiler::find_mid_prod_index(*rule, *start_rule);
                    } else
                        rhs_index = rule->nRHS.size();
                    std::optional<std::string> token_type = std::nullopt;
                    if (explicit_type) {
                        token_type = explicit_type;
                        explicit_type = std::nullopt;
                    } else {
                        std::shared_ptr<const SymbolTableNode> sym =
                          GenCompiler::find_sym(rule, dollar_number);
                        token_type = sym->token_type;
                    }
                    int rhs_index_int =
                      rhs_index.has_value() ? static_cast<int>(*rhs_index) : -1;
                    if (token_type)
                        this->fp << "(yypvt[" << dollar_number - rhs_index_int
                                 << "]." << token_type.value() << ")/* "
                                 << rule_count << ' ' << rhs_index_int << " */";
                    else
                        this->fp << "yypvt[" << dollar_number - rhs_index_int
                                 << "]/* " << rule_count << ' ' << rhs_index_int
                                 << " */";

                    this->fp << c;
                    reading_number = false;
                    dollar_number = 0;
                } else {
                    this->fp << c;
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
                    this->fp << ' '; // this->print_break();
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
                this->fp << c;
                if (c == '\"' && last_c != '\\')
                    state = CODE;
                break;
            case CODE_SINGLE_QUOTE:
                this->fp << c;
                if (c == '\'')
                    state = CODE;
                break;
            case CODE_COMMENT:
                this->fp << c;
                if (c == '/' && last_c == '*')
                    state = CODE;
                break;
            case CODE_COMMENT2:
                this->fp << c;
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
        }

        // putc(c, stdout);
        last_last_c = last_c;
        last_c = c;

        position.col++;
        if (c == '\n') {
            position.line++;
            position.col = 1;
        }
    }
}

void
GenCompiler::write_parsing_table_arrays(
  const std::optional<LRkPTArray>& lrk_pt_array,
  const YAlgorithm& y_algorithm)
{
    this->get_final_states();

    this->print_parsing_tbl_col(y_algorithm); // yytbltok[]
    this->print_parsing_tbl(y_algorithm);     // yytblact[], yyrowoffset[]

    print_yyr1(this->fp, y_algorithm.grammar); // yyr1[]
    print_yyr2(this->fp, y_algorithm.grammar); // yyr2[]

    if (!use_lrk(lrk_pt_array)) {
        fp << std::endl << "#ifdef YYDEBUG" << std::endl << std::endl;
        fp << "typedef struct {char *t_name; int t_val;} yytoktype;"
           << std::endl
           << std::endl;
        print_yynonterminals(fp, y_algorithm.grammar); // yynts[]. nonterminals.

        print_yytoks(fp, y_algorithm.grammar.tokens); // yytoks[]. tokens.
        print_yyreds(fp,
                     y_algorithm.grammar); // yyreds[]. Productions of grammar.
        fp << "#endif /* YYDEBUG */" << std::endl << std::endl;

    } else { // use LR(k).
        fp << "typedef struct {char *t_name; int t_val;} yytoktype;"
           << std::endl
           << std::endl;
        print_yynonterminals(fp, y_algorithm.grammar); // yynts[]. nonterminals.
        print_yytoks(fp, y_algorithm.grammar.tokens);  // yytoks[]. tokens.

        fp << std::endl << "#ifdef YYDEBUG" << std::endl << std::endl;
        print_yyreds(fp,
                     y_algorithm.grammar); // yyreds[]. Productions of grammar.
        fp << "#endif /* YYDEBUG */" << std::endl << std::endl;

        write_lrk_table_arrays(fp, lrk_pt_array, y_algorithm.ParsingTblColHdr);
    }
}

void
GenCompiler::print_parsing_tbl_col_entry(std::ofstream& fp,
                                         const std::optional<Action> action,
                                         const int token_value,
                                         uint32_t& count)
{
    bool is_entry = false;
    if (action.has_value()) {
        fp << token_value << ", ";
        is_entry = true;
    }

    if (is_entry) {
        count++;
        if (count % ITEM_PER_LINE == 0 && count != 0)
            fp << std::endl;
    }
}

void
GenCompiler::print_parsing_tbl(const YAlgorithm& y_algorithm)
{
    std::vector<uint32_t> rowoffset;
    rowoffset.reserve(ParsingTblRows);
    uint32_t count = 0;

    this->fp << "static YYCONST yytabelem yyptblact[] = {" << std::endl;

    if (y_algorithm.options.use_remove_unit_production) {
        for (size_t row = 0; row < ParsingTblRows; row++) {
            if (is_reachable_state(row)) {

                if constexpr (USE_REM_FINAL_STATE) {
                    if (final_state_list.at(row) < 0) {
                        print_parsing_tbl_entry(this->fp,
                                                Action::Shift,
                                                final_state_list.at(row),
                                                count);
                        rowoffset.push_back(count);
                        continue;
                    }
                }
                for (size_t col = 0; col < y_algorithm.ParsingTblColHdr.size();
                     col++) {
                    std::shared_ptr<const SymbolTableNode> n =
                      y_algorithm.ParsingTblColHdr.at(col);
                    if (is_goal_symbol(y_algorithm.grammar, n) == false &&
                        is_parent_symbol(n) == false) {
                        auto [action, state_no] =
                          y_algorithm.get_action(n->type, col, row);
                        if (action == Action::Shift || action == Action::Goto)
                            state_no = *get_actual_state(state_no);
                        // std::cout  <<  action <<  state_no<< "\t";
                        print_parsing_tbl_entry(
                          this->fp, action, state_no, count);
                    }
                }

                rowoffset.push_back(count);
                // std::cout  << std::endl;
            }
        }
    } else {
        for (size_t i = 0; i < ParsingTblRows; i++) {

            if constexpr (USE_REM_FINAL_STATE) {
                if (final_state_list.at(i) < 0) {
                    print_parsing_tbl_entry(
                      this->fp, Action::Shift, final_state_list.at(i), count);
                    rowoffset.push_back(count);
                    continue;
                }
            }

            for (size_t j = 0; j < y_algorithm.ParsingTblColHdr.size(); j++) {
                auto [action, state_no] = y_algorithm.get_action(
                  y_algorithm.ParsingTblColHdr.at(j)->type, j, i);
                // std::cout  <<  action <<  state_no<< ", ";
                print_parsing_tbl_entry(this->fp, action, state_no, count);
            }

            rowoffset.push_back(count);
            // std::cout  << std::endl;
        }
    }

    this->fp << "-10000000};" << std::endl
             << std::endl; // -10000000 is space filler

    this->fp << "static YYCONST yytabelem yyrowoffset[] = {\n0, " << std::endl;
    for (size_t i = 0; i < rowoffset.size(); i++) {
        this->fp << rowoffset.at(i);
        if (i < rowoffset.size() - 1)
            this->fp << ", ";
        if (i % ITEM_PER_LINE == 0 && i != 0)
            this->fp << std::endl;
    }
    this->fp << "};" << std::endl
             << std::endl; // NOTE: the last entry is (yyptbl.size - 1).
}

///////////////////////////////////////////////////////
// Functions to print parsing table arrays. END.
///////////////////////////////////////////////////////

static void
write_special_info(std::ofstream& fp)
{
    fp << std::endl << "YYSTYPE yylval;" << std::endl;
    if (Options::get().use_yydebug) {
        fp << std::endl << "#define YYDEBUG 1" << std::endl;
    }
}

/// Do this is use LR(k).
static void
get_lrk_hyacc_path(const std::optional<LRkPTArray>& lrk_pt_array)
{
    if (use_lrk(lrk_pt_array)) {
        std::cout << "lrk used" << std::endl;
        HYACC_PATH += 'k';
        std::cout << "LR(k) HYACC_PATH: " << HYACC_PATH << std::endl;
    }
}

void
YAlgorithm::generate_compiler(YSymbol& y_symbol,
                              std::string yystype_definition,
                              const std::optional<LRkPTArray>& lrk_pt_array,
                              const std::string& infile,
                              const FileNames& files)
{
    // count number of lines in yacc input file.
    auto& options = this->options;
    auto gen_compiler = GenCompiler(options, yystype_definition);

    gen_compiler.fp_yacc.open(infile);
    if (!gen_compiler.fp_yacc.is_open()) {
        throw std::runtime_error(std::string("error: can't open file ") +
                                 infile);
    }

    gen_compiler.prepare_outfile(
      files, options.use_header_file); // open output compiler file.

    if (options.use_lines)
        gen_compiler.fp << std::endl
                        << "# line 1 \"" << infile << '\"' << std::endl;
    Position position = gen_compiler.process_yacc_file_section1(
      this->grammar.tokens); // declaration section.

    write_special_info(gen_compiler.fp);

    goto_section3(gen_compiler.fp_yacc, position.line);

    if (options.use_lines)
        gen_compiler.fp << std::endl
                        << "# line " << position.line << " \"" << infile << '\"'
                        << std::endl;

    gen_compiler.process_yacc_file_section3(); // code section.

    gen_compiler.fp << std::endl << "#define YYCONST const" << std::endl;
    gen_compiler.fp << "typedef int yytabelem;" << std::endl << std::endl;
    gen_compiler.write_parsing_table_arrays(lrk_pt_array, *this);

    get_lrk_hyacc_path(lrk_pt_array); // do this if LR(k) is used

    copy_yaccpar_file_1(gen_compiler.fp, HYACC_PATH);
    goto_section2(gen_compiler.fp_yacc, position.line);
    gen_compiler.process_yacc_file_section2(y_symbol,
                                            this->grammar,
                                            infile,
                                            position); // get reduction code.
    copy_yaccpar_file_2(gen_compiler.fp, HYACC_PATH);

    gen_compiler.fp.close();
    if (options.use_header_file)
        gen_compiler.fp_h.close();
    gen_compiler.fp_yacc.close();
}
