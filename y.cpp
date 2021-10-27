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

/****************************************************************
 * y.c
 *
 * Generates a LR(1) parsing machine given a grammar.
 *
 * @author: Xin Chen
 * @Date Started: 8/30/2006
 * @Last modified: 3/24/2009
 ****************************************************************/

#include "y.hpp"
#include "lane_tracing.hpp"
#include <cstddef>
#include <iostream>
#include <stdexcept>
#include <string>

FILE* fp_v;
bool USE_COMBINE_COMPATIBLE_CONFIG;
bool USE_COMBINE_COMPATIBLE_STATES;
bool USE_REMOVE_UNIT_PRODUCTION;
bool USE_REMOVE_REPEATED_STATES;
bool SHOW_GRAMMAR;
bool SHOW_PARSING_TBL;
bool DEBUG_GEN_PARSING_MACHINE;
bool DEBUG_COMB_COMP_CONFIG;
bool DEBUG_BUILD_MULTIROOTED_TREE;
bool DEBUG_REMOVE_UP_STEP_1_2;
bool DEBUG_REMOVE_UP_STEP_4;
bool SHOW_TOTAL_PARSING_TBL_AFTER_RM_UP;
bool SHOW_THEADS;
bool DEBUG_EXPAND_ARRAY;
bool DEBUG_HASH_TBL;
bool SHOW_SS_CONFLICTS;
bool SHOW_STATE_TRANSITION_LIST;
bool SHOW_STATE_CONFIG_COUNT;
bool SHOW_ACTUAL_STATE_ARRAY;
bool USE_YYDEBUG;
bool USE_LINES;
bool USE_VERBOSE;
const char* y_tab_c;
const char* y_tab_h;
bool USE_OUTPUT_FILENAME;
bool USE_FILENAME_PREFIX;
bool USE_HEADER_FILE;
bool USE_GENERATE_COMPILER;
bool PRESERVE_UNIT_PROD_WITH_CODE;
const char* y_output;
const char* y_gviz;
bool USE_GRAPHVIZ;
bool USE_LR0;
bool USE_LALR;
bool USE_LANE_TRACING;
bool USE_LR_K;
bool SHOW_ORIGINATORS;
int MAX_K;
std::array<HashTblNode, HT_SIZE> HashTbl;
Queue* config_queue;
int OriginatorList_Len_Init;
int ss_count;
int rr_count;
int rs_count;
int expected_sr_conflict;
Grammar grammar;
StateCollection* states_new;
State_array* states_new_array;
size_t PARSING_TABLE_SIZE;
int* ParsingTable;
int ParsingTblCols;
int ParsingTblRows;
SymbolTblNode** ParsingTblColHdr;
SymbolList F_ParsingTblColHdr;
int F_ParsingTblCols;
int* states_reachable;
int states_reachable_count;
int* actual_state_no;
int actual_state_no_ct;
int n_symbol;
int n_rule;
int n_rule_opt;
int n_state_opt1;
int n_state_opt12;
int n_state_opt123;
int* final_state_list;
SymbolList tokens;
SymbolNode* tokens_tail;
int tokens_max_ct;
int tokens_ct;
StateNoArray* states_inadequate;

/* Declaration of functions. */
extern void
use_grammar(int grammar_number);
void
dump_state_collections();
auto
is_final_configuration(const Configuration* c) -> bool;
auto
create_config(int rule_id, int marker, int is_core_config) -> Configuration*;
void
copy_config(Configuration* c_dest, const Configuration* c_src);
void
init_parsing_table();
void
init_start_state();
void
write_parsing_table_col_header();
auto
create_production(char* lhs, char* rhs[], int rhs_count) -> Production*;
void
write_configuration(const Configuration& c);
void
combine_compatible_config(State* s);
auto
combine_compatible_states(State* s_dest, State* s_src) -> bool;
void
insert_reduction_to_parsing_table(const Configuration* c, int state_no);
void
get_reachable_states(int cur_state, int states_reachable[], int* states_count);
void
write_ups_states();
void
print_int_array(int a[], int a_ct);
auto
create_state_collection() -> StateCollection*;
void
destroy_state_collection(StateCollection* c);
void
expand_parsing_table();
void
copy_context(Context* dest, const Context* src);
void
copy_production(Production* dest, Production* src);
void
free_production(Production* p);
void
clear_production(Production* p);
void
clear_context(Context* c);
void
free_context(Context* c);
void
free_config(Configuration* c);
auto
get_scanned_symbol(const Configuration* c) -> SymbolTblNode*;
auto
is_compatible_config(Configuration* c1, Configuration* c2) -> bool;
void
insert_action(SymbolTblNode* lookahead, int row, int state_dest);
auto
add_to_conflict_array(int state,
                      SymbolTblNode* lookahead,
                      int action1,
                      int action2) -> Conflict*;
void
write_state_transition_list();
void
write_symbol_node_array(SymbolNode* str);

char* hyacc_filename;

auto
get_grammar_rule_count() -> size_t
{
    return grammar.rules.size();
}

/*
 * For StateList. Start.
 */

auto
StateList::create() -> StateList*
{
    auto* l = new StateList;
    l->state_list.reserve(5);
    return l;
}

void
StateList::destroy(StateList* L)
{
    if (L == nullptr)
        return;
    delete L;
}

// void
// StateList_expand(StateList* L)
// {
//     if (nullptr == L)
//         return;
//     HYY_EXPAND(&L->state_list, L->size * 2);
//     L->size *= 2;
// }

/*
 * Add if not exist yet.
 * @Return: true is added, false if not added.
 */
auto
StateList::add(State* s) -> bool
{
    if (s == nullptr)
        return false;
    for (const auto& elem : this->state_list) {
        if (elem->state_no == s->state_no)
            return false;
    }
    this->state_list.push_back(s);
    return true;
}

auto
StateList::clone() -> StateList*
{
    auto* L = new StateList;
    L->state_list = this->state_list;
    return L;
}

void
StateList::write()
{
    yyprintf("\n  parents_list(%d): ", this->state_list.size());
    for (int i = 0; i < this->state_list.size(); i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%d", this->state_list[i]->state_no);
    }
    yyprintf("\n");
}

/*
 * For StateList. End.
 */

void
Production::write(int marker)
{
    yyprintf("%s ", this->nLHS->snode->symbol);
    yyprintf("-> ");

    int i = 0;
    for (SymbolNode* n = this->nRHS_head; n != nullptr; n = n->next) {
        if (i == marker)
            yyprintf(". ");
        yyprintf("%s ", n->snode->symbol);
        i++;
    }
    if (i == marker)
        yyprintf(". ");

    // print this only when marker = -1.
    // i.e. called from writeGrammar().
    if (marker == -1 && this->isUnitProduction)
        yyprintf("(unit production)");

    if (marker == -1 && this->lastTerminal != nullptr)
        yyprintf(" (Precedence Terminal: %s)", this->lastTerminal->symbol);

    // if write configration, then don't go to new line.
    // since the context has not been written.
    if (marker < 0)
        yyprintf("\n");
}

void
Grammar::write_rules()
{
    int count = 0;
    yyprintf("Rules: \n");
	int i = 0;
    for (int i = 0; i < this->rule_count; i++) {
        yyprintf("(%d) ", i);
        this->rules[i]->write(-1);
        count++;
    }
    yyprintf("Number of Rules: %d\n", count);
}

void
Grammar::write_rules_no_unit_prod()
{
    int count = 0;
    yyprintf("Rules: \n");
    for (int i = 0; i < this->rule_count; i++) {
        if ((is_unit_production(i) == false) || i == 0) {
            yyprintf("(%d) ", i);
            this->rules[i]->write(-1);
            count++;
        }
    }
    yyprintf("Number of Rules: %d\n", count);
}

auto
Grammar::get_opt_rule_count() -> int
{
    int count = 0;
    for (int i = 0; i < this->rule_count; i++) {
        if (!is_unit_production(i) || i == 0)
            count++;
    }
    return count;
}

void
Grammar::write_terminals()
{
    yyprintf("Terminals (%d): \n", this->terminal_count);

    SymbolNode* a = this->terminal_list;
    if (a != nullptr) {
        yyprintf("%s\n", a->snode->symbol);
        for (a = a->next; a != nullptr; a = a->next) {
            yyprintf("%s\n", a->snode->symbol);
        }
    }
    yyprintf("\n");
}

void
Grammar::write_non_terminals()
{
    yyprintf("Non-terminals (%d): \n", this->non_terminal_count);
    SymbolNode* a = this->non_terminal_list;
    if (a != nullptr) {
        yyprintf("%s\n", a->snode->symbol);
        for (a = a->next; a != nullptr; a = a->next) {
            yyprintf("%s\n", a->snode->symbol);
        }
    }
    yyprintf("\n");
}

void
Grammar::write_vanish_symbols()
{
    SymbolNode* a = nullptr;
    yyprintf("Vanish symbols (%d): \n", this->vanish_symbol_count);
    if ((a = this->vanish_symbol_list) != nullptr) {
        yyprintf("%s\n", a->snode->symbol);
        for (a = this->vanish_symbol_list; a != nullptr; a = a->next) {
            yyprintf("%s\n", a->snode->symbol);
        }
    }
    yyprintf("\n");
}

void
Grammar::write(bool before_rm_unit_prod)
{
    yyprintf("\n--Grammar--\n");
    this->write_terminals();
    this->write_non_terminals();
    this->write_vanish_symbols();
    yyprintf("Goal symbol: %s\n", this->goal_symbol->snode->symbol);

    if (before_rm_unit_prod || USE_REMOVE_UNIT_PRODUCTION == false) {
        this->write_rules();
    } else { // after remove unit production.
        this->write_rules_no_unit_prod();
    }
    yyprintf("\n");
}

/*
 * Free variables dynamically allocated by the program.
 * Called by function main().
 */
void
free_vars()
{
    // free dynamically allocated variables in grammar.
    for (int i = 0; i < grammar.rule_count; i++) {
        free_production(grammar.rules[i]);
    }

    // free dynamically allocated variables in states_new.
    destroy_state_collection(states_new);

    delete[] states_reachable;
    delete actual_state_no;
    delete ParsingTable;

    hash_tbl_destroy();

#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
    queue_destroy(config_queue);
#endif
}

auto
Conflict::create(int state, SymbolTblNode* lookahead, int r, int s)
  -> ConflictNode*
{
    auto* c = new Conflict;
    c->state = state;
    c->lookahead = lookahead;
    c->r = r;
    c->s = s;
    c->next = nullptr;
    return c;
}

void
Conflict::destroy_node(Conflict* c)
{
    delete c;
}

void
Conflict::destroy_list(Conflict* a)
{
    Conflict* b = nullptr;
    if (a == nullptr)
        return;
    while (a != nullptr) {
        b = a->next;
        delete a;
        a = b;
    }
}

auto
State_array::create() -> State_array*
{
    auto* s = new State_array();
    return s;
}

void
add_state_to_state_array(State_array& a, State* s)
{
    a.state_list.push_back(s);
}

void
inc_conflict_count(int s, int state)
{
    if (s > 0) {
        rs_count++;
        states_new_array->rs_count[state]++;
    } else {
        rr_count++;
        states_new_array->rr_count[state]++;
    }
}

/*
 * insert in incresing order by conflict's state no.
 *
 * Note: no need to check size of conflict arrays,
 * which is handled in expandParsingTable().
 */
auto
add_to_conflict_array(int state,
                      SymbolTblNode* lookahead,
                      int action1,
                      int action2) -> Conflict*
{
    int r = 0, s = 0; // r < s
    Conflict *c = nullptr, *b = nullptr, *b_prev = nullptr;

    if (action1 < action2) {
        r = action1;
        s = action2;
    } else {
        r = action2;
        s = action1;
    }

    if (states_new_array->rr_count[state] == 0 &&
        states_new_array->rs_count[state] == 0) {
        c = ConflictNode::create(state, lookahead, r, s);
        states_new_array->conflict_list[state] = c;
        inc_conflict_count(s, state);
        return c;
    }

    for (b = states_new_array->conflict_list[state]; b != nullptr;
         b_prev = b, b = b->next) {
        if (state == b->state && lookahead == b->lookahead && r == b->r &&
            s == b->s)
            return nullptr; // exits already.

        if (state < b->state) {
            c = ConflictNode::create(state, lookahead, r, s);

            if (b_prev == nullptr) { // insert at the head.
                c->next = states_new_array->conflict_list[state];
                states_new_array->conflict_list[state] = c;
            } else { // insert in the middle.
                c->next = b;
                b_prev->next = c;
            }
            inc_conflict_count(s, state);
            return c;
        }
    } // end of for.

    c = ConflictNode::create(state, lookahead, r, s);
    b_prev->next = c; // insert at the tail.
    inc_conflict_count(s, state);

    return c;
}

/*
 * Initialize variables when the program starts.
 * Called by function main().
 */
void
init()
{
    states_new = create_state_collection();
    states_new_array = State_array::create(); // size == PARSING_TABLE_SIZE

    if (USE_LALR) {
        states_inadequate = create_state_no_array();
        OriginatorList_Len_Init = 2;
    }

#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
    config_queue = queue_create(); // for getClosure().
#endif

    // for finding same/compatible states fast.
    init_state_hash_tbl();
    init_start_state();
    init_parsing_table();

    ss_count = rr_count = rs_count = 0;
}

void
write_context(Context* c)
{
    yyprintf(" {");

    const SymbolNode* s = c->nContext;
    if (s != nullptr) {
        yyprintf("%s", s->snode->symbol);
        while ((s = s->next) != nullptr) {
            yyprintf(", %s", s->snode->symbol);
        }
    }

    if (USE_LR_K) {
        // specifically for LR(k). This can be combined with the
        // above if block. Single this part out here is to keep
        // the code easier to maintain for LR(1) and LR(k) separately.
        for (c = c->next; c != nullptr; c = c->next) {
            yyprintf("; ");
            if ((s = c->nContext) != nullptr) {
                yyprintf("%s", s->snode->symbol);
                while ((s = s->next) != nullptr) {
                    yyprintf(", %s", s->snode->symbol);
                }
            }
        }
    }

    yyprintf("} ");
}

void
write_configuration(const Configuration& c)
{
    grammar.rules[c.ruleID]->write(c.marker);

    if (USE_LR0 && USE_LALR == false) { // LR(0), no context.
        // do nothing unless is goal production.
        if (c.ruleID == 0)
            yyprintf("%s", strEnd);
    } else {
        write_context(c.context);
    }

    if (c.isCoreConfig == 1u)
        yyprintf(" (core) ");

    if (USE_LALR && SHOW_ORIGINATORS) {
        if (c.LANE_END == 1u) {
            yyprintf(" [LANE_END]");
        }
        if (c.LANE_CON == 1u) {
            yyprintf(" [LANE_CON]");
        }
        if (c.COMPLETE == 1u) {
            yyprintf(" [COMPLETE]");
        }
        yyprintf("\n");
        // yyprintf(" [owner: %d]", c->owner->state_no);
        write_config_originators(c); /* in lane_tracing.c */
        write_config_transitors(c);  /* in lane_tracing.c */
    } else {
        yyprintf("\n");
    }
}

void
write_core_configuration(State* s)
{
    yyprintf("~~~~~Core configurations.Start~~~~~\n");
    for (int i = 0; i < s->core_config_count; i++) {
        write_configuration(*s->config[i]);
    }
    yyprintf("~~~~~Core configurations.End~~~~~\n");
}

void
write_successor_list(State& s)
{
    if (s.successor_count > 0)
        yyprintf("\n");
    //  yyprintf("\n-successor list-\n");
    for (int i = 0; i < s.successor_count; i++) {
        yyprintf("%s : %d\n",
                 s.successor_list[i]->trans_symbol->snode->symbol,
                 s.successor_list[i]->state_no);
    }
}

void
write_state_conflict_list(int state)
{
    if (USE_REMOVE_UNIT_PRODUCTION) {
        state = get_actual_state(state);
        // if (state == -1) return; // removed state.
    }

    if (state < 0)
        return;

    if (states_new_array->rr_count[state] == 0 &&
        states_new_array->rs_count[state] == 0)
        return;

    for (Conflict* c = states_new_array->conflict_list[state]; c != nullptr;
         c = c->next) {
        yyprintf("%d: ", c->state);
        if (c->s > 0) {
            yyprintf("shift/reduce conflict ");
            yyprintf("(shift %d, red'n %d)", c->s, (-1) * c->r);
        } else {
            yyprintf("reduce/reduce conflict ");
            yyprintf("red'n %d, red'n %d]", (-1) * c->s, (-1) * c->r);
        } // end if
        yyprintf(" on '%s'\n", c->lookahead->symbol);
    } // end for
}

void
write_grammar_conflict_list()
{
    if (rs_count == 0 && rr_count == 0)
        return;
    // yyprintf("==Conflict List==\n\n");
    yyprintf("Conflicts:");
    yyprintf("  %d shift/reduce, %d reduce/reduce\n\n", rs_count, rr_count);

    for (int i = 0; i < ParsingTblRows; i++) {
        if (states_new_array->rs_count[i] > 0) {
            yyprintf("  state %d: %d shift/reduce conflict%s",
                     i,
                     states_new_array->rs_count[i],
                     (states_new_array->rs_count[i] == 1) ? "" : "s");
            if (states_new_array->rr_count[i] > 0) {
                yyprintf(", %d reduce/reduce conflict%s",
                         states_new_array->rr_count[i],
                         (states_new_array->rr_count[i] == 1) ? "" : "s");
            }
            yyprintf("\n");
        } else if (states_new_array->rr_count[i] > 0) {
            yyprintf("  state %d: %d reduce/reduce conflict%s\n",
                     i,
                     states_new_array->rr_count[i],
                     (states_new_array->rr_count[i] == 1) ? "" : "s");
        }
    }
    yyprintf("\n");
}

/*
 *  Used when USE_REMOVE_UNIT_PROD is used.
 */
void
write_grammar_conflict_list2()
{
    int i = 0, state = 0, diff = 0;
    int final_rs_count = 0;
    int final_rr_count = 0;
    State_array* a = states_new_array;

    if (rs_count == 0 && rr_count == 0)
        return;
    // yyprintf("==Conflict List==\n\n");
    yyprintf("Conflicts:");
    yyprintf("  %d shift/reduce, %d reduce/reduce]\n\n", rs_count, rr_count);

    for (i = 0; i < ParsingTblRows; i++) {
        if (is_reachable_state(i) == false)
            continue;

        state = get_actual_state(i);

        if (a->rs_count[i] > 0) {
            yyprintf("  state %d: %d shift/reduce conflict%s",
                     state,
                     a->rs_count[i],
                     (a->rs_count[i] == 1) ? "" : "s");
            final_rs_count += a->rs_count[i];
            if (a->rr_count[i] > 0) {
                yyprintf(", %d reduce/reduce conflict%s",
                         a->rr_count[i],
                         (a->rr_count[i] == 1) ? "" : "s");
                final_rr_count += a->rr_count[i];
            }
            yyprintf("\n");
        } else if (a->rr_count[i] > 0) {
            yyprintf("  state %d: %d reduce/reduce conflict%s\n",
                     state,
                     a->rr_count[i],
                     (a->rr_count[i] == 1) ? "" : "s");
            final_rr_count += a->rr_count[i];
        }
    }

    if ((diff = rs_count - final_rs_count) > 0)
        yyprintf("  [%d shift/reduce conflict%s in removed states]\n",
                 diff,
                 (diff > 1) ? "s" : "");
    if ((diff = rr_count - final_rr_count) > 0)
        yyprintf("  [%d reduce/reduce conflict%s in removed states]\n",
                 diff,
                 (diff > 1) ? "s" : "");

    yyprintf("\n");
}

void
write_state(State& s)
{
    write_state_conflict_list(s.state_no);

    yyprintf("--state %d-- config count:%d, core_config count:%d\n",
             s.state_no,
             s.config_count,
             s.core_config_count);
    for (int i = 0; i < s.config_count; i++) {
        write_configuration(*s.config[i]);
    }

    write_successor_list(s);

    if (USE_LALR && SHOW_ORIGINATORS) {
        s.parents_list->write();
    }

    // writeCoreConfiguration(s);
    yyprintf("\n");
}

void
write_state_collection(StateCollection* c)
{
    State* s;

    yyprintf("==State Collection: (count=%d)\n", c->state_count);

    s = c->states_head;
    while (s != nullptr) {
        write_state(*s);
        s = s->next;
        // yyprintf("------------------------------------------\n");
        yyprintf("\n");
    }

    if (c->state_count == 0)
        yyprintf("(empty)\n");
    yyprintf("\n");
}

void
StateNode::destroy_state(State* s)
{
    if (s == nullptr) {
        // printf("destroyState warning: s is nullptr\n");
        return;
    }
    for (int i = 0; i < s->config_count; i++) {
        free_config(s->config[i]);
    }
    delete[] s->config;
    delete s->trans_symbol; // TODO: or delete[] ?
    delete[] s->successor_list;
    StateList::destroy(s->parents_list);
    delete s;
}

/*
 * Determine if string s is a non-terminal of grammar g.
 *
 * Instead of searching the entire list of non_terminal_list,
 * use hash table node's type. In O(1) time.
 * The type was obtained when calling get_terminals() and
 * get_nonterminals().
 */
auto
is_non_terminal(SymbolTblNode* s) -> bool
{
    return s->type == symbol_type::NONTERMINAL;
}

auto
is_terminal(SymbolTblNode* s) -> bool
{
    return s->type == symbol_type::TERMINAL;
}

/*
 * Add the given symbol to the context in increasing order.
 * There is no need to sort context later.
 */
auto
add_symbol2_context(SymbolTblNode* snode, Context* c) -> bool
{
    SymbolNode *s, *t, *s_prev;
    int cmp_val;

    for (s_prev = nullptr, s = c->nContext; s != nullptr;
         s_prev = s, s = s->next) {

        cmp_val = strcmp(s->snode->symbol, snode->symbol);
        if (cmp_val == 0)
            return false;  // already in context.
        if (cmp_val > 0) { // s_prev < snode < s
            t = create_symbol_node(snode);
            t->next = s;
            if (s_prev == nullptr) {
                c->nContext = t;
            } else {
                s_prev->next = t;
            }
            c->context_count++;

            return true; // inserted in the middle of context list.
        }
        // else: cmp_val < 0, go to next context node.
    }

    // insert at the end of list. now s == nullptr.
    if (s_prev == nullptr) {
        c->nContext = create_symbol_node(snode);
    } else {
        s_prev->next = create_symbol_node(snode);
    }
    c->context_count++;

    return true;
}

/*
 * Insert snode to the list, no repetition allowed, increasing order.
 * Do it like insertion sort.
 *
 * @parameters:
 *  exist - label whether snode already existed.
 */
auto
insert_symbol_list_unique_inc(SymbolList list,
                              SymbolTblNode* snode,
                              bool* exist) -> SymbolNode*
{
    *exist = false;
    if (list == nullptr)
        return create_symbol_node(snode);

    SymbolNode *n = list, *n_prev = nullptr;
    for (; n != nullptr; n_prev = n, n = n->next) {
        if (n->snode == snode) {
            *exist = true;
            return list; // existing node.
        }
        if (strcmp(n->snode->symbol, snode->symbol) > 0) {
            SymbolNode* new_node = create_symbol_node(snode);
            // insert new_snode before n.

            if (n_prev == nullptr) {
                new_node->next = list;
                return new_node;
            }
            new_node->next = n;
            n_prev->next = new_node;
            return list;
        }
    } // end of for.

    // insert as the last node.
    n_prev->next = create_symbol_node(snode);
    return list;
}

/*
 * Insert symbol to list tail if not exist, un-ordered.
 */
auto
insert_unique_symbol_list(SymbolList list, SymbolTblNode* snode, bool* exist)
  -> SymbolNode*
{
    *exist = false;

    if (list == nullptr)
        return create_symbol_node(snode);

    SymbolNode *n = list, *n_prev = nullptr;
    for (; n != nullptr; n_prev = n, n = n->next) {
        if (n->snode == snode) {
            *exist = true;
            return list; // existing node.
        }
    } // end of for.

    // insert as the last node.
    n_prev->next = create_symbol_node(snode);
    return list;
}

void
write_symbol_node_array(SymbolNode* str)
{
    for (SymbolNode* a = str; a != nullptr; a = a->next) {
        if (a != str)
            yyprintf(", ");
        yyprintf("%s", a->snode->symbol);
    }
    yyprintf("\n");
}

void
show_t_heads(SymbolList alpha, SymbolList theads)
{
    yyprintf("string '");

    SymbolNode* a = alpha;
    for (a = alpha; a != nullptr; a = a->next)
        yyprintf("%s ", a->snode->symbol);

    yyprintf("' has theads: ");

    for (a = theads; a != nullptr; a = a->next)
        yyprintf("%s ", a->snode->symbol);

    yyprintf("\n");
}

void
insert_alpha_to_heads(SymbolNode* s, SymbolNode* heads, SymbolNode* theads)
{
    bool exist = false;

    for (SymbolNode* a = s; a != nullptr; a = a->next) {
        if (is_vanish_symbol(*a->snode)) {
            // note: a vanishable symbol must be a non-terminal.
            heads->next =
              insert_unique_symbol_list(heads->next, a->snode, &exist);
        } else { // not vanlish symbol. break after insertion.
            if (is_terminal(a->snode)) { // here actually can insert to tail.
                theads->next =
                  insert_symbol_list_unique_inc(theads->next, a->snode, &exist);
            } else { // non_terminal.
                heads->next =
                  insert_unique_symbol_list(heads->next, a->snode, &exist);
            }
            return;
        } // end for
    }

    // all symbols are vanishable, since didn't return in the cycle.
    SymbolTblNode* snode = hash_tbl_find("");
    theads->next = insert_symbol_list_unique_inc(theads->next, snode, &exist);
}

/*
 * Insert the RHS symbols to heads up to an unvanishable symbol.
 */
void
insert_rhs_to_heads(SymbolNode* s, SymbolNode* heads, SymbolNode* theads)
{
    bool exist = false;

    for (SymbolNode* a = s; a != nullptr; a = a->next) {
        if (is_vanish_symbol(*a->snode)) {
            // note: a vanishable symbol must be a non-terminal.
            heads->next =
              insert_unique_symbol_list(heads->next, a->snode, &exist);
        } else { // not vanlish symbol. break after inserting last one.
            if (is_terminal(a->snode)) {
                theads->next =
                  insert_symbol_list_unique_inc(theads->next, a->snode, &exist);
            } else { // non_terminal.
                heads->next =
                  insert_unique_symbol_list(heads->next, a->snode, &exist);
            }
            return;
        } // end for
    }
}

/*
 * Algorithm:
 *
 * insert each symbol S in alpha to heads until S is NOT vanishable.
 * if S is a terminal, then insert S to theads; else insert to heads.
 * if all symbols are N.T., insert empty string to theads.
 *
 * for each symbol A in heads {
 *   for each grammar rule r where A is the LHS {
 *     for each symbol B in r's RHS until NOT vanishable {
 *       if B is NT, insert B to heads's tail;
 *       else B is T, insert (like insertion sort) to theads.
 *     }
 *   }
 * }
 *
 * @added to replace the old one on: 3/9/2008
 */
auto
get_theads(SymbolNode* alpha) -> SymbolNode*
{

    // dummy header of the lists heads and theads.
    SymbolNode* heads = create_symbol_node(hash_tbl_find(""));
    SymbolNode* theads = create_symbol_node(hash_tbl_find(""));

    insert_alpha_to_heads(alpha, heads, theads);

    SymbolNode* n = nullptr;
    for (n = heads->next; n != nullptr; n = n->next) {
        for (const RuleIDNode* rules = n->snode->ruleIDList; rules != nullptr;
             rules = rules->next) {
            Production* p = grammar.rules[rules->ruleID];
            insert_rhs_to_heads(p->nRHS_head, heads, theads);
        }
    }

    free_symbol_node_list(heads);

    // remove the dummy header of theads list.
    n = theads;
    theads = theads->next;
    free_symbol_node(n);

    if (DEBUG_GEN_PARSING_MACHINE) {
        yyprintf("==getTHeads: theads for: ");
        write_symbol_node_array(alpha);
        write_symbol_node_array(theads);
    }

    return theads;
}

/*
 * Helper function for getContext().
 * This section of code is called three times.
 */
void
get_context_do(Configuration* cfg, Context* context)
{
    SymbolNode* a = cfg->context->nContext;
    while (a != nullptr) {
        add_symbol2_context(a->snode, context);
        a = a->next;
    }
}

/*
 * Obtain the context for a configuration.
 */
void
get_context(Configuration* cfg, Context* context)
{
    SymbolList theads = nullptr;
    Production* production = grammar.rules[cfg->ruleID];

    if (cfg->marker == production->RHS_count - 1) {
        // is last symbol, just copy the context.
        get_context_do(cfg, context);
    } else { // need to find thead(alpha)
        if (SHOW_THEADS)
            write_configuration(*cfg);

        // alpha is the string after scanned symbol.
        SymbolList alpha =
          cfg->nMarker->next; // we know cfg->nMarker != nullptr.
        theads = get_theads(alpha);

        if (SHOW_THEADS) {
            show_t_heads(alpha, theads);
        }

        // if theads_count == 0, just copy the context.
        if (theads == nullptr) {
            get_context_do(cfg, context);
        } else { // theads_count > 0
            for (SymbolNode* a = theads; a != nullptr; a = a->next) {
                if (strlen(a->snode->symbol) == 0) { // empty string.
                    // Entire alpha vanishable. Copy context.
                    get_context_do(cfg, context);
                } else {
                    add_symbol2_context(a->snode, context);
                }
            } // end of for
        }
    } // end of if-else

    free_symbol_node_list(theads);
}

/*
 * Empty a context.
 * Note: if a is nullptr, free(a) causes crash.
 */
void
clear_context(Context* c)
{
    if (c == nullptr)
        return;

    c->context_count = 0;
    free_symbol_node_list(c->nContext);
    c->nContext = nullptr;
}

void
free_context(Context* c)
{
    if (c == nullptr)
        return;
    clear_context(c);
    free(c);
}

void
clear_production(Production* p)
{
    SymbolNode *a, *b;

    if (p == nullptr)
        return;
    if (p->nLHS != nullptr)
        free(p->nLHS);
    if (p->nRHS_head != nullptr) {
        a = p->nRHS_head;
        p->nRHS_head = nullptr;
        while (a != nullptr) {
            b = a->next;
            free_symbol_node(a);
            a = b;
        }
    }
}

void
free_production(Production* p)
{
    clear_production(p);
    free(p);
}

void
free_config(Configuration* c)
{
    if (c == nullptr)
        return;
    clear_context(c->context);
    free(c);
}

/*
 * Determine if productions p1 and p2 are the same.
 * Can be used for general production comparison.
 * In this program this is no longer used.
 * But can be used to find out if the grammar has
 * repeated rules etc.
 */
auto
is_same_production(const Production* p1, const Production* p2) -> bool
{
    if (p1->nLHS->snode != p2->nLHS->snode) {
        return false;
    }
    if (p1->RHS_count != p2->RHS_count) {
        return false;
    }

    const SymbolNode* a = p1->nRHS_head;
    const SymbolNode* b = p2->nRHS_head;

    while (a != nullptr) {
        if (a->snode != b->snode)
            return false;
        a = a->next;
        b = b->next;
    }
    return true;
}

/*
 * Determine if contexts c1 and c2 are the same.
 */
auto
is_same_context(const Context* c1, const Context* c2) -> bool
{
    if (c1->context_count != c2->context_count)
        return false;

    const SymbolNode* a = c1->nContext;
    const SymbolNode* b = c2->nContext;
    while (a != nullptr) {
        if (a->snode != b->snode)
            return false;
        a = a->next;
        b = b->next;
    }
    return true;
}

/*
 * Determine if configurations con and c are the same.
 */
auto
is_same_config(const Configuration* con, const Configuration* c) -> bool
{
    if (con->marker != c->marker)
        return false;
    if (con->ruleID != c->ruleID)
        return false;
    if (is_same_context(con->context, c->context) == false)
        return false;
    return true;
}

/*
 * Note that a successor config's marker = 0.
 */
auto
is_existing_successor_config(const State* s, int rule_id, const Context* con)
  -> bool
{
    for (int i = 0; i < s->config_count; i++) {
        const Configuration* c = s->config[i];
        if (c->marker == 0 && rule_id == c->ruleID &&
            is_same_context(con, c->context))
            return true; // existing config
    }
    return false;
}

void
add_successor_config_to_state(State* s, int rule_id, Context* con)
{
    if (s->config_count >= s->config_max_count - 1) {
        s->config_max_count *= 2;
        HYY_EXPAND(&s->config, s->config_max_count);

        // printf("addSuccessorConfigToState: state size expanded to %d\n",
        //       s->config_max_count);
    }

    // marker = 0, isCoreConfig = 0.
    Configuration* c = create_config(rule_id, 0, 0);
    c->owner = s;

    copy_context(c->context, con);

    s->config[s->config_count] = c;
    s->config_count++;
}

auto
is_final_configuration(const Configuration* c) -> bool
{
    if (c->marker == grammar.rules[c->ruleID]->RHS_count)
        return true;
    return false;
}

auto
is_empty_production(const Configuration* c) -> bool
{
    if (grammar.rules[c->ruleID]->RHS_count == 0)
        return true;
    return false;
}

/*
 * Used by addCoreConfig2State() only.
 * comparison is made alphabetically on:
 * 1) production,
 * 2) marker,
 * 3) context.
 * Actually, for the same state,
 * core configurations are different only
 * by production. But for the same state,
 * different core configuration comparison
 * will be sure to end in production comparison
 * -- either > or <. So does not matter to
 * add in the extra code to compare marker and
 * context. This function thus can be used
 * for general config comparison, although in
 * this program it's used only here.
 */
auto
config_cmp(const Configuration* c1, const Configuration* c2) -> int
{
    const Production* c1_production = grammar.rules[c1->ruleID];
    const Production* c2_production = grammar.rules[c2->ruleID];

    // printf("compare LHS\n");
    int cmp_val = strcmp(c1_production->nLHS->snode->symbol,
                         c2_production->nLHS->snode->symbol);
    if (cmp_val != 0)
        return cmp_val;

    // printf("compare RHS\n");
    int count = c1_production->RHS_count;
    if (count > c2_production->RHS_count) {
        count = c2_production->RHS_count;
    }
    const SymbolNode* a = c1_production->nRHS_head;
    const SymbolNode* b = c2_production->nRHS_head;
    for (int i = 0; i < count; i++) {
        cmp_val = strcmp(a->snode->symbol, b->snode->symbol);
        if (cmp_val != 0)
            return cmp_val;
        a = a->next;
        b = b->next;
    }

    cmp_val = c1_production->RHS_count - c2_production->RHS_count;
    if (cmp_val > 0) {
        return 1;
    } // c1 RHS is longer.
    if (cmp_val < 0) {
        return -1;
    } // c2 RHS is longer.

    // If productions are the same, go on to compare context.
    // printf("compare marker\n");
    cmp_val = c1->marker - c2->marker;
    if (cmp_val > 0) {
        return 1;
    }
    if (cmp_val < 0) {
        return -1;
    }

    // If production and marker are the same, go on to compare context.
    // printf("compare context\n");
    count = c1->context->context_count;
    if (count > c2->context->context_count) {
        count = c2->context->context_count;
    }

    a = c1->context->nContext;
    b = c2->context->nContext;
    while (a != nullptr) {
        cmp_val = strcmp(a->snode->symbol, b->snode->symbol);
        if (cmp_val != 0)
            return cmp_val;
        a = a->next;
        b = b->next;
    }

    // printf("compare context count\n");
    cmp_val = c1->context->context_count - c2->context->context_count;
    if (cmp_val > 0) {
        return 1;
    }
    if (cmp_val < 0) {
        return -1;
    }

    return 0; // the same
}

/*
 * Add core configurations in increasing order
 * The order is by production, and marker.
 * Assumption: a core config won't be inserted twice.
 */
void
add_core_config2_state(State* s, Configuration* new_config)
{
    if (s->config_count >= s->config_max_count - 1) {
        s->config_max_count *= 2;
        HYY_EXPAND(&s->config, s->config_max_count);
        // printf("addCoreConfig2State: state size expanded to %d\n",
        //       s->config_max_count);
    }

    int i = 0;
    for (; i < s->config_count; i++) {
        const int cmp_val = config_cmp(s->config[i], new_config);
        if (cmp_val == 0) {
            // a core config shouldn't be added twice.
            throw std::runtime_error(
              "add_core_config2_state: a core config shouldn't be added "
              "twice."); // should never happen.
        }
        if (cmp_val > 0)
            break; // found insertion point.
    }

    for (int j = s->config_count; j > i; j--) {
        s->config[j] = s->config[j - 1];
    }

    s->config[i] = new_config; // i == j is insert point.

    s->config_count++;
    s->core_config_count++;
}

#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE

/////////////////////////////////////////////////////
// Use config_queue when get closure for a state.
/////////////////////////////////////////////////////

/*
 * Note that a successor config's marker = 0.
 * Returns the index of the compatible config in the state.
 */
auto
is_compatible_successor_config(const State* s, int rule_id) -> int
{
    for (int i = 0; i < s->config_count; i++) {
        const Configuration* c = s->config[i];
        if (c->marker == 0 && rule_id == c->ruleID)
            return i; // existing compatible config
    }
    return -1;
}

/*
 * Assumption: public variable config_queue contains
 * the configurations to be processed.
 */
void
get_config_successors(State* s)
{
    static Context tmp_context;
    tmp_context.nContext = nullptr;

    while (queue_count(config_queue) > 0) {
        Configuration* config = s->config[queue_pop(config_queue)];

        if (config->marker >= 0 &&
            config->marker < grammar.rules[config->ruleID]->RHS_count) {
            SymbolTblNode* scanned_symbol = get_scanned_symbol(config);

            if (is_non_terminal(scanned_symbol)) {

                clear_context(&tmp_context); // clear tmp_context
                get_context(config, &tmp_context);

                for (const RuleIDNode* r = scanned_symbol->ruleIDList;
                     r != nullptr;
                     r = r->next) {
                    // Grammar.rules[r->ruleID] starts with this scanned symbol.

                    // If not an existing config, add to state s.
                    const int index =
                      is_compatible_successor_config(s, r->ruleID);

                    if (index == -1) { // new config.
                        add_successor_config_to_state(
                          s, r->ruleID, &tmp_context);
                        queue_push(config_queue, s->config_count - 1);

                    } else if (combine_context(s->config[index]->context,
                                               &tmp_context) ==
                               true) { // compatible config
                        // if this config has no successor, don't insert to
                        // config_queue. This saves time.
                        // marker = 0 here, no need to check marker >= 0.
                        if (is_final_configuration(s->config[index]))
                            continue;
                        if (is_terminal(get_scanned_symbol(s->config[index])) ==
                            true)
                            continue;
                        if (queue_exist(config_queue, index) == 1)
                            continue;

                        // else, insert to config_queue.
                        queue_push(config_queue, index);
                    }
                    // else { // same config, do nothing. }

                } // end for
            }     // else, is a terminal, stop.
        }         // end if config-marker >= 0 ...
    }             // end of while
}

void
get_closure(State* s)
{
    int i;
    // queue_clear(config_queue);
    for (i = 0; i < s->config_count; i++) {
        queue_push(config_queue, i);
    }
    get_config_successors(s);
}

#else

//////////////////////////////////////////////////////
// Not use config_queue, combine compatible
// configurations after they are all generated.
// This is much slower, and is used for testing only.
//////////////////////////////////////////////////////

void
getSuccessorForConfig(State* s, Configuration* config)
{
    RuleIDNode* r;
    SymbolTblNode* scanned_symbol = nullptr;
    static Context tmp_context;
    tmp_context.nContext = nullptr;

    if (config->marker >= 0 &&
        config->marker < grammar.rules[config->ruleID]->RHS_count) {
        scanned_symbol = getScannedSymbol(config);

        if (isNonTerminal(scanned_symbol)) {

            clearContext(&tmp_context); // clear tmp_context
            getContext(config, &tmp_context);

            for (r = scanned_symbol->ruleIDList; r != nullptr; r = r->next) {
                // Grammar.rules[r->ruleID] starts with this scanned symbol.

                // If not an existing config, add to state s.
                if (isExistingSuccessorConfig(s, r->ruleID, &tmp_context) ==
                    false) {
                    addSuccessorConfigToState(s, r->ruleID, &tmp_context);
                }

            } // end for
        }     // else, is a terminal, stop.
    }         // end if config-marker >= 0 ...
}

/*
 * To combine compatible configurations of the given state.
 *
 * Compatible configurations are those that have the same
 * production and marker, but different in contexts.
 */
void
combineCompatibleConfig(State* s)
{
    int i, j;
    Configuration* c;
    if (s == nullptr) {
        // printf("combineCompatibleConfig: warning: s is nullptr\n");
        return;
    }
    if (s->config_count <= 1)
        return;

    if (DEBUG_COMB_COMP_CONFIG) {
        yyprintf("combineCompatibleCfg (state %d). before: %d, ",
                 s->state_no,
                 s->config_count);
        // writeState(s);
    }

    for (i = 1; i < s->config_count; i++) {
        c = s->config[i];
        if (c == nullptr)
            continue;
        for (j = 0; j < i; j++) {
            if (isCompatibleConfig(c, s->config[j])) {
                combine_context(s->config[j]->context, c->context);
                freeConfig(c); // combine config i to j, then remove i.
                s->config[i] = nullptr;
                break;
            } // end if
        }     // end for
    }         // end for

    // shrink state configs to remove those nullptr ones
    // after the last step.
    i = 0;
    j = 0;
    while (j < s->config_count) {
        while (s->config[j] == nullptr) {
            j++;
        }
        if (j >= s->config_count)
            break;
        s->config[i] = s->config[j];
        j++;
        i++;
    }
    s->config_count = i;

    if (DEBUG_COMB_COMP_CONFIG) {
        yyprintf("after: %d\n", s->config_count);
        // writeState(s);
    }
}

/*
 * xx - no. of calls to getClosure,
 * yy - number of config in all states before combine,
 * zz - max config count in all states before combine.
 * yyy - number of config in all stetes after combine,
 * zzz - max config count in all states after combine.
 */
int xx, yy, zz;
int yyy, zzz;

/*
 * Get the configuration closure of a state s given its
 * core configuratins. Called by generate_parsing_machine().
 *
 * Algorithm:
 * For each config in the state:
 *   If it is NOT a final config, then
 *     Get scanned symbol s.
 *     If s is a Non-Terminal, then
 *       Get context c (*).
 *       For each rule r in the grammar whose RHS starts with s,
 *         Combine r and c to get the next possible config.
 *         If this new config already exists in the state, stop.
 *         Else, add this new config to the end of current state.
 *
 * NOTE *: This is found by get thead(alpha), where alpha is the
 *         string following scanned symbol s. If thead(alpha) is
 *         empty, then use the context of current config.
 */
void
getClosure(State* s)
{
    int i;

    for (i = 0; i < s->config_count; i++) {
        getSuccessorForConfig(s, s->config[i]);
    }

    xx++;
    yy += s->config_count;
    if (zz < s->config_count)
        zz = s->config_count;

    if (USE_COMBINE_COMPATIBLE_CONFIG)
        combineCompatibleConfig(s);

    yyy += s->config_count;
    if (zzz < s->config_count)
        zzz = s->config_count;
}

#endif

///////////////////////////////////////////
// StateCollection functions. START.
///////////////////////////////////////////

auto
create_state_collection() -> StateCollection*
{
    auto* c = new StateCollection;
    if (c == nullptr)
        YYERR_EXIT("createStateCollection error: out of memory\n");

    c->states_head = nullptr;
    c->states_tail = nullptr;
    c->state_count = 0;

    return c;
}

void
destroy_state_collection(StateCollection* c)
{
    if (c == nullptr)
        return;

    State* s = c->states_head;
    while (s != nullptr) {
        State* next = s->next;
        State::destroy_state(s);
        s = next;
    }

    delete c;
}

auto
StateCollection::add_state2(State* new_state) -> State*
{
    if (new_state == nullptr)
        return nullptr;

    new_state->next = nullptr;

    if (this->state_count == 0) {
        this->states_head = new_state;
        this->states_tail = new_state;
    } else {
        this->states_tail->next = new_state;
        this->states_tail = new_state;
    }
    this->state_count++;

    return this->states_tail;
}

///////////////////////////////////////////
// StateCollection functions. END.
///////////////////////////////////////////

/*
 * Get the scanned symbol of configuration c.
 * The scanned symbol can be obtained by nMarker pointer
 * as here, or by marker which needs more calculation.
 */
auto
get_scanned_symbol(const Configuration* c) -> SymbolTblNode*
{
    if (c->nMarker == nullptr)
        return nullptr;
    return c->nMarker->snode;
}

/*
 * Used by function transition.
 * Returns the successor state that is the result of
 * transition following the given symbol.
 */
auto
find_state_for_scanned_symbol(const StateCollection* c,
                              const SymbolTblNode* symbol) -> State*
{
    if (c == nullptr)
        return nullptr;

    State* s = c->states_head;
    while (s != nullptr) {
        if (symbol == s->trans_symbol->snode)
            return s;
        s = s->next;
    }

    return nullptr;
}

auto
create_context() -> Context*
{
    Context* c;
    c = new Context;
    c->nContext = nullptr;
    c->context_count = 0;
    c->next = nullptr; // used by LR(k) only.
    return c;
}

auto
create_config(int rule_id, int marker, int is_core_config) -> Configuration*
{
    auto* c = new Configuration;
    if (c == nullptr) {
        throw std::runtime_error("create_config error: out of memory");
    }

    c->context = create_context();

    c->ruleID = rule_id;
    c->marker = marker;
    c->isCoreConfig = is_core_config;

    c->nMarker = nullptr;
    if (rule_id >= 0)
        c->nMarker = grammar.rules[rule_id]->nRHS_head;

    c->owner = nullptr;
    if (USE_LALR) {
        c->ORIGINATOR_CHANGED = false;
        c->COMPLETE = 0;
        c->IN_LANE = 0;
        c->LANE_END = 0;
        c->LANE_CON = 0;
        c->CONTEXT_CHANGED = 0;
        c->originators = create_originator_list();
        c->transitors = create_originator_list();
    }

    return c;
}

void
copy_context(Context* dest, const Context* src)
{
    dest->context_count = src->context_count;

    dest->nContext = nullptr;
    if (src->nContext != nullptr) {
        SymbolNode* a = src->nContext;
        SymbolNode* b = dest->nContext = create_symbol_node(a->snode);
        while ((a = a->next) != nullptr) {
            b->next = create_symbol_node(a->snode);
            b = b->next;
        }
    }
}

/*
 * return the copy of a config.
 * used by function transition when creating new state.
 */
void
copy_config(Configuration* c_dest, const Configuration* c_src)
{
    c_dest->marker = c_src->marker;
    c_dest->isCoreConfig = c_src->isCoreConfig;
    c_dest->ruleID = c_src->ruleID;
    c_dest->nMarker = c_src->nMarker;
    copy_context(c_dest->context, c_src->context);
}

/////////////////////////////////////////////////////////
// Functions for combining compatible states. START.
/////////////////////////////////////////////////////////

/*
 * Two configurations are common core configurations if
 * they have the same production and marker, but
 * DIFFERENT contexts.
 */
auto
is_common_config(const Configuration* con, const Configuration* c) -> bool
{
    if (con->marker != c->marker)
        return false;
    if (con->ruleID != c->ruleID)
        return false;
    // If all same, then are same config, not common config!
    if (is_same_context(con->context, c->context))
        return false;
    return true;
}

/*
 * Pre-assumption: s1, s2 have at least one core config.
 * Returns true if at least one config pair have common config.
 */
auto
has_common_core(const State* s1, const State* s2) -> bool
{
    if (s1 == nullptr || s2 == nullptr)
        return false;
    // printf("hasCommonCore: \n");

    bool result = false;
    if (s1->core_config_count != s2->core_config_count)
        return false;
    if (s1->core_config_count == 0)
        return false;
    for (int i = 0; i < s1->core_config_count; i++) {
        if (is_same_config(s1->config[i], s2->config[i])) {
            // do nothing.
        } else if (is_common_config(s1->config[i], s2->config[i])) {
            result = true;
        } else { // not same, and not common core.
            return false;
        }
    }
    return result;
}

/*
 * Pre-assumption: contexts c1 and c2 are sorted in increasing order.
 * See function addSymbol2Context().
 */
auto
has_empty_intersection(const Context* c1, const Context* c2) -> bool
{
    const SymbolNode* a = c1->nContext;
    const SymbolNode* b = c2->nContext;

    // intrinsically this is O(m + n).
    while (a != nullptr && b != nullptr) {
        if (a->snode == b->snode)
            return false; // common element found
        if (strcmp(a->snode->symbol, b->snode->symbol) < 0) {
            a = a->next;
        } else {
            b = b->next;
        }
    }

    return true;
}

/*
 * condition (a).
 * if (a) is satisfied, return true, otherwise false.
 */
auto
is_compatible_state_a(const State* s1, const State* s2) -> bool
{
    int count = s1->core_config_count;
    for (int i = 0; i < count; i++) {
        for (int j = 0; j < count; j++) {
            if (i != j) {
                const Context* c1 = s1->config[i]->context;
                const Context* c2 = s2->config[j]->context;
                if (has_empty_intersection(c1, c2) == false) {
                    return false;
                }
            }
        }
    }
    return true;
}

/*
 * condition (b) or (c).
 * if (b) or (c) is satisfied, return true, otherwise false.
 */
auto
is_compatible_state_bc(const State* s) -> bool
{
    int count = s->core_config_count;
    for (int i = 0; i < count - 1; i++) {
        for (int j = i + 1; j < count; j++) {
            const Context* c1 = s->config[i]->context;
            const Context* c2 = s->config[j]->context;
            if (has_empty_intersection(c1, c2)) {
                return false;
            } // end if
        }     // end for
    }         // end for
    return true;
}

/*
 * Pre-assumption:
 *   s1 and s2 have at least one core configuration.
 */
auto
is_compatible_states(const State* s1, const State* s2) -> bool
{

    if (has_common_core(s1, s2) == false)
        return false;

    const int count = s1->core_config_count;
    if (count == 1)
        return true;

    // now check context to see if s1 and s2 are compatible.
    if (is_compatible_state_a(s1, s2))
        return true;
    if (is_compatible_state_bc(s1))
        return true;
    if (is_compatible_state_bc(s2))
        return true;

    return false;
}

/*
 * Used by combineCompatibleStates() and propagateContextChange().
 */
void
update_state_parsing_tbl_entry(const State* s)
{
    for (int i = 0; i < s->config_count; i++) {
        SymbolTblNode* scanned_symbol = get_scanned_symbol(s->config[i]);

        // for final config and empty reduction.
        if (is_final_configuration(s->config[i]) ||
            strlen(scanned_symbol->symbol) == 0) {
            insert_reduction_to_parsing_table(s->config[i], s->state_no);
        }
    }
}

/*
 * Used by propagateContextChange() and push_state_context_change().
 *
 * Find a similar core config for c in State t.
 *
 * similar core config:
 * - production and marker are the same.
 * - Context does not matter.
 */
auto
find_similar_core_config(const State* t,
                         const Configuration* c,
                         int* config_index) -> Configuration*
{
    for (int i = 0; i < t->core_config_count; i++) {
        Configuration* tmp = t->config[i];

        // don't compare context, it'll be compared in combine_context().
        if (tmp->marker == c->marker && tmp->ruleID == c->ruleID) {
            (*config_index) = i;
            return tmp;
        }
    }
    return nullptr;
}

/*
 * For each successor state s, update context of core
 * configurations, then get closure, add change to
 * parsing table. Do this recursively.
 *
 * In more detail:
 * For each successor state t of s:
 *   look through each config c in the config list of s:
 *     if scanned symbol of c == trans_symbol of t {
 *       // c transits to a core config in t
 *       find core config d in t which is the transition
 *       result of c,
 *       update the context of d from c.
 *       if the context of d is changed {
 *         get closure of d again,
 *         update parsing table, and
 *         propagate context change to d's successors.
 *     }
 */
void
propagate_context_change(const State* s)
{
    State* t = nullptr; // successor.
    int config_index = 0;

    if (s->successor_count == 0)
        return;

    for (int i = 0; i < s->successor_count; i++) {
        t = s->successor_list[i];
        const SymbolTblNode* trans_symbol = t->trans_symbol->snode;
        bool is_changed = false;

        for (int j = 0; j < s->config_count; j++) {
            Configuration* c = s->config[j]; // bug removed: i -> j. 3-4-2008.
            if (is_final_configuration(c))
                continue;
            // if not a successor on symbol, next.
            if (trans_symbol != get_scanned_symbol(c))
                continue;

            c->marker += 1;
            const Configuration* d =
              find_similar_core_config(t, c, &config_index);
            c->marker -= 1;
            if (d == nullptr)
                continue;

            if (combine_context(d->context, c->context)) {
                // printf("context of state %d updated\n", t->state_no);
                is_changed = true;

#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
                // queue_clear(config_queue);
                queue_push(config_queue, config_index);
                get_config_successors(t);
#else
                getSuccessorForConfig(t, d);
#endif
            }
        } // end of for

        if (is_changed) {
#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
#else
            combineCompatibleConfig(t);
#endif
            update_state_parsing_tbl_entry(t);
            propagate_context_change(t);
        }
    } // end of for
}

/*
 * Assumption: s_dest and s_src have common core,
 *   and are weakly compatible.
 * NOTE: s_dest is from states_new.
 */
auto
combine_compatible_states(State* s_dest, const State* s_src) -> bool
{
    bool is_changed = false;
    for (int i = 0; i < s_dest->core_config_count; i++) {
        if (combine_context(s_dest->config[i]->context,
                            s_src->config[i]->context)) {
            is_changed = true;

#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
            // queue_clear(config_queue);
            queue_push(config_queue, i);
            get_config_successors(s_dest);
#else
            getSuccessorForConfig(s_dest, s_dest->config[i]);
#endif
        }
    }

    // Now propagate the context change to successor states.
    if (is_changed) {
#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
#else
        combineCompatibleConfig(s_dest);
#endif

        update_state_parsing_tbl_entry(s_dest);
        propagate_context_change(s_dest);
    }
    return is_changed;
}

/////////////////////////////////////////////////////////
// Functions for combining compatible states. END.
/////////////////////////////////////////////////////////

/*
 * Determine if two states are the same.
 * This is done by checking if the two states have the
 * same core configurations.
 * Since the core confiurations are both in increasing order,
 * just compare them by pairs.
 */
auto
is_same_state(const State* s1, const State* s2) -> bool
{
    if (s1->core_config_count != s2->core_config_count) {
        return false;
    }

    for (int i = 0; i < s1->core_config_count; i++) {
        if (is_same_config(s1->config[i], s2->config[i]) == false)
            return false;
    }

    return true;
}

/*
 * Determine if a state exists in a state collection.
 * This is done by comparing s with each state in sc.
 *
 * NOTE: combining compatible states is done here!
 */
auto
is_existing_state(const StateCollection* sc, const State* s, int* is_compatible)
  -> State*
{
    State* t = sc->states_head;
    int i = 0; // index of state.

    while (t != nullptr) {
        if (is_same_state(t, s)) {
            return t;
        }
        if (USE_COMBINE_COMPATIBLE_STATES) {
            if (is_compatible_states(t, s)) {
                combine_compatible_states(t, s);
                (*is_compatible) = 1;
                return t;
            }
        }
        i++;
        t = t->next;
    }

    return nullptr;
}

auto
create_state() -> State*
{
    State* s = new State;
    if (s == nullptr) {
        throw std::runtime_error("create_state error: out of memory\n");
    }
    s->next = nullptr;
    s->config_max_count = STATE_INIT_SIZE;
    s->config = new Configuration*[s->config_max_count];
    s->config_count = 0;
    s->state_no = -1;
    s->trans_symbol = create_symbol_node(hash_tbl_find(""));
    s->core_config_count = 0;

    // Initialization for successor list.
    s->successor_max_count = STATE_SUCCESSOR_INIT_MAX_COUNT;
    s->successor_list = new State*[s->successor_max_count];
    if (s->successor_list == nullptr) {
        YYERR_EXIT("createState error: out of memory\n");
    }
    s->successor_count = 0;

    s->parents_list = StateList::create();

    s->PASS_THRU = 0;
    s->REGENERATED = 0;

    return s;
}

void
insert_reduction_to_parsing_table(const Configuration* c, int state_no)
{
    if (grammar.rules[c->ruleID]->nLHS->snode ==
        grammar.goal_symbol->snode) { // accept, action = "a";
        for (const SymbolNode* a = c->context->nContext; a != nullptr;
             a = a->next) {
            insert_action(a->snode, state_no, CONST_ACC);
        }
    } else { // reduct, action = "r";
        for (const SymbolNode* a = c->context->nContext; a != nullptr;
             a = a->next) {
            insert_action(a->snode, state_no, (-1) * c->ruleID);
        }
    }
}

/*
 * Input:
 * s - src state, n - new state.
 * Add n to the successor list of s.
 */
void
add_successor(State* s, State* n)
{
    s->successor_list[s->successor_count] = n;
    s->successor_count++;

    // printf(":: state %d, succesor is state %d on symbol %s\n",
    //    s->state_no, n->state_no, n->trans_symbol);

    if (s->successor_count >= s->successor_max_count) {
        s->successor_max_count *= 2; // there won't be many
        delete[] s->successor_list;
        s->successor_list = new State*[s->successor_max_count];
        if (s->successor_list == nullptr) {
            YYERR_EXIT("addSuccessor error: out of memory\n");
        }
        // printf("Successor list of State %d is expanded to %d\n",
        //        s->state_no, s->successor_max_count);
    }

    if (USE_LALR) {
        // add s to the parents_list of n. To get originators in lane-tracing.
        n->parents_list->add(s);
    }
}

/*
 * Insert a new state to the parsing machine.
 *
 * Used in 3 places: y.c, lr0.c, lane_tracing.c.
 * This can be changed to macro later.
 */
void
insert_state_to_pm(State* s)
{
    s->state_no = states_new->state_count;

    states_new->add_state2(s);
    add_state_to_state_array(*states_new_array, s);

    // expand size of parsing table array if needed.
    if (states_new->state_count >= PARSING_TABLE_SIZE) {
        expand_parsing_table();
    }
}

/*
 * For each of the new temp states,
 *   If it is not one of the existing states in
 *     states_new, then add it to states_new.
 *   Also always add the transition to parsing table.
 *
 * Note:
 * The "is_compatible" variable is for this situation:
 *
 * state s_x is a successor of state s_0.
 * in function isExistingState, s_0 is already in
 * states_new, but s_x is not.
 * now s_0 is in coll too, and when calling function
 * isExistingState, new s_0 is combined to old s_0,
 * which causes propagation of new context to successors
 * of s_0. However this does not include s_x since it's
 * inserted into states_new only after that.
 * The following code ensures that the propagation is
 * performed again over all the states.
 *
 */
auto
add_transition_states2_new(StateCollection* coll, State* src_state) -> bool
{
    bool src_state_changed = false;
    State* s = coll->states_head;

    while (s != nullptr) {
        State* next = s->next;
        // is_compatible = 0;

        // Two alternatives. search_state_hash_tbl is slightly faster.
        // if ((os = isExistingState(states_new, s, & is_compatible)) ==
        // nullptr) {
        int is_compatible = 0;
        State* os = search_state_hash_tbl(s, &is_compatible);
        if (os == nullptr) {
            insert_state_to_pm(s);

            // Add this new state as successor to src_state.
            add_successor(src_state, s);

            // insert shift.
            insert_action(
              s->trans_symbol->snode, src_state->state_no, s->state_no);

        } else { // same or compatible with an existing state.
            // insert shift.
            insert_action(
              os->trans_symbol->snode, src_state->state_no, os->state_no);

            add_successor(src_state, os);

            State::destroy_state(s); // existing or compatible. No use.

            // This should only happen for general practical method.
            if (os == src_state && is_compatible == 1) {
                src_state_changed = true;
                // printf("src state is changed\n");
            }
        }

        s = next;
    } // end of while.

    return src_state_changed;
}

/*
 * Perform transition opertaion on a state to get successors.
 *
 * For each config c in the state s,
 *   If c is a final config, stop
 *   Else, get the scanned symbol x,
 *     If a new temp state for x does not exist yet, create it.
 *     add x to the temp state.
 * (Now get several new temp states)
 * Add these new temp states to states_new if not existed,
 * and add transition to parsing table as well.
 */
void
transition(State* s)
{
    StateCollection* coll = create_state_collection();

    for (int i = 0; i < s->config_count; i++) {
        Configuration* c = s->config[i];
        if (is_final_configuration(c)) {
            // yyprintf("a final config. so reduce.\n");
            // writeConfiguration(c);
            insert_reduction_to_parsing_table(c, s->state_no);
        } else { // do transit operation.
            SymbolTblNode* scanned_symbol = get_scanned_symbol(c);
            if (strlen(scanned_symbol->symbol) ==
                0) { // insert empty reduction.
                insert_reduction_to_parsing_table(c, s->state_no);
                continue;
            }
            State* new_state =
              find_state_for_scanned_symbol(coll, scanned_symbol);
            if (new_state == nullptr) {
                new_state = create_state();
                // record which symbol this state is a successor by.
                new_state->trans_symbol = create_symbol_node(scanned_symbol);
                coll->add_state2(new_state);
            }
            // create a new core config for new_state.
            Configuration* new_config = create_config(-1, 0, 1);

            new_config->owner = new_state;
            copy_config(new_config, c);
            new_config->isCoreConfig = 1;
            new_config->marker++;
            if (new_config->nMarker != nullptr)
                new_config->nMarker = new_config->nMarker->next;

            add_core_config2_state(new_state, new_config);
        }
    } // end for

    if (coll->state_count > 0) {
        bool src_state_changed = add_transition_states2_new(coll, s);
        if (src_state_changed) {
            propagate_context_change(s);
        }
    }
}

auto
is_compatible_config(Configuration* c1, Configuration* c2) -> bool
{
    if (c1 == nullptr || c2 == nullptr)
        return false;
    if (c1->marker != c2->marker)
        return false;
    if (c1->ruleID != c2->ruleID)
        return false;
    return true;
}

/*
 * Combine c_src into c_dest by copying symbols that
 * occur in c_src but not in c_dest to c_dest.
 *
 * Returns true if any change is made.
 */
auto
combine_context(Context* c_dest, Context* c_src) -> bool
{
    bool is_changed = false;
    if (c_dest == nullptr || c_src == nullptr)
        return false;

    for (SymbolNode* a = c_src->nContext; a != nullptr; a = a->next) {
        if (add_symbol2_context(a->snode, c_dest)) {
            is_changed = true;
        }
    }

    return is_changed;
}

/*
 * The main function to generate parsing machine.
 */
void
generate_parsing_machine()
{
    State* new_state = states_new->states_head;

    if (DEBUG_GEN_PARSING_MACHINE) {
        yyprintf("\n\n--generate parsing machine--\n");
    }

    while (new_state != nullptr) {
        if (DEBUG_GEN_PARSING_MACHINE) {
            yyprintf("%d states, current state is %d\n",
                     states_new->state_count,
                     new_state->state_no);
        }

        get_closure(new_state); // get closure of this state.

        // get successor states and add them to states_new.
        transition(new_state);

        new_state = new_state->next; // point to next unprocessed state.
    }

    ParsingTblRows = states_new->state_count;
    n_state_opt1 = states_new->state_count;
}

void
dump_state_collections()
{
    write_state_collection(states_new);
}

/////////////////////////////////////////////////////////////////
// Parsing table functions.
/////////////////////////////////////////////////////////////////

/*
 * Three places the parsing table is directly manipulated:
 * 1) updateDestState, 2) getAction, 3) insertAction.
 */

/*
 * Use a one-dimension array to store the matrix and
 * calculate the row and column number myself:
 * row i, col j is ParsingTable[col_no * i + j];
 *
 * Here we have:
 * 0 <= i < total states count
 * 0 <= j < col_no
 */
void
init_parsing_table()
{
    PARSING_TABLE_SIZE = PARSING_TABLE_INIT_SIZE;
    size_t total_cells = PARSING_TABLE_SIZE * ParsingTblCols;
    auto* ParsingTable = new int[total_cells];
    memset((void*)ParsingTable, 0, 4 * total_cells);
}

void
expand_parsing_table()
{
    size_t total_cells = PARSING_TABLE_SIZE * ParsingTblCols;
    HYY_EXPAND(&ParsingTable, 2 * total_cells);
    memset((void*)(ParsingTable + total_cells), 0, 4 * total_cells);
    PARSING_TABLE_SIZE *= 2;

    // states_new_array->expand(PARSING_TABLE_SIZE); // useless now

    // yyprintf("expandParsingTable message: ");
    // yyprintf("expand parsing table size to %d\n", PARSING_TABLE_SIZE);
}

/*
 * Given a state and a transition symbol, find the
 * action and the destination state.
 *
 * row - source state.
 *
 * Results are stored in variables action and state_dest.
 */
void
get_action(symbol_type symbol_type,
           int col,
           int row,
           char* action,
           int* state_dest)
{
    int x = ParsingTable[row * ParsingTblCols + col];

    if (x == 0) {
        *action = 0;
        *state_dest = 0;
        return;
    }
    if (x > 0) { // 's' or 'g'
        if (symbol_type == symbol_type::TERMINAL) {
            *action = 's';
        } else if (symbol_type == symbol_type::NONTERMINAL) {
            *action = 'g';
        } else {
            using std::to_string;
            throw std::runtime_error(
              std::string("getAction error: unknown symbol type ") +
              to_string(static_cast<int>(symbol_type)) +
              ", col:" + to_string(col));
        }
        *state_dest = x;
        return;
    }
    if (x == CONST_ACC) { // 'a'
        *action = 'a';
        *state_dest = 0;
        return;
    } // x < 0, 'r'
    *action = 'r';
    *state_dest = (-1) * x;
    return;
}

/*
 * Inserts an action into the parsing table.
 * Input variables include:
 *   state_src - given state.
 *   symbol - transition symbol.
 *   action - action on this symbol at this state.
 *   state_dest - destination state of the action.
 *
 * Actions can be:
 *   r - reduce, state_dest < 0
 *   s - shift,  state_dest > 0
 *   a - accept, state_dest == CONST_ACC
 *   g - goto.   state_dest > 0
 *
 * This is one of the 3 places updating the ParsingTable
 * array directly. The 2nd place is function updateAction().
 * The 3rd place is in the macro
 *   clearStateTerminalTransitions(state_no)
 * in lane_tracing.c.
 */
void
insert_action(SymbolTblNode* lookahead, int row, int state_dest)
{
    int reduce = 0, shift = 0; // for shift/reduce conflict.
    struct TerminalProperty *tp_s = nullptr, *tp_r = nullptr;

    int cell = row * ParsingTblCols + get_col(lookahead);

    if (ParsingTable[cell] == 0) {
        ParsingTable[cell] = state_dest;
        return;
    }

    if (ParsingTable[cell] == state_dest)
        return;

    // ParsingTable[cell] != 0 && ParsingTable[cell] != state_dest.
    // The following code process shift/reduce and reduce/reduce conflicts.

    if (ParsingTable[cell] == CONST_ACC || state_dest == CONST_ACC) {
        if (USE_LR0 && (ParsingTable[cell] < 0 || state_dest < 0)) {
            ParsingTable[cell] = CONST_ACC; // ACC wins over reduce.
            return;
        }
        throw std::runtime_error(
          std::string("warning: conflict between ACC and an action: ") +
          std::to_string(ParsingTable[cell]) + ", " +
          std::to_string(state_dest));
    }

    // printf("conflict (state %d, lookahead %s): %d v.s. %d\n",
    //        row, lookahead->symbol, ParsingTable[cell], state_dest);

    // reduce/reduce conflict, use the rule appears first.
    // i.e., the ruleID is smaller, or when negated, is bigger.
    if (ParsingTable[cell] < 0 && state_dest < 0) {
        // printf("r/r conflict: [%d, %s] - %d v.s. %d\n",
        //        row, lookahead->symbol, ParsingTable[cell], state_dest);
        Conflict* c =
          add_to_conflict_array(row, lookahead, ParsingTable[cell], state_dest);

        if (state_dest > ParsingTable[cell])
            ParsingTable[cell] = state_dest;

        if (c != nullptr) {
            c->decision = ParsingTable[cell];
        }

        // include r/r conflict for inadequate states.
        if (USE_LALR) {
            add_state_no_array(states_inadequate, row);
        }

        return;
    }

    // shift/shift conflict.
    if (ParsingTable[cell] > 0 && state_dest > 0) {
        if (SHOW_SS_CONFLICTS) {
            std::cerr << "warning: shift/shift conflict: " << state_dest
                      << " v.s. " << ParsingTable[cell] << " @ (" << row << ", "
                      << lookahead->symbol << ")" << std::endl;
        }
        // exit(1);
        // ParsingTable[cell] = state_dest; // change causes infinite loop.
        ss_count++;
        return;
    }

    if (ParsingTable[cell] < 0) {
        reduce = ParsingTable[cell];
        shift = state_dest;
    } else {
        reduce = state_dest;
        shift = ParsingTable[cell];
    }

    tp_s = lookahead->TP;
    if (grammar.rules[(-1) * reduce]->lastTerminal != nullptr)
        tp_r = grammar.rules[(-1) * reduce]->lastTerminal->TP;

    if (tp_s == nullptr || tp_r == nullptr || tp_s->precedence == 0 ||
        tp_r->precedence == 0) {
        // printf("s/r conflict: [%d, %s] - %d v.s. %d\n",
        //        row, lookahead->symbol, ParsingTable[cell], state_dest);
        Conflict* c =
          add_to_conflict_array(row, lookahead, ParsingTable[cell], state_dest);

        ParsingTable[cell] = shift; // use shift over reduce by default.
        // printf("default using %d\n", ParsingTable[cell]);

        if (c != nullptr) {
            c->decision = ParsingTable[cell];
        }

        // include s/r conflicts not handled by precedence/associativity.
        if (USE_LALR) {
            add_state_no_array(states_inadequate, row);
        }

        return;
    }

    if (tp_r->precedence > tp_s->precedence ||
        (tp_r->precedence == tp_s->precedence &&
         tp_r->assoc == associativity::LEFT)) {
        ParsingTable[cell] = reduce;
        // printf("resolved by using %d\n", ParsingTable[cell]);
        return;
    }

    // include s/r conflicts not handled by precedence/associativity.
    if (USE_LALR) {
        add_state_no_array(states_inadequate, row);
    }

    ParsingTable[cell] = shift;
    // printf("resolved by using %d\n", ParsingTable[cell]);
}

auto
is_goal_symbol(const SymbolTblNode* snode) -> bool
{
    if (snode == grammar.goal_symbol->snode)
        return true;
    return false;
}

void
print_parsing_table_note()
{
    yyprintf("Note: \n");
    yyprintf("1. si means shift and stack state i\n");
    yyprintf("2. ri means reduce by production numbered i\n");
    yyprintf("3. a0 means accept\n");
    yyprintf("4. gi means go to state i\n");
    yyprintf("5. 0 means error\n");
}

/*
 * Print the parsing table after LR(1) parsing machine
 * is generated, by before removing unit productions.
 *
 * Note: The value of variable ParsingTblRows is
 * assigned at the end of function generate_parsing_table().
 *
 * Parsing table: Ref. Aho&Ullman p219.
 */
void
print_parsing_table()
{
    int row_size = ParsingTblRows;
    int col_size = ParsingTblCols;

    yyprintf("\n--Parsing Table--\n");
    yyprintf("State\t");
    write_parsing_table_col_header();

    for (int row = 0; row < row_size; row++) {
        yyprintf("%d\t", row);
        for (int col = 0; col < ParsingTblCols; col++) {
            const SymbolTblNode* n = ParsingTblColHdr[col];
            if (is_goal_symbol(n) == false) {
                char action = 0;
                int state = 0;
                get_action(n->type, col, row, &action, &state);
                yyprintf("%c%d\t", action, state);
            }
        }
        yyprintf("\n");
    }

    print_parsing_table_note();
}

/*
 * Create a state based on the goal production (first
 * production of grammmar) and insert it to states_new.
 *
 * Called by function init() only.
 *
 * Assumption: grammar.rules[0] is the goal production.
 */
void
init_start_state()
{
    int is_compatible = 0;
    State* state0 = create_state();
    state0->config_count = 1;
    state0->core_config_count = 1;
    state0->state_no = 0;

    // ruleID = 0, marker = 0, isCoreConfig = 1.
    state0->config[0] = create_config(0, 0, 1);

    state0->config[0]->owner = state0;
    state0->config[0]->context->context_count = 1;
    hash_tbl_insert(strEnd);
    state0->config[0]->context->nContext =
      create_symbol_node(hash_tbl_find(strEnd));

    // writeState(state0);

    states_new->add_state2(state0);
    add_state_to_state_array(*states_new_array, state0);

    // insert to state hash table as the side effect of search.
    search_state_hash_tbl(state0, &is_compatible);
}

/*
 * Get a list of those states in the ParsingTable whose only
 * actions are a single reduction. Such states are called
 * final states.
 *
 * Use final state default reduction in the hyaccpar parse engine.
 * This significantly decreases the size of the generated parser
 * and overcomes the problem of always need to get the new lookahead
 * token to proceed parsing. Array final_state_list is
 * used in gen_compiler.c, and function writeParsingTblRow() of y.c.
 */
void
get_final_state_list()
{
    SymbolTblNode* n = nullptr;

    final_state_list = new int[ParsingTblRows];
    for (size_t i = 0; i < ParsingTblRows; i++) {
        final_state_list[i] = 0;
    }

#if USE_REM_FINAL_STATE

    if (USE_REMOVE_UNIT_PRODUCTION) {

        for (int i = 0; i < ParsingTblRows; i++) {
            if (is_reachable_state(i) == false)
                continue;

            int row_start = i * ParsingTblCols;
            int action = 0, new_action = 0;
            int j = 0;
            for (; j < ParsingTblCols; j++) {
                n = ParsingTblColHdr[j];

                if (is_goal_symbol(n) || is_parent_symbol(n))
                    continue;

                new_action = ParsingTable[row_start + j];
                if (new_action > 0 || new_action == CONST_ACC)
                    break;
                if (new_action == 0)
                    continue;
                if (action == 0)
                    action = new_action;
                if (action != new_action)
                    break;
            }
            if (j == ParsingTblCols)
                final_state_list[i] = action;
        }

    } else {
        for (int i = 0; i < ParsingTblRows; i++) {
            int row_start = i * ParsingTblCols;
            int action = 0, new_action = 0;
            int j = 0;
            for (; j < ParsingTblCols; j++) {
                new_action = ParsingTable[row_start + j];
                if (new_action > 0 || new_action == CONST_ACC)
                    break;
                if (new_action == 0)
                    continue;
                if (action == 0)
                    action = new_action;
                if (action != new_action)
                    break;
            }
            if (j == ParsingTblCols)
                final_state_list[i] = action;
        }
    }

#endif
}

void
get_avg_config_count()
{
    int i = 0, sum = 0;
    const State* a = states_new->states_head;
    int max = a->config_count;
    int min = a->config_count;
    yyprintf("\n--No. of configurations for each state--\n");
    for (; a != nullptr; a = a->next) {
        if ((++i) % 20 == 1)
            yyprintf("\n%d: ", i);
        yyprintf("%d ", a->config_count);
        sum += a->config_count;
        if (min > a->config_count)
            min = a->config_count;
        if (max < a->config_count)
            max = a->config_count;
    }
    yyprintf("\n");
    yyprintf("Average configurations per state: %.2f (min: %d, max: %d)\n",
             ((double)sum / states_new->state_count),
             min,
             max);
}

void
show_state_config_info()
{

#if USE_CONFIG_QUEUE_FOR_GET_CLOSURE
    queue_info(config_queue);
#else
    yyprintf("%d states in total.\n", xx);
    yyprintf("before combine: total cfg: %d, max cfg: %d, cfg/state: %.2f\n",
             yy,
             zz,
             ((double)yy) / xx); // before combineCompatibleConfig.
    yyprintf("after combine: total cfg: %d, max cfg: %d, cfg/state: %.2f\n",
             yyy,
             zzz,
             ((double)yyy) / xx);
#endif

    get_avg_config_count();
    state_hash_tbl_dump();
}

/*
 * print size of different objects. For development use only.
 */
void
print_size()
{
    std::cout << "size of Grammar: " << sizeof(Grammar) << std::endl
              << "size of StateCollection: " << sizeof(StateCollection)
              << std::endl
              << "size of State: " << sizeof(State) << std::endl
              << "size of (State *): " << sizeof(State*) << std::endl
              << "size of Context: " << sizeof(Context) << std::endl
              << "size of Production: " << sizeof(Production) << std::endl
              << "size of Configuration: " << sizeof(Configuration) << std::endl
              << "size of ParsingTblColHdr: " << sizeof(ParsingTblColHdr)
              << std::endl;
}

void
show_conflict_count()
{
    // no conflicts.
    if (rs_count == 0 && rr_count == 0 && ss_count == 0)
        return;

    // no r/r conflicts, and s/r conflicts number is expected.
    if (rs_count == expected_sr_conflict && rr_count == 0 && ss_count == 0)
        return;

    // otherwise, report conflicts.
    std::cout << hyacc_filename << ": conflicts: ";
    if (rs_count > 0) {
        std::cout << rs_count << " shift/reduce";
        if (rr_count > 0)
            std::cout << ", " << rr_count << " reduce/reduce";
    } else if (rr_count > 0) {
        std::cout << rr_count << " reduce/reduce";
    }
    std::cout << std::endl;
    if (ss_count > 0)
        std::cout << "warning: " << ss_count << " shift/shift conflicts"
                  << std::endl;
}

/* Show statistics of the grammar and it's parsing machine. */
void
show_stat()
{
    if (USE_VERBOSE == false)
        return;

    write_state_transition_list();
    if (SHOW_STATE_CONFIG_COUNT)
        show_state_config_info();
    if (SHOW_ACTUAL_STATE_ARRAY)
        write_actual_state_array();

    yyprintf("\n");
    // yyprintf("--statistics--\n");
    // yyprintf("[Note: A first rule '$accept -> start_symbol' is added]\n\n");
    // yyprintf("Symbols count: %d\n", n_symbol);
    yyprintf("%d terminals, %d nonterminals\n",
             grammar.terminal_count,
             grammar.non_terminal_count);
    yyprintf("%d grammar rules\n", n_rule);
    if (USE_REMOVE_UNIT_PRODUCTION) {
        yyprintf("%d grammar rules after remove unit productions\n",
                 n_rule_opt);
    }
    if (USE_COMBINE_COMPATIBLE_STATES) {
        if (USE_LR0) {
            yyprintf("%d states without optimization\n", n_state_opt1);
        } else {
            yyprintf("%d states after combine compatible states\n",
                     n_state_opt1);
        }
        if (USE_REMOVE_UNIT_PRODUCTION) {
            yyprintf("%d states after remove unit productions\n",
                     n_state_opt12);
            if (USE_REMOVE_REPEATED_STATES)
                yyprintf("%d states after remove repeated states\n",
                         n_state_opt123);
        }
    } else {
        yyprintf("%d states without optimization\n", n_state_opt1);
    }

    // conflicts summary.
    yyprintf("%d shift/reduce conflict%s, %d reduce/reduce conflict%s\n",
             rs_count,
             (rs_count > 1) ? "s" : "",
             rr_count,
             (rr_count > 1) ? "s" : "");
    if (USE_REMOVE_UNIT_PRODUCTION && ss_count > 0) {
        yyprintf(
          "%d shift/shift conflict%s\n\n", ss_count, (ss_count > 1) ? "s" : "");
    }

    // print_size();
}

/////////////////////////////////////////////
// Functions to write state transition list
/////////////////////////////////////////////

/*
 * Used when --lr0 or --lalr is used.
 * Under such situation USE_LR0 or USE_LALR is true.
 */
void
write_parsing_tbl_row_lalr(int state)
{
    int row_start = state * ParsingTblCols;
    int reduction = 1;
    int only_one_reduction = true;

    // write shift/acc actions.
    // note if a state has acc action, then that's the only action.
    // so don't have to put acc in a separate loop.
    for (int col = 0; col < ParsingTblCols; col++) {
        int v = ParsingTable[row_start + col];
        const SymbolTblNode* s = ParsingTblColHdr[col];
        if (v > 0) {
            if (USE_REMOVE_UNIT_PRODUCTION)
                v = get_actual_state(v);

            if (ParsingTblColHdr[col]->type == symbol_type::TERMINAL) {
                yyprintf("  %s [%d] shift %d\n", s->symbol, s->value, v);
            }
        } else if (v == CONST_ACC) {
            yyprintf("  %s [%d] Accept\n", s->symbol, s->value);
        } else if (v < 0) {
            if (reduction > 0) {
                reduction = v;
            } // first reduction.
            else if (reduction != v) {
                only_one_reduction = false;
            }
            // else, is the same as first reduction. do nothing.
        }
    }

    // write reduce actions.
    if (only_one_reduction) {
        if (reduction < 0) {
            yyprintf("  . reduce (%d)\n", (-1) * reduction);
        } else {
            yyprintf("  . error %d\n", reduction);
        } // no reduction.
    } else {
        for (int col = 0; col < ParsingTblCols; col++) {
            int v = ParsingTable[row_start + col];
            const SymbolTblNode* s = ParsingTblColHdr[col];
            if (v < 0 && v != CONST_ACC) {
                yyprintf(
                  "  %s [%d] reduce (%d)\n", s->symbol, s->value, (-1) * v);
            }
        }
    }

    // write goto action.
    yyprintf("\n");
    for (int col = 0; col < ParsingTblCols; col++) {
        int v = ParsingTable[row_start + col];
        const SymbolTblNode* s = ParsingTblColHdr[col];
        if (v > 0) {
            if (USE_REMOVE_UNIT_PRODUCTION)
                v = get_actual_state(v);

            if (ParsingTblColHdr[col]->type == symbol_type::NONTERMINAL) {
                yyprintf("  %s [%d] goto %d\n", s->symbol, s->value, v);
            }
        }
    }
}

void
write_parsing_tbl_row(int state)
{
    int row_start = state * ParsingTblCols;

    yyprintf("\n");

    if (final_state_list[state] < 0) {
        yyprintf("  . reduce (%d)\n", (-1) * final_state_list[state]);
        return;
    }

    if (USE_LR0 || USE_LALR) {
        write_parsing_tbl_row_lalr(state);
        return;
    }

    for (int col = 0; col < ParsingTblCols; col++) {
        int v = ParsingTable[row_start + col];
        const SymbolTblNode* s = ParsingTblColHdr[col];
        if (v > 0) {
            if (USE_REMOVE_UNIT_PRODUCTION)
                v = get_actual_state(v);

            if (ParsingTblColHdr[col]->type == symbol_type::NONTERMINAL) {
                yyprintf("  %s [%d] goto %d\n", s->symbol, s->value, v);
            } else {
                yyprintf("  %s [%d] shift %d\n", s->symbol, s->value, v);
            }
        } else if (v == CONST_ACC) {
            yyprintf("  %s [%d] Accept\n", s->symbol, s->value);
        } else if (v < 0) {
            yyprintf("  %s [%d] reduce (%d)\n", s->symbol, s->value, (-1) * v);
        }
    }
}

void
write_state_info(State& s)
{
    write_state_conflict_list(s.state_no);

    if (s.PASS_THRU == 1u) {
        yyprintf("[PASS_THRU]\n");
    }

    yyprintf("state %d\n\n", s.state_no);
    // yyprintf("  [config count : %d, core_config count : %d]\n\n",
    yyprintf("  [config: %d, core config: %d]\n\n",
             s.config_count,
             s.core_config_count);

    for (int i = 0; i < s.config_count; i++) {
        yyprintf("  (%d) ", s.config[i]->ruleID);
        write_configuration(*s.config[i]);
    }

    // writeSuccessorList(s);
    if (USE_LALR && SHOW_ORIGINATORS) {
        s.parents_list->write();
    }

    write_parsing_tbl_row(s.state_no);

    // writeCoreConfiguration(s);
    yyprintf("\n\n");
}

void
write_state_collection_info(StateCollection* c)
{
    // yyprintf("\n==State List: (count=%d)==\n\n", c->state_count);
    yyprintf("\n\n");
    State* s = c->states_head;
    while (s != nullptr) {
        write_state_info(*s);
        s = s->next;
    }

    if (c->state_count == 0)
        yyprintf("(empty)\n");
    yyprintf("\n");
}

void
write_state_info_from_parsing_tbl()
{
    // yyprintf("\n==States (count = %d)==\n", actual_state_no_ct / 2);
    for (int row = 0; row < ParsingTblRows; row++) {
        if (is_reachable_state(row)) {
            yyprintf("\n\nstate %d\n", get_actual_state(row));
            write_parsing_tbl_row(row);
            // writeStateConflictList(get_actual_state(row));
            // writeStateConflictList(row);
        }
    }
    yyprintf("\n\n");
}

/*
 * A list like the list in AT&T yacc and Bison's y.output file.
 */
void
write_state_transition_list()
{
    if (SHOW_STATE_TRANSITION_LIST == false)
        return;

    if (USE_REMOVE_UNIT_PRODUCTION) {
        // write from the parsing table.
        write_state_info_from_parsing_tbl();
        write_grammar_conflict_list2();
    } else {
        // write from the state objects.
        write_state_collection_info(states_new);
        write_grammar_conflict_list();
    }
}

/*
 * LR1 function.
 */
auto
lr1(int argc, char** argv) -> int
{
    hash_tbl_init();

    fp_v = nullptr; // for y.output
    if (USE_VERBOSE) {
        if ((fp_v = fopen(y_output, "w")) == nullptr) {
            throw std::runtime_error(std::string("cannot open file ") +
                                     y_output);
        }
        yyprintf("/* y.output. Generated by HYACC. */\n");
        yyprintf("/* Input file: %s */\n", hyacc_filename);
    }

    get_yacc_grammar(hyacc_filename);

    if (DEBUG_HASH_TBL) {
        hash_tbl_dump();
    }

    init();
    if (SHOW_GRAMMAR) {
        grammar.write(true);
    }

    generate_parsing_machine();

    if (SHOW_PARSING_TBL)
        print_parsing_table();

    if (USE_GRAPHVIZ && !USE_REMOVE_UNIT_PRODUCTION) {
        gen_graphviz_input();
    } /*O0,O1*/

    if (USE_REMOVE_UNIT_PRODUCTION) {
        remove_unit_production();
        if (SHOW_PARSING_TBL)
            yyprintf("\nAFTER REMOVING UNIT PRODUCTION:\n");
        if (SHOW_TOTAL_PARSING_TBL_AFTER_RM_UP) {
            yyprintf("\n--Entire parsing table ");
            yyprintf("after removing unit productions--\n");
            print_parsing_table();
        }

        if (SHOW_PARSING_TBL)
            print_final_parsing_table();
        if (USE_REMOVE_REPEATED_STATES) {
            further_optimization();
            if (SHOW_PARSING_TBL) {
                yyprintf("\nAFTER REMOVING REPEATED STATES:\n");
                print_final_parsing_table();
            }
        }
        get_actual_state_no(); /* update actual_state_no[]. */
        if (SHOW_PARSING_TBL)
            print_condensed_final_parsing_table();
        if (SHOW_GRAMMAR) {
            yyprintf("\n--Grammar after removing unit productions--\n");
            grammar.write(false);
        }
        if (USE_GRAPHVIZ) {
            gen_graphviz_input2();
        } /*O2,O3*/
    }
    get_final_state_list();

    if (USE_GENERATE_COMPILER)
        generate_compiler(hyacc_filename);

    show_stat();
    show_conflict_count();

    if (USE_VERBOSE)
        fclose(fp_v);
    // free_vars(); // let system take care of it.
    return 0;
}

auto
lr0(int argc, char** argv) -> int
{

    /// USE_COMBINE_COMPATIBLE_STATES = false; ///
    hash_tbl_init();

    fp_v = nullptr; // for y.output
    if (USE_VERBOSE) {
        if ((fp_v = fopen(y_output, "w")) == nullptr) {
            throw std::runtime_error(std::string("cannot open file ") +
                                     y_output);
        }
        yyprintf("/* y.output. Generated by HYACC. */\n");
        yyprintf("/* Input file: %s */\n", hyacc_filename);
    }

    get_yacc_grammar(hyacc_filename);

    if (DEBUG_HASH_TBL) {
        hash_tbl_dump();
    }

    init();
    if (SHOW_GRAMMAR) {
        grammar.write(true);
    }

    generate_lr0_parsing_machine(); //

    if (USE_LALR) {
        lane_tracing();
        // outputParsingTable_LALR();
    }

    if (SHOW_PARSING_TBL)
        print_parsing_table();

    if (USE_GRAPHVIZ && !USE_REMOVE_UNIT_PRODUCTION) {
        gen_graphviz_input();
    } /*O0,O1*/

    if (USE_REMOVE_UNIT_PRODUCTION) {
        remove_unit_production();
        if (SHOW_PARSING_TBL)
            yyprintf("\nAFTER REMOVING UNIT PRODUCTION:\n");
        if (SHOW_TOTAL_PARSING_TBL_AFTER_RM_UP) {
            yyprintf("\n--Entire parsing table ");
            yyprintf("after removing unit productions--\n");
            print_parsing_table();
        }

        if (SHOW_PARSING_TBL)
            print_final_parsing_table();
        if (USE_REMOVE_REPEATED_STATES) {
            further_optimization();
            if (SHOW_PARSING_TBL) {
                yyprintf("\nAFTER REMOVING REPEATED STATES:\n");
                print_final_parsing_table();
            }
        }
        get_actual_state_no(); /* update actual_state_no[]. */
        if (SHOW_PARSING_TBL)
            print_condensed_final_parsing_table();
        if (SHOW_GRAMMAR) {
            yyprintf("\n--Grammar after removing unit productions--\n");
            grammar.write(false);
        }
        if (USE_GRAPHVIZ) {
            gen_graphviz_input2();
        } /*O2,O3*/
    }
    get_final_state_list();

    if (USE_GENERATE_COMPILER)
        generate_compiler(hyacc_filename);

    show_stat();
    show_conflict_count();

    if (USE_LR_K) {
        std::cout << "Max K in LR(k): " << MAX_K << std::endl;
    }

    if (USE_VERBOSE)
        fclose(fp_v);
    // free_vars(); // let system take care of it.

    return 0;
}

/*
 * main function.
 */
auto
main(int argc, char** argv) -> int
{
    try {
        int infile_index = 0;
        DEBUG_EXPAND_ARRAY = false;

        // test_x();

        infile_index = get_options(argc, argv);
        hyacc_filename = argv[infile_index];
        // printf("file to open: %s\n", hyacc_filename);

        if (USE_LR0) {
            lr0(argc, argv);
        } else {
            lr1(argc, argv);
        }

        if (false) { // for reading memory.
            char keyval = 0;
            std::cout << "press ENTER to end... ";
            scanf("%c", &keyval); // stop here for reading memory.
        }
    } catch (std::runtime_error error) {
        std::cerr << error.what() << std::endl;
        return 1;
    }
    return 0;
}
