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
 * get_options.cpp
 *
 * Gets switch options from the command line.
 *
 * @Author: Xin Chen
 * @Created on: Febuary 8, 2007
 * @Last modified: 7/25/2007
 */

#include "y.hpp"
#include <iostream>
#include <stdexcept>

static int cmd_argc; /* argc passed from main(). */
static int argc_pt;  /* index of current cmd parameter. */

static bool ERR_NO_INPUT_FILE = false;
static bool ERR_UNKNOWN_SWITCH_O = false;
static bool ERR_UNKNOWN_SWITCH_O_use_lr0 = false;
static bool ERR_UNKNOWN_SWITCH_D = false;
static bool ERR_NO_OUTFILE_NAME = false;
static bool ERR_NO_FILENAME_PREFIX = false;

void
show_helpmsg_exit()
{
    if (ERR_NO_INPUT_FILE) {
        std::cout << "no input file" << std::endl;
    }

    if (ERR_UNKNOWN_SWITCH_O) {
        std::cout
          << "unknown parameter to switch -O. choices are -O0, -O1, -O2, -O3."
          << std::endl;
    }

    if (ERR_UNKNOWN_SWITCH_O_use_lr0) {
        std::cout
          << "switch -O cannot be used together with -P (--lane-tracing-pgm), "
          << std::endl
          << "-Q (--lane-tracing-ltt), -R (--lalr1) or -S (--lr0)."
          << std::endl;
    }

    if (ERR_UNKNOWN_SWITCH_D) {
        std::cout
          << "unknown parameter to switch -D. choices are -Di (i = 0 ~ 14)."
          << std::endl;
    }

    if (ERR_NO_OUTFILE_NAME) {
        std::cout
          << "output file name is not specified by -o or --output-file=="
          << std::endl;
    }

    if (ERR_NO_FILENAME_PREFIX) {
        std::cout
          << "file name prefix is not specified by -b or --file-prefix=="
          << std::endl;
    }

    std::cout << "Usage: hyacc [-bcCdDghKlmnOPQRStvV] inputfile" << std::endl
              << "Man page: hyacc -m" << std::endl;
    exit(0);
}

/*
 * Get output file name if -o or --outfile-name==
 * switches are used.
 */
void
get_outfile_name(size_t len, const char* name)
{
    if (len == 0 || strlen(name) == 0) {
        ERR_NO_OUTFILE_NAME = true;
        show_helpmsg_exit();
    }
    // printf("len = %d, name = %s\n", len, name);
    std::string y_tab_c_temp = name;
    y_tab_c_temp += ".cpp";
    y_tab_c = y_tab_c_temp;
    std::string y_tab_h_temp = name;
    y_tab_h_temp += ".hpp";
    y_tab_h = y_tab_h_temp;
    std::string y_output_temp = name;
    y_output_temp += ".output";
    y_output = y_output_temp;
    std::string y_gviz_temp = name;
    y_gviz_temp += ".gviz";
    y_gviz = y_gviz_temp;
}

/*
 * Get output file name if -b or --file_prefix==
 * switches are used.
 */
void
get_filename_prefix(size_t len, const char* name)
{
    if (len == 0 || strlen(name) == 0) {
        ERR_NO_OUTFILE_NAME = true;
        show_helpmsg_exit();
    }
    // printf("len = %d, name = %s\n", len, name);
    std::string y_tab_c_temp = name;
    y_tab_c_temp += ".tab.cpp";
    y_tab_c = y_tab_c_temp;
    std::string y_tab_h_temp = name;
    y_tab_h_temp += ".tab.hpp";
    y_tab_h = y_tab_h_temp;
    std::string y_output_temp = name;
    y_output_temp += ".output";
    y_output = y_output_temp;
    std::string y_gviz_temp = name;
    y_gviz_temp += ".gviz";
    y_gviz = y_gviz_temp;
}

void
write_options(int infile_index, char** argv)
{
    // exit(0);
}

void
init_options()
{
    auto& options = Options::get();
    options.use_combine_compatible_config = true;

    // default: -O1
    options.use_combine_compatible_states = true;
    options.use_remove_unit_production = false;
    options.use_remove_repeated_states = false;

    options.show_grammar = false;
    options.show_parsing_tbl = false;
    options.debug_gen_parsing_machine = false;
    options.debug_comb_comp_config = false;
    options.debug_build_multirooted_tree = false;
    options.debug_remove_up_step_1_2 = false;
    options.debug_remove_up_step_4 = false;
    options.show_total_parsing_tbl_after_rm_up = false;
    options.show_theads = false;
    options.use_generate_compiler = true;
    options.preserve_unit_prod_with_code = false;
    options.debug_hash_tbl = false;
    options.show_ss_conflicts = false;
    options.show_state_transition_list = true;
    options.show_state_config_count = false;
    options.show_actual_state_array = false;

    options.use_yydebug = false;
    options.use_lines = true;
    options.use_verbose = false; /* not implemented in code yet */
    y_tab_c = "y.tab.cpp";
    y_tab_h = "y.tab.hpp";
    y_output = "y.output";
    y_gviz = "y.gviz";
    options.use_output_filename = false;
    options.use_filename_prefix = false;
    options.use_header_file = false;
    options.use_graphviz = false;

    options.use_lr0 = false;
    options.use_lalr = false;
    options.use_lane_tracing = false;
    options.use_lr_k = false;

    options.show_originators = false;
}

void
set_lalr1(Options& options)
{
    options.use_lalr = true;
    options.use_lr0 = true;
}

void
set_lr0(Options& options)
{
    options.use_lr0 = true;
}

/*
 * Use PGM to combine states in the end.
 */
void
set_lane_tracing_pgm(Options& options)
{
    options.use_lane_tracing = true;
    options.use_lalr = true;
    options.use_lr0 = true;
    options.use_combine_compatible_states = true;
}

/*
 * Use the other (lane-based) method to combine states in the end.
 */
void
set_lane_tracing_ltt(Options& options)
{
    options.use_lane_tracing = true;
    options.use_lalr = true;
    options.use_lr0 = true;
    options.use_combine_compatible_states = false;
}

/*
 * -P plus LR(k).
 */
void
set_lrk(Options& options)
{
    set_lane_tracing_ltt(options);
    options.use_lr_k = true;
}

void
get_single_letter_option(char* s, unsigned int pos)
{
    auto& options = Options::get();
    int switch_param = -1;
    char c = s[pos];

    // printf("c = %c\n", c);
    switch (c) {
        case 'b': /* file name prefix */
            if (argc_pt >= cmd_argc - 1) {
                ERR_NO_FILENAME_PREFIX = true;
                show_helpmsg_exit();
            }
            options.use_filename_prefix = true;
            break;
        case 'c':
            options.use_generate_compiler = false;
            break;
        case 'C':
            options.preserve_unit_prod_with_code = true;
            break;
        case 'd': /* define: create y.tab.h */
            options.use_header_file = true;
            break;
        case 'g': /* create y.gviz */
            options.use_graphviz = true;
            break;
        case 'h':
        case '?':
            show_helpmsg_exit();
            break;
        case 'l': /* no line directives in y.tab.cpp */
            options.use_lines = false;
            break;
        case 'm':           /* show man page */
            show_manpage(); /* in hyacc_path.cpp */
            exit(0);
            break;
        case 'o': /* output file name */
            if (argc_pt >= cmd_argc - 1) {
                ERR_NO_OUTFILE_NAME = true;
                show_helpmsg_exit();
            }
            options.use_output_filename = true;
            break;
        case 'O': /* optimization. */
            if (strlen(s) <= pos + 1) {
                ERR_UNKNOWN_SWITCH_O = true;
                show_helpmsg_exit();
            }
            if (options.use_lr0) {
                ERR_UNKNOWN_SWITCH_O_use_lr0 = true;
                show_helpmsg_exit();
            }
            sscanf(s + pos + 1, "%d", &switch_param);
            switch (switch_param) {
                case 0:
                    options.use_combine_compatible_states = false;
                    options.use_remove_unit_production = false;
                    options.use_remove_repeated_states = false;
                    break;
                case 1:
                    options.use_combine_compatible_states = true;
                    options.use_remove_unit_production = false;
                    options.use_remove_repeated_states = false;
                    break;
                case 2:
                    options.use_combine_compatible_states = true;
                    options.use_remove_unit_production = true;
                    options.use_remove_repeated_states = false;
                    break;
                case 3: /* use all optimizations */
                    options.use_combine_compatible_states = true;
                    options.use_remove_unit_production = true;
                    options.use_remove_repeated_states = true;
                    break;
                default:
                    ERR_UNKNOWN_SWITCH_O = true;
                    show_helpmsg_exit();
                    break;
            }
            break;
        // case 'p':        /* symbol name prefix */ // to be implemented.
        //           break;
        case 'K':
            set_lrk(options);
            break;
        case 'P':
            set_lane_tracing_pgm(options);
            break;
        case 'Q':
            set_lane_tracing_ltt(options);
            break;
        case 'R':
            set_lalr1(options);
            break;
        case 'S':
            set_lr0(options);
            break;
        case 't': /* debug: output y.parse */
            options.use_yydebug = true;
            break;
        case 'v': /* verbose */
            options.use_verbose = true;
            break;
        case 'V': /* show version information. */
            print_version();
            exit(0);
            break;
        case 'D': /* debug print options. */
            if (strlen(s) <= pos + 1) {
                ERR_UNKNOWN_SWITCH_D = true;
                show_helpmsg_exit();
            }
            sscanf(s + pos + 1, "%d", &switch_param);
            switch (switch_param) {
                case 0:
                    options.show_grammar = true;
                    options.show_parsing_tbl = true;
                    options.debug_gen_parsing_machine = true;
                    options.debug_comb_comp_config = true;
                    options.debug_build_multirooted_tree = true;
                    options.debug_remove_up_step_1_2 = true;
                    options.debug_remove_up_step_4 = true;
                    options.show_total_parsing_tbl_after_rm_up = true;
                    options.show_theads = true;
                    options.debug_hash_tbl = true;
                    options.show_ss_conflicts = true;
                    options.show_state_transition_list = true;
                    options.show_state_config_count = true;
                    options.show_actual_state_array = true;
                    break;
                case 1:
                    options.show_grammar = true;
                    break;
                case 2:
                    options.show_parsing_tbl = true;
                    break;
                case 3:
                    options.debug_gen_parsing_machine = true;
                    break;
                case 4:
                    options.debug_comb_comp_config = true;
                    break;
                case 5:
                    options.debug_build_multirooted_tree = true;
                    break;
                case 6:
                    options.debug_remove_up_step_1_2 = true;
                    break;
                case 7:
                    options.debug_remove_up_step_4 = true;
                    break;
                case 8:
                    options.show_total_parsing_tbl_after_rm_up = true;
                    break;
                case 9:
                    options.show_theads = true;
                    break;
                case 10:
                    options.debug_hash_tbl = true;
                    break;
                case 11:
                    options.show_ss_conflicts = true;
                    break;
                case 12:
                    options.show_state_transition_list = false;
                    break;
                case 13:
                    options.show_state_config_count = true;
                    break;
                case 14:
                    options.show_actual_state_array = true;
                    break;
                case 15:
                    options.show_originators = true;
                    break;
                default:
                    ERR_UNKNOWN_SWITCH_D = true;
                    show_helpmsg_exit();
                    break;
            }
            options.use_verbose = true; /* allow write to y.output */
            break;
        default:                           /* ignore other switches. */
            if (!(c >= '0' && c <= '9')) { /* not a digit */
                throw std::runtime_error(std::string("unknown switch ") +
                                         std::to_string(c));
            }
            break;
    }
}

void
get_mnemonic_long_option(const char* s)
{
    auto& options = Options::get();
    if (strcmp(s, "--debug") == 0) { // -t
        options.use_yydebug = true;
    } else if (strcmp(s, "--defines") == 0) { // -d
        options.use_header_file = true;
    } else if (strcmp(s, "--no-compiler") == 0) { // -c
        options.use_generate_compiler = false;
    } else if (strcmp(s, "--keep-unit-production-with-action") == 0) { // -C
        options.preserve_unit_prod_with_code = false;
    } else if (strncmp(s, "--file-prefix==", 15) == 0) { // -b
        // 15 for strlen("--file-prefix==").
        get_filename_prefix(strlen(s) - 15, s + 15);
    } else if (strcmp(s, "--graphviz") == 0) { // -g
        options.use_graphviz = true;
    } else if (strcmp(s, "--help") == 0) { // -h
        show_helpmsg_exit();
    } else if (strcmp(s, "--lalr1") == 0) { // -a
        set_lalr1(options);
    } else if (strcmp(s, "--lane-tracing-pgm") == 0) { // -P
        // set_lane_tracing_no_pgm();
        set_lane_tracing_pgm(options);
    } else if (strcmp(s, "--lane-tracing-ltt") == 0) { // -Q
        set_lane_tracing_ltt(options);
    } else if (strcmp(s, "--lr0") == 0) { // -r
        set_lr0(options);
    } else if (strcmp(s, "--lrk") == 0) {
        set_lrk(options);
    } else if (strcmp(s, "--man-page") == 0) { // -m
        show_manpage();                        /* in hyacc_path.cpp */
        exit(0);
        //} else if (strncmp(s, "--name-prefix==", 15) == 0) { // -p
        // to be implemented.
    } else if (strcmp(s, "--no-lines") == 0) { // -l
        options.use_lines = false;
    } else if (strncmp(s, "--output-file==", 15) == 0) { // -o
        // 15 for strlen("--output-file==").
        get_outfile_name(strlen(s) - 15, s + 15);
    } else if (strcmp(s, "--verbose") == 0) { // -v
        options.use_verbose = true;
    } else if (strcmp(s, "--version") == 0) { // -V
        print_version();
        exit(0);
    } else {
        throw std::runtime_error(std::string("unknown switch: ") + s);
    }
}

/*
 * Note:
 *   optimization 1: combine compatible states.
 *   optimization 2: remove unit productions after 1.
 *   optimization 3: further remove repeated states after 2.
 */
auto
get_options(int argc, char** argv) -> int
{
    if ((cmd_argc = argc) == 1) {
        ERR_NO_INPUT_FILE = true;
        show_helpmsg_exit();
    }

    init_options();
    auto& options = Options::get();

    int infile_index = -1;
    for (int i = 1; i < argc; i++) {
        argc_pt = i;
        auto* argv_i = argv[i];
        // printf("%d, %s\n", i, argv_i);
        size_t len = strlen(argv_i);

        if (options.use_output_filename) { // get output file name.
            get_outfile_name(len, argv_i);
            options.use_output_filename = false;
            continue;
        }
        if (options.use_filename_prefix) { // -b
            get_filename_prefix(len, argv_i);
            options.use_filename_prefix = false;
            continue;
        }

        if (len >= 2 && argv_i[0] == '-' && argv_i[1] == '-') {
            get_mnemonic_long_option(argv_i);
        } else if (argv_i[0] == '-') {
            // printf ("single letter switch %s\n", argv[i]);
            for (int pos = 1; pos < len; pos++) {
                get_single_letter_option(argv_i, pos);
            }
        } else {
            if (infile_index == -1) {
                infile_index = i;
                // printf("file to open: %s\n", argv[i]);
            }
        }
    }

    if (infile_index == -1) {
        ERR_NO_INPUT_FILE = true;
        show_helpmsg_exit();
    }

    write_options(infile_index, argv);
    return infile_index;
}
