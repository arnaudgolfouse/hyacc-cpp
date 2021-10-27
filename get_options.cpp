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
static bool ERR_UNKNOWN_SWITCH_O_USE_LR0 = false;
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

    if (ERR_UNKNOWN_SWITCH_O_USE_LR0) {
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
    auto* y_tab_c_temp = new char[len + 5];
    sprintf(y_tab_c_temp, "%s.cpp", name);
    y_tab_c = y_tab_c_temp;
    auto* y_tab_h_temp = new char[len + 5];
    sprintf(y_tab_h_temp, "%s.hpp", name);
    y_tab_h = y_tab_h_temp;
    auto* y_output_temp = new char[len + 8];
    sprintf(y_output_temp, "%s.output", name);
    y_output = y_output_temp;
    auto* y_gviz_temp = new char[len + 8];
    sprintf(y_gviz_temp, "%s.gviz", name);
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
    auto* y_tab_c_temp = new char[len + 7];
    sprintf(y_tab_c_temp, "%s.tab.cpp", name);
    y_tab_c = y_tab_c_temp;
    auto* y_tab_h_temp = new char[len + 7];
    sprintf(y_tab_h_temp, "%s.tab.hpp", name);
    y_tab_h = y_tab_h_temp;
    auto* y_output_temp = new char[len + 6];
    sprintf(y_output_temp, "%s.output", name);
    y_output = y_output_temp;
    auto* y_gviz_temp = new char[len + 6];
    sprintf(y_gviz_temp, "%s.gviz", name);
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
    USE_COMBINE_COMPATIBLE_CONFIG = true;

    // default: -O1
    USE_COMBINE_COMPATIBLE_STATES = true;
    USE_REMOVE_UNIT_PRODUCTION = false;
    USE_REMOVE_REPEATED_STATES = false;

    SHOW_GRAMMAR = false;
    SHOW_PARSING_TBL = false;
    DEBUG_GEN_PARSING_MACHINE = false;
    DEBUG_COMB_COMP_CONFIG = false;
    DEBUG_BUILD_MULTIROOTED_TREE = false;
    DEBUG_REMOVE_UP_STEP_1_2 = false;
    DEBUG_REMOVE_UP_STEP_4 = false;
    SHOW_TOTAL_PARSING_TBL_AFTER_RM_UP = false;
    SHOW_THEADS = false;
    USE_GENERATE_COMPILER = true;
    PRESERVE_UNIT_PROD_WITH_CODE = false;
    DEBUG_HASH_TBL = false;
    SHOW_SS_CONFLICTS = false;
    SHOW_STATE_TRANSITION_LIST = true;
    SHOW_STATE_CONFIG_COUNT = false;
    SHOW_ACTUAL_STATE_ARRAY = false;

    USE_YYDEBUG = false;
    USE_LINES = true;
    USE_VERBOSE = false; /* not implemented in code yet */
    y_tab_c = "y.tab.cpp";
    y_tab_h = "y.tab.hpp";
    y_output = "y.output";
    y_gviz = "y.gviz";
    USE_OUTPUT_FILENAME = false;
    USE_FILENAME_PREFIX = false;
    USE_HEADER_FILE = false;
    USE_GRAPHVIZ = false;

    USE_LR0 = false;
    USE_LALR = false;
    USE_LANE_TRACING = false;
    USE_LR_K = false;

    SHOW_ORIGINATORS = false;
}

void
set_lalr1()
{
    USE_LALR = true;
    USE_LR0 = true;
}

void
set_lr0()
{
    USE_LR0 = true;
}

/*
 * Use PGM to combine states in the end.
 */
void
set_lane_tracing_pgm()
{
    USE_LANE_TRACING = true;
    USE_LALR = true;
    USE_LR0 = true;
    USE_COMBINE_COMPATIBLE_STATES = true;
}

/*
 * Use the other (lane-based) method to combine states in the end.
 */
void
set_lane_tracing_ltt()
{
    USE_LANE_TRACING = true;
    USE_LALR = true;
    USE_LR0 = true;
    USE_COMBINE_COMPATIBLE_STATES = false;
}

/*
 * -P plus LR(k).
 */
void
set_lrk()
{
    set_lane_tracing_ltt();
    USE_LR_K = true;
}

void
get_single_letter_option(char* s, unsigned int pos)
{
    int switch_param = -1;
    char c = s[pos];

    // printf("c = %c\n", c);
    switch (c) {
        case 'b': /* file name prefix */
            if (argc_pt >= cmd_argc - 1) {
                ERR_NO_FILENAME_PREFIX = true;
                show_helpmsg_exit();
            }
            USE_FILENAME_PREFIX = true;
            break;
        case 'c':
            USE_GENERATE_COMPILER = false;
            break;
        case 'C':
            PRESERVE_UNIT_PROD_WITH_CODE = true;
            break;
        case 'd': /* define: create y.tab.h */
            USE_HEADER_FILE = true;
            break;
        case 'g': /* create y.gviz */
            USE_GRAPHVIZ = true;
            break;
        case 'h':
        case '?':
            show_helpmsg_exit();
            break;
        case 'l': /* no line directives in y.tab.cpp */
            USE_LINES = false;
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
            USE_OUTPUT_FILENAME = true;
            break;
        case 'O': /* optimization. */
            if (strlen(s) <= pos + 1) {
                ERR_UNKNOWN_SWITCH_O = true;
                show_helpmsg_exit();
            }
            if (USE_LR0) {
                ERR_UNKNOWN_SWITCH_O_USE_LR0 = true;
                show_helpmsg_exit();
            }
            sscanf(s + pos + 1, "%d", &switch_param);
            switch (switch_param) {
                case 0:
                    USE_COMBINE_COMPATIBLE_STATES = false;
                    USE_REMOVE_UNIT_PRODUCTION = false;
                    USE_REMOVE_REPEATED_STATES = false;
                    break;
                case 1:
                    USE_COMBINE_COMPATIBLE_STATES = true;
                    USE_REMOVE_UNIT_PRODUCTION = false;
                    USE_REMOVE_REPEATED_STATES = false;
                    break;
                case 2:
                    USE_COMBINE_COMPATIBLE_STATES = true;
                    USE_REMOVE_UNIT_PRODUCTION = true;
                    USE_REMOVE_REPEATED_STATES = false;
                    break;
                case 3: /* use all optimizations */
                    USE_COMBINE_COMPATIBLE_STATES = true;
                    USE_REMOVE_UNIT_PRODUCTION = true;
                    USE_REMOVE_REPEATED_STATES = true;
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
            set_lrk();
            break;
        case 'P':
            set_lane_tracing_pgm();
            break;
        case 'Q':
            set_lane_tracing_ltt();
            break;
        case 'R':
            set_lalr1();
            break;
        case 'S':
            set_lr0();
            break;
        case 't': /* debug: output y.parse */
            USE_YYDEBUG = true;
            break;
        case 'v': /* verbose */
            USE_VERBOSE = true;
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
                    SHOW_GRAMMAR = true;
                    SHOW_PARSING_TBL = true;
                    DEBUG_GEN_PARSING_MACHINE = true;
                    DEBUG_COMB_COMP_CONFIG = true;
                    DEBUG_BUILD_MULTIROOTED_TREE = true;
                    DEBUG_REMOVE_UP_STEP_1_2 = true;
                    DEBUG_REMOVE_UP_STEP_4 = true;
                    SHOW_TOTAL_PARSING_TBL_AFTER_RM_UP = true;
                    SHOW_THEADS = true;
                    DEBUG_HASH_TBL = true;
                    SHOW_SS_CONFLICTS = true;
                    SHOW_STATE_TRANSITION_LIST = true;
                    SHOW_STATE_CONFIG_COUNT = true;
                    SHOW_ACTUAL_STATE_ARRAY = true;
                    break;
                case 1:
                    SHOW_GRAMMAR = true;
                    break;
                case 2:
                    SHOW_PARSING_TBL = true;
                    break;
                case 3:
                    DEBUG_GEN_PARSING_MACHINE = true;
                    break;
                case 4:
                    DEBUG_COMB_COMP_CONFIG = true;
                    break;
                case 5:
                    DEBUG_BUILD_MULTIROOTED_TREE = true;
                    break;
                case 6:
                    DEBUG_REMOVE_UP_STEP_1_2 = true;
                    break;
                case 7:
                    DEBUG_REMOVE_UP_STEP_4 = true;
                    break;
                case 8:
                    SHOW_TOTAL_PARSING_TBL_AFTER_RM_UP = true;
                    break;
                case 9:
                    SHOW_THEADS = true;
                    break;
                case 10:
                    DEBUG_HASH_TBL = true;
                    break;
                case 11:
                    SHOW_SS_CONFLICTS = true;
                    break;
                case 12:
                    SHOW_STATE_TRANSITION_LIST = false;
                    break;
                case 13:
                    SHOW_STATE_CONFIG_COUNT = true;
                    break;
                case 14:
                    SHOW_ACTUAL_STATE_ARRAY = true;
                    break;
                case 15:
                    SHOW_ORIGINATORS = true;
                    break;
                default:
                    ERR_UNKNOWN_SWITCH_D = true;
                    show_helpmsg_exit();
                    break;
            }
            USE_VERBOSE = true; /* allow write to y.output */
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
    if (strcmp(s, "--debug") == 0) { // -t
        USE_YYDEBUG = true;
    } else if (strcmp(s, "--defines") == 0) { // -d
        USE_HEADER_FILE = true;
    } else if (strcmp(s, "--no-compiler") == 0) { // -c
        USE_GENERATE_COMPILER = false;
    } else if (strcmp(s, "--keep-unit-production-with-action") == 0) { // -C
        PRESERVE_UNIT_PROD_WITH_CODE = false;
    } else if (strncmp(s, "--file-prefix==", 15) == 0) { // -b
        // 15 for strlen("--file-prefix==").
        get_filename_prefix(strlen(s) - 15, s + 15);
    } else if (strcmp(s, "--graphviz") == 0) { // -g
        USE_GRAPHVIZ = true;
    } else if (strcmp(s, "--help") == 0) { // -h
        show_helpmsg_exit();
    } else if (strcmp(s, "--lalr1") == 0) { // -a
        set_lalr1();
    } else if (strcmp(s, "--lane-tracing-pgm") == 0) { // -P
        // set_lane_tracing_no_pgm();
        set_lane_tracing_pgm();
    } else if (strcmp(s, "--lane-tracing-ltt") == 0) { // -Q
        set_lane_tracing_ltt();
    } else if (strcmp(s, "--lr0") == 0) { // -r
        set_lr0();
    } else if (strcmp(s, "--lrk") == 0) {
        set_lrk();
    } else if (strcmp(s, "--man-page") == 0) { // -m
        show_manpage();                        /* in hyacc_path.cpp */
        exit(0);
        //} else if (strncmp(s, "--name-prefix==", 15) == 0) { // -p
        // to be implemented.
    } else if (strcmp(s, "--no-lines") == 0) { // -l
        USE_LINES = false;
    } else if (strncmp(s, "--output-file==", 15) == 0) { // -o
        // 15 for strlen("--output-file==").
        get_outfile_name(strlen(s) - 15, s + 15);
    } else if (strcmp(s, "--verbose") == 0) { // -v
        USE_VERBOSE = true;
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

    int infile_index = -1;
    for (int i = 1; i < argc; i++) {
        argc_pt = i;
        // printf("%d, %s\n", i, argv[i]);
        size_t len = strlen(argv[i]);

        if (USE_OUTPUT_FILENAME) { // get output file name.
            get_outfile_name(len, argv[i]);
            USE_OUTPUT_FILENAME = false;
            continue;
        }
        if (USE_FILENAME_PREFIX) { // -b
            get_filename_prefix(len, argv[i]);
            USE_FILENAME_PREFIX = false;
            continue;
        }

        if (len >= 2 && argv[i][0] == '-' && argv[i][1] == '-') {
            get_mnemonic_long_option(argv[i]);
        } else if (argv[i][0] == '-') {
            // printf ("single letter switch %s\n", argv[i]);
            for (int pos = 1; pos < len; pos++) {
                get_single_letter_option(argv[i], pos);
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
