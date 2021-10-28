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

enum class ErrorFlags
{
    NO_INPUT_FILE,
    ERROR_FLAGS,
    UNKNOWN_SWITCH_O,
    UNKNOWN_SWITCH_O_USE_LR0,
    UNKNOWN_SWITCH_D,
    NO_OUTFILE_NAME,
    NO_FILENAME_PREFIX,
    SHOW_HELP,
};

void
show_helpmsg_exit(const ErrorFlags error_flags)
{
    if (error_flags == ErrorFlags::NO_INPUT_FILE) {
        std::cout << "no input file" << std::endl;
    }

    if (error_flags == ErrorFlags::UNKNOWN_SWITCH_O) {
        std::cout
          << "unknown parameter to switch -O. choices are -O0, -O1, -O2, -O3."
          << std::endl;
    }

    if (error_flags == ErrorFlags::UNKNOWN_SWITCH_O_USE_LR0) {
        std::cout
          << "switch -O cannot be used together with -P (--lane-tracing-pgm), "
          << std::endl
          << "-Q (--lane-tracing-ltt), -R (--lalr1) or -S (--lr0)."
          << std::endl;
    }

    if (error_flags == ErrorFlags::UNKNOWN_SWITCH_D) {
        std::cout
          << "unknown parameter to switch -D. choices are -Di (i = 0 ~ 14)."
          << std::endl;
    }

    if (error_flags == ErrorFlags::NO_OUTFILE_NAME) {
        std::cout
          << "output file name is not specified by -o or --output-file=="
          << std::endl;
    }

    if (error_flags == ErrorFlags::NO_FILENAME_PREFIX) {
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
get_outfile_name(const std::string name)
{
    if (name.empty()) {
        show_helpmsg_exit(ErrorFlags::NO_OUTFILE_NAME);
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
get_filename_prefix(std::string name)
{
    if (name.empty()) {
        show_helpmsg_exit(ErrorFlags::NO_OUTFILE_NAME);
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
init_options(Options& options)
{
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

/// @brief Get a single letter option out of `s`.
///
/// This will always advance `s` at least once. Some options require an
/// additional arguments, advancing `s` again.
///
/// @param cmd_argc arcg passed from main.
/// @param argc_pt index of current cmd parameter.
void
get_single_letter_option(Options& options,
                         int cmd_argc,
                         int argc_pt,
                         std::string::iterator& s,
                         const std::string::iterator& end)
{
    char switch_param = 0;
    char c = *s;
    ++s;

    // printf("c = %c\n", c);
    switch (c) {
        case 'b': /* file name prefix */
            if (argc_pt >= cmd_argc - 1) {
                show_helpmsg_exit(ErrorFlags::NO_FILENAME_PREFIX);
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
            show_helpmsg_exit(ErrorFlags::SHOW_HELP);
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
                show_helpmsg_exit(ErrorFlags::NO_OUTFILE_NAME);
            }
            options.use_output_filename = true;
            break;
        case 'O': /* optimization. */
            if (s == end) {
                show_helpmsg_exit(ErrorFlags::UNKNOWN_SWITCH_O);
            }
            if (options.use_lr0) {
                show_helpmsg_exit(ErrorFlags::UNKNOWN_SWITCH_O_USE_LR0);
            }
            switch_param = *s;
            ++s;
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
                    show_helpmsg_exit(ErrorFlags::UNKNOWN_SWITCH_O);
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
            if (s == end) {
                show_helpmsg_exit(ErrorFlags::UNKNOWN_SWITCH_D);
            }
            switch_param = *s;
            ++s;
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
                case 5: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.debug_build_multirooted_tree = true;
                    break;
                case 6: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.debug_remove_up_step_1_2 = true;
                    break;
                case 7: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.debug_remove_up_step_4 = true;
                    break;
                case 8: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_total_parsing_tbl_after_rm_up = true;
                    break;
                case 9: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_theads = true;
                    break;
                case 10: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.debug_hash_tbl = true;
                    break;
                case 11: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_ss_conflicts = true;
                    break;
                case 12: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_state_transition_list = false;
                    break;
                case 13: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_state_config_count = true;
                    break;
                case 14: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_actual_state_array = true;
                    break;
                case 15: // NOLINT(cppcoreguidelines-avoid-magic-numbers)
                    options.show_originators = true;
                    break;
                default:
                    show_helpmsg_exit(ErrorFlags::UNKNOWN_SWITCH_D);
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
get_mnemonic_long_option(Options& options, const std::string& s)
{
    if (s == "--debug") { // -t
        options.use_yydebug = true;
    } else if (s == "--defines") { // -d
        options.use_header_file = true;
    } else if (s == "--no-compiler") { // -c
        options.use_generate_compiler = false;
    } else if (s == "--keep-unit-production-with-action") { // -C
        options.preserve_unit_prod_with_code = false;
    } else if (s.rfind("--file-prefix==", 0) == 0) { // -b
        get_filename_prefix(s.substr(strlen("--file-prefix==")));
    } else if (s == "--graphviz") { // -g
        options.use_graphviz = true;
    } else if (s == "--help") { // -h
        show_helpmsg_exit(ErrorFlags::SHOW_HELP);
    } else if (s == "--lalr1") { // -a
        set_lalr1(options);
    } else if (s == "--lane-tracing-pgm") { // -P
        // set_lane_tracing_no_pgm();
        set_lane_tracing_pgm(options);
    } else if (s == "--lane-tracing-ltt") { // -Q
        set_lane_tracing_ltt(options);
    } else if (s == "--lr0") { // -r
        set_lr0(options);
    } else if (s == "--lrk") {
        set_lrk(options);
    } else if (s == "--man-page") { // -m
        show_manpage();             /* in hyacc_path.cpp */
        exit(0);
        //} else if (strncmp(s, "--name-prefix==", 15) == 0) { // -p
        // to be implemented.
    } else if (s == "--no-lines") { // -l
        options.use_lines = false;
    } else if (s.rfind("--output-file==", 0) == 0) { // -o
        get_outfile_name(s.substr(strlen("--output-file==")));
    } else if (s == "--verbose") { // -v
        options.use_verbose = true;
    } else if (s == "--version") { // -V
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
get_options(int argc, char** argv, Options& options) -> int
{
    if (argc == 1) {
        show_helpmsg_exit(ErrorFlags::NO_INPUT_FILE);
    }

    init_options(options);

    int infile_index = -1;
    for (int i = 1; i < argc; i++) {
        int argc_pt = i;
        std::string argv_i =
          argv[i]; // NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        // printf("%d, %s\n", i, argv_i);
        size_t len = argv_i.size();

        if (options.use_output_filename) { // get output file name.
            get_outfile_name(argv_i);
            options.use_output_filename = false;
            continue;
        }
        if (options.use_filename_prefix) { // -b
            get_filename_prefix(argv_i);
            options.use_filename_prefix = false;
            continue;
        }

        std::string::iterator argv_i_iter = argv_i.begin();
        const std::string::iterator argv_i_iter_end = argv_i.end();

        if (len >= 2 && *argv_i_iter == '-') {
            ++argv_i_iter;
            if (*argv_i_iter == '-') {
                get_mnemonic_long_option(options, argv_i.data());
            } else {
                for (; argv_i_iter != argv_i_iter_end; ++argv_i_iter) {
                    char c = *argv_i_iter;
                    get_single_letter_option(
                      options, argc, argc_pt, argv_i_iter, argv_i_iter_end);
                }
            }
        } else {
            if (infile_index == -1) {
                infile_index = i;
                // printf("file to open: %s\n", argv[i]);
            }
        }
    }

    if (infile_index == -1) {
        show_helpmsg_exit(ErrorFlags::NO_INPUT_FILE);
    }

    write_options(infile_index, argv);
    return infile_index;
}
