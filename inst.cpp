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
 * inst.c
 *
 * Used by makefile when calling 'make install'.
 * This will generate a new hyacc_path.c file given the
 * installation path, which is passed to main() as a
 * command line argument by makefile.
 *
 * @compile: gcc inst.c -o inst
 *
 * @Author: Xin Chen
 * @Created on: Feb 10, 2007
 * @Last updated: Feb 10, 2007
 */

#include <cstdlib> /* system, exit. */
#include <fstream>
#include <iostream>
#include <span>

static const char* const S1 = "/*\n\
 * Generated by inst.cpp \n\
 * This file is used to retrieve files hyaccpar and hyaccmanpage.\n\
 * This file is updated each time hyacc is installed.\n\
 */\n\n\
#include \"y.hpp\"\n\n\
void show_manpage()\n\
{\n\
  system(\"less ";

static const char* const S2 = "/lib/hyacc/hyaccmanpage\");\n\
}\n\n\
std::string HYACC_PATH = \n\
\"";

static const char* const S3 = "/lib/hyacc/hyaccpar\";\n\n";

auto
main(int argc, const char* argv[]) -> int
{
    std::span<const char* const> args(argv, argc);
    if (args.size() == 1)
        return 0;

    const char* filename = "hyacc_path.tmp";
    std::ofstream fp2;
    fp2.open(filename);
    fp2 << S1 << args[1] << S2 << args[1] << S3;
    fp2.close();

    system("mv -f hyacc_path.tmp hyacc_path.cpp");

    return 0;
}
