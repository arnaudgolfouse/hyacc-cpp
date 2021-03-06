/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007-2017 Xin Chen. chenx@hawaii.edu

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
 * version.c
 *
 * Gives version number of the hyacc release.
 *
 * @Author: Xin Chen
 * @Date started: February 8, 2007
 * @Last modified: February 16, 2017
 */

#include "y.hpp"
#include <iostream>
#include <string_view>

const std::string_view HYACC_VERSION = "HYACC version 0.98 (2017)\n\
Copyright (C) 2007-2017 Xin Chen\n\n\
Hyacc uses GNU GPL license. \n\
The parser engines hyaccpar and hyaccpark use BSD license. \n\
See the source for license information.\
";

void
print_version()
{
    std::cout << HYACC_VERSION << std::endl;
}
