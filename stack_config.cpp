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
 * stack_config.c
 *
 * An expandable stack for Configuration pointer.
 *
 * @Author: Xin Chen
 * @Created on: 2/27/2006
 * @Last modified: 6/19/2007
 */

#include "stack_config.hpp"
#include "y.hpp"
#include <iostream>

void
Stack::dump(const Grammar& grammar) const noexcept
{
    constexpr size_t LINE_SIZE = 10;
    std::cout << "stack capacity: " << this->array.capacity()
              << ", count: " << this->count() << std::endl;
    if (this->count() == 0)
        return;

    for (size_t i = 0; i < this->count(); i++) {
        if ((i > 0) && (i % LINE_SIZE == 0))
            std::cout << std::endl;
        auto c = this->array[i];
        std::cout << "[" << i << "] ";
        if (c == nullptr) {
            std::cout << "0" << std::endl;
        } else if (c.has_value()) {
            stdout_write_config(grammar, *c);
        } else {
            std::cout << "*" << std::endl;
        }
    }
    std::cout << std::endl;
}
