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

#ifndef _STACK_CONFIG_H_
#define _STACK_CONFIG_H_

/*
 * stack_config.h
 *
 * An expandable integer stack, implemented as an array.
 *
 * @Author: Xin Chen
 * @Created on: 2/27/2006
 * @Last modified: 6/19/2007
 */

#include "y.hpp"
#include <cstddef>
#include <iostream>
#include <vector>

#define DEBUG_STACK 0
constexpr size_t STACK_INIT_SIZE = 256;
constexpr bool USE_WARNING = false;

struct Stack
{
    std::vector<Configuration*> array;

    static auto create() -> Stack*;
    static auto create2(size_t init_capacity) -> Stack*;
    static void destroy(Stack* s);

    void dump() const;
    inline void push(Configuration* n) { this->array.push_back(n); }
    inline auto pop() -> Configuration*
    {
        if (this->array.empty()) {
            if (USE_WARNING)
                std::cout << "stack_pop warning: underflow, return nullptr"
                          << std::endl;
            return nullptr;
        }
        auto* last = this->array.back();
        this->array.pop_back();
        return last;
    }
    inline auto top() -> Configuration*
    {
        if (this->array.empty()) {
            if (USE_WARNING)
                std::cout << "stack_pop warning: underflow, return nullptr"
                          << std::endl;
            return nullptr;
        }
        return this->array.back();
    }
    /* number of elements in stack */
    inline auto count() const -> size_t { return this->array.size(); }
};

#endif
