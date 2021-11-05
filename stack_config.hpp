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

#pragma once

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
#include <iostream>
#include <vector>

struct Stack
{
    std::vector<std::optional<Configuration*>> array;

    explicit Stack() { this->array.reserve(Stack::INIT_SIZE); }

    void dump(const Grammar& grammar) const noexcept;
    inline void push(std::optional<Configuration*> n)
    {
        this->array.push_back(n);
    }
    /// Pop the element at the top of the stack.
    ///
    /// If the stack if empty, this will return `nullptr`.
    inline auto pop() -> std::optional<Configuration*>
    {
        if (this->array.empty()) {
            if (Stack::USE_WARNING)
                std::cerr << "Stack::pop warning: underflow, return nullptr"
                          << std::endl;
            return nullptr;
        }
        auto last = this->array.back();
        this->array.pop_back();
        return last;
    }
    /// Get the element at the top of the stack.
    ///
    /// If the stack if empty, this will return `nullptr`.
    [[nodiscard]] inline auto top() const -> std::optional<Configuration*>
    {
        if (this->array.empty()) {
            if (Stack::USE_WARNING)
                std::cerr << "Stack::top warning: underflow, return nullptr"
                          << std::endl;
            return nullptr;
        }
        return this->array.back();
    }
    /* number of elements in stack */
    [[nodiscard]] inline auto count() const -> size_t
    {
        return this->array.size();
    }

  private:
    constexpr static size_t INIT_SIZE = 256;
    constexpr static bool USE_WARNING = false;
};
