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
 * queue.c
 *
 * A circular, expandable integer queue.
 *
 * @Author: Xin Chen
 * @Created on: 3/3/2007
 * @Last modified: 3/21/2007
 */

#include "y.hpp"
#include <iomanip>
#include <iostream>
#include <limits>
#include <optional>
#include <stdexcept>
#include <vector>

void
Queue::clear() noexcept
{
    this->start = 0;
    this->array.clear();
}

void
Queue::clear_all() noexcept
{
    this->clear();
    this->max_count = 0;
    this->sum_count = 0;
    this->call_count = 0;
}

void
Queue::push(size_t n)
{
    this->array.push_back(n);

    // std::cout << "push " << n << " (size=" << this->m_size << "): ";
    // this->dump();
    // std::cout << std::endl;

    if constexpr (DEBUG_QUEUE) {
        if (this->max_count < this->size())
            this->max_count = this->size();
        this->call_count++;
        this->sum_count += this->size();
    }
}

auto
Queue::pop() noexcept -> std::optional<size_t>
{
    if (this->size() == 0)
        return std::nullopt;

    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    size_t item = this->array[this->start];
    this->start += 1;
    if ((this->size() < this->array.size() / 2) &&
        (this->array.size() >= Queue::QUEUE_INIT_SIZE)) {
        this->set_start_to_0();
    }
    return item;
}

auto
Queue::peek() const noexcept -> std::optional<size_t>
{
    if (this->size() == 0)
        return std::nullopt;
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    return this->array[this->start];
}

auto
Queue::exist(const size_t n) const noexcept -> bool
{
    for (size_t i = 0; i < this->size(); i++) {
        const size_t item = this->get_no_check(i);
        if (item == n)
            return true;
    }
    return false;
}

void
Queue::dump() const noexcept
{
    for (size_t i = 0; i < this->size(); i++) {
        std::cout << "[" << i << "] " << this->get_no_check(i) << " ";
    }
    std::cout << std::endl;
}

void
Queue::info() const noexcept
{
    if constexpr (DEBUG_QUEUE) {
        std::cout << "queue_push is called " << this->call_count << " times, ";
        std::cout << "max count: " << this->max_count
                  << ", average count: " << std::setprecision(2)
                  << (static_cast<double>(this->sum_count)) /
                       static_cast<double>(this->call_count)
                  << std::endl;
    }
}

/// Move all elements to the start of the backing vector.
void
Queue::set_start_to_0()
{
    if (this->start != 0) {
        for (size_t i = 0; i < this->size(); i++) {
            this->array[i] = this->array[i + this->start];
        }
        this->start = 0;
    }
}