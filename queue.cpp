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
#include <stdexcept>

constexpr size_t QUEUE_INIT_SIZE = 256; // hidden from outside
constexpr bool DEBUG_QUEUE = false;

auto
Queue::create() -> Queue*
{
    Queue* q = new Queue;
    if (q == nullptr) {
        throw std::runtime_error("out of memory");
    }
    q->capacity = QUEUE_INIT_SIZE;
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    q->array = new int[q->capacity];
    q->size = q->start = 0;
    q->max_count = q->sum_count = q->call_count = 0;
    return q;
}

void
Queue::expand()
{
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    auto* new_array = new int[this->capacity * 2];
    size_t this_i = this->start;
    for (size_t i = 0; i < this->size; i++, this_i++) {
        if (this_i == this->capacity) {
            this_i = 0;
        }
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        new_array[i] = this->array[this_i];
    }
    delete[] this->array;
    this->array = new_array;
    this->start = 0;
    this->capacity *= 2;
}

void
Queue::destroy(Queue* q) noexcept
{
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    delete[] q->array;
    delete q;
}

void
Queue::clear() noexcept
{
    this->size = 0;
    this->start = 0;
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
Queue::push(int n)
{
    if (n == QUEUE_ERR_CODE) {
        throw std::runtime_error(
          "Queue error: push a number that equals QUEUE_ERR_CODE");
    }
    if (this->size == this->capacity) {
        this->expand();
    }
    this->get_no_check(this->size) = n;
    this->size += 1;

    // std::cout << "push " << n << " (size=" << this->size << "): ";
    // this->dump();
    // std::cout << std::endl;

    if constexpr (DEBUG_QUEUE) {
        if (this->max_count < this->size)
            this->max_count = this->size;
        this->call_count++;
        this->sum_count += this->size;
    }
}

auto
Queue::pop() noexcept -> int
{
    if (this->size == 0)
        return QUEUE_ERR_CODE;

    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    int item = this->array[this->start];
    this->start += 1;
    if (this->start == this->capacity) {
        this->start = 0;
    }
    this->size -= 1;
    // std::cout << "pop " << item << " (size=" << this->size << "): ";
    // this->dump();
    // std::cout << std::endl;
    return item;
}

auto
Queue::peek() const noexcept -> int
{
    if (this->size == 0)
        return QUEUE_ERR_CODE;
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    return this->array[this->start];
}

auto
Queue::exist(const int n) const noexcept -> bool
{
    for (size_t i = 0; i < this->size; i++) {
        const int item = this->get_no_check(i);
        if (item == n)
            return true;
    }
    return false;
}

void
Queue::dump() const noexcept
{
    for (size_t i = 0; i < this->size; i++) {
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
