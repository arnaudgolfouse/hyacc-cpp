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
#include <iostream>
#include <stdexcept>

constexpr size_t QUEUE_INIT_SIZE = 256; // hidden from outside
#define DEBUG_QUEUE 0

auto
queue_create() -> Queue*
{
    Queue* q = new Queue;
    if (q == nullptr) {
        throw std::runtime_error("out of memory");
    }

    q->size = QUEUE_INIT_SIZE;
    q->array = new int[q->size];
    if (q->array == nullptr) {
        throw std::runtime_error("out of memory");
    }

    q->head = 0;
    q->tail = 0;
    q->count = 0;

    q->max_count = q->sum_count = q->call_count = 0;

    return q;
}

void
queue_destroy(Queue* q)
{
    delete[] q->array;
    delete q;
}

void
queue_clear(Queue* q)
{
    q->head = 0;
    q->tail = 0;
    q->count = 0;
}

void
queue_clear_all(Queue* q)
{
    q->head = 0;
    q->tail = 0;
    q->count = 0;
    q->max_count = q->sum_count = q->call_count = 0;
}

void
queue_expand(Queue* q)
{
    if (q->count < q->size)
        return;
    int* new_array = new int[2 * q->size];
    if (new_array == nullptr) {
        throw std::runtime_error("out of memory");
    }

    int* old_array = q->array;

    // copy
    int pt = q->head;
    int i = 0;
    for (; i < q->count; i++) {
        new_array[i] = q->array[pt];
        pt++;
        if (pt == q->size)
            pt = 0;
    }

    q->array = new_array;
    q->size *= 2;
    q->head = 0;
    q->tail = i;

    delete[] old_array;

    // printf("expand queue size to %d: ", q->size);
    // queue_dump(q); printf("\n");
}

/*
 * push at tail.
 */
void
queue_push(Queue* q, int n)
{
    if (q->count >= q->size)
        queue_expand(q);

    if (n == QUEUE_ERR_CODE) {
        throw std::runtime_error(
          "Queue error: push a number that equals QUEUE_ERR_CODE");
    }

    q->array[q->tail] = n;
    q->tail++;
    q->count++;

    // wrap around.
    if (q->tail == q->size)
        q->tail = 0;

        // printf("push %d (size=%d): ", n, q->count);
        // queue_dump(q); printf("\n");

#if DEBUG_QUEUE
    if (q->max_count < q->count)
        q->max_count = q->count;
    q->call_count++;
    q->sum_count += q->count;
#endif
}

/*
 * pop at front
 */
auto
queue_pop(Queue* q) -> int
{
    if (q->count == 0)
        return QUEUE_ERR_CODE;

    int item = q->array[q->head];
    q->head++;
    if (q->head == q->size)
        q->head = 0;

    q->count--;

    // printf("pop %d (size=%d): ", item, q->count);
    // queue_dump(q); printf("\n");

    return item;
}

auto
queue_peek(Queue* q) -> int
{
    if (q->count == 0)
        return QUEUE_ERR_CODE;
    return q->array[q->head];
}

auto
queue_count(Queue* q) -> int
{
    return q->count;
}

/*
 * Check if number n exists in the q.
 */
auto
queue_exist(Queue* q, int n) -> int
{
    int pt = q->head;
    for (int i = 0; i < q->count; i++) {
        if (q->array[pt] == n)
            return 1;
        pt++;
        if (pt == q->size)
            pt = 0;
    }
    return 0;
}

void
queue_dump(Queue* q)
{
    int pt = q->head;
    for (int i = 0; i < q->count; i++) {
        std::cout << "[" << pt << "] " << q->array[pt] << " ";
        pt++;
        if (pt == q->size)
            pt = 0;
    }
    std::cout << std::endl;
}

/*
 * Print the usage information of the queue.
 */
void
queue_info(Queue* q)
{
#if DEBUG_QUEUE
    printf("queue_push is called %d times, ", q->call_count);
    printf("max count: %d, average count: %.2f\n",
           q->max_count,
           ((double)q->sum_count) / q->call_count);
#endif
}
