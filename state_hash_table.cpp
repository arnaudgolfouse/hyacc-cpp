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
 * state_hash_table.c
 *
 * This hash table stores state numbers. It hashes a state by
 * a function of the rule index  and  marker  position of the
 * state's core configurations.
 *
 * This hash table is used to expidite the the process of fi-
 * nding a same or compatible state in the function addTrans-
 * itionStates2New().
 *
 * This is an alternative of the function  isExistingState(),
 * which uses linear search in all the states, and is slower.
 *
 * For example, for C.y, in -O0 (no optimization),  there are
 * 1605 states in the parsing machine,  so from the first  to
 * the last state as they are added, in average there will be
 * sum_{k=1_to_n}(k/2) = n(n+1)/4 searches,  where  n = 1605.
 * This is 644,407.
 *
 * But using this state hash table, in average there are 5.02
 * states per list (from StateHashTbl_dump() output),  so the
 * cost is (1 + 5.02) * 1605 = 9,662, where 1 is the  cost of
 * getting the hash value for a state.
 *
 * This is 644,407/9,662 = 67 times faster.
 *
 * @Author: Xin Chen
 * @Created on: 3/3/2006
 * @Last modified: 3/21/2007
 */

#include "y.hpp"
#include <array>
#include <fstream>
#include <iomanip>
#include <memory>
#include <ostream>
#include <unordered_set>
#include <utility>

void
StateHashTable::init() noexcept
{
    for (auto& cell : this->data) {
        cell.clear();
    }
}

/// The result is garanteed to be strictly less than `SHT_SIZE`.
auto
StateHashTable::get_value(const State& s) noexcept -> size_t
{
    // Magic hashing numbers
    constexpr size_t MAGIC_1 = 97;
    constexpr size_t MAGIC_2 = 7;
    size_t sum = 0;
    for (size_t i = 0; i < s.core_config_count; i++) {
        sum = (sum + static_cast<size_t>(s.config[i]->ruleID) * MAGIC_1 +
               static_cast<size_t>(s.config[i]->marker) * MAGIC_2 + i) %
              SHT_SIZE;
    }
    return sum;
}

/// @brief Search the state hash table for the state `s`.
///
/// @param s state to search
/// @param y_algorithm `YAlgorithm` structure, used if states need to be
/// combined.
///
/// If not found, insert it and return nullptr and false. else, return the found
/// state and true.
///
/// # Combine states
/// If the `use_combine_compatible_states` option is activated, this will also
/// search for a state that can be combined with `s`. If it is found, the two
/// states are combined, and this function additionally returns `true`.
///
/// # Safety
/// `s` must be valid.
auto
StateHashTable::search(const std::shared_ptr<State>& s,
                       YAlgorithm& y_algorithm) noexcept
  -> std::pair<std::shared_ptr<State>, bool>
{
    const size_t v = StateHashTable::get_value(*s);
    auto& bucket = this->data.at(v);

    for (const auto& state : bucket) {
        if (is_same_state(*state, *s)) {
            return { state, false };
        }
        if (y_algorithm.options.use_combine_compatible_states) {
            if (is_compatible_states(state.get(), s.get())) {
                y_algorithm.combine_compatible_states(state, *s);
                return { state, true };
            }
        }
    }

    bucket.emplace_back(s);
    return { nullptr, false };
}

/// Search the state hash table for state s.
/// If not found, insert it and return nullptr.
/// else, return the found state.
///
/// Is similar to `StateHashTable::search`, but does not use
/// weak compatibility.
auto
StateHashTable::search_same_state(std::shared_ptr<State> s)
  -> std::shared_ptr<State>
{
    const size_t v = StateHashTable::get_value(*s);
    auto& bucket = this->data.at(v);

    for (const auto& state : bucket) {
        if (is_same_state(*state, *s)) {
            return state;
        }
    }

    bucket.emplace_back(s);
    return nullptr;
}

/*
 * load factor: number of entry / hash table size.
 * hash table cell usage:
 *   number of used hash table cells / hash table size.
 */
void
StateHashTable::dump(std::ostream& os) const noexcept
{
    size_t states_count = 0, list_count = 0;

    os << std::endl << "--state hash table--\n";
    os << "-----------------------" << std::endl;
    os << "cell |   count  | state" << std::endl;
    os << "-----------------------" << std::endl;
    size_t i = 0;
    for (const auto& bucket : this->data) {
        if (bucket.empty())
            continue;

        list_count++;
        states_count += bucket.size();

        os << "[" << i << "] (count=" << bucket.size() << ") : ";
        bool first_element = true;
        for (const auto& element : bucket) {
            if (first_element) {
                first_element = false;
            } else {
                os << ", ";
            }
            os << element->state_no;
        }
        os << std::endl;
        i++;
    }

    os << states_count << " states, " << list_count << " lists, in average "
       << std::setprecision(2)
       << static_cast<double>(states_count) / static_cast<double>(list_count)
       << " states/list." << std::endl;
    os << "load factor: " << std::setprecision(2)
       << ((double)states_count) / SHT_SIZE
       << ", hash table cell usage: " << std::setprecision(2)
       << ((double)list_count) / SHT_SIZE << std::endl;
}
