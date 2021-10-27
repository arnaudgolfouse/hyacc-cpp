/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007, 2008 Xin Chen. chenx@hawaii.edu

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

#include "y.hpp"

#ifndef _MRT_H_
#define _MRT_H_

/*
 * mrt.h
 *
 * Functions for multi-rooted tree. Used by unit production elimination
 * algorithm.
 *
 * @Author: Xin Chen
 * @Date started: March 9, 2007
 * @Last modified: March 9, 2007
 */

/*
 * MRTreeNode.parent is a dynamically allocated array.
 */
struct MRTreeNode
{
    SymbolNode* symbol;
    MRTreeNode** parent; // dynamic array.
    int parent_count;
    int parent_max_count; // expand parent array if reached.
};

constexpr size_t MRTreeNode_INIT_PARENT_COUNT = 8;

/*
 * The Multi-rooted forest grows upon this array.
 * The Multi-rooted forest consists of 1 or more trees.
 * The leaves of the each tree are in this array,
 * the leaves then link to the internal nodes of the tree.
 */
extern MRTreeNode** MRLeaves;
extern int MRLeaves_count;
extern int MRLeaves_max_count;
constexpr size_t MRLeaves_INIT_MAX_COUNT = 8;

/*
 * Stores the parent symbols in all multirooted trees.
 *
 * Used in steps 3 and 5 of unit production removal
 * algorithm, to avoid having to traverse through
 * the MRTree to find out if a symbol is a parent symbol
 * each time.
 */

constexpr size_t MRParents_INIT_MAX_COUNT = 8;

struct MRParents
{
    SymbolNode** parents; // dynamic array.
    int count;
    int max_count;
};

extern MRParents* all_parents;

/*
 * Used in step 5.
 * Value obtained when calling getAllMRParents().
 * It eventually calls getNode(), which does the work.
 *
 * Used to retrieve a leaf for a parent.
 * There may be more than one leaf for a parent,
 * just choose the first one in the order of grammar rules.
 *
 * The correspondence relationship is:
 * all_parents[index] => MRLeaves[leafIndexForParent[index]]
 *
 * Has the same size as all_parents->max_count.
 */
extern int* leafIndexForParent;

/*
 * leafIndexForParent[] array is build in function
 * getNode(), which is called by getParentsForMRLeaf().
 * getParentsForMRLeaf() is called by functions
 * (1) getAllMRParents() and
 * (2) remove_unit_production_step1and2().
 * This boolean varaible is set to TRUE after calling (1),
 * so calling (2) won't change the values stored in
 * leafIndexForParent[].
 *
 * Used in functions getAllMRParents() and getNode().
 */
extern bool leafIndexForParent_Done;

/* function declarations */

extern auto
create_mr_parents() -> MRParents*;
extern auto
get_index_in_mr_parents(const SymbolTblNode* s, MRParents* p) -> int;
extern void
get_parents_for_mr_leaf(int leaf_index, MRParents* parents);
extern void
destroy_mr_parents(MRParents* p);
extern void
build_multirooted_tree();

#endif