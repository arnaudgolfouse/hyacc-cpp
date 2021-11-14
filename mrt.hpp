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
#include <memory>
#include <ostream>
#include <vector>

#pragma once

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

using MRLeaves = std::vector<std::shared_ptr<struct MRTreeNode>>;

/*
 * MRTreeNode.parent is a dynamically allocated array.
 */
struct MRTreeNode
{
  public:
    std::shared_ptr<SymbolNode> symbol;
    std::vector<std::shared_ptr<MRTreeNode>> parent;

    static auto create(std::shared_ptr<SymbolTableNode> symbol)
      -> std::shared_ptr<MRTreeNode>;
    // Prints out all node sequences starting from
    // the given node to its ancestors.
    void write_leaf_branch(std::ostream& os,
                           SymbolList& branch,
                           SymbolList& branch_tail);
    //  Insert a child node of treeNode.
    // The child node contains the given symbol.
    static void insert_child(std::shared_ptr<MRTreeNode> self,
                             MRLeaves& mr_leaves,
                             std::shared_ptr<SymbolTableNode> symbol);
    void insert_parent(std::shared_ptr<SymbolTableNode> symbol);
    // Returns the index in array MRLeaves[] if given node
    // is a leaf, otherwise returns -1.
    [[nodiscard]] auto is_mr_leaf(const MRLeaves& mr_leaves) const noexcept
      -> std::optional<size_t>;
    // Both parent and child nodes are in the Multi-rooted
    // forest already, just add the link between them.
    static void insert_parent_child_relation(std::shared_ptr<MRTreeNode> parent,
                                             MRTreeNode* child,
                                             MRLeaves& mr_leaves);
    static auto find_node_in_tree(const std::shared_ptr<MRTreeNode>& node,
                                  std::shared_ptr<const SymbolTableNode> symbol)
      -> std::shared_ptr<MRTreeNode>;
};

constexpr size_t MR_TREE_NODE_INIT_PARENT_COUNT = 8;

/*
 * The Multi-rooted forest grows upon this array.
 * The Multi-rooted forest consists of 1 or more trees.
 * The leaves of the each tree are in this array,
 * the leaves then link to the internal nodes of the tree.
 */
// extern std::vector<std::shared_ptr<MRTreeNode>> MRLeaves;
constexpr size_t MR_LEAVES_INIT_MAX_COUNT = 8;

/*
 * Stores the parent symbols in all multirooted trees.
 *
 * Used in steps 3 and 5 of unit production removal
 * algorithm, to avoid having to traverse through
 * the MRTree to find out if a symbol is a parent symbol
 * each time.
 */

constexpr size_t MR_PARENTS_INIT_MAX_COUNT = 8;

using MRParents = std::vector<std::shared_ptr<SymbolNode>>;

extern std::shared_ptr<MRParents> all_parents;

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
 * Has the same size as all_parents->capacity().
 */
extern std::vector<size_t> leaf_index_for_parent;

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
extern bool leaf_index_for_parent_done;

/* function declarations */

extern auto
create_mr_parents() -> std::shared_ptr<MRParents>;
extern auto
get_index_in_mr_parents(std::shared_ptr<const SymbolTableNode> s,
                        const MRParents& p) -> int;
extern void
get_parents_for_mr_leaf(const MRLeaves& mr_leaves,
                        size_t leaf_index,
                        MRParents* parents);
