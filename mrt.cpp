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

/*
 * mrt.cpp
 *
 * Functions for multi-rooted tree. Used by unit production elimination
 * algorithm.
 *
 * @Author: Xin Chen
 * @Date started: March 9, 2007
 * @Last modified: March 9, 2007
 */

#include "mrt.hpp"
#include "y.hpp"
#include <cstddef>
#include <fstream>
#include <iostream>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <vector>

using MRLeaves = std::vector<std::shared_ptr<MRTreeNode>>;

std::shared_ptr<MRParents> all_parents;
std::vector<size_t> leaf_index_for_parent;
bool leaf_index_for_parent_done;

/* Function declarations. */

static void
get_all_mr_parents(const MRLeaves& mr_leaves);
static void
write_all_mr_parents(std::ostream& os, const MRLeaves& mr_leaves);

////////////////////////////////////////////
// Functions for multi-rooted tree. START.
////////////////////////////////////////////

void
init_array_leaf_index_for_parent()
{
    leaf_index_for_parent.clear();
    leaf_index_for_parent.reserve(MR_PARENTS_INIT_MAX_COUNT);
}

void
expand_array_leaf_index_for_parent()
{
    leaf_index_for_parent.clear();
    leaf_index_for_parent.reserve(all_parents->capacity());

    if (Options::get().debug_expand_array)
        std::cout << "leaf_index_for_parent size expanded to "
                  << all_parents->capacity() << std::endl;
}

void
destroy_leaf_index_for_parent()
{
    leaf_index_for_parent.clear();
}

auto
create_mr_parents() -> std::shared_ptr<MRParents>
{
    auto p = std::make_shared<MRParents>();
    p->reserve(MR_PARENTS_INIT_MAX_COUNT);
    return p;
}

static void
check_array_size_mr_parents(const MRParents* p)
{
    // if all_parents array is expanded, expand leafIndexForParent too.
    if (p == all_parents.get())
        expand_array_leaf_index_for_parent();
}

/*
 * Find a string in a string array.
 * Returns the array index if found, -1 otherwise.
 */
auto
get_index_in_mr_parents(const std::shared_ptr<const SymbolTableNode> s,
                        const MRParents& p) -> int
{
    int i = 0;
    for (const auto& parent : p) {
        if (s == parent->snode)
            return i;
        i++;
    }
    return -1;
}

/*
 * Determines if string s is in an array a of length count.
 */
auto
is_in_mr_parents(const std::shared_ptr<const SymbolTableNode> s,
                 const MRParents& p) -> bool
{
    return (get_index_in_mr_parents(s, p) >= 0);
}

auto
is_parent_symbol(const std::shared_ptr<const SymbolTableNode> s) -> bool
{
    return is_in_mr_parents(s, *all_parents);
}

auto
MRTreeNode::find_node_in_tree(
  const std::shared_ptr<MRTreeNode>& node,
  const std::shared_ptr<const SymbolTableNode> symbol)
  -> std::shared_ptr<MRTreeNode>
{
    if (node == nullptr) {
        // std::cout << "findNodeInTree warning: node is null" << std::endl;
        return nullptr;
    }
    if (node->symbol->snode == symbol) {
        return node;
    } // Search parent nodes.
    for (const auto& i : node->parent) {
        const auto p_node = find_node_in_tree(i, symbol);
        if (p_node != nullptr)
            return p_node;
    }
    return nullptr;
}

auto
find_node_in_forest(const MRLeaves& mr_leaves,
                    std::shared_ptr<SymbolTableNode> symbol)
  -> std::shared_ptr<MRTreeNode>
{
    for (const auto& mr_leave : mr_leaves) {
        const std::shared_ptr<MRTreeNode> node =
          MRTreeNode::find_node_in_tree(mr_leave, symbol);
        if (node != nullptr)
            return node;
    }
    return nullptr;
}

auto
MRTreeNode::create(std::shared_ptr<SymbolTableNode> symbol)
  -> std::shared_ptr<MRTreeNode>
{
    auto node = std::make_shared<MRTreeNode>();
    node->parent.reserve(MR_TREE_NODE_INIT_PARENT_COUNT);
    node->symbol = std::make_shared<SymbolNode>(symbol);
    return node;
}

/*
 * Insert a new tree to the Multi-rooted forest.
 * The new tree's root node contains symbol parent,
 * and leaf node contains symbol child.
 */
void
insert_new_tree(MRLeaves& mr_leaves,
                const std::shared_ptr<SymbolTableNode> parent,
                const std::shared_ptr<SymbolTableNode> child)
{
    std::shared_ptr<MRTreeNode> leaf = MRTreeNode::create(child);
    leaf->parent.push_back(MRTreeNode::create(parent));

    // Insert node leaf to the mr_leaves array.
    mr_leaves.push_back(leaf);
}

void
MRTreeNode::insert_parent(std::shared_ptr<SymbolTableNode> symbol)
{
    this->parent.push_back(MRTreeNode::create(symbol));
}

auto
MRTreeNode::is_mr_leaf(const MRLeaves& mr_leaves) const noexcept
  -> std::optional<size_t>
{
    for (size_t i = 0; i < mr_leaves.size(); i++) {
        if (this == mr_leaves[i].get())
            return i;
    }
    return std::nullopt;
}

static void
write_branch(std::ostream& os, const SymbolList& branch)
{
    for (const auto& a : branch) {
        os << a.snode->symbol << ", ";
    }
}

void
MRTreeNode::write_leaf_branch(std::ostream& os,
                              SymbolList& branch,
                              SymbolList& branch_tail)
{
    os << this->symbol->snode->symbol;
    if (this->parent.empty()) {
        os << std::endl;
        return;
    }
    os << ", ";

    if (branch.size() == 1) {
        branch.emplace_back(this->symbol->snode);
        branch_tail.emplace_back(this->symbol->snode);
    } else {
        SymbolList new_branch_tail;
        new_branch_tail.push_back(branch_tail.front());
        new_branch_tail.emplace_back(this->symbol->snode);
        branch_tail = new_branch_tail;
    }

    for (size_t i = 0; i < this->parent.size(); i++) {
        if (i > 0)
            write_branch(os, branch); // TODO: should be branch.next !
        this->parent[i]->write_leaf_branch(os, branch, branch_tail);
    }
}

static void
write_mr_forest(std::ostream& os, const MRLeaves& mr_leaves)
{
    SymbolList branch;
    SymbolList branch_tail;
    branch.emplace_back(hash_tbl_find(""));
    branch_tail.emplace_back(hash_tbl_find(""));

    os << std::endl
       << "==writeMRForest (mr_leaves_co" << std::endl
       << "t: " << mr_leaves.size() << ")==" << std::endl;
    for (const auto& mr_leave : mr_leaves) {
        mr_leave->write_leaf_branch(os, branch, branch_tail);
    }
}

void
MRTreeNode::insert_child(const std::shared_ptr<MRTreeNode> self,
                         MRLeaves& mr_leaves,
                         std::shared_ptr<SymbolTableNode> symbol)
{
    std::shared_ptr<MRTreeNode> child = MRTreeNode::create(symbol);
    std::optional<size_t> leaf_index = self->is_mr_leaf(mr_leaves);

    if (!leaf_index.has_value()) {
        // treeNode not a leaf, just insert child as a leaf.
        mr_leaves.push_back(child);
    } else {
        // change the pointer to treeNode to point to child
        mr_leaves[*leaf_index] = child;
    }
    child->parent.push_back(self);
}

void
MRTreeNode::insert_parent_child_relation(
  const std::shared_ptr<MRTreeNode> parent,
  MRTreeNode* child,
  MRLeaves& mr_leaves)
{
    child->parent.push_back(parent);

    // if parent node is a leaf, remove it from the leaves array.
    std::optional<size_t> leaf_index = parent->is_mr_leaf(mr_leaves);
    if (leaf_index.has_value()) {
        mr_leaves.erase(mr_leaves.begin() +
                        static_cast<ptrdiff_t>(*leaf_index));
    }
}

auto
build_multirooted_tree(const Grammar& grammar) -> MRLeaves
{
    // initialization.
    all_parents = create_mr_parents();
    init_array_leaf_index_for_parent();
    MRLeaves mr_leaves;
    mr_leaves.reserve(MR_LEAVES_INIT_MAX_COUNT);

    for (size_t i = 1; i < grammar.rules.size(); i++) {
        if (grammar.is_unit_production(i)) {
            std::shared_ptr<MRTreeNode> lhs =
              find_node_in_forest(mr_leaves, grammar.rules[i]->nLHS->snode);
            std::shared_ptr<MRTreeNode> rhs = find_node_in_forest(
              mr_leaves, grammar.rules[i]->nRHS.front().snode);
            if (lhs != nullptr && rhs == nullptr) {
                // insert rhs as child of lhs.
                MRTreeNode::insert_child(
                  lhs, mr_leaves, grammar.rules[i]->nRHS.front().snode);
            } else if (lhs == nullptr && rhs != nullptr) {
                // insert lhs as parent of rhs.
                rhs->insert_parent(grammar.rules[i]->nLHS->snode);
            } else if (lhs == nullptr && rhs == nullptr) {
                // insert as new tree.
                insert_new_tree(mr_leaves,
                                grammar.rules[i]->nLHS->snode,
                                grammar.rules[i]->nRHS.front().snode);
            } else { // just add this relationship.
                MRTreeNode::insert_parent_child_relation(
                  lhs, rhs.get(), mr_leaves);
            } // end if
        }     // end if
    }         // end for

    get_all_mr_parents(mr_leaves);

    if (Options::get().debug_build_multirooted_tree) {
        write_mr_forest(grammar.fp_v, mr_leaves);
        write_all_mr_parents(grammar.fp_v, mr_leaves);
    }
    return mr_leaves;
}

/// Adds node itself and all its parent nodes to array
/// parents[] if not already in the array.
///
/// Called by function getParentsForMRLeaf() only.
static void
get_node(const size_t leaf_index, MRTreeNode* node, MRParents* parents)
{
    if (node == nullptr) {
        // std::cout << "getNode warning: node is nullptr" << std::endl;
        return;
    }

    if (is_in_mr_parents(node->symbol->snode, *parents) == false) {
        check_array_size_mr_parents(parents); // expand size if needed.
        if (leaf_index_for_parent_done == false) {
            leaf_index_for_parent[parents->size()] = leaf_index;
        }
        parents->push_back(std::make_shared<SymbolNode>(node->symbol->snode));
    }

    for (const auto& parent : node->parent) {
        get_node(leaf_index, parent.get(), parents);
    }
}

/*
 * Obtains an array of all the parent nodes of the
 * give leaf node.
 *
 * Note: so far this is called for each cycle of states,
 *       so may not be very efficient.
 */
void
get_parents_for_mr_leaf(const MRLeaves& mr_leaves,
                        const size_t leaf_index,
                        MRParents* parents)
{
    for (const auto& parent : mr_leaves[leaf_index]->parent) {
        get_node(leaf_index, parent.get(), parents);
    }
}

/*
 * Get all the parent nodes in the multi-rooted forest.
 */
void
get_all_mr_parents(const MRLeaves& mr_leaves)
{
    leaf_index_for_parent_done = false;

    for (size_t i = 0; i < mr_leaves.size(); i++) {
        get_parents_for_mr_leaf(mr_leaves, i, all_parents.get());
    }

    leaf_index_for_parent_done = true;
}

/*
 * Prints a list of all the parent nodes in the
 * multi-rooted forest, as well as the leaf
 * of each parent in parenthesis.
 */
void
write_all_mr_parents(std::ostream& os, const MRLeaves& mr_leaves)
{
    os << std::endl
       << "==all MR Par" << std::endl
       << "ts (" << std::endl
       << "side '()' is a corresp" << std::endl
       << "d" << std::endl
       << "g leaf):\n";
    for (size_t i = 0; i < all_parents->size(); i++) {
        os << (*all_parents)[i]->snode->symbol << " (=>"
           << mr_leaves[leaf_index_for_parent[i]]->symbol->snode->symbol << ")"
           << std::endl;
    }
    os << std::endl;
}