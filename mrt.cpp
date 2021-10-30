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
 * mrt.c
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
#include <iostream>
#include <memory>
#include <stdexcept>
#include <vector>

using MRLeaves = std::vector<std::shared_ptr<MRTreeNode>>;

std::shared_ptr<MRParents> all_parents;
std::vector<int> leaf_index_for_parent;
bool leaf_index_for_parent_done;

/* Function declarations. */

static void
get_all_mr_parents(const MRLeaves& mr_leaves);
static void
write_all_mr_parents(const MRLeaves& mr_leaves);

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

void
write_leaf_index_for_parent()
{
    for (int i = 0; i < all_parents->size(); i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%d", leaf_index_for_parent[i]);
    }
    yyprintf("\n");
}

/*
 * Find a string in a string array.
 * Returns the array index if found, -1 otherwise.
 */
auto
get_index_in_mr_parents(const SymbolTblNode* s, const MRParents& p) -> int
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
is_in_mr_parents(const SymbolTblNode* s, const MRParents& p) -> bool
{
    return (get_index_in_mr_parents(s, p) >= 0);
}

auto
is_parent_symbol(const SymbolTblNode* s) -> bool
{
    return is_in_mr_parents(s, *all_parents);
}

auto
MRTreeNode::find_node_in_tree(const std::shared_ptr<MRTreeNode>& node,
                              const SymbolTblNode* symbol)
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
find_node_in_forest(const MRLeaves& mr_leaves, SymbolTblNode* symbol)
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
MRTreeNode::create(SymbolTblNode* symbol) -> std::shared_ptr<MRTreeNode>
{
    auto node = std::make_shared<MRTreeNode>();
    node->parent.reserve(MR_TREE_NODE_INIT_PARENT_COUNT);
    node->symbol = SymbolNode::create(symbol);
    return node;
}

/*
 * Insert a new tree to the Multi-rooted forest.
 * The new tree's root node contains symbol parent,
 * and leaf node contains symbol child.
 */
void
insert_new_tree(MRLeaves& mr_leaves,
                SymbolTblNode* parent,
                SymbolTblNode* child)
{
    std::shared_ptr<MRTreeNode> leaf = MRTreeNode::create(child);
    leaf->parent.push_back(MRTreeNode::create(parent));

    // Insert node leaf to the mr_leaves array.
    mr_leaves.push_back(leaf);
}

void
MRTreeNode::insert_parent(SymbolTblNode* symbol)
{
    this->parent.push_back(MRTreeNode::create(symbol));
}

auto
MRTreeNode::is_mr_leaf(const MRLeaves& mr_leaves) noexcept -> int const
{
    for (int i = 0; i < mr_leaves.size(); i++) {
        if (this == mr_leaves[i].get())
            return i;
    }
    return -1;
}

void
write_branch(SymbolList branch)
{
    SymbolNode* a = nullptr;
    for (a = branch; a != nullptr; a = a->next) {
        if (a != branch)
            yyprintf(", ");
        yyprintf("%s", a->snode->symbol);
    }
    if (a != branch)
        yyprintf(", ");
}

void
MRTreeNode::write_leaf_branch(SymbolList branch, SymbolNode* branch_tail)
{
    yyprintf("%s", this->symbol->snode->symbol);
    if (this->parent.empty()) {
        yyprintf("\n");
        return;
    }
    yyprintf(", ");

    if (branch->next == nullptr) {
        branch_tail->next = branch->next =
          SymbolNode::create(this->symbol->snode);
    } else {
        branch_tail->next->next = SymbolNode::create(this->symbol->snode);
        branch_tail->next = branch_tail->next->next;
    }

    for (int i = 0; i < this->parent.size(); i++) {
        if (i > 0)
            write_branch(branch->next);
        this->parent[i]->write_leaf_branch(branch, branch_tail);
    }
}

void
write_mr_forest(const MRLeaves& mr_leaves)
{
    SymbolList branch = SymbolNode::create(hash_tbl_find(""));
    SymbolNode* branch_tail = SymbolNode::create(hash_tbl_find(""));

    yyprintf("\n==writeMRForest (mr_leaves_count: %d)==\n", mr_leaves.size());
    for (const auto& mr_leave : mr_leaves) {
        mr_leave->write_leaf_branch(branch, branch_tail);
    }

    free_symbol_node_list(branch);
    free_symbol_node(branch_tail);
}

void
MRTreeNode::insert_child(const std::shared_ptr<MRTreeNode> self,
                         MRLeaves& mr_leaves,
                         SymbolTblNode* symbol)
{
    std::shared_ptr<MRTreeNode> child = MRTreeNode::create(symbol);
    int leaf_index = self->is_mr_leaf(mr_leaves);

    if (leaf_index == -1) {
        // treeNode not a leaf, just insert child as a leaf.
        mr_leaves.push_back(child);
    } else {
        // change the pointer to treeNode to point to child
        mr_leaves[leaf_index] = child;
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
    int leaf_index = parent->is_mr_leaf(mr_leaves);
    if (leaf_index != -1) {
        mr_leaves.erase(mr_leaves.begin() + leaf_index);
    }
}

auto
build_multirooted_tree() -> MRLeaves
{
    // initialization.
    all_parents = create_mr_parents();
    init_array_leaf_index_for_parent();
    MRLeaves mr_leaves;
    mr_leaves.reserve(MR_LEAVES_INIT_MAX_COUNT);

    for (int i = 1; i < grammar.rules.size(); i++) {
        if (is_unit_production(i)) {
            std::shared_ptr<MRTreeNode> lhs =
              find_node_in_forest(mr_leaves, grammar.rules[i]->nLHS->snode);
            std::shared_ptr<MRTreeNode> rhs = find_node_in_forest(
              mr_leaves, grammar.rules[i]->nRHS_head->snode);
            if (lhs != nullptr && rhs == nullptr) {
                // insert rhs as child of lhs.
                MRTreeNode::insert_child(
                  lhs, mr_leaves, grammar.rules[i]->nRHS_head->snode);
            } else if (lhs == nullptr && rhs != nullptr) {
                // insert lhs as parent of rhs.
                rhs->insert_parent(grammar.rules[i]->nLHS->snode);
            } else if (lhs == nullptr && rhs == nullptr) {
                // insert as new tree.
                insert_new_tree(mr_leaves,
                                grammar.rules[i]->nLHS->snode,
                                grammar.rules[i]->nRHS_head->snode);
            } else { // just add this relationship.
                MRTreeNode::insert_parent_child_relation(
                  lhs, rhs.get(), mr_leaves);
            } // end if
        }     // end if
    }         // end for

    get_all_mr_parents(mr_leaves);

    if (Options::get().debug_build_multirooted_tree) {
        write_mr_forest(mr_leaves);
        write_all_mr_parents(mr_leaves);
    }
    return mr_leaves;
}

/*
 * Adds node itself and all its parent nodes to array
 * parents[] if not already in the array.
 *
 * Called by function getParentsForMRLeaf() only.
 */
void
get_node(int leaf_index, MRTreeNode* node, MRParents* parents)
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

        parents->push_back(SymbolNode::create(node->symbol->snode));
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
                        size_t leaf_index,
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

    for (int i = 0; i < mr_leaves.size(); i++) {
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
write_all_mr_parents(const MRLeaves& mr_leaves)
{
    yyprintf("\n==all MR Parents (inside '()' is a corresponding leaf):\n");
    for (int i = 0; i < all_parents->size(); i++) {
        yyprintf("%s (=>%s)\n",
                 (*all_parents)[i]->snode->symbol,
                 mr_leaves[leaf_index_for_parent[i]]->symbol->snode->symbol);
    }
    yyprintf("\n");
    // writeLeafIndexForParent();
}