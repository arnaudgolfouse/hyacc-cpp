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
#include <iostream>
#include <stdexcept>

MRTreeNode** MRLeaves;
int MRLeaves_count;
int MRLeaves_max_count;
MRParents* all_parents;
int* leafIndexForParent;
bool leafIndexForParent_Done;

/* Function declarations. */

void
get_all_mr_parents();
void
write_all_mr_parents();

////////////////////////////////////////////
// Functions for multi-rooted tree. START.
////////////////////////////////////////////

void
init_array_leaf_index_for_parent()
{
    leafIndexForParent = new int[MRParents_INIT_MAX_COUNT];
    if (leafIndexForParent == nullptr)
        YYERR_EXIT("initArray_leafIndexForparent error: out of memory\n");
}

void
expand_array_leaf_index_for_parent()
{
    delete[] leafIndexForParent;
    leafIndexForParent = new int[all_parents->max_count];
    if (leafIndexForParent == nullptr)
        YYERR_EXIT("expandArray_leafIndexForparent error: out of memory\n");

    if (Options::get().debug_expand_array)
        std::cout << "leafIndexForParent size expanded to "
                  << all_parents->max_count << std::endl;
}

void
destroy_leaf_index_for_parent()
{
    delete[] leafIndexForParent;
}

auto
create_mr_parents() -> MRParents*
{
    MRParents* p = (MRParents*)malloc(sizeof(MRParents));
    if (p == nullptr)
        YYERR_EXIT("createMRParents error: out of memory\n");

    p->max_count = MRParents_INIT_MAX_COUNT;
    p->parents = (SymbolNode**)malloc(sizeof(SymbolNode*) * p->max_count);
    if (p->parents == nullptr)
        YYERR_EXIT("createMRParents error: out of memory\n");
    p->count = 0;

    return p;
}

void
check_array_size_mr_parents(MRParents* p)
{
    if (p->count < p->max_count)
        return;

    p->max_count *= 2;
    delete[] p->parents;
    p->parents = new SymbolNode*[p->max_count];

    if (p->parents == nullptr)
        YYERR_EXIT("checkArraySize_MRParents error: out of meory\n");

    if (Options::get().debug_expand_array)
        std::cout << "MRParents size expanded to " << p->max_count << std::endl;

    // if all_parents array is expanded, expand leafIndexForParent too.
    if (p == all_parents)
        expand_array_leaf_index_for_parent();
}

void
destroy_mr_parents(MRParents* p)
{
    if (p == nullptr)
        return;
    for (int i = 0; i < p->count; i++)
        delete p->parents[i];
    delete[] p->parents;
    delete p;
}

void
create_mr_leaves_array()
{
    MRLeaves_max_count = MRLeaves_INIT_MAX_COUNT;
    MRLeaves = new MRTreeNode*[MRLeaves_max_count];
    if (MRLeaves == nullptr)
        YYERR_EXIT("createMRLeavesArray error: out of memory\n");
}

void
check_mr_leaves_array_size()
{
    if (MRLeaves_count < MRLeaves_max_count)
        return;

    MRLeaves_max_count *= 2;
    delete[] MRLeaves;
    MRLeaves = new MRTreeNode*[MRLeaves_max_count];
    if (MRLeaves == nullptr)
        YYERR_EXIT("checkMRLeavesArraySize error: out of memory\n");

    if (Options::get().debug_expand_array)
        yyprintf("MRLeaves size expanded to %d\n", MRLeaves_max_count);
}

void
destroy_mr_leaves_array()
{
    delete[] MRLeaves;
}

void
write_leaf_index_for_parent()
{
    for (int i = 0; i < all_parents->count; i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%d", leafIndexForParent[i]);
    }
    yyprintf("\n");
}

/*
 * Find a string in a string array.
 * Returns the array index if found, -1 otherwise.
 */
auto
get_index_in_mr_parents(const SymbolTblNode* s, MRParents* p) -> int
{
    for (int i = 0; i < p->count; i++) {
        if (s == p->parents[i]->snode)
            return i;
    }
    return -1;
}

/*
 * Determines if string s is in an array a of length count.
 */
auto
is_in_mr_parents(const SymbolTblNode* s, MRParents* p) -> bool
{
    return (get_index_in_mr_parents(s, p) >= 0);
}

auto
is_parent_symbol(const SymbolTblNode* s) -> bool
{
    return is_in_mr_parents(s, all_parents);
}

auto
find_node_in_tree(MRTreeNode* node, SymbolTblNode* symbol) -> MRTreeNode*
{
    if (node == nullptr) {
        // printf("findNodeInTree warning: node is null\n");
        return nullptr;
    }
    if (node->symbol->snode == symbol) {
        return node;
    } // Search parent nodes.
    for (int i = 0; i < node->parent_count; i++) {
        MRTreeNode* p_node = find_node_in_tree(node->parent[i], symbol);
        if (p_node != nullptr)
            return p_node;
    }
    return nullptr;
}

auto
find_node_in_forest(SymbolTblNode* symbol) -> MRTreeNode*
{
    for (int i = 0; i < MRLeaves_count; i++) {
        MRTreeNode* node = find_node_in_tree(MRLeaves[i], symbol);
        if (node != nullptr)
            return node;
    }
    return nullptr;
}

auto
create_mr_tree_node(SymbolTblNode* symbol) -> MRTreeNode*
{
    MRTreeNode* node = new MRTreeNode;
    if (node == nullptr) {
        throw std::runtime_error("createMRTreeNode error: out of memory");
    }
    node->parent = new MRTreeNode*[MRTreeNode_INIT_PARENT_COUNT];
    if (node->parent == nullptr) {
        YYERR_EXIT("createMRTreeNode error: out of memory\n");
    }
    node->parent_count = 0;
    node->parent_max_count = MRTreeNode_INIT_PARENT_COUNT;
    node->symbol = create_symbol_node(symbol);
    return node;
}

void
destroy_mr_tree_node(MRTreeNode* node)
{
    if (node == nullptr)
        return;
    delete node->symbol;
    delete[] node->parent;
    delete node;
}

/*
 * Insert a new tree to the Multi-rooted forest.
 * The new tree's root node contains symbol parent,
 * and leaf node contains symbol child.
 */
void
insert_new_tree(SymbolTblNode* parent, SymbolTblNode* child)
{
    MRTreeNode* leaf = create_mr_tree_node(child);
    leaf->parent[0] = create_mr_tree_node(parent);
    leaf->parent_count = 1;

    // Insert node leaf to the MRLeaves array.
    check_mr_leaves_array_size();
    MRLeaves[MRLeaves_count] = leaf;
    MRLeaves_count++;
}

/*
 * If the parent array of node reaches size limit,
 * expand it.
 */
void
check_parent_array_size(MRTreeNode* node)
{
    if (node->parent_count < node->parent_max_count)
        return;

    node->parent_max_count *= 2;
    delete[] node->parent;
    node->parent = new MRTreeNode*[node->parent_max_count];
    if (node->parent == nullptr)
        YYERR_EXIT("checkParentArraySize: out of memory\n");

    if (Options::get().debug_expand_array)
        std::cout << "MRTreeNode.parent size expanded to "
                  << node->parent_max_count << std::endl;
}

void
insert_parent(MRTreeNode* node, SymbolTblNode* symbol)
{
    check_parent_array_size(node);
    MRTreeNode* parent = create_mr_tree_node(symbol);
    node->parent[node->parent_count] = parent;
    node->parent_count++;
}

/*
 * Returns the index in array MRLeaves[] if given node
 * is a leaf, otherwise returns -1.
 */
auto
is_mr_leaf(MRTreeNode* node) -> int
{
    for (int i = 0; i < MRLeaves_count; i++) {
        if (node == MRLeaves[i])
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

/*
 * Prints out all node sequences starting from
 * the given node to its ancestors.
 */
void
write_leaf_branch(MRTreeNode* node, SymbolList branch, SymbolNode* branch_tail)
{

    if (node == nullptr) {
        // printf("writeLeafBranch warning: node is nullptr\n");
        return;
    }
    yyprintf("%s", node->symbol->snode->symbol);
    if (node->parent_count == 0) {
        yyprintf("\n");
        return;
    }
    yyprintf(", ");

    if (branch->next == nullptr) {
        branch_tail->next = branch->next =
          create_symbol_node(node->symbol->snode);
    } else {
        branch_tail->next->next = create_symbol_node(node->symbol->snode);
        branch_tail->next = branch_tail->next->next;
    }

    for (int i = 0; i < node->parent_count; i++) {
        if (i > 0)
            write_branch(branch->next);
        write_leaf_branch(node->parent[i], branch, branch_tail);
    }
}

void
write_mr_forest()
{
    SymbolList branch = create_symbol_node(hash_tbl_find(""));
    SymbolNode* branch_tail = create_symbol_node(hash_tbl_find(""));

    yyprintf("\n==writeMRForest (MRLeaves_count: %d)==\n", MRLeaves_count);
    for (int i = 0; i < MRLeaves_count; i++) {
        write_leaf_branch(MRLeaves[i], branch, branch_tail);
    }

    free_symbol_node_list(branch);
    free_symbol_node(branch_tail);
}

/*
 * Insert a child node of treeNode.
 * The child node contains the given symbol.
 */
void
insert_child(MRTreeNode* tree_node, SymbolTblNode* symbol)
{
    MRTreeNode* child = create_mr_tree_node(symbol);
    int leaf_index = is_mr_leaf(tree_node);

    if (leaf_index == -1) {
        // treeNode not a leaf, just insert child as a leaf.
        check_mr_leaves_array_size();
        MRLeaves[MRLeaves_count] = child;
        MRLeaves_count++;
    } else {
        // change the pointer to treeNode to point to child
        MRLeaves[leaf_index] = child;
    }

    child->parent[0] = tree_node;
    child->parent_count = 1;
}

/*
 * Both parent and child nodes are in the Multi-rooted
 * forest already, just add the link between them.
 */
void
insert_parent_child_relation(MRTreeNode* parent, MRTreeNode* child)
{
    check_parent_array_size(child);
    child->parent[child->parent_count] = parent;
    child->parent_count++;

    // if parent node is a leaf, remove it from the leaves array.
    int leaf_index = is_mr_leaf(parent);
    if (leaf_index != -1) {
        for (int i = leaf_index; i < MRLeaves_count - 1; i++) {
            MRLeaves[i] = MRLeaves[i + 1];
        }
        MRLeaves_count--;
    }
}

void
build_multirooted_tree()
{
    // initialization.
    all_parents = create_mr_parents();
    init_array_leaf_index_for_parent();
    create_mr_leaves_array();

    for (int i = 1; i < grammar.rules.size(); i++) {
        if (is_unit_production(i) == true) {
            MRTreeNode* lhs =
              find_node_in_forest(grammar.rules[i]->nLHS->snode);
            MRTreeNode* rhs =
              find_node_in_forest(grammar.rules[i]->nRHS_head->snode);
            if (lhs != nullptr && rhs == nullptr) {
                // insert rhs as child of lhs.
                insert_child(lhs, grammar.rules[i]->nRHS_head->snode);
            } else if (lhs == nullptr && rhs != nullptr) {
                // insert lhs as parent of rhs.
                insert_parent(rhs, grammar.rules[i]->nLHS->snode);
            } else if (lhs == nullptr && rhs == nullptr) {
                // insert as new tree.
                insert_new_tree(grammar.rules[i]->nLHS->snode,
                                grammar.rules[i]->nRHS_head->snode);
            } else { // just add this relationship.
                insert_parent_child_relation(lhs, rhs);
            } // end if
        }     // end if
    }         // end for

    get_all_mr_parents();

    if (Options::get().debug_build_multirooted_tree) {
        write_mr_forest();
        write_all_mr_parents();
    }
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
        // printf("getNode warning: node is nullptr\n");
        return;
    }

    if (is_in_mr_parents(node->symbol->snode, parents) == false) {
        check_array_size_mr_parents(parents); // expand size if needed.

        if (leafIndexForParent_Done == false) {
            leafIndexForParent[parents->count] = leaf_index;
        }

        parents->parents[parents->count] =
          create_symbol_node(node->symbol->snode);
        parents->count++;
    }

    if (node->parent_count > 0) {
        for (int i = 0; i < node->parent_count; i++) {
            get_node(leaf_index, node->parent[i], parents);
        }
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
get_parents_for_mr_leaf(int leaf_index, MRParents* parents)
{
    MRTreeNode* node = MRLeaves[leaf_index];
    if (node->parent_count == 0)
        return;

    for (int i = 0; i < node->parent_count; i++) {
        get_node(leaf_index, node->parent[i], parents);
    }
}

/*
 * Get all the parent nodes in the multi-rooted forest.
 */
void
get_all_mr_parents()
{
    leafIndexForParent_Done = false;

    for (int i = 0; i < MRLeaves_count; i++) {
        get_parents_for_mr_leaf(i, all_parents);
    }

    leafIndexForParent_Done = true;
}

/*
 * Prints a list of all the parent nodes in the
 * multi-rooted forest, as well as the leaf
 * of each parent in parenthesis.
 */
void
write_all_mr_parents()
{
    yyprintf("\n==all MR Parents (inside '()' is a corresponding leaf):\n");
    for (int i = 0; i < all_parents->count; i++) {
        yyprintf("%s (=>%s)\n",
                 all_parents->parents[i]->snode->symbol,
                 MRLeaves[leafIndexForParent[i]]->symbol->snode->symbol);
    }
    yyprintf("\n");
    // writeLeafIndexForParent();
}

/*
 * Prints a list of all the parent nodes of leaf.
 *
 * Pre-assumption: the list of parent nodes of the given
 * leaf is contained the array parents[].
 */
void
write_mr_parents(MRTreeNode* leaf, MRParents* parents)
{
    int i;
    yyprintf("parents for leaf '%s': ", leaf->symbol->snode->symbol);
    for (i = 0; i < parents->count; i++) {
        if (i > 0)
            yyprintf(", ");
        yyprintf("%s", parents->parents[i]->snode->symbol);
    }
    yyprintf("\n");
}
