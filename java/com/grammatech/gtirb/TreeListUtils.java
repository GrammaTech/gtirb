/*
 *  Copyright (C) 2020-2021 GrammaTech, Inc.
 *
 *  This code is licensed under the MIT license. See the LICENSE file in the
 *  project root for license terms.
 *
 *  This project is sponsored by the Office of Naval Research, One Liberty
 *  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
 *  N68335-17-C-0700.  The content of the information does not necessarily
 *  reflect the position or policy of the Government and no official
 *  endorsement should be inferred.
 *
 */

package com.grammatech.gtirb;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Utilities for storing and retrieving generic items in sorted order.
 * Implementation is a TreeMap with nodes indexed by address
 * (or in some cases offset) where an address/offset is allowed to
 * have multiple items at the same index, and when multiple items have the
 * same index they are stored as an ArrayList.
 */
public class TreeListUtils<Type> implements Iterable<Type> {

    private TreeMap<Long, List<Type>> treeMap;

    public TreeListUtils(TreeMap<Long, List<Type>> treeMap) {
        this.treeMap = treeMap;
    }

    /**
     * Generic method for inserting items into a TreeList.
     *
     * @param item    The item to be added.
     * @param tree    The tree to be inserted into.
     * @return The updated tree node.
     */
    public static <T extends TreeListItem> List<T>
    insertItem(T item, TreeMap<Long, List<T>> tree) {
        Long index = item.getIndex();
        List<T> itemList;
        if (tree.containsKey(index))
            itemList = tree.get(index);
        else
            itemList = new ArrayList<T>();
        itemList.add(item);
        tree.put(index, itemList);
        return itemList;
    }

    /**
     * Generic method for removing items from a TreeList.
     *
     * @param item    The item to be removed.
     * @param tree    The tree to be removed from.
     * @return The updated tree node.
     */
    public static <T extends TreeListItem> List<T>
    removeItem(T item, TreeMap<Long, List<T>> tree) {
        Long index = item.getIndex();
        List<T> itemList = tree.get(index);
        if (itemList == null)
            // no blocks at this offset
            return null;
        if (!itemList.remove(item))
            // didn't remove, maybe no matching item?
            return null;
        // Did remove, update tree.
        // If the block list is now empty, remove the node from the tree.
        // Otherwise update the tree.
        if (itemList.size() == 0)
            tree.remove(index);
        else
            tree.put(index, itemList);
        // Return list, even if empty, because null means did not remove.
        return itemList;
    }

    /**
     * Generic method for retrieving items that intersect with a given address
     *
     * @param index    The address/offset to be matched.
     * @param tree     The tree to be matched in.
     * @return         A list of matching items.
     */
    public static <T extends TreeListItem> List<T>
    getItemsIntersectingIndex(long index, TreeMap<Long, List<T>> tree) {
        // Iterate through the tree nodes, but leave out those nodes who start
        // after the searched-for index.
        List<T> resultList = new ArrayList<T>();
        SortedMap<Long, List<T>> subTree = tree.headMap(index, true);
        for (List<T> itemList : subTree.values()) {
            for (T item : itemList) {
                long start = item.getIndex();
                long end = start + item.getSize();
                // Check if this item overlaps, including end points:
                //    item starts at or below this offset AND
                //    item ends at or above this offset
                // If so add it to the list.
                if (start <= index && end >= index) {
                    resultList.add(item);
                }
            }
        }
        return resultList;
    }

    /**
     * Generic method for retrieving items that intersect with a given address
     * range
     *
     * @param startIndex  The start of the address range to be matched.
     * @param endIndex    The end of the address range.
     * @param tree        The tree to be matched in.
     * @return            A list of matching items.
     */
    public static <T extends TreeListItem> List<T>
    getItemsIntersectingIndexRange(long startIndex, long endIndex,
                                   TreeMap<Long, List<T>> tree) {
        // Iterate through the tree nodes, but leave out those nodes who start
        // after the searched-for index.
        List<T> resultList = new ArrayList<T>();
        SortedMap<Long, List<T>> subTree = tree.headMap(endIndex, true);
        for (List<T> itemList : subTree.values()) {
            for (T item : itemList) {
                long start = item.getIndex();
                long end = start + item.getSize();
                // Check for overlap, including end points:
                //    (1) The range starts within the range of this interval:
                //        Item start is at or above the range start AND
                //        item start is at or below the range end
                // OR (2) The range ends within the range of this interval:
                //        Item end is at or above the range start AND
                //        item end is at or below the range end
                // OR (3) The range includes the range of this interval
                //        Item start is below range start AND
                //        item end is above range end
                // If so add it to the list to return.
                if (startIndex >= start && startIndex <= end)
                    resultList.add(item);
                else if (endIndex >= start && endIndex <= end)
                    resultList.add(item);
                else if (startIndex < start && endIndex > end)
                    resultList.add(item);
            }
        }
        return resultList;
    }

    /**
     * Generic method for retrieving items that start at a given address.
     *
     * @param index       The address to be matched.
     * @param tree        The tree to be matched in.
     * @return            A list of matching items.
     */
    public static <T extends TreeListItem> List<T>
    getItemsAtStartIndex(long index, TreeMap<Long, List<T>> tree) {
        List<T> resultList = new ArrayList<T>();
        if (tree.containsKey(index))
            resultList.addAll(tree.get(index));
        return (resultList);
    }

    /**
     * Generic method for retrieving items that start at a given address range.
     *
     * @param startIndex  The start of the address range to be matched.
     *     (inclusive)
     * @param endIndex    The end of the address range. (exclusive)
     * @param tree        The tree to be matched in.
     * @return            A list of matching items.
     */
    public static <T extends TreeListItem> List<T>
    getItemsAtStartIndexRange(long startIndex, long endIndex,
                              TreeMap<Long, List<T>> tree) {
        ArrayList<T> resultList = new ArrayList<T>();

        // First key in range can be found with ceilingKey()
        // Successive keys, if there are any, can be found with higherKey()
        Long key = tree.ceilingKey(startIndex);
        if (key != null && key < endIndex) {
            resultList.addAll(tree.get(key));
            key = tree.higherKey(key);
        }

        // Keep adding as long as you find items in range
        while (key != null && key < endIndex) {
            resultList.addAll(tree.get(key));
            key = tree.higherKey(key);
        }
        return resultList;
    }

    /**
     * Generic method for iterating through all items.
     *
     * @return An iterator for the tree.
     */
    @Override
    public Iterator<Type> iterator() {
        Iterator<Type> it = new Iterator<Type>() {
            private int treeSize = treeMap.size();
            private int currentTreeIndex = 0;
            private int currentListIndex = 0;
            // Get the (current set of keys for the whole tree, as an array of
            // Longs, so that it can be incremented through
            private Set<Long> keySet = treeMap.keySet();
            private Long keyArray[] = keySet.toArray(new Long[0]);

            // We need a currentList variable in this scope, but it the tree may
            // be empty, so we can't initialize it here. So set it to emptyList,
            // which will report a size of 0.
            private List<Type> currentList = Collections.emptyList();

            private boolean listHasNext() {
                if (currentList.size() > 0 &&
                    currentListIndex < currentList.size())
                    return true;
                return false;
            }

            private boolean treeHasNext() {
                if (keySet.size() > 0 && currentTreeIndex < keySet.size())
                    return true;
                return false;
            }

            @Override
            public boolean hasNext() {
                if (treeSize == 0)
                    return false;
                if (listHasNext() || treeHasNext())
                    return true;
                return false;
            }

            @Override
            public Type next() {
                if (treeSize == 0)
                    return null;
                if (listHasNext())
                    return currentList.get(currentListIndex++);
                else if (treeHasNext()) {
                    currentList = treeMap.get(keyArray[currentTreeIndex++]);
                    currentListIndex = 0;
                    return currentList.get(currentListIndex++);
                }
                return null;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
        return it;
    }
}
