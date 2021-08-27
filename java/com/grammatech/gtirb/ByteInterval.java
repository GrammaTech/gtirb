/*
 *  Copyright (C) 2020 GrammaTech, Inc.
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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.ByteIntervalOuterClass;
import com.grammatech.gtirb.proto.SymbolicExpressionOuterClass;

/**
 * The ByteInterval class has an array of bytes and it stores references to them
 * in the form of Blocks and SymbolicExpressions.
 */
public final class ByteInterval extends Node implements TreeListItem {

    private TreeMap<Long, List<ByteBlock>> blockTree =
        new TreeMap<Long, List<ByteBlock>>();
    private TreeMap<Long, List<SymbolicExpression>> symbolicExpressionTree =
        new TreeMap<Long, List<SymbolicExpression>>();
    private Long address;
    private byte[] bytes;
    private Section section;

    /**
     * Class constructor for a ByteInterval from a protobuf byte interval.
     * @param  protoByteInterval  The byte interval as serialized into a
     * protocol buffer.
     * @param  section            The Section that owns this ByteInterval.
     */
    public ByteInterval(ByteIntervalOuterClass.ByteInterval protoByteInterval,
                        Section section) {
        this.section = section;
        super.setUuid(Util.byteStringToUuid(protoByteInterval.getUuid()));
        if (protoByteInterval.getHasAddress()) {
            this.address = Long.valueOf(protoByteInterval.getAddress());
        } else {
            this.address = null;
        }

        this.bytes = protoByteInterval.getContents().toByteArray();
        List<ByteIntervalOuterClass.Block> protoBlockList =
            protoByteInterval.getBlocksList();
        for (ByteIntervalOuterClass.Block protoBlock : protoBlockList) {
            ByteBlock newBlock;
            if (protoBlock.getValueCase() ==
                ByteIntervalOuterClass.Block.ValueCase.CODE) {
                newBlock = new CodeBlock(protoBlock, this);
            } else if (protoBlock.getValueCase() ==
                       ByteIntervalOuterClass.Block.ValueCase.DATA) {
                newBlock = new DataBlock(protoBlock, this);
            } else {
                throw new IllegalArgumentException(
                    "Block must be either a CodeBlock or a DataBlock.");
            }
            this.insertByteBlock(newBlock);
        }

        Map<Long, SymbolicExpressionOuterClass.SymbolicExpression>
            protoSymbolicExpressions =
                protoByteInterval.getSymbolicExpressionsMap();
        for (Map.Entry<Long, SymbolicExpressionOuterClass.SymbolicExpression>
                 entry : protoSymbolicExpressions.entrySet()) {
            SymbolicExpressionOuterClass
                .SymbolicExpression protoSymbolicExpression = entry.getValue();

            SymbolicExpression symbolicExpression;
            if (protoSymbolicExpression.getValueCase() ==
                SymbolicExpressionOuterClass.SymbolicExpression.ValueCase
                    .ADDR_CONST) {
                // SymbolicExpressionOuterClass.SymAddrConst protoSymAddrConst =
                // protoSymbolicExpression.getAddrConst();
                symbolicExpression = new SymAddrConst(protoSymbolicExpression);
            } else if (protoSymbolicExpression.getValueCase() ==
                       SymbolicExpressionOuterClass.SymbolicExpression.ValueCase
                           .ADDR_ADDR) {
                // SymbolicExpressionOuterClass.SymAddrAddr protoSymAddrAddr =
                // protoSymbolicExpression.getAddrAddr();
                symbolicExpression = new SymAddrAddr(protoSymbolicExpression);
            } else {
                throw new IllegalArgumentException(
                    "Symbolic Expression must be either a SymAddrConst or a SymAddrAddr.");
            }
            this.insertSymbolicExpression(symbolicExpression);
        }
    }

    /**
     * Class Constructor.
     * @param  bytes      The array of bytes to be stored in the ByteInterval.
     * @param  address    The address of the ByteInterval, or null.
     * @param  section    The Section that owns this ByteInterval.
     */
    public ByteInterval(byte[] bytes, long address, Section section) {
        this.section = section;
        UUID myUuid = UUID.randomUUID();
        super.setUuid(myUuid);
        this.address = address;
        this.bytes = bytes;
    }

    /**
     * Get the address of this ByteInterval.
     *
     * @return  An address if the ByteInterval has one, otherwise null.
     */
    public Long getAddress() { return this.address; }

    /**
     * Set the address of this ByteInterval.
     *
     * @param address    The new address to give to this ByteInterval
     * @return  An address if the ByteInterval has one, otherwise null.
     */
    public Long setAddress(Long address) {
        this.address = address;
        return this.address;
    }

    /**
     * Check that this ByteInterval has an address.
     *
     * @return       <code>true</code> if this ByteInterval has a non-null
     * address; <code>false</code> otherwise.
     */
    public boolean hasAddress() { return (this.getAddress() != null); }

    /**
     * Get the blocks of this ByteInterval.
     *
     * @return  The list of blocks belonging to this ByteInterval.
     */
    public List<ByteBlock> getBlockList() {
        List<ByteBlock> blockList = new ArrayList<ByteBlock>();
        for (List<ByteBlock> entry : this.blockTree.values()) {
            blockList.addAll(entry);
        }
        return blockList;
    }

    /**
     * Get the size of this ByteBlock.
     *
     * @return  The number of bytes in this ByteInterval.
     */
    public long getSize() { return this.bytes.length; }

    /**
     * Set the size of this ByteInterval. If the new size is greater than the
     * actual number of bytes, the size will not be changed. If the new size is
     * less than the actual number of bytes, the byte array will be truncated to
     * the new length.
     *
     * @param size  The new size to give to this ByteInterval.
     * @return      The new number of bytes in this ByteInterval.
     */
    public long setSize(long size) {
        if (size < this.bytes.length) {
            // Create truncated byte array of the given size
            this.bytes = Arrays.copyOfRange(this.bytes, 0, (int)size);
            //            ArrayList<Byte> subItems =
            //                new ArrayList<Byte>(this.bytes.subList(0,
            //                (int)size));
            //            this.bytes = subItems;
        }
        //        return this.bytes.size();
        return this.bytes.length;
    }

    /**
     * Get the byte array of this ByteInterval.
     *
     * @return  The array of bytes belonging to this ByteInterval.
     */
    public byte[] getBytes() { return this.bytes; }

    /**
     * Set the byte array of this ByteInterval.
     *
     * @param bytes    The new byte array to give to this ByteInterval.
     */
    public void setBytes(byte[] bytes) { this.bytes = bytes; }

    // DEPRECATED:
    //  NOTE: getBytes() returns a byte array and should be used instead of
    //  this.
    //    /**
    //     * Get the byte array from the protocol buffer.
    //     *
    //     * @return  If this byte interval was imported from protocol buffer,
    //     this
    //     * procedure returns the byte array in the protocol buffer byte
    //     interval;
    //     * otherwise returns null.
    //     */
    //    public byte[] getBytesDirect() {
    //        if (this.protoByteInterval == null)
    //            return null;
    //        return this.protoByteInterval.getContents().toByteArray();
    //    }

    /**
     * Get the section this ByteInterval belongs to.
     *
     * @return  The Section that this ByteInterval belongs to, or null if it
     * does not belong to any section.
     */
    public Section getSection() { return this.section; }

    /**
     * Get the index to manage this ByteInterval with.
     *
     * This is the index is used for storing and retrieving the ByteInterval, as
     * required by the TreeListItem interface. ByteIntervals are ordered by
     * address, so this method just returns the address.
     * @return  The ByteInterval index, which is it's address.
     */
    public long getIndex() {
        if (this.address == null)
            return 0L;
        return this.address;
    }

    /////////////////////////////////////////////////////////////
    // GENERIC METHODS
    /////////////////////////////////////////////////////////////

    //    // Generic method for retrieving items that intersect with a given
    //    address private <T extends ByteIntervalItem> ArrayList<T>
    //    getItemsIntersectingAddress(long address,
    //                                TreeMap<Long, ArrayList<T>> tree) {
    //        // Address is not valid if it is below the start if this byte
    //        interval. if (this.address == null || address < this.address)
    //            return null;
    //        Long offset = address - this.address;
    //        offset = offset < 0 ? 0L : offset;
    //        ArrayList<T> resultList = new ArrayList<T>();
    //
    //        // Iterate through the blockTree, but leave out those block who
    //        start
    //        // after the searched-for address
    //        SortedMap<Long, ArrayList<T>> subTree = tree.headMap(offset,
    //        true); for (ArrayList<T> itemList : subTree.values()) {
    //            for (T item : itemList) {
    //                long start = item.getOffset();
    //                long end = start + item.getSize();
    //                // Check if this code block overlaps, including end
    //                points:
    //                //    block starts at or below this offset AND
    //                //    block ends at or above this offset
    //                // If so add it to the list.
    //                if (start <= offset && end >= offset) {
    //                    resultList.add(item);
    //                }
    //            }
    //        }
    //        return resultList;
    //    }
    // Might just as well replace all the references to this with the
    // TreeListUtil version Generic method for retrieving items that intersect
    // with a given address
    private <T extends TreeListItem> List<T>
    getItemsIntersectingAddress(long address, TreeMap<Long, List<T>> tree) {
        // Address is not valid if it is below the start if this byte interval.
        if (this.address == null || address < this.address)
            return null;
        Long offset = address - this.address;
        offset = offset < 0 ? 0L : offset;

        return TreeListUtils.getItemsIntersectingIndex(offset, tree);
    }

    // Generic method for retrieving items that intersect with a given address
    // range
    private <T extends TreeListItem> List<T>
    getItemsIntersectingAddressRange(long startAddress, long endAddress,
                                     TreeMap<Long, List<T>> tree) {
        // Address is not valid if it is below the start if this byte interval.
        if (this.address == null || endAddress < this.address)
            return null;
        Long startOffset = startAddress - this.address;
        Long endOffset = endAddress - this.address;
        startOffset = startOffset < 0 ? 0 : startOffset;
        endOffset = endOffset < 0 ? 0 : endOffset;

        return TreeListUtils.getItemsIntersectingIndexRange(startOffset,
                                                            endOffset, tree);
    }

    // Generic method for retrieving items that start at a given address
    private <T extends TreeListItem> List<T>
    getItemsAtStartAddress(long address, TreeMap<Long, List<T>> tree) {
        // Address is not valid if it is below the start if this byte interval.
        if (this.address == null || address < this.address)
            return null;
        Long offset = address - this.address;
        offset = offset < 0 ? 0L : offset;
        return TreeListUtils.getItemsAtStartIndex(offset, tree);
    }

    // Generic method for retrieving items that start at a given address range
    private <T extends TreeListItem> List<T>
    getItemsAtStartAddressRange(long startAddress, long endAddress,
                                TreeMap<Long, List<T>> tree) {
        // Address is not valid if it is below the start if this byte interval.
        if (this.address == null ||
            startAddress > (this.getAddress() + this.getSize()) ||
            endAddress < this.getAddress())
            return null;

        Long startOffset = startAddress - this.address;
        Long endOffset = endAddress - this.address;
        startOffset = startOffset < 0 ? 0L : startOffset;
        endOffset = endOffset < 0 ? 0L : endOffset;
        return TreeListUtils.getItemsAtStartIndexRange(startOffset, endOffset,
                                                       tree);
    }

    /////////////////////////////////////////////////////////////
    // BYTEBLOCK METHODS
    /////////////////////////////////////////////////////////////

    /**
     * Insert a block into this ByteInterval.
     *
     * The Block must already have an offset. This will be used to determine
     * where to insert it.
     *
     * @param block    The ByteBlock to add to this ByteInterval.
     * @return         An updated list of blocks at this offset, or null if
     * the insert fails.
     */
    public List<ByteBlock> insertByteBlock(ByteBlock block) {
        List<ByteBlock> blockList;
        Long offset = block.getOffset();
        if (this.blockTree.containsKey(offset))
            blockList = blockTree.get(offset);
        else
            blockList = new ArrayList<ByteBlock>();
        blockList.add(block);
        this.blockTree.put(offset, blockList);
        return blockList;
    }

    /**
     * Remove a block from this ByteInterval.
     *
     * @param block    The ByteBlock to add to this ByteInterval.
     * @return         An updated list of blocks at this offset, or null if
     * the delete fails.
     */
    public List<ByteBlock> removeByteBlock(ByteBlock block) {
        Long offset = block.getOffset();
        List<ByteBlock> blockList = this.blockTree.get(offset);
        if (blockList == null)
            // no blocks at this offset
            return null;
        if (!blockList.remove(block))
            // didn't remove, maybe no matching block?
            return null;
        // Did remove, update tree.
        // If the block list is now empty, remove the node from the tree.
        // Otherwise update the tree.
        if (blockList.size() == 0)
            this.blockTree.remove(offset);
        else
            this.blockTree.put(offset, blockList);
        // Return list, even if empty, because null means did not remove.
        return blockList;
    }

    /**
     * Get all ByteBlocks at an offset.
     *
     * @param offset      The offset of this ByteBlock from the beginning of the
     * ByteInterval.
     * @return            A list of blocks at this offset, or null if none.
     */
    public List<ByteBlock> findBlocksAtOffset(long offset) {
        return this.blockTree.get(offset);
    }

    /**
     * Get a ByteBlock iterator.
     *
     * @return  An iterator for iterating through all the blocks in this
     * ByteInterval.
     */
    public Iterator<ByteBlock> byteBlockIterator() {
        TreeListUtils<ByteBlock> blockTreeIterator =
            new TreeListUtils<ByteBlock>(this.blockTree);
        return blockTreeIterator.iterator();
    }

    /////////////////////////////////////////////////////////////
    // CODEBLOCK METHODS
    /////////////////////////////////////////////////////////////

    /**
     * Find all the blocks that have bytes that lie within the address
     * specified.
     *
     * @param address      The address to look for.
     * @return             A list of Code Blocks that intersect this address, or
     * null if none.
     */
    public List<CodeBlock> findCodeBlocksOn(long address) {
        List<ByteBlock> foundList =
            getItemsIntersectingAddress(address, this.blockTree);
        if (foundList == null)
            return null;
        List<CodeBlock> resultList = new ArrayList<CodeBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof CodeBlock)
                resultList.add((CodeBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /**
     * Find all the code blocks that have bytes that lie within the addresses
     * specified.
     *
     * @param startAddress      The beginning of the address range to look for.
     * @param endAddress        The last address of the address range to look
     * for.
     * @return                  A list of Code Blocks that intersect this
     * address range, or null if none.
     */
    public List<CodeBlock> findCodeBlocksOn(long startAddress,
                                            long endAddress) {
        List<ByteBlock> foundList = getItemsIntersectingAddressRange(
            startAddress, endAddress, this.blockTree);
        if (foundList == null)
            return null;
        List<CodeBlock> resultList = new ArrayList<CodeBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof CodeBlock)
                resultList.add((CodeBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /**
     * Find all the code blocks that start at an address.
     *
     * @param address      The address to look for.
     * @return             A list of Code Blocks that that start at this
     * address, or null if none.
     */
    public List<CodeBlock> findCodeBlocksAt(long address) {
        List<ByteBlock> foundList =
            getItemsAtStartAddress(address, this.blockTree);
        if (foundList == null)
            return null;
        List<CodeBlock> resultList = new ArrayList<CodeBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof CodeBlock)
                resultList.add((CodeBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /**
     * Find all the code blocks that start between a range of addresses.
     *
     * @param startAddress      The beginning of the address range to look for.
     * @param endAddress        The last address in the address to look for.
     * @return                  A list of Code Blocks that that start at this
     * address, or null if none.
     */
    public List<CodeBlock> findCodeBlocksAt(long startAddress,
                                            long endAddress) {
        List<ByteBlock> foundList = getItemsAtStartAddressRange(
            startAddress, endAddress, this.blockTree);
        if (foundList == null)
            return null;
        List<CodeBlock> resultList = new ArrayList<CodeBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof CodeBlock)
                resultList.add((CodeBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /////////////////////////////////////////////////////////////
    // DATABLOCK METHODS
    /////////////////////////////////////////////////////////////

    /**
     * Find all the data blocks that have bytes that lie within the address
     * specified
     *
     * @param address      The address to look for.
     * @return             A list of Data Blocks that intersect this address, or
     * null if none.
     */
    public List<DataBlock> findDataBlocksOn(long address) {
        List<ByteBlock> foundList =
            getItemsIntersectingAddress(address, this.blockTree);
        if (foundList == null)
            return null;
        List<DataBlock> resultList = new ArrayList<DataBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof DataBlock)
                resultList.add((DataBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /**
     * Find all the data blocks that have bytes that lie within the address
     * range specified
     *
     * @param startAddress      The beginning of the address range to look for.
     * @param endAddress        The last address of the address range to look
     * for.
     * @return                  A list of Data Blocks that intersect this
     * address range, or null if none.
     */
    public List<DataBlock> findDataBlocksOn(long startAddress,
                                            long endAddress) {
        List<ByteBlock> foundList = getItemsIntersectingAddressRange(
            startAddress, endAddress, this.blockTree);
        if (foundList == null)
            return null;
        List<DataBlock> resultList = new ArrayList<DataBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof DataBlock)
                resultList.add((DataBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /**
     * Find all the data blocks that start at an address.
     *
     * @param address      The address to look for.
     * @return             A list of Data Blocks that that start at this
     * address, or null if none.
     */
    public List<DataBlock> findDataBlocksAt(long address) {
        List<ByteBlock> foundList =
            getItemsAtStartAddress(address, this.blockTree);
        if (foundList == null)
            return null;
        List<DataBlock> resultList = new ArrayList<DataBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof DataBlock)
                resultList.add((DataBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /**
     * Find all the data blocks that start between a range of addresses.
     *
     * @param startAddress      The beginning of the address range to look for.
     * @param endAddress        The last address in the address to look for.
     * @return                  A list of Data Blocks that that start at this
     * address, or null if none.
     */
    public List<DataBlock> findDataBlocksAt(long startAddress,
                                            long endAddress) {
        List<ByteBlock> foundList = getItemsAtStartAddressRange(
            startAddress, endAddress, this.blockTree);
        if (foundList == null)
            return null;
        List<DataBlock> resultList = new ArrayList<DataBlock>();
        for (ByteBlock block : foundList) {
            if (block instanceof DataBlock)
                resultList.add((DataBlock)block);
        }
        if (resultList.size() > 0)
            return resultList;
        return null;
    }

    /////////////////////////////////////////////////////////////
    // SYMBOLICEXPRESSION METHODS
    /////////////////////////////////////////////////////////////

    /**
     * Insert a symbolic expression into this ByteInterval.
     *
     * The symbolic expression must already have an offset. This will be used to
     * determine where to insert it.
     *
     * @param symbolicExpression    The SymbolicExpression to add to this
     * ByteInterval.
     * @return            An updated list of symbolic expressions at this
     * offset, or null if the insert fails.
     */
    public List<SymbolicExpression>
    insertSymbolicExpression(SymbolicExpression symbolicExpression) {
        List<SymbolicExpression> symbolicExpressionList;
        Long offset = symbolicExpression.getOffset();
        if (this.symbolicExpressionTree.containsKey(offset))
            symbolicExpressionList = symbolicExpressionTree.get(offset);
        else
            symbolicExpressionList = new ArrayList<SymbolicExpression>();
        symbolicExpressionList.add(symbolicExpression);
        this.symbolicExpressionTree.put(offset, symbolicExpressionList);
        return symbolicExpressionList;
    }

    /**
     * Remove a symbolicExpression from this ByteInterval.
     *
     * @param symbolicExpression    The SymbolicExpression to add to this
     * ByteInterval.
     * @return            An updated list of symbolic expressions at this
     * offset, or null if the delete fails.
     */
    public List<SymbolicExpression>
    removeSymbolicExpression(SymbolicExpression symbolicExpression) {
        Long offset = symbolicExpression.getOffset();
        List<SymbolicExpression> symbolicExpressionList =
            this.symbolicExpressionTree.get(offset);
        if (symbolicExpressionList == null)
            // no symbolic expressions at this offset
            return null;
        if (!symbolicExpressionList.remove(symbolicExpression))
            // didn't remove, maybe no matching symbolic expressions?
            return null;
        // Did remove. If the list is now empty, remove the node from the tree.
        // Otherwise update the tree.
        if (symbolicExpressionList.size() == 0)
            this.symbolicExpressionTree.remove(offset);
        else
            this.symbolicExpressionTree.put(offset, symbolicExpressionList);
        // Return the list, even if empty, because null means the remove failed.
        return symbolicExpressionList;
    }

    /**
     * Get a SymbolicExpression iterator.
     *
     * @return  An iterator for iterating through all the symbolic expressions
     * in this ByteInterval.
     */
    public Iterator<SymbolicExpression> symbolicExpressionIterator() {
        TreeListUtils<SymbolicExpression> symbolicExpressionTreeIterator =
            new TreeListUtils<SymbolicExpression>(this.symbolicExpressionTree);
        return symbolicExpressionTreeIterator.iterator();
    }

    /**
     * Find all the symbolic expressions that start at an address.
     *
     * Note that only one symbolic expression can be at any given offset, so
     * this will only ever return 0 or 1 elements.
     *
     * @param address      The address to look for.
     * @return             A list of Symbolic Expressions that that start at
     * this address, or null if none.
     */
    public List<SymbolicExpression> findSymbolicExpressionsAt(long address) {
        List<SymbolicExpression> resultList =
            getItemsAtStartAddress(address, this.symbolicExpressionTree);
        if (resultList == null || resultList.size() == 0)
            return null;
        return resultList;
    }

    /**
     * Find all the symbolic expressions that start between a range of
     * addresses.
     *
     * @param startAddress      The beginning of the address range to look for.
     * @param endAddress        The last address in the address to look for.
     * @return                  A list of Symbolic Expressions that that start
     * at this address, or null if none.
     */
    public List<SymbolicExpression> findSymbolicExpressionsAt(long startAddress,
                                                              long endAddress) {
        List<SymbolicExpression> resultList = getItemsAtStartAddressRange(
            startAddress, endAddress, this.symbolicExpressionTree);
        if (resultList == null || resultList.size() == 0)
            return null;
        return resultList;
    }

    /**
     * De-serialize a ByteInterval from a protobuf .
     *
     * @param  protoByteInterval  The byte interval as serialized into a
     * protocol buffer.
     * @param  section            The Section that owns this ByteInterval.
     * @return An initialized ByteInterval.
     */
    public static ByteInterval
    fromProtobuf(ByteIntervalOuterClass.ByteInterval protoByteInterval,
                 Section section) {
        return new ByteInterval(protoByteInterval, section);
    }

    /**
     * Serialize this ByteInterval into a protobuf .
     *
     * @return ByteInterval protocol buffer.
     */
    public ByteIntervalOuterClass.ByteInterval.Builder toProtobuf() {
        ByteIntervalOuterClass.ByteInterval.Builder protoByteInterval =
            ByteIntervalOuterClass.ByteInterval.newBuilder();
        protoByteInterval.setUuid(Util.uuidToByteString(this.getUuid()));
        protoByteInterval.setAddress(this.getAddress());
        protoByteInterval.setSize(this.getSize());
        if (this.address != null)
            protoByteInterval.setHasAddress(true);
        else
            protoByteInterval.setHasAddress(false);

        // Iterate through blocks, adding them
        Iterator<ByteBlock> blocks = this.byteBlockIterator();
        while (blocks.hasNext()) {
            ByteBlock block = blocks.next();
            ByteIntervalOuterClass.Block.Builder protoBlock =
                block.toProtobuf();
            protoByteInterval.addBlocks((int)block.getOffset(), protoBlock);
        }

        // Iterate through symbolic expressions, adding them
        Iterator<SymbolicExpression> symbolicExpressions =
            this.symbolicExpressionIterator();
        while (symbolicExpressions.hasNext()) {
            SymbolicExpression symbolicExpression = symbolicExpressions.next();
            SymbolicExpressionOuterClass.SymbolicExpression
                .Builder protoSymbolicExpression =
                symbolicExpression.toProtobuf();
            protoByteInterval.putSymbolicExpressions(
                symbolicExpression.getOffset(),
                protoSymbolicExpression.build());
        }
        protoByteInterval.setContents(ByteString.copyFrom(this.bytes));
        return protoByteInterval;
    }
}
