/*
 *  Copyright (C) 2020-2023 GrammaTech, Inc.
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

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.ByteIntervalOuterClass;
import com.grammatech.gtirb.proto.SymbolicExpressionOuterClass;
import java.util.*;

/**
 * The ByteInterval class has an array of bytes and it stores references to them
 * in the form of Blocks and SymbolicExpressions.
 */
public final class ByteInterval extends Node implements TreeListItem {

    private TreeMap<Long, List<ByteBlock>> blockTree = new TreeMap<>();
    private TreeMap<Long, SymbolicExpression> symbolicExpressionTree =
        new TreeMap<>();
    private OptionalLong address;
    private long size;
    private byte[] bytes;
    private Section section;

    /**
     * Class constructor for a ByteInterval from a protobuf byte interval.
     * @param  protoByteInterval  The byte interval as serialized into a
     * protocol buffer.
     * @param  section            The Section that owns this ByteInterval.
     */
    private ByteInterval(ByteIntervalOuterClass.ByteInterval protoByteInterval,
                         Section section) {
        super(Util.byteStringToUuid(protoByteInterval.getUuid()));
        this.section = section;
        if (protoByteInterval.getHasAddress()) {
            this.address = OptionalLong.of(protoByteInterval.getAddress());
        } else {
            this.address = OptionalLong.empty();
        }

        this.bytes = protoByteInterval.getContents().toByteArray();
        this.size = protoByteInterval.getSize();
        List<ByteIntervalOuterClass.Block> protoBlockList =
            protoByteInterval.getBlocksList();
        for (ByteIntervalOuterClass.Block protoBlock : protoBlockList) {
            ByteBlock newBlock = null;
            // Avoid using protoBlock.hasData() or protoBlock.hasCode() for
            // compatibility with older protobuf versions.
            if (protoBlock.getValueCase() ==
                ByteIntervalOuterClass.Block.ValueCase.DATA) {
                newBlock = DataBlock.fromProtobuf(protoBlock, this);
            } else if (protoBlock.getValueCase() ==
                       ByteIntervalOuterClass.Block.ValueCase.CODE) {
                newBlock = CodeBlock.fromProtobuf(protoBlock, this);
            }
            if (newBlock == null) {
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
                symbolicExpression = new SymAddrConst(protoSymbolicExpression);
            } else if (protoSymbolicExpression.getValueCase() ==
                       SymbolicExpressionOuterClass.SymbolicExpression.ValueCase
                           .ADDR_ADDR) {
                symbolicExpression = new SymAddrAddr(protoSymbolicExpression);
            } else {
                throw new IllegalArgumentException(
                    "Symbolic Expression must be either a SymAddrConst or a SymAddrAddr.");
            }
            this.insertSymbolicExpression(entry.getKey(), symbolicExpression);
        }
    }

    /**
     * Class Constructor.
     * @param  bytes      The array of bytes to be stored in the ByteInterval.
     * @param  address    The address of the ByteInterval.
     * @param  section    The Section that owns this ByteInterval.
     */
    public ByteInterval(byte[] bytes, long address, Section section) {
        super();
        this.section = section;
        this.address = OptionalLong.of(address);
        this.bytes = bytes;
        if (bytes != null) {
            this.size = bytes.length;
        }
    }

    /**
     * Class Constructor.
     * @param  section    The Section that owns this ByteInterval.
     */
    public ByteInterval(Section section) {
        super();
        this.section = section;
        this.address = OptionalLong.empty();
    }

    /**
     * Get the address of this ByteInterval.
     *
     * @return  An OptionalLong that either is empty or holds a valid address.
     */
    public OptionalLong getAddress() { return this.address; }

    /**
     * Set the address of this ByteInterval.
     *
     * @param address    The new address to give to this ByteInterval
     */
    public void setAddress(long address) {
        this.address = OptionalLong.of(address);
    }

    /**
     * Clears the address of this ByteInterval.
     */
    public void clearAddress() { this.address = OptionalLong.empty(); }

    /**
     * Check that this ByteInterval has an address.
     *
     * @return       <code>true</code> if this ByteInterval has an
     * address; <code>false</code> otherwise.
     */
    public boolean hasAddress() { return address.isPresent(); }

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
    public long getSize() { return this.size; }

    /**
     * Set the size of this ByteInterval.
     *
     * If the new size is less than the actual bytes, the byte array will be
     * truncated to the new length.
     *
     * @param size  The new size to give to this ByteInterval.
     */
    public void setSize(long size) {
        if (this.bytes != null && size < this.bytes.length) {
            // Create truncated byte array of the given size
            this.bytes = Arrays.copyOfRange(this.bytes, 0, (int)size);
        }
        this.size = size;
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
    public void setBytes(byte[] bytes) {
        if (bytes != null) {
            this.size = bytes.length;
        }
        this.bytes = bytes;
    }

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
    public long getIndex() { return this.address.orElse(0); }

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
        if (!this.address.isPresent())
            return null;
        long ownAddress = this.address.getAsLong();

        // Address is not valid if it is below the start if this byte interval.
        if (Long.compareUnsigned(address, ownAddress) < 0)
            return null;
        long offset = address - ownAddress;

        return TreeListUtils.getItemsIntersectingIndex(offset, tree);
    }

    // Generic method for retrieving items that intersect with a given address
    // range
    private <T extends TreeListItem> List<T>
    getItemsIntersectingAddressRange(long startAddress, long endAddress,
                                     TreeMap<Long, List<T>> tree) {
        if (!this.address.isPresent())
            return null;
        long ownAddress = this.address.getAsLong();

        // End address cannot be below the start of this ByteInterval
        if (Long.compareUnsigned(endAddress, ownAddress) < 0)
            return null;
        long endOffset = endAddress - ownAddress;

        // Allow ranges that start before this ByteInterval
        long startOffset = 0;
        if (Long.compareUnsigned(startAddress, ownAddress) >= 0) {
            startOffset = startAddress - ownAddress;
        }

        return TreeListUtils.getItemsIntersectingIndexRange(startOffset,
                                                            endOffset, tree);
    }

    // Generic method for retrieving items that start at a given address
    private <T extends TreeListItem> List<T>
    getItemsAtStartAddress(long address, TreeMap<Long, List<T>> tree) {
        if (!this.address.isPresent())
            return null;
        long ownAddress = this.address.getAsLong();

        // Address is not valid if it is below the start of this byte interval.
        if (Long.compareUnsigned(address, ownAddress) < 0)
            return null;
        long offset = address - ownAddress;

        return TreeListUtils.getItemsAtStartIndex(offset, tree);
    }

    // Generic method for retrieving items that start at a given address range
    private <T extends TreeListItem> List<T>
    getItemsAtStartAddressRange(long startAddress, long endAddress,
                                TreeMap<Long, List<T>> tree) {
        if (!this.address.isPresent())
            return null;
        long ownAddress = this.address.getAsLong();

        // Range is not valid if either address falls outside the ByteInterval.
        if (Long.compareUnsigned(startAddress, ownAddress + this.size) > 0 ||
            Long.compareUnsigned(endAddress, ownAddress) < 0)
            return null;

        long startOffset = startAddress - ownAddress;
        long endOffset = endAddress - ownAddress;
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
     * @param offset The offset within this ByteInterval at which the address
     * described by this Symbolic Expression belongs; not to be confused with
     * the symbol-relative offset that may be part of a {@link SymAddrConst}.
     * @param symbolicExpression The SymbolicExpression to add to this
     * ByteInterval.
     */
    public void
    insertSymbolicExpression(long offset,
                             SymbolicExpression symbolicExpression) {
        this.symbolicExpressionTree.put(offset, symbolicExpression);
    }

    /**
     * Remove a symbolicExpression from this ByteInterval.
     *
     * @param offset The offset within this ByteInterval of the Symbolic
     *               Expression.
     */
    public void removeSymbolicExpression(long offset) {
        this.symbolicExpressionTree.remove(offset);
    }

    /**
     * Get a SymbolicExpression iterator.
     *
     * @return  An iterator for iterating through the symbolic expressions
     * in this ByteInterval. Each value returned by the iterator is a of type
     * Map.Entry<Long, SymbolicExpression>, where the key is the offset of
     * the SYmbolicExpression in the ByteInterval.
     */
    public Iterator<Map.Entry<Long, SymbolicExpression>>
    symbolicExpressionIterator() {
        return this.symbolicExpressionTree.entrySet().iterator();
    }

    /**
     * Find all the symbolic expressions that start at an address.
     *
     * Note that only one symbolic expression can be at any given offset,
     * so this will return at most one SymbolicExpression.
     *
     * @param address      The address to look for.
     * @return             A Symbolic Expression at this address,
     * or null if none.
     */
    public SymbolicExpression findSymbolicExpressionAt(long address) {
        return this.symbolicExpressionTree.get(address);
    }

    /**
     * Find all the symbolic expressions that start between a range of
     * addresses.
     *
     * @param startAddress      The beginning of the address range to look for.
     * @param endAddress        The last address in the address to look for.
     * @return                  An iterator of the set of SymbolicExpressions
     * found, if any, as Map entries, where the key is the offset of the
     * SymbolicExpression in the ByteInterval.
     */
    public Iterator<Map.Entry<Long, SymbolicExpression>>
    findSymbolicExpressionsAt(long startAddress, long endAddress) {
        long start = startAddress;
        long end = endAddress;

        if (endAddress < startAddress) {
            start = endAddress;
            end = startAddress;
        }

        SortedMap<Long, SymbolicExpression> subTree =
            symbolicExpressionTree.subMap(start, end);
        return subTree.entrySet().iterator();
    }

    /**
     * De-serialize a ByteInterval from a protobuf .
     *
     * @param  protoByteInterval  The byte interval as serialized into a
     * protocol buffer.
     * @param  section            The Section that owns this ByteInterval.
     * @return An initialized ByteInterval.
     */
    static ByteInterval
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
        protoByteInterval.setAddress(this.getAddress().orElse(0));
        protoByteInterval.setSize(this.getSize());
        protoByteInterval.setHasAddress(this.hasAddress());

        // Iterate through blocks, adding them
        Iterator<ByteBlock> blocks = this.byteBlockIterator();
        while (blocks.hasNext()) {
            ByteBlock block = blocks.next();
            ByteIntervalOuterClass.Block.Builder protoBlock =
                block.toProtobuf();
            protoByteInterval.addBlocks(protoBlock);
        }

        // Iterate through symbolic expressions, adding them
        for (Map.Entry<Long, SymbolicExpression> symbolicEntry :
             symbolicExpressionTree.entrySet()) {
            SymbolicExpression symbolicExpression = symbolicEntry.getValue();
            SymbolicExpressionOuterClass.SymbolicExpression
                .Builder protoSymbolicExpression =
                symbolicExpression.toProtobuf();
            protoByteInterval.putSymbolicExpressions(
                symbolicEntry.getKey(), protoSymbolicExpression.build());
        }
        if (this.bytes == null) {
            protoByteInterval.setContents(ByteString.EMPTY);
        } else {
            protoByteInterval.setContents(ByteString.copyFrom(this.bytes));
        }
        return protoByteInterval;
    }
}
