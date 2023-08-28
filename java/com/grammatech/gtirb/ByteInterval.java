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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * A ByteInterval represents a piece of runtime memory.
 *
 * The ByteInterval class has a size and can store contents as a byte
 * array, Not having a byte array, or having one with a size smaller than
 * the ByteInterval size is allowed, generally this would represent
 * uninitialized memory.
 *
 * The byte blocks (code blocks and data blocks) and symbolic expressions
 * attached to a byte interval reference the memory range it contains,
 * and it may or may not have an assigned address at any one time.
 */
public final class ByteInterval extends Node implements TreeListItem {

    private TreeMap<Long, List<ByteBlock>> blockTree = new TreeMap<>();
    private TreeMap<Long, SymbolicExpression> symbolicExpressionTree =
        new TreeMap<>();
    private OptionalLong address;
    private long size;
    private byte[] bytes;
    private Optional<Section> section;

    /**
     * Class constructor for a ByteInterval from a protobuf byte interval.
     * @param  protoByteInterval  The byte interval as serialized into a
     * protocol buffer.
     */
    private ByteInterval(ByteIntervalOuterClass.ByteInterval protoByteInterval)
        throws IOException {
        super(Util.byteStringToUuid(protoByteInterval.getUuid()));
        this.section = Optional.empty();
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
                newBlock = DataBlock.fromProtobuf(protoBlock);
            } else if (protoBlock.getValueCase() ==
                       ByteIntervalOuterClass.Block.ValueCase.CODE) {
                newBlock = CodeBlock.fromProtobuf(protoBlock);
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
     */
    public ByteInterval(byte[] bytes, long address) {
        super();
        this.section = Optional.empty();
        this.address = OptionalLong.of(address);
        this.bytes = bytes;
        if (bytes != null) {
            this.size = bytes.length;
        }
    }

    /**
     * Class Constructor.
     * @param  size  The size of the new ByteInterval.
     */
    public ByteInterval(long size) {
        super();
        this.setSize(size);
        this.section = Optional.empty();
        this.address = OptionalLong.empty();
    }

    /**
     * Class Constructor.
     */
    public ByteInterval() {
        super();
        this.section = Optional.empty();
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
     * @return  An unmodifiable {@link Block} list of all the
     * blocks in this {@link ByteInterval}.
     */
    public List<ByteBlock> getBlockList() {
        List<ByteBlock> blockList = new ArrayList<ByteBlock>();
        for (List<ByteBlock> entry : this.blockTree.values()) {
            blockList.addAll(entry);
        }
        return Collections.unmodifiableList(blockList);
    }

    /**
     * Get the size of this ByteBlock.
     *
     * @return  The number of bytes assigned to this ByteInterval.
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
        if (bytes != null && bytes.length > this.size) {
            this.size = bytes.length;
        }
        this.bytes = bytes;
    }

    /**
     * Get the size of this ByteBlock.
     *
     * @return  The number of bytes actually stored in this ByteInterval.
     */
    public long getInitializedSize() {
        if (this.bytes != null)
            return this.bytes.length;
        return 0L;
    }

    /**
     * Get the section this ByteInterval belongs to.
     *
     * @return  The Section that this ByteInterval belongs to, or null if it
     * does not belong to any section.
     */
    public Optional<Section> getSection() { return this.section; }

    /**
     * Set the section this ByteInterval belongs to.
     *
     * @param  The Section that this ByteInterval belongs to, or null if it
     * does not belong to any section.
     */
    void setSection(Optional<Section> section) { this.section = section; }

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
        block.setByteInterval(Optional.of(this));
        return blockList;
    }

    /**
     * Remove a block from this ByteInterval.
     *
     * @param block    The ByteBlock to add to this ByteInterval.
     * @return boolean true if the byte interval contained the block,
     * and it was removed.
     */
    public boolean removeByteBlock(ByteBlock block) {
        if (block.getByteInterval().isEmpty() ||
            block.getByteInterval().get() != this)
            return false;
        Long offset = block.getOffset();
        List<ByteBlock> blockList = this.blockTree.get(offset);
        if (blockList == null)
            // no blocks at this offset
            return false;
        if (!blockList.remove(block))
            // didn't remove, maybe no matching block?
            return false;
        // Did remove, update tree.
        // If the block list is now empty, remove the node from the tree.
        // Otherwise update the tree.
        if (blockList.size() == 0)
            this.blockTree.remove(offset);
        else
            this.blockTree.put(offset, blockList);
        // List empty means did not remove
        if (blockList != null) {
            block.setByteInterval(Optional.empty());
            return true;
        }
        return false;
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
     * (inclusive)
     * @param endAddress        The last address of the address range to look
     * for. (exclusive)
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
     * (inclusive)
     * @param endAddress        The last address in the address range to look
     *     for.
     * (exclusive)
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
     * (inclusive)
     * @param endAddress        The last address of the address range to look
     * for. (exclusive)
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
     * (inclusive)
     * @param endAddress        The last address in the address to look for.
     * (exclusive)
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
     * Remove a symbolic expression from this byte interval.
     *
     * @param offset The offset within this {@link ByteInterval} of the
     * {@link SymbolicExpression}.
     * @return boolean true if the byte interval contained the symbolic
     * expression, and it was removed.
     */
    public boolean removeSymbolicExpression(long offset) {
        return (this.symbolicExpressionTree.remove(offset) != null);
    }

    /**
     * Get a SymbolicExpression iterator.
     *
     * @return  An iterator for iterating through the symbolic expressions
     * in this ByteInterval. Each value returned by the iterator is a of type
     * Map.Entry&lt;Long, SymbolicExpression&gt;, where the key is the offset of
     * the SymbolicExpression in the ByteInterval.
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
     * (inclusive)
     * @param endAddress        The last address in the address to look for.
     * (exlusive)
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
    fromProtobuf(ByteIntervalOuterClass.ByteInterval protoByteInterval)
        throws IOException {
        return new ByteInterval(protoByteInterval);
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
