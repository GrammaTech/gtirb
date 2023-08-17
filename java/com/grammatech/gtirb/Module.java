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

import com.grammatech.gtirb.proto.ModuleOuterClass;
import com.grammatech.gtirb.proto.ProxyBlockOuterClass;
import com.grammatech.gtirb.proto.SectionOuterClass;
import com.grammatech.gtirb.proto.SymbolOuterClass;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

/**
 * Represents a single binary (library or executable),
 */
public class Module extends AuxDataContainer {

    /**
     * 	Identifies an executable file format.
     */
    public enum FileFormat {
        Format_Undefined,
        COFF,
        ELF,
        PE,
        IdaProDb32,
        IdaProDb64,
        XCOFF,
        MACHO,
        RAW
    }

    /**
     * 	Identifies an instruction set architecture.
     */
    public enum ISA {
        ISA_Undefined,
        IA32,
        PPC32,
        X64,
        ARM,
        ValidButUnsupported,
        PPC64,
        ARM64,
        MIPS32,
        MIPS64
    }

    /**
     * 	Identifies a byte ordering (endianness).
     */
    public enum ByteOrder { ByteOrder_Undefined, BigEndian, LittleEndian }

    private Optional<IR> ir;
    private String binaryPath;
    private long preferredAddr;
    private long rebaseDelta;
    private FileFormat fileFormat;
    private ISA isa;
    private String name;
    private TreeMap<Long, List<Section>> sectionTree;
    private List<Symbol> symbolList;
    private List<ProxyBlock> proxyBlockList;
    private CodeBlock entryPoint;
    private ByteOrder byteOrder;

    /**
     * Class constructor for a Module from a protobuf module.
     * @param  protoModule   The module as serialized into a protocol buffer.
     */
    Module(ModuleOuterClass.Module protoModule) {
        super(protoModule.getUuid(), protoModule.getAuxDataMap());
        this.ir = Optional.empty();
        this.binaryPath = protoModule.getBinaryPath();
        this.preferredAddr = protoModule.getPreferredAddr();
        this.rebaseDelta = protoModule.getRebaseDelta();
        this.fileFormat = FileFormat.values()[protoModule.getFileFormatValue()];
        this.isa = ISA.values()[protoModule.getIsaValue()];
        this.name = protoModule.getName();
        this.byteOrder = ByteOrder.values()[protoModule.getByteOrderValue()];

        initializeSectionList(protoModule.getSectionsList());
        initializeSymbolList(protoModule.getSymbolsList());
        initializeProxyBlockList(protoModule.getProxiesList());

        // Sections must be initialized before looking up the entry point
        UUID entryUUID = Util.byteStringToUuid(protoModule.getEntryPoint());
        Node entryNode = Node.getByUuid(entryUUID);
        if (entryNode instanceof CodeBlock)
            this.entryPoint = (CodeBlock)entryNode;
    }

    /**
     * Class Constructor.
     * @param  binaryPath       The binary path of this Module.
     * @param  preferredAddr    The preferred address of this Module.
     * @param  rebaseDelta      The rebase delta of this Module.
     * @param  fileFormat       The file format of this Module.
     * @param  isa              The ISA of this Module.
     * @param  name             The name of this Module.
     * @param  sections         A list of Sections belonging to this Module.
     * @param  symbols          A list of Symbols belonging to this Module.
     * @param  proxyBlocks      A list of ProxyBlocks belonging to this Module.
     * @param  entryPoint       The entry point of this module or null.
     */
    public Module(String binaryPath, long preferredAddr, long rebaseDelta,
                  FileFormat fileFormat, ISA isa, String name,
                  List<Section> sections, List<Symbol> symbols,
                  List<ProxyBlock> proxyBlocks, CodeBlock entryPoint) {
        super();
        this.ir = Optional.empty();
        this.binaryPath = binaryPath;
        this.preferredAddr = preferredAddr;
        this.rebaseDelta = rebaseDelta;
        this.fileFormat = fileFormat;
        this.isa = isa;
        this.name = name;
        this.entryPoint = entryPoint;
        this.setSymbols(symbols);
        this.setProxyBlocks(proxyBlocks);
        this.setSections(sections);
    }

    /**
     * Class Constructor for a minimal module with no sections, symbols, or
     * proxyBlocks.
     * @param  binaryPath       The binary path of this Module.
     * @param  preferredAddr    The preferred address of this Module.
     * @param  rebaseDelta      The rebase delta of this Module.
     * @param  fileFormat       The file format of this Module.
     * @param  isa              The ISA of this Module.
     * @param  name             The name of this Module.
     */
    public Module(String binaryPath, long preferredAddr, long rebaseDelta,
                  FileFormat fileFormat, ISA isa, String name) {
        super();
        this.ir = Optional.empty();
        this.binaryPath = binaryPath;
        this.preferredAddr = preferredAddr;
        this.rebaseDelta = rebaseDelta;
        this.fileFormat = fileFormat;
        this.isa = isa;
        this.name = name;
        this.symbolList = new ArrayList<Symbol>();
        this.sectionTree = new TreeMap<Long, List<Section>>();
        this.proxyBlockList = new ArrayList<ProxyBlock>();
        this.entryPoint = null;
    }

    /**
     * Get the {@link IR} this Module belongs to.
     *
     * @return  An Optional that contains the IR this module belongs to,
     * or empty if it does not belong to an IR.
     */
    public Optional<IR> getIr() { return this.ir; }

    /**
     * Set the {@link IR} this Module belongs to.
     *
     * @param  An Optional that contains the IR this module will belongs
     * to, or empty if it should not belong to an IR.
     */
    void setIr(Optional<IR> ir) { this.ir = ir; }

    /**
     * Get the location of the corresponding binary on disk.
     *
     * @return  The path to the corresponding binary on disk.
     */
    public String getBinaryPath() { return this.binaryPath; }

    /**
     * Set the location of the corresponding binary on disk.
     *
     * This is for informational purposes only and will not be
     * used to open the image, so it does not need to be the path
     * of an existing file.
     *
     * @param binaryPath    The path name to use.
     */
    public void setBinaryPath(String binaryPath) {
        this.binaryPath = binaryPath;
    }

    /**
     * Get the preferred address for loading this module.
     *
     * @return  The preferred address.
     */
    public long getPreferredAddr() { return this.preferredAddr; }

    /**
     * Set the preferred address for loading this module.
     *
     * @param preferredAddr  The module preferred address.
     */
    public void setPreferredAddr(long preferredAddr) {
        this.preferredAddr = preferredAddr;
    }

    /**
     * Get the difference between this module's preferred address and the
     * address where it was actually loaded.
     *
     * @return  The rebase delta.
     */
    public long getRebaseDelta() { return this.rebaseDelta; }

    /**
     * Set the difference between this module's preferred address and the
     * address where it was actually loaded.
     *
     * @param rebaseDelta    The module rebase delta.
     */
    public void setRebaseDelta(long rebaseDelta) {
        this.rebaseDelta = rebaseDelta;
    }

    /**
     * Get the format of the binary pointed to by getBinaryPath().
     *
     * @return  The format of the binary associated with this module, as a
     * {@link FileFormat} enumerator.
     */
    public FileFormat getFileFormat() { return this.fileFormat; }

    /**
     * Set the file format of this Module.
     *
     * @param fileFormat    The module file format.
     */
    public void setFileFormat(FileFormat fileFormat) {
        this.fileFormat = fileFormat;
    }

    /**
     * Get the ISA of the instructions in this Module.
     *
     * @return  The module {@link ISA}.
     */
    public ISA getIsa() { return this.isa; }

    /**
     * Set the ISA of the instructions in this Module.
     *
     * @param isa    The module {@link ISA}.
     */
    public void setIsa(ISA isa) { this.isa = isa; }

    /**
     * Get the name of this Module.
     *
     * @return  The module name.
     */
    public String getName() { return this.name; }

    /**
     * Set the name of this Module.
     *
     * @param name    The module name.
     */
    public void setName(String name) { this.name = name; }

    /**
     * Get the sections of this Module.
     *
     * @return  An unmodifiable {@link Section} list of all the
     * sections in this {@link Module}.
     */
    public List<Section> getSections() {
        List<Section> sectionList = new ArrayList<Section>();
        for (List<Section> entry : this.sectionTree.values()) {
            sectionList.addAll(entry);
        }
        return Collections.unmodifiableList(sectionList);
    }

    /**
     * Set the section list of this Module.
     *
     * @param sectionList    The module section list.
     */
    private void setSections(List<Section> sectionList) {
        if (sectionTree == null) {
            sectionTree = new TreeMap<Long, List<Section>>();
        } else {
            sectionTree.clear();
        }
        for (Section section : sectionList) {
            TreeListUtils.insertItem(section, sectionTree);
            section.setModule(Optional.of(this));
        }
    }

    /**
     * Add a section to this Module.
     *
     * @param section  The {@link Section} to add.
     */
    public void addSection(Section section) {
        TreeListUtils.insertItem(section, this.sectionTree);
        section.setModule(Optional.of(this));
    }

    /**
     * Remove a section from this Module.
     *
     * @param section  The {@link Section} to remove.
     * @return boolean true if the Module contained the section, and it was
     * removed.
     */
    public boolean removeSection(Section section) {
        if (section.getModule().isPresent() &&
            section.getModule().get() == this) {
            TreeListUtils.removeItem(section, this.sectionTree);
            section.setModule(Optional.empty());
            return true;
        } else
            return false;
    }

    /**
     * Get the symbols of this Module.
     *
     * @return  An unmodifiable {@link Symbol} list of all the
     * symbols in this {@link Module}.
     */
    public List<Symbol> getSymbols() {
        return Collections.unmodifiableList(this.symbolList);
    }

    /**
     * Add a symbol to this Module.
     *
     * @param symbol  The {@link Symbol} to add to this {@link Module}.
     */
    public void addSymbol(Symbol symbol) {
        this.symbolList.add(symbol);
        symbol.setModule(Optional.of(this));
    }

    /**
     * Remove a symbol from this Module.
     *
     * @param symbol  The {@link Symbol} to remove from this {@link Module}.
     * @return boolean true if the Module contained the symbol, and it was
     * removed.
     */
    public boolean removeSymbol(Symbol symbol) {
        if (symbol.getModule().isPresent() &&
            symbol.getModule().get() == this &&
            this.symbolList.remove(symbol)) {
            symbol.setModule(Optional.empty());
            return true;
        } else
            return false;
    }

    /**
     * Set the symbol list of this Module.
     *
     * @param symbolList    The module symbol list.
     */
    private void setSymbols(List<Symbol> symbolList) {
        if (this.symbolList == null) {
            this.symbolList = new ArrayList<Symbol>();
        }
        for (Symbol symbol : symbolList)
            this.addSymbol(symbol);
    }

    /**
     * Get a list of proxy blocks in this Module.
     *
     * @return  An unmodifiable {@link ProxyBlock} list of all the
     * proxy blocks in this {@link Module}.
     */
    public List<ProxyBlock> getProxyBlocks() {
        return Collections.unmodifiableList(this.proxyBlockList);
    }

    /**
     * Add a proxy block to this Module.
     *
     * @param proxyBlock    The {@link ProxyBlock} to add to this {@link
     * Module}.
     */
    public void addProxyBlock(ProxyBlock proxyBlock) {
        this.proxyBlockList.add(proxyBlock);
        proxyBlock.setModule(Optional.of(this));
    }

    /**
     * Remove a proxy block from this Module.
     *
     * @param proxyBlock    The {@link ProxyBlock} to remove from this {@link
     * Module}.
     * @return boolean true if the Module contained the proxy block, and it was
     * removed.
     */
    public boolean removeProxyBlock(ProxyBlock proxyBlock) {
        if (proxyBlock.getModule().isPresent() &&
            proxyBlock.getModule().get() == this &&
            this.proxyBlockList.remove(proxyBlock)) {
            proxyBlock.setModule(Optional.empty());
            return true;
        } else
            return false;
    }

    /**
     * Set the proxy block list of this Module.
     *
     * @param proxyBlockList    The module proxyBlock list.
     */
    private void setProxyBlocks(List<ProxyBlock> proxyBlockList) {
        if (this.proxyBlockList == null) {
            this.proxyBlockList = new ArrayList<ProxyBlock>();
        }
        for (ProxyBlock proxyBlock : proxyBlockList)
            this.addProxyBlock(proxyBlock);
    }

    /**
     * Get the entry point of this module, or null if not present.
     *
     * @return  The module entry point (a code block) or null if no entry is
     * point has been designated.
     */
    public CodeBlock getEntryPoint() { return entryPoint; }

    /**
     * Set the entry point of this Module.
     *
     * @param entryCodeBlock    The module entry point (a {@link CodeBlock}).
     */
    public void setEntryPoint(CodeBlock entryCodeBlock) {
        this.entryPoint = entryCodeBlock;
    }

    /**
     * Get the ByteOrder of this Module.
     *
     * @return  The module byte order (endianness).
     */
    public ByteOrder getByteOrder() { return this.byteOrder; }

    /**
     * Set the byte order of this Module.
     *
     * @param  byteOrder    The module byte order (endianness).
     */
    public void setByteOrder(ByteOrder byteOrder) {
        this.byteOrder = byteOrder;
    }

    /**
     * Initialize this module's sections from section protocol buffers
     *
     * When creating a Module from a protocol buffer module, use this method to
     * initialize the sections belonging to this module from the protocol
     * buffers of those sections.
     *
     */
    private void
    initializeSectionList(List<SectionOuterClass.Section> protoSectionList) {
        this.sectionTree = new TreeMap<>();
        // For each section, add to sectionList in this class
        for (SectionOuterClass.Section protoSection : protoSectionList) {
            Section newSection = Section.fromProtobuf(protoSection);
            this.addSection(newSection);
        }
    }

    /**
     * Initialize this module's symbols from symbol protocol buffers
     *
     * When creating a Module from a protocol buffer module, use this method to
     * initialize the symbols belonging to this module from the protocol
     * buffers of those symbols.
     *
     */
    private void
    initializeSymbolList(List<SymbolOuterClass.Symbol> protoSymbolList) {
        this.symbolList = new ArrayList<Symbol>();
        // For each symbol, add to symbolList in this class
        for (SymbolOuterClass.Symbol protoSymbol : protoSymbolList) {
            Symbol newSymbol = Symbol.fromProtobuf(protoSymbol);
            this.addSymbol(newSymbol);
        }
    }

    /**
     * Initialize this module's proxy blocks from proxy block protocol buffers
     *
     * When creating a Module from a protocol buffer module, use this method to
     * initialize the proxy blocks belonging to this module from the protocol
     * buffers of those proxy blocks.
     *
     */
    private void initializeProxyBlockList(
        List<ProxyBlockOuterClass.ProxyBlock> protoProxyBlockList) {
        this.proxyBlockList = new ArrayList<ProxyBlock>();
        // For each proxy block, add to proxyBlockList in this class
        for (ProxyBlockOuterClass.ProxyBlock protoProxyBlock :
             protoProxyBlockList) {
            ProxyBlock newProxyBlock =
                ProxyBlock.fromProtobuf(protoProxyBlock, this);
            this.addProxyBlock(newProxyBlock);
        }
    }

    /**
     * Find all the sections that have bytes that intersect with the address
     * specified.
     *
     * @param address      The address to look for.
     * @return             A list of {@link Section} objects that intersect this
     * address, or empty list if none.
     */
    public List<Section> findSectionsOn(long address) {
        return TreeListUtils.getItemsIntersectingIndex(address,
                                                       this.sectionTree);
    }

    /**
     * Find all the sections that have bytes that intersect with the address
     * range specified.
     *
     * @param startAddress      The beginning of the address range to look for.
     * (inclusive)
     * @param endAddress        The last address of the address range to look
     * for. (exclusive)
     * @return                  A list of {@link Section} objects that intersect
     * this address range, or empty list if none.
     */
    public List<Section> findSectionsOn(long startAddress, long endAddress) {
        return TreeListUtils.getItemsIntersectingIndexRange(
            startAddress, endAddress, this.sectionTree);
    }

    /**
     * Find all the sections that start at an address.
     *
     * @param address      The address to look for.
     * @return             A list of {@link Section} objects that start at the
     * address.
     */
    public List<Section> findSectionsAt(long address) {
        return TreeListUtils.getItemsAtStartIndex(address, this.sectionTree);
    }

    /**
     * Find all the sections that start between a range of addresses.
     *
     * @param startAddress      The beginning of the address range to look for.
     * (inclusive)
     * @param endAddress        The last address in the address to look for.
     * (exclusive)
     * @return                  A list of {@link Section} objects that that
     * start at this address, or null if none.
     */
    public List<Section> findSectionsAt(long startAddress, long endAddress) {
        return TreeListUtils.getItemsAtStartIndexRange(startAddress, endAddress,
                                                       this.sectionTree);
    }

    /**
     * De-serialize this Module from a protobuf .
     *
     * @param  protoModule   The module as serialized into a protocol buffer.
     * @return An initialized Module.
     */
    static Module fromProtobuf(ModuleOuterClass.Module protoModule) {
        return new Module(protoModule);
    }

    /**
     * Serialize this Module into a protobuf .
     *
     * @return Module protocol buffer.
     */
    ModuleOuterClass.Module.Builder toProtobuf() {
        ModuleOuterClass.Module.Builder protoModule =
            ModuleOuterClass.Module.newBuilder();
        protoModule.setUuid(Util.uuidToByteString(this.getUuid()));
        protoModule.setBinaryPath(this.getBinaryPath());
        protoModule.setPreferredAddr(this.getPreferredAddr());
        protoModule.setRebaseDelta(this.getRebaseDelta());
        // The enums values are mapped one-to-one
        protoModule.setFileFormatValue(this.fileFormat.ordinal());
        protoModule.setIsaValue(this.isa.ordinal());
        protoModule.setName(this.getName());
        if (this.byteOrder != null) {
            protoModule.setByteOrderValue(this.byteOrder.ordinal());
        }
        CodeBlock entryPoint = this.getEntryPoint();
        if (entryPoint != null) {
            protoModule.setEntryPoint(
                Util.uuidToByteString(this.getEntryPoint().getUuid()));
        }
        // Add collections by calling toProtobuf on each item
        for (Symbol symbol : this.symbolList)
            protoModule.addSymbols(symbol.toProtobuf());
        for (ProxyBlock proxyBlock : this.proxyBlockList)
            protoModule.addProxies(proxyBlock.toProtobuf());
        Iterator<Section> sectionIterator =
            new TreeListUtils<Section>(this.sectionTree).iterator();
        while (sectionIterator.hasNext()) {
            Section section = sectionIterator.next();
            protoModule.addSections(section.toProtobuf());
        }
        // Add auxData by calling toProtobuf on each type
        Map<String, AuxData> auxDataMap = getAuxDataMap();
        Set<String> auxDataNames = auxDataMap.keySet();
        for (String auxDataName : auxDataNames) {
            AuxData auxData = auxDataMap.get(auxDataName);
            protoModule.putAuxData(auxDataName, auxData.toProtobuf().build());
        }
        return protoModule;
    }
}
