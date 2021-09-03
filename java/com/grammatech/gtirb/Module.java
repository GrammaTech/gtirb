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

import com.grammatech.gtirb.proto.ModuleOuterClass;
import com.grammatech.gtirb.proto.ProxyBlockOuterClass;
import com.grammatech.gtirb.proto.SectionOuterClass;
import com.grammatech.gtirb.proto.SymbolOuterClass;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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

    private ModuleOuterClass.Module protoModule;

    private IR ir;
    private String binaryPath;
    private long preferredAddr;
    private long rebaseDelta;
    private FileFormat fileFormat;
    private ISA isa;
    private String name;
    private TreeMap<Long, List<Section>> sectionTree;
    private List<Symbol> symbolList;
    private List<ProxyBlock> proxyBlockList;
    private UUID entryPointUuid;
    private ByteOrder byteOrder;

    /**
     * Class constructor for a Module from a protobuf module.
     * @param  protoModule   The module as serialized into a protocol buffer.
     * @param  ir            The IR that owns this Module.
     */
    public Module(ModuleOuterClass.Module protoModule, IR ir) {
        super(protoModule.getAuxDataMap());
        this.protoModule = protoModule;
        this.uuid = Util.byteStringToUuid(protoModule.getUuid());
        this.binaryPath = protoModule.getBinaryPath();
        this.preferredAddr = protoModule.getPreferredAddr();
        this.rebaseDelta = protoModule.getRebaseDelta();
        this.fileFormat = FileFormat.values()[protoModule.getFileFormatValue()];
        this.isa = ISA.values()[protoModule.getIsaValue()];
        this.name = protoModule.getName();
        this.entryPointUuid =
            Util.byteStringToUuid(protoModule.getEntryPoint());
        this.byteOrder = ByteOrder.values()[protoModule.getByteOrderValue()];
        this.ir = ir;
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
     * @param  entryPoint       The entry point of this module if known, null
     * otherwise.
     * @param  ir               The Intermediate Representation that owns this
     * Section.
     */
    public Module(String binaryPath, long preferredAddr, long rebaseDelta,
                  FileFormat fileFormat, ISA isa, String name,
                  List<Section> sections, List<Symbol> symbols,
                  List<ProxyBlock> proxyBlocks, CodeBlock entryPoint, IR ir) {
        super(null);
        this.protoModule = null;
        this.uuid = UUID.randomUUID();
        this.binaryPath = binaryPath;
        this.preferredAddr = preferredAddr;
        this.rebaseDelta = rebaseDelta;
        this.fileFormat = fileFormat;
        this.isa = isa;
        this.name = name;
        this.symbolList = symbols;
        this.proxyBlockList = proxyBlocks;
        this.entryPointUuid = entryPoint.getUuid();
        this.ir = ir;
    }

    /**
     * Get the {@link IR} this Module belongs to.
     *
     * @return  The IR this module belongs to.
     */
    public IR getIr() { return this.ir; }

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
     * Get the section list of this Module.
     *
     * @return  The module section list.
     */
    public List<Section> getSections() {
        List<Section> sectionList = new ArrayList<Section>();
        for (List<Section> entry : this.sectionTree.values()) {
            sectionList.addAll(entry);
        }
        return sectionList;
    }

    /**
     * Set the section list of this Module.
     *
     * @param sectionList    The module section list.
     */
    public void setSections(List<Section> sectionList) {
        this.sectionTree.clear();
        for (Section section : sectionList)
            TreeListUtils.insertItem(section, this.sectionTree);
    }

    /**
     * Get the symbol list of this Module.
     *
     * @return  The module symbol list.
     */
    public List<Symbol> getSymbols() { return this.symbolList; }

    /**
     * Set the symbol list of this Module.
     *
     * @param symbolList   The module symbol list.
     */
    public void setSymbols(List<Symbol> symbolList) {
        this.symbolList = symbolList;
    }

    /**
     * Get the proxy block list of this Module.
     *
     * @return  The module proxy block list.
     */
    public List<ProxyBlock> getProxyBlocks() { return this.proxyBlockList; }

    /**
     * Set the proxy block list of this Module.
     *
     * @param proxyBlockList    The module proxy block list.
     */
    public void setProxyBlocks(List<ProxyBlock> proxyBlockList) {
        this.proxyBlockList = proxyBlockList;
    }

    /**
     * Get the entry point of this module, or null if not present.
     *
     * @return  The module entry point (a code block) or null if no entry is
     * point has been designated.
     */
    public CodeBlock getEntryPoint() {
        Node cb = Node.getByUuid(this.entryPointUuid);
        if ((cb != null) && (cb instanceof CodeBlock))
            return (CodeBlock)cb;
        return null;
    }

    /**
     * Set the entry point of this Module.
     *
     * @param entryCodeBlock    The module entry point (a {@link CodeBlock}).
     */
    public void setEntryPoint(CodeBlock entryCodeBlock) {
        this.entryPointUuid = entryCodeBlock.uuid;
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
     * Get the original protobuf of this Module.
     *
     * @return The protobuf the module was imported from, or
     * null if it was not imported from a protobuf.
     */
    public ModuleOuterClass.Module getProtoModule() { return this.protoModule; }

    /**
     * Initialize this module's sections from section protocol buffers
     *
     * When creating a Module from a protocol buffer module, use this method to
     * initialize the sections belonging to this module from the protocol
     * buffers of those sections.
     *
     */
    private void initializeSectionList() {
        this.sectionTree = new TreeMap<Long, List<Section>>();
        // For each section, add to sectionList in this class
        List<SectionOuterClass.Section> protoSectionList =
            protoModule.getSectionsList();
        for (SectionOuterClass.Section protoSection : protoSectionList) {
            Section newSection = new Section(protoSection, this);
            TreeListUtils.insertItem(newSection, this.sectionTree);
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
    private void initializeSymbolList() {
        this.symbolList = new ArrayList<Symbol>();
        // For each symbol, add to symbolList in this class
        List<SymbolOuterClass.Symbol> protoSymbolList =
            protoModule.getSymbolsList();
        for (SymbolOuterClass.Symbol protoSymbol : protoSymbolList) {
            Symbol newSymbol = new Symbol(protoSymbol, this);
            symbolList.add(newSymbol);
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
    private void initializeProxyBlockList() {
        this.proxyBlockList = new ArrayList<ProxyBlock>();
        // For each proxy block, add to proxyBlockList in this class
        List<ProxyBlockOuterClass.ProxyBlock> protoProxyBlockList =
            protoModule.getProxiesList();
        for (ProxyBlockOuterClass.ProxyBlock protoProxyBlock :
             protoProxyBlockList) {
            ProxyBlock newProxyBlock = new ProxyBlock(protoProxyBlock, this);
            proxyBlockList.add(newProxyBlock);
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
     * @param endAddress        The last address of the address range to look
     * for.
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
     * @param endAddress        The last address in the address to look for.
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
     * @param  ir            The IR that owns this Module.
     * @return An initialized Module.
     */
    public static Module fromProtobuf(ModuleOuterClass.Module protoModule,
                                      IR ir) {
        Module module = new Module(protoModule, ir);
        module.initializeSectionList();
        module.initializeSymbolList();
        module.initializeProxyBlockList();
        return module;
    }

    /**
     * Serialize this Module into a protobuf .
     *
     * @return Module protocol buffer.
     */
    public ModuleOuterClass.Module.Builder toProtobuf() {
        ModuleOuterClass.Module.Builder protoModule =
            ModuleOuterClass.Module.newBuilder();
        protoModule.setUuid(Util.uuidToByteString(this.getUuid()));
        protoModule.setBinaryPath(this.getBinaryPath());
        protoModule.setPreferredAddr(this.getPreferredAddr());
        protoModule.setRebaseDelta(this.getRebaseDelta());
        // The enums values are mapped one-to-one
        protoModule.setFileFormatValue(this.fileFormat.ordinal());
        protoModule.setIsaValue(this.isa.ordinal());
        protoModule.setByteOrderValue(this.byteOrder.ordinal());
        protoModule.setEntryPoint(
            Util.uuidToByteString(this.getEntryPoint().getUuid()));
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
