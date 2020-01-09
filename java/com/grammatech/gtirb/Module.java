package com.grammatech.gtirb;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class Module extends Node {

    enum FileFormat {
        Format_Undefined(
                com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.Format_Undefined_VALUE),
        COFF(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.COFF_VALUE),
        ELF(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.ELF_VALUE),
        PE(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.PE_VALUE),
        IdaProDb32(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.IdaProDb32_VALUE),
        IdaProDb64(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.IdaProDb64_VALUE),
        XCOFF(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.XCOFF_VALUE),
        MACHO(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.MACHO_VALUE),
        RAW(com.grammatech.gtirb.proto.ModuleOuterClass.FileFormat.RAW_VALUE);

        private int value;

        private FileFormat(int value) {
            this.value = value;
        }
    };

    enum ISA {
        ISA_Undefined(com.grammatech.gtirb.proto.ModuleOuterClass.ISA.ISA_Undefined_VALUE),
        IA32(com.grammatech.gtirb.proto.ModuleOuterClass.ISA.IA32_VALUE),
        PPC32(com.grammatech.gtirb.proto.ModuleOuterClass.ISA.PPC32_VALUE),
        X64(com.grammatech.gtirb.proto.ModuleOuterClass.ISA.X64_VALUE),
        ARM(com.grammatech.gtirb.proto.ModuleOuterClass.ISA.ARM_VALUE),
        ValidButUnsupported(
                com.grammatech.gtirb.proto.ModuleOuterClass.ISA.ValidButUnsupported_VALUE);

        private int value;

        private ISA(int value) {
            this.value = value;
        }
    };

    private com.grammatech.gtirb.proto.ModuleOuterClass.Module protoModule;

    private String binaryPath;
    private long preferredAddr;
    private long rebaseDelta;
    private ISA isa;
    private FileFormat fileFormat;
    private ArrayList<Section> sectionList;
    private ArrayList<Symbol> symbolList;
    private ArrayList<ProxyBlock> proxyBlockList;
    private AuxData auxData;
    private String name;
    private UUID entryPointUuid;

    public Module(com.grammatech.gtirb.proto.ModuleOuterClass.Module protoModule) {
        this.protoModule = protoModule;

        UUID uuid = Util.byteStringToUuid(protoModule.getUuid());
        super.setUuid(uuid);
        this.binaryPath = protoModule.getBinaryPath();
        this.preferredAddr = protoModule.getPreferredAddr();
        this.rebaseDelta = protoModule.getRebaseDelta();
        this.fileFormat = FileFormat.values()[protoModule.getFileFormatValue()];
        this.isa = ISA.values()[protoModule.getIsaValue()];
        this.name = protoModule.getName();
        this.entryPointUuid = Util.byteStringToUuid(protoModule.getEntryPoint());
    }

    public boolean initializeSectionList() {

        this.sectionList = new ArrayList<Section>();

        // For each section, add to sectionList in this class
        List<com.grammatech.gtirb.proto.SectionOuterClass.Section> protoSectionList =
                protoModule.getSectionsList();
        for (com.grammatech.gtirb.proto.SectionOuterClass.Section protoSection : protoSectionList) {
            Section newSection = new Section(protoSection);
            sectionList.add(newSection);
        }
        return true;
    }

    public boolean initializeSymbolList() {

        this.symbolList = new ArrayList<Symbol>();

        // For each symbol, add to symbolList in this class
        List<com.grammatech.gtirb.proto.SymbolOuterClass.Symbol> protoSymbolList =
                protoModule.getSymbolsList();
        for (com.grammatech.gtirb.proto.SymbolOuterClass.Symbol protoSymbol : protoSymbolList) {
            Symbol newSymbol = new Symbol(protoSymbol);
            symbolList.add(newSymbol);
        }
        return true;
    }

    public boolean initializeProxyBlockList() {

        this.proxyBlockList = new ArrayList<ProxyBlock>();

        // For each proxy block, add to proxyBlockList in this class
        List<com.grammatech.gtirb.proto.ProxyBlockOuterClass.ProxyBlock> protoProxyBlockList =
                protoModule.getProxiesList();
        for (com.grammatech.gtirb.proto.ProxyBlockOuterClass.ProxyBlock protoProxyBlock :
                protoProxyBlockList) {
            ProxyBlock newProxyBlock = new ProxyBlock(protoProxyBlock);
            proxyBlockList.add(newProxyBlock);
        }
        return true;
    }

    public boolean initializeAuxData() {
        this.auxData = new AuxData();

        Map<String, com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData> auxDataMap =
                this.protoModule.getAuxDataMap();
        if (auxDataMap == null) {
            return false;
        }
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoFunctionEntries =
                auxDataMap.get("functionEntries");
        if (protoFunctionEntries != null) {
            auxData.initializeFunctionEntries(protoFunctionEntries);
        }

        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoFunctionBlocks =
                auxDataMap.get("functionBlocks");
        if (protoFunctionBlocks != null) {
            auxData.initializeFunctionBlocks(protoFunctionBlocks);
        }

        return true;
    }

    public ArrayList<Section> getSections() {
        return this.sectionList;
    }

    public ArrayList<Symbol> getSymbols() {
        return this.symbolList;
    }

    public int getFileFormat() {
        return this.protoModule.getFileFormatValue();
    }

    public int getISA() {
        return this.protoModule.getIsaValue();
    }

    public String getName() {
        return this.name;
    }

    public AuxData getAuxData() {
        return this.auxData;
    }

    public CodeBlock getEntryPoint() {
        Node cb = Node.getByUuid(this.entryPointUuid);
        if ((cb != null) && (cb instanceof CodeBlock)) return (CodeBlock) cb;
        else return null;
    }
}
