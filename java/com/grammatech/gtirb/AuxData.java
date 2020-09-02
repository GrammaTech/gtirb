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

import com.grammatech.gtirb.proto.AuxDataOuterClass;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class AuxData {

    private Map<String, com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData>
        protoAuxDataMap;

    public AuxData(
        Map<String, com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData>
            protoAuxDataMap) {
        this.protoAuxDataMap = protoAuxDataMap;
    }

    public Set<String> getAuxDataTypes() {
        return this.protoAuxDataMap.keySet();
    }

    public Map<UUID, Long> getAlignment() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoAlignment =
            this.protoAuxDataMap.get("alignment");
        if (protoAlignment != null) {
            Map<UUID, Long> alignment = new HashMap<UUID, Long>();
            Serialization serialization =
                new Serialization(protoAlignment.getData().toByteArray());
            long numAlignment = serialization.getLong();

            for (int i = 0; i < numAlignment; i++) {
                UUID alignmentUuid = serialization.getUuid();
                Long alignmentValue = serialization.getLong();
                alignment.put(alignmentUuid, alignmentValue);
            }
            return alignment;
        }
        return null;
    }

    public ArrayList<String> getBinaryType() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoBinaryType =
            this.protoAuxDataMap.get("binaryType");
        if (protoBinaryType != null) {
            ArrayList<String> binaryTypeList = new ArrayList<String>();
            Serialization serialization =
                new Serialization(protoBinaryType.getData().toByteArray());
            long numTypes = serialization.getLong();

            for (int i = 0; i < numTypes; i++) {
                String aType = serialization.getString();
                binaryTypeList.add(aType);
            }
            return binaryTypeList;
        }
        return null;
    }

    public Map<Offset, ArrayList<Directive>> getCfiDirectives() {
        com.grammatech.gtirb.proto.AuxDataOuterClass
            .AuxData protoCfiDirectives =
            this.protoAuxDataMap.get("cfiDirectives");
        if (protoCfiDirectives != null) {
            Map<Offset, ArrayList<Directive>> cfiDirectives =
                new HashMap<Offset, ArrayList<Directive>>();
            Serialization serialization =
                new Serialization(protoCfiDirectives.getData().toByteArray());
            long numCfiDirectives = serialization.getLong();

            for (int i = 0; i < numCfiDirectives; i++) {

                // Each CFI DIRECTIVE has an Offset
                UUID elementId = serialization.getUuid();
                long displacement = serialization.getLong();
                Offset offset = new Offset(elementId, displacement);

                // Each CFI DIRECTIVE has a list of Directives
                long numDirectives = serialization.getLong();
                ArrayList<Directive> directiveList = new ArrayList<Directive>();

                for (int j = 0; j < numDirectives; j++) {

                    // Get a directive
                    String directiveString = serialization.getString();
                    ArrayList<Long> directiveValues = new ArrayList<Long>();
                    long numValues = serialization.getLong();
                    for (int k = 0; k < numValues; k++) {
                        Long value = Long.valueOf(serialization.getLong());
                        directiveValues.add(value);
                    }
                    UUID directiveUuid = serialization.getUuid();

                    // Put in list
                    Directive directive = new Directive(
                        directiveString, directiveValues, directiveUuid);
                    directiveList.add(directive);
                }
                cfiDirectives.put(offset, directiveList);
            }
            return cfiDirectives;
        }
        return null;
    }

    public Map<Offset, String> getComments() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoComments =
            this.protoAuxDataMap.get("comments");
        if (protoComments != null) {
            Map<Offset, String> comments = new HashMap<Offset, String>();
            Serialization serialization =
                new Serialization(protoComments.getData().toByteArray());
            long numComments = serialization.getLong();

            for (int i = 0; i < numComments; i++) {
                UUID elementId = serialization.getUuid();
                long displacement = serialization.getLong();
                Offset offset = new Offset(elementId, displacement);
                String comment = serialization.getString();
                comments.put(offset, comment);
            }
            return comments;
        }
        return null;
    }

    public Map<UUID, ArrayList<Long>> getElfSectionProperties() {
        com.grammatech.gtirb.proto.AuxDataOuterClass
            .AuxData protoElfSectionProperties =
            this.protoAuxDataMap.get("elfSectionProperties");
        if (protoElfSectionProperties != null) {
            Map<UUID, ArrayList<Long>> elfSectionProperties =
                new HashMap<UUID, ArrayList<Long>>();
            Serialization serialization = new Serialization(
                protoElfSectionProperties.getData().toByteArray());
            long numSectionProperties = serialization.getLong();

            for (int i = 0; i < numSectionProperties; i++) {
                UUID sectionUuid = serialization.getUuid();
                ArrayList<Long> propertyList = new ArrayList<Long>();
                long sectionType = serialization.getLong();
                propertyList.add(Long.valueOf(sectionType));
                long sectionFlags = serialization.getLong();
                propertyList.add(Long.valueOf(sectionFlags));
                elfSectionProperties.put(sectionUuid, propertyList);
            }
            return elfSectionProperties;
        }
        return null;
    }

    public Map<UUID, SymbolInfo> getElfSymbolInfo() {
        com.grammatech.gtirb.proto.AuxDataOuterClass
            .AuxData protoElfSymbolInfo =
            this.protoAuxDataMap.get("elfSymbolInfo");
        if (protoElfSymbolInfo != null) {
            Map<UUID, SymbolInfo> elfSymbolInfo =
                new HashMap<UUID, SymbolInfo>();
            Serialization serialization =
                new Serialization(protoElfSymbolInfo.getData().toByteArray());
            long numSymbolInfo = serialization.getLong();

            for (int i = 0; i < numSymbolInfo; i++) {
                UUID symbolUuid = serialization.getUuid();
                long symbolSize = serialization.getLong();
                String symbolType = serialization.getString();
                String symbolBinding = serialization.getString();
                String symbolVisibility = serialization.getString();
                long symbolSection = serialization.getLong();
                SymbolInfo symbolInfo =
                    new SymbolInfo(symbolSize, symbolType, symbolBinding,
                                   symbolVisibility, symbolSection);
                elfSymbolInfo.put(symbolUuid, symbolInfo);
            }
            return elfSymbolInfo;
        }
        return null;
    }

    public Map<UUID, String> getEncodings() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoEncodings =
            this.protoAuxDataMap.get("encodings");
        if (protoEncodings != null) {
            Map<UUID, String> encodings = new HashMap<UUID, String>();
            Serialization serialization =
                new Serialization(protoEncodings.getData().toByteArray());
            long numEncodings = serialization.getLong();

            for (int i = 0; i < numEncodings; i++) {
                UUID encodingUuid = serialization.getUuid();
                String encodingString = serialization.getString();
                encodings.put(encodingUuid, encodingString);
            }
            return encodings;
        }
        return null;
    }

    public Map<UUID, ArrayList<UUID>> getFunctionEntries() {
        com.grammatech.gtirb.proto.AuxDataOuterClass
            .AuxData protoFunctionEntries =
            this.protoAuxDataMap.get("functionEntries");
        if (protoFunctionEntries != null) {
            Map<UUID, ArrayList<UUID>> functionEntries =
                new HashMap<UUID, ArrayList<UUID>>();
            Serialization serialization =
                new Serialization(protoFunctionEntries.getData().toByteArray());
            long numFunctionEntries = serialization.getLong();

            for (int i = 0; i < numFunctionEntries; i++) {
                UUID functionUuid = serialization.getUuid();
                int numEntryPoints = (int)serialization.getLong();
                ArrayList<UUID> pointList = new ArrayList<UUID>();
                for (int j = 0; j < numEntryPoints; j++) {
                    UUID entryPointUuid = serialization.getUuid();
                    pointList.add(entryPointUuid);
                }
                functionEntries.put(functionUuid, pointList);
            }
            return functionEntries;
        }
        return null;
    }

    public Map<UUID, ArrayList<UUID>> getFunctionBlocks() {
        com.grammatech.gtirb.proto.AuxDataOuterClass
            .AuxData protoFunctionBlocks =
            this.protoAuxDataMap.get("functionBlocks");
        if (protoFunctionBlocks != null) {
            Map<UUID, ArrayList<UUID>> functionBlocks =
                new HashMap<UUID, ArrayList<UUID>>();
            Serialization serialization =
                new Serialization(protoFunctionBlocks.getData().toByteArray());
            long numFunctions = serialization.getLong();

            for (int i = 0; i < numFunctions; i++) {
                UUID functionUuid = serialization.getUuid();
                int numBlocks = (int)serialization.getLong();
                ArrayList<UUID> blockList = new ArrayList<UUID>();
                for (int j = 0; j < numBlocks; j++) {
                    UUID blockUuid = serialization.getUuid();
                    blockList.add(blockUuid);
                }
                functionBlocks.put(functionUuid, blockList);
            }
            return functionBlocks;
        }
        return null;
    }

    public ArrayList<String> getLibraries() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoLibraries =
            this.protoAuxDataMap.get("libraries");
        if (protoLibraries != null) {
            ArrayList<String> libraries = new ArrayList<String>();
            Serialization serialization =
                new Serialization(protoLibraries.getData().toByteArray());
            long numLibraries = serialization.getLong();

            for (int i = 0; i < numLibraries; i++) {
                String library = serialization.getString();
                libraries.add(library);
            }
            return libraries;
        }
        return null;
    }

    public ArrayList<String> getLibraryPaths() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoLibraryPaths =
            this.protoAuxDataMap.get("libraryPaths");
        if (protoLibraryPaths != null) {
            ArrayList<String> libraryPaths = new ArrayList<String>();
            Serialization serialization =
                new Serialization(protoLibraryPaths.getData().toByteArray());
            long numLibraryPaths = serialization.getLong();

            for (int i = 0; i < numLibraryPaths; i++) {
                String libraryPath = serialization.getString();
                libraryPaths.add(libraryPath);
            }
            return libraryPaths;
        }
        return null;
    }

    public Map<Long, Long> getPadding() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoPadding =
            this.protoAuxDataMap.get("padding");
        if (protoPadding != null) {
            Map<Long, Long> padding = new HashMap<Long, Long>();
            Serialization serialization =
                new Serialization(protoPadding.getData().toByteArray());
            long numPadding = serialization.getLong();

            for (int i = 0; i < numPadding; i++) {
                Long paddingAddr = serialization.getLong();
                Long paddingValue = serialization.getLong();
                padding.put(paddingAddr, paddingValue);
            }
            return padding;
        }
        return null;
    }

    public Map<UUID, UUID> getSymbolForwarding() {
        com.grammatech.gtirb.proto.AuxDataOuterClass
            .AuxData protoSymbolForwarding =
            this.protoAuxDataMap.get("symbolForwarding");
        if (protoSymbolForwarding != null) {
            Map<UUID, UUID> symbolForwarding = new HashMap<UUID, UUID>();
            Serialization serialization = new Serialization(
                protoSymbolForwarding.getData().toByteArray());
            long numSymbolForwarding = serialization.getLong();

            for (int i = 0; i < numSymbolForwarding; i++) {
                UUID fromUuid = serialization.getUuid();
                UUID toUuid = serialization.getUuid();
                symbolForwarding.put(fromUuid, toUuid);
            }
            return symbolForwarding;
        }
        return null;
    }

    public Map<UUID, Long> getSCCs() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoSCCs =
            this.protoAuxDataMap.get("SCCs");
        if (protoSCCs != null) {
            Map<UUID, Long> SCCs = new HashMap<UUID, Long>();
            Serialization serialization =
                new Serialization(protoSCCs.getData().toByteArray());
            long numSCCs = serialization.getLong();

            for (int i = 0; i < numSCCs; i++) {
                UUID sccUuid = serialization.getUuid();
                Long sccAddr = serialization.getLong();
                SCCs.put(sccUuid, sccAddr);
            }
            return SCCs;
        }
        return null;
    }

    public Map<UUID, String> getTypes() {
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoTypes =
            this.protoAuxDataMap.get("types");
        if (protoTypes != null) {
            Map<UUID, String> types = new HashMap<UUID, String>();
            Serialization serialization =
                new Serialization(protoTypes.getData().toByteArray());
            long numTypes = serialization.getLong();

            for (int i = 0; i < numTypes; i++) {
                UUID typeUuid = serialization.getUuid();
                String typeString = serialization.getString();
                types.put(typeUuid, typeString);
            }
            return types;
        }
        return null;
    }

    public AuxDataOuterClass.AuxData getProtoAuxData(String auxDataType) {
        return protoAuxDataMap.get(auxDataType);
    }
}
