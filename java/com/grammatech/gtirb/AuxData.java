/** */
package com.grammatech.gtirb;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class AuxData {

    Map<UUID, ArrayList<UUID>> functionEntries =
        new HashMap<UUID, ArrayList<UUID>>();
    Map<UUID, ArrayList<UUID>> functionBlocks =
        new HashMap<UUID, ArrayList<UUID>>();

    public boolean initializeFunctionEntries(
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoAuxData) {
        Serialization serialization =
            new Serialization(protoAuxData.getData().toByteArray());
        int numFunctionEntries = serialization.getSize();

        int numEntryPoints;
        UUID functionUuid;
        UUID entryPointUuid;
        for (int i = 0; i < numFunctionEntries; i++) {
            functionUuid = serialization.getUuid();
            numEntryPoints = serialization.getSize();
            ArrayList<UUID> pointList = new ArrayList<UUID>();
            for (int j = 0; j < numEntryPoints; j++) {
                entryPointUuid = serialization.getUuid();
                pointList.add(entryPointUuid);
            }
            functionEntries.put(functionUuid, pointList);
        }
        return true;
    }

    public Map<UUID, ArrayList<UUID>> getFunctionEntries() {
        return this.functionEntries;
    }

    public boolean initializeFunctionBlocks(
        com.grammatech.gtirb.proto.AuxDataOuterClass.AuxData protoAuxData) {
        Serialization serialization =
            new Serialization(protoAuxData.getData().toByteArray());
        int numFunctions = serialization.getSize();

        int numBlocks;
        UUID functionUuid;
        UUID blockUuid;
        for (int i = 0; i < numFunctions; i++) {
            functionUuid = serialization.getUuid();
            numBlocks = serialization.getSize();
            ArrayList<UUID> blockList = new ArrayList<UUID>();
            for (int j = 0; j < numBlocks; j++) {
                blockUuid = serialization.getUuid();
                blockList.add(blockUuid);
            }
            functionBlocks.put(functionUuid, blockList);
        }
        return true;
    }

    public Map<UUID, ArrayList<UUID>> getFunctionBlocks() {
        return this.functionBlocks;
    }
}
