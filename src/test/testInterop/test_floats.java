package com.grammatech.gtirb.testing;
import com.grammatech.gtirb;
import com.grammatech.gtirb.AuxData;
import com.grammatech.gtirb.AuxSerialization;
import com.grammatech.gtirb.AuxSerialization.AuxDataSerialization;

import java.io;
import java.util.Map;

public class testAuxData {
    public static void createFloats(String filename) {
        IR ir;
        Map<String, AuxData> auxDataMap = ir.getAuxDataMap();
        AuxData aFloat = AuxData(0.5, "AFloat", "float");
        auxDataMap.put("AFloat", aFloat);
        ir.saveFile(filename);
    }

    public static boolean checkFloats(String filename) {
        IR ir = IR.loadFile(filename);
        Map<String, AuxData> auxDataMap = ir.getAuxDataMap();
        AuxData auxData = auxDataMap.get("AFloat");
        AuxDataSerialization serialization =
            AuxDataSerialization(auxData.getData());
        Object obj = serialization.decode(auxData.getData(), auxData.getType());
        return obj == 0.5;
    }

    public static void main(String[] args) {
        if (args.length < 3) {
            System.exit(-1);
        }
        switch (args[1].charAt(1)) {
        case 'w':
            createFloats(args[2]);
            System.exit(0);
        case 'r':
            if (checkFloats(args[2])) {
                System.exit(0);
            }
        default:
            System.exit(1);
        }
    }
}
