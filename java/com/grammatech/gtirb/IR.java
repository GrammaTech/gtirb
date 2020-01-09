package com.grammatech.gtirb;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

public class IR {

    private com.grammatech.gtirb.proto.IROuterClass.IR protoIR;
    // TODO: Make this a list
    private Module module;

    public IR() {
        this.protoIR = com.grammatech.gtirb.proto.IROuterClass.IR.getDefaultInstance();
    }

    public static IR loadFile(InputStream fileIn) {
        IR ir = new IR();
        boolean rv = ir.doLoadFile(fileIn);
        if (rv == true) {
            return ir;
        } else {
            return null;
        }
    }

    private boolean doLoadFile(InputStream fileIn) {
        try {
            this.protoIR = com.grammatech.gtirb.proto.IROuterClass.IR.parseFrom(fileIn);
        } catch (FileNotFoundException fe) {
            return false;
        } catch (IOException ie) {
            return false;
        }

        // Create a GTIRB API Module from the first protobuf Module
        com.grammatech.gtirb.proto.ModuleOuterClass.Module m = protoIR.getModulesList().get(0);
        if (m == null) {
            return false;
        }

        this.module = new Module(m);
        boolean sectionListInitialized = module.initializeSectionList();
        boolean symbolListInitialized = module.initializeSymbolList();
        boolean proxyBlockListInitialized = module.initializeProxyBlockList();
        boolean auxDataInitialized = module.initializeAuxData();

        if ((!sectionListInitialized)
                || (!symbolListInitialized)
                || (!proxyBlockListInitialized)
                || (!auxDataInitialized)) {
            return false;
        }
        return true;
    }

    public Module getModule() {
        return this.module;
    }
}
