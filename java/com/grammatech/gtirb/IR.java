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

import com.grammatech.gtirb.proto.IROuterClass;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;

public class IR {

    private com.grammatech.gtirb.proto.IROuterClass.IR protoIR;
    // TODO: Need to support a list of modules, not just one.
    private Module module;
    private CFG cfg;
    private UUID uuid;
    private int version;

    public IR() {
        this.protoIR =
            com.grammatech.gtirb.proto.IROuterClass.IR.getDefaultInstance();
    }

    public static IR loadFile(InputStream fileIn) {
        IR ir = new IR();
        boolean rv = ir.doLoadFile(fileIn);
        if (rv == true) {
            return ir;
        }
        return null;
    }

    // TODO: support opening a file based on file name, this is experimental
    public static IR loadFileByName(String filename) {
        try {
            File fileIn = new File(filename);
            FileInputStream fileInputStream = new FileInputStream(fileIn);
            return loadFile(fileInputStream);
        } catch (Exception e) {
            return null;
        }
    }

    private boolean doLoadFile(InputStream fileIn) {
        try {
            this.protoIR =
                com.grammatech.gtirb.proto.IROuterClass.IR.parseFrom(fileIn);
        } catch (FileNotFoundException fe) {
            return false;
        } catch (IOException ie) {
            return false;
        }

        // True, these aren't used. But won't they be needed eventually?
        this.version = protoIR.getVersion();
        this.uuid = Util.byteStringToUuid(protoIR.getUuid());

        // Create a GTIRB API Module from the first protobuf Module
        com.grammatech.gtirb.proto.ModuleOuterClass.Module m =
            protoIR.getModulesList().get(0);
        if (m == null) {
            return false;
        }

        this.module = new Module(m);
        boolean sectionListInitialized = module.initializeSectionList();
        boolean symbolListInitialized = module.initializeSymbolList();
        boolean proxyBlockListInitialized = module.initializeProxyBlockList();
        boolean auxDataInitialized = module.initializeAuxData();

        com.grammatech.gtirb.proto.CFGOuterClass.CFG protoCfg =
            protoIR.getCfg();
        this.cfg = new CFG(protoCfg);

        if ((!sectionListInitialized) || (!symbolListInitialized) ||
            (!proxyBlockListInitialized) || (!auxDataInitialized)) {
            return false;
        }
        return true;
    }

    public Module getModule() { return this.module; }

    public CFG getCfg() { return this.cfg; }

    public IROuterClass.IR getProtoIR() { return this.protoIR; }
}
