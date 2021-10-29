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

import com.grammatech.gtirb.proto.IROuterClass;
import com.grammatech.gtirb.proto.ModuleOuterClass;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * A complete internal representation. IR describes the internal representation
 * of a software artifact.
 */
public class IR extends AuxDataContainer {

    private IROuterClass.IR protoIR;
    private List<Module> modules;
    private CFG cfg;
    private int version; // This is the protobuf version from the protoIr

    /**
     * Default class constructor for IR.
     */
    public IR() {
        super();
        // shouldn't this be null?
        this.protoIR = IROuterClass.IR.getDefaultInstance();
    }

    /**
     * Class constructor for IR from an IR protobuf.
     * @param  protoIr  The {@link IR} as serialized into a protocol buffer.
     */
    public IR(IROuterClass.IR protoIr) {
        super(protoIr.getUuid(), protoIr.getAuxDataMap());
        this.protoIR = protoIr;
    }

    /**
     * Load IR from protobuf.
     *
     * @return  true if load is successful, false otherwise.
     */
    private boolean loadProtobuf() {
        // If no protobuf, can't load it.
        if (this.protoIR == null)
            return false;
        this.version = protoIR.getVersion();
        // Import the modules
        this.modules = new ArrayList<Module>();
        for (ModuleOuterClass.Module protoModule : protoIR.getModulesList()) {
            Module module = Module.fromProtobuf(protoModule, this);
            this.modules.add(module);
        }
        // Import the CFG
        this.cfg = new CFG(protoIR.getCfg());
        return true;
    }

    /**
     * Load IR from a protobuf file stream.
     *
     * @return  IR if load is successful, null otherwise.
     */
    public static IR loadFile(InputStream fileIn) {
        IROuterClass.IR protoIr;
        try {
            protoIr = IROuterClass.IR.parseFrom(fileIn);
        } catch (FileNotFoundException fe) {
            return null;
        } catch (IOException ie) {
            return null;
        }
        IR ir = new IR(protoIr);
        boolean rv = ir.loadProtobuf();
        if (rv == true)
            return ir;
        return null;
    }

    /**
     * Load IR from a protobuf file.
     *
     * @return  IR if load is successful, null otherwise.
     */
    public static IR loadFile(String fileInName) {
        try {
            File fileIn = new File(fileInName);
            FileInputStream fileInputStream = new FileInputStream(fileIn);
            return loadFile(fileInputStream);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Get the list of modules belonging to this {@link IR}.
     *
     * @return  A {@link Module} list.
     */
    public List<Module> getModules() { return this.modules; }

    /**
     * Set the list of modules belonging to this {@link IR}.
     *
     * @param modules  A {@link Module} list.
     */
    public void setModules(List<Module> modules) { this.modules = modules; }

    /**
     * Get the CFG belonging to this {@link IR}.
     *
     * @return  A {@link CFG}.
     */
    public CFG getCfg() { return this.cfg; }

    /**
     * Set the CFG belonging to this {@link IR}.
     *
     * @param cfg  A {@link CFG}.
     */
    public void setCfg(CFG cfg) { this.cfg = cfg; }

    /**
     * Get the original protobuf of this {@link IR}.
     *
     * @return The protobuf the IR was imported from, or the IR
     * {@link com.grammatech.gtirb.proto.IROuterClass.IR#getDefaultInstance()
     * DefaultInstance} if it was not imported from a protobuf.
     */
    public IROuterClass.IR getProtoIR() { return this.protoIR; }

    /**
     * Get the protobuf version of this {@link IR}.
     *
     * @return Protobuf version.
     */
    public int getVersion() { return this.version; }

    /**
     * Set the protobuf version of this {@link IR}.
     *
     * @param version Protobuf version.
     */
    public void setVersion(int version) { this.version = version; }

    /**
     * Serialize this IR into a protobuf.
     *
     * @return IR protocol buffer.
     */
    public IROuterClass.IR.Builder toProtobuf() {
        IROuterClass.IR.Builder protoIr = IROuterClass.IR.newBuilder();
        protoIr.setUuid(Util.uuidToByteString(this.getUuid()));
        protoIr.setVersion(this.version);
        // Add modules
        for (Module module : this.modules) {
            ModuleOuterClass.Module.Builder protoModule = module.toProtobuf();
            protoIr.addModules(protoModule);
        }
        // Add CFG
        protoIr.setCfg(this.cfg.toProtobuf());
        return protoIr;
    }

    /**
     * Save IR to a protobuf file stream.
     */
    public void saveFile(OutputStream fileOut) throws IOException {
        IROuterClass.IR protoIr = this.toProtobuf().build();
        protoIr.writeTo(fileOut);
    }

    /**
     * Save IR to a protobuf file.
     */
    public void saveFile(String fileOutName) throws IOException {
        File fileOut = new File(fileOutName);
        FileOutputStream fileOutputStream = new FileOutputStream(fileOut);
        this.saveFile(fileOutputStream);
    }
}
