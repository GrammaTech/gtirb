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

import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.proto.IROuterClass;
import com.grammatech.gtirb.proto.ModuleOuterClass;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * A complete internal representation. IR describes the internal representation
 * of a software artifact.
 */
public class IR extends AuxDataContainer {

    private List<Module> modules;
    private CFG cfg;
    private int version; // This is the protobuf version from the protoIr

    static final private byte[] GTIRB_MAGIC_CHARS =
        "GTIRB".getBytes(Charset.forName("ASCII"));
    static final private int GTIRB_MAGIC_LENGTH = 5;

    /**
     * Default class constructor for IR.
     */
    public IR() {
        super();
        this.modules = new ArrayList<Module>();
    }

    /**
     * Class constructor for IR from an IR protobuf.
     * @param  protoIr  The {@link IR} as serialized into a protocol buffer.
     */
    private IR(IROuterClass.IR protoIr) {
        super(protoIr.getUuid(), protoIr.getAuxDataMap());
    }

    /**
     * Load IR from protobuf.
     *
     * @return  The {@link IR} loaded from the protobuf.
     */
    private static IR loadProtobuf(IROuterClass.IR protoIr) {
        // If no protobuf, can't load it.
        if (protoIr == null)
            return null;
        IR ir = new IR(protoIr);
        ir.version = protoIr.getVersion();
        // Import the modules
        ir.modules = new ArrayList<Module>();
        for (ModuleOuterClass.Module protoModule : protoIr.getModulesList()) {
            Module module = Module.fromProtobuf(protoModule);
            ir.modules.add(module);
        }
        // Import the CFG
        ir.cfg = new CFG(protoIr.getCfg());
        return ir;
    }

    /**
     * Load IR from a protobuf file stream.
     *
     * @return  IR if load is successful, null otherwise.
     */
    public static IR loadFile(InputStream fileIn) {
        byte[] magic = new byte[GTIRB_MAGIC_LENGTH];
        try {
            // Magic signature
            // Bytes 0-4 contain the ASCII characters: GTIRB.
            // Bytes 5-6 are considered reserved for future use and should be 0.
            // Byte 7 contains the GTIRB protobuf spec version in use.
            int bytes_read = fileIn.read(magic);
            if (bytes_read != GTIRB_MAGIC_LENGTH ||
                !Arrays.equals(magic, GTIRB_MAGIC_CHARS)) {
                return null;
            }
            fileIn.skip(2);
            int ver = fileIn.read();
            if (ver != Version.gtirbProtobufVersion) {
                return null;
            }
        } catch (IOException ie) {
            return null;
        }

        IROuterClass.IR protoIr;
        try {
            protoIr = IROuterClass.IR.parseFrom(fileIn);
        } catch (FileNotFoundException fe) {
            return null;
        } catch (IOException ie) {
            return null;
        }
        IR ir = IR.loadProtobuf(protoIr);
        return ir;
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
     * @return  An unmodifiable {@link Module} list of all the
     * modules in this {@link IR}. Any attempt to remove an element of
     * this list will throw an UnsupportedOperationException.
     */
    public List<Module> getModules() {
        return Collections.unmodifiableList(this.modules);
    }

    /**
     * Add a module to this {@link IR}.
     *
     * @param module  {@link Module} to add.
     */
    public void addModule(Module module) {
        this.modules.add(module);
        module.setIr(Optional.of(this));
    }

    /**
     * Add a list of modules to this {@link IR}.
     *
     * @param modules  Modules to add.
     */
    public void addModules(List<Module> modules) {
        for (Module module : modules) {
            this.addModule(module);
        }
    }

    /**
     * Remove a module from this {@link IR}.
     *
     * @param module  {@link Module} to remove.
     */
    public void removeModule(Module module) {
        this.modules.remove(module);
        module.setIr(Optional.empty());
    }

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
        protoIr.setVersion(Version.gtirbProtobufVersion);
        // Add modules
        for (Module module : this.modules) {
            ModuleOuterClass.Module.Builder protoModule = module.toProtobuf();
            protoIr.addModules(protoModule);
        }
        // Add CFG
        if (this.cfg != null) {
            protoIr.setCfg(this.cfg.toProtobuf());
        }
        return protoIr;
    }

    /**
     * Save IR to a protobuf file stream.
     */
    public void saveFile(OutputStream fileOut) throws IOException {
        fileOut.write(GTIRB_MAGIC_CHARS);
        fileOut.write(0);
        fileOut.write(0);
        fileOut.write(Version.gtirbProtobufVersion);

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
