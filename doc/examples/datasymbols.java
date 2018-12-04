// An example program which opens an IR and prints information about all
// symbols pointing to data.
//
// To run this example, do the following.
//
// 1. Install the protobuf compiler (protoc) from
//    https://github.com/protocolbuffers/protobuf/releases (if you have
//    not already done so).
//
// 2. Download the protobuf Java runtime from
//    https://mvnrepository.com/artifact/com.google.protobuf/protobuf-java
//    and save it somewhere suitable.
//
// 3. Generate Java message definitions.
//
//    $ mkdir -p java $ for f in src/proto/*.proto; do
//         protoc -Isrc/proto --java_out=java $f
//      done
//
//    This will create a subdirectory java/proto/, containing a
//    number of files with names of the form <bn>OuterClass.java:
//    one for each <bn>.proto in src/proto/.
//
// 4. Compile the Java message definitions, making sure the protobuf
//    Java runtime .jar file is in your CLASSPATH.
//
//    $ mkdir -p java/classfiles
//    $ CLASSPATH=<path/to/protobuf_jar> \
//         javac -d java/classfiles java/proto/*.java
//
// 5. Compile the datasymbols class defined in this file, making sure
//    your CLASSPATH contains both the Java runtime .jar file and the
//    compiled Java message definition classes.
//    (Note that the path separator is OS-dependent.)
//
//    $ CLASSPATH=<path/to/protobuf_jar>:./java/classfiles/ \
//         javac -classpath  doc/examples/datasymbols.java
//
// 6. Execute the following command to run the program on the
//    serialized GTIRB data located at <path-to-ir>.
//
//    $  CLASSPATH=<path/to/protobuf_jar>:./java/classfiles/:doc/examples/ \
//          java datasymbols <path-to-ir>

import com.google.protobuf.ByteString;
import proto.IROuterClass.IR;
import proto.ModuleOuterClass.Module;
import proto.DataObjectOuterClass.DataObject;
import proto.SymbolOuterClass.Symbol;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.util.*;
import java.lang.Long;

class datasymbols {

    public static void main(String[] args) {

        IR ir = IR.getDefaultInstance();
        DataObject data_obj;

        if (args.length < 1){
            System.err.println("No GTIRB file specified.");
            System.exit(-1);
        }

        // Read serialized GTIRB data from the specified file.
        try {
            ir = IR.parseFrom(new FileInputStream(args[0]));
        } catch (FileNotFoundException fe) {
            System.err.println("File not found: " + args[0]);
            System.exit(-1);
        } catch (IOException ie) {
            System.err.println("Problem reading file: " + args[0]);
            System.exit(-1);
        }


        for (Module m : ir.getModulesList()) {

            // Print the name of each Module.
            System.out.println("Module " + m.getName());

            // Make a map uuid->DataObject for the Module.
            Map<Long, DataObject> datamap = m.getDataMap();
            Map<ByteString, DataObject> data_objects = new HashMap<ByteString,DataObject>();
            for (DataObject d : datamap.values()){
            data_objects.put(d.getUuid(), d);
            }

            // Examine all symbols in the module
            for (Symbol sym : m.getSymbolsList()) {
                data_obj = data_objects.get(sym.getReferentUuid());
                if (data_obj!=null){
                    // If the symbol refers to data, print some information about it
                    System.out.println(String.format("%s:\t0x%08X\t %d bytes",
                                                     sym.getName(),
                                                     data_obj.getAddress(),
                                                     data_obj.getSize()));
                }
            }
        }
    }
}
