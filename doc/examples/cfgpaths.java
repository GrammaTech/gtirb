// An example program which opens an IR and prints every control-flow
// path from some basic block to another basic block.
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
//         javac doc/examples/cfgpaths.java
//
// 6. Execute the following command to run the program on the
//    serialized GTIRB data located at <path-to-ir> printing every
//    control-flow path between the block with address <source-addr>
//    and the block with address <target-addr>.
//
//    $  CLASSPATH=<path/to/protobuf_jar>:./java/classfiles/:doc/examples/ \
//          java cfgpaths <path-to-ir>  <source-addr> <target-addr>

import com.google.protobuf.ByteString;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.Long;
import java.util.*;
import proto.BlockOuterClass.Block;
import proto.CFGOuterClass.CFG;
import proto.CFGOuterClass.Edge;
import proto.IROuterClass.IR;
import proto.ModuleOuterClass.Module;

class cfgpaths {

    // Avoid duplicated effort by caching the uuids of Blocks from
    // which we have the target is unreachable.
    static Set<ByteString> unreachability_cache = new HashSet<ByteString>();

    // Print one path.
    static void printPath(Stack<Long> path) {
        Set<Long> pathset = new HashSet<Long>();
        pathset.addAll(path);
        //	assert path.size() == pathset.size();
        for (Long addr : path) {
            System.out.print(String.format("0x%08X ", addr));
        }
        System.out.println();
    }

    // Use depth-first search to print all paths from the Block with
    // uuid src to targ to the Block with uuid targ.
    static int printPathsRec(ByteString src, ByteString targ,
                             Map<ByteString, Block> blocks,
                             Map<ByteString, Set<ByteString>> edges,
                             Set<ByteString> visited, Stack<Long> path) {

        int printed = 0;
        path.push(blocks.get(src).getAddress());
        visited.add(src);
        if (src.equals(targ)) {
            printPath(path);
            printed = 1;
        } else {
            for (ByteString next : edges.get(src)) {
                if (!visited.contains(next) &&
                    !unreachability_cache.contains(next)) {
                    printed +=
                        printPathsRec(next, targ, blocks, edges, visited, path);
                }
            }
        }
        visited.remove(src);
        path.pop();
        if (printed == 0) {
            unreachability_cache.add(src);
        }
        return printed;
    }

    // Print all paths from source to target in cfg.
    static void printPaths(Block source, Block target, CFG cfg) {
        Map<ByteString, Block> blocks = new HashMap<ByteString, Block>();
        Map<ByteString, Set<ByteString>> edges =
            new HashMap<ByteString, Set<ByteString>>();
        Set<ByteString> visited = new HashSet<ByteString>();

        for (Block b : cfg.getBlocksList()) {
            blocks.put(b.getUuid(), b);
            edges.put(b.getUuid(), new HashSet<ByteString>());
        }

        for (Edge e : cfg.getEdgesList()) {
            edges.get(e.getSourceUuid()).add(e.getTargetUuid());
        }

        int numpaths =
            printPathsRec(source.getUuid(), target.getUuid(), blocks, edges,
                          new HashSet<ByteString>(), new Stack<Long>());
        System.out.println(numpaths + " paths found.");
    }

    static Block findBlockByAddr(Long addr, CFG cfg) {
        for (Block b : cfg.getBlocksList()) {
            if (b.getAddress() == addr) {
                return b;
            }
        }
        return Block.getDefaultInstance();
    }

    public static void main(String[] args) {

        IR ir = IR.getDefaultInstance();
        Block defaultblock = Block.getDefaultInstance();
        Block from_block = defaultblock;
        Block to_block = defaultblock;
        CFG cfg = CFG.getDefaultInstance();
        boolean blocks_found = false;
        Long from_addr = -1L;
        Long to_addr = -1L;

        if (args.length < 3) {
            System.err.println(
                "Requires three arguments: <gtirb-file> <from-address> <to-address> ");
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

        try {
            from_addr = Long.decode(args[1]);
            to_addr = Long.decode(args[2]);
        } catch (NumberFormatException nfe) {
            System.err.println(
                "Specify block addresses as numbers in decimal, hexadecimal, or octal format.");
            System.exit(-1);
        }

        // Check one module at a time (the GTIRB CFGs do not have
        // edges across module boundaries, so if the specified blocks
        // are in different modules there cannot be any paths between
        // them).
        for (Module m : ir.getModulesList()) {
            cfg = m.getCfg();
            from_block = findBlockByAddr(from_addr, cfg);
            to_block = findBlockByAddr(to_addr, cfg);

            // Neither block is in this module: go on to next module.
            if (from_block == defaultblock && to_block == defaultblock) {
                System.out.println("Blocks not found in module " + m.getName());
                continue;
            }

            // One block is in this module and the other isn't: error.
            if (to_block == defaultblock) {
                System.err.println(String.format(
                    "The block at from-address 0x%08X is located in a module that does not contain a block at to-address 0x%08X.  The block at 0x%08X may be in a different module, or may not exist.",
                    from_addr, to_addr, to_addr));
                System.exit(-1);
            }
            if (from_block == defaultblock) {
                System.err.println(String.format(
                    "The block at  to-address 0x%08X is located in a module that does not contain a block at from-address 0x%08X. The block at 0x%08X may be in a different module, or may not exist.",
                    to_addr, from_addr, from_addr));
                System.exit(-1);
            }

            // At this point, both to_block and from_block must be
            // present in this module.
            blocks_found = true;
            System.out.println("Blocks found in module " + m.getName());
            printPaths(from_block, to_block, cfg);
            break;
        }

        // If never found either block, notify user.
        if (!blocks_found) {
            System.err.println(String.format(
                "No blocks found at either of the specified addresses: \n to-address: 0x%08X \n from-address: 0x%08X",
                to_addr, from_addr));
            System.exit(-1);
        }
    }
}
