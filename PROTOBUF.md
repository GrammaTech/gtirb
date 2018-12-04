Using Serialized GTIRB Data
===========================

The serialized [protobuf](https://github.com/google/protobuf/wiki)
data produced by GTIRB allows for exploration and manipulation in the
language of your choice. The [Google protocol
buffers](https://developers.google.com/protocol-buffers/) homepage
lists the languages in which protocol buffers can be used directly;
users of other languages can convert the protobuf-formatted data to
JSON format and then use the JSON data in their applications. In the
future we intend to define a standard JSON schema for GTIRB.

Directory `gtirb/src/proto' contains the protocol buffer message type
definitions for GTIRB. You can inspect these `.proto` files to
determine the structure of the various GTIRB message types. The
top-level message type is `IR`.


- [General Guidelines](#general-guidelines)
- [Python Applications](#python-applications)
- [Java Applications](#java-applications)


# General Guidelines

If you have not used protocol buffers before, there are several useful
resources available at
https://developers.google.com/protocol-buffers/, including an
installation guide and a tutorial.

In general, writing an application to use GTIRB data in protocol
buffer format will involve the following steps.

1. Install the protocol buffer compiler (`protoc`) from
   https://github.com/protocolbuffers/protobuf/releases, if you
   haven't already done so.

2. Install any required protocol buffer library or libraries for the
   programming language you are using.

3. Invoke the protocol buffer compiler on the `.proto` files in
   `gtirb/src/proto/` to generate code in the language you wish to use.

4. Write your application, importing/including the file or files you
   generated in step 3.

The [Protocol Buffers API
Reference](https://developers.google.com/protocol-buffers/docs/reference/overview)
provides language-specific instructions for the various supported
programming languages, along with links to information for cases where
support is provided by third-party plug-ins.


# Python Applications

To create a Python application that uses serialized GTIRB data, do the
following.

1. Install the protocol buffer compiler (`protoc`).

2. Install the Python protobuf library, if you haven't already done so.

       $ pip install protobuf

3. Generate Python message definitions in a dedicated directory (for
   example, `python/`).

       $ mkdir -p python
       $ for f in src/proto/*.proto; do
            protoc -Isrc/proto --python_out=python $f
         done
     
   This will create a number of files with names of the form
   `<bn>_pb2.py` in the `python/` subdirectory of your working
   directory: one for each `<bn>.proto` in src/proto/, including
   `IR_pb2.py`.

4. Write your application. Make sure that it imports `IR_pb2`, or the
   parts of it that you require.

5. Run your application, making sure that the directory containing
   your message definitions is in the `PYTHONPATH`.

## Python Examples

Directory `gtirb/doc/examples` contains several example Python scripts
that use protocol buffers to explore serialized GTIRB data.
- [cfg-paths.py](doc/examples/cfg-paths.py)
- [data-symbols.py](doc/examples/data-symbols.py)


# Java Applications


To create a Java application that uses serialized GTIRB data, do the
following.

1. Install the protocol buffer compiler (`protoc`).

2. Download the `protobuf` Java runtime from
   [https://mvnrepository.com/artifact/com.google.protobuf/protobuf-java](https://mvnrepository.com/artifact/com.google.protobuf/protobuf-java)
   and save it somewhere suitable.

3. Generate Java message definitions in a dedicated directory (for example,
   `java/`).

       $ mkdir -p java
       $ for f in src/proto/*.proto; do
            protoc -Isrc/proto --java_out=java $f
         done
     
   This will create a subdirectory `java/proto/', containing a number
   of files with names of the form `<bn>OuterClass.java`: one for each
   `<bn>.proto` in `src/proto/`.

4. Compile the Java message definitions, making sure the `protobuf`
   Java runtime `.jar` file is in your `CLASSPATH`.

     $ mkdir -p java/classfiles
     $ CLASSPATH=<path/to/protobuf_jar> \
       javac -d java/classfiles java/proto/*.java 

   (If you want to build a `.jar` file to combine all these
   `.class` files, do so at this stage.)

5. Write your application. Make sure that it imports all the classes
   you need from the `proto` package.

6. Compile and run your application, making sure that your CLASSPATH
   contains both the `protobuf` Java runtime `.jar` file and the
   location of the your compiled message definition classes.


## Java Examples

Directory `gtirb/doc/examples` contains several example Java programs
that use protocol buffers to explore serialized GTIRB data.

- [datasymbols.java](doc/examples/datasymbols.java)
