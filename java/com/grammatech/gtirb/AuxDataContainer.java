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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.AuxDataOuterClass;

/**
 * Provides functionality for associating auxiliary
 * data with elements of the intermediate representation.
 */
public class AuxDataContainer extends Node {

    private final Map<String, AuxData> auxDataMap;

    /**
     * Class constructor for an AuxDataContainer from a protobuf AuxData Map.
     * @param  protoUuid        The UUID of this container.
     * @param  protoAuxDataMap  A Map of AuxData names to protobuf AuxData
     * objects.
     */
    AuxDataContainer(ByteString protoUuid,
                     Map<String, AuxDataOuterClass.AuxData> protoAuxDataMap) {
        super(Util.byteStringToUuid(protoUuid));
        this.auxDataMap = new HashMap<>();
        if (protoAuxDataMap != null) {
            Set<String> auxDataNames = protoAuxDataMap.keySet();
            for (String name : auxDataNames) {
                AuxDataOuterClass.AuxData protoAuxData =
                    protoAuxDataMap.get(name);
                AuxData auxDataNew = new AuxData(protoAuxData, name);
                auxDataMap.put(name, auxDataNew);
            }
        }
    }

    public AuxDataContainer() {
        super();
        this.auxDataMap = new HashMap<>();
    }

    private List<String> importListOfString(AuxData auxData) {
        List<String> entries = new ArrayList<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                String entry = serialization.getString();
                entries.add(entry);
            }
        }
        return entries;
    }

    private void exportListOfString(List<String> entries, String name,
                                    String schemata) {
        // Determine size required: 8 bytes for number of entries and
        // 8 bytes added to every string, to store length.
        int bytesRequired = 8;
        for (String entry : entries) {
            bytesRequired += entry.getBytes().length + 8;
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (String entry : entries) {
            serialization.putString(entry);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Set<TwoTuple<String, Long>>
    importSetOfTupleOfStringAndLong(AuxData auxData) {
        Set<TwoTuple<String, Long>> entries = new HashSet<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                String elementOne = serialization.getString();
                Long elementTwo = serialization.getLong();
                TwoTuple<String, Long> tuple =
                    new TwoTuple<>(elementOne, elementTwo);
                entries.add(tuple);
            }
        }
        return entries;
    }

    private void
    exportSetOfTupleOfStringAndLong(Set<TwoTuple<String, Long>> entries,
                                    String name, String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: Size of string bytes plus 8 bytes to store string length, plus
        // 8 bytes for Long.
        int bytesRequired = 8;
        for (TwoTuple<String, Long> tuple : entries) {
            bytesRequired += tuple.getFirst().getBytes().length + 16;
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (TwoTuple<String, Long> tuple : entries) {
            serialization.putString(tuple.getFirst());
            serialization.putLong(tuple.getSecond());
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, UUID> importMapOfUuidToUuid(AuxData auxData) {
        Map<UUID, UUID> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID key = serialization.getUuid();
                UUID value = serialization.getUuid();
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfUuidToUuid(Map<UUID, UUID> entries, String name,
                                       String schemata) {
        // Determine size required: 8 bytes for number of entries and
        // 16 bytes for each of 2 UUIDs.
        int bytesRequired = 8 + entries.size() * 32;
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        Set<UUID> uuids = entries.keySet();
        for (UUID uuid : uuids) {
            UUID value = entries.get(uuid);
            serialization.putUuid(uuid);
            serialization.putUuid(value);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, Long> importMapOfUuidToLong(AuxData auxData) {
        Map<UUID, Long> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID key = serialization.getUuid();
                Long value = serialization.getLong();
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfUuidToLong(Map<UUID, Long> entries, String name,
                                       String schemata) {
        // Determine size required: 8 bytes for number of entries and
        // 16 bytes UUID and 8 bytes for Long.
        int bytesRequired = 8 + entries.size() * 24;
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        Set<UUID> uuids = entries.keySet();
        for (UUID uuid : uuids) {
            Long value = entries.get(uuid);
            serialization.putUuid(uuid);
            serialization.putLong(value);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, String> importMapOfUuidToString(AuxData auxData) {
        Map<UUID, String> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID key = serialization.getUuid();
                String value = serialization.getString();
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfUuidToString(Map<UUID, String> entries, String name,
                                         String schemata) {
        // Determine size required: 8 bytes for number of entries and
        // 16 bytes UUID and string bytes plus 8 bytes to store length of
        // String.
        Set<UUID> uuids = entries.keySet();
        int bytesRequired = 8;
        for (UUID uuid : uuids) {
            String value = entries.get(uuid);
            bytesRequired += value.getBytes().length + 8;
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (UUID uuid : uuids) {
            String value = entries.get(uuid);
            serialization.putUuid(uuid);
            serialization.putString(value);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, TwoTuple<Long, Long>>
    importMapOfUuidToTupleOf2xLong(AuxData auxData) {
        Map<UUID, TwoTuple<Long, Long>> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID uuid = serialization.getUuid();
                long valueOne = serialization.getLong();
                long valueTwo = serialization.getLong();
                TwoTuple<Long, Long> tuple = new TwoTuple<>(valueOne, valueTwo);
                entries.put(uuid, tuple);
            }
        }
        return entries;
    }

    private void
    exportMapOfUuidToTupleOf2xLong(Map<UUID, TwoTuple<Long, Long>> entries,
                                   String name, String schemata) {
        // Determine size required: 8 bytes for number of entries and
        // 16 bytes for UUID and 8 bytes each for two longs.
        int bytesRequired = 8 + entries.size() * 32;
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        Set<UUID> uuids = entries.keySet();
        for (UUID uuid : uuids) {
            TwoTuple<Long, Long> tuple = entries.get(uuid);
            Long valueOne = tuple.getFirst();
            Long valueTwo = tuple.getSecond();
            serialization.putUuid(uuid);
            serialization.putLong(valueOne);
            serialization.putLong(valueTwo);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<Long, TwoTuple<Long, String>>
    importMapOfLongToTupleOfLongAndString(AuxData auxData) {
        Map<Long, TwoTuple<Long, String>> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                Long key = serialization.getLong();
                Long valueOne = serialization.getLong();
                String valueTwo = serialization.getString();
                TwoTuple<Long, String> tuple =
                    new TwoTuple<>(valueOne, valueTwo);
                entries.put(key, tuple);
            }
        }
        return entries;
    }

    private void exportMapOfLongToTupleOfLongAndString(
        Map<Long, TwoTuple<Long, String>> entries, String name,
        String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: 8 bytes for longs plus string size + 8 bytes to store string
        // size.
        Set<Long> keys = entries.keySet();
        int bytesRequired = 8;
        for (Long key : keys) {
            TwoTuple<Long, String> entry = entries.get(key);
            String valueTwo = entry.getSecond();
            bytesRequired += valueTwo.getBytes().length + 16;
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (Long key : keys) {
            TwoTuple<Long, String> entry = entries.get(key);
            Long valueOne = entry.getFirst();
            String valueTwo = entry.getSecond();
            serialization.putLong(key);
            serialization.putLong(valueOne);
            serialization.putString(valueTwo);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, Set<UUID>> importMapOfUuidToSetOfUuid(AuxData auxData) {
        Map<UUID, Set<UUID>> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID key = serialization.getUuid();
                long numElements = serialization.getLong();
                Set<UUID> value = new HashSet<>();
                for (int j = 0; j < numElements; j++) {
                    UUID element = serialization.getUuid();
                    value.add(element);
                }
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfUuidToSetOfUuid(Map<UUID, Set<UUID>> entries,
                                            String name, String schemata) {
        // Determine size required: 8 bytes for number of entries, and for each
        // entry: 16 bytes for UUID key, 8 bytes for number of UUIDs in set, and
        // 16 bytes for each UUID is set.
        Set<UUID> uuids = entries.keySet();
        int bytesRequired = 8;
        for (UUID uuid : uuids) {
            Set<UUID> entry = entries.get(uuid);
            bytesRequired += 24 + 16 * entry.size();
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (UUID key : uuids) {
            serialization.putUuid(key);
            Set<UUID> entry = entries.get(key);
            long numElements = entry.size();
            serialization.putLong(numElements);
            for (UUID element : entry)
                serialization.putUuid(element);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<Offset, Long> importMapOfOffsetToLong(AuxData auxData) {
        Map<Offset, Long> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID offsetUuid = serialization.getUuid();
                long offsetDisplacement = serialization.getLong();
                Offset key = new Offset(offsetUuid, offsetDisplacement);
                Long value = serialization.getLong();
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfOffsetToLong(Map<Offset, Long> entries, String name,
                                         String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: 24 bytes for Offset and 8 bytes for Long.
        Set<Offset> offsets = entries.keySet();
        int bytesRequired = 8 + entries.size() * 32;
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (Offset offset : offsets) {
            UUID offsetUuid = offset.getElementId();
            long offsetDisplacement = offset.getDisplacement();
            Long value = entries.get(offset);
            serialization.putUuid(offsetUuid);
            serialization.putLong(offsetDisplacement);
            serialization.putLong(value);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<Offset, String> importMapOfOffsetToString(AuxData auxData) {
        Map<Offset, String> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID offsetUuid = serialization.getUuid();
                long offsetDisplacement = serialization.getLong();
                Offset key = new Offset(offsetUuid, offsetDisplacement);
                String value = serialization.getString();
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfOffsetToString(Map<Offset, String> entries,
                                           String name, String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: 24 bytes for Offset and string bytes plus 8 bytes to store
        // length of String.
        Set<Offset> offsets = entries.keySet();
        int bytesRequired = 8;
        for (Offset offset : offsets) {
            String value = entries.get(offset);
            bytesRequired += value.getBytes().length + 32;
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (Offset offset : offsets) {
            UUID offsetUuid = offset.getElementId();
            long offsetDisplacement = offset.getDisplacement();
            String value = entries.get(offset);
            serialization.putUuid(offsetUuid);
            serialization.putLong(offsetDisplacement);
            serialization.putString(value);
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, List<TwoTuple<String, Long>>>
    importMapOfUuidToListOfTupleOfStringAndLong(AuxData auxData) {
        Map<UUID, List<TwoTuple<String, Long>>> entries = new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID key = serialization.getUuid();
                long numElements = serialization.getLong();
                List<TwoTuple<String, Long>> value = new ArrayList<>();
                for (int j = 0; j < numElements; j++) {
                    String elementOne = serialization.getString();
                    long elementTwo = serialization.getLong();
                    TwoTuple<String, Long> tuple =
                        new TwoTuple<>(elementOne, elementTwo);
                    value.add(tuple);
                }
                entries.put(key, value);
            }
        }
        return entries;
    }

    private void exportMapOfUuidToListOfTupleOfStringAndLong(
        Map<UUID, List<TwoTuple<String, Long>>> entries, String name,
        String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: 16 bytes for UUID key, 8 bytes for size of tuple list for each
        // tuple: 8 bytes for each long + string bytes plus 8 bytes to store
        // size of string.
        Set<UUID> keys = entries.keySet();
        int bytesRequired = 8; // number of entries
        for (UUID key : keys) {
            bytesRequired += 24; // UUID + size of list
            List<TwoTuple<String, Long>> value = entries.get(key);
            for (TwoTuple<String, Long> tuple : value) {
                String firstElement = tuple.getFirst();
                bytesRequired += firstElement.getBytes().length + 16;
            }
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (UUID key : keys) {
            List<TwoTuple<String, Long>> value = entries.get(key);
            serialization.putUuid(key);
            serialization.putLong(value.size());
            for (TwoTuple<String, Long> tuple : value) {
                String firstElement = tuple.getFirst();
                Long secondElement = tuple.getSecond();
                serialization.putString(firstElement);
                serialization.putLong(secondElement);
            }
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<UUID, FiveTuple<Long, String, String, String, Long>>
    importMapOfUuidToTupleOfLongAnd3xStringAndLong(AuxData auxData) {
        Map<UUID, FiveTuple<Long, String, String, String, Long>> entries =
            new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID key = serialization.getUuid();
                long elementOne = serialization.getLong();
                String elementTwo = serialization.getString();
                String elementThree = serialization.getString();
                String elementFour = serialization.getString();
                long elementFive = serialization.getLong();
                FiveTuple<Long, String, String, String, Long> tuple =
                    new FiveTuple<>(elementOne, elementTwo, elementThree,
                                    elementFour, elementFive);
                entries.put(key, tuple);
            }
        }
        return entries;
    }

    private void exportMapOfUuidToTupleOfLongAnd3xStringAndLong(
        Map<UUID, FiveTuple<Long, String, String, String, Long>> entries,
        String name, String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: 8 bytes each for 2 Longs, and the bytes of 3 strings plus 8
        // bytes each for string length
        Set<UUID> keys = entries.keySet();
        int bytesRequired = 8; // number of entries
        for (UUID key : keys) {
            FiveTuple<Long, String, String, String, Long> tuple =
                entries.get(key);
            String secondElement = tuple.getSecond();
            String thirdElement = tuple.getThird();
            String fourthElement = tuple.getFourth();
            bytesRequired += secondElement.getBytes().length +
                             thirdElement.getBytes().length +
                             fourthElement.getBytes().length + 56;
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (UUID key : keys) {
            FiveTuple<Long, String, String, String, Long> tuple =
                entries.get(key);
            serialization.putUuid(key);
            serialization.putLong(tuple.getFirst());
            serialization.putString(tuple.getSecond());
            serialization.putString(tuple.getThird());
            serialization.putString(tuple.getFourth());
            serialization.putLong(tuple.getFifth());
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    private Map<Offset, List<ThreeTuple<String, List<Long>, UUID>>>
    importMapOfOffsetToListOfTupleOfStringAndListOfLongAndUuid(
        AuxData auxData) {
        Map<Offset, List<ThreeTuple<String, List<Long>, UUID>>> entries =
            new HashMap<>();
        if (auxData != null) {
            Serialization serialization = new Serialization(auxData.getData());
            long numEntries = serialization.getLong();
            for (int i = 0; i < numEntries; i++) {
                UUID offsetUuid = serialization.getUuid();
                long offsetDisplacement = serialization.getLong();
                Offset key = new Offset(offsetUuid, offsetDisplacement);
                List<ThreeTuple<String, List<Long>, UUID>> tupleList =
                    new ArrayList<>();
                long numNodes = serialization.getLong();
                for (int j = 0; j < numNodes; j++) {
                    String elementOne = serialization.getString();
                    List<Long> elementTwo = new ArrayList<>();
                    long numLongs = serialization.getLong();
                    for (int k = 0; k < numLongs; k++) {
                        Long node = serialization.getLong();
                        elementTwo.add(node);
                    }
                    UUID elementThree = serialization.getUuid();
                    ThreeTuple<String, List<Long>, UUID> tuple =
                        new ThreeTuple<>(elementOne, elementTwo, elementThree);
                    tupleList.add(tuple);
                }
                entries.put(key, tupleList);
            }
        }
        return entries;
    }

    private void exportMapOfOffsetToListOfTupleOfStringAndListOfLongAndUuid(
        Map<Offset, List<ThreeTuple<String, List<Long>, UUID>>> entries,
        String name, String schemata) {
        // Determine size required: 8 bytes for number of entries and for each
        // entry: 24 bytes for Offset and 8 bytes for number of tuples and for
        // each tuple: string bytes plus 8 bytes for string length, 8 for size
        // of long list plus 8 for each long, plus 16 for UUID.
        Set<Offset> offsets = entries.keySet();
        int bytesRequired = 8;
        for (Offset offset : offsets) {
            // 32 = size of Offset plus 8 for size of tuple list
            bytesRequired += 32;
            List<ThreeTuple<String, List<Long>, UUID>> tuples =
                entries.get(offset);
            for (ThreeTuple<String, List<Long>, UUID> tuple : tuples) {
                String elementOne = tuple.getFirst();
                List<Long> elementTwo = tuple.getSecond();
                // 32 = 8 for size of string, 8 for size of list of longs, and
                // 16 for UUID
                bytesRequired +=
                    elementOne.getBytes().length + elementTwo.size() * 8 + 32;
            }
        }
        // Serialize the data
        byte[] data = new byte[bytesRequired];
        Serialization serialization = new Serialization(data);
        serialization.putLong(entries.size());
        for (Offset offset : offsets) {
            UUID offsetUuid = offset.getElementId();
            long offsetDisplacement = offset.getDisplacement();
            serialization.putUuid(offsetUuid);
            serialization.putLong(offsetDisplacement);
            List<ThreeTuple<String, List<Long>, UUID>> tuples =
                entries.get(offset);
            serialization.putLong(tuples.size());
            for (ThreeTuple<String, List<Long>, UUID> tuple : tuples) {
                serialization.putString(tuple.getFirst());
                List<Long> elementTwo = tuple.getSecond();
                serialization.putLong(elementTwo.size());
                for (Long node : elementTwo)
                    serialization.putLong(node);
                serialization.putUuid(tuple.getThird());
            }
        }
        // If there is already auxdata of this type, update it, otherwise create
        // new.
        if (this.auxDataMap.containsKey(name)) {
            AuxData auxDataNew = this.auxDataMap.get(name);
            auxDataNew.setData(data);
        } else {
            AuxData auxDataNew = new AuxData(data, name, schemata);
            this.auxDataMap.put(name, auxDataNew);
        }
    }

    /////////////////////////////////////////////////
    // END OF PRIVATES
    /////////////////////////////////////////////////

    /**
     * Import binaryType auxData into a list of strings.
     *
     * @return  The list of binary types from the binaryType AuxData.
     */
    public List<String> getBinaryType() {
        return (importListOfString(this.auxDataMap.get("binaryType")));
    }

    /**
     * Export binaryType a list of strings to AuxData.
     *
     * @param binaryTypeList     The list of binary types to go into binaryType
     * AuxData.
     */
    public void setBinaryType(List<String> binaryTypeList) {
        exportListOfString(binaryTypeList, "binaryType", "sequence<string>");
    }

    /**
     * Import libraryPaths auxData into a list of strings.
     *
     * @return  The list of library paths from the libraryPaths AuxData.
     */
    public List<String> getLibraryPaths() {
        return (importListOfString(this.auxDataMap.get("libraryPaths")));
    }

    /**
     * Export libraryPaths a list of strings to AuxData.
     *
     * @param libraryPathsList     The list of library paths to go into
     * libraryPaths AuxData.
     */
    public void setlibraryPaths(List<String> libraryPathsList) {
        exportListOfString(libraryPathsList, "libraryPaths",
                           "sequence<string>");
    }

    /**
     * Import libraries auxData into a list of strings.
     *
     * @return  The list of libraries types from the libraries AuxData.
     */
    public List<String> getLibraries() {
        return (importListOfString(this.auxDataMap.get("libraries")));
    }

    /**
     * Export libraries a list of strings to AuxData.
     *
     * @param librariesList     The list of libraries types to go into libraries
     * AuxData.
     */
    public void setLibraries(List<String> librariesList) {
        exportListOfString(librariesList, "libraries", "sequence<string>");
    }

    /**
     * Import dynamicEntries auxData into a set of tuples.
     *
     * @return  The set of dynamic entries from the dynamicEntries AuxData.
     */
    public Set<TwoTuple<String, Long>> getDynamicEntries() {
        return (importSetOfTupleOfStringAndLong(
            this.auxDataMap.get("dynamicEntries")));
    }

    /**
     * Export dynamicEntries from a set of tuples to AuxData.
     *
     * @param dynamicEntriesSet     The set of dynamic entries to go into
     * dynamicEntries AuxData.
     */
    public void
    setDynamicEntries(Set<TwoTuple<String, Long>> dynamicEntriesSet) {
        exportSetOfTupleOfStringAndLong(dynamicEntriesSet, "dynamicEntries",
                                        "set<tuple<string,uint64_t>>");
    }

    /**
     * Import comments auxData into {@link Comments}.
     *
     * @return  The {@link Comments} from the comments AuxData.
     */
    public Comments getComments() {
        Map<Offset, String> map =
            importMapOfOffsetToString(this.auxDataMap.get("comments"));
        return new Comments(map);
    }

    /**
     * Export comments from {@link Comments} to AuxData.
     *
     * @param comments   The {@link Comments} to go into comments AuxData.
     */
    public void setComments(Comments comments) {
        exportMapOfOffsetToString(comments.getMap(), "comments",
                                  "mapping<Offset,string>");
    }

    /**
     * Import padding auxData into {@link Padding}.
     *
     * @return  The {@link Padding} from the padding AuxData.
     */
    public Padding getPadding() {
        Map<Offset, Long> map =
            importMapOfOffsetToLong(this.auxDataMap.get("padding"));
        return new Padding(map);
    }

    /**
     * Export padding from {@link Padding} to AuxData.
     *
     * @param padding  The {@link Padding} to go into padding AuxData.
     */
    public void setPadding(Padding padding) {
        exportMapOfOffsetToLong(padding.getMap(), "padding",
                                "mapping<Offset,uint64_t>");
    }

    /**
     * Import symbolicExpressionSizes auxData into a map of longs.
     *
     * @return  The map of symbolic expression sizes from the
     * symbolicExpressionSizes AuxData.
     */
    public Map<Offset, Long> getSymbolicExpressionSizes() {
        return (importMapOfOffsetToLong(
            this.auxDataMap.get("symbolicExpressionSizes")));
    }

    /**
     * Export symbolicExpressionSizes from a map of longs to AuxData.
     *
     * @param symbolicExpressionSizesMap     The map of symbolic expression
     * sizes to go into symbolicExpressionSizes AuxData.
     */
    public void
    setSymbolicExpressionSizes(Map<Offset, Long> symbolicExpressionSizesMap) {
        exportMapOfOffsetToLong(symbolicExpressionSizesMap,
                                "symbolicExpressionSizes",
                                "mapping<Offset,uint64_t>");
    }

    /**
     * Import SCCs auxData into a map of longs.
     *
     * @return  The map of SCCs from the SCCs AuxData.
     */
    public Map<UUID, Long> getSccs() {
        return (importMapOfUuidToLong(this.auxDataMap.get("SCCs")));
    }

    /**
     * Export SCCs from a map of longs to AuxData.
     *
     * @param sccsMap     The map of SCCs to go into SCCs AuxData.
     */
    public void setSccs(Map<UUID, Long> sccsMap) {
        exportMapOfUuidToLong(sccsMap, "SCCs", "mapping<UUID,int64_t>");
    }

    /**
     * Import alignment auxData into {@link Alignment}.
     *
     * @return  The {@link Alignment} from the alignment AuxData.
     */
    public Alignment getAlignment() {
        Map<UUID, Long> map =
            importMapOfUuidToLong(this.auxDataMap.get("alignment"));
        return new Alignment(map);
    }

    /**
     * Export alignment from {@link Alignment} to AuxData.
     *
     * @param alignment  The {@link Alignment} to go into alignment
     * AuxData.
     */
    public void setAlignment(Alignment alignment) {
        exportMapOfUuidToLong(alignment.getMap(), "alignment",
                              "mapping<UUID,uint64_t>");
    }

    /**
     * Import functionBlocks auxData to {@link FunctionBlocks}.
     *
     * @return  {@link FunctionBlocks} from the functionBlocks AuxData.
     */
    public FunctionBlocks getFunctionBlocks() {
        Map<UUID, Set<UUID>> map =
            importMapOfUuidToSetOfUuid(this.auxDataMap.get("functionBlocks"));
        return new FunctionBlocks(map);
    }

    /**
     * Export functionBlocks from {@link FunctionBlocks} to AuxData.
     *
     * @param functionBlocks   {@link FunctionBlocks} to go into AuxData.
     */
    public void setFunctionBlocks(FunctionBlocks functionBlocks) {
        exportMapOfUuidToSetOfUuid(functionBlocks.getMap(), "functionBlocks",
                                   "mapping<UUID,set<UUID>>");
    }

    /**
     * Import functionEntries auxData to {@link FunctionEntries}.
     *
     * @return  {@link FunctionEntries} from the functionBlocks AuxData.
     */
    public FunctionEntries getFunctionEntries() {
        Map<UUID, Set<UUID>> map =
            importMapOfUuidToSetOfUuid(this.auxDataMap.get("functionEntries"));
        return new FunctionEntries(map);
    }

    /**
     * Export functionEntries from {@link FunctionEntries} to AuxData.
     *
     * @param functionEntries   {@link FunctionEntries} to go into AuxData.
     */
    public void setFunctionEntries(FunctionEntries functionEntries) {
        exportMapOfUuidToSetOfUuid(functionEntries.getMap(), "functionEntries",
                                   "mapping<UUID,set<UUID>>");
    }

    /**
     * Import encodings auxData into a map of string.
     *
     * @return  The map of encodings from the encodings AuxData.
     */
    public Map<UUID, String> getEncodings() {
        return (importMapOfUuidToString(this.auxDataMap.get("encodings")));
    }

    /**
     * Export encodings from a map of string to AuxData.
     *
     * @param encodingsMap     The map of encodings to go into encodings
     * AuxData.
     */
    public void setEncodings(Map<UUID, String> encodingsMap) {
        exportMapOfUuidToString(encodingsMap, "encodings",
                                "mapping<UUID,string>");
    }

    /**
     * Import types auxData into {@link Types}.
     *
     * @return  The {@link Types} from the types AuxData.
     */
    public Types getTypes() {
        Map<UUID, String> map =
            importMapOfUuidToString(this.auxDataMap.get("types"));
        return new Types(map);
    }

    /**
     * Export types from {@link Types} to AuxData.
     *
     * @param types  The {@link Types} to go into types AuxData.
     */
    public void setTypes(Types types) {
        exportMapOfUuidToString(types.getMap(), "types",
                                "mapping<UUID,string>");
    }

    /**
     * Import functionNames auxData into {@link FunctionNames}.
     *
     * @return  The {@link FunctionNames} from the functionNames AuxData.
     */
    public FunctionNames getFunctionNames() {
        Map<UUID, UUID> map =
            importMapOfUuidToUuid(this.auxDataMap.get("functionNames"));
        return new FunctionNames(map);
    }

    /**
     * Export functionNames from {@link FunctionNames} to AuxData.
     *
     * @param functionNames  The {@link FunctionNames} to go into
     * functionNames AuxData.
     */
    public void setFunctionNames(FunctionNames functionNames) {
        exportMapOfUuidToUuid(functionNames.getMap(), "functionNames",
                              "mapping<UUID,UUID>");
    }

    /**
     * Import symbolForwarding auxData {@link SymbolForwarding}.
     *
     * @return  The {@link SymbolForwarding} from the symbolForwarding AuxData.
     */
    public SymbolForwarding getSymbolForwarding() {
        Map<UUID, UUID> map =
            importMapOfUuidToUuid(this.auxDataMap.get("symbolForwarding"));
        return new SymbolForwarding(map);
    }

    /**
     * Export symbolForwarding from {@link SymbolForwarding} to AuxData.
     *
     * @param symbolForwarding  The {@link SymbolForwarding} to go into
     * symbolForwarding AuxData.
     */
    public void setSymbolForwarding(SymbolForwarding symbolForwarding) {
        exportMapOfUuidToUuid(symbolForwarding.getMap(), "symbolForwarding",
                              "mapping<UUID,UUID>");
    }

    /**
     * Import elfSectionProperties auxData into a map of tuples.
     *
     * @return  The map of ELF section properties from the elfSectionProperties
     * AuxData.
     */
    public Map<UUID, TwoTuple<Long, Long>> getElfSectionProperties() {
        return (importMapOfUuidToTupleOf2xLong(
            this.auxDataMap.get("elfSectionProperties")));
    }

    /**
     * Export elfSectionProperties from a map of tuples to AuxData.
     *
     * @param elfSectionPropertiesMap     The map of ELF section properties to
     * go into elfSectionProperties AuxData.
     */
    public void setElfSectionProperties(
        Map<UUID, TwoTuple<Long, Long>> elfSectionPropertiesMap) {
        exportMapOfUuidToTupleOf2xLong(
            elfSectionPropertiesMap, "elfSectionProperties",
            "mapping<UUID,tuple<uint64_t,uint64_t>>");
    }

    /**
     * Import elfSymbolTableIdxInfo auxData into a map of tuples.
     *
     * @return  The map of ELF symbol table index info from the
     * elfSymbolTableIdxInfo AuxData.
     */
    public Map<UUID, List<TwoTuple<String, Long>>> getElfSymbolTableIdxInfo() {
        return (importMapOfUuidToListOfTupleOfStringAndLong(
            this.auxDataMap.get("elfSymbolTabIdxInfo")));
    }

    /**
     * Export elfSymbolTableIdxInfo from a map of lists of tuples to AuxData.
     *
     * @param elfSymbolTableIdxInfoMap     The map of ELF symbol table index
     * info to go into elfSymbolTableIdxInfo AuxData.
     */
    public void setElfSymbolTableIdxInfo(
        Map<UUID, List<TwoTuple<String, Long>>> elfSymbolTableIdxInfoMap) {
        exportMapOfUuidToListOfTupleOfStringAndLong(
            elfSymbolTableIdxInfoMap, "elfSymbolTabIdxInfo",
            "mapping<UUID,sequence<tuple<string,uint64_t>>>");
    }

    /**
     * Import elfSymbolInfo auxData into a map of lists of tuples.
     *
     * @return  The map of ELF symbol info from the elfSymbolInfo AuxData.
     */
    public Map<UUID, FiveTuple<Long, String, String, String, Long>>
    getElfSymbolInfo() {
        return (importMapOfUuidToTupleOfLongAnd3xStringAndLong(
            this.auxDataMap.get("elfSymbolInfo")));
    }

    /**
     * Export elfSymbolInfo from a map of tuples to AuxData.
     *
     * @param elfSymbolInfoMap     The map of ELF symbol info to go into
     * elfSymbolInfo AuxData.
     */
    public void
    setElfSymbolInfo(Map<UUID, FiveTuple<Long, String, String, String, Long>>
                         elfSymbolInfoMap) {
        exportMapOfUuidToTupleOfLongAnd3xStringAndLong(
            elfSymbolInfoMap, "elfSymbolInfo",
            "mapping<UUID,tuple<uint64_t,string,string,string,uint64_t>>");
    }

    /**
     * Import cfiDirectives auxData into a map of list of tuples.
     *
     * @return  The map of CFI directives from the cfiDirectives AuxData.
     */
    public Map<Offset, List<ThreeTuple<String, List<Long>, UUID>>>
    getCfiDirectives() {
        return (importMapOfOffsetToListOfTupleOfStringAndListOfLongAndUuid(
            this.auxDataMap.get("cfiDirectives")));
    }

    /**
     * Export cfiDirectives from a map of list of tuples to AuxData.
     *
     * @param cfiDirectivesMap     The map of CFI directives to go into
     * cfiDirectives AuxData.
     */
    public void
    setCfiDirectives(Map<Offset, List<ThreeTuple<String, List<Long>, UUID>>>
                         cfiDirectivesMap) {
        exportMapOfOffsetToListOfTupleOfStringAndListOfLongAndUuid(
            cfiDirectivesMap, "cfiDirectives",
            "mapping<Offset,sequence<tuple<string,sequence<int64_t>,UUID>>>");
    }

    /**
     * Import symbolicOperandInfo auxData into a map of tuples.
     *
     * @return  The map of symbolic operand info from the symbolicOperandInfo
     * AuxData.
     */
    public Map<Long, TwoTuple<Long, String>> getSymbolicOperandInfo() {
        return (importMapOfLongToTupleOfLongAndString(
            this.auxDataMap.get("symbolicOperandInfo")));
    }

    /**
     * Export symbolicOperandInfo from a map of tuples to AuxData.
     *
     * @param symbolicOperandInfoMap     The map of symbolic operand info to go
     * into symbolicOperandInfo AuxData.
     */
    public void setSymbolicOperandInfo(
        Map<Long, TwoTuple<Long, String>> symbolicOperandInfoMap) {
        exportMapOfLongToTupleOfLongAndString(
            symbolicOperandInfoMap, "symbolicOperandInfo",
            "mapping<Addr,tuple<uint64_t,string>>");
    }

    /**
     * Get the {@link AuxData} map.
     *
     * @return The map of AuxData names to AuxData.
     */
    public Map<String, AuxData> getAuxDataMap() { return this.auxDataMap; }

    /**
     * Retrieve an arbitrary {@link AuxData} item from this container if it
     * exists.
     *
     * @param name The schema name
     * @return An {@link AuxData} object, or null if not found.
     */
    public AuxData getAuxData(String name) { return this.auxDataMap.get(name); }

    /**
     * Adds an arbitrary {@link AuxData} item to this container.
     *
     * @param auxData The data to add
     */
    public void putAuxData(AuxData auxData) {
        this.auxDataMap.put(auxData.getName(), auxData);
    }
}
