package com.grammatech.gtirb;

import com.grammatech.gtirb.AuxSerialization.*;
import java.util.*;

public class AuxDataSchemas {

    public final static AuxDataSchema<HashMap<UUID, UUID>> functionNames =
        new AuxDataSchema<>(
            "functionNames",
            new HashMapCodec<>(new UuidCodec(), new UuidCodec()));
}
