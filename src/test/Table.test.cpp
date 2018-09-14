//===- Table.test.cpp -------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#include <gtirb/Context.hpp>
#include <gtirb/Table.hpp>
#include <proto/Table.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Table, eaMapProtobufRoundTrip) {
  using MapT = std::map<Addr, table::ValueType>;
  Table Original = MapT({{Addr(1), {"a"}}, {Addr(2), {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M[Addr(1)]), "a");
  EXPECT_EQ(std::get<std::string>(M[Addr(2)]), "b");
}

TEST(Unit_Table, intMapProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  Table Original = MapT({{1, {"a"}}, {2, {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M[1]), "a");
  EXPECT_EQ(std::get<std::string>(M[2]), "b");
}

TEST(Unit_Table, stringMapProtobufRoundTrip) {
  using MapT = std::map<std::string, table::ValueType>;
  Table Original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M["1"]), "a");
  EXPECT_EQ(std::get<std::string>(M["2"]), "b");
}

TEST(Unit_Table, uuidMapProtobufRoundTrip) {
  using MapT = std::map<UUID, table::ValueType>;
  UUID Id1 = Node::Create(Ctx)->getUUID();
  UUID Id2 = Node::Create(Ctx)->getUUID();
  Table Original = MapT({{Id1, {"a"}}, {Id2, {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M[Id1]), "a");
  EXPECT_EQ(std::get<std::string>(M[Id2]), "b");
}

TEST(Unit_Table, mapVectorProtobufRoundTrip) {
  table::InnerMapType item({{"key", {1}}});
  Table Original = std::vector<table::InnerMapType>({item});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto v = std::get<std::vector<table::InnerMapType>>(Result);
  EXPECT_EQ(v.size(), 1);
  EXPECT_EQ(std::get<int64_t>(v[0]["key"]), 1);
}

TEST(Unit_Table, eaVectorProtobufRoundTrip) {
  Table Original = std::vector<Addr>({Addr(1), Addr(2), Addr(3)});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<Addr>>(Result),
            std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
}

TEST(Unit_Table, intVectorProtobufRoundTrip) {
  Table Original = std::vector<int64_t>({1, 2, 3});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<int64_t>>(Result),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_Table, stringVectorProtobufRoundTrip) {
  Table Original = std::vector<std::string>({"1", "2", "3"});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<std::string>>(Result),
            std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_Table, uuidVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  Table Original = std::vector<UUID>({Id1, Id2, Id3});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<UUID>>(Result),
            std::vector<UUID>({Id1, Id2, Id3}));
}

TEST(Unit_Table, instructionVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  std::vector<InstructionRef> vec({{Id1, 1}, {Id2, 2}, {Id3, 3}});
  Table Original = vec;

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto vec2 = std::get<std::vector<InstructionRef>>(Result);
  EXPECT_EQ(vec2[0].BlockRef.getUUID(), vec[0].BlockRef.getUUID());
  EXPECT_EQ(vec2[0].Offset, vec[0].Offset);
  EXPECT_EQ(vec2[1].BlockRef.getUUID(), vec[1].BlockRef.getUUID());
  EXPECT_EQ(vec2[1].Offset, vec[1].Offset);
  EXPECT_EQ(vec2[2].BlockRef.getUUID(), vec[2].BlockRef.getUUID());
  EXPECT_EQ(vec2[2].Offset, vec[2].Offset);
}

TEST(Unit_Table, valueProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  table::InnerMapType Inner({{"a", 1}});
      {2, 6},                    // int64
      {3, "7"},                  // string
      {4, Inner},                // InnerMapType
      {5, Id},                   // UUID
      {6, InstructionRef{Id, 1}} // InstructionRef
  });

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 6);
  EXPECT_EQ(std::get<Addr>(M[1]), Addr(5));
  EXPECT_EQ(std::get<int64_t>(M[2]), 6);
  EXPECT_EQ(std::get<std::string>(M[3]), "7");
  EXPECT_EQ(std::get<int64_t>(std::get<table::InnerMapType>(M[4])["a"]), 1);
  EXPECT_EQ(std::get<UUID>(M[5]), Id);
  auto Ref = std::get<InstructionRef>(M[6]);
  EXPECT_EQ(Ref.BlockRef.getUUID(), Id);
  EXPECT_EQ(Ref.Offset, 1);
}

TEST(Unit_Table, innerValueProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID();
  table::InnerMapType inner({
      {"a", Addr(1)},                         // Addr
      {"b", 2},                               // int
      {"c", "3"},                             // string
      {"d", std::vector<Addr>({Addr(4)})},    // Addr vector
      {"e", std::vector<int64_t>({5})},       // int vector
      {"f", std::vector<std::string>({"6"})}, // string vector
      {"g", Id1},                             // UUID,
      {"h", std::vector<UUID>({Id2})},        // UUID vector
      {"i", InstructionRef{Id1, 1}},          // InstructionRef
      {"j", std::vector<InstructionRef>(
                {{Id1, 1}, {Id2, 2}})}, // InstructionRef vector
  });
  Table Original = MapT({{1, inner}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto M = std::get<table::InnerMapType>(std::get<MapT>(Result)[1]);
  EXPECT_EQ(M.size(), 10);
  EXPECT_EQ(std::get<Addr>(M["a"]), Addr(1));
  EXPECT_EQ(std::get<int64_t>(M["b"]), 2);
  EXPECT_EQ(std::get<std::string>(M["c"]), "3");
  EXPECT_EQ(std::get<std::vector<Addr>>(M["d"]), std::vector<Addr>({Addr(4)}));
  EXPECT_EQ(std::get<std::vector<int64_t>>(M["e"]), std::vector<int64_t>({5}));
  EXPECT_EQ(std::get<std::vector<std::string>>(M["f"]),
            std::vector<std::string>({"6"}));
  EXPECT_EQ(std::get<UUID>(M["g"]), Id1);
  EXPECT_EQ(std::get<std::vector<UUID>>(M["h"]), std::vector<UUID>({Id2}));

  auto Ref = std::get<InstructionRef>(M["i"]);
  EXPECT_EQ(Ref.BlockRef.getUUID(), Id1);
  EXPECT_EQ(Ref.Offset, 1);
  auto Refs = std::get<std::vector<InstructionRef>>(M["j"]);
  EXPECT_EQ(Refs[0].BlockRef.getUUID(), Id1);
  EXPECT_EQ(Refs[0].Offset, 1);
  EXPECT_EQ(Refs[1].BlockRef.getUUID(), Id2);
  EXPECT_EQ(Refs[1].Offset, 2);
}

#include <boost/endian/conversion.hpp>
#include <list>
#include <type_traits>
#include <typeinfo>
#include <vector>

template <class T> struct is_sequence : std::false_type {};
template <class T> struct is_sequence<std::vector<T>> : std::true_type {};
template <class T> struct is_sequence<std::list<T>> : std::true_type {};

template <class T> struct is_mapping : std::false_type {};
template <class T, class U>
struct is_mapping<std::map<T, U>> : std::true_type {};
// Explicitly disable multimap since its semantics are different.
template <class T, class U>
struct is_mapping<std::multimap<T, U>> : std::false_type {};

template <class T> struct is_tuple : std::false_type {};
template <class... Args>
struct is_tuple<std::tuple<Args...>> : std::true_type {};

using to_iterator = std::back_insert_iterator<std::string>;
using from_iterator = std::string::const_iterator;

// Serialize and deserialize by copying the object representation directly.
template <class T> struct default_serialization {
  static void toBytes(const T& object, to_iterator it) {
    // Store as little-endian.
    T reversed = boost::endian::conditional_reverse<
        boost::endian::order::little, boost::endian::order::native>(object);
    auto srcBytes = as_bytes(gsl::make_span(&reversed, 1));
    std::transform(srcBytes.begin(), srcBytes.end(), it,
                   [](auto b) { return char(b); });
  }

  static from_iterator fromBytes(T& object, from_iterator it) {
    auto dest = as_writeable_bytes(gsl::make_span(&object, 1));
    std::for_each(dest.begin(), dest.end(), [&it](auto& b) {
      b = std::byte(*it);
      ++it;
    });
    // Data stored as little-endian.
    boost::endian::conditional_reverse_inplace<boost::endian::order::little,
                                               boost::endian::order::native>(
        object);
    return it;
  }
};

template <class T, class Enable = void> struct table_traits {
  static void toBytes(const T& Object, to_iterator it) = delete;
  static from_iterator fromBytes(T& Object, from_iterator it) = delete;
};
template <class... Ts> struct TypeId {};

template <> struct table_traits<std::byte> : default_serialization<std::byte> {
  static std::string type_id() { return "byte"; }
};

template <> struct table_traits<Addr> : default_serialization<Addr> {
  static std::string type_id() { return "Addr"; }
};

template <> struct table_traits<UUID> : default_serialization<UUID> {
  static std::string type_id() { return "UUID"; }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                 std::is_signed<T>::value>>
    : default_serialization<T> {
  static std::string type_id() {
    return "int" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                 std::is_unsigned<T>::value>>
    : default_serialization<T> {
  static std::string type_id() {
    return "uint" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <> struct table_traits<std::string> {
  static std::string type_id() { return "string"; }

  static void toBytes(const std::string& Object, to_iterator It) {
    table_traits<uint64_t>::toBytes(Object.size(), It);
    std::copy(Object.begin(), Object.end(), It);
  }

  static from_iterator fromBytes(std::string& Object, from_iterator It) {
    size_t Count;
    It = table_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& elt) {
      It = table_traits<char>::fromBytes(elt, It);
    });

    return It;
  }
};

template <> struct table_traits<InstructionRef> {
  static std::string type_id() { return "InstructionRef"; }

  static void toBytes(const InstructionRef& Object, to_iterator It) {
    table_traits<UUID>::toBytes(Object.BlockRef.getUUID(), It);
    table_traits<uint64_t>::toBytes(Object.Offset, It);
  }

  static from_iterator fromBytes(InstructionRef& Object, from_iterator It) {
    UUID Id;
    It = table_traits<UUID>::fromBytes(Id, It);
    Object.BlockRef = NodeRef<Block>(Id);

    It = table_traits<uint64_t>::fromBytes(Object.Offset, It);

    return It;
  }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<is_sequence<T>::value>> {
  static std::string type_id() {
    return "sequence<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    table_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      table_traits<typename T::value_type>::toBytes(Elt, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    size_t Count;
    It = table_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& Elt) {
      It = table_traits<typename T::value_type>::fromBytes(Elt, It);
    });

    return It;
  }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<is_mapping<T>::value>> {
  static std::string type_id() {
    return "mapping<" +
           TypeId<typename T::key_type, typename T::mapped_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    table_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      table_traits<typename T::key_type>::toBytes(Elt.first, It);
      table_traits<typename T::mapped_type>::toBytes(Elt.second, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    size_t Count;
    It = table_traits<uint64_t>::fromBytes(Count, It);

    for (size_t i = 0; i < Count; i++) {
      typename T::key_type K;
      It = table_traits<decltype(K)>::fromBytes(K, It);
      typename T::mapped_type V;
      It = table_traits<decltype(V)>::fromBytes(V, It);
      Object.emplace(std::move(K), std::move(V));
    }
    return It;
  }
};

// Prevent instantiation of table_traits for tuples we can't serialize.
template <class... Ts> struct TupleCheck;
template <> struct TupleCheck<> {};
template <class T, class... Ts>
struct TupleCheck<T, Ts...> : TupleCheck<Ts...> {
  // If you see this error, it's because your tuple contains a type which does
  // not use the default serialization method (probably a container or string).
  // This is not currently supported.
  static_assert(
      std::is_base_of<default_serialization<T>, table_traits<T>>::value,
      "Can't serialize a tuple containing this type.");
  static void check() {}
};

template <class... Ts> std::string tupleId(const std::tuple<Ts...>&) {
  TupleCheck<Ts...>::check();
  return TypeId<Ts...>::value();
}

template <class T>
struct table_traits<T, typename std::enable_if_t<is_tuple<T>::value>>
    : default_serialization<T> {
  static std::string type_id() { return "tuple<" + tupleId(T()) + ">"; }
};

template <> struct TypeId<> {
  static std::string get() { return ""; }
};
template <class T> struct TypeId<T> {
  static std::string value() { return table_traits<T>::type_id(); }
};

template <class T, class... Ts> struct TypeId<T, Ts...> {
  static std::string value() {
    return table_traits<T>::type_id() + "," + TypeId<Ts...>::value();
  }
};

class PodTableImpl {
public:
  virtual void toBytes(std::string& Bytes) const = 0;
  virtual void fromBytes(const std::string& Bytes) = 0;
  virtual const std::type_info& storedType() const = 0;
  virtual std::string typeName() const = 0;
  virtual void* get() = 0;
};

template <class T> class PodTableTemplate : public PodTableImpl {
public:
  PodTableTemplate() = default;
  PodTableTemplate(const T& Val) : Object(Val){};
  PodTableTemplate(T&& Val) : Object(std::move(Val)){};

  void toBytes(std::string& Bytes) const override {
    table_traits<T>::toBytes(Object, std::back_inserter(Bytes));
  }

  void fromBytes(const std::string& Bytes) override {
    table_traits<T>::fromBytes(Object, Bytes.begin());
  }

  const std::type_info& storedType() const override { return typeid(T); }

  std::string typeName() const override { return TypeId<T>::value(); }

  void* get() { return static_cast<void*>(&Object); }

  T Object;
};

class PodTable {
public:
  PodTable() = default;

  template <typename T>
  PodTable(T&& Value)
      : Impl(std::make_unique<PodTableTemplate<std::remove_reference_t<T>>>(
            std::forward<T>(Value))) {}

  template <typename T> PodTable& operator=(T&& Value) {
    this->Impl = std::make_unique<PodTableTemplate<std::remove_reference_t<T>>>(
        std::forward<T>(Value));
    return *this;
  }

  template <typename T> friend T& get(PodTable& Table);
  template <typename T> friend std::add_pointer_t<T> get_if(PodTable* Table);

  using MessageType = proto::PodTable;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

private:
  std::unique_ptr<PodTableImpl> Impl;
  std::string RawBytes;
  std::string TypeName;
};

template <typename T> T& get(PodTable& Table) {
  if (!Table.RawBytes.empty()) {
    // Reconstruct from deserialized data
    Table.Impl = std::make_unique<PodTableTemplate<T>>();
    Expects(Table.Impl->typeName() == Table.TypeName);
    Table.Impl->fromBytes(Table.RawBytes);
    Table.RawBytes.clear();
    Table.TypeName.clear();
  } else {
    Expects(Table.Impl != nullptr);
    Expects(typeid(T) == Table.Impl->storedType());
  }

  return *static_cast<T*>(Table.Impl->get());
}

template <typename T> std::add_pointer_t<T> get_if(PodTable* Table) {
  if (Table == nullptr) {
    return nullptr;
  }

  if (!Table->RawBytes.empty()) {
    if (Table->Impl->typeName() != Table->TypeName) {
      return nullptr;
    }
  } else {
    if (Table->Impl == nullptr || typeid(T) != Table->Impl->storedType()) {
      return nullptr;
    }
  }

  return &get<T>(*Table);
}

void PodTable::toProtobuf(MessageType* message) const {
  if (this->Impl != nullptr) {
    message->set_type_name(this->Impl->typeName());
    message->mutable_data()->clear();
    this->Impl->toBytes(*message->mutable_data());
  }
}

void PodTable::fromProtobuf(const MessageType& message) {
  this->Impl = nullptr;
  this->TypeName = message.type_name();
  this->RawBytes = message.data();
}

TEST(Unit_PodTable, typeName) {
  EXPECT_EQ(PodTableTemplate<uint64_t>().typeName(), "uint64_t");
  EXPECT_EQ(PodTableTemplate<std::vector<uint64_t>>().typeName(),
            "sequence<uint64_t>");
  std::string X = PodTableTemplate<std::map<int64_t, uint64_t>>().typeName();
  EXPECT_EQ(X, "mapping<int64_t,uint64_t>");

  X = PodTableTemplate<std::map<int64_t, std::vector<uint64_t>>>().typeName();
  EXPECT_EQ(X, "mapping<int64_t,sequence<uint64_t>>");

  X = PodTableTemplate<std::vector<std::map<int64_t, uint64_t>>>().typeName();
  EXPECT_EQ(X, "sequence<mapping<int64_t,uint64_t>>");

  X = PodTableTemplate<std::vector<std::tuple<int64_t, uint64_t>>>().typeName();
  EXPECT_EQ(X, "sequence<tuple<int64_t,uint64_t>>");
}

TEST(Unit_PodTable, getPrimitiveTypes) {
  PodTable P;
  P = char('a');
  EXPECT_EQ(get<char>(P), 'a');

  P = uint64_t(123);
  EXPECT_EQ(get<uint64_t>(P), 123);

  P = int64_t(-123);
  EXPECT_EQ(get<int64_t>(P), -123);

  P = uint8_t(123);
  EXPECT_EQ(get<uint8_t>(P), 123);

  P = int(-123);
  EXPECT_EQ(get<int>(P), -123);

  P = unsigned(123);
  EXPECT_EQ(get<unsigned>(P), 123);

  P = std::byte(123);
  EXPECT_EQ(get<std::byte>(P), std::byte(123));
}

TEST(Unit_PodTable, getVector) {
  std::vector<int64_t> Orig({1, 2, 3});
  PodTable P(Orig);

  auto& result = get<std::vector<int64_t>>(P);
  EXPECT_EQ(result, Orig);
}

TEST(Unit_PodTable, getString) {
  std::string Orig("abcd");
  PodTable P(Orig);

  EXPECT_EQ(get<std::string>(P), "abcd");
}

TEST(Unit_PodTable, getAddr) {
  Addr Orig(0x1234);
  PodTable P(Orig);

  EXPECT_EQ(get<Addr>(P), Addr(0x1234));
}

TEST(Unit_PodTable, getMap) {
  std::map<char, int64_t> Orig({{'a', 1}, {'b', 2}, {'c', 3}});
  PodTable P(Orig);

  auto& Result = get<std::map<char, int64_t>>(P);
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_PodTable, getTuple) {
  std::tuple<char, int64_t> Orig('a', 1);
  PodTable P(Orig);

  auto& Result = get<std::tuple<char, int64_t>>(P);
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_PodTable, getUnsetValue) {
  PodTable P;
  EXPECT_DEATH(get<int64_t>(P), "");
}

TEST(Unit_PodTable, getWrongType) {
  PodTable P;
  P = uint64_t(1);
  EXPECT_DEATH(get<int64_t>(P), "");
}

TEST(Unit_PodTable, getWrongContainer) {
  PodTable P;
  P = std::vector<int>();
  EXPECT_DEATH(get<std::list<int>>(P), "");
}

TEST(Unit_PodTable, protobufRoundTrip) {
  int64_t A = 123;
  PodTable Original;
  Original = A;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(A, get<int64_t>(Result));
}

TEST(Unit_PodTable, vectorProtobufRoundTrip) {
  std::vector<int64_t> V({1, 2, 3});
  PodTable Original;
  Original = V;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(V)>(Result), V);
}

TEST(Unit_PodTable, listProtobufRoundTrip) {
  std::list<int64_t> V({1, 2, 3});
  PodTable Original;
  Original = V;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(V)>(Result), V);
}

TEST(Unit_PodTable, listToVectorProtobufRoundTrip) {
  std::list<int64_t> Lst({1, 2, 3});
  PodTable Original;
  Original = Lst;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  auto vec = std::vector<int64_t>(Lst.begin(), Lst.end());
  EXPECT_EQ(get<decltype(vec)>(Result), vec);
}

TEST(Unit_PodTable, stringProtobufRoundTrip) {
  std::string S("abcd");
  PodTable Original;
  Original = S;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(S)>(Result), S);
}

TEST(Unit_PodTable, addrProtobufRoundTrip) {
  Addr A(0x1234);
  PodTable Original;
  Original = A;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(A)>(Result), A);
}

TEST(Unit_PodTable, mapProtobufRoundTrip) {
  std::map<char, int64_t> M({{'a', 1}, {'b', 2}, {'c', 3}});
  PodTable Original;
  Original = M;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(M)>(Result), M);
}

TEST(Unit_PodTable, tupleProtobufRoundTrip) {
  std::tuple<char, int64_t> T('a', 1);
  PodTable Original;
  Original = T;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(T)>(Result), T);
}

TEST(Unit_PodTable, uuidProtobufRoundTrip) {
  UUID Val = Node::Create(Ctx)->getUUID();
  PodTable Original;
  Original = Val;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(Val)>(Result), Val);
}

TEST(Unit_PodTable, instructionRefProtobufRoundTrip) {
  InstructionRef Val{{Node::Create(Ctx)->getUUID()}, 123};
  PodTable Original;
  Original = Val;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  auto NewVal = get<decltype(Val)>(Result);
  EXPECT_EQ(NewVal.BlockRef.getUUID(), Val.BlockRef.getUUID());
  EXPECT_EQ(NewVal.Offset, Val.Offset);
}

TEST(Unit_PodTable, nestedProtobufRoundTrip) {
  PodTable Original;
  PodTable::MessageType Message;
  PodTable Result;

  // Outer vector
  std::vector<std::map<char, std::tuple<int64_t, uint64_t>>> N1;
  N1.push_back({{'a', {0, 1}}, {'b', {2, 3}}});
  N1.push_back({{'c', {4, 5}}, {'d', {6, 7}}});

  Original = N1;

  Original.toProtobuf(&Message);
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(N1)>(Result), N1);

  // Outer map
  std::map<std::string, std::vector<int64_t>> N2{{"a", {1, 2, 3}}};
  Original = N2;
  Original.toProtobuf(&Message);
  Result.fromProtobuf(Message);

  EXPECT_EQ(get<decltype(N2)>(Result), N2);
}

TEST(Unit_PodTable, wrongTypeAfterProtobufRoundTrip) {
  PodTable Original;
  Original = 1234;

  PodTable::MessageType Message;
  Original.toProtobuf(&Message);
  PodTable Result;
  Result.fromProtobuf(Message);

  EXPECT_DEATH(get<std::string>(Result), "");
}

struct MoveTest {
  MoveTest() = default;
  MoveTest(int X) : Val(X) {}
  MoveTest(const MoveTest& other) : Val(other.Val) { CopyCount++; };
  MoveTest(MoveTest&& other) : Val(std::move(other.Val)) { MoveCount++; }
  static int CopyCount;
  static int MoveCount;
  int Val{0};
};
int MoveTest::CopyCount;
int MoveTest::MoveCount;

template <> struct table_traits<MoveTest> : default_serialization<MoveTest> {
  static std::string type_id() { return "MoveTest"; }
};

TEST(Unit_PodTable, movesAndCopies) {
  MoveTest::CopyCount = 0;
  MoveTest::MoveCount = 0;

  MoveTest M(123);
  EXPECT_EQ(MoveTest::CopyCount, 0);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  PodTable Table;
  Table = M;
  EXPECT_EQ(get<decltype(M)>(Table).Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 1);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  Table = std::move(M);
  EXPECT_EQ(get<decltype(M)>(Table).Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 1);
  EXPECT_EQ(MoveTest::MoveCount, 1);
}

TEST(Unit_PodTable, getIf) {
  PodTable Table(int(123));
  EXPECT_EQ(*get_if<int>(&Table), 123);
  EXPECT_EQ(get_if<std::string>(&Table), nullptr);

  PodTable Empty;
  EXPECT_EQ(get_if<int>(&Empty), nullptr);
  EXPECT_EQ(get_if<int>(nullptr), nullptr);
}
