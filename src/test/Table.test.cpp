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
  UUID Id = Node::Create(Ctx)->getUUID();
  Table Original = MapT({
      {1, Addr(5)},              // Addr
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

#include <boost/type_index.hpp>
#include <type_traits>

class PodTableImpl {
public:
  virtual size_t objectSize(const void* object) const = 0;

  virtual void toBytes(const void* object, std::string& bytes) const {
    auto* srcBytes = static_cast<const char*>(object);
    std::copy(srcBytes, srcBytes + this->objectSize(object), std::back_inserter(bytes));
  }

  virtual void fromBytes(void* object, const std::string& bytes) {
    Expects(bytes.size() == this->objectSize(object));
    char* destBytes = static_cast<char*>(object);
    std::copy(bytes.begin(), bytes.end(), destBytes);
  }

  virtual boost::typeindex::type_index storedType() const = 0;
};

template <class T> class PodTableTemplate : public PodTableImpl {
  virtual size_t objectSize(const void*) const override { return sizeof(T); }
  virtual boost::typeindex::type_index storedType() const override {
    return boost::typeindex::type_id<T>();
  }
};

template <class T> class PodTableVector : public PodTableTemplate<T> {
  using value_type = typename T::value_type;
  virtual size_t objectSize(const void* object) const override {
    return static_cast<const T*>(object)->size() * sizeof(value_type);
  }

  virtual void toBytes(const void* object, std::string& bytes) const override {
    // XXX: why doesn't static_cast work here?
    auto* srcBytes = reinterpret_cast<const char*>(static_cast<const T*>(object)->data());
    std::copy(srcBytes, srcBytes + this->objectSize(object), std::back_inserter(bytes));
  }

  virtual void fromBytes(void* object, const std::string& bytes) override {
    auto vec = static_cast<T*>(object);
    const size_t elementSize = sizeof(value_type);

    for (auto it = bytes.begin(); it != bytes.end(); it += elementSize) {
      value_type element;
      // XXX: why doesn't static_cast work here?
      std::copy(it, it + elementSize, reinterpret_cast<char*>(&element));
      vec->push_back(std::move(element));
    }
  }
};

template <class T> class PodTableMap : public PodTableTemplate<T> {
  using value_type = typename T::value_type;
  virtual size_t objectSize(const void* object) const override {
    return static_cast<const T*>(object)->size() * sizeof(value_type);
  }

  virtual void toBytes(const void* object, std::string& bytes) const override {
    for (const auto& elt : *static_cast<const T*>(object)) {
      auto* srcBytes = reinterpret_cast<const char*>(&elt);
      std::copy(srcBytes, srcBytes + sizeof(typename T::value_type), std::back_inserter(bytes));
    }
  }

  virtual void fromBytes(void* object, const std::string& bytes) override {
    auto m = static_cast<T*>(object);
    const size_t elementSize = sizeof(value_type);

    for (auto it = bytes.begin(); it != bytes.end(); it += elementSize) {
      value_type element;
      // XXX: why doesn't static_cast work here?
      std::copy(it, it + elementSize, reinterpret_cast<char*>(&element));
      m->insert(std::move(element));
    }
  }
};

template <class T> struct is_vector : std::false_type {};
template <class T> struct is_vector<std::vector<T>> : std::true_type {};

template <class T> struct is_map : std::false_type {};
template <class T, class U> struct is_map<std::map<T, U>> : std::true_type {};

class PodTable {
public:
  // TODO: destructor
  // TODO: constructors, etc

  // TODO: move/reference versions
  template <typename T> void set(T value) {
    // XXX: static assert of is_pod
    this->object = new T(value);
    this->impl = std::make_unique<PodTableTemplate<T>>();
  }

  template <typename T> void set(std::vector<T> value) {
    using V = std::vector<T>;
    this->object = new V(value);
    this->impl = std::make_unique<PodTableVector<V>>();
  }

  template <typename T, typename U> void set(std::map<T, U> value) {
    using V = std::map<T, U>;
    this->object = new V(value);
    this->impl = std::make_unique<PodTableMap<V>>();
  }

  template <typename T>
  typename std::enable_if<!is_vector<T>::value && !is_map<T>::value, T>::type& get() {
    if (!this->rawBytes.empty()) {
      // Reconstruct from deserialized data
      auto ti = boost::typeindex::type_id<T>();
      Expects(ti.pretty_name() == this->typeName);

      this->impl = std::make_unique<PodTableTemplate<T>>();
      this->object = new T;
      this->impl->fromBytes(static_cast<char*>(this->object), this->rawBytes);
      this->rawBytes.clear();
      this->typeName.clear();
    } else {
      Expects(this->object != nullptr);
      Expects(boost::typeindex::type_id<T>() == this->impl->storedType());
    }

    return *static_cast<T*>(this->object);
  }

  template <typename T> typename std::enable_if<is_vector<T>::value, T>::type& get() {
    if (!this->rawBytes.empty()) {
      // Reconstruct from deserialized data
      auto ti = boost::typeindex::type_id<T>();
      Expects(ti.pretty_name() == this->typeName);

      this->impl = std::make_unique<PodTableVector<T>>();
      this->object = new T;
      this->impl->fromBytes(static_cast<char*>(this->object), this->rawBytes);
      this->rawBytes.clear();
      this->typeName.clear();
    } else {
      Expects(this->object != nullptr);
      Expects(boost::typeindex::type_id<T>() == this->impl->storedType());
    }

    return *static_cast<T*>(this->object);
  }

  template <typename T> typename std::enable_if<is_map<T>::value, T>::type& get() {
    if (!this->rawBytes.empty()) {
      // Reconstruct from deserialized data
      auto ti = boost::typeindex::type_id<T>();
      Expects(ti.pretty_name() == this->typeName);

      this->impl = std::make_unique<PodTableMap<T>>();
      this->object = new T;
      this->impl->fromBytes(static_cast<char*>(this->object), this->rawBytes);
      this->rawBytes.clear();
      this->typeName.clear();
    } else {
      Expects(this->object != nullptr);
      Expects(boost::typeindex::type_id<T>() == this->impl->storedType());
    }

    return *static_cast<T*>(this->object);
  }

  using MessageType = proto::PodTable;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  std::unique_ptr<PodTableImpl> impl;
  void* object{nullptr};
  std::string rawBytes;
  std::string typeName;
};

void PodTable::toProtobuf(MessageType* message) const {
  if (this->object != nullptr) {
    message->set_size(this->impl->objectSize(this->object));
    message->set_type_name(this->impl->storedType().pretty_name());
    this->impl->toBytes(this->object, *message->mutable_data());
  }
}

void PodTable::fromProtobuf(const MessageType& message) {
  this->object = nullptr;
  this->typeName = message.type_name();
  this->rawBytes = message.data();
}

TEST(Unit_PodTable, getPod) {
  struct Test {
    int x;
    float y;
  };

  Test a = {1, 2.5};
  PodTable p;
  p.set(a);

  auto& b = p.get<Test>();

  EXPECT_EQ(a.x, b.x);
  EXPECT_EQ(a.y, b.y);
}

TEST(Unit_PodTable, getVector) {
  std::vector<int> orig({1, 2, 3});
  PodTable p;
  p.set(orig);

  auto& result = p.get<std::vector<int>>();
  EXPECT_EQ(result, orig);
}

TEST(Unit_PodTable, getMap) {
  std::map<char, int> orig({{'a', 1}, {'b', 2}, {'c', 3}});
  PodTable p;
  p.set(orig);

  auto& result = p.get<std::map<char, int>>();
  EXPECT_EQ(result, orig);
}

TEST(Unit_PodTable, getUnsetValue) {
  PodTable p;
  EXPECT_DEATH(p.get<int>(), "");
}

TEST(Unit_PodTable, getWrongType) {
  PodTable p;
  p.set(uint64_t(1));
  EXPECT_DEATH(p.get<float>(), "");
}

TEST(Unit_PodTable, protobufRoundTrip) {
  struct Test {
    int x;
    float y;
  };

  Test a = {1, 2.5};
  PodTable original;
  original.set(a);

  PodTable::MessageType message;
  original.toProtobuf(&message);
  PodTable result;
  result.fromProtobuf(message);

  auto& b = result.get<Test>();

  EXPECT_EQ(a.x, b.x);
  EXPECT_EQ(a.y, b.y);
}

TEST(Unit_PodTable, vectorProtobufRoundTrip) {
  std::vector<int> v({1, 2, 3});
  PodTable original;
  original.set(v);

  PodTable::MessageType message;
  original.toProtobuf(&message);
  PodTable result;
  result.fromProtobuf(message);

  EXPECT_EQ(result.get<decltype(v)>(), v);
}

TEST(Unit_PodTable, mapProtobufRoundTrip) {
  std::map<char, int> m({{'a', 1}, {'b', 2}, {'c', 3}});
  PodTable original;
  original.set(m);

  PodTable::MessageType message;
  original.toProtobuf(&message);
  PodTable result;
  result.fromProtobuf(message);

  EXPECT_EQ(result.get<decltype(m)>(), m);
}

// TODO:
// Destructors/memory management
// Byte order
// Strings
