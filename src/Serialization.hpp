#ifndef GTIRB_SERIALIZATION_H
#define GTIRB_SERIALIZATION_H

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <google/protobuf/map.h>
#include <google/protobuf/repeated_field.h>
#include <type_traits>

// Utilities for serialization

namespace gtirb {
///
/// Create UUID from string containing raw bytes.
///
UUID uuidFromBytes(const std::string& Bytes);

///
/// Copy raw bytes of UUID into a string.
///
void uuidToBytes(UUID Uuid, std::string& Bytes);

///
/// Copy raw bytes of Node's UUID into a string.
///
void nodeUUIDToBytes(const Node* Node, std::string& Bytes);

///
/// Set Node's UUID from a string containing raw bytes.
///
void setNodeUUIDFromBytes(Node* Node, const std::string& Bytes);

// Generic protobuf conversion for IR classes which implement toProtobuf.
template <typename T> typename T::MessageType toProtobuf(const T& Val) {
  typename T::MessageType Message;
  Val.toProtobuf(&Message);
  return Message;
}

// Serialize EA to uint64_t
uint64_t toProtobuf(const EA Val);

// Overloads for various standard types
std::string toProtobuf(const std::string& Val);
int64_t toProtobuf(const int64_t& Val);
uint64_t toProtobuf(const uint64_t& Val);
std::string toProtobuf(const UUID& Val);

template <size_t size>
std::string toProtobuf(const std::array<uint8_t, size>& Value) {
  return std::string(Value.begin(), Value.end());
}

template <typename T>
auto toProtobuf(const std::shared_ptr<T>& Val) -> decltype(toProtobuf(*Val)) {
  return toProtobuf(*Val);
}

template <typename T, typename U>
auto toProtobuf(const std::pair<T, U>& Val)
    -> google::protobuf::MapPair<decltype(toProtobuf(Val.first)),
                                 decltype(toProtobuf(Val.second))> {
  return {toProtobuf(Val.first), toProtobuf(Val.second)};
}

// Generic interface for setting up a container. Clear and reserve space
// if the container supports it.
template <typename T>
void initContainer(google::protobuf::RepeatedField<T>* Container, size_t Size) {
  Container->Clear();
  Container->Reserve(static_cast<int>(Size));
}
template <typename T>
void initContainer(google::protobuf::RepeatedPtrField<T>* Container,
                   size_t Size) {
  Container->Clear();
  Container->Reserve(static_cast<int>(Size));
}
template <typename T>
void initContainer(std::vector<T>& Container, size_t Size) {
  Container.clear();
  Container.reserve(Size);
}
template <typename T> void initContainer(T* Container, size_t) {
  Container->clear();
}
template <typename T> void initContainer(T& Container, size_t) {
  Container.clear();
}

// Generic interface for adding elements to a container.
template <typename T>
void addElement(google::protobuf::RepeatedField<T>* Container, T&& Element) {
  Container->Add(std::move(Element));
}
template <typename T>
void addElement(google::protobuf::RepeatedPtrField<T>* Container, T&& Element) {
  *Container->Add() = std::move(Element);
}
template <typename T, typename U>
void addElement(google::protobuf::Map<T, U>* Container,
                typename google::protobuf::Map<T, U>::value_type&& Element) {
  Container->insert(std::move(Element));
}
template <typename T> void addElement(std::vector<T>& Container, T&& Element) {
  Container.push_back(std::move(Element));
}
template <typename T, typename U>
void addElement(std::map<T, U>* Container,
                typename std::map<T, U>::value_type&& Element) {
  Container->insert(std::move(Element));
}
template <typename T>
void addElement(std::set<T>& Container,
                typename std::set<T>::value_type&& Element) {
  Container.insert(std::move(Element));
}

// Convert the contents of a Container into protobuf messages.
template <typename ContainerT, typename MessageT>
void containerToProtobuf(const ContainerT& Values, MessageT* Message) {
  initContainer(Message, Values.size());
  std::for_each(Values.begin(), Values.end(), [Message](const auto& N) {
    addElement(Message, toProtobuf(N));
  });
}

// Generic conversion from protobuf for IR classes which implement fromProtobuf;
template <typename T, typename U>
void fromProtobuf(T& Result, const U& Message) {
  Result.fromProtobuf(Message);
}

// Generic template for simple types which require no conversion.
template <typename T> void fromProtobuf(T& Result, const T& Message) {
  Result = Message;
}

// Overrides for various other types.
template <typename T, typename U>
void fromProtobuf(std::shared_ptr<T>& Val, const U& Message) {
  Val = std::make_shared<T>();
  fromProtobuf(*Val, Message);
}
template <size_t size>
void fromProtobuf(std::array<uint8_t, size>& Result, const std::string& Value) {
  std::copy(Value.begin(), Value.end(), Result.data());
}
template <typename T, typename U, typename V, typename W>
void fromProtobuf(std::pair<T, U>& Val,
                  const google::protobuf::MapPair<V, W>& Message) {
  fromProtobuf(Val.first, Message.first);
  fromProtobuf(Val.second, Message.second);
}
void fromProtobuf(EA& Result, const uint64_t& Message);
void fromProtobuf(UUID& Result, const std::string& Message);

// Convert the contents for a Container into IR classes
template <typename ContainerT, typename MessageT>
void containerFromProtobuf(ContainerT& Values, MessageT& Message) {
  initContainer(Values, Message.size());
  std::for_each(Message.begin(), Message.end(), [&Values](const auto& M) {
    typename ContainerT::value_type Val;
    fromProtobuf(Val, M);
    addElement(Values, std::move(Val));
  });
}

// Special case for std::map
template <typename KeyType, typename ValueType, typename MessageT>
void containerFromProtobuf(std::map<KeyType, ValueType>& Values,
                           MessageT& Message) {
  Values.clear();
  std::for_each(Message.begin(), Message.end(), [&Values](const auto& M) {
    // NOTE: if we could use MapT::value_type here, then this could
    // all be rolled into containerFromProtobuf. But that gives us a
    // pair where the first Element is const, so we can't pass it to
    // fromProtobuf().
    std::pair<KeyType, ValueType> Val;
    fromProtobuf(Val, M);
    Values.insert(std::move(Val));
  });
}
} // namespace gtirb

#endif // GTIRB_SERIALIZATION_H
