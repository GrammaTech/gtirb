#pragma once

#include <google/protobuf/map.h>
#include <google/protobuf/repeated_field.h>
#include <gtirb/Node.hpp>
#include <type_traits>

// Utilities for serialization

namespace gtirb {
///
/// Create UUID from string containing raw bytes.
///
UUID uuidFromBytes(const std::string& bytes);

///
/// Copy raw bytes of UUID into a string.
///
void uuidToBytes(UUID uuid, std::string& bytes);

///
/// Copy raw bytes of Node's UUID into a string.
///
void nodeUUIDToBytes(const Node* node, std::string& bytes);

///
/// Set Node's UUID from a string containing raw bytes.
///
void setNodeUUIDFromBytes(Node* node, const std::string& bytes);

// Generic protobuf conversion for IR classes which implement toProtobuf.
template <typename T> typename T::MessageType toProtobuf(const T& val) {
  typename T::MessageType message;
  val.toProtobuf(&message);
  return message;
}

// Serialize EA to uint64_t
uint64_t toProtobuf(const EA val);

// Overloads for various standard types
std::string toProtobuf(const std::string& val);
int64_t toProtobuf(const int64_t& val);
uint64_t toProtobuf(const uint64_t& val);

template <size_t size> std::string toProtobuf(const std::array<uint8_t, size>& value) {
  return std::string(value.begin(), value.end());
}

template <typename T> auto toProtobuf(const std::shared_ptr<T>& val) -> decltype(toProtobuf(*val)) {
  return toProtobuf(*val);
}

template <typename T, typename U>
auto toProtobuf(const std::pair<T, U>& val)
    -> google::protobuf::MapPair<decltype(toProtobuf(val.first)),
                                 decltype(toProtobuf(val.second))> {
  return {toProtobuf(val.first), toProtobuf(val.second)};
}

// Generic interface for setting up a container. Clear and reserve space
// if the container supports it.
template <typename T>
void initContainer(google::protobuf::RepeatedField<T>* container, size_t size) {
  container->Clear();
  container->Reserve(size);
}
template <typename T>
void initContainer(google::protobuf::RepeatedPtrField<T>* container, size_t size) {
  container->Clear();
  container->Reserve(size);
}
template <typename T> void initContainer(std::vector<T>& container, size_t size) {
  container.clear();
  container.reserve(size);
}
template <typename T> void initContainer(T* container, size_t) { container->clear(); }
template <typename T> void initContainer(T& container, size_t) { container.clear(); }

// Generic interface for adding elements to a container.
template <typename T> void addElement(google::protobuf::RepeatedField<T>* container, T&& element) {
  container->Add(std::move(element));
}
template <typename T>
void addElement(google::protobuf::RepeatedPtrField<T>* container, T&& element) {
  *container->Add() = std::move(element);
}
template <typename T, typename U>
void addElement(google::protobuf::Map<T, U>* container,
                typename google::protobuf::Map<T, U>::value_type&& element) {
  container->insert(std::move(element));
}
template <typename T> void addElement(std::vector<T>& container, T&& element) {
  container.push_back(std::move(element));
}
template <typename T, typename U>
void addElement(std::map<T, U>* container, typename std::map<T, U>::value_type&& element) {
  container->insert(std::move(element));
}
template <typename T>
void addElement(std::set<T>& container, typename std::set<T>::value_type&& element) {
  container.insert(std::move(element));
}

// Convert the contents of a container into protobuf messages.
template <typename ContainerT, typename MessageT>
void containerToProtobuf(const ContainerT& values, MessageT* message) {
  initContainer(message, values.size());
  std::for_each(values.begin(), values.end(),
                [message](const auto& n) { addElement(message, toProtobuf(n)); });
}

// Generic conversion from protobuf for IR classes which implement fromProtobuf;
template <typename T, typename U> void fromProtobuf(T& result, const U& message) {
  result.fromProtobuf(message);
}

// Generic template for simple types which require no conversion.
template <typename T> void fromProtobuf(T& result, const T& message) { result = message; }

// Overrides for various other types.
template <typename T, typename U> void fromProtobuf(std::shared_ptr<T>& val, const U& message) {
  val = std::make_shared<T>();
  fromProtobuf(*val, message);
}
template <size_t size>
void fromProtobuf(std::array<uint8_t, size>& result, const std::string& value) {
  std::copy(value.begin(), value.end(), result.data());
}
template <typename T, typename U, typename V, typename W>
void fromProtobuf(std::pair<T, U>& val, const google::protobuf::MapPair<V, W>& message) {
  fromProtobuf(val.first, message.first);
  fromProtobuf(val.second, message.second);
}
void fromProtobuf(EA& result, const uint64_t& message);

// Convert the contents for a container into IR classes
template <typename ContainerT, typename MessageT>
void containerFromProtobuf(ContainerT& values, MessageT& message) {
  initContainer(values, message.size());
  std::for_each(message.begin(), message.end(), [&values](const auto& m) {
    typename ContainerT::value_type val;
    fromProtobuf(val, m);
    addElement(values, std::move(val));
  });
}

// Special case for std::map
template <typename KeyType, typename ValueType, typename MessageT>
void containerFromProtobuf(std::map<KeyType, ValueType>& values, MessageT& message) {
  values.clear();
  std::for_each(message.begin(), message.end(), [&values](const auto& m) {
    // NOTE: if we could use MapT::value_type here, then this could
    // all be rolled into containerFromProtobuf. But that gives us a
    // pair where the first element is const, so we can't pass it to
    // fromProtobuf().
    std::pair<KeyType, ValueType> val;
    fromProtobuf(val, m);
    values.insert(std::move(val));
  });
}
}
