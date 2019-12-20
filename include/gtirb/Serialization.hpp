//===- Serialization.hpp ----------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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
#ifndef GTIRB_SERIALIZATION_H
#define GTIRB_SERIALIZATION_H

#include <gtirb/Addr.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Node.hpp>
#include <google/protobuf/map.h>
#include <google/protobuf/repeated_field.h>
#include <type_traits>

// Utilities for serialization

namespace gtirb {
/// \brief Create UUID from string containing raw bytes.
///
/// \param Bytes  A string containing the raw bytes of the UUID.
///
/// \return A UUID constructed from the contents of Bytes.
UUID uuidFromBytes(const std::string& Bytes);

/// \brief Copy raw bytes of UUID into a string.
///
/// \param Uuid   The UUID to store.
/// \param Bytes  The string in which to store the UUID bytes.
///
/// \return void
void uuidToBytes(UUID Uuid, std::string& Bytes);

/// \brief Copy raw bytes of Node's UUID into a string.
///
/// \param Node   Store this Node's UUID.
/// \param Bytes  The string in which to store the UUID bytes.
///
/// \return void
void nodeUUIDToBytes(const Node* Node, std::string& Bytes);

/// \brief Set Node's UUID from a string containing raw bytes.
///
/// \param Node   The Node to modify.
/// \param Bytes  A string containing the raw bytes of the UUID.
///
/// \return void
void setNodeUUIDFromBytes(Node* Node, const std::string& Bytes);

// Generic protobuf conversion for IR classes which implement toProtobuf.
template <typename T> typename T::MessageType toProtobuf(const T& Val) {
  typename T::MessageType Message;
  Val.toProtobuf(&Message);
  return Message;
}

// Serialize Addr to uint64_t
uint64_t toProtobuf(const Addr Val);

// Overloads for various standard types
std::string toProtobuf(const std::string& Val);
int64_t toProtobuf(const int64_t& Val);
uint64_t toProtobuf(const uint64_t& Val);
std::string toProtobuf(const UUID& Val);

template <typename T> T& deref_if_ptr(T& V) { return V; }
template <typename T> const T& deref_if_ptr(const T& V) { return V; }
template <typename T> T& deref_if_ptr(T* V) { return *V; }
template <typename T> const T& deref_if_ptr(const T* V) { return *V; }

template <typename T, typename U>
auto toProtobuf(const std::pair<T, U>& Val) -> google::protobuf::MapPair<
    decltype(toProtobuf(Val.first)),
    decltype(toProtobuf(deref_if_ptr(Val.second)))> {
  return {toProtobuf(Val.first), toProtobuf(deref_if_ptr(Val.second))};
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
template <typename T, typename ContainerType>
std::enable_if_t<std::is_destructible_v<
    decltype(std::declval<ContainerType>().insert(std::declval<T>()))*>>
addElement(ContainerType& Container, T&& Element) {
  Container.insert(std::move(Element));
}

// Convert the contents of a Container into protobuf messages.
template <typename ContainerT, typename MessageT>
void containerToProtobuf(const ContainerT& Values, MessageT* Message) {
  initContainer(Message, Values.size());
  std::for_each(Values.begin(), Values.end(), [Message](const auto& N) {
    addElement(Message, toProtobuf(deref_if_ptr(N)));
  });
}

template <typename IterT, typename MessageT>
void sequenceToProtobuf(IterT First, IterT Last, MessageT* Message) {
  while (First != Last)
    addElement(Message, toProtobuf(deref_if_ptr(*First++)));
}

// Generic conversion from protobuf for IR classes which implement fromProtobuf;
template <typename T, typename U>
T* fromProtobuf(Context& C, const U& Message) {
  return T::fromProtobuf(C, Message);
}

// Generic template for simple types which require no conversion.
template <typename T> void fromProtobuf(Context&, T& Result, const T& Message) {
  Result = Message;
}

inline void fromProtobuf(Context& C, Offset& Result,
                         const Offset::MessageType& Message) {
  Result.fromProtobuf(C, Message);
}

namespace details {
template <typename T> struct remove_pointer_ref_quals {
  using type =
      std::remove_pointer_t<std::remove_reference_t<std::remove_cv_t<T>>>;
};
template <typename T>
using remove_pointer_ref_quals_t = typename remove_pointer_ref_quals<T>::type;
} // namespace details

// Overrides for various other types.
template <typename T, typename U, typename V, typename W>
void fromProtobuf(Context& C, std::pair<T, U>& Val,
                  const google::protobuf::MapPair<V, W>& Message) {
  fromProtobuf(C, Val.first, Message.first);
  fromProtobuf(C, Val.second, Message.second);
}

void fromProtobuf(Context&, Addr& Result, const uint64_t& Message);
void fromProtobuf(Context&, UUID& Result, const std::string& Message);

// Convert the contents for a Container into IR classes; does not participate
// in overload resolution if the container stores Node subclasses.
template <typename ContainerT, typename MessageT>
void containerFromProtobuf(
    Context& C, ContainerT& Values, MessageT& Message,
    std::enable_if_t<
        !std::is_base_of_v<Node, details::remove_pointer_ref_quals_t<
                                     typename ContainerT::value_type>>>* =
        nullptr) {
  initContainer(Values, Message.size());
  std::for_each(Message.begin(), Message.end(), [&Values, &C](const auto& M) {
    typename ContainerT::value_type Val;
    fromProtobuf(C, Val, M);
    addElement(Values, std::move(Val));
  });
}

// Convert the contents for a Container into IR classes. Only participates in
// overload resolution if the container stores Node subclasses.
template <typename ContainerT, typename MessageT>
void containerFromProtobuf(
    Context& C, ContainerT& Values, MessageT& Message,
    std::enable_if_t<
        std::is_base_of_v<Node, details::remove_pointer_ref_quals_t<
                                    typename ContainerT::value_type>>>* =
        nullptr) {
  using BaseType =
      details::remove_pointer_ref_quals_t<typename ContainerT::value_type>;
  initContainer(Values, Message.size());
  std::for_each(Message.begin(), Message.end(), [&Values, &C](const auto& M) {
    addElement(Values, BaseType::fromProtobuf(C, M));
  });
}

// Special case for std::map
template <typename KeyType, typename ValueType, typename MessageT>
void containerFromProtobuf(Context& C, std::map<KeyType, ValueType>& Values,
                           MessageT& Message) {
  Values.clear();
  std::for_each(Message.begin(), Message.end(), [&Values, &C](const auto& M) {
    // NOTE: if we could use MapT::value_type here, then this could
    // all be rolled into containerFromProtobuf. But that gives us a
    // pair where the first Element is const, so we can't pass it to
    // fromProtobuf().
    std::pair<KeyType, ValueType> Val;
    fromProtobuf(C, Val, M);
    Values.insert(std::move(Val));
  });
}
} // namespace gtirb

#endif // GTIRB_SERIALIZATION_H
