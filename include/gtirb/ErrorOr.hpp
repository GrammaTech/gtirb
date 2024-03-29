//===- ErrorOr.hpp ----------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
//===-Addition License Information-----------------------------------------===//
//
// This file was initially written for the LLVM Compiler Infrastructure
// project where it is distributed under the Apache License v2.0 with LLVM
// Exceptions. See the LICENSE file in the project root for license terms.
//
//===----------------------------------------------------------------------===//

#ifndef GTIRB_ERROROR_H
#define GTIRB_ERROROR_H

#include <gtirb/Export.hpp>
#include <cassert>
#include <iostream>
#include <system_error>
#include <type_traits>
#include <utility>

namespace gtirb {

/// A small struct to hold an error code
/// along with a string holding additional details
struct GTIRB_EXPORT_API ErrorInfo {
  std::error_code ErrorCode;
  std::string Msg;
  ErrorInfo() = default;
  ErrorInfo(const std::error_code& EC, const std::string& S)
      : ErrorCode(EC), Msg(S){};
  std::string message() const;
};

template <typename CharT, typename Traits>
std::ostream& operator<<(std::basic_ostream<CharT, Traits>& os,
                         const ErrorInfo& Info) {
  os << Info.ErrorCode.message() << " " << Info.Msg;
  return os;
}

/// Represents either an error or a value T.
///
/// ErrorOr<T> is a pointer-like class that represents the result of an
/// operation. The result is either an error, or a value of type T. This is
/// designed to emulate the usage of returning a pointer where nullptr indicates
/// failure. However instead of just knowing that the operation failed, we also
/// have an error_code and optional user data that describes why it failed.
///
/// It is used like the following.
/// \code
///   ErrorOr<Buffer> getBuffer();
///
///   auto buffer = getBuffer();
///   if (error_code ec = buffer.getError())
///     return ec;
///   buffer->write("adena");
/// \endcode
///
///
/// Implicit conversion to bool returns true if there is a usable value. The
/// unary * and -> operators provide pointer like access to the value. Accessing
/// the value when there is an error has undefined behavior.
///
/// When T is a reference type the behavior is slightly different. The reference
/// is held in a std::reference_wrapper<std::remove_reference<T>::type>, and
/// there is special handling to make operator -> work as if T was not a
/// reference.
///
/// T cannot be a rvalue reference.
template <class T> class ErrorOr {
  template <class OtherT> friend class ErrorOr;

  static constexpr bool isRef = std::is_reference<T>::value;

  using wrap = std::reference_wrapper<std::remove_reference_t<T>>;

public:
  using storage_type = std::conditional_t<isRef, wrap, T>;

private:
  using reference = std::remove_reference_t<T>&;
  using const_reference = const std::remove_reference_t<T>&;
  using pointer = std::remove_reference_t<T>*;
  using const_pointer = const std::remove_reference_t<T>*;

public:
  template <class E>
  ErrorOr(E ErrorCode, const std::string& Msg = "",
          std::enable_if_t<std::is_error_code_enum<E>::value ||
                               std::is_error_condition_enum<E>::value,
                           void*> = nullptr)
      : HasError(true) {
    new (getErrorStorage()) ErrorInfo{make_error_code(ErrorCode), Msg};
  }

  ErrorOr(std::error_code EC, const std::string& Msg = "") : HasError(true) {
    new (getErrorStorage()) ErrorInfo{EC, Msg};
  }

  ErrorOr(const ErrorInfo& EI) : HasError(true) {
    new (getErrorStorage()) ErrorInfo(EI);
  }

  template <class OtherT>
  ErrorOr(OtherT&& Val,
          std::enable_if_t<std::is_convertible<OtherT, T>::value>* = nullptr)
      : HasError(false) {
    new (getStorage()) storage_type(std::forward<OtherT>(Val));
  }

  ErrorOr(const ErrorOr& Other) { copyConstruct(Other); }

  template <class OtherT>
  ErrorOr(const ErrorOr<OtherT>& Other,
          std::enable_if_t<std::is_convertible<OtherT, T>::value>* = nullptr) {
    copyConstruct(Other);
  }

  template <class OtherT>
  explicit ErrorOr(
      const ErrorOr<OtherT>& Other,
      std::enable_if_t<!std::is_convertible<OtherT, const T&>::value>* =
          nullptr) {
    copyConstruct(Other);
  }

  ErrorOr(ErrorOr&& Other) { moveConstruct(std::move(Other)); }

  template <class OtherT>
  ErrorOr(ErrorOr<OtherT>&& Other,
          std::enable_if_t<std::is_convertible<OtherT, T>::value>* = nullptr) {
    moveConstruct(std::move(Other));
  }

  // This might eventually need SFINAE but it's more complex than is_convertible
  // & I'm too lazy to write it right now.
  template <class OtherT>
  explicit ErrorOr(
      ErrorOr<OtherT>&& Other,
      std::enable_if_t<!std::is_convertible<OtherT, T>::value>* = nullptr) {
    moveConstruct(std::move(Other));
  }

  ErrorOr& operator=(const ErrorOr& Other) {
    copyAssign(Other);
    return *this;
  }

  ErrorOr& operator=(ErrorOr&& Other) {
    moveAssign(std::move(Other));
    return *this;
  }

  ~ErrorOr() {
    if (!HasError)
      getStorage()->~storage_type();
  }

  /// Return false if there is an error.
  explicit operator bool() const { return !HasError; }

  reference get() { return *getStorage(); }
  const_reference get() const { return const_cast<ErrorOr<T>*>(this)->get(); }

  ErrorInfo getError() const {
    return HasError ? *getErrorStorage() : ErrorInfo();
  }

  pointer operator->() { return toPointer(getStorage()); }

  const_pointer operator->() const { return toPointer(getStorage()); }

  reference operator*() { return *getStorage(); }

  const_reference operator*() const { return *getStorage(); }

private:
  template <class OtherT> void copyConstruct(const ErrorOr<OtherT>& Other) {
    if (!Other.HasError) {
      // Get the other value.
      HasError = false;
      new (getStorage()) storage_type(*Other.getStorage());
    } else {
      // Get other's error.
      HasError = true;
      new (getErrorStorage()) ErrorInfo(Other.getError());
    }
  }

  template <class T1>
  static bool compareThisIfSameType(const T1& a, const T1& b) {
    return &a == &b;
  }

  template <class T1, class T2>
  static bool compareThisIfSameType(const T1&, const T2&) {
    return false;
  }

  template <class OtherT> void copyAssign(const ErrorOr<OtherT>& Other) {
    if (compareThisIfSameType(*this, Other))
      return;

    this->~ErrorOr();
    new (this) ErrorOr(Other);
  }

  template <class OtherT> void moveConstruct(ErrorOr<OtherT>&& Other) {
    if (!Other.HasError) {
      // Get the other value.
      HasError = false;
      new (getStorage()) storage_type(std::move(*Other.getStorage()));
    } else {
      // Get other's error.
      HasError = true;
      new (getErrorStorage()) ErrorInfo(std::move(*Other.getErrorStorage()));
    }
  }

  template <class OtherT> void moveAssign(ErrorOr<OtherT>&& Other) {
    if (compareThisIfSameType(*this, Other))
      return;

    this->~ErrorOr();
    new (this) ErrorOr(std::move(Other));
  }

  pointer toPointer(pointer Val) { return Val; }

  const_pointer toPointer(const_pointer Val) const { return Val; }

  pointer toPointer(wrap* Val) { return &Val->get(); }

  const_pointer toPointer(const wrap* Val) const { return &Val->get(); }

  storage_type* getStorage() {
    assert(!HasError && "Cannot get value when an error exists!");
    return reinterpret_cast<storage_type*>(TStorage);
  }

  const storage_type* getStorage() const {
    assert(!HasError && "Cannot get value when an error exists!");
    return reinterpret_cast<const storage_type*>(TStorage);
  }

  ErrorInfo* getErrorStorage() {
    assert(HasError && "Cannot get error when a value exists!");
    return reinterpret_cast<ErrorInfo*>(ErrorStorage);
  }

  const ErrorInfo* getErrorStorage() const {
    return const_cast<ErrorOr<T>*>(this)->getErrorStorage();
  }

  union {
    alignas(storage_type) char TStorage[sizeof(storage_type)];
    alignas(ErrorInfo) char ErrorStorage[sizeof(ErrorInfo)];
  };
  bool HasError : 1;
};

template <class T, class E>
std::enable_if_t<std::is_error_code_enum<E>::value ||
                     std::is_error_condition_enum<E>::value,
                 bool>
operator==(const ErrorOr<T>& Err, E Code) {
  return Err.getError().ErrorCode == Code;
}
} // end namespace gtirb

#endif // GTIRB_ERROROR_H
