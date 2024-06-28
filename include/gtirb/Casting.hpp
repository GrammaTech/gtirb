//===- Casting.hpp ----------------------------------------------*- C++ -*-===//
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
// project where it is distributed under the University of Illinois Open Source
// License. See the LICENSE file in the project root for license terms.
//
//===----------------------------------------------------------------------===//
#ifndef GTIRB_CASTING_H
#define GTIRB_CASTING_H

#include <cassert>
#include <type_traits>

namespace gtirb {
/// \file Casting.hpp
/// \ingroup casting
/// \brief The various casting and type checking operations that apply
/// to gtirb::Node subclasses.
///
/// \see \ref casting.

/// \defgroup casting Casting
///
/// \brief \ref gtirb::Node "Node" and its subclasses support custom casting
/// machinery that allows for type checking, safer static casting, and
/// safe dynamic casting without needing the overhead of a vtable or
/// RTTI.
///
/// File Casting.hpp defines the various operations that apply
/// to \ref gtirb::Node "Node" subclasses.
/// - \ref ISA "isa<Ty>" Performs a type check.
/// - \ref CAST "cast<Ty>" Returns the given argument cast to the
///   specified type;  the argument cannot be null.
/// - \ref CAST_OR_NULL "cast_or_null<Ty>" Returns the given argument
///   cast to the specified type; the argument can be null.
/// - \ref DYN_CAST "dyn_cast<Ty>" Returns the given argument cast to
///   the specified type, or null if the casting operation fails
///   because the types do not match; the argument cannot be null.
/// - \ref DYN_CAST_OR_NULL "dyn_cast_or_null<Ty>" Returns the given
///   argument cast to the specified type, or null if the casting
///   operation fails because the types do not match; the argument can
///   be null.
///
/// \section casting_operations Casting Operations
///
/// \subsection ISA isa<Ty>
///
/// Perform a type check.
///
/// \param Val The object to check. Cannot be null.
///
/// \return \c true if \p Val is an instance of the template parameter
/// type, \c false otherwise.
///
/// - isa<Ty>(const Y& Val)
///
/// Example usage:
/// \code
/// void f(Node *N) { if (isa<Block>(N) { ... } }
/// \endcode
///
///
/// \subsection CAST cast<Ty>
///
/// Cast to the specified type; the argument cannot be null.
///
/// \param Val the value to cast. Cannot be null; consider using
/// \ref CAST_OR_NULL "cast_or_null<Ty>" in that case.
///
/// \return The result of casting \p Val to the specified type.
///   This function asserts that the types match and will not return
///   null on failure.
///
/// - cast<Ty>(Y* Val)
/// - cast<Ty>(Y& Val)
/// - cast<Ty>(const Y& Val)
///
/// Example usage:
/// \code
/// void f(Node * N) { auto *B = cast<Block>(N); }
/// \endcode
///
///
/// \subsection CAST_OR_NULL cast_or_null<Ty>
///
/// Cast to the specified type; the argument can be null.
///
/// \param Val the value to cast. Can be null.
///
/// \return The result of casting \p Val to the specified type.
///   This function asserts that the types match and will not return
///   null on failure. If \p Val is null, returns a null pointer cast
///   to the given type.
///
/// - cast_or_null<Ty>(Y* Val)
/// - cast_or_null<Ty>(Y& Val)
/// - cast_or_null<Ty>(const Y& Val)
///
/// Example usage:
/// \code
///  void f(Node *N) { auto *B = cast_or_null<Block>(N); }
/// \endcode
///
///
/// \subsection  DYN_CAST dyn_cast<Ty>
///
/// Dynamic cast to the specified type; the argument cannot be null.
///
/// \param Val the value to cast. Cannot be null; consider using
/// \ref DYN_CAST_OR_NULL "dyn_cast_or_null<Ty>" in that case.
///
/// \return The result of casting \p Val to the specified type, or
/// null if the casting operation fails because the types do not
/// match.
///
/// - dyn_cast<Ty>(Y* Val)
/// - dyn_cast<Ty>(Y& Val)
/// - dyn_cast<Ty>(const Y& Val)
///
/// Example usage:
/// \code
/// void f(Node * N) { auto *B = dyn_cast<Block>(N); }
/// \endcode
///
///
/// \subsection DYN_CAST_OR_NULL dyn_cast_or_null<Ty>
///
/// Cast to the specified type; the argument can be null.
///
/// \param Val the value to cast. Can be null.
///
/// \return The result of casting \p Val to the specified type, or
/// null if the casting operation fails because the types do not
/// match. If \p Val is null, returns a null pointer cast to the given
/// type.
///
/// - dyn_cast_or_null<Ty>(Y* Val)
/// - dyn_cast_or_null<Ty>(Y& Val)
/// - dyn_cast_or_null<Ty>(const Y& Val)
///
/// Example usage:
/// \code
/// void f(Node *N) { auto *B = dyn_cast_or_null<Block>(N); }
/// \endcode
/// @{ @}

/// \fn isa(const Y& Val)
/// See \ref ISA.

/// \fn cast(Y* Val)
/// See \ref CAST.

/// \fn cast(Y& Val)
/// See \ref CAST.

/// \fn cast(const Y& Val)
/// See \ref CAST.

/// \fn cast_or_null(Y* Val)
/// See \ref CAST_OR_NULL.

/// \fn cast_or_null(Y& Val)
/// See \ref CAST_OR_NULL.

/// \fn cast_or_null(const Y& Val)
/// See \ref CAST_OR_NULL.

/// \fn dyn_cast(Y* Val)
/// See \ref DYN_CAST.

/// \fn dyn_cast(Y& Val)
/// See \ref DYN_CAST.

/// \fn dyn_cast(const Y& Val)
/// See \ref DYN_CAST.

/// \fn dyn_cast_or_null(Y* Val)
/// See \ref DYN_CAST_OR_NULL.

/// \fn dyn_cast_or_null(Y& Val)
/// See \ref DYN_CAST_OR_NULL.

/// \fn dyn_cast_or_null(const Y& Val)
/// See \ref DYN_CAST_OR_NULL.

//===----------------------------------------------------------------------===//
//                          isa<x> Support Templates
//===----------------------------------------------------------------------===//

/// @cond INTERNAL
// Define a template that can be specialized by smart pointers to reflect the
// fact that they are automatically dereferenced, and are not involved with the
// template selection process...  the default implementation is a noop.
//
template <typename From> struct simplify_type {
  using SimpleType = From; // The real type this represents...

  // An accessor to get the real value...
  static SimpleType& getSimplifiedValue(From& Val) { return Val; }
};
/// @endcond

/// @cond INTERNAL
// The core of the implementation of isa<X> is here; To and From should be
// the names of classes.  This template can be specialized to customize the
// implementation of isa<> without rewriting it from scratch.
template <typename To, typename From, typename Enabler = void> struct isa_impl {
  static inline bool doit(const From& Val) { return To::classof(&Val); }
};
/// @endcond

/// @cond INTERNAL
// Always allow upcasts, and perform no dynamic check for them.
template <typename To, typename From>
struct isa_impl<
    To, From, typename std::enable_if<std::is_base_of<To, From>::value>::type> {
  static inline bool doit(const From&) { return true; }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename From> struct isa_impl_cl {
  static inline bool doit(const From& Val) {
    return isa_impl<To, From>::doit(Val);
  }
};
/// @endcond

/// @cond  INTERNAL
template <typename To, typename From> struct isa_impl_cl<To, const From> {
  static inline bool doit(const From& Val) {
    return isa_impl<To, From>::doit(Val);
  }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename From> struct isa_impl_cl<To, From*> {
  static inline bool doit(const From* Val) {
    assert(Val && "isa<> used on a null pointer");
    return isa_impl<To, From>::doit(*Val);
  }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename From> struct isa_impl_cl<To, From* const> {
  static inline bool doit(const From* Val) {
    assert(Val && "isa<> used on a null pointer");
    return isa_impl<To, From>::doit(*Val);
  }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename From> struct isa_impl_cl<To, const From*> {
  static inline bool doit(const From* Val) {
    assert(Val && "isa<> used on a null pointer");
    return isa_impl<To, From>::doit(*Val);
  }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename From>
struct isa_impl_cl<To, const From* const> {
  static inline bool doit(const From* Val) {
    assert(Val && "isa<> used on a null pointer");
    return isa_impl<To, From>::doit(*Val);
  }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename From, typename SimpleFrom>
struct isa_impl_wrap {
  // When From != SimplifiedType, we can simplify the type some more by using
  // the simplify_type template.
  static bool doit(const From& Val) {
    return isa_impl_wrap<To, SimpleFrom,
                         typename simplify_type<SimpleFrom>::SimpleType>::
        doit(simplify_type<const From>::getSimplifiedValue(Val));
  }
};
/// @endcond

/// @cond INTERNAL
template <typename To, typename FromTy>
struct isa_impl_wrap<To, FromTy, FromTy> {
  // When From == SimpleType, we are as simple as we are going to get.
  static bool doit(const FromTy& Val) {
    return isa_impl_cl<To, FromTy>::doit(Val);
  }
};
/// @endcond

// isa<X> - Return true if the parameter to the template is an instance of the
// template type argument.  Used like this:
//
//  if (isa<Type>(myVal)) { ... }
//
template <class X, class Y> [[nodiscard]] inline bool isa(const Y& Val) {
  return isa_impl_wrap<X, const Y,
                       typename simplify_type<const Y>::SimpleType>::doit(Val);
}

//===----------------------------------------------------------------------===//
//                          cast<x> Support Templates
//===----------------------------------------------------------------------===//
/// @cond INTERNAL
template <class To, class From> struct cast_retty;
/// @endcond

/// @cond INTERNAL
// Calculate what type the 'cast' function should return, based on a requested
// type of To and a source type of From.
template <class To, class From> struct cast_retty_impl {
  using ret_type = To&; // Normal case, return Ty&
};
template <class To, class From> struct cast_retty_impl<To, const From> {
  using ret_type = const To&; // Normal case, return Ty&
};

template <class To, class From> struct cast_retty_impl<To, From*> {
  using ret_type = To*; // Pointer arg case, return Ty*
};

template <class To, class From> struct cast_retty_impl<To, const From*> {
  using ret_type = const To*; // Constant pointer arg case, return const Ty*
};

template <class To, class From> struct cast_retty_impl<To, const From* const> {
  using ret_type = const To*; // Constant pointer arg case, return const Ty*
};
/// @endcond

/// @cond INTERNAL
template <class To, class From, class SimpleFrom> struct cast_retty_wrap {
  // When the simplified type and the from type are not the same, use the type
  // simplifier to reduce the type, then reuse cast_retty_impl to get the
  // resultant type.
  using ret_type = typename cast_retty<To, SimpleFrom>::ret_type;
};

template <class To, class FromTy> struct cast_retty_wrap<To, FromTy, FromTy> {
  // When the simplified type is equal to the from type, use it directly.
  using ret_type = typename cast_retty_impl<To, FromTy>::ret_type;
};

template <class To, class From> struct cast_retty {
  using ret_type = typename cast_retty_wrap<
      To, From, typename simplify_type<From>::SimpleType>::ret_type;
};
/// @endcond

/// @cond INTERNAL
// Ensure the non-simple values are converted using the simplify_type template
// that may be specialized by smart pointers...
//
template <class To, class From, class SimpleFrom> struct cast_convert_val {
  // This is not a simple type, use the template to simplify it...
  static typename cast_retty<To, From>::ret_type doit(From& Val) {
    return cast_convert_val<To, SimpleFrom,
                            typename simplify_type<SimpleFrom>::SimpleType>::
        doit(simplify_type<From>::getSimplifiedValue(Val));
  }
};

template <class To, class FromTy> struct cast_convert_val<To, FromTy, FromTy> {
  // This _is_ a simple type, just cast it.
  static typename cast_retty<To, FromTy>::ret_type doit(const FromTy& Val) {
    typename cast_retty<To, FromTy>::ret_type Res2 =
        (typename cast_retty<To, FromTy>::ret_type) const_cast<FromTy&>(Val);
    return Res2;
  }
};
/// @endcond

/// @cond INTERNAL
template <class X> struct is_simple_type {
  static const bool value =
      std::is_same<X, typename simplify_type<X>::SimpleType>::value;
};
/// @endcond

// cast<X> - Return the argument parameter cast to the specified type.  This
// casting operator asserts that the type is correct, so it does not return null
// on failure.  It does not allow a null argument (use cast_or_null for that).
// It is typically used like this:
//
//  cast<Instruction>(myVal)->getParent()
//
template <class X, class Y>
inline typename std::enable_if<!is_simple_type<Y>::value,
                               typename cast_retty<X, const Y>::ret_type>::type
cast(const Y& Val) {
  assert(isa<X>(Val) && "cast<Ty>() argument of incompatible type!");
  return cast_convert_val<
      X, const Y, typename simplify_type<const Y>::SimpleType>::doit(Val);
}

template <class X, class Y>
inline typename cast_retty<X, Y>::ret_type cast(Y& Val) {
  assert(isa<X>(Val) && "cast<Ty>() argument of incompatible type!");
  return cast_convert_val<X, Y, typename simplify_type<Y>::SimpleType>::doit(
      Val);
}

template <class X, class Y>
inline typename cast_retty<X, Y*>::ret_type cast(Y* Val) {
  assert(isa<X>(Val) && "cast<Ty>() argument of incompatible type!");
  return cast_convert_val<X, Y*, typename simplify_type<Y*>::SimpleType>::doit(
      Val);
}

// cast_or_null<X> - Functionally identical to cast, except that a null value is
// accepted.
//
template <class X, class Y>
[[nodiscard]] inline
    typename std::enable_if<!is_simple_type<Y>::value,
                            typename cast_retty<X, const Y>::ret_type>::type
    cast_or_null(const Y& Val) {
  if (!Val)
    return nullptr;
  assert(isa<X>(Val) && "cast_or_null<Ty>() argument of incompatible type!");
  return cast<X>(Val);
}

template <class X, class Y>
[[nodiscard]] inline
    typename std::enable_if<!is_simple_type<Y>::value,
                            typename cast_retty<X, Y>::ret_type>::type
    cast_or_null(Y& Val) {
  if (!Val)
    return nullptr;
  assert(isa<X>(Val) && "cast_or_null<Ty>() argument of incompatible type!");
  return cast<X>(Val);
}

template <class X, class Y>
[[nodiscard]] inline typename cast_retty<X, Y*>::ret_type cast_or_null(Y* Val) {
  if (!Val)
    return nullptr;
  assert(isa<X>(Val) && "cast_or_null<Ty>() argument of incompatible type!");
  return cast<X>(Val);
}

// dyn_cast<X> - Return the argument parameter cast to the specified type.  This
// casting operator returns null if the argument is of the wrong type, so it can
// be used to test for a type as well as cast if successful.  This should be
// used in the context of an if statement like this:
//
//  if (const Instruction *I = dyn_cast<Instruction>(myVal)) { ... }
//

template <class X, class Y>
[[nodiscard]] inline
    typename std::enable_if<!is_simple_type<Y>::value,
                            typename cast_retty<X, const Y>::ret_type>::type
    dyn_cast(const Y& Val) {
  return isa<X>(Val) ? cast<X>(Val) : nullptr;
}

template <class X, class Y>
[[nodiscard]] inline typename cast_retty<X, Y>::ret_type dyn_cast(Y& Val) {
  return isa<X>(Val) ? cast<X>(Val) : nullptr;
}

template <class X, class Y>
[[nodiscard]] inline typename cast_retty<X, Y*>::ret_type dyn_cast(Y* Val) {
  return isa<X>(Val) ? cast<X>(Val) : nullptr;
}

// dyn_cast_or_null<X> - Functionally identical to dyn_cast, except that a null
// value is accepted.
//
template <class X, class Y>
[[nodiscard]] inline
    typename std::enable_if<!is_simple_type<Y>::value,
                            typename cast_retty<X, const Y>::ret_type>::type
    dyn_cast_or_null(const Y& Val) {
  return (Val && isa<X>(Val)) ? cast<X>(Val) : nullptr;
}

template <class X, class Y>
[[nodiscard]] inline
    typename std::enable_if<!is_simple_type<Y>::value,
                            typename cast_retty<X, Y>::ret_type>::type
    dyn_cast_or_null(Y& Val) {
  return (Val && isa<X>(Val)) ? cast<X>(Val) : nullptr;
}

template <class X, class Y>
[[nodiscard]] inline typename cast_retty<X, Y*>::ret_type
dyn_cast_or_null(Y* Val) {
  return (Val && isa<X>(Val)) ? cast<X>(Val) : nullptr;
}
} // namespace gtirb


#ifndef GTIRB_WRAP_UTILS_IN_NAMESPACE

using gtirb::isa;
using gtirb::cast;
using gtirb::cast_or_null;
using gtirb::dyn_cast;
using gtirb::dyn_cast_or_null;

#endif // GTIRB_WRAP_UTILS_IN_NAMESPACE

#endif // GTIRB_CASTING_H
