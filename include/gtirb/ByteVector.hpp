//===- ByteVector.hpp -------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_BYTE_VECTOR_H
#define GTIRB_BYTE_VECTOR_H

#include <gtirb/Export.hpp>
#include <boost/endian/conversion.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <vector>

namespace gtirb {
/// @cond INTERNAL

/// \class ByteVector
///
/// This is an internal class used to expose the byte vector from a
/// \ref ByteInterval to needed child classes like \ref CodeBlock and \ref
/// DataBlock.
class GTIRB_EXPORT_API ByteVector {
public:
  /// \brief The endianess of data: Either big or little-endian.
  using Endian = boost::endian::order;

private:
  /// \class Reference
  ///
  /// \brief A reference to a section of the byte interval, allowing for
  /// seamless reading and writing of chunks of data.
  template <typename VectorType, typename ResultType> class Reference {
  public:
    Reference(VectorType* V_, size_t I_, Endian InputOrder_,
              Endian OutputOrder_)
        : V(V_), I(I_), InputOrder(InputOrder_), OutputOrder(OutputOrder_) {}

    /// \brief Use this reference as an rvalue, automatically handling
    /// endian conversion.
    operator ResultType() const {
      return boost::endian::conditional_reverse(*(ResultType*)(V->data() + I),
                                                InputOrder, OutputOrder);
    }

    /// \brief Use this reference as an lvalue, automatically handling
    /// endian conversion.
    Reference<VectorType, ResultType>& operator=(ResultType rhs) {
      *(ResultType*)(V->data() + I) =
          boost::endian::conditional_reverse(rhs, OutputOrder, InputOrder);
      return *this;
    }

  private:
    VectorType* V;
    size_t I;
    Endian InputOrder;
    Endian OutputOrder;
  };

  /// \brief An iterator over the bytes in this byte vector.
  ///
  /// \tparam VectorType  Either "Vector" or "const Vector".
  /// \tparam ResultType  The type of the data to iterate over. Must be a POD
  /// type that satisfies Boost's EndianReversible concept.
  template <typename VectorType, typename ResultType>
  class BaseIterator
      : public boost::iterator_facade<BaseIterator<VectorType, ResultType>,
                                      ResultType,
                                      boost::random_access_traversal_tag,
                                      Reference<VectorType, ResultType>> {
  public:
    using self = BaseIterator<VectorType, ResultType>;
    using reference = Reference<VectorType, ResultType>;

    BaseIterator(VectorType* V_, size_t I_, Endian InputOrder_,
                 Endian OutputOrder_)
        : V(V_), I(I_), InputOrder(InputOrder_), OutputOrder(OutputOrder_) {}

    // Beginning of functions for iterator facade compatibility.
    reference dereference() const {
      return reference(V, I, InputOrder, OutputOrder);
    }

    bool equal(const self& other) const { return V == other.V && I == other.I; }

    void increment() { I += sizeof(ResultType); }

    void decrement() { I -= sizeof(ResultType); }

    void advance(typename self::difference_type n) {
      I += n * sizeof(ResultType);
    }

    typename self::difference_type distance_to(const self& other) const {
      return (other.I - I) / sizeof(ResultType);
    }
    // End of functions for iterator facade compatibility.

    /// \brief Convert this iterator into a const iterator.
    operator BaseIterator<const VectorType, ResultType>() const {
      return BaseIterator<const VectorType, ResultType>(V, I, InputOrder,
                                                        OutputOrder);
    }

    /// \brief Return the raw data underlying this byte vector at this
    /// iterator's position.
    ///
    /// Much like \ref std::vector::data, this function is low-level and
    /// potentially unsafe. This pointer refers to valid memory only where an
    /// iterator would be valid to point to. Modifying the size of the byte
    /// vector may invalidate this pointer. Any endian conversions will not be
    /// performed.
    ResultType* data() { return reinterpret_cast<ResultType*>(V->data()); }

    /// \brief Return the raw data underlying this byte vector at this
    /// iterator's position.
    ///
    /// Much like \ref std::vector::data, this function is low-level and
    /// potentially unsafe. This pointer refers to valid memory only where an
    /// iterator would be valid to point to. Modifying the size of the byte
    /// vector may invalidate this pointer. Any endian conversions will not be
    /// performed.
    const ResultType* data() const {
      return reinterpret_cast<const ResultType*>(V->data());
    }

  private:
    VectorType* V;
    size_t I;
    Endian InputOrder;
    Endian OutputOrder;

    /// \brief Convert this iterator into an underlying \ref Vector::iterator.
    typename VectorType::iterator getIterator() { return V->begin() + I; }

    /// \brief Convert this iterator into an underlying \ref Vector::iterator.
    typename VectorType::const_iterator getIterator() const {
      return V->begin() + I;
    }

    friend class ByteVector;
  };

public:
  /// \brief Construct an empty byte vector.
  ByteVector() = default;

  /// \brief Construct a byte vector, filled with zeroes to the given size.
  ByteVector(uint64_t Size) : Bytes(Size) {}

  /// \brief Construct a byte vector from an iterator range of bytes.
  template <typename InputIterator>
  ByteVector(InputIterator Begin, InputIterator End) : Bytes(Begin, End) {}

  /// \brief Construct a byte vector from an iterator range of bytes,
  /// potentially filling remaining space with zeroes.
  template <typename InputIterator>
  ByteVector(InputIterator Begin, InputIterator End, uint64_t Size)
      : Bytes(Begin, End) {
    Bytes.resize(Size);
  }

  /// \brief Iterator over bytes.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename ResultType>
  using iterator = BaseIterator<std::vector<uint8_t>, ResultType>;
  /// \brief Range over bytes.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename ResultType>
  using range = boost::iterator_range<iterator<ResultType>>;
  /// \brief Const iterator over bytes.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename ResultType>
  using const_iterator = BaseIterator<const std::vector<uint8_t>, ResultType>;
  /// \brief Const range over bytes.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename ResultType>
  using const_range = boost::iterator_range<const_iterator<ResultType>>;

  /// \brief Get an iterator to the beginning of this byte vector.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename ResultType>
  iterator<ResultType> begin(Endian InputOrder = Endian::native,
                             Endian OutputOrder = Endian::native) {
    return iterator<ResultType>(&Bytes, 0, InputOrder, OutputOrder);
  }

  /// \brief Get an iterator past the end of this byte vector.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename ResultType>
  iterator<ResultType> end(Endian InputOrder = Endian::native,
                           Endian OutputOrder = Endian::native) {
    return iterator<ResultType>(&Bytes, Bytes.size(), InputOrder, OutputOrder);
  }

  /// \brief Get aa range of data in this byte vector.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename ResultType>
  range<ResultType> bytes(Endian InputOrder = Endian::native,
                          Endian OutputOrder = Endian::native) {
    return range<ResultType>(begin<ResultType>(InputOrder, OutputOrder),
                             end<ResultType>(InputOrder, OutputOrder));
  }

  /// \brief Get an iterator to the beginning of this byte vector.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename ResultType>
  const_iterator<ResultType> begin(Endian InputOrder = Endian::native,
                                   Endian OutputOrder = Endian::native) const {
    return const_iterator<ResultType>(&Bytes, 0, InputOrder, OutputOrder);
  }

  /// \brief Get an iterator past the end of this byte vector.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename ResultType>
  const_iterator<ResultType> end(Endian InputOrder = Endian::native,
                                 Endian OutputOrder = Endian::native) const {
    return const_iterator<ResultType>(&Bytes, Bytes.size(), InputOrder,
                                      OutputOrder);
  }

  /// \brief Get a range of data in this byte vector.
  ///
  /// \tparam ResultType  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename ResultType>
  const_range<ResultType> bytes(Endian InputOrder = Endian::native,
                                Endian OutputOrder = Endian::native) const {
    return const_range<ResultType>(begin<ResultType>(InputOrder, OutputOrder),
                                   end<ResultType>(InputOrder, OutputOrder));
  }

  /// \brief Get the number of bytes held in this vector.
  uint64_t getSize() const { return Bytes.size(); }
  /// \brief Set the number of bytes held in this vector, filling with zeroes or
  /// truncating as appropriate.
  void setSize(uint64_t S) { Bytes.resize(S); }

  /// \brief Insert a single datum into this byte vector.
  ///
  /// \tparam ResultType  The type of data you wish to insert into the byte
  /// vector. Must be a POD type that satisfies Boost's EndianReversibleInplace
  /// concept.
  ///
  /// \param  Pos           The position in the byte vector to insert data at.
  /// \param  X             The data to insert.
  /// \param  VectorOrder   The endianess of the data in the byte vector.
  /// \param  ElementOrder  The endianess of the data to be inserted.
  ///
  /// \return An iterator pointing to the element inserted by this call.
  template <typename ResultType>
  const_iterator<ResultType> insert(const const_iterator<ResultType> Pos,
                                    ResultType X,
                                    Endian VectorOrder = Endian::native,
                                    Endian ElementOrder = Endian::native) {
    boost::endian::conditional_reverse_inplace(X, ElementOrder, VectorOrder);
    const auto* ResultAsBytes = (const uint8_t*)(void*)&X;
    Bytes.insert(Pos.getIterator(), ResultAsBytes,
                 ResultAsBytes + sizeof(ResultType));
    return Pos;
  }

  /// \brief Insert data into this byte vector.
  ///
  /// \tparam ResultType  The type of data you wish to insert into the byte
  /// vector. Must be a POD type that satisfies Boost's EndianReversibleInplace
  /// concept.
  ///
  /// \tparam InputIterator      The type of an iterator yielding T.
  ///
  /// \param  Pos           The position in the byte vector to insert data at.
  /// \param  Begin         The start of the data to insert.
  /// \param  End           The end of the data to insert.
  /// \param  VectorOrder   The endianess of the data in the byte vector.
  /// \param  ElementsOrder The endianess of the data to be inserted.
  ///
  /// \return An iterator pointing to the first element inserted by this call.
  template <typename ResultType, typename InputIterator>
  const_iterator<ResultType> insert(const const_iterator<ResultType> Pos,
                                    InputIterator Begin, InputIterator End,
                                    Endian VectorOrder = Endian::native,
                                    Endian ElementsOrder = Endian::native) {
    auto It = Pos;
    for (ResultType X : boost::make_iterator_range(Begin, End)) {
      insert(It, X, VectorOrder, ElementsOrder);
      It++;
    }
    return Pos;
  }

  /// \brief Erase data from this byte vector.
  ///
  /// \tparam ResultType  The type of data you wish to erase.
  ///
  /// \param  Begin The start of the data to erase.
  /// \param  End   The end of the data to erase.
  ///
  /// \return An iterator pointing to the first element after those erased by
  /// this call.
  template <typename ResultType>
  const_iterator<ResultType> erase(const const_iterator<ResultType> Begin,
                                   const const_iterator<ResultType> End) {
    Bytes.erase(Begin.getIterator(), End.getIterator());
    return Begin;
  }

private:
  std::vector<uint8_t> Bytes;
};

class ByteInterval;
/// \brief Return the \ref ByteVector stored in a \ref ByteInterval.
GTIRB_EXPORT_API ByteVector& getByteVector(ByteInterval* BI);
/// \brief Return the \ref ByteVector stored in a \ref ByteInterval.
GTIRB_EXPORT_API const ByteVector& getByteVector(const ByteInterval* BI);
/// @endcond
} // namespace gtirb
#endif // GTIRB_BYTE_VECTOR_H
