#pragma once

#include <gtirb/Table.hpp>
#include <unordered_map>

namespace gtirb
{
    ///
    /// \class TableTemplate
    /// \author John E. Farrier
    ///
    /// The Table acts like a look-up table for data for either cross-module or intra-module data.
    /// While some data may be very node-specific, the table can store arbitrary data that spans
    /// many Node objects.
    ///
    template <typename R, typename C, typename T>
    class TableTemplate : public Table
    {
    public:
        ///
        /// Defaulted constructor.
        ///
        TableTemplate() = default;

        ///
        /// Virtual destructor.
        ///
        /// This class can be inherited from.
        ///
        ~TableTemplate() override = default;

        ///
        /// Computes the total number of elements stored in the table.
        ///
        /// Mirrors the STL API.
        ///
        /// \return The total number of elements stored in the table.
        ///
        virtual size_t size() const override
        {
            size_t s{0};

            for(const auto& subContainer : this->table)
            {
                s += subContainer.second.size();
            }

            return s;
        }

        ///
        /// Clears all elements from the table.
        /// Mirrors the STL API.  Invalidates any references, pointers, or iterators referring to
        /// contained elements. May also invalidate past-the-end iterators.
        ///
        virtual void clear() override
        {
            this->table.clear();
        }

        ///
        /// Access or insert specified element.
        ///
        /// Creates the row if it doesn't exist.  Creates the column if it doesn't exist.
        ///
        /// Returns a reference to the value that is mapped to a key equivalent to key, performing
        /// an insertion if such key does not already exist.
        /// 1) Inserts a value_type object constructed in-place from std::piecewise_construct,
        /// std::forward_as_tuple(key), std::tuple<>() if the key does not exist. This function is
        /// equivalent to return this->try_emplace(key).first->second;. (since C++17)
        /// When the default allocator is used, this results in the key being copy constructed from
        /// key and the mapped value being value-initialized.
        /// 2) Inserts a value_type object constructed in-place from std::piecewise_construct,
        /// std::forward_as_tuple(std::move(key)), std::tuple<>() if the key does not exist. This
        /// function is equivalent to return this->try_emplace(std::move(key)).first->second;.
        /// (since C++17)
        /// When the default allocator is used, this results in the key being move constructed from
        /// key and the mapped value being value-initialized.
        /// If an insertion occurs and results in a rehashing of the container, all iterators are
        /// invalidated. Otherwise iterators are not affected. References are not invalidated.
        /// Rehashing occurs only if the new number of elements is greater than
        /// max_load_factor()*bucket_count().
        ///
        /// \return     Reference to the mapped value of the new element if no element with key key
        /// existed. Otherwise a reference to the mapped value of the existing element whose key is
        /// equivalent to key.
        ///
        std::unordered_map<C, T>& operator[](R&& x)
        {
            return this->table[x];
        }

        ///
        /// Mirrors the STL API.
        ///
        /// Throws std::out_of_range if the container does not have an element with the specified
        /// key.
        ///
        /// \return     A reference to the internal map at the given row.
        ///
        std::unordered_map<C, T>& at(R&& x)
        {
            return this->table.at(x);
        }

        ///
        /// Mirrors the STL API.
        ///
        /// Throws std::out_of_range if the container does not have an element with the specified
        /// key.
        ///
        /// \return     A const reference to the internal map at the given row.
        ///
        const std::unordered_map<C, T>& at(R&& x) const
        {
            return this->table.at(x);
        }

        ///
        /// Similar to the STL API.
        ///
        /// Throws std::out_of_range if the container does not have an element with the specified
        /// key.
        ///
        /// \return     A reference to the value at the given row and column.  The row and column
        /// must exist.
        ///
        T& at(R&& x, C&& y)
        {
            return this->table.at(x).at(y);
        }

        ///
        /// Similar to the STL API.
        ///
        /// Returns a reference to the mapped value of the element with key equivalent to key. If no
        /// such element exists, an exception of type std::out_of_range is thrown.
        /// Complexity: Average case: constant, worst case: linear in size.
        ///
        /// \return     A const reference to the internal map at the given row.
        ///
        const T& at(R&& x, C&& y) const
        {
            return this->table.at(x).at(y);
        }

        ///
        /// \return     A reference to the internal map.
        ///
        std::unordered_map<R, std::unordered_map<C, T>>& data()
        {
            return this->table;
        }

        ///
        /// \return     A const reference to the internal map.
        ///
        const std::unordered_map<R, std::unordered_map<C, T>>& data() const
        {
            return this->table;
        }

        ///
        /// Swaps rows and columns and returns a new data structure.
        ///
        /// Rotating the structure in this way may improve cache performance for certain algorithms.
        ///
        /// \return     A newly allocated container that contains the same total number of elements,
        /// but has the rows and columns swapped.
        ///
        std::unordered_map<C, std::unordered_map<R, T>> getRotated() const
        {
            std::unordered_map<C, std::unordered_map<R, T>> rotated;

            for(const auto& row : this->table)
            {
                for(const auto& column : row.second)
                {
                    rotated[column.first][row.first] = column.second;
                }
            }

            return rotated;
        }

        ///
        /// Serialization support.
        ///
        virtual void serialize(boost::archive::polymorphic_iarchive& ar,
                               const unsigned int version = 0) override = 0;

        ///
        /// Serialization support.
        ///
        virtual void serialize(boost::archive::polymorphic_oarchive& ar,
                               const unsigned int version = 0) const override = 0;

    protected:
        /// Protected so that derived types can serialize it.
        std::unordered_map<R, std::unordered_map<C, T>> table;
    };
}
