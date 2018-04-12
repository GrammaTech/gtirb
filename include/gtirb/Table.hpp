#pragma once

#include <string>
#include <unordered_map>

namespace gtirb
{
    ///
    /// \class Table
    ///
    /// \author John E. Farrier
    ///
    /// The Table acts like a look-up table for data for either cross-module or intra-module data.
    /// While some data may be very node-specific, the table can store arbitrary data that spans many Node objects.
    ///
    template <typename R = int, typename C = int, typename T = std::string>
    class Table
    {
    public:
        ///
        /// Defaulted constructor.
        ///
        Table() = default;

        ///
        /// A constructor overload to set the table name on creation.
        ///
        Table(std::string x) : tableName{x}
        {
        }

        ///
        /// Virtual destructor.
        ///
        /// This class can be inherited from.
        ///
        virtual ~Table() = default;

        ///
        /// Computes the total number of elements stored in the table.
        ///
        /// Mirrors the STL API.
        ///
        /// \return The total number of elements stored in the table.
        ///
        size_t size() const
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
        /// Mirrors the STL API.
        ///
        void clear()
        {
            this->table.clear();
        }

        void setTableName(std::string x)
        {
            this->tableName = std::move(x);
        }

        std::string getTableName() const
        {
            return this->tableName;
        }

        ///
        /// Natural API
        /// Creates the row if it doesn't exist.
        /// Creates the column if it doesn't exist.
        ///
        /// \return     A reference to the internal map at the given row.
        ///
        std::unordered_map<C, T>& operator[](R&& x)
        {
            return this->table[x];
        }

        ///
        /// Natural API
        /// Creates the row if it doesn't exist.
        /// Creates the column if it doesn't exist.
        ///
        /// \return     A const reference to the internal map at the given row.
        ///
        const std::unordered_map<C, T>& operator[](R&& x) const
        {
            return this->table[x];
        }

        ///
        /// Mirrors the STL API.
        ///
        /// Throws std::out_of_range if the container does not have an element with the specified key.
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
        /// Throws std::out_of_range if the container does not have an element with the specified key.
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
        /// Throws std::out_of_range if the container does not have an element with the specified key.
        ///
        /// \return     A reference to the value at the given row and column.  The row and column must exist.
        ///
        T& at(R&& x, C&& y)
        {
            return this->table.at(x).at(y);
        }

        ///
        /// Similar to the STL API.
        ///
        /// Throws std::out_of_range if the container does not have an element with the specified key.
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

    private:
        std::string tableName;
        std::unordered_map<R, std::unordered_map<C, T>> table;
    };
}