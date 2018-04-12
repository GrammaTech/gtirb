#pragma once

#include <gtirb/Export.hpp>
#include <string>
#include <utility>

namespace gtirb
{
	///
	/// \class Exception
	///
	/// The base class for all GT-IRB exceptions.
	/// Compatible with std::exception.
	///
	class GTIRB_GTIRB_EXPORT_API Exception : public std::exception
	{
	public:
		Exception() = default;
		Exception(std::string file, int line);
		
		virtual ~Exception() = default;

		void setLocation(std::string file, int line);
		std::pair<std::string, int> getLocation() const;

		virtual const char* what() const noexcept override;

	private:
		/// The file name which generated the exception.
		std::string file{};

		/// The line number within the file that generated the exception.
		int line{0};
	};
}
