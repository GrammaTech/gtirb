#pragma once

#include <gtirb/Export.hpp>
#include <string>
#include <utility>

namespace gtirb
{
	class GTIRB_GTIRB_EXPORT_API Exception : public std::exception
	{
	public:
		void setLocation(std::string file, int line);
		std::pair<std::string, int> getLocation() const;
	
	private:
		std::string file;
		int line{0};
	};
}
