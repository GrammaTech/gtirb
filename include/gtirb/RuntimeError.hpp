#pragma once

#include <gtirb/Exception.hpp>

namespace gtirb
{
	class GTIRB_GTIRB_EXPORT_API RuntimeError : public gtirb::Exception
	{
	public:
		RuntimeError() = default;
		RuntimeError(std::string file, int line);
		
		virtual ~RuntimeError() = default;
		
		virtual const char* what() const noexcept override;
	};
}
