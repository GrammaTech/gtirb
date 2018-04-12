#include <gtirb/Exception.hpp>

using namespace gtirb;

void Exception::setLocation(std::string f, int l)
{
	this->file = f;
	this->line = l;
}
		
std::pair<std::string, int> Exception::getLocation() const
{
	return std::make_pair(this->file, this->line);
}
