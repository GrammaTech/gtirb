#include <gtirb/Exception.hpp>
#include <iostream>

using namespace gtirb;

Exception::Exception(const char* what) : std::logic_error(what)
{
	
}

Exception::Exception(const std::string& what) : std::logic_error(what)
{

}

Exception::Exception(const std::string& what, std::string f, int l) : std::logic_error(what), file{f}, line{l}
{
}

void Exception::setLocation(std::string f, int l)
{
    this->file = f;
    this->line = l;
}

std::pair<std::string, int> Exception::getLocation() const
{
    return std::make_pair(this->file, this->line);
}
