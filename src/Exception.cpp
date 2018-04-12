#include <gtirb/Exception.hpp>
#include <iostream>

using namespace gtirb;

Exception::Exception(std::string f, int l) : file{f}, line{l}
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

const char* Exception::what() const noexcept
{
	return "GT-IRB Exception.";
}
