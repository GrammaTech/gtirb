#include <gtirb/CFGNodeInfoCall.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFGNodeInfoCall);

void CFGNodeInfoCall::setKey(int64_t x)
{
	this->key = x;
}

int64_t CFGNodeInfoCall::getKey() const
{
	return this->key;
}

void CFGNodeInfoCall::setReturnSpAdjust(int64_t x)
{
	this->returnSpAdjust = x;
}

int64_t CFGNodeInfoCall::getReturnSpAdjust() const
{
	return this->returnSpAdjust;
}

void CFGNodeInfoCall::setImportTableEntryEA(gtirb::EA x)
{
	this->importTableEntryEa = x;
}

gtirb::EA CFGNodeInfoCall::getImportTableEntryEA() const
{
	return this->importTableEntryEa;
}

