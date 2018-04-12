#pragma once

#ifdef WIN32
	#if defined GTIRB_GTIRB_EXPORTS
		#define GTIRB_GTIRB_EXPORT_API _declspec(dllexport)
	#else
		#define GTIRB_GTIRB_EXPORT_API _declspec(dllimport)
	#endif
#else
	#define GTIRB_GTIRB_EXPORT_API __attribute__ ((visibility ("default")))
#endif
