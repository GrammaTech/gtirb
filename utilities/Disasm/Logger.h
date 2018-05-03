#pragma once

#include <iostream>

/// \todo   Replace these trivial logger macros with boost logger or g3log.

#ifdef _DEBUG
#define LOG_INFO std::cout << "[INFO] (" << __FILE__ << ":" < __LINE__ << ")  "
#define LOG_ERROR std::cout << "[ERROR] (" << __FILE__ << ":" << __LINE__ << ") "
#else
#define LOG_INFO std::cout << "[INFO]  "
#define LOG_ERROR std::cout << "[ERROR] "
#endif

#define LOG_DEBUG std::cout << "[DEBUG] (" << __FILE__ << ":" << __LINE__ << ") "
