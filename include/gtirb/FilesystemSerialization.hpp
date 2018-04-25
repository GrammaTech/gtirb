#pragma once

///
/// \def GTIRB_SERIALIZE_FILESYSTEM_PATH
///
/// An evil macro to support serialization of boost::filesystem::path.
/// This will need used until boost::serialization supports std::filesystem.
///
#define GTIRB_SERIALIZE_FILESYSTEM_PATH(ARCHIVE, PATH) \
    {                                                  \
        std::string s;                                 \
        if(Archive::is_saving::value)                  \
        {                                              \
            s = PATH.string();                         \
        }                                              \
        ARCHIVE& s;                                    \
        if(Archive::is_loading::value)                 \
        {                                              \
            PATH = s;                                  \
        }                                              \
    }
