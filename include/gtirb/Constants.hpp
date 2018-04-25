#pragma once

#include <cstdint>
#include <limits>

namespace gtirb
{
    ///
    /// \namespace gtirb::constants
    ///
    /// Constants prevent the use of magic numbers within GT-IRB.
    ///
    namespace constants
    {
        ///
        /// \var BadAddress
        ///
        /// The initial value for an EA.
        ///
        static constexpr uint64_t BadAddress{std::numeric_limits<uint64_t>::max()};

        ///
        /// \var BadFood
        ///
        /// Used by Microsoft's debug HeapAlloc() to mark uninitialized allocated heap memory
        ///
        static constexpr uint64_t BadFood{0xbaadf00d};

        ///
        /// \var FaceFeed
        ///
        /// Used by Mach-O to identify flat (single architecture) object files.
        ///
        static constexpr uint64_t FaceFeed{0xcefaedfe};

        ///
        /// \var DeadBeef
        ///
        /// Dead beef, it's a hex code of 4 bytes, typically used as an example IP address.
        /// 0xDEADBEEF ("dead beef") is used by IBM RS/6000 systems, Mac OS on 32-bit PowerPC
        /// processors and the Commodore Amiga as a magic debug value. On Sun Microsystems' Solaris,
        /// it marks freed kernel memory.
        ///
        static constexpr uint64_t DeadBeef{0xdeadbeef};

        ///
        /// \var DeadDead
        ///
        static constexpr uint64_t DeadDead{0xdeaddead};

        ///
        /// \var PageSize
        ///
        /// The size of a page of data when mapping loaded file images.
        ///
        static constexpr uint64_t PageSize{4096};
    }
}
