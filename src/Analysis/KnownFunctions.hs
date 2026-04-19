-- | Shared lists of well-known C library function names that appear in
--   multiple analysis, linter, and transformer modules.
--
--   Centralising these eliminates the risk of one module adding a function
--   name while another forgets to, and makes it trivial to extend coverage.
module Analysis.KnownFunctions
  ( allocFns
  , sizeArgFunctions
  , ioWriteFns
  , ioReadFns
  , networkSendFns
  , handleTypes
  , intrinsicPrefixes
  ) where

-- | Heap allocation functions whose size argument is a byte count.
allocFns :: [String]
allocFns = ["malloc", "calloc", "realloc"]

-- | Functions whose arguments are byte-count / size values.
--   A superset of 'allocFns' that also includes memory-manipulation
--   routines (memcpy, memmove, memset, memcmp) and alloca.
sizeArgFunctions :: [String]
sizeArgFunctions =
    allocFns ++
    [ "alloca"
    , "memcpy", "memmove", "memset", "memcmp"
    ]

-- | I/O functions that write a buffer to a file descriptor or stream.
ioWriteFns :: [String]
ioWriteFns = ["fwrite", "write"]

-- | I/O functions that read into a buffer from a file descriptor or stream.
ioReadFns :: [String]
ioReadFns = ["fread", "read"]

-- | Network functions that send a buffer over a socket.
networkSendFns :: [String]
networkSendFns = ["send", "sendto"]

-- | Windows HANDLE-like opaque pointer types.
handleTypes :: [String]
handleTypes =
    [ "HANDLE", "HMODULE", "HWND", "HINSTANCE"
    , "HKEY", "HDC", "HBITMAP", "SOCKET", "HANDLE_PTR"
    ]

-- | Prefix patterns for x86-specific SIMD / compiler intrinsics.
intrinsicPrefixes :: [String]
intrinsicPrefixes =
    [ "_mm_", "_mm256_", "_mm512_", "__mm", "_m_", "__builtin_ia32"
    ]
