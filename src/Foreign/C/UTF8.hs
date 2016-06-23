module Foreign.C.UTF8
  ( peekUTF8String, peekUTF8StringLen
  , newUTF8String, newUTF8StringLen
  , withUTF8String, withUTF8StringLen
  ) where

import qualified GHC.Foreign as F
import Foreign.C.String (CString, CStringLen)
import System.IO (utf8)

-- | Analogous to peekCString. Converts UTF8 CString to String.
peekUTF8String :: CString -> IO String
peekUTF8String = F.peekCString utf8

-- | Analogous to peekCStringLen. Converts UTF8 CString to String.
-- The resulting String will end either when @len@ bytes
-- have been converted, or when a NULL is found.
peekUTF8StringLen :: CStringLen -> IO String
peekUTF8StringLen = F.peekCStringLen utf8

-- | Analogous to newCString. Creates UTF8 encoded CString.
newUTF8String :: String -> IO CString
newUTF8String = F.newCString utf8

-- | Analogous to newCStringLen.
-- The length returned is in bytes (encoding units), not chars.
newUTF8StringLen :: String -> IO CStringLen
newUTF8StringLen = F.newCStringLen utf8

-- | Analogous to withCString. Creates UTF8 encoded CString.
withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String = F.withCString utf8

-- | Analogous to withCStringLen.
-- The length returned is in bytes (encoding units), not chars.
withUTF8StringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8StringLen = F.withCStringLen utf8
