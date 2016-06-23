{-# LANGUAGE ForeignFunctionInterface #-}

#include "enchant/enchant.h"

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Kwang Yul Seo
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Kwang Yul Seo <kwangyul.seo@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Binding to the Enchant library.
----------------------------------------------------------------------------
module Language.Enchant
  ( Broker
  , brokerDescribe
  , brokerDictExists
  , brokerFree
  , brokerFreeDict
  , brokerGetError
  , brokerGetParam
  , brokerInit
  , brokerListDicts
  , brokerRequestDict
  , brokerRequestPwlDict
  , brokerSetOrdering
  , brokerSetParam
  , Dict
  , dictAdd
  , dictAddToSession
  , dictCheck
  , dictDescribe
  , dictGetError
  , dictIsAdded
  , dictIsRemoved
  , dictRemove
  , dictRemoveFromSession
  , dictStoreReplacement
  , dictSuggest
  , getVersion
  , Provider(..)
  , withBroker
  , withDict
  ) where

import Control.Exception (bracket)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Foreign.C.String
import Foreign.C.Types
import Foreign.C.UTF8
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

data Broker'
{#pointer *EnchantBroker as Broker -> Broker' #}
data Dict'
{#pointer *EnchantDict as Dict -> Dict' #}

-- | Returns the Enchant version.
{#fun unsafe enchant_get_version as getVersion {} -> `String' peekUTF8String*#}

-- | Creates a new broker object.
{#fun unsafe enchant_broker_init as brokerInit {} -> `Broker' id#}

-- | Frees a broker resource with all its dictionaries.
{#fun unsafe enchant_broker_free as brokerFree {id `Broker'} -> `()'#}

-- | Creates a new dictionary using tag, the non-empty language tag you wish
-- to request a dictionary for ("en_US", "de_DE", ...)
{#fun unsafe enchant_broker_request_dict as brokerRequestDict {id `Broker', withUTF8String* `String'} -> `Dict' id#}

-- | Creates a dictionary using a PWL file. A PWL file is personal word file
-- one word per line.
{#fun unsafe enchant_broker_request_pwl_dict as brokerRequestPwlDict {id `Broker', withUTF8String* `String'} -> `Dict' id#}

-- | Frees a dictionary resource.
{#fun unsafe enchant_broker_free_dict as brokerFreeDict {id `Broker', id `Dict'} -> `()'#}

-- | Tells if a dictionary exists or not, using a non-empty tags.
{#fun unsafe enchant_broker_dict_exists as brokerDictExists {id `Broker', withUTF8String* `String'} -> `Bool'#}

-- | @brokerSetOrdering tag ordering@ declares a preference of dictionaries to
-- use for the language described/referred to by @tag@. The ordering is a comma
-- delimited list of provider names. As a special exception, the "*" tag can be
-- used as a language tag to declare a default ordering for any language that
-- does not explicitly declare an ordering.
{#fun unsafe enchant_broker_set_ordering as brokerSetOrdering {id `Broker', withUTF8String* `String', withUTF8String* `String'} -> `()'#}

-- | Returns the last error which occurred in this broker.
{#fun unsafe enchant_broker_get_error as brokerGetError {id `Broker'} -> `String' peekUTF8String*#}

{#fun unsafe enchant_broker_get_param as brokerGetParam {id `Broker', withUTF8String* `String'} -> `String' peekUTF8String*#}
{#fun unsafe enchant_broker_set_param as brokerSetParam {id `Broker', withUTF8String* `String', withUTF8String* `String'} -> `()'#}

type BrokerDescribeFn = CString -> CString -> CString -> Ptr () -> IO ()

foreign import ccall "wrapper"
  createBrokerDescriberFn :: BrokerDescribeFn -> IO (FunPtr BrokerDescribeFn)

withBrokerDescribeFn f a = createBrokerDescriberFn f >>= a

{#fun enchant_broker_describe as _brokerDescribe {id `Broker', withBrokerDescribeFn* `BrokerDescribeFn', id `Ptr ()'} -> `()'#}

-- | Information of the Enchant provider
data Provider = Provider
  { langTag :: String         -- ^ The dictionary's language tag (eg: en_US, de_AT, ...)
  , providerName :: String    -- ^ The provider's name (eg: Aspell)
  , providerDesc :: String    -- ^ The provider's description (eg: Aspell 0.50.3)
  , providerDllName :: String -- ^ The DLL/SO where this dict's provider was loaded from in Glib file encoding
  } deriving (Eq, Show)

-- | Enumerates the Enchant providers and tells you some rudimentary information about them.
brokerDescribe :: Broker -> IO [Provider]
brokerDescribe b = do
  acc <- newIORef []
  let cb pName pDesc pDllFile _ = do name <- peekUTF8String pName
                                     desc <- peekUTF8String pDesc
                                     dllFile <- peekUTF8String pDllFile
                                     let p = Provider "" name desc dllFile
                                     modifyIORef acc (p:)
  _brokerDescribe b cb nullPtr
  readIORef acc

withUTF8StringLenIntConv :: Num n => String -> ((CString, n) -> IO a) -> IO a
withUTF8StringLenIntConv s f = withUTF8StringLen s $ \(p, n) -> f (p, fromIntegral n)

-- | Checks whether a word is correctly spelled or not.
{#fun unsafe enchant_dict_check as dictCheck {id `Dict', withUTF8StringLenIntConv* `String'&} -> `Bool'#}

-- | Adds a word to the given dictionary.
{#fun unsafe enchant_dict_add as dictAdd {id `Dict', withUTF8StringLenIntConv* `String'&} -> `()'#}

-- | Adds a word to the given dictionary. It will be added only for the active
-- spell-checking session.
{#fun unsafe enchant_dict_add_to_session as dictAddToSession {id `Dict', withUTF8StringLenIntConv* `String'&} -> `()'#}

-- | Removes the word from the given dictionary.
{#fun unsafe enchant_dict_remove as dictRemove {id `Dict', withUTF8StringLenIntConv* `String'&} -> `()'#}

-- | Removes the word from the given dictionary. It will be only removed from
-- the active spell-checking session.
{#fun unsafe enchant_dict_remove_from_session as dictRemoveFromSession {id `Dict', withUTF8StringLenIntConv* `String'&} -> `()'#}

-- | Returns true if the word is added to the given dictionary.
{#fun unsafe enchant_dict_is_added as dictIsAdded {id `Dict', withUTF8StringLenIntConv* `String'&} -> `Bool'#}

-- | Returns true if the word is removed from the given dictionary.
{#fun unsafe enchant_dict_is_removed as dictIsRemoved {id `Dict', withUTF8StringLenIntConv* `String'&} -> `Bool'#}

-- | @dictStoreReplacement dict mis cor@ adds a correction for @mis@ using @cor@.
--
-- Notes that you replaced @mis@ with @cor@, so it's possibly more likely that
-- future occurrences of @mis@ will be replaced with @cor@. So it might bump
-- @cor@ up in the suggestion list.
{#fun unsafe enchant_dict_store_replacement as dictStoreReplacement {id `Dict', withUTF8StringLenIntConv* `String'&, withUTF8StringLenIntConv* `String'&} -> `()'#}

{#fun unsafe enchant_dict_suggest as _dictSuggest {id `Dict', withUTF8StringLenIntConv* `String'&, alloca- `CULong' peek*} -> `Ptr CString' id#}
{#fun unsafe enchant_dict_free_string_list as _dictFreeStringList {id `Dict', id `Ptr CString'} -> `()'#}

-- | Return a list of suggestions if the word is bad spelled.
dictSuggest :: Dict -> String -> IO [String]
dictSuggest d s = bracket (_dictSuggest d s) cleanup go
  where
    go (p, n) = do
      cs <- peekArray ((fromIntegral n) - 1) p
      mapM peekUTF8String cs
    cleanup (p, _) = _dictFreeStringList d p

-- | Returns the last error of the current spelling-session.
{#fun unsafe enchant_dict_get_error as dictGetError {id `Dict'} -> `String' peekUTF8String*#}

type DictDescribeFn = CString -> CString -> CString -> CString -> Ptr () -> IO ()

foreign import ccall "wrapper"
  createDictDescriberFn :: DictDescribeFn -> IO (FunPtr DictDescribeFn)

withDictDescribeFn f a = createDictDescriberFn f >>= a

{#fun enchant_dict_describe as _dictDescribe {id `Dict', withDictDescribeFn* `DictDescribeFn', id `Ptr ()'} -> `()'#}

-- | Returns the details of the dictionary.
dictDescribe :: Dict -> IO [Provider]
dictDescribe d = do
  acc <- newIORef []
  let cb pLangTag pName pDesc pDllFile _ = do langTag <- peekUTF8String pLangTag
                                              name <- peekUTF8String pName
                                              desc <- peekUTF8String pDesc
                                              dllFile <- peekUTF8String pDllFile
                                              let p = Provider langTag name desc dllFile
                                              modifyIORef acc (p:)
  _dictDescribe d cb nullPtr
  readIORef acc

{#fun enchant_broker_list_dicts as _brokerListDicts {id `Broker', withDictDescribeFn* `DictDescribeFn', id `Ptr ()'} -> `()'#}

-- | Returns a list of available dictionaries with their details.
brokerListDicts :: Broker -> IO [Provider]
brokerListDicts d = do
  acc <- newIORef []
  let cb pLangTag pName pDesc pDllFile _ = do langTag <- peekUTF8String pLangTag
                                              name <- peekUTF8String pName
                                              desc <- peekUTF8String pDesc
                                              dllFile <- peekUTF8String pDllFile
                                              let p = Provider langTag name desc dllFile
                                              modifyIORef acc (p:)
  _brokerListDicts d cb nullPtr
  readIORef acc

-- | @'withBroker' act@ opens a broker using 'brokerInit'
-- and passes the resulting broker to the computation @act@.  The resource
-- will be freed on exit from 'withBroker', whether by normal
-- termination or by raising an exception.
withBroker :: (Broker -> IO a) -> IO a
withBroker = bracket brokerInit brokerFree

-- | @'withDict' broker tag act@ opens a dict using 'brokerRequestDict'
-- and passes the resulting dict to the computation @act@.  The resource
-- will be freed on exit from 'withDict', whether by normal
-- termination or by raising an exception.
withDict :: Broker -> String -> (Dict -> IO a) -> IO a
withDict broker tag = bracket (brokerRequestDict broker tag) (brokerFreeDict broker)

