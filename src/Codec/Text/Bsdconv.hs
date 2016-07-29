{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Text.Bsdconv (bsdconv, bsdconvWithErrCount) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Monoid

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU

import System.IO.Unsafe

C.context (C.baseCtx <> C.bsCtx)

C.include "<bsdconv.h>"

-- | Convert a 'ByteString' with bsdconv using conversion @conv@.
bsdconv :: String -- ^ the conversion passed to badconv, see https://github.com/buganini/bsdconv for examples
        -> ByteString -- ^ the bytestring to be converted
        -> Maybe ByteString -- ^ @Just bs@ if there no error occured during conversion, otherwise @Nothing@
bsdconv conv bs =
  case bsdconvWithErrCount conv bs of
    (bs, 0, 0) -> Just bs
    _          -> Nothing

-- | An alternative version of 'bsdconv' that also reports error counts.
bsdconvWithErrCount :: String
                    -> ByteString
                    -> (ByteString, CSize, CSize) -- ^ (converted bytestring, input error count, output error count)
bsdconvWithErrCount conv bs = unsafePerformIO $
    withCString conv $ \cconv ->
      alloca4 $ \(pstr :: Ptr CString) (plen :: Ptr CSize) (pierr :: Ptr CSize) (poerr :: Ptr CSize) -> do
       [CU.block| void {
         struct bsdconv_instance* inst = bsdconv_create($(const char* cconv));
         bsdconv_init(inst);
         inst->input.data = $bs-ptr:bs;
         inst->input.len = $bs-len:bs;
         inst->input.next = NULL;
         inst->output_mode = BSDCONV_AUTOMALLOC;
         inst->output.data = NULL;
         bsdconv(inst);
         *$(const char** pstr) = inst->output.data;
         *$(size_t* plen) = inst->output.len;
         *$(size_t* pierr) = *inst->ierr;
         *$(size_t* poerr) = *inst->oerr;
         bsdconv_destroy(inst);
       } |]
       strlen <- (,) <$> peek pstr <*> (fromIntegral <$> peek plen)
       ierr <- peek pierr
       oerr <- peek poerr
       bs <- BS.unsafePackMallocCStringLen strlen
       pure (bs, ierr, oerr)
  where alloca4 :: (Storable a, Storable b, Storable c, Storable d) => (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO e) -> IO e
        alloca4 f = alloca $ \a -> alloca $ \b -> alloca $ \c -> alloca $ \d -> f a b c d

