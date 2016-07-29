{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Text.Bsdconv as B
import qualified Data.ByteString as BS
import System.Environment
import System.IO

main :: IO ()
main =
  getArgs >>= \case
    (conv:_) -> do input <- BS.getContents
                   let (output, ierr, oerr) = bsdconvWithErrCount conv input
                   BS.putStr output
                   hPutChar stderr '\n'
                   hPutStr stderr "ierr: " >> hPrint stderr ierr
                   hPutStr stderr "oerr: " >> hPrint stderr oerr
    [] -> hPutStrLn stderr "Usage: hs-bsdconv conversion < input-file > output-file"
