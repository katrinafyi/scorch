module Compiler.Loader where

import Control.Applicative (liftA2)
import Data.Binary.Get (Get, getWord16be, isEmpty, runGet)
import qualified Data.ByteString.Lazy as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)

many :: Get a -> Get [a]
many g = do
  b <- isEmpty
  if b then pure [] else liftA2 (:) g (many g)

getManyWord16 :: Get [Word16]
getManyWord16 = many getWord16be

loadProgram :: B.ByteString -> Map Integer Integer
loadProgram bin = Map.fromList $ zip [0 ..] $ fromIntegral <$> binwords
  where
    binwords = runGet getManyWord16 bin
