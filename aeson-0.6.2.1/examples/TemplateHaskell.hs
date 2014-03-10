-- We can use Template Haskell (TH) to generate instances of the
-- FromJSON and ToJSON classes automatically.  This is the fastest way
-- to add JSON support for a type.

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)

-- This splice will derive instances of ToJSON and FromJSON for us.
--
-- The use of "id" below is a placeholder function to transform the
-- names of the type's fields.  We don't want to transform them, so we
-- use the identity function.

$(deriveJSON id ''Coord)

main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord 123.4 20
  BL.putStrLn (encode reply)
