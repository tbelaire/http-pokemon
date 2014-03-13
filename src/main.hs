{-# LANGUAGE 
  OverloadedStrings,
  TemplateHaskell,
  FlexibleInstances,
  TypeFamilies,
  TypeOperators,
  DataKinds
  #-}

module  Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.Random

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

import Data.Aeson
import Network.HTTP.Conduit

import Data.Vinyl
import Data.Vinyl.JSON

type Url = T.Text
name = Field :: ("name" ::: T.Text)
resource_uri = Field :: ("resource_uri" ::: Url)


type MoveSummary = PlainRec '[
         "name" ::: T.Text,
         "resource_uri" ::: Url
         ]

type MoveFull = PlainRec '[
         "name" ::: T.Text,
         "accuracy" ::: Int,
         "power" ::: Int,
         "resource_uri" ::: Url
         ]

type Pokemon = PlainRec '[
        "name" ::: T.Text,
        "attack" ::: Int,
        "sp_atk" ::: Int,
        "moves" ::: [MoveSummary]
        ]

charmander = "{\"name\":\"Charmander\",\"speed\":65,\"defense\":43,\"sp_atk\":60,\"attack\":52,\"hp\":39,\"sp_def\":50}"

api_base_url = "http://pokeapi.co/"

construct_pokemon_url id = api_base_url ++ "api/v1/pokemon/" ++ show id ++ "/"

get_pokemon :: Int -> IO (Either String Pokemon)
get_pokemon id = fmap eitherDecode $ simpleHttp (construct_pokemon_url id)

-- get_name rec = name `rGet` rec

main = do pokemon_id <- randomRIO (0,251)
          putStrLn $ "Looking up: " ++ show (pokemon_id :: Int)
          maybe_poke <- get_pokemon pokemon_id 
          case maybe_poke of
            Left msg -> putStr $ "Failed to parse" ++ msg
            Right p -> do
              L.putStrLn $ encode p
              putStrLn $ T.unpack (name `rGet` p)
--}
