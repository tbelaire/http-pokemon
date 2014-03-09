{-# LANGUAGE 
  OverloadedStrings,
  TemplateHaskell
  #-}

module  Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.Random

import Data.Text
import qualified Data.ByteString.Lazy as L

import Data.Aeson
import Network.HTTP.Conduit

import Data.Record
import Data.Record.TH


data MoveFull = MoveFull {
    move_full_name :: Text,
    move_full_accuracy :: Int,
    move_full_power :: Int
}


data Pokemon = Pokemon {
    pokemon_name :: Text,
    hp :: Int,
    attack :: Int,
    defense :: Int,
    sp_atk :: Int,
    sp_def :: Int,
    speed :: Int,
    moves :: [MoveSummary]
    } deriving (Eq, Show)

charmander = "{\"name\":\"Charmander\",\"speed\":65,\"defense\":43,\"sp_atk\":60,\"attack\":52,\"hp\":39,\"sp_def\":50}"

api_base_url = "http://pokeapi.co/"

construct_pokemon_url id = api_base_url ++ "api/v1/pokemon/" ++ show id ++ "/"

get_pokemon :: Int -> IO (Either String Pokemon)
get_pokemon id = fmap eitherDecode $ simpleHttp (construct_pokemon_url id)

main = do pokemon_id <- randomRIO (0,251)
          putStrLn $ "Looking up: " ++ show (pokemon_id :: Int)
          maybe_poke <- get_pokemon pokemon_id 
          case maybe_poke of
            Left msg -> putStr $ "Failed to parse" ++ msg
            Right p -> do
              L.putStrLn $ encode p
              putStrLn $ unpack (pokemon_name p)

