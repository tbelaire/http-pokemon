{-# LANGUAGE OverloadedStrings #-}

module  Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.Random

import Data.Text
import qualified Data.ByteString.Lazy as L

import Data.Aeson
import Network.HTTP.Conduit

data MoveSummary = MoveSummary {
   move_summary_name :: Text,
   move_summary_uri :: Text
} deriving (Eq, Show)

instance FromJSON MoveSummary where
    parseJSON (Object v) = MoveSummary <$>
            v .: "name" <*>
            v .: "resource_uri"
    parseJSON _ = mzero

instance ToJSON MoveSummary where
    toJSON (MoveSummary name uri) = object [
        "name" .= name,
        "resource_uri" .= uri]

data MoveFull = MoveFull {
    move_full_name :: Text,
    move_full_accuracy :: Int,
    move_full_power :: Int
}

instance FromJSON MoveFull where
    parseJSON (Object v) = MoveFull <$>
            v .: "name" <*>
            v .: "accuracy" <*>
            v .: "power"
    parseJSON _ = mzero
                            

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

instance FromJSON Pokemon where
    parseJSON (Object v) = Pokemon <$>
                           v .: "name" <*>
                           v .: "hp" <*>
                           v .: "attack" <*>
                           v .: "defense" <*>
                           v .: "sp_atk" <*>
                           v .: "sp_def" <*>
                           v .: "speed" <*>
                           v .: "moves"
    parseJSON _ = mzero

instance ToJSON Pokemon where
    toJSON p = object [ "name" .= pokemon_name p,
                        "hp" .= hp p,
                        "attack" .= attack p,
                        "defense" .= defense p,
                        "sp_atk" .= sp_atk p,
                        "sp_def" .= sp_def p,
                        "speed" .= speed p,
                        "moves" .= moves p]

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



