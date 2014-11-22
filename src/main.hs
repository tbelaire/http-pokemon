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
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy as L

import Data.Aeson
import Network.HTTP.Conduit

import Data.Vinyl
import Data.Vinyl.JSON

import Control.Lens

type Url = T.Text
name         = Field :: ("name"         ::: T.Text)
resource_uri = Field :: ("resource_uri" ::: Url)
moves        = Field :: ("moves"        ::: [MoveSummary])


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

type PokemonFull = PlainRec '[
        "name" ::: T.Text,
        "attack" ::: Int,
        "sp_atk" ::: Int,
        "moves" ::: [MoveFull]
        ]

charmander = "{\"name\":\"Charmander\",\"speed\":65,\"defense\":43,\"sp_atk\":60,\"attack\":52,\"hp\":39,\"sp_def\":50}"

api_base_url = "http://pokeapi.co/"

construct_pokemon_url id = api_base_url ++ "api/v1/pokemon/" ++ show id ++ "/"

get_pokemon :: Int -> IO (Either String Pokemon)
get_pokemon id = fmap eitherDecode $ simpleHttp (construct_pokemon_url id)

get_move :: Url -> IO (Maybe MoveFull)
get_move uri = (fmap decode) $ simpleHttp (api_base_url ++ T.unpack uri)

-- [] `is_longer_than` _ = False
-- _ `is_longer_than` 0 = True
-- (x:xs) `is_longer_than` n = xs `is_longer_than` (n-1)

concat_maybes = concatMap (maybe [] (:[]))

set_at _ [] elem = []
set_at 0 (_:xs) elem = elem : xs
set_at n (x:xs) elem = x : (set_at (n-1) xs elem)

-- Resivoir sampleing [http://stackoverflow.com/questions/54059/efficiently-selecting-a-set-of-random-elements-from-a-linked-list]
pick :: Int -> [a] -> IO [a]
pick n lst = pick' n (take n lst) (drop n lst)
    where
        pick' :: Int -> [a] -> [a] -> IO [a]
        pick' seen resivoir [] = return resivoir
        pick' seen resivoir (x:xs) =
            do j <- randomRIO (1, seen)
               if j < n
               then pick' (seen + 1) (set_at (j+1) resivoir x) xs
               else pick' (seen + 1) resivoir xs

main = do pokemon_id <- randomRIO (0,251)
          putStrLn $ "Looking up: " ++ show (pokemon_id :: Int)
          maybe_poke <- get_pokemon pokemon_id 
          case maybe_poke of
            Left msg -> putStr $ "Failed to parse" ++ msg
            Right p -> do
              L.putStrLn $ encode p
              putStr "A wild "
              putStr $ T.unpack (rGet name p)
              putStr " appeared!"
              putStrLn ""

              learned_moves <- pick 4 (rGet moves p)
              let p' :: Pokemon
                  p' = rPut moves learned_moves p

              putStrLn $ "It has moves " ++ show [ rGet name move | move <- rGet moves p' ]

              moves_full <- mapM (\move -> get_move (rGet resource_uri move))
                                 (rGet moves p')
              forM_ (concat_maybes moves_full) $ \move ->
                  do putStr $ "Name: "
                     T.putStrLn $ rGet name move
--}
