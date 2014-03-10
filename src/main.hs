{-# LANGUAGE 
  OverloadedStrings,
  TemplateHaskell,
  FlexibleInstances,
  TypeFamilies,
  TypeOperators
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

import Data.Record hiding (Name)
import Data.Record.TH
import Data.TypeFun
import Data.Kind

import Language.Haskell.TH as TH


type Url = Text

$(fields [ ("FakeName", [t| Text |], "name", ALL)
         , ("Resource_uri", [t| Text |], "resource_api", ALL)
         , ("Accuracy", [t| Int |], "accuracy", ALL)
         , ("Power", [t| Int |], "power", ALL)
         , ("Attack", [t| Int |], "attack", ALL)
         , ("Sp_atk", [t| Int |], "sp_atk", ALL)
         -- ("Moves", [t| [MoveFull] |], "moves", ALL)
         ])

-- type Move = X :& FName ::: Text :& Resource_uri ::: Url

takle :: (X :& FakeName ::: Text :& Resource_uri ::: Text) (Id KindStar)
takle = X :& FakeName := (pack "Tackle") :& Resource_uri := pack "/api/nope"


-- main = TH.runQ (fields [("Power", [t| Maybe Int |], "power", ALL)]) >>= putStrLn. TH.pprint

-- main = L.putStr $ encode takle

type MoveFull = (X 
        :& FakeName ::: Text 
        :& Accuracy ::: Int 
        :& Power ::: Int 
        :& Resource_uri ::: Url
        ) (Id KindStar)

type Pokemon = (X
        :& FakeName ::: Text
        :& Attack ::: Int
        :& Sp_atk ::: Int
        -- :& Moves ::: [Move]
        ) (Id KindStar)

charmander = "{\"name\":\"Charmander\",\"speed\":65,\"defense\":43,\"sp_atk\":60,\"attack\":52,\"hp\":39,\"sp_def\":50}"

api_base_url = "http://pokeapi.co/"

construct_pokemon_url id = api_base_url ++ "api/v1/pokemon/" ++ show id ++ "/"

get_pokemon :: Int -> IO (Either String Pokemon)
get_pokemon id = fmap eitherDecode $ simpleHttp (construct_pokemon_url id)

get_name rec = case convert rec of
                    X :& FakeName := name -> name

main = do pokemon_id <- randomRIO (0,251)
          putStrLn $ "Looking up: " ++ show (pokemon_id :: Int)
          maybe_poke <- get_pokemon pokemon_id 
          case maybe_poke of
            Left msg -> putStr $ "Failed to parse" ++ msg
            Right p -> do
              L.putStrLn $ encode p
              putStrLn $ unpack (get_name p)
--}
