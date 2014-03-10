{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Data.Record.TH (JSONSpec(..), fields, Field, Rec, TypeOf) where
import Data.Record
import Language.Haskell.TH hiding (Name)
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Data.Kind
import Data.TypeFun
import qualified Data.HashMap.Strict as H

-- | Specify what level of JSON generation you want
data JSONSpec = ALL | TO | FROM | NONE deriving (Show, Eq)

class ToJSONField a where
    toJSONField :: a -> (Text,Value)

class FromJSONField a where
    fromJSONField :: Object -> Parser a

-- | The data carried by a particular field name
type family TypeOf c

-- | A field representing its TypeOf
type Field a = (a ::: TypeOf a)

-- | A record using the Id type function as its sort
type Rec a = a (Id KindStar)

-- | Generate field declarations for the given strings. For example:
-- @
--  $(fieldsJSON [("A", [t| Int |], "a") ])
-- @
-- generates the code
-- @
-- data A = A
-- instance Name A where name = A
-- instance ToJSON x => ToJSONField ((A ::: x) (Id KindStar)) where
--      toJSONField (A := x) = ("a", toJSON x)
-- instance FromJSON x => ToJSONField ((A ::: x) (Id KindStar)) where
--      toJSONField (A := x) = ("a", toJSON x)
-- @
fields :: [(String, TypeQ, String, JSONSpec)] -> Q [Dec]
fields ss = liftM concat $ forM ss $ \(s,t,j_name,a)-> do
    let con_name = mkName s
    let jsonName = litE (stringL j_name)
    t' <- t

    let tyQ = [t| ($(conT con_name) ::: $t) (Id KindStar) |]
    let op = (case t' of
                (AppT (ConT a) _) -> if a == ''Maybe then '(.:?) else '(.:)
                _ -> '(.:))

    let data_decl = [DataD [] con_name [] [NormalC con_name []] [''Show, ''Eq]]
    main <- [d|
        instance Data.Record.Name $(conT con_name) where
            name = $(conE con_name)
        type instance TypeOf $(conT con_name) = $t |]

    to <- [d|
        instance ToJSONField $tyQ where
            toJSONField ( _ := y ) = ( $jsonName, toJSON y ) |]
    from <- [d|
        instance FromJSONField $tyQ where
            fromJSONField v = do f <- ($(varE op)) v $jsonName -- v .: jsonName
                                 return $ $(conE con_name) := f |]
    case a of
            NONE -> return $ data_decl ++ main
            FROM -> return $ data_decl ++ main ++ from
            TO -> return $ data_decl ++ main ++ to
            ALL -> return $ data_decl ++ main ++ to ++ from

instance ToJSON (X (Id KindStar)) where toJSON X = Object H.empty
instance (ToJSON (a (Id KindStar)), ToJSONField (b (Id KindStar))) 
        => ToJSON ((a :& b) (Id KindStar)) where
    toJSON (a :& b) =
        case toJSON a of
            Object o ->
                let (k,v) = toJSONField b
                in if v == Null then Object o else Object (H.insert k v o)
            _ -> error "Expecting an object in toJSON method"

instance FromJSON (X (Id KindStar)) where parseJSON _ = return X
instance (FromJSON (a (Id KindStar)), FromJSONField (b (Id KindStar))) => FromJSON ((a :& b) (Id KindStar)) where
    parseJSON a@(Object o) = do
        rest <- parseJSON a
        it <- fromJSONField o
        return $ rest :& it
    parseJSON _ = mzero

instance Default (X style) where def = X
instance (Default (a style), Default (App style f), Name n) => Default ((a :& (n ::: f)) style) where
    def = def :& name := def

instance Eq (X style) where _ == _ = True
instance (Eq (a style), Eq (App style f), Name n) => Eq ((a :& (n ::: f)) style) where
    (as :& (_ := a)) == (bs :& (_ := b)) = as == bs && a == b

{-
instance ToJSON x => ToJSONField ((A ::: x) (Id KindStar)) where
    toJSONField (A := x) = ("A", toJSON x)
-}

{-
instance FromJSON x => FromJSONField ((A ::: Maybe x) (Id KindStar)) where
    fromJSONField o = do
        n <- o .:? "A"
        return $ A := n
-}
