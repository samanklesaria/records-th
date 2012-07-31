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

-- | A record of kind *
type Rec a = a (Id KindStar)

-- | Generate field declarations for the given strings. For example:
-- @
--  $(fields ["A", "B"])
-- @
-- generates the code
-- @
-- data A = A
-- instance Name A where name = A
-- @
fields :: [(String, TypeQ, JSONSpec)] -> Q [Dec]
fields ss = liftM concat $ forM ss $ \(s,t,a)-> do
	let n = mkName s
	t' <- t
	y <- newName "y"
	f <- newName "f"
	ty <- appT (appT (appT [t|(:::)|] (conT n)) t) [t| Id KindStar|]
	let op = case t' of
		(AppT (ConT a) _) -> if a == ''Maybe then '(.:?) else '(.:)
		_ -> '(.:)
	let main = [
		DataD [] n [] [NormalC n []] [''Show],
		InstanceD [] (AppT (ConT ''Name) (ConT n)) [
			FunD 'name [Clause [] (NormalB (ConE n)) []]],
		TySynInstD ''TypeOf [ConT n] t']
	let to = 
		InstanceD [] (AppT (ConT ''ToJSONField) ty) [
			FunD 'toJSONField [
				Clause [ ConP '(:=) [WildP, VarP y]]
				(NormalB $ TupE [LitE $ StringL s, AppE (VarE 'toJSON) (VarE y)]) [] ]]
	let from = 
		InstanceD [] (AppT (ConT ''FromJSONField) ty) [
			FunD 'fromJSONField [
				Clause [VarP y] (
					NormalB $  DoE [
						BindS (VarP f) (AppE (AppE (VarE op) (VarE y)) (LitE $ StringL s)),
						NoBindS $ AppE (VarE 'return) (AppE (AppE (ConE '(:=)) (ConE n)) (VarE f)) ]) []]]
	case a of
		NONE -> return main
		FROM -> return $ main ++ [from]
		TO -> return $ main ++ [to]
		ALL -> return $ main ++ [to, from]

instance ToJSON (X (Id KindStar)) where toJSON X = Object H.empty
instance (ToJSON (a (Id KindStar)), ToJSONField (b (Id KindStar))) => ToJSON ((a :& b) (Id KindStar)) where
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