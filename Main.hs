{-# LANGUAGE OverloadedStrings #-}

module Main where

import Interpreter

import Data.Text.Lazy as Text (Text, pack)
import System.IO
import System.Environment
import Control.Applicative

import Data.Aeson
import Data.Aeson.Types (Parser)
import Web.Scotty

newtype Expression = Expression {getExpr :: String} deriving (Show)

instance FromJSON Expression where
    parseJSON :: Value -> Parser Expression
    parseJSON = withObject "Expression" $ \v -> Expression
        <$> v .: "expr"

instance ToJSON Expression where
    toJSON :: Expression -> Value
    toJSON (Expression expr) = object ["expr" .= expr]

main :: IO ()
main = do
    scotty 8000 $ do
        put "/interpreter" $ do
            input <- jsonData :: ActionM Expression
            json $ Expression $ show $ evalLambdaExpr (getExpr input)