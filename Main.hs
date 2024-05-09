{-# LANGUAGE OverloadedStrings #-}

module Main where

import Interpreter

import Data.Text.Lazy as Text (Text, pack, unpack)
import System.IO
import System.Environment
import Control.Applicative

import Data.Aeson
import Data.Aeson.Types (Parser)
import Web.Scotty

newtype Response = StringResp {message :: String} deriving (Show)

instance ToJSON Response where
    toJSON :: Response -> Value
    toJSON (StringResp msg) = object ["error" .= msg]

newtype Expression = Expression {getExpr :: String} deriving (Show)

instance FromJSON Expression where
    parseJSON :: Value -> Parser Expression
    parseJSON = withObject "Expression" $ \v -> Expression
        <$> v .: "expr"

instance Parsable Expression where
    parseParam :: Text -> Either Text Expression
    parseParam = Right . Expression . unpack

instance ToJSON LambdaExpr where
    toJSON :: LambdaExpr -> Value
    toJSON expr = object ["expr" .= show expr]

main :: IO ()
main = do
    scotty 8000 $ do
        put "/interpreter" $ do
            authHeader <- header "Authorization"
            case authHeader of
                Nothing -> do
                    text "Unauthorized access"
                Just _ -> do
                    input <- formParam "code"
                    text $ pack $ show $ evalLambdaExpr (getExpr input)