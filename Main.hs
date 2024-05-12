{-# LANGUAGE OverloadedStrings #-}

module Main where

import Interpreter

import Data.Text.Lazy as Text (Text, pack, unpack)
import qualified Data.ByteString.Lazy.UTF8 as BL
import System.IO
import System.Environment
import Control.Applicative

import Web.Scotty

main :: IO ()
main = do
    scotty 8001 $ do
        post "/interpreter" $ do
            authHeader <- header "Authorization"
            case authHeader of
                Nothing -> do
                    text "Unauthorized access"
                Just _ -> do
                    input <- body
                    let result = show $ evalLambdaExpr (BL.toString input)
                        message = if result == BL.toString input then "The lambda expression " ++ result ++ " loops infinitely." else "Result: " ++ result
                    text $ pack message