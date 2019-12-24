{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Network.HTTP.Simple                ( httpBS, getResponseBody )
import Control.Lens                       ( preview )
import Data.Aeson.Lens                    ( key, _String )
import qualified Data.ByteString.Char8 as BS
import Data.Text                          ( Text, pack, unpack )
import qualified Data.Text.IO          as TIO

fetchJSON :: IO BS.ByteString
fetchJSON = do
  res <- httpBS "https://api.coindesk.com/v1/bpi/currentprice.json"
  return (getResponseBody res)

type CurrencySymbol = String

getRateForCurrency :: CurrencySymbol -> BS.ByteString -> Maybe String
getRateForCurrency symbol = (unpack <$>) . preview (key "bpi" . key (pack symbol) . key "rate" . _String)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

getRate :: CurrencySymbol -> BS.ByteString -> String
getRate symbol json = case (getRateForCurrency symbol json) of
  Nothing   -> "Could not find the Bitcoin rate for currency symbol " <> symbol <> "."
  Just rate -> "The current Bitcoin rate is " <> rate <> symbol <> "."

main :: IO ()
main = do
  args <- getArgs
  json <- fetchJSON
  case ((\symbol -> getRate symbol json) <$> (safeHead args)) of
    Nothing   -> print "Missing currency symbol!"
    Just res  -> print res
