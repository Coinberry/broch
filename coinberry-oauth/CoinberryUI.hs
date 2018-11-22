{-# LANGUAGE OverloadedStrings #-}

module CoinberryUI
    ( approvalPage
    )
where

import           Control.Monad (forM_)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time.Clock.POSIX

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A hiding (scope)

import Broch.Model (Scope, scopeName, Client (..))

approvalPage :: Client -> [Scope] -> Int64 -> Html
approvalPage client scopes now = docTypeHtml $ html $ do
    H.head $ do
        link ! rel "stylesheet" ! href "css/coinberry/login.css"
        link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Muli:200,400,600"
        H.title "Coinberry - Authorization"
    body $ do
        header $ img ! src "images/brd.svg"
        main $ do
          H.div ! class_ "disclaimer-box" $
            H.span ! class_ "disclaimer" $ do
              strong $ toHtml ("BRD" :: Text)
              toHtml (" would like to request the following: the ability to initialize accounts, perform withdrawals, access your balance and email address." :: Text)
          H.div ! class_ "button-box" $
            H.form ! method "post" ! action "/approval" $ do
                input ! type_ "hidden" ! name "client_id" ! value (toValue (clientId client))
                input ! type_ "hidden" ! name "expiry" ! value (toValue oneMonth)
                input ! type_ "hidden" ! name "requested_scope" ! value "openid"
                input ! type_ "hidden" ! name "scope" ! value "openid"
                input ! class_ "button" ! type_ "submit" ! value "I Agree"
        footer $ H.span ! class_ "disclaimer" $ toHtml ("No additional personal information is shared with BRD " :: Text)
  where
    aDay    = round posixDayLength :: Int64
    oneDay  = now + aDay
    oneWeek = now + 7*aDay
    oneMonth = now + 30*aDay
