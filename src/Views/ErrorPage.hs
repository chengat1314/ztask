{-# LANGUAGE OverloadedStrings #-}

module Views.ErrorPage where

import           Prelude                     hiding (div)
import           Text.Blaze.Html             (Html, toHtml)
import           Text.Blaze.Html5            (a, div, p, (!))
import           Text.Blaze.Html5.Attributes (class_, href, onclick)
import           Views.Common

errorPage :: String -> Html
errorPage err = layout "error" $
  div ! class_ "alert alert-error text-center" $ do
    p $ toHtml err
    p $ a ! href "##" ! onclick "history.go(-1); return false;" $ "Go back"
