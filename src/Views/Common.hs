{-# LANGUAGE OverloadedStrings #-}

module Views.Common where

import           Data.Monoid                 (mempty)
import           Prelude                     hiding (div, head, id)
import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5            (a, body, button, dataAttribute,
                                              div, docTypeHtml, head, li, link,
                                              meta, script, title, ul, (!))
import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                              httpEquiv, media, name, rel, src,
                                              type_)

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
  head $ do
    title t
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
    meta ! name "description" ! content "Inspire Text"
    meta ! name "viewport" ! content "width=device-width"
    link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! rel  "stylesheet" ! media "screen"
  body $ do
    navBar >> b
    script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
    script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

navBar :: Html
navBar = div ! class_ "navbar navbar-default navbar-static-top" $ div ! class_ "container" $ do
           div ! class_ "navbar-header" $
             button ! type_ "button"
                    ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $
               a ! class_ "navbar-brand" ! href "#" $ "λ"
           div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav" $
             li ! class_ "active" $ a ! href "/shoes" $ "List shoes"
