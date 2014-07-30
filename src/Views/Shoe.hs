{-# LANGUAGE OverloadedStrings #-}

module Views.Shoe where

import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Models.Shoe
import           Prelude                     hiding (div, head, id)
import           Text.Blaze.Html             (Html, toHtml)
import           Text.Blaze.Html5            (a, dd, div, dl, dt, h2, img,
                                              table, tbody, td, th, thead,
                                              toValue, tr, (!))
import           Text.Blaze.Html5.Attributes (class_, href, src)
import           Views.Common

listShoes :: [(Text, Shoe)] -> Html
listShoes shoes = layout "list shoes" $
  div ! class_ "container" $
    div ! class_ "jumbotron" $ do
      h2 "List shoes"
      table ! class_ "table table-hover" $ do
        thead $
          tr $ do
            th "ID"
            th "Description"
        tbody $
          mapM_ shoeToHtml shoes
 where shoeToHtml (id, s) =
         tr $ do
           td $ toHtml $
             a ! href ("/shoes/" <> toValue id) $ toHtml id
           td $ toHtml $ shoeDescription s

viewShoes :: Text -> Shoe -> Html
viewShoes id shoes = layout "list shoes" $
  div ! class_ "container" $
    div ! class_ "jumbotron" $ do
      h2 "View shoes"
      dl ! class_ "dl-horizontal" $ do
        dt "Description"
        dd $ toHtml $ shoeDescription shoes
        dt "Color"
        dd $ toHtml $ shoeColor shoes
        dt "Size"
        dd $ toHtml $ shoeSize shoes
        dt "Photo"
        dd $
          img ! class_ "img-responsive" ! src ("/" <> toValue id <> ".jpg")
