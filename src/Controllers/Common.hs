{-# LANGUAGE OverloadedStrings #-}

module Controllers.Common where

import           AppError
import           Control.Monad.Trans           (MonadIO, liftIO)
import           Network.HTTP.Types
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (Html)
import           Web.Scotty.Trans              (ActionT, ScottyError, html,
                                                json, raise, rescue, status,
                                                text)

import           Views.ErrorPage

-- | Helper for reducing boilerplate in rendering views.
blaze :: (ScottyError e, Monad m) => Html -> ActionT e m ()
blaze = html . renderHtml

-- | Helper to rethrow all exception from an action as JSONRequestError.
-- Wrap this around action involve JSON request, error response will be JSON.
rescueJSON :: (Monad m) => ActionT AppError m a -> ActionT AppError m a
rescueJSON = flip rescue (raise . JSONRequestError . show)

-- | Handler for our custom error AppError.
-- For 'JSONRequestError', response with json error message.
-- For 'NotFound', redirect to error page.
-- For 'Other', simply response text error message.
handleEx :: (MonadIO m) => AppError -> ActionT AppError m ()
handleEx e@(JSONRequestError _) = status badRequest400 >> json e
handleEx e@NotFound = blaze $ errorPage $ show e
handleEx e = liftIO (print e) >> status internalServerError500 >> text "Internal Server Error"
