-- -*- coding:utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}

module Handler.Recipe where

import Import

import Data.Text(pack, unpack)
import Yesod.Form.Fields
import Data.Time
import Text.Printf
import Data.Maybe
import Data.Traversable
import Text.Read
import qualified Control.Monad as M
import Control.Monad(forM_)

import Text.Lucius
import Yesod.Auth

import Handler.Util

maybeRead = fmap fst . listToMaybe .reads

getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildviewPosted]
  sess <- (lookupSession (pack "recipe"))
  let recipeSess = sess >>= (maybeRead . unpack) :: Maybe Recipe
  (widget, enctype) <- generateFormPost $ recipeForm recipeSess
  muser <- maybeAuth
  let recipeTable = $(widgetFile "recipeTable")
  defaultLayout $ do
    setTitle "recipe"
    $(widgetFile "navbar")
    [whamlet|
  <form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}
    <input type=submit class="btn btn-primary" value="submit">
    ^{recipeTable}
|]
          
postRecipeR :: Handler Html
postRecipeR = do
  ((result, widget), enctype) <- runFormPost $ recipeForm Nothing
  case result of
    FormSuccess recipe -> do
      time <- liftIO getCurrentTime
      res <- runDB $ insertBy $ Resource (fuel recipe) (amm recipe) (steel recipe) (baux recipe)
      let resourceId = either entityKey id res
      forM_ (shipIds recipe) $ \shipId ->
        runDB $ insert $ Shipbuild (Key $ PersistInt64 1) shipId time (secId recipe) (secLv recipe) (hqLv recipe) resourceId
      setSession (pack "recipe") (pack $ show recipe)
      return ()
    _ -> error "error"
  redirect RecipeR
