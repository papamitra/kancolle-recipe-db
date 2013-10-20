-- -*- coding:utf-8 -*-
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

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

import Yesod.Auth.OAuth(twitterUrl)

maybeRead = fmap fst . listToMaybe .reads

getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildviewPosted]
  sess <- (lookupSession (pack "recipe"))
  let recipeSess = sess >>= (maybeRead . unpack) :: Maybe (Recipe ShipId)
  !shiplist <- shipList
  (widget, enctype) <- generateFormPost $ recipeForm recipeSess shiplist
  muser <- maybeAuth
  setUltDest RecipeR -- ログイン後このページに戻ってくるための設定
  defaultLayout $ do
    setTitle "recipe"
    $(widgetFile "navbar")
    [whamlet|
  <div .page-header>
    <h2>建造
  <form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}
    <input type=submit class="btn btn-primary" value="submit">
|]
    $(widgetFile "recipeTable")

postRecipeR :: Handler Html
postRecipeR = do
  !shiplist <- shipList
  ((result, widget), enctype) <- runFormPost $ recipeForm Nothing shiplist
  case result of
    FormSuccess recipe -> do
      time <- liftIO getCurrentTime
      res <- runDB $ insertBy $ Resource (fuel recipe) (amm recipe) (steel recipe) (baux recipe)
      let resourceId = either entityKey id res
      forM_ (createdIds recipe) $ \shipId ->
        runDB $ insert $ Shipbuild (Key $ PersistInt64 1) shipId time (secId recipe) (secLv recipe) (hqLv recipe) resourceId
      setSession (pack "recipe") (pack $ show recipe)
      return ()
    _ -> error "error"
  redirect RecipeR
