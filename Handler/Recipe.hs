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

import Handler.Util

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
    setTitle "建造 / 艦これレシピDB"
    let table = $(widgetFile "recipeTable")
    $(widgetFile "navbar")
    [whamlet|
  <div .page-header>
    <h2>建造
  <div .row>
    <div .col-md-6>
      ^{table}
    <div .col-md-5>
      <div .well>
        $maybe _ <- muser
        $nothing
          <div .alert .alert-info>
            現在未ログインです.
        <h3>建造入力
        <form method=post action=@{RecipeR} entype=#{enctype}>
          ^{widget}
          <input type=submit class="btn btn-primary" value="submit">
|]


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
