{-*- coding: utf-8 -*-}
{-# LANGUAGE BangPatterns #-}

module Handler.Devel where

import Import
import Data.Text(pack,unpack)
import Data.Time
import Control.Monad(forM_)
import Yesod.Form.Fields

import Handler.Util

equipList :: Handler (OptionList EquipmentId)
equipList = do
  equipments <- runDB $ selectList [] [Asc EquipmentId]
  optionsPairs [(pack $ equipmentName equipment, equipid)| Entity equipid equipment <- equipments]

getDevelR :: Handler Html
getDevelR = do
  recipes <- runDB $ selectList [] [Desc DevelopviewPosted]
  sess <- (lookupSession (pack "devel"))
  let recipeSess = sess >>= (maybeRead . unpack) :: Maybe (Recipe EquipmentId)
  muser <- maybeAuth
  !elist <- equipList
  (widget, enctype) <- generateFormPost $ recipeForm recipeSess elist
  setUltDest DevelR -- ログイン後このページに戻ってくるための設定
  defaultLayout $ do
    setTitle "開発"
    $(widgetFile "navbar")
    let table = $(widgetFile "develTable")
    [whamlet|
  <div .page-header>
    <h2>開発
  <div .row>
    <div .span6>
      ^{table}
    <div .span6>
      <div .well>
        <h3>開発入力
        <form method=post action=@{DevelR} entype=#{enctype}>
          ^{widget}
  
           <input type=submit class="btn btn-primary" value="submit">
|]


postDevelR :: Handler Html
postDevelR = do
  !elist <- equipList
  ((result, widget), enctype) <- runFormPost $ recipeForm Nothing elist
  muser <- maybeAuth
  
  let userid = case muser of
        Just (Entity userId _) -> userId
        Nothing -> Key (PersistInt64 1)
  case result of
    FormSuccess recipe -> do
      time <- liftIO getCurrentTime
      res <- runDB $ insertBy $ Resource (fuel recipe) (amm recipe) (steel recipe) (baux recipe)
      let resourceId = either entityKey id res
      forM_ (createdIds recipe) $ \equipId ->
        runDB $ insert $ Develop userid equipId time (secId recipe) (secLv recipe) (hqLv recipe) resourceId
      setSession (pack "devel") (pack $ show recipe)
      return ()
    _ -> error "Form result error"
  redirect DevelR
