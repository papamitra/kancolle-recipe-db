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

equipClassTree :: Widget
equipClassTree = do
    equipclasses <- handlerToWidget $ runDB $ selectList [] [Asc EquipmentClassId]
    [whamlet|
    $forall (Entity classid equipClass) <- equipclasses
      <ul>
        <li>#{equipmentClassName equipClass}
          ^{equipTree classid}
   |]


equipTree :: EquipmentClassId -> Widget
equipTree classid = do
  equips <- handlerToWidget $ runDB $ selectList [EquipmentType ==. classid] [Asc EquipmentId]
  [whamlet|
   <ul>
     $forall (Entity equipid equip) <- equips
       <li><a href=@{EquipmentR $ pack $ equipmentName equip}>#{equipmentName equip}</a>
  |]


getDevelR :: Handler Html
getDevelR = do
  recipes <- runDB $ selectList [] [LimitTo 10, Desc DevelopviewPosted]
  sess <- (lookupSession (pack "devel"))
  let recipeSess = sess >>= (maybeRead . unpack) :: Maybe (Recipe EquipmentId)
  muser <- maybeAuth
  !elist <- equipList
  (widget, enctype) <- generateFormPost $ recipeForm recipeSess elist
  setUltDest DevelR -- ログイン後このページに戻ってくるための設定
  defaultLayout $ do
    setTitle "開発 / 艦これレシピDB"
    $(widgetFile "navbar")
    let table = $(widgetFile "develTable")
    [whamlet|
  <div .page-header>
    <h2>開発
  <div .row>
    <div .span6>
      ^{table}
      ^{equipClassTree}
    <div .span6>
      <div .well>
        $maybe _ <- muser
        $nothing
          <div .alert .alert-info>
            現在未ログインです.
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
