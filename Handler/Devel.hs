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
    toWidget [julius|
      $(document).ready(function (){
        $('label.tree-toggler').click(function (){
          $(this).parent().children('ul.tree').toggle(300);
        });
        $('label.tree-toggler').parent().children('ul.tree').toggle(300);
      });
    |]
    [whamlet|
  <div .panel-group id="accordion">
    <div .panel .panel-default>
      $forall (Entity classid equipClass) <- equipclasses
        <div .panel-heading>
          <a data-toggle="collapse" data-parent="#accordion" href="##{equipmentClassName equipClass}">
            #{equipmentClassName equipClass}
        <div id="#{equipmentClassName equipClass}" .panel-collapse .collapse >
          <div .panel-body>
            ^{equipTree classid}
   |]


equipTree :: EquipmentClassId -> Widget
equipTree classid = do
  equips <- handlerToWidget $ runDB $ selectList [EquipmentType ==. classid] [Asc EquipmentId]
  [whamlet|
   <ul .list-group>
     $forall (Entity equipid equip) <- equips
       <li .list-group-item>
         <a href=@{EquipmentR $ pack $ equipmentName equip}>#{equipmentName equip}
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
    <div .col-md-2>
      ^{equipClassTree}
    <div .col-md-5>
      <div .panel .panel-default>
        <div .panel-heading>
          最近の入力
        <div .panel-body>
          ^{table}
    <div .col-md-5>
      <div .panel .panel-default>
        <div .panel-heading>
          開発入力
        <div .panel-body>
          $maybe _ <- muser
          $nothing
            <div .alert .alert-info>
              現在未ログインです.
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
