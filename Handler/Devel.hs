{-*- coding: utf-8 -*-}
module Handler.Devel where

import Import
import Data.Text(pack)
import Data.Time
import Control.Monad(forM_)
import Yesod.Auth
import Yesod.Form.Fields

import Handler.Util

equipList :: Handler (OptionList EquipmentId)
equipList = do
  equipments <- runDB $ selectList [] [Asc EquipmentId]
  optionsPairs [(pack $ equipmentName equipment, equipid)| Entity equipid equipment <- equipments]

getDevelR :: Handler Html
getDevelR = do
  recipes <- runDB $ selectList [] [Desc DevelopviewPosted]
  muser <- maybeAuth
  (widget, enctype) <- generateFormPost $ recipeForm Nothing equipList
  defaultLayout $ do
    setTitle "開発"
    $(widgetFile "navbar")
    [whamlet|
  <form method=post action=@{DevelR} entype=#{enctype}>
    ^{widget}
    <input type=submit class="btn btn-primary" value="submit">
|]
    $(widgetFile "develTable")

postDevelR :: Handler Html
postDevelR = do
  ((result, widget), enctype) <- runFormPost $ recipeForm Nothing equipList
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
