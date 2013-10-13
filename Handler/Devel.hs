{-*- coding: utf-8 -*-}
module Handler.Devel where

import Import
import Data.Text(pack)

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
  <form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}
    <input type=submit class="btn btn-primary" value="submit">
|]
    $(widgetFile "develTable")

postDevelR :: Handler Html
postDevelR = error "Not yet implemented: postDevelR"
