module Handler.Devel where

import Import

import Yesod.Auth

getDevelR :: Handler Html
getDevelR = do
  recipes <- runDB $ selectList [] [Desc DevelopviewPosted]
  muser <- maybeAuth
  defaultLayout $ do
    setTitle "開発"
    $(widgetFile "navbar")
    $(widgetFile "develTable")

postDevelR :: Handler Html
postDevelR = error "Not yet implemented: postDevelR"
