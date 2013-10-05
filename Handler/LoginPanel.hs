module Handler.LoginPanel where

import Import
import Yesod.Auth.OAuth(twitterUrl)

getLoginPanelR :: Handler Html
getLoginPanelR = defaultLayout $(widgetFile "login")
