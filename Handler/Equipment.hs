module Handler.Equipment where

import Data.Text
import Import

getEquipmentR :: Text -> Handler Html
getEquipmentR equipName = do
  Entity equipId equip <- runDB $ getBy404 $ UniqueEquipmentName (unpack equipName) :: Handler (Entity Equipment)
  recipes <- runDB $ selectList [EquipmentviewEquipid ==. equipId] [Desc EquipmentviewCount]
  muser <- maybeAuth
  defaultLayout $ do
    setTitle "レシピ"
    $(widgetFile "navbar")
      
