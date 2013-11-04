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
    [whamlet|
  <div .page-header>
    <h2>開発 #{equipmentName equip}
    <table .table>
      <thead>
        <th scope="col">レシピ
        <th scope="col">出現数
        <th scope="col">出現確率
      <tbody>
        $forall Entity equipmentviewId recp <- recipes
          <tr>
            <td>#{show $ equipmentviewFuel recp}/#{show $ equipmentviewAmm recp}/#{show $ equipmentviewSteel recp}/#{show $ equipmentviewBaux recp}
            <td>#{show $ equipmentviewCount recp}
            <td>#{show $ ((*) 100 ((/) (fromIntegral (equipmentviewCount recp)) (fromIntegral (equipmentviewTotal recp)))) } %
|]

