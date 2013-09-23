{-# LANGUAGE OverloadedStrings #-}

module Handler.Recipe where

import Import

import Data.Text(pack, unpack)
import Yesod.Form.Fields
import Data.Time
  
getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildviewPosted]
  sess <- (lookupSession (pack "recipe"))
  let recipeSess = sess >>= (return . read . unpack) :: Maybe Recipe
  (widget, enctype) <- generateFormPost $ recipeForm recipeSess
  defaultLayout $ do
    setTitle "recipe"
    [whamlet|
<form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}
    <input type=submit class="btn btn-primary" value="submit">
$if null recipes
  <p>レシピはありません
$else
  <table class="table">
    <thead>
      <tr>
        <th scope="col">レシピ
        <th scope="col">建造艦
        <th scope="col">司令Lv
        <th scope="col">秘書艦(Lv)
    <tbody>
    $forall Entity shipbuildviewId shipbuild <- recipes
      <tr>
        <td>#{show $ shipbuildviewFuel shipbuild}/#{show $ shipbuildviewAmm shipbuild}/#{show $ shipbuildviewSteel shipbuild}/#{show $ shipbuildviewBaux shipbuild}
        <td>#{shipbuildviewShipname shipbuild}
        <td>#{shipbuildviewHqlv shipbuild}
        <td>#{shipbuildviewSecname shipbuild}(#{shipbuildviewSeclv shipbuild})
|]

data Recipe = Recipe {hqLv::Int,
                      secId::ShipId,
                      secLv::Int,
                      fuel::Int,
                      amm::Int,
                      steel::Int,
                      baux::Int,
                      shipId::ShipId} deriving (Show, Read)
recipeForm :: Maybe Recipe -> Html -> MForm Handler (FormResult Recipe, Widget)
recipeForm recipe extra = do
             (vHqLv, fHqLv) <- mreq intField "司令Lv" (fmap hqLv recipe)
             (vSecId, fSecId) <- mreq (selectField shipList) "秘書艦" (fmap secId recipe)
             (vSecLv, fSecId) <- mreq intField "秘書艦Lv" (fmap secLv recipe)
             (vFuel, fFuel) <- mreq intField "燃料" (fmap fuel recipe)
             (vAmm, fAmm) <- mreq intField "弾薬" (fmap amm recipe)
             (vSteel, fSteel) <- mreq intField "鋼材" (fmap steel recipe)
             (vBaux, fBaux) <- mreq intField "ボーキサイト" (fmap baux recipe)
             (vShipId, fShipId) <- mreq (selectField shipList) "建造1" Nothing
             let inputValue = Recipe <$> vHqLv <*> vSecId <*> vSecLv <*> vFuel <*> vAmm <*> vSteel <*> vBaux <*> vShipId
             let widget = do
                   [whamlet|
                    #{extra}
                    <div .control-group>
                      <label .control-label>#{fvLabel fHqLv}
                      <div .controls>^{fvInput fHqLv}
                    <div .row>
                      <div .span3>
                        <label .control-label>#{fvLabel fFuel}
                        <div .controls>^{fvInput fFuel}

                        <label .control-label>#{fvLabel fAmm}
                        <div .controls>^{fvInput fAmm}

                      <div .span3>
                        <label .control-label>#{fvLabel fSteel}
                        <div .controls>^{fvInput fSteel}

                        <label .control-label>#{fvLabel fBaux}
                        <div .controls>^{fvInput fBaux}

                   |]
             return (inputValue, widget)
  where
    shipList = do
      ships <- runDB $ selectList [] [Asc ShipId]
      optionsPairs [(pack $ shipName ship, shipid)| Entity shipid ship <- ships]

          
postRecipeR :: Handler Html
postRecipeR = do
  ((result, widget), enctype) <- runFormPost $ recipeForm Nothing
  case result of
    FormSuccess recipe -> do
      time <- liftIO getCurrentTime
      res <- runDB $ insertBy $ Resource (fuel recipe) (amm recipe) (steel recipe) (baux recipe)
      let resourceId = either entityKey id res
      _ <- runDB $ insert $ Shipbuild (Key $ PersistInt64 1) (shipId recipe) time (secId recipe) (secLv recipe) (hqLv recipe) resourceId -- fixme
      setSession (pack "recipe") (pack $ show recipe)
      return ()
    _ -> error "error"
  redirect RecipeR
