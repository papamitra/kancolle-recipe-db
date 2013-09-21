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
    <input type=submit value="submit">
$if null recipes
  <p>レシピはありません
$else
  <ul>
    $forall Entity shipbuildviewId shipbuild <- recipes
      <li>
        #{shipbuildviewShipname shipbuild}

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
recipeForm recipe = renderDivs $ Recipe
             <$> areq intField "司令Lv" (fmap hqLv recipe)
             <*> areq (selectField shipList) "秘書艦" (fmap secId recipe)
             <*> areq intField "秘書艦Lv" (fmap secLv recipe)
             <*> areq intField "燃料" (fmap fuel recipe)
             <*> areq intField "弾薬" (fmap amm recipe)
             <*> areq intField "鋼材" (fmap steel recipe)
             <*> areq intField "ボーキサイト" (fmap baux recipe)
             <*> areq (selectField shipList) "建造1" Nothing
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
