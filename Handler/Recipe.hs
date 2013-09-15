{-# LANGUAGE OverloadedStrings #-}

module Handler.Recipe where

import Import

import Data.Text(pack)
import Yesod.Form.Fields
import Data.Time

getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildPosted]
  (widget, enctype) <- generateFormPost recipeForm
  defaultLayout $ do
    setTitle "recipe"
    [whamlet|
$if null recipes
  <p>レシピはありません
    <form method=post action=@{RecipeR} entype=#{enctype}>
      <fieldset>
        <legend>Recipe
        ^{widget}
        <input type=submit value="Submit">

$else
  <ul>
    $forall Entity shipbuildId shipbuild <- recipes
      <li>
        #{show $ shipbuildPosted shipbuild}

  <form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}
    <input type=submit value="submit">

|]

data Recipe = Recipe Int ShipId Int Int Int Int Int ShipId deriving (Show)
recipeForm :: Html -> MForm Handler (FormResult Recipe, Widget)
recipeForm = renderDivs $ Recipe
             <$> areq intField "鎮守府Lv" Nothing
             <*> areq (selectField shipList) "Secretary" Nothing
             <*> areq intField "Secretary Lv" Nothing
             <*> areq intField "燃料" Nothing
             <*> areq intField "弾薬" Nothing
             <*> areq intField "鋼材" Nothing
             <*> areq intField "ボーキサイト" Nothing
             <*> areq (selectField shipList) "Ship" Nothing
  where
    shipList = do
      ships <- runDB $ selectList [] [Asc ShipId]
      optionsPairs [(pack $ shipName ship, shipid)| Entity shipid ship <- ships]

          
postRecipeR :: Handler Html
postRecipeR = do
  ((result, widget), enctype) <- runFormPost $ recipeForm
  case result of
    FormSuccess (Recipe hqLv secId secLv fuel amm steel baux shipId) -> do
      time <- liftIO getCurrentTime
      res <- runDB $ insertBy $ Resource fuel amm steel baux
      let resourceId = either entityKey id res
      _ <- runDB $ insert $ Shipbuild (Key $ PersistInt64 1) shipId time secId secLv hqLv resourceId -- fixme
      return ()
    _ -> error "error"
  redirect RecipeR
