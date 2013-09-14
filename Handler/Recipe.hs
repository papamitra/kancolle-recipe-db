{-# LANGUAGE OverloadedStrings #-}

module Handler.Recipe where

import Data.Text(pack)

import Import

getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildPosted]
  ships <- runDB $ selectList [] [Asc ShipId]
  let shipList = [(pack $ shipName ship, shipid)| Entity shipid ship <- ships]
  (widget, enctype) <- generateFormPost $ recipeForm shipList
  defaultLayout $ do
    setTitle "recipe"
    [whamlet|
$if null recipes
  <p>レシピはありません
    <form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}

$else
  <ul>
    $forall Entity shipbuildId shipbuild <- recipes
      <li>
        #{show $ shipbuildPosted shipbuild}

  <form method=post action=@{RecipeR} entype=#{enctype}>
    ^{widget}

|]

data Recipe = Recipe Int ShipId
recipeForm :: [(Text, Key Ship)] -> Html -> MForm Handler (FormResult Recipe, Widget)
recipeForm shipList = renderDivs $ Recipe
             <$> areq intField "shipId" Nothing
             <*> areq (selectFieldList shipList) "Ship" Nothing

postRecipeR :: Handler Html
postRecipeR = error "Not yet implemented: postRecipeR"
