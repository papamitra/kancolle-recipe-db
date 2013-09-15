{-# LANGUAGE OverloadedStrings #-}

module Handler.Recipe where

import Data.Text(pack)

import Import

import Yesod.Form.Fields

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

data Recipe = Recipe Int ShipId deriving (Show)
recipeForm :: Html -> MForm Handler (FormResult Recipe, Widget)
recipeForm = renderDivs $ Recipe
             <$> areq intField "shipId" Nothing
             <*> areq (selectField shipList) "Ship" Nothing
  where
    shipList = do
      ships <- runDB $ selectList [] [Asc ShipId]
      optionsPairs [(pack $ shipName ship, shipid)| Entity shipid ship <- ships]

          
postRecipeR :: Handler Html
postRecipeR = do
  ((result, widget), enctype) <- runFormPost $ recipeForm
  case result of
    FormSuccess recipe -> liftIO $ print (show recipe)
    _ -> error "error"
  redirect RecipeR
