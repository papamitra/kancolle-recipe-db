module Handler.Recipe where

import Import

getRecipeR :: Handler Html
getRecipeR = do
  (widget, enctype) <- generateFormPost recipeForm
  recipes <- runDB $ selectList [] [Desc ShipbuildPosted]
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

data Recipe = Recipe Int 
recipeForm = renderDivs $ Recipe
             <$> areq intField "shipId" Nothing

postRecipeR :: Handler Html
postRecipeR = error "Not yet implemented: postRecipeR"
