module Handler.Recipe where

import Import

getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildPosted]
  defaultLayout $ do
    setTitle "recipe"
    [whamlet|
$if null recipes
  <p>レシピはありません
$else
  <ul>
    $forall Entity shipbuildId shipbuild <- recipes
      <li>
        #{show $ shipbuildPosted shipbuild}
|]

postRecipeR :: Handler Html
postRecipeR = error "Not yet implemented: postRecipeR"
