module Handler.DevelRecipeAll where

import Import

getDevelRecipeAllR :: Handler Html
getDevelRecipeAllR = do
  recipes <- runDB $ selectList [] [Desc DevelrecipeallviewCnt]
  muser <- maybeAuth
  defaultLayout $ do
    setTitle "開発レシピ一覧"
    $(widgetFile "navbar")
    [whamlet|
  <div .page-header>
    <h2>開発レシピ一覧
    <table .table>
      <thead>
        <th scope="col">レシピ
        <th scope="col">回数
      <tbody>
        $forall Entity develrecipeallviewId recp <- recipes
          <tr>
            <td>
              <a href=@{DevelRecipeR (fuel recp) (amm recp) (steel recp) (baux recp)}>
                #{recipestr recp}
            <td>#{show $ develrecipeallviewCnt recp}
|]
  where
    fuel = develrecipeallviewFuel
    amm = develrecipeallviewAmm
    steel = develrecipeallviewSteel
    baux = develrecipeallviewBaux
    recipestr recipe = 
      (show $ fuel recipe) ++ "/" ++ (show $ amm recipe ) ++ "/" ++ (show $ steel recipe) ++ "/" ++ (show $ baux recipe)
