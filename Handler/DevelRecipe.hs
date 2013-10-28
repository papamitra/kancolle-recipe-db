module Handler.DevelRecipe where

import Import

getDevelRecipeR :: Int -> Int -> Int -> Int -> Handler Html
getDevelRecipeR fuel amm steel baux = do
  Entity resourceId _ <- runDB $ getBy404 $ UniqueResource fuel amm steel baux
  recipes <- runDB $ selectList [DeveloprecipeviewResourceid ==. resourceId] [Desc DeveloprecipeviewEquipcount]
  muser <- maybeAuth
  defaultLayout $ do
    setTitle "開発レシピ"
    $(widgetFile "navbar")
    recipeTable recipes
  where recipeTable recipes = let recipecount = foldl (+) 0 $ map (\(Entity _ recp) -> developrecipeviewEquipcount recp)recipes
                              in [whamlet|
  <div .page-header>
    <h2>開発 #{show fuel}/#{show amm}/#{show steel}/#{show baux}
    <table .table>
      <thead>
        <th scope="col">装備名
        <th scope="col">出現数
        <th scope="col">出現確率
      <tbody>
        $forall Entity developrecipeviewId recp <- recipes
          <tr>
            <td>#{developrecipeviewEquipname recp}
            <td>#{show $ developrecipeviewEquipcount recp}
            <td>#{show $ ((*) 100 ((/) (fromIntegral (developrecipeviewEquipcount recp)) (fromIntegral recipecount))) } %
|]

      
