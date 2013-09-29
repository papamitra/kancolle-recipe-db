-- -*- coding:utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}

module Handler.Recipe where

import Import

import Data.Text(pack, unpack)
import Yesod.Form.Fields
import Data.Time
import Text.Printf
import Data.Maybe
import Data.Traversable
import Text.Read
import qualified Control.Monad as M
import Control.Monad(forM_)
import Data.String(fromString)

maybeRead = fmap fst . listToMaybe .reads

getRecipeR :: Handler Html
getRecipeR = do
  recipes <- runDB $ selectList [] [Desc ShipbuildviewPosted]
  sess <- (lookupSession (pack "recipe"))
  let recipeSess = sess >>= (maybeRead . unpack) :: Maybe Recipe
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

threesome :: [a] -> [[a]]
threesome [] = []
threesome xs = let (ys, rest) = splitAt 3 xs
               in (ys : (threesome rest))
                  
data Recipe = Recipe {hqLv::Int,
                      secId::ShipId,
                      secLv::Int,
                      fuel::Int,
                      amm::Int,
                      steel::Int,
                      baux::Int,
                      shipIds::[ShipId]} deriving (Show, Read)
recipeForm :: Maybe Recipe -> Html -> MForm Handler (FormResult Recipe, Widget)
recipeForm recipe extra = do
             (vHqLv, fHqLv) <- mreq intField "司令Lv" (fmap hqLv recipe)
             (vSecId, fSecId) <- mreq (selectField shipList) "秘書艦" (fmap secId recipe)
             (vSecLv, fSecLv) <- mreq intField "秘書艦Lv" (fmap secLv recipe)
             (vFuel, fFuel) <- mreq intField "燃料" (fmap fuel recipe)
             (vAmm, fAmm) <- mreq intField "弾薬" (fmap amm recipe)
             (vSteel, fSteel) <- mreq intField "鋼材" (fmap steel recipe)
             (vBaux, fBaux) <- mreq intField "ボーキサイト" (fmap baux recipe)
             ships <- M.sequence [mopt (selectField shipList) (fromString s) Nothing | i <- [1..6], let s = printf "建造%s" (show i)] -- printf "%d" i だとうまくいかない.
             let ships3 = threesome ships
             let inputValue = Recipe <$> vHqLv <*> vSecId <*> vSecLv <*> vFuel <*> vAmm <*> vSteel <*> vBaux <*> (catMaybes <$> sequenceA (map fst ships))

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

                    <div .control-group>
                      <label .control-label>#{fvLabel fSecId}(Lv)
                      <div .controls>^{fvInput fSecId}
                        ^{fvInput fSecLv}

                    <div .row>
                      $forall ships <- ships3
                        <div .span3>
                          $forall (_, fShipId) <- ships
                            <div .controls>^{fvInput fShipId}
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
      forM_ (shipIds recipe) $ \shipId ->
        runDB $ insert $ Shipbuild (Key $ PersistInt64 1) shipId time (secId recipe) (secLv recipe) (hqLv recipe) resourceId
      setSession (pack "recipe") (pack $ show recipe)
      return ()
    _ -> error "error"
  redirect RecipeR
