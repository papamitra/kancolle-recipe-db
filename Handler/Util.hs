{-# LANGUAGE OverloadedStrings #-}

module Handler.Util where

import Import

import Data.Text(pack)
import qualified Control.Monad as M
import Data.Traversable
import Data.Maybe
import Text.Printf
import Data.String(fromString)

threesome :: [a] -> [[a]]
threesome [] = []
threesome xs = let (ys, rest) = splitAt 3 xs
               in (ys : (threesome rest))

shipList = do
  ships <- runDB $ selectList [] [Asc ShipId]
  optionsPairs [(pack $ shipName ship, shipid)| Entity shipid ship <- ships]

-- 建造 or 開発のレシピ
data Recipe s = Recipe {hqLv::Int,
                      secId::ShipId,
                      secLv::Int,
                      fuel::Int,
                      amm::Int,
                      steel::Int,
                      baux::Int,
                      createdIds::[s]} deriving (Show, Read)

recipeForm :: (PersistEntity m) => Maybe (Recipe (Key m)) -> Handler (OptionList (Key m)) -> Html -> MForm Handler (FormResult (Recipe (Key m)), Widget)
recipeForm recipe createds extra = do
             (vHqLv, fHqLv) <- mreq intField "司令Lv" (fmap hqLv recipe)
             (vSecId, fSecId) <- mreq (selectField shipList) "秘書艦" (fmap secId recipe)
             (vSecLv, fSecLv) <- mreq intField "秘書艦Lv" (fmap secLv recipe)
             (vFuel, fFuel) <- mreq intField "燃料" (fmap fuel recipe)
             (vAmm, fAmm) <- mreq intField "弾薬" (fmap amm recipe)
             (vSteel, fSteel) <- mreq intField "鋼材" (fmap steel recipe)
             (vBaux, fBaux) <- mreq intField "ボーキサイト" (fmap baux recipe)
             cs <- M.sequence [mopt (selectField createds) (fromString s) Nothing | i <- [1..6], let s = printf "%s" (show i)] -- printf "%d" i だとうまくいかない.
             let cs3 = threesome cs
             let inputValue = Recipe <$> vHqLv <*> vSecId <*> vSecLv <*> vFuel <*> vAmm <*> vSteel <*> vBaux <*> (catMaybes <$> sequenceA (map fst cs))

             let widget = do
                   [whamlet|
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
                      $forall cs <- cs3
                        <div .span3>
                          $forall (_, fCreatedId) <- cs
                            <div .controls>^{fvInput fCreatedId}
                   |]
             return (inputValue, widget)
