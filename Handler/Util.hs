{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Handler.Util where

import Import

import Data.Text(pack)
import qualified Control.Monad as M
import Data.Traversable
import Data.Maybe
import Text.Printf
import Data.String(fromString)
import Control.Monad(forM)

maybeRead = fmap fst . listToMaybe .reads

threesome :: [a] -> [[a]]
threesome [] = []
threesome xs = let (ys, rest) = splitAt 3 xs
               in (ys : (threesome rest))

shipList :: Handler (OptionList ShipId)
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

resourceFormSettings label = FieldSettings (fromString label) Nothing Nothing Nothing [("class", "form-control")]

recipeForm :: (PersistEntity m) => Maybe (Recipe (Key m)) -> (OptionList (Key m)) -> Html -> MForm Handler (FormResult (Recipe (Key m)), Widget)
recipeForm recipe createds extra = do
             (vHqLv, fHqLv) <- mreq intField (FieldSettings (fromString "司令Lv") Nothing Nothing Nothing [("class", "form-control")]) (fmap hqLv recipe)
             (vSecId, fSecId) <- mreq (selectField shipList) (FieldSettings (fromString "秘書艦") Nothing Nothing Nothing [("class", "form-control")]) (fmap secId recipe)
             (vSecLv, fSecLv) <- mreq intField (FieldSettings (fromString "秘書艦Lv") Nothing Nothing Nothing [("class", "form-control")]) (fmap secLv recipe)
             (vFuel, fFuel) <- mreq intField (resourceFormSettings "燃料") (fmap fuel recipe)
             (vAmm, fAmm) <- mreq intField (resourceFormSettings "弾薬") (fmap amm recipe)
             (vSteel, fSteel) <- mreq intField (resourceFormSettings "鋼材") (fmap steel recipe)
             (vBaux, fBaux) <- mreq intField (resourceFormSettings "ボーキサイト") (fmap baux recipe)
             cs <- M.sequence [mopt (selectField (return createds)) (FieldSettings (fromString s) Nothing Nothing Nothing [("class", "form-control")]) Nothing | i <- [1..6], let s = printf "%s" (show i)] -- printf "%d" i だとうまくいかない
             let cs3 = threesome cs
             let inputValue = Recipe <$> vHqLv <*> vSecId <*> vSecLv <*> vFuel <*> vAmm <*> vSteel <*> vBaux <*> (catMaybes <$> sequenceA (map fst cs))

             let widget = do
                   [whamlet|
                  #{extra}
                  <div .form-group>
                    <div .row>
                      <label .control-label .col-md-2 for=##{fvId fHqLv}>
                        #{fvLabel fHqLv}
                      <div .col-md-3>
                        ^{fvInput fHqLv}

                  <div .form-group>
                    <div .row>
                      <label .control-label .col-md-2 for=##{fvId fSecId}>
                        #{fvLabel fSecId}
                      <div .col-md-3>
                        ^{fvInput fSecId}
                      <div .input-group .col-md-3>
                        <span .input-group-addon>
                          Lv
                        ^{fvInput fSecLv}
                      <div .col-md-4>

                  <div .form-group>
                    <div .row>
                      <div .form-inline>
                        <div .input-group .col-md-3>
                          <span .input-group-addon>燃</span>^{fvInput fFuel}
                        <div .input-group .col-md-3>
                          <span .input-group-addon>弾</span>^{fvInput fAmm}
                        <div .input-group .col-md-3>
                          <span .input-group-addon>鋼</span>^{fvInput fSteel}
                        <div .input-group .col-md-3>
                          <span .input-group-addon>ボ</span>^{fvInput fBaux}

                  <div .form-group>
                    <div .row>
                      $forall cs <- cs3
                        <div .col-md-4>
                          $forall (_, fCreatedId) <- cs
                            <div .form-group>^{fvInput fCreatedId}
                   |]
             return (inputValue, widget)
