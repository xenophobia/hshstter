{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module THUtility where
import Language.Haskell.TH
import Control.Monad
import Control.Applicative

mapT :: Int -> ExpQ -> ExpQ
mapT count f = do
  vars <- replicateM count $ newName "x"
  lamE [tupP $ map varP vars] (tupE $ map (appE f . varE) vars)

mapMT :: Int -> ExpQ -> ExpQ
mapMT count f = do
  vars <- replicateM count $ newName "x"
  let ops = '(<$>):repeat '(<*>)
  lamE [tupP $ map varP vars] $ foldl (\x -> \(op, y) -> (appE (appE (varE op) x) (appE f (varE y)))) (conE $ tupleDataName count) (zip ops vars)