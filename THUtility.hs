{-# LANGUAGE TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction #-}
module THUtility where
import Language.Haskell.TH
import Control.Monad
import Data.IORef
import Graphics.UI.Gtk hiding (add)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Glade
import Control.Applicative
import Control.Arrow

mapT :: Int -> ExpQ -> ExpQ
mapT count f = do
  vars <- replicateM count $ newName "x"
  lamE [tupP $ map varP vars] (tupE $ map (appE f . varE) vars)

mapMT :: Int -> ExpQ -> ExpQ
mapMT count f = do
  vars <- replicateM count $ newName "x"
  let ops = '(<$>):repeat '(<*>)
  lamE [tupP $ map varP vars] $ foldl (\x -> \(op, y) -> (appE (appE (varE op) x) (appE f (varE y)))) (conE $ tupleDataName count) (zip ops vars)

-- GUI型からGUIウィジェット読込コードを生成
castToGUI :: Name -> ExpQ
castToGUI dataName = do
  TyConI (DataD _ _ _ [RecC constructor fieldsData] _) <- reify dataName
  xml <- newName "xml"
  let fields = map (\(a, _, ConT b) -> (nameBase a, mkName $ "castTo" ++ nameBase b)) fieldsData
      binds = map (\(name, castFunc) -> bindS (varP . mkName $ name) (appE (appE (appE (varE 'xmlGetWidget) (varE xml)) (varE castFunc)) (litE (stringL name)))) fields
      ret = noBindS $ appE (varE 'return) $ foldl (\c v -> appE c (varE . mkName . fst $ v)) (conE constructor) fields
  lamE [varP xml] (doE $ binds ++ [ret])