{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Main where

import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.TwoD.Vector as D

import qualified Data.Sequence as S
import Text.Printf

import GHC.TypeLits

import Control.Monad.List
import Control.Monad.Writer
import Control.Monad.State

import Control.Lens hiding (zoom, transform)
import Control.Lens.TH

import Control.Monad.Extra

import Data.Foldable (toList)

import Debug.Trace

main = error "hmm"

-- utils
dunit :: Applicative m => m ()
dunit = pure ()

elseDef :: Applicative m => Bool -> a -> m a -> m a
elseDef b def action = if b then action else pure def

elseUnit :: Applicative m => Bool -> m () -> m ()
elseUnit b = elseDef b ()

leftEndo :: [Endo a] -> a -> a
leftEndo = appEndo . foldl (<>) mempty

--example = lwG 0.05 . mconcat . map fromOffsets
--         $ [ [r *^ e (r @@ rad)]
--           | r <- [33 * tau/32, 34 * tau/32 .. 2 * tau]
--           ]

-- Annotations
data Ann t = Ann { ann :: String, val :: t }
instance Show (Ann t) where
    show (Ann s _) = s

-- Point generators
data PG
  = Single Double
  | Join (S.Seq PG) -- combine tick generators, superimpose
  | IterateN Int (Ann (Int -> Double -> Double)) PG
  | OffsetN Int Double PG
  | PowN Int Double PG
  | PowNCap Int Double PG
  | ScaleN Double PG
  | OffsetFrom PG (Ann (Double -> PG))
  deriving Show

--run :: PG -> S.Seq Double
--run (Single x) = [x]
--run (Join xs) = foldMap run xs
--run (IterateN n f gen) = do
--    i <- [0..n-1]
--    x <- run gen
--    pure $ val f i x
--run (OffsetN n step gen) =
--    let f = \i x -> x + step * fromIntegral i
--     in run $ IterateN n (Ann "OffsetN" f) gen
--run (PowN n base gen) =
--    let f = \p x -> x * base ** fromIntegral p
--     in run $ IterateN n (Ann "PowN" f) gen
--run (PowNCap n base gen) =
--    run $ Join [PowN n base gen, Single $ base ** fromIntegral n]
--run (ScaleN factor gen) =
--    (factor *) <$> run gen
--run (OffsetFrom gen f) = do
--    x <- run gen
--    [x] <> run (val f x)
--
--pg1 = OffsetN 10 1 (Single 1)

data Transform1
    = Translate { _gtrOffset :: Double, _gtrScale :: Double }
    deriving Show

type GenTrans = Transform1
newtype Transform = Transform [Transform1]

runGenTransform1 :: Transform1 -> Double -> Double
runGenTransform1 Translate {..} x = _gtrOffset + _gtrScale * x
runGenTransform :: Transform -> Double -> Double
runGenTransform (Transform ones) = leftEndo (map (Endo . runGenTransform1) ones)

pushTransform :: Transform1 -> Transform -> Transform
pushTransform t1 (Transform ts) = Transform (t1 : ts)

popTransform :: Transform -> Transform
popTransform (Transform []) = Transform []
popTransform (Transform (x:xs)) = Transform xs

transform :: GenTrans -> GenTrans -> GenTrans
transform sub original =
    Translate
        { _gtrOffset = _gtrOffset original +  _gtrScale original * _gtrOffset sub
        , _gtrScale = _gtrScale original * _gtrScale sub
        }

data GenState meta = GenState { _currTrans :: GenTrans, _out :: S.Seq (Double, meta) }
    deriving (Show)

makeLenses ''GenState

type Gen meta = ListT (State (GenState meta))
runGen :: Gen meta a -> GenState meta
runGen gen = execState (runListT gen) (GenState (Translate 0 1) [])

list :: [a] -> Gen meta a
list xs = ListT $ pure xs

save :: meta -> Double -> Gen meta ()
save meta x = do
    Translate {..} <- gets _currTrans
    let res = _gtrOffset + _gtrScale * x
    out <>= [(res, meta)]

zoom :: Double -> Double -> Gen meta a -> Gen meta a
zoom o s = zoomG (Translate o s)

offset, scale :: Double -> Gen meta a -> Gen meta a
offset o = zoom o 1
scale = zoom 0

zoomG :: GenTrans -> Gen meta a -> Gen meta a
zoomG transformation gen = do
    parentState <- gets _currTrans
    currTrans %= transform transformation
    a <- gen
    currTrans .= parentState
    pure a

data Meta = Meta { height :: Double, mtext :: Maybe String }
    deriving Show
meta :: Double -> String -> Meta
meta h t = Meta h (Just t)
metaN :: Double -> Meta
metaN h = Meta h Nothing

powC :: Int -> Gen Meta ()
powC n = do
    save (meta 1 "1") (10 ** fromIntegral n)
    p <- list [0..n-1]
    zoom 0 (10 ** fromIntegral p) c

c :: Gen Meta ()
c = do
    save (meta 0.1 "π") pi
    x <- list [1..9]
    save (meta 1 (show x)) (fromIntegral x)
    offset (fromIntegral x) $
        if | x <= 3    -> partition [(2,0.7), (5,0.5)]
           | x <= 6    -> partition [(5,0.5)]
           | x <= 9    -> partition [(2,0.5)]
           | otherwise -> pure ()

partition :: [(Int, Double)] -> Gen Meta ()
partition [] = pure ()
partition (curr:rest) = do
    let (n, hgt) = curr
    x <- list [0..n-1]
    zoom 0 (1 / fromIntegral n) $ do
        elseUnit (x /= 0) $ save (metaN hgt) (fromIntegral x)
        zoom (fromIntegral x) 1 $ partition rest

-- Rendering
total = D.bgFrame 0.1 D.white $ D.vcat [a]

a :: D.Diagram D.B
a = foldMap (mkTick False) $ toList $ _out $ runGen $ powC 4

mkTick :: Bool -> (Double, Meta) -> D.Diagram D.B
mkTick above (inp, meta) =
    let x = logBase 1000 inp
        origTick = laserline [D.r2 (0, 0.04 * height meta)]
        withLabel =
            case mtext meta of
              Nothing -> origTick
              Just text ->
                  let label
                        = D.alignedText 0.5 (if above then 0.0 else 1.0) text
                        & D.fontSizeL 0.015 & D.fc D.black & D.font "monospace"
                   in D.vsep 0.005 $ (if above then id else reverse) [label, origTick]
     in D.translate (D.r2 (x, 0)) withLabel

laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin & D.lc D.black
