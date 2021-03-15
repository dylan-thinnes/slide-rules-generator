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
import qualified Diagrams.TwoD.Text as D

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
import System.IO.Unsafe
import System.Random

import Debug.Trace

main = error "hmm"

-- utils
dunit :: Applicative m => m ()
dunit = pure ()

elseUnit :: Monad m => Bool -> m a -> m ()
elseUnit b action = if b then action >> dunit else dunit

e :: Double
e = sum $ map (recip . fromIntegral . fac) [0..17]
    where
        fac :: Integer -> Integer
        fac n = product ([2..n] :: [Integer])

-- Annotations
data Ann t = Ann { ann :: String, val :: t }
instance Show (Ann t) where
    show (Ann s _) = s

data Transform1
    = Offset Double
    | Scale Double
    | Log Double
    | LogLog Double
    deriving Show

newtype Transform = Transform [Transform1]
    deriving Show

runGenTransform :: Transform -> Double -> Double
runGenTransform (Transform ones) = appEndo $ foldMap (Endo . runGenTransform1) $ reverse ones

runGenTransform1 :: Transform1 -> Double -> Double
runGenTransform1 (Offset offset) x = offset + x
runGenTransform1 (Scale scale) x = scale * x
runGenTransform1 (Log base) x = logBase base x

pushTransform :: Transform1 -> Transform -> Transform
pushTransform t1 (Transform ts) = Transform (t1 : ts)

data Tick meta = Tick { prePos :: Double, postPos :: Double, tickMeta :: meta }
    deriving (Show)

data GenState meta = GenState
    { _preTrans :: Transform
    , _postTrans :: Transform
    , _out :: S.Seq (Tick meta)
    }
    deriving (Show)

makeLenses ''GenState

type Gen meta = ListT (State (GenState meta))
runGen :: Gen meta a -> GenState meta
runGen gen = execState (runListT gen) (GenState (Transform []) (Transform []) [])

list :: [a] -> Gen meta a
list xs = ListT $ pure xs

preTransformG, postTransformG :: Double -> Gen meta Double
preTransformG x = gets (runGenTransform . _preTrans) <&> ($ x)
postTransformG x = gets (runGenTransform . _postTrans) <&> ($ x)

transformG :: Double -> Gen meta (Double, Double)
transformG x = do
    pre <- preTransformG x
    post <- postTransformG pre
    pure (pre, post)

measureG :: Double -> Double -> Gen meta Double
measureG a b = do
    (_, a') <- transformG a
    (_, b') <- transformG b
    pure $ b' - a'

save :: meta -> Double -> Gen meta Double
save tickMeta x = do
    (prePos, postPos) <- transformG x
    out <>= [Tick {..}]
    pure prePos

savePre :: (Double -> meta) -> Double -> Gen meta Double
savePre tickMetaF x = do
    tickMeta <- tickMetaF <$> preTransformG x
    save tickMeta x

zoom :: Double -> Double -> Gen meta a -> Gen meta a
zoom o s = offset o . scale s

offset, scale :: Double -> Gen meta a -> Gen meta a
offset offset = preZoomG (Offset offset)
scale scale = preZoomG (Scale scale)
logGen base = preZoomG (Log base)

preZoomG, postZoomG :: Transform1 -> Gen meta a -> Gen meta a
preZoomG transformation gen = do
    oldState <- use preTrans
    preTrans %= pushTransform transformation
    a <- gen
    preTrans .= oldState
    pure a
postZoomG transformation gen = do
    oldState <- use postTrans
    postTrans %= pushTransform transformation
    a <- gen
    postTrans .= oldState
    pure a

data TextPosition = Above Double | Below Double | Alongside Double
    deriving Show

data Meta = Meta { _start :: Double, _end :: Double, _mtext :: Maybe (TextPosition, Double, String) }
    deriving Show
makeLenses ''Meta

defMeta :: Meta
defMeta = Meta { _start = 0, _end = 1, _mtext = Nothing }

meta :: Double -> Double -> Maybe String -> Meta
meta s e mt = Meta s e ((Above 0, 0.3,) <$> mt)

meta0 :: Double -> Maybe String -> Meta
meta0 = meta 0
meta0J :: Double -> String -> Meta
meta0J h t = meta0 h (Just t)
meta0N :: Double -> Meta
meta0N h = meta0 h Nothing

powC :: Int -> Gen Meta ()
powC n =
    postZoomG (Log $ 10 ** fromIntegral n) $ do
        save (meta0J 1 "1") (10 ** fromIntegral n)
        p <- list [0..n-1]
        postZoomG (Scale $ 10 ** fromIntegral p) $ do
            logScale

logScale :: Gen Meta ()
logScale = do
    save (Meta 0.6 0.7 $ Just (Above (-0.05), 0.25, "π")) pi
    save (Meta 0.6 0.7 $ Just (Above (-0.05), 0.25, "e")) e
    x <- list [1..9]
    save (meta0J 1 (show x)) (fromIntegral x)
    offset (fromIntegral x) $ tryPartitions 0.0025 (False, False)
        [[(2, const $ Meta 0 0.7 $ Just (Above (-0.05), 0.25, ".5")), (5, const $ meta0N 0.5), (5, const $ meta0N 0.25)]
        ,[(2, const $ Meta 0 0.7 $ Just (Above (-0.05), 0.25, ".5")), (5, const $ meta0N 0.5), (2, const $ meta0N 0.3)]
        ,[(2, const $ Meta 0 0.7 $ Just (Above (-0.05), 0.25, ".5")), (5, const $ meta0N 0.5)]
        ,[(5, const $ meta0N 0.5)]
        ,[(2, const $ meta0N 0.5),(2, const $ meta0N 0.3)]
        ,[(2, const $ meta0N 0.5)]
        ]

partition :: (Bool, Bool) -> [(Integer, Double -> Meta)] -> Gen Meta ()
partition (tickStart, tickEnd) [] = pure ()
partition (tickStart, tickEnd) (curr:rest) = do
    let (n, metaF) = curr
    elseUnit tickEnd $ savePre metaF (fromIntegral n)
    x <- list [0..n-1]
    zoom 0 (1 / fromIntegral n) $ do
        elseUnit (tickStart || x /= 0) $ savePre metaF (fromIntegral x)
        zoom (fromIntegral x) 1 $ partition (False, False) rest

tryPartitions :: Double -> (Bool, Bool) -> [[(Integer, Double -> Meta)]] -> Gen Meta ()
tryPartitions threshold startEnd [] = pure ()
tryPartitions threshold startEnd (partitionSpec:specs) = do
    minPartitionWidth <- minPartitionWidth partitionSpec
    if minPartitionWidth > threshold
       then partition startEnd partitionSpec
       else tryPartitions threshold startEnd specs

minPartitionWidth :: [(Integer, Double -> Meta)] -> Gen meta Double
minPartitionWidth xs = do
    let sliceWidth = product $ map fst xs
        b = 1
        a = b - recip (fromIntegral sliceWidth)
    measureG a b

-- Rendering
total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 [a, b, c]

a, b, c :: D.Diagram D.B
a = foldMap (renderTick False) $ toList $ _out $ runGen $ powC 1
b = foldMap (renderTick False) $ toList $ _out $ runGen $ powC 2
c = foldMap (renderTick False) $ toList $ _out $ runGen $ powC 3

renderTick :: Bool -> Tick Meta -> D.Diagram D.B
renderTick above Tick { prePos, postPos, tickMeta = meta } =
    let tickVec   = D.r2 (0, 0.04 * (meta^.end - meta^.start))
        tickStart = D.r2 (0, 0.04 * meta^.start)
        tickEnd   = D.r2 (0, 0.04 * meta^.end)
        origTick = laserline [tickVec]
                 & D.translate tickStart
        withLabel =
            case meta^.mtext  of
              Nothing -> origTick
              Just (position, fontSize, text) ->
                  let labelOffset =
                          case position of
                            Above spacing     -> tickEnd + D.r2 (0, 0.04 * spacing)
                            Below spacing     -> tickStart - D.r2 (0, 0.04 * spacing)
                            Alongside spacing -> tickEnd + D.r2 (spacing, 0)
                      (labelAlignmentX, labelAlignmentY) =
                          case position of
                            Above _     -> (0.5, 0)
                            Below _     -> (0.5, 1)
                            Alongside _ -> (0, 0.5)
                      label
                        = D.alignedText labelAlignmentX labelAlignmentY text
                        & D.fontSizeL (0.04 * fontSize) & D.fc D.black & D.font "monospace"
                        & D.translate labelOffset
                   in label <> origTick
     in D.translate (D.r2 (postPos, 0)) withLabel

laserline :: [D.V2 Double] -> D.Diagram D.B
laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin & D.lc D.black
