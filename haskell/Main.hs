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

import Data.List.Extra (dropWhileEnd)
import Control.Monad.Extra

import Data.Foldable (toList)
import System.IO.Unsafe
import System.Random

import Numeric.Decimal
import qualified Numeric.Decimal.Operation as NDO
import qualified Numeric.Decimal.Arithmetic as NDA

import Data.Either
import Data.Maybe

import Debug.Trace

main = error "hmm"

type InternalFloat = BasicDecimal

-- UTILS
dunit :: Applicative m => m ()
dunit = pure ()

elseUnit :: Monad m => Bool -> m a -> m ()
elseUnit b action = if b then action >> dunit else dunit

e :: Floating a => a
e = sum $ map (recip . fromIntegral . fac) [0..17]
    where
        fac :: Integer -> Integer
        fac n = product ([2..n] :: [Integer])

ll :: Floating a => a -> a -> a
ll n = logBase n . logBase n

reduce :: BasicDecimal -> BasicDecimal
reduce i
  = fromRight (error "BasicDecimal reduction errored out!")
  $ NDA.evalArith (NDO.reduce i) NDA.newContext

-- ANNOTATIONS
data Ann t = Ann { ann :: String, val :: t }
instance Show (Ann t) where
    show (Ann s _) = s

-- TRANSFORMATIONS
data Transform1
    = Offset InternalFloat
    | Scale  InternalFloat
    | Log    InternalFloat
    | LogLog InternalFloat
    | Above  InternalFloat
    | Below  InternalFloat
    deriving Show

newtype Transform = Transform [Transform1]
    deriving Show

runGenTransform :: Transform -> InternalFloat -> Maybe InternalFloat
runGenTransform (Transform ones)
  = appEndo (foldMap (\t -> Endo (>>= runGenTransform1 t)) (reverse ones))
  . Just

runGenTransform1 :: Transform1 -> InternalFloat -> Maybe InternalFloat
runGenTransform1 (Offset offset) x = Just $ offset + x
runGenTransform1 (Scale scale) x = Just $ scale * x
runGenTransform1 (Log base) x = Just $ logBase base x
runGenTransform1 (LogLog base) x = Just $ ll base x
runGenTransform1 (Above lower) x = guard (lower <= x) >> pure x
runGenTransform1 (Below upper) x =
    if upper >= x
       then Just x
       else traceShow ("Can't render", x) Nothing

pushTransform :: Transform1 -> Transform -> Transform
pushTransform t1 (Transform ts) = Transform (t1 : ts)

data Tick meta = Tick
    { prePos :: InternalFloat
    , postPos :: InternalFloat
    , tickMeta :: meta
    }
    deriving (Show)

data GenState meta = GenState
    { _preTrans :: Transform
    , _postTrans :: Transform
    , _out :: S.Seq (Tick meta)
    }
    deriving (Show)

-- "GENERATOR" MONAD TRANSFORMER STACK
type Gen meta = ListT (State (GenState meta))

execGen :: Gen meta a -> GenState meta
execGen gen = execState (runListT gen) (GenState (Transform []) (Transform []) [])

evalGen :: Gen meta a -> [a]
evalGen gen = evalState (runListT gen) (GenState (Transform []) (Transform []) [])

runGen :: Gen meta a -> ([a], GenState meta)
runGen gen = runState (runListT gen) (GenState (Transform []) (Transform []) [])

list :: [a] -> Gen meta a
list xs = ListT $ pure xs

altn :: [Gen meta a] -> Gen meta a
altn = join . list

alt :: Gen meta a -> Gen meta a -> Gen meta a
alt x y = altn [x,y]

makeLenses ''GenState

preTransformG, postTransformG :: InternalFloat -> Gen meta InternalFloat
preTransformG x = do
    f <- use preTrans
    list $ maybeToList $ runGenTransform f x
postTransformG x = do
    f <- use postTrans
    list $ maybeToList $ runGenTransform f x

transformG :: InternalFloat -> Gen meta (InternalFloat, InternalFloat)
transformG x = do
    pre <- preTransformG x
    post <- postTransformG pre
    pure (pre, post)

measureG :: InternalFloat -> InternalFloat -> Gen meta InternalFloat
measureG a b = do
    (_, a') <- transformG a
    (_, b') <- transformG b
    pure $ b' - a'

save :: meta -> InternalFloat -> Gen meta InternalFloat
save tickMeta x = do
    (prePos, postPos) <- transformG x
    out <>= [Tick {..}]
    pure prePos

savePre :: (InternalFloat -> meta) -> InternalFloat -> Gen meta InternalFloat
savePre tickMetaF x = do
    tickMeta <- tickMetaF <$> preTransformG x
    save tickMeta x

zoom :: InternalFloat -> InternalFloat -> Gen meta a -> Gen meta a
zoom o s = offset o . scale s

offset, scale :: InternalFloat -> Gen meta a -> Gen meta a
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

-- TICK METADATA
data TextAnchor = TextAnchor
    { textAnchorXPct :: Double
    , textAnchorYPct :: Double
    }
    deriving Show

data TickAnchor = Pct Double | Abs Double
    deriving Show

data Anchor = Anchor
    { textAnchor :: TextAnchor
    , tickAnchor :: TickAnchor
    , anchorOffset :: D.V2 Double
    }
    deriving Show

alongsideTopLeft, aboveCenter :: Double -> Anchor
alongsideTopLeft sep =
    Anchor
        { textAnchor = TextAnchor
            { textAnchorXPct = 0
            , textAnchorYPct = 1
            }
        , tickAnchor = Pct 1
        , anchorOffset = D.V2 sep 0
        }
aboveCenter sep =
    Anchor
        { textAnchor = TextAnchor
            { textAnchorXPct = 0.5
            , textAnchorYPct = 0
            }
        , tickAnchor = Pct 1
        , anchorOffset = D.V2 0 sep
        }

data TextMeta = TextMeta
    { _anchor :: Anchor
    , _fontSize :: Double
    , _text :: String
    }
    deriving Show

data Meta = Meta { _start :: Double, _end :: Double, _mtext :: Maybe TextMeta }
    deriving Show

makeLenses ''TextMeta
makeLenses ''Meta

defMeta :: Meta
defMeta = Meta { _start = 0, _end = 1, _mtext = Nothing }

meta :: Double -> Double -> Maybe String -> Meta
meta s e mt = Meta s e (TextMeta (aboveCenter 0) 0.3 <$> mt)

meta0 :: Double -> Maybe String -> Meta
meta0 = meta 0
meta0J :: Double -> String -> Meta
meta0J h t = meta0 h (Just t)
meta0N :: Double -> Meta
meta0N h = meta0 h Nothing

-- SCALE GENERATION
powC :: Int -> Gen Meta ()
powC n =
    postZoomG (Log $ 10 ^ n) $ do
        save (meta0J 1 "1") (10 ^ n)
        p <- list [0..n-1]
        postZoomG (Scale $ 10 ^ p) $ do
            logScale

logScale :: Gen Meta ()
logScale = do
    save (Meta 0.6 0.7 $ Just $ TextMeta (aboveCenter 0) 0.25 "π") pi
    save (Meta 0.6 0.7 $ Just $ TextMeta (aboveCenter 0) 0.25 "e") e
    x <- list [1..9]
    save (meta0J 1 (show x)) (fromIntegral x)
    offset (fromIntegral x) $ tryPartitions 0.0025 (False, False)
        [[(2, const $ Meta 0 0.7 $ Just $ TextMeta (aboveCenter 0) 0.25 ".5"), (5, const $ meta0N 0.5), (5, const $ meta0N 0.25)]
        ,[(2, const $ meta0N 0.7), (5, const $ meta0N 0.5), (2, const $ meta0N 0.3)]
        ,[(2, const $ meta0N 0.7), (5, const $ meta0N 0.5)]
        ,[(5, const $ meta0N 0.5)]
        ,[(2, const $ meta0N 0.5),(2, const $ meta0N 0.3)]
        ,[(2, const $ meta0N 0.5)]
        ]

-- SCALE PARTITIONING
partition :: (Bool, Bool) -> [(Integer, InternalFloat -> Meta)] -> Gen Meta ()
partition (tickStart, tickEnd) [] = pure ()
partition (tickStart, tickEnd) (curr:rest) = do
    let (n, metaF) = curr
    elseUnit tickEnd $ savePre metaF (fromIntegral n)
    x <- list [0..n-1]
    zoom 0 (1 / fromIntegral n) $ do
        elseUnit (tickStart || x /= 0) $ savePre metaF (fromIntegral x)
        zoom (fromIntegral x) 1 $ partition (False, False) rest

tryPartitions :: InternalFloat -> (Bool, Bool) -> [[(Integer, InternalFloat -> Meta)]] -> Gen Meta ()
tryPartitions threshold startEnd [] = pure ()
tryPartitions threshold startEnd (partitionSpec:specs) = do
    minPartitionWidth <- minPartitionWidth partitionSpec
    if minPartitionWidth > threshold
       then partition startEnd partitionSpec
       else tryPartitions threshold startEnd specs

minPartitionWidth :: [(Integer, InternalFloat -> Meta)] -> Gen meta InternalFloat
minPartitionWidth xs = do
    let sliceWidth = product $ map fst xs
        b = 1
        a = b - recip (fromIntegral sliceWidth)
    measureG a b

-- RENDERING
total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 [a, b, c, d]

a, b, c, d :: D.Diagram D.B
a = foldMap (renderTick False) $ toList $ _out $ execGen $ powC 1
b = foldMap (renderTick False) $ toList $ _out $ execGen $ powC 2
c = foldMap (renderTick False) $ toList $ _out $ execGen $ powC 3
d = foldMap (renderTick False) $ toList $ _out $ execGen dTicks

dTicks =
    postZoomG (Offset $ negate $ ll 10 $ 10 ** 0.001) $
    postZoomG (LogLog 10) $
    preZoomG (Below $ 10 ** 0.01) $
    preZoomG (Offset 1) $
        let genMeta b h x
                = Meta 0 h
                $ if b
                     then Just $ TextMeta (alongsideTopLeft 0.003) 0.25 $ dropWhileEnd (=='0') $ show x
                     else Nothing
        -- x <- list $ [2.5] ++ map fromIntegral ([3,4..10] ++ [15,20])
        in
        altn
            [ preZoomG (Scale 0.001) $ do
                savePre (genMeta True 0.7) 2.5
                pure ()
            , preZoomG (Scale 0.001) $ do
                x <- list [3..9]
                offset (fromIntegral x) $ do
                    savePre (genMeta True 0.7) 0
                    tryPartitions 0.0025 (False, False)
                        [[(2, genMeta False 0.5), (5, genMeta False 0.25), (5, genMeta False 0.125)]
                        ,[(2, genMeta False 0.5), (5, genMeta False 0.25), (2, genMeta False 0.125)]
                        ,[(2, genMeta False 0.5), (5, genMeta False 0.25)]
                        ,[(5, genMeta False 0.5)]
                        ,[(2, genMeta False 0.5),(2, genMeta False 0.25)]
                        ,[(2, genMeta False 0.5)]
                        ]
            , preZoomG (Scale 0.01) $ do
                x <- list [1,1.5,2]
                offset x $ do
                    savePre (genMeta True 0.7) 0
                    tryPartitions 0.0025 (False, False)
                        [[(2, genMeta False 0.5), (5, genMeta False 0.25), (5, genMeta False 0.125)]
                        ,[(2, genMeta False 0.5), (5, genMeta False 0.25), (2, genMeta False 0.125)]
                        ,[(2, genMeta False 0.5), (5, genMeta False 0.25)]
                        ,[(5, genMeta False 0.5)]
                        ,[(2, genMeta False 0.5),(2, genMeta False 0.25)]
                        ,[(2, genMeta False 0.5)]
                        ]
            ]

renderTick :: Bool -> Tick Meta -> D.Diagram D.B
renderTick above Tick { prePos, postPos, tickMeta = meta } =
    let tickVec   = D.r2 (0, 0.04 * (meta^.end - meta^.start))
        tickStart = D.r2 (0, 0.04 * meta^.start)
        tickEnd   = D.r2 (0, 0.04 * meta^.end)
        tickDiff  = tickEnd - tickStart
        origTick = laserline [tickVec] & D.translate tickStart
        withLabel :: D.Diagram D.B
        withLabel =
            case meta^.mtext  of
              Nothing -> origTick
              Just textMeta@TextMeta {..} ->
                  let Anchor {..} = _anchor
                      TextAnchor {..} = textAnchor
                      labelOffset :: D.V2 Double
                      labelOffset
                        = anchorOffset * D.r2 (1, 0.04)
                        + case tickAnchor of
                            Pct p -> tickStart + tickDiff * D.V2 p p
                            Abs x -> D.r2 (0, 0.04 * x)
                      label :: D.Diagram D.B
                      label
                        = D.alignedText textAnchorXPct textAnchorYPct _text
                        & D.fontSizeL (0.04 * _fontSize) & D.fc D.black
                        & D.font "Comfortaa"
                        & D.translate labelOffset
                   in label <> origTick
     in withLabel & D.translate (D.r2 (realToFrac postPos, 0))

laserline :: [D.V2 Double] -> D.Diagram D.B
laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin & D.lc D.black
