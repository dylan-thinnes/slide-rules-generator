{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module OldMain where

import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

import qualified Data.ByteString.Lazy
import qualified Graphics.Svg.Core

import qualified Data.Sequence                as S
import           Text.Printf

import           GHC.TypeLits

import           Control.Monad.List
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Trans.Maybe

import           Control.Lens                 hiding (Fold, transform, zoom)
import           Control.Lens.TH

import           Control.Monad.Extra
import           Data.List.Extra              (dropWhileEnd)

import           Data.Foldable                (toList)
import           System.IO.Unsafe
import           System.Random

import           Numeric.Decimal
import qualified Numeric.Decimal.Arithmetic   as NDA
import qualified Numeric.Decimal.Operation    as NDO

import           Data.Either
import           Data.Maybe

import           Numeric
import           Text.Show                    (ShowS)

import qualified Data.Text                    as T

import           Debug.Trace

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

k = const

logClamp :: BasicDecimal -> BasicDecimal
logClamp x
  | abs x >= 10 = logClamp (x / 10)
  | abs x < 1   = logClamp (x * 10)
  | otherwise   = x

deshow :: (a -> ShowS) -> (a -> String)
deshow f x = f x ""

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
    | Fold   InternalFloat
    | Pred   Predicate
    deriving Show

data Predicate
    = Above InternalFloat
    | Below InternalFloat
    | Within InternalFloat InternalFloat
    deriving Show

newtype Transform = Transform [Transform1]
    deriving Show

runTransformPred :: Transform -> InternalFloat -> Maybe InternalFloat
runTransformPred (Transform ones)
  = appEndo (foldMap (Endo . (=<<) . runTransform1Pred) (reverse ones))
  . Just

runTransformNoPred :: Transform -> InternalFloat -> InternalFloat
runTransformNoPred (Transform ones)
  = appEndo $ foldMap (Endo . runTransform1NoPred) $ reverse ones

runTransform1Pred :: Transform1 -> InternalFloat -> Maybe InternalFloat
runTransform1Pred (Pred pred) x = guard (runPredicate pred x) >> pure x
runTransform1Pred trans x       = pure $ runTransform1NoPred trans x

runTransform1NoPred :: Transform1 -> InternalFloat -> InternalFloat
runTransform1NoPred (Offset offset) x = offset + x
runTransform1NoPred (Scale scale)   x = scale * x
runTransform1NoPred (Log base)      x = logBase base x
runTransform1NoPred (LogLog base)   x = ll base x
runTransform1NoPred (Fold point)    x = if x >= point then x - point else 1 - (point - x)
runTransform1NoPred (Pred pred)     x = x

runPredicate :: Predicate -> InternalFloat -> Bool
runPredicate (Above lower) x = lower <= x
runPredicate (Below upper) x = x <= upper
runPredicate (Within lower upper) x = runPredicate (Above lower) x && runPredicate (Below upper) x

pushTransform :: Transform1 -> Transform -> Transform
pushTransform t1 (Transform ts) = Transform (t1 : ts)

data Tick meta = Tick
    { prePos   :: InternalFloat
    , postPos  :: InternalFloat
    , tickMeta :: meta
    }
    deriving (Show)

data GenState meta = GenState
    { _preTrans  :: Transform
    , _postTrans :: Transform
    , _out       :: S.Seq (Tick meta)
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

preTransformG, postTransformG :: InternalFloat -> MaybeT (Gen meta) InternalFloat
preTransformG x = do
    f <- use preTrans
    MaybeT $ pure $ runTransformPred f x
postTransformG x = do
    f <- use postTrans
    MaybeT $ pure $ runTransformPred f x

transformG :: InternalFloat -> MaybeT (Gen meta) (InternalFloat, InternalFloat)
transformG x = do
    pre <- preTransformG x
    post <- postTransformG pre
    pure (pre, post)

measureG :: InternalFloat -> InternalFloat -> MaybeT (Gen meta) InternalFloat
measureG a b = do
    (_, a') <- transformG a
    (_, b') <- transformG b
    pure $ b' - a'

preTransformGNoPred, postTransformGNoPred :: InternalFloat -> Gen meta InternalFloat
preTransformGNoPred x = do
    f <- use preTrans
    pure $ runTransformNoPred f x
postTransformGNoPred x = do
    f <- use postTrans
    pure $ runTransformNoPred f x

transformGNoPred :: InternalFloat -> Gen meta (InternalFloat, InternalFloat)
transformGNoPred x = do
    pre <- preTransformGNoPred x
    post <- postTransformGNoPred pre
    pure (pre, post)

measureGNoPred :: InternalFloat -> InternalFloat -> Gen meta InternalFloat
measureGNoPred a b = do
    (_, a') <- transformGNoPred a
    (_, b') <- transformGNoPred b
    pure $ b' - a'

save' :: meta -> InternalFloat -> MaybeT (Gen meta) InternalFloat
save' tickMeta x = do
    (prePos, postPos) <- transformG x
    if True -- prePos `elem` ([0.23120, 0.23160] :: [InternalFloat])
       then do
           preStack <- use preTrans
           postStack <- use postTrans
           -- traceShow (x, prePos, postPos, preStack, postStack) $ pure ()
           pure ()
       else pure ()
    out <>= [Tick {..}]
    pure prePos

savePre' :: (InternalFloat -> meta) -> InternalFloat -> MaybeT (Gen meta) InternalFloat
savePre' tickMetaF x = do
    tickMeta <- tickMetaF <$> preTransformG x
    save' tickMeta x

save :: meta -> InternalFloat -> Gen meta ()
save meta x = runMaybeT (save' meta x) >> pure ()

savePre :: (InternalFloat -> meta) -> InternalFloat -> Gen meta ()
savePre tickMetaF x = runMaybeT (savePre' tickMetaF x) >> pure ()

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
    { textAnchor   :: TextAnchor
    , tickAnchor   :: TickAnchor
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
    { _anchor   :: Anchor
    , _fontSize :: Double
    , _text     :: String
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
    save (Meta 0.6 0.7 $ Just $ TextMeta (aboveCenter (-0.1)) 0.25 "π") pi
    save (Meta 0.6 0.7 $ Just $ TextMeta (aboveCenter (-0.1)) 0.25 "e") e
    x <- list [1..9]
    save (meta0J 1 (show x)) (fromIntegral x)
    offset (fromIntegral x) $ tryPartitions 0.00125 (False, False)
        [[(2, const $ Meta 0 0.7 $ Just $ TextMeta (aboveCenter 0) 0.25 ".5"), (5, const $ meta0N 0.5), (2, const $ meta0N 0.375), (5, const $ meta0N 0.25)]
        ,[(2, const $ Meta 0 0.7 $ Just $ TextMeta (aboveCenter 0) 0.25 ".5"), (5, const $ meta0N 0.5), (5, const $ meta0N 0.25)]
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
    measureGNoPred a b

-- RENDERING
total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 [ab, bb, cb, db, eb, fb, gb, hb]

ab, bb, cb, db, eb :: D.Diagram D.B
ab = renderSlide $ powC 1
bb = renderSlide $ powC 2
cb = renderSlide $ powC 3
db = renderSlide dTicks
eb = renderSlide eTicks
fb = renderSlide fTicks
gb = renderSlide gTicks
hb = renderSlide hTicks

genMeta b h x =
    let label = show x
        front = takeWhile (/= '.') label
        end = label & dropWhile (/= '.') & dropWhileEnd (== '0')
        cleanLabel = front ++ end
        meta = TextMeta (alongsideTopLeft 0.003) 0.175 cleanLabel
    in
    Meta 0 h $ guard b >> pure meta

genMetaS b h s =
    let meta = TextMeta (alongsideTopLeft 0.003) 0.175 s
    in
    Meta 0 h $ guard b >> pure meta

z scale1 items scale2 =
    scale scale1 $ do
        x <- list items
        offset x $ scale scale2 $ do
            savePre (genMeta True 0.7) 0
            tryPartitions 0.00125 (False, False)
                [[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25), (5, k $ meta0N 0.125)]
                ,[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25), (2, k $ meta0N 0.125)]
                ,[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25)]
                ,[(5, k $ meta0N 0.25)]
                ,[(2, k $ meta0N 0.5), (2, k $ meta0N 0.25)]
                ,[(2, k $ meta0N 0.5)]
                ]

splitting :: (InternalFloat -> Meta) -> [InternalFloat] -> Gen Meta ()
splitting metaShower items =
    let f a b =
            offset a $ scale (b - a) $ do
                savePre metaShower 0
                tryPartitions 0.00125 (False, False)
                    [[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25), (5, k $ meta0N 0.125)]
                    ,[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25), (2, k $ meta0N 0.125)]
                    ,[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25)]
                    ,[(5, k $ meta0N 0.25)]
                    ,[(2, k $ meta0N 0.5), (2, k $ meta0N 0.25)]
                    ,[(2, k $ meta0N 0.5)]
                    ]
    in
    altn $ zipWith f items (tail items)

dTicks =
    postZoomG (Offset $ negate $ ll 10 $ 10 ** 0.001) $
    postZoomG (LogLog 10) $
    postZoomG (Pred $ Within (10 ** 0.001) (10 ** 0.01)) $
    preZoomG (Offset 1) $
    preZoomG (Scale 0.001) $
        splitting (genMeta True 0.7) $ [2,2.5] ++ map fromIntegral [3..9] ++ [10, 15, 20, 25]

eTicks =
    postZoomG (Offset $ negate $ ll 10 $ 10 ** 0.01) $
    postZoomG (LogLog 10) $
    postZoomG (Pred $ Within (10 ** 0.01) (10 ** 0.1)) $
    preZoomG (Offset 1) $
    preZoomG (Scale 0.01) $
        splitting (genMeta True 0.7) $ [2,2.5] ++ map fromIntegral [3..9] ++ [10, 15, 20, 25, 30]

fTicks =
    postZoomG (Offset $ negate $ ll 10 $ 10 ** 0.1) $
    postZoomG (LogLog 10) $
    postZoomG (Pred $ Within (10 ** 0.1) (10 ** 1)) $
        altn
            [ preZoomG (Offset 1) $
              preZoomG (Scale 0.1) $
                splitting (genMeta True 0.7) $ [2,2.5] ++ map fromIntegral [3..10]
            , splitting (genMeta True 0.7) $ [2,2.5] ++ map fromIntegral [3..10]
            , z 1 [10] 1
            ]

splitting' :: [(InternalFloat, InternalFloat -> Meta, [[(Integer, InternalFloat -> Meta)]])] -> Gen Meta ()
splitting' items =
    let f (start, metaShower, partitions) (end, _, _) =
            offset start $ scale (end - start) $ do
                savePre metaShower 0
                tryPartitions 0.00125 (False, False) partitions
    in
    altn $ zipWith f items (tail items)

partitions10 =
    [[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25), (5, k $ meta0N 0.125)]
    ,[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25), (2, k $ meta0N 0.125)]
    ,[(2, k $ meta0N 0.5), (5, k $ meta0N 0.25)]
    ,[(5, k $ meta0N 0.25)]
    ,[(2, k $ meta0N 0.5), (2, k $ meta0N 0.25)]
    ,[(2, k $ meta0N 0.5)]
    ]

partitions3 =
    [[(3, k $ meta0N 0.5), (5, k $ meta0N 0.25), (5, k $ meta0N 0.125)]
    ,[(3, k $ meta0N 0.5), (5, k $ meta0N 0.25), (2, k $ meta0N 0.125)]
    ,[(3, k $ meta0N 0.5), (5, k $ meta0N 0.25)]
    ,[(3, k $ meta0N 0.25)]
    ,[(3, k $ meta0N 0.5), (2, k $ meta0N 0.25)]
    ,[(3, k $ meta0N 0.5)]
    ]

partitions5 =
    [[(5, k $ meta0N 0.5), (5, k $ meta0N 0.25), (5, k $ meta0N 0.125)]
    ,[(5, k $ meta0N 0.5), (5, k $ meta0N 0.25), (2, k $ meta0N 0.125)]
    ,[(5, k $ meta0N 0.5), (5, k $ meta0N 0.25)]
    ,[(5, k $ meta0N 0.5), (2, k $ meta0N 0.25)]
    ,[(5, k $ meta0N 0.5)]
    ]

partitions4LL =
    [[(4, k $ meta0N 0.5), (5, k $ meta0N 0.25), (5, k $ meta0N 0.125)]
    ,[(4, k $ meta0N 0.5), (5, k $ meta0N 0.25), (2, k $ meta0N 0.125)]
    ,[(4, k $ meta0N 0.5), (5, k $ meta0N 0.25)]
    ,[(4, k $ meta0N 0.5), (2, k $ meta0N 0.25)]
    ,[(4, k $ meta0N 0.5)]
    ]

gTicks =
    let interval1 = genMeta True 0.7
        interval2 = genMetaS True 0.7 . deshow (showEFloat (Just 0))
        interval3 = const $ genMetaS False 0.7 ""
    in
    postZoomG (Offset $ negate $ ll 10 $ 10 ** 1) $
    postZoomG (LogLog 10) $
    postZoomG (Pred $ Within (10 ** 1) (10 ** 10)) $ do
        savePre interval2 1e10
        splitting'
            [(10, interval1, partitions10)
            ,(15, interval1, partitions10)
            ,(20, interval1, partitions10)
            ,(30, interval1, partitions10)
            ,(40, interval1, partitions10)
            ,(50, interval1, partitions10)
            ,(100, interval1, partitions10)
            ,(200, interval1, partitions10)
            ,(500, interval1, partitions10)
            ,(1000, interval1, partitions10)
            ,(2000, interval1, partitions10)
            ,(5000, interval1, partitions10)
            ,(10000, interval2, partitions10)
            ,(20000, interval2, partitions3)
            ,(50000, interval2, partitions5)
            ,(100000, interval2, partitions4LL)
            ,(500000, interval3, partitions5)
            ,(1000000, interval2, partitions4LL)
            ,(5000000, interval3, partitions5)
            ,(10000000, interval2, partitions4LL)
            ,(50000000, interval3, partitions5)
            ,(100000000, interval2, partitions4LL)
            ,(500000000, interval3, partitions5)
            ,(1000000000, interval2, partitions4LL)
            ,(5000000000, interval3, partitions5)
            ,(10000000000, interval2, [])
            ]

hTicks = postZoomG (Fold $ logBase 10 pi) $ powC 1

hScale = 0.02
tScale = 0.04

renderSlide :: Gen Meta a -> D.Diagram D.B
renderSlide gen =
    let ticks = foldMap (renderTick False) $ toList $ _out $ execGen gen
    in
    ticks <> laserline [D.r2 (0, 0), D.r2 (1, 0)]

renderTick :: Bool -> Tick Meta -> D.Diagram D.B
renderTick above Tick { prePos, postPos, tickMeta = meta } =
    let tickStart = D.r2 (0, hScale * meta^.start)
        tickEnd   = D.r2 (0, hScale * meta^.end)
        tickDiff  = tickEnd - tickStart
        origTick = laserline [tickDiff] & D.translate tickStart
        withLabel =
            case meta^.mtext  of
              Nothing -> origTick
              Just textMeta@TextMeta {..} ->
                  let Anchor {..} = _anchor
                      TextAnchor {..} = textAnchor
                      labelOffset :: D.V2 Double
                      labelOffset
                        = anchorOffset * D.r2 (1, hScale)
                        + case tickAnchor of
                            Pct p -> tickStart + tickDiff * D.V2 p p
                            Abs x -> D.r2 (0, hScale * x)
                      label :: D.Diagram D.B
                      label
                        = D.alignedText textAnchorXPct textAnchorYPct _text
                        & D.fontSizeL (tScale * _fontSize) & D.fc D.black
                        & D.font "Comfortaa"
                        & D.translate labelOffset
                   in label <> origTick
     in withLabel & D.translate (D.r2 (realToFrac postPos, 0))

laserline :: [D.V2 Double] -> D.Diagram D.B
laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin & D.lc D.black

main :: IO ()
main = do
    let options = D.SVGOptions (D.mkWidth 2000) Nothing (T.pack "") [] True
    let svgDoc = D.renderDia D.SVG options total
    let bs = Graphics.Svg.Core.renderBS svgDoc
    Data.ByteString.Lazy.writeFile "circle.svg" bs
