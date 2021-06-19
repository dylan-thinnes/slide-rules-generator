{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module SlideRules.Partitions where

-- base
import Control.Monad
import Data.Functor ((<&>))
import qualified Numeric

-- containers
import qualified Data.Set
import Data.Foldable (toList)

-- decimal
import Data.Decimal

-- lens
import Control.Lens (use, (%~))

-- mtl
import Control.Monad.State (get, gets)
import Control.Monad.Reader (ask, asks)

-- local (sliderules)
import SlideRules.Utils
import SlideRules.Types
import SlideRules.Tick
import SlideRules.Generator hiding (_tickCreator)

data Partition = Partition
    { _n :: Integer
    , _startAt :: Integer
    , _tickCreatorF :: TickCreator -> TickCreator
    }

instance Show Partition where
    show Partition { _n, _startAt } =
        "Partition { _n = " ++ show _n ++ ", _startAt = " ++ show _startAt ++ " }"

mkPartition :: Integer -> Partition
mkPartition n = Partition { _n = n, _startAt = 0, _tickCreatorF = id }

mkPartitionWithCreator :: Integer -> (TickCreator -> TickCreator) -> Partition
mkPartitionWithCreator n tickCreatorF = Partition { _n = n, _startAt = 0, _tickCreatorF = tickCreatorF }

data OptionTree = OptionTree
    { oPartitions :: [Partition]
    , nextOptions :: [(Integer, Integer, [OptionTree])]
    }
    deriving Show

optionFromRanges :: [Partition] -> [(Integer, Integer)] -> [OptionTree] -> OptionTree
optionFromRanges oPartitions ranges nextOptions =
    OptionTree
        { oPartitions
        , nextOptions = ranges <&> \(start, end) -> (start, end, nextOptions)
        }

data PartitionTree = PartitionTree
    { partitions     :: [Partition]
    , nextPartitions :: [(Integer, Integer, PartitionTree)]
    }
    deriving Show

fillOptionTree :: [Partition] -> [OptionTree] -> OptionTree
fillOptionTree partitions suboptions =
    OptionTree
        { oPartitions = partitions
        , nextOptions = zip3 [0..product (map _n partitions) - 1] [0..product (map _n partitions) - 1] (repeat suboptions)
        }

runPartition :: (Bool, Bool) -> Partition -> (Integer -> Generator ()) -> Generator ()
runPartition (outputFirst, outputLast) Partition { _n, _startAt, _tickCreatorF } subact = do
    let scaling x = fromIntegral x / fromIntegral (_startAt + _n)
    withTickCreator _tickCreatorF $ do
        if outputLast
          then output $ scaling (_startAt + _n)
          else pure ()
        forM ([0..] `zip` map scaling [_startAt.._startAt+_n-1]) $ \(i, x) -> do
            if i /= 0 || outputFirst then output x else pure ()
            translate x (scaling 1) (subact i)
    pure ()

runPartitions :: (Bool, Bool) -> [Partition] -> (Integer -> Generator ()) -> Generator ()
runPartitions outputFirstLast globalPartitions f = go outputFirstLast 0 globalPartitions
    where
        go _ i [] = f i
        go outputFirstLast i (p:rest) =
            runPartition outputFirstLast p (\j -> go (False, False) (i + j * product (map _n rest)) rest)

data SmallestTickDistance = NoTicks | OneTick InternalFloat | ManyTicks InternalFloat
    deriving (Show)

getSmallestTickDistance :: Generator a -> Generator SmallestTickDistance
getSmallestTickDistance act = do
    ownState <- get
    ownSettings <- ask
    let logging = generateWith ownSettings act ownState
    let postPoses = truePos <$> toList (fst (unlogging logging))
    case postPoses of
        [] -> pure NoTicks -- No ticks emitted
        [x] -> pure (OneTick x) -- One tick emitted
        _ ->
            let sortedPoses = postPoses
                distances = abs <$> zipWith (-) sortedPoses (tail sortedPoses)
                minDistance = minimum distances
            in
            pure $ ManyTicks minDistance

meetsTolerance :: Generator a -> Generator Bool
meetsTolerance act = do
    tolerance <- asks tolerance
    smallestTickDistance <- getSmallestTickDistance act
    case smallestTickDistance of
        NoTicks -> pure False -- Ignore zerotick & onetick variants
        OneTick _ -> pure False
        ManyTicks smallestTickDistance -> pure $ smallestTickDistance > tolerance

getFirstMatching :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
getFirstMatching f [] = pure Nothing
getFirstMatching f (x:xs) = f x >>= \b -> if b then pure (Just x) else getFirstMatching f xs

getFirstJust :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
getFirstJust f [] = pure Nothing
getFirstJust f (x:xs) = do
    mayB <- f x
    case mayB of
        Just x -> pure $ Just x
        Nothing -> getFirstJust f xs

getLastJust :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
getLastJust = go Nothing
    where
        go prev f [] = pure prev
        go prev f (x:xs) = do
            mayB <- f x
            case mayB of
                Just x -> go (Just x) f xs
                Nothing -> pure prev

bestPartitions :: [OptionTree] -> Generator (Maybe PartitionTree)
bestPartitions = getFirstJust bestPartition

bestPartition :: OptionTree -> Generator (Maybe PartitionTree)
bestPartition = go id
    where
        go :: (Generator () -> Generator ()) -> OptionTree -> Generator (Maybe PartitionTree)
        go selfTransform OptionTree { oPartitions, nextOptions } = do
            meets <-
                if product (map _n oPartitions) == 1
                  then pure True
                  else meetsTolerance $ selfTransform $ runPartitions (False, False) oPartitions (\_ -> pure ())
            if not meets
              then pure Nothing
              else do
                bestOptions <- flip traverse nextOptions $ \(rangeStart, rangeEnd, optionTrees) -> do
                    let rangedSelfTransform gen
                            = selfTransform
                            $ runPartitions (False, False) oPartitions
                            $ \i -> if rangeEnd >= i && i >= rangeStart then gen else pure ()
                    firstJust <- getFirstJust (go rangedSelfTransform) optionTrees
                    case firstJust of
                        Nothing -> pure []
                        Just bestPartitionTree -> pure [(rangeStart, rangeEnd, bestPartitionTree)]
                pure $ Just $ PartitionTree { partitions = oPartitions, nextPartitions = concat bestOptions }

runPartitionTree :: (Bool, Bool) -> PartitionTree -> Generator ()
runPartitionTree outputFirstLast (PartitionTree { partitions, nextPartitions }) =
    runPartitions outputFirstLast partitions $ \i -> do
        case filter (\(rangeStart, rangeEnd, tree) -> rangeEnd >= i && i >= rangeStart) nextPartitions of
            [] -> pure ()
            ((_, _, tree):_) -> runPartitionTree (False, False) tree

runOptionTrees :: (Bool, Bool) -> [OptionTree] -> Generator ()
runOptionTrees outputFirstLast = bestPartitions >=> maybeM () (runPartitionTree outputFirstLast)

tenIntervals :: Decimal -> Decimal -> Integer
tenIntervals start end =
    let (digits, p) = Numeric.floatToDigits 10 $ realToFrac $ end - start
    in
    foldl (\x n -> x * 10 + fromIntegral n) 0 digits

smartPartitionTens :: (Integer -> [OptionTree]) -> [Decimal] -> Generator ()
smartPartitionTens handler points =
    let intervals = zip points (tail points)
    in
    sequence_ $
        intervals <&> \(intervalStart, intervalEnd) -> do
            let n = tenIntervals intervalStart intervalEnd
            translate (realToFrac intervalStart) (realToFrac intervalEnd - realToFrac intervalStart) $ do
                output 0
                runOptionTrees (False, False) (handler n)

partitionIntervals :: [(InternalFloat, [OptionTree])] -> Generator ()
partitionIntervals points =
    let intervals = zip points (tail points)
    in
    sequence_ $
        intervals <&> \((intervalStart, optionTrees), (intervalEnd, _)) ->
            translate intervalStart (intervalEnd - intervalStart) $ do
                output 0
                runOptionTrees (False, False) optionTrees

genIntervals :: [(InternalFloat, Generator ())] -> Generator ()
genIntervals points =
    let intervals = zip points (tail points)
    in
    sequence_ $
        intervals <&> \((intervalStart, generator), (intervalEnd, _)) ->
            translate intervalStart (intervalEnd - intervalStart) $ do
                output 0
                generator
