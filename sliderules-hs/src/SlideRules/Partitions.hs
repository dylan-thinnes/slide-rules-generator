{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module SlideRules.Partitions where

-- base
import Control.Monad
import Debug.Trace

-- containers
import qualified Data.Set
import Data.Foldable (toList)

-- lens
import Control.Lens (use)

-- mtl
import Control.Monad.State (get, gets)

-- local (sliderules)
import SlideRules.Types
import SlideRules.Tick
import SlideRules.Generator hiding (_tickCreator)

data Partition = Partition
    { _n :: Integer
    , _startAt :: Integer
    , _tickCreatorF :: ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo)
    }

instance Show Partition where
    show Partition { _n, _startAt } =
        "Partition { _n = " ++ show _n ++ ", _startAt = " ++ show _startAt ++ " }"

mkPartition :: Integer -> Partition
mkPartition n = Partition { _n = n, _startAt = 0, _tickCreatorF = id }

data OptionTree = OptionTree
    { oPartition :: Partition
    , nextOptions :: [(Integer, Integer, [OptionTree])]
    }
    deriving Show

data PartitionTree = PartitionTree
    { partition      :: Partition
    , nextPartitions :: [(Integer, Integer, PartitionTree)]
    }
    deriving Show

fillOptionTree :: Partition -> [OptionTree] -> OptionTree
fillOptionTree partition suboptions =
    OptionTree
        { oPartition = partition
        , nextOptions = zip3 [0.._n partition - 1] [0.._n partition - 1] (repeat suboptions)
        }

runPartition :: Partition -> (Integer -> Generator a) -> Generator a
runPartition Partition { _n, _startAt, _tickCreatorF } subact = do
    let scaling x = fromIntegral x / fromIntegral (_startAt + _n)
    (i, x) <- list $ [0..] `zip` map scaling [_startAt.._startAt+_n-1]
    withTickCreator _tickCreatorF $ do
        if i /= 0 then output x else pure ()
        translate x (scaling 1) (subact i)

getSmallestTickDistance :: Generator a -> Generator (Maybe InternalFloat)
getSmallestTickDistance act = do
    ownState <- get
    let subrun = generateWith act ownState
    let postPoses = toList $ _postPos <$> _out subrun
    if null postPoses
      then pure Nothing
      else do
        let sortedPoses = Data.Set.toList $ Data.Set.fromList postPoses
        let distances = abs <$> zipWith (-) sortedPoses (tail sortedPoses)
        let minDistance = minimum distances
        pure $ Just minDistance

meetsTolerance :: InternalFloat -> Generator a -> Generator Bool
meetsTolerance tolerance act = do
    smallestTickDistance <- getSmallestTickDistance act
    case smallestTickDistance of
        Nothing -> pure True
        Just smallestTickDistance -> pure $ smallestTickDistance > tolerance

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

bestPartitions :: InternalFloat -> OptionTree -> Generator (Maybe PartitionTree)
bestPartitions tolerance = go id
    where
        go :: (Generator () -> Generator ()) -> OptionTree -> Generator (Maybe PartitionTree)
        go selfTransform OptionTree { oPartition, nextOptions } = do
            meets <- meetsTolerance tolerance $ selfTransform $ runPartition oPartition (\_ -> pure ())
            if not meets
              then pure Nothing
              else do
                bestOptions <- flip traverse nextOptions $ \(rangeStart, rangeEnd, optionTrees) -> do
                    let rangedSelfTransform gen
                            = selfTransform
                            $ runPartition oPartition
                            $ \i -> if rangeEnd >= i && i >= rangeStart then gen else pure ()
                    firstJust <- getFirstJust (go rangedSelfTransform) optionTrees
                    case firstJust of
                        Nothing -> pure []
                        Just bestPartitionTree -> pure [(rangeStart, rangeEnd, bestPartitionTree)]
                pure $ Just $ PartitionTree { partition = oPartition, nextPartitions = concat bestOptions }

runPartitionTree :: PartitionTree -> Generator ()
runPartitionTree (PartitionTree { partition, nextPartitions }) =
    runPartition partition $ \i -> do
        case filter (\(rangeStart, rangeEnd, tree) -> rangeEnd >= i && i >= rangeStart) nextPartitions of
            [] -> pure ()
            ((_, _, tree):_) -> runPartitionTree tree
