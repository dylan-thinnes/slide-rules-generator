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
    { oPartitions :: [Partition]
    , nextOptions :: [(Int, Int, [OptionTree])]
    }

instance Show OptionTree where
    show OptionTree { oPartitions } = "OptionTree { partitions = " ++ show oPartitions ++ " }"

data PartitionTree = PartitionTree
    { partitions     :: [Partition]
    , nextPartitions :: [(Int, PartitionTree)]
    }

runPartition :: Partition -> Generator () -> Generator ()
runPartition Partition { _n, _startAt, _tickCreatorF } subact = do
    let scaling x = fromIntegral x / fromIntegral (_startAt + _n)
    (i, x) <- list $ [0..] `zip` map scaling [_startAt.._startAt+_n-1]
    withTickCreator _tickCreatorF $ do
        if i /= 0 then output x else pure ()
        translate x (scaling 1) subact

runPartitions :: [Partition] -> Generator () -> Generator ()
runPartitions [] subact = subact
runPartitions (partition:rest) subact = runPartition partition (runPartitions rest subact)

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

firstMatching :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstMatching f [] = pure Nothing
firstMatching f (x:xs) = f x >>= \b -> if b then pure (Just x) else firstMatching f xs

    {-
bestPartitions :: InternalFloat -> [(Int, [OptionTree])] -> Generator ()
bestPartitions tolerance = go
    where
        go OptionTree { oPartitions, nextOptions } = do
            pret <- gets _preTransformations
            postt <- gets _postTransformations
            meets <- traceShow ("meets", oPartitions, pret, postt) $ meetsTolerance tolerance (runPartitions oPartitions (pure ()))
            if not meets then pure ()
              else together
                [ runPartitions oPartitions $ pure ()
                --, flip mapM_ nextOptions $ \(rangeStart, rangeEnd, nextOptions) -> do
                --    let mkGenerator optionTree = do
                --            offset <- list [rangeStart..rangeEnd]
                --            let scale = 1.0 / fromIntegral (product $ map _n oPartitions)
                --            translate (fromIntegral offset) scale (go optionTree)
                --    matchingGen <- firstMatching (meetsTolerance tolerance) (map mkGenerator nextOptions)
                --    case matchingGen of
                --        Nothing -> pure ()
                --        Just matchingGen -> matchingGen
                ]
    -}
