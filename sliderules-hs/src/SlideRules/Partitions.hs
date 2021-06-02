{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module SlideRules.Partitions where

-- base
import Control.Monad
import Data.Functor ((<&>))
import qualified Numeric

-- containers
import qualified Data.Set
import Data.Foldable (toList)

-- lens
import Control.Lens (use)

-- mtl
import Control.Monad.State (get, gets)

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

runPartition :: Partition -> (Integer -> Generator a) -> Generator a
runPartition Partition { _n, _startAt, _tickCreatorF } subact = do
    let scaling x = fromIntegral x / fromIntegral (_startAt + _n)
    (i, x) <- list $ [0..] `zip` map scaling [_startAt.._startAt+_n-1]
    withTickCreator _tickCreatorF $ do
        if i /= 0 then output x else pure ()
        translate x (scaling 1) (subact i)

runPartitions :: [Partition] -> (Integer -> Generator a) -> Generator a
runPartitions globalPartitions f = go 0 globalPartitions
    where
        go i [] = f i
        go i (p:rest) = runPartition p (\j -> go (i + j * product (map _n rest)) rest)

data SmallestTickDistance = NoTicks | OneTick InternalFloat | ManyTicks InternalFloat

getSmallestTickDistance :: Generator a -> Generator SmallestTickDistance
getSmallestTickDistance act = do
    ownState <- get
    let subrun = generateWith act ownState
    let postPoses = toList $ _postPos <$> _out subrun
    case postPoses of
        [] -> pure NoTicks -- No ticks emitted
        [x] -> pure (OneTick x) -- One tick emitted
        _ ->
            let sortedPoses = Data.Set.toList $ Data.Set.fromList postPoses
                distances = abs <$> zipWith (-) sortedPoses (tail sortedPoses)
                minDistance = minimum distances
            in
            pure $ ManyTicks minDistance

meetsTolerance :: InternalFloat -> Generator a -> Generator Bool
meetsTolerance tolerance act = do
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

bestPartitions :: InternalFloat -> OptionTree -> Generator (Maybe PartitionTree)
bestPartitions tolerance = go id
    where
        go :: (Generator () -> Generator ()) -> OptionTree -> Generator (Maybe PartitionTree)
        go selfTransform OptionTree { oPartitions, nextOptions } = do
            meets <- meetsTolerance tolerance $ selfTransform $ runPartitions oPartitions (\_ -> pure ())
            if not meets
              then pure Nothing
              else do
                bestOptions <- flip traverse nextOptions $ \(rangeStart, rangeEnd, optionTrees) -> do
                    let rangedSelfTransform gen
                            = selfTransform
                            $ runPartitions oPartitions
                            $ \i -> if rangeEnd >= i && i >= rangeStart then gen else pure ()
                    firstJust <- getFirstJust (go rangedSelfTransform) optionTrees
                    case firstJust of
                        Nothing -> pure []
                        Just bestPartitionTree -> pure [(rangeStart, rangeEnd, bestPartitionTree)]
                pure $ Just $ PartitionTree { partitions = oPartitions, nextPartitions = concat bestOptions }

runPartitionTree :: PartitionTree -> Generator ()
runPartitionTree (PartitionTree { partitions, nextPartitions }) =
    runPartitions partitions $ \i -> do
        case filter (\(rangeStart, rangeEnd, tree) -> rangeEnd >= i && i >= rangeStart) nextPartitions of
            [] -> pure ()
            ((_, _, tree):_) -> runPartitionTree tree

tenIntervals :: InternalFloat -> InternalFloat -> Integer
tenIntervals start end =
    let (digits, p) = Numeric.floatToDigits 10 $ end - start
    in
    foldl (\x n -> x * 10 + fromIntegral n) 0 digits

smartPartitionTens :: InternalFloat -> (Integer -> [(Integer, Integer)]) -> [OptionTree] -> [InternalFloat] -> Generator ()
smartPartitionTens tolerance handler part10 points =
    let intervals = zip points (tail points)
    in
    together $
        intervals <&> \(start, end) ->
            let n = tenIntervals start end
                optionTree = optionFromRanges [mkPartition n] (handler n) part10
                gen =
                    translate start (end - start) $
                        maybeM () runPartitionTree =<<
                            bestPartitions tolerance optionTree
            in
            gen
