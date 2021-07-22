{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module SlideRules.Partitions where

-- base
import Control.Monad
import Data.Functor ((<&>))
import Data.Foldable (fold)
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
runPartition (outputFirst, outputLast) Partition { _n, _startAt, _tickCreatorF } subact =
    let scaling x = fromIntegral x / fromIntegral (_startAt + _n)
        outputLastAct =
            if outputLast
              then output (scaling (_startAt + _n))
              else pure mempty
    in
    withTickCreator _tickCreatorF $ fold
        [ outputLastAct
        , flip foldMap ([0..] `zip` map scaling [_startAt.._startAt+_n-1]) $ \(i, x) -> fold
            [ if i /= 0 || outputFirst then output x else pure mempty
            , translate x (scaling 1) (subact i)
            ]
        ]

runPartitions :: (Bool, Bool) -> [Partition] -> (Integer -> Generator ()) -> Generator ()
runPartitions outputFirstLast globalPartitions f = go outputFirstLast 0 globalPartitions
    where
        go _ i [] = f i
        go outputFirstLast i (p:rest) =
            runPartition outputFirstLast p (\j -> go (False, False) (i + j * product (map _n rest)) rest)

data SmallestTickDistance = NoTicks | OneTick InternalFloat | ManyTicks InternalFloat
    deriving (Show)

getSmallestTickDistance :: Generator a -> Generator SmallestTickDistance
getSmallestTickDistance act (ownSettings, ownState) =
    let logging = generateWith ownSettings act ownState
        postPoses = truePos <$> toList (fst (unlogging logging))
    in
    case postPoses of
        [] -> (NoTicks, mempty) -- No ticks emitted
        [x] -> (OneTick x, mempty) -- One tick emitted
        _ ->
            let sortedPoses = postPoses
                distances = abs <$> zipWith (-) sortedPoses (tail sortedPoses)
                minDistance = minimum distances
            in
            (ManyTicks minDistance, mempty)

meetsTolerance :: Generator a -> Generator Bool
meetsTolerance act (settings@Settings{ tolerance }, genState) =
    case fst $ getSmallestTickDistance act (settings, genState) of
        NoTicks -> (False, mempty) -- Ignore zerotick & onetick variants
        OneTick _ -> (False, mempty)
        ManyTicks smallestTickDistance -> (smallestTickDistance > tolerance, mempty)

getFirstMatching :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
getFirstMatching f [] = pure Nothing
getFirstMatching f (x:xs) = f x >>= \b -> if b then pure (Just x) else getFirstMatching f xs

getFirstJust :: (a -> Generator (Maybe PartitionTree)) -> [a] -> Generator (Maybe PartitionTree)
getFirstJust f [] _ = (Nothing, mempty)
getFirstJust f (x:xs) settingsState =
    case f x settingsState of
        (Just x, log) -> (Just x, log)
        (Nothing, _) -> getFirstJust f xs settingsState

getLastJust :: (a -> Generator (Maybe PartitionTree)) -> [a] -> Generator (Maybe PartitionTree)
getLastJust = go (Nothing, mempty)
    where
        go prev f [] _ = prev
        go prev f (x:xs) settingsState =
            case f x settingsState of
                (Just x, log) -> go (Just x, log) f xs settingsState
                (Nothing, _) -> prev

bestPartitions :: [OptionTree] -> Generator (Maybe PartitionTree)
bestPartitions = getFirstJust bestPartition

bestPartition :: OptionTree -> Generator (Maybe PartitionTree)
bestPartition = go id
    where
        go :: (Generator () -> Generator ()) -> OptionTree -> Generator (Maybe PartitionTree)
        go selfTransform OptionTree { oPartitions, nextOptions } settingsState =
            let (meets, log) =
                    if product (map _n oPartitions) == 1
                      then (True, mempty)
                      else meetsTolerance (selfTransform $ runPartitions (False, False) oPartitions (\_ -> pure ((), mempty))) settingsState
            in
            if not meets
              then (Nothing, log)
              else
                let bestOptions = flip map nextOptions $ \(rangeStart, rangeEnd, optionTrees) ->
                        let rangedSelfTransform gen
                                = selfTransform
                                $ runPartitions (False, False) oPartitions
                                $ \i -> if rangeEnd >= i && i >= rangeStart then gen else pure ((), mempty)
                            (firstJust, log) = getLastJust (go rangedSelfTransform) (reverse optionTrees) settingsState
                        in
                        case firstJust of
                            Nothing -> []
                            Just bestPartitionTree -> [(rangeStart, rangeEnd, bestPartitionTree)]
                in
                (Just $ PartitionTree { partitions = oPartitions, nextPartitions = concat bestOptions }, log)

runPartitionTree :: (Bool, Bool) -> PartitionTree -> Generator ()
runPartitionTree outputFirstLast (PartitionTree { partitions, nextPartitions }) =
    runPartitions outputFirstLast partitions $ \i ->
        case filter (\(rangeStart, rangeEnd, tree) -> rangeEnd >= i && i >= rangeStart) nextPartitions of
            [] -> pure ((), mempty)
            ((_, _, tree):_) -> runPartitionTree (False, False) tree

runOptionTrees :: (Bool, Bool) -> [OptionTree] -> Generator ()
runOptionTrees outputFirstLast optionTrees settingsState =
    case bestPartitions optionTrees settingsState of
        (Nothing, log) -> ((), log)
        (Just partitionTree, log) -> ((), log) <> runPartitionTree outputFirstLast partitionTree settingsState

tenIntervals :: Decimal -> Decimal -> Integer
tenIntervals start end =
    let (digits, p) = Numeric.floatToDigits 10 $ realToFrac $ abs (end - start)
    in
    foldl (\x n -> x * 10 + fromIntegral n) 0 digits

smartPartitionTens :: (Integer -> [OptionTree]) -> [Decimal] -> Generator ()
smartPartitionTens handler points =
    let intervals = zip points (tail points)
    in
    fold $
        intervals <&> \(intervalStart, intervalEnd) ->
            let n = tenIntervals intervalStart intervalEnd
            in
            translate (realToFrac intervalStart) (realToFrac intervalEnd - realToFrac intervalStart) $ fold
                [ output 0
                , runOptionTrees (False, False) (handler n)
                ]

partitionIntervals :: [(InternalFloat, [OptionTree])] -> Generator ()
partitionIntervals points =
    let intervals = zip points (tail points)
    in
    fold $
        intervals <&> \((intervalStart, optionTrees), (intervalEnd, _)) ->
            translate intervalStart (intervalEnd - intervalStart) $ fold
                [ output 0
                , runOptionTrees (False, False) optionTrees
                ]

genIntervals :: [(InternalFloat, Generator ())] -> Generator ()
genIntervals points =
    let intervals = zip points (tail points)
    in
    fold $
        intervals <&> \((intervalStart, generator), (intervalEnd, _)) ->
            translate intervalStart (intervalEnd - intervalStart) $ fold
                [ output 0
                , generator
                ]
