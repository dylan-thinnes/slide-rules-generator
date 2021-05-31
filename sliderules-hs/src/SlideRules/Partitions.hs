{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module SlideRules.Partitions where

-- base
import Control.Monad

-- lens
import Control.Lens (use)

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
    show Partition { _n } = "Partition { _n = " ++ show _n ++ " }"

mkPartition :: Integer -> Partition
mkPartition n = Partition { _n = n, _startAt = 0, _tickCreatorF = id }

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

meetsTolerance :: InternalFloat -> [Partition] -> Generator Bool
meetsTolerance tolerance partitions = do
    let a = 1 - 1 / fromIntegral (product $ map _n partitions)
    let b = 1
    (_, visualDistance) <- measure a b
    pure $ abs visualDistance > tolerance

data OptionTree = OptionTree
    { partitions  :: [Partition]
    , nextOptions :: [OptionTree]
    }

instance Show OptionTree where
    show OptionTree { partitions } = "OptionTree { partitions = " ++ show partitions ++ " }"

bestPartitions :: InternalFloat -> [OptionTree] -> Generator ()
bestPartitions tolerance = go
    where
        go options = do
            meetingTolerance <- filterM (meetsTolerance tolerance . partitions) options
            case meetingTolerance of
                [] -> pure ()
                (OptionTree { partitions, nextOptions } : _) ->
                    runPartitions partitions (go nextOptions)
