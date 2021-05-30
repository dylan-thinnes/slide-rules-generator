{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module SlideRules.Generator where

-- base
import qualified Data.Sequence as S

-- default
import Data.Default

-- mtl
import Control.Monad.State

-- pipes
import Pipes
import qualified Pipes.Prelude as PP

-- local (sliderules)
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

data GenState = GenState
    { preTransformations  :: [Transformation]
    , postTransformations :: [Transformation]
    , currTick            :: InternalFloat -> TickInfo
    , out                 :: S.Seq Tick
    }
    -- deriving (Show)

genTick :: InternalFloat -> GenState -> Maybe Tick
genTick x s = do
    let info = currTick s x
    prePos <- runTransformations (preTransformations s) x
    postPos <- runTransformations (postTransformations s) prePos
    pure $ Tick { info, prePos, postPos }

instance Default GenState where
    def = GenState [] [] (const def) $ S.fromList []

generate :: Effect (State GenState) a -> GenState
generate eff = execState (runEffect eff) def

outputAll, output :: Consumer InternalFloat (State GenState) ()
outputAll = forever output
output = PP.wither (gets . genTick) `for` \tick ->
    modify $ \s -> s { out = out s <> S.fromList [tick] }

preTransform :: MonadState GenState m => Transformation -> m r -> m r
preTransform transformation pipe = do
    modify $ \s -> s { preTransformations = transformation : preTransformations s }
    r <- pipe
    modify $ \s -> s { preTransformations = tail $ preTransformations s }
    pure r

postTransform :: MonadState GenState m => Transformation -> m r -> m r
postTransform transformation pipe = do
    modify $ \s -> s { postTransformations = transformation : postTransformations s }
    r <- pipe
    modify $ \s -> s { postTransformations = tail $ postTransformations s }
    pure r

ex55 :: Producer InternalFloat (State GenState) ()
ex55 = enumerate $ do
    x <- Select $ each [1..10]
    y <- Select $ each [1..x]
    return $ x * 10 + y

ex100 :: Producer InternalFloat (State GenState) ()
ex100 =
    each [1..9]
    `for`
    \x -> preTransform (Offset $ fromIntegral x) $ preTransform (Scale 0.1) (each [0..9])
