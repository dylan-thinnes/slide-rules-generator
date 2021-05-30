{-# LANGUAGE NamedFieldPuns #-}
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
    { preTrans  :: [Transformation]
    , postTrans :: [Transformation]
    , currTick  :: InternalFloat -> TickInfo
    , out       :: S.Seq Tick
    }
    -- deriving (Show)

genTick :: InternalFloat -> GenState -> Maybe Tick
genTick x s = do
    let info = currTick s x
    prePos <- runTransformations (preTrans s) x
    postPos <- runTransformations (postTrans s) prePos
    pure $ Tick { info, prePos, postPos }

instance Default GenState where
    def = GenState [] [] (const def) $ S.fromList []

generate :: Effect (State GenState) a -> GenState
generate eff = execState (runEffect eff) def

outputAll, output :: Consumer InternalFloat (State GenState) ()
outputAll = forever output
output = PP.loop calculateOutput >-> do
    tick <- await
    lift $ modify $ \s -> s { out = out s <> S.fromList [tick] }
    where
        calculateOutput :: InternalFloat -> ListT (State GenState) Tick
        calculateOutput x = do
            Just tick <- gets $ genTick x
            pure tick

outputEx :: Consumer InternalFloat (State GenState) ()
outputEx = outputAll

ex55 :: Producer InternalFloat (State GenState) ()
ex55 = enumerate $ do
    x <- Select $ each [1..10]
    y <- Select $ each [1..x]
    return $ x * 10 + y
