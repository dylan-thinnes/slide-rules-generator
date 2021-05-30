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
    , out       :: S.Seq Tick
    }
    deriving (Show)

generate :: Effect (State GenState) a -> GenState
generate eff = execState (runEffect eff) (GenState [] [] $ S.fromList [])

sinkAll :: Consumer a (State GenState) ()
sinkAll = forever await

output :: Consumer Tick (State GenState) ()
output = forever $ do
    x <- await
    lift $ modify $ \s -> s { out = out s <> S.fromList [x] }

outputEx :: Consumer InternalFloat (State GenState) ()
outputEx = PP.map (\x -> def { postPos = x }) >-> output

ex55 :: Producer InternalFloat (State GenState) ()
ex55 = enumerate $ do
    x <- Select $ each [1..10]
    y <- Select $ each [1..x]
    return $ x * 10 + y
