module SlideRules.Generator where

-- base
import qualified Data.Sequence as S

-- mtl
import Control.Monad.State

-- pipes
import Pipes

-- local (sliderules)
import SlideRules.Tick
import SlideRules.Transformations

data GenState b = GenState
    { preTrans  :: [Transformation]
    , postTrans :: [Transformation]
    , out       :: S.Seq b
    }
    deriving (Show)

type Generator b = ListT (State (GenState b))

generate :: Generator b a -> GenState b
generate g = execState (runListT g) (GenState [] [] $ S.fromList [])

output :: a -> Generator a a
output x = do
    lift $ modify (\s -> s { out = out s <> S.fromList [x] })
    pure x

ex55 :: Generator b Int
ex55 = do
    x <- Select $ each [1..10]
    y <- Select $ each [1..10]
    guard $ y <= x
    pure $ x * 10 + y

ex1000 :: Generator b Int
ex1000 = do
    x <- Select $ each [1..10]
    y <- Select $ each [1..10]
    z <- Select $ each [1..10]
    pure $ x * 100 + y * 10 + z

ex1 :: Generator Int Int
ex1 = join $ Select $ each $ replicate 3 $ ex55 >>= output

ex2 :: Generator Int Int
ex2 = do
    ex55 >>= output
    ex55 >>= output
    ex55 >>= output
