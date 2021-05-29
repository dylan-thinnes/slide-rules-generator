module SlideRules.Generator where

-- base
import qualified Data.Sequence as S

-- default
import Data.Default

-- mtl
import Control.Monad.State

-- pipes
import Pipes

-- local (sliderules)
import SlideRules.Tick
import SlideRules.Transformations

data GenState = GenState
    { preTrans  :: [Transformation]
    , postTrans :: [Transformation]
    , out       :: S.Seq Tick
    }
    deriving (Show)

type Generator = ListT (State GenState)

generate :: Generator b -> GenState
generate g = execState (runListT g) (GenState [] [] $ S.fromList [])

output :: Tick -> Generator ()
output x = lift $ modify' (\s -> s { out = out s <> S.fromList [x] })

ex55 :: Generator Int
ex55 = do
    x <- Select $ each [1..10]
    y <- Select $ each [1..10]
    guard $ y <= x
    pure $ x * 10 + y

ex1000 :: Generator Int
ex1000 = do
    x <- Select $ each [1..10]
    y <- Select $ each [1..10]
    z <- Select $ each [1..10]
    pure $ x * 100 + y * 10 + z

exOutput = \x -> output $ def { postPos = fromIntegral x }

ex1 :: Generator ()
ex1 = join $ Select $ each $ replicate 3 $ ex55 >>= exOutput

ex2 :: Generator ()
ex2 = do
    ex55 >>= exOutput
    ex55 >>= exOutput
    ex55 >>= exOutput
