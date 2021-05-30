{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module SlideRules.Generator where

-- base
import qualified Data.Sequence as S

-- default
import Data.Default

-- lens
import Control.Lens.Combinators hiding (each)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)

-- mtl
import Control.Monad.State

-- pipes
import Pipes
import qualified Pipes.Prelude as PP

-- local (sliderules)
import SlideRules.Lenses
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

data GenState = GenState
    { _preTransformations  :: [Transformation]
    , _postTransformations :: [Transformation]
    , _currTick            :: InternalFloat -> TickInfo
    , _out                 :: S.Seq Tick
    }
    -- deriving (Show)

makeLenses ''GenState

generate :: ListT (State GenState) a -> GenState
generate act = execState (runListT act) def

genTick :: InternalFloat -> GenState -> Maybe Tick
genTick x s = genTickWithInfo x id s

genTickWithInfo :: InternalFloat -> (TickInfo -> TickInfo) -> GenState -> Maybe Tick
genTickWithInfo x infoF s = do
    let info = infoF $ _currTick s x
    prePos <- runTransformations (_preTransformations s) x
    postPos <- runTransformations (_postTransformations s) prePos
    pure $ Tick { info, prePos, postPos }

instance Default GenState where
    def = GenState [] [] (const def) $ S.fromList []

together :: [ListT (State GenState) a] -> ListT (State GenState) a
together = join . Select . each

withPrevious :: Lens' GenState a -> (a -> a) -> ListT (State GenState) b -> ListT (State GenState) ()
withPrevious lens f action = do
    previous <- use lens
    together
        [ lens %= f
        , action >> pure ()
        , lens .= previous
        ]

preTransform :: Transformation -> ListT (State GenState) a -> ListT (State GenState) ()
preTransform transformation = withPrevious preTransformations (transformation :)

postTransform :: Transformation -> ListT (State GenState) a -> ListT (State GenState) ()
postTransform transformation = withPrevious postTransformations (transformation :)

withInfo :: ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo) -> ListT (State GenState) a -> ListT (State GenState) ()
withInfo handlerF = withPrevious currTick handlerF

output :: InternalFloat -> ListT (State GenState) ()
output x = outputWithInfo x id

outputWithInfo :: InternalFloat -> (TickInfo -> TickInfo) -> ListT (State GenState) ()
outputWithInfo x infoF = do
    Just tick <- gets $ genTickWithInfo x infoF
    out <>= S.fromList [tick]

ex100 :: ListT (State GenState) ()
ex100 = together
    [ withInfo (\f x -> (f x) { mlabel = Just (def { fontSize = 10, text = showMax x }) }) $ do
        x <- Select $ each [1..9]
        output x
        preTransform (Offset x) $ preTransform (Scale 0.1) $ do
            x <- Select $ each [1..9]
            output x
    , withInfo (\f x -> (f x) { start = 0.5, end = 0.6, mlabel = Just (def { fontSize = 10, text = "pi" }) })
        (output pi)
    , withInfo (\f x -> (f x) { start = 0.5, end = 0.6, mlabel = Just (def { fontSize = 10, text = "e" }) })
        (output e)
    ]
