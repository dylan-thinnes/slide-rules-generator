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

generate :: ListT (State GenState) a -> GenState
generate act = execState (runListT act) def

genTick :: InternalFloat -> GenState -> Maybe Tick
genTick x s = genTickWithInfo x id s

genTickWithInfo :: InternalFloat -> (TickInfo -> TickInfo) -> GenState -> Maybe Tick
genTickWithInfo x infoF s = do
    let info = infoF $ currTick s x
    prePos <- runTransformations (preTransformations s) x
    postPos <- runTransformations (postTransformations s) prePos
    pure $ Tick { info, prePos, postPos }

instance Default GenState where
    def = GenState [] [] (const def) $ S.fromList []

together :: [ListT (State GenState) a] -> ListT (State GenState) a
together = join . Select . each

output :: InternalFloat -> ListT (State GenState) ()
output x = do
    Just tick <- gets $ genTick x
    modify $ \s -> s { out = out s <> S.fromList [tick] }

preTransform :: Transformation -> ListT (State GenState) a -> ListT (State GenState) ()
preTransform transformation pipe = do
    together
        [ modify $ \s -> s { preTransformations = transformation : preTransformations s }
        , pipe >> pure ()
        , modify $ \s -> s { preTransformations = tail $ preTransformations s }
        ]

postTransform :: Transformation -> ListT (State GenState) a -> ListT (State GenState) ()
postTransform transformation pipe = do
    together
        [ modify $ \s -> s { postTransformations = transformation : postTransformations s }
        , pipe >> pure ()
        , modify $ \s -> s { postTransformations = tail $ postTransformations s }
        ]

outputWithInfo :: InternalFloat -> (TickInfo -> TickInfo) -> ListT (State GenState) ()
outputWithInfo x infoF = do
    Just tick <- gets $ genTickWithInfo x infoF
    modify $ \s -> s { out = out s <> S.fromList [tick] }

ex100 :: ListT (State GenState) ()
ex100 = together
    [ do
        x <- Select $ each [1..9]
        output x
        postTransform (Offset x) $ postTransform (Scale 0.1) $ do
            x <- Select $ each [1..9]
            output x
    , outputWithInfo pi (\info -> info { start = 0.5, end = 0.6, mlabel = Just (def { fontSize = 10, text = "pi" }) })
    , outputWithInfo e (\info -> info { start = 0.5, end = 0.6, mlabel = Just (def { fontSize = 10, text = "e" }) })
    ]
