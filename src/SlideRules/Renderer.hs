{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module SlideRules.Renderer where

import Linear.V2

import SlideRules.Tick hiding (renderTick, renderTickStatic)
import SlideRules.Types

data Proxy a = Proxy

-- Rendering settings
data RenderSettings = RenderSettings
    { heightMultiplier :: InternalFloat
    , textMultiplier :: InternalFloat
    , padding :: InternalFloat
    , lineWidth :: InternalFloat
    , xPow :: Int
    , yPow :: Int
    }

class Renderer a where
    type Representation a :: *
    renderTick :: Proxy a -> RenderSettings -> Tick -> Representation a
    renderTickStatic :: Proxy a -> RenderSettings -> Tick -> Representation a
    renderTicks :: Proxy a -> RenderSettings -> [Tick] -> Representation a
    writeRepToFile :: Proxy a -> FilePath -> Representation a -> IO ()

-- Calculating Tick bounds for better rendering
data Bounds = Bounds { lower :: InternalFloat, upper :: InternalFloat }
    deriving (Show)
mkBounds x y = Bounds { lower = min x y, upper = max x y }
originBounds = Bounds 0 0

instance Semigroup Bounds where
    (<>) bounds1 bounds2 = Bounds { lower = min (lower bounds1) (lower bounds2), upper = max (upper bounds1) (upper bounds2) }

newtype Bounds2D = Bounds2D (V2 Bounds)
    deriving (Show, Semigroup)
originBounds2D = Bounds2D (V2 originBounds originBounds)

bounds :: Tick -> Bounds2D
bounds tick@Tick { _postPos, _offset, _info = TickInfo { _start, _end } } =
    case _offset of
        Vertical y ->
            let xStart = _postPos
                yStart = y + _start
                xEnd = _postPos
                yEnd = y + _end
            in
            Bounds2D (V2 (mkBounds xStart xEnd) (mkBounds yStart yEnd))
        Radial r ->
            let xScale = cos (2 * pi * _postPos)
                yScale = sin (2 * pi * _postPos)
                xStart = xScale * (r + _start)
                yStart = yScale * (r + _start)
                xEnd = xScale * (r + _end)
                yEnd = yScale * (r + _end)
            in
            Bounds2D (V2 (mkBounds xStart xEnd) (mkBounds yStart yEnd))
