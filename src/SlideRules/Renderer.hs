{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module SlideRules.Renderer where

import Linear.V2

import SlideRules.Tick hiding (renderTick, renderTickStatic)
import SlideRules.Types

data Proxy a = Proxy

-- Rendering settings
data RenderSettings fl = RenderSettings
    { heightMultiplier :: fl
    , textMultiplier :: fl
    , padding :: fl
    , lineWidth :: fl
    , xPow :: Int
    , yPow :: Int
    }

class Renderer a fl where
    type Representation a fl :: *
    renderTick :: Proxy (a, fl) -> RenderSettings fl -> Tick fl -> Representation a fl
    renderTickStatic :: Proxy (a, fl) -> RenderSettings fl -> Tick fl -> Representation a fl
    renderTicks :: Foldable f => Proxy (a, fl) -> RenderSettings fl -> f (Tick fl) -> Representation a fl
    writeRepToFile :: Proxy (a, fl) -> FilePath -> Representation a fl -> IO ()

-- Calculating Tick bounds for better rendering
data Bounds fl = Bounds { lower :: fl, upper :: fl }
    deriving (Show)
mkBounds x y = Bounds { lower = min x y, upper = max x y }

originBounds :: Num fl => Bounds fl
originBounds = Bounds 0 0

instance Ord fl => Semigroup (Bounds fl) where
    (<>) bounds1 bounds2 = Bounds { lower = min (lower bounds1) (lower bounds2), upper = max (upper bounds1) (upper bounds2) }

newtype Bounds2D fl = Bounds2D (V2 (Bounds fl))
    deriving (Show, Semigroup)

originBounds2D :: Num fl => Bounds2D fl
originBounds2D = Bounds2D (V2 originBounds originBounds)

bounds :: (Num fl, Ord fl, Floating fl) => Tick fl -> Bounds2D fl
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
