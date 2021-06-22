{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module SlideRules.FasterSVG where

-- base
import Data.Foldable
import Data.Function ((&))
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString
import Data.ByteString.Builder

-- linear
import Linear.V2

-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- local (sliderules)
import SlideRules.Types
import SlideRules.Tick

-- Calculating Tick bounds
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

-- Utils
show7 :: Show a => a -> Builder
show7 = string7 . show

newtype Cart = Cart (V2 InternalFloat)
    deriving (Num, Show)
cart x y = cartV2 (V2 x y)
cartV2 = Cart
uncurryC f cart = f (x cart) (y cart)
x (Cart (V2 x _)) = x
y (Cart (V2 _ y)) = negate y

-- Basic SVG utils
svg :: Builder -> Builder
svg content = fold
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
    , "<svg height=\"2200\" width=\"1200\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.11.1\" xmlns=\"http://www.w3.org/2000/svg\" "
    , "viewBox=\"-0.1 -2.1 1.2 2.2\""
    , ">"
    -- , "<rect width=\"1000\" height=\"1000\" fill=\"transparent\"></rect>"
    , content  -- & gTranslate (cart 500 (-500))
    , "</svg>"
    ]

gTransform :: Builder -> Builder -> Builder
gTransform transform children = fold
    [ "<g ", attribute "transform" transform, ">", children, "</g>" ]

gTranslate :: Cart -> Builder -> Builder
gTranslate cart = gTransform (fold ["translate(", show7 $ x cart, ",", show7 $ y cart, ")"])

gScale :: InternalFloat -> Builder -> Builder
gScale factor = gTransform (fold ["scale(", show7 factor, ",", show7 factor, ")"])

gRotate :: InternalFloat -> Builder -> Builder
gRotate pct = gTransform (fold ["rotate(", show7 (360 * pct), ")"])

textBdr :: Builder -> SimpleAnchor -> InternalFloat -> Builder
textBdr t simpleAnchor fontSize = fold
    [ "<text "
    , attribute "font-family" "Comfortaa"
    , " "
    , attribute "font-size" (string7 (show fontSize))
    , " "
    , simpleYToAttr (simpleY simpleAnchor)
    , " "
    , simpleXToAttr (simpleX simpleAnchor)
    , ">"
    , t
    , "</text>"
    ]

textUTF :: T.Text -> SimpleAnchor -> InternalFloat -> Builder
textUTF t simpleAnchor fontSize =
    textBdr (byteString $ T.encodeUtf8 t) simpleAnchor fontSize

segment :: ByteString -> Cart -> Builder
segment strokeColor cart = fold
    [ "<path ", attribute "stroke-width" "0.001", " ", attribute "d" (fold [ "M 0,0 l ", show7 $ x cart, ",", show7 $ y cart ]), " ", attribute "stroke" (byteString strokeColor), "/>" ]

attribute :: ByteString -> Builder -> Builder
attribute name value = fold
    [ byteString name, "=\"", value, "\"" ]

-- SVG anchors

data SimpleAnchorX = Start | Middle | End
data SimpleAnchorY = Bottom | Center | Top
data SimpleAnchor = SimpleAnchor { simpleX :: SimpleAnchorX, simpleY :: SimpleAnchorY }

roundSimple :: TextAnchor -> SimpleAnchor
roundSimple TextAnchor{..} = SimpleAnchor simpleX simpleY
    where
    simpleX
      | _xPct < 0.25 = Start
      | _xPct > 0.75 = End
      | otherwise = Middle
    simpleY
      | _yPct < 0.25 = Bottom
      | _yPct > 0.75 = Top
      | otherwise = Center

simpleYToAttr :: SimpleAnchorY -> Builder
simpleYToAttr Top    = attribute "dominant-baseline" "text-top"
simpleYToAttr Center = attribute "dominant-baseline" "central"
simpleYToAttr Bottom = attribute "dominant-baseline" "text-bottom"

simpleXToAttr :: SimpleAnchorX -> Builder
simpleXToAttr Start  = attribute "text-anchor" "start"
simpleXToAttr Middle = attribute "text-anchor" "middle"
simpleXToAttr End    = attribute "text-anchor" "end"

-- Converting ticks

tickToElement :: InternalFloat -> InternalFloat -> Tick -> Builder
tickToElement heightMultiplier textMultiplier tick =
    let staticTick = tickToElementStatic heightMultiplier textMultiplier tick
    in
    case _offset tick of
        Vertical y ->
            staticTick
                & gTranslate (cart (_postPos tick) (y * heightMultiplier))
        Radial rad ->
            error "implement"
        --     staticTick
        --         & D.translate (D.r2 (0, rad))
        --         & D.rotateBy (negate $ realToFrac $ _postPos tick)

tickToElementStatic :: InternalFloat -> InternalFloat -> Tick -> Builder
tickToElementStatic heightMultiplier textMultiplier tick =
    let Tick { _prePos, _postPos, _info } = tick
        TickInfo { _start, _end, _mlabel } = _info
        startV2 = cart 0 (heightMultiplier * _start)
        endV2   = cart 0 (heightMultiplier * _end)
        diffV2  = endV2 - startV2
        tickDia = segment "#FF0000" diffV2 & gTranslate startV2
        labelDia = fromMaybe mempty $ do
            Label {..} <- _mlabel
            let labelOffset :: Cart
                labelOffset
                  = cartV2 _anchorOffset * cart 1 heightMultiplier
                  + case _tickAnchor of
                      Pct p -> startV2 + diffV2 * cart p p
                      FromTopAbs x -> endV2 + cart 0 (heightMultiplier * x)
                      FromBottomAbs x -> startV2 + cart 0 (heightMultiplier * x)
            let 
            pure $ gTranslate labelOffset $
                textUTF
                    (T.pack _text)
                    (roundSimple _textAnchor)
                    (heightMultiplier * textMultiplier * _fontSize)
     in tickDia <> labelDia
