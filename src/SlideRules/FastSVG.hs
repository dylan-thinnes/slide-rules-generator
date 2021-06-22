{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module SlideRules.FastSVG where

-- base
import Data.Foldable
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.Exts (IsString(..))

-- linear
import Linear.V2

-- svg-builder
import Graphics.Svg

-- text
import qualified Data.Text as T
import Data.Text (Text)

-- local (sliderules)
import SlideRules.Types
import SlideRules.Tick
import SlideRules.Utils

newtype Cart = Cart (V2 InternalFloat)
    deriving (Num, Show)
cart x y = cartV2 (V2 x y)
cartV2 = Cart
uncurryC f cart = f (x cart) (y cart)
x (Cart (V2 x _)) = x
y (Cart (V2 _ y)) = negate y

writeX :: Element -> IO ()
writeX = writeFile "x.svg" . show . svg

textYTop, textYCenter, textYBottom, textXLeft, textXMiddle, textXRight :: Attribute
textYTop = Dominant_baseline_ <<- "text-top"
textYCenter = Dominant_baseline_ <<- "central"
textYBottom = Dominant_baseline_ <<- "text-bottom"
textXLeft = Text_anchor_ <<- "start"
textXMiddle = Text_anchor_ <<- "middle"
textXRight = Text_anchor_ <<- "end"

svg :: Element -> Element
svg content = fold
    [ doctype
    , with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "1200" , Height_ <<- "2200", ViewBox_ <<- "-0.1 -2.1 1.2 2.2"]
    ]

svgTranslate :: Cart -> Element -> Element
svgTranslate cart = g_ [ Transform_ <<- T.pack (concat ["translate(", show $ x cart, ", ", show $ y cart, ")"]) ]

--svgScale :: Cart -> Element -> Element
--svgScale cart = g_ [ Transform_ <<- T.pack (concat ["translate(", show $ x cart, ", ", show $ y cart, ")"]) ]

svgLine :: Text -> [Cart] -> Element
svgLine strokeColor segments =
    path_
        [ Stroke_ <<- strokeColor
        , Stroke_width_ <<- "0.001"
        , D_ <<- (mA 0 0 <> foldMap (uncurryC lR) segments)
        ]

tickToElement :: InternalFloat -> InternalFloat -> Tick -> Element
tickToElement heightMultiplier textMultiplier tick =
    let staticTick = tickToElementStatic heightMultiplier textMultiplier tick
    in
    case _offset tick of
        Vertical y ->
            staticTick
                & svgTranslate (cart (_postPos tick) (y * heightMultiplier))
        Radial rad ->
            error "implement"
        --     staticTick
        --         & D.translate (D.r2 (0, rad))
        --         & D.rotateBy (negate $ realToFrac $ _postPos tick)

tickToElementStatic :: InternalFloat -> InternalFloat -> Tick -> Element
tickToElementStatic heightMultiplier textMultiplier tick =
    let Tick { _prePos, _postPos, _info } = tick
        TickInfo { _start, _end, _mlabel } = _info
        startV2 = cart 0 (heightMultiplier * _start)
        endV2   = cart 0 (heightMultiplier * _end)
        diffV2  = endV2 - startV2
        tickDia = svgLine "#FF0000" [diffV2] & svgTranslate startV2
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
            pure $ svgTranslate labelOffset $
                text_
                    [ Font_size_ <<- T.pack (show $ heightMultiplier * textMultiplier * _fontSize)
                    , Font_family_ <<- "Comfortaa"
                    , simpleXToAttr $ simpleX $ roundSimple _textAnchor
                    , simpleYToAttr $ simpleY $ roundSimple _textAnchor
                    ]
                    (fromString _text)
     in tickDia <> labelDia

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

simpleYToAttr Top    = Dominant_baseline_ <<- "text-top"
simpleYToAttr Center = Dominant_baseline_ <<- "central"
simpleYToAttr Bottom = Dominant_baseline_ <<- "text-bottom"
simpleXToAttr Start  = Text_anchor_ <<- "start"
simpleXToAttr Middle = Text_anchor_ <<- "middle"
simpleXToAttr End    = Text_anchor_ <<- "end"

--extick = Tick 1 0.5 (Vertical 0) (TickInfo 0 1 Nothing)
extick =
    Tick
        { _prePos = 1.0
        , _postPos = 0.5 -- 0.0
        , _offset = Vertical 0.0
        , _info = TickInfo
            { _start = 0.0
            , _end = 1.0
            , _mlabel = Just (Label
                { _fontSize = 0.5
                , _text = "1"
                , _textAnchor = TextAnchor
                    { _xPct = 0.5
                    , _yPct = 0.0
                    }
                , _tickAnchor = FromTopAbs 0.0
                , _anchorOffset = V2 0.0 5.0e-2
                })
            }
        }
