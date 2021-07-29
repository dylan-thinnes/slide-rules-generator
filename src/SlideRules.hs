{-# LANGUAGE TypeApplications #-}
module SlideRules where

-- base
import Data.Foldable (fold)
import Data.Function ((&))
import Numeric

-- decimal
import Data.Decimal

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- lens
import Control.Lens ((%~), (^.), (.~), _2)

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils
import SlideRules.Scales
import SlideRules.Renderer
import qualified SlideRules.Renderer.Diagrams as SRD

mainText = fromInfo $ end .~ 1 <<< label %~ (labelCenterOver 0.05 <<< fontSize .~ 0.5)
mainTextUnder = fromInfo $ end .~ 1 <<< label %~ (labelCenterUnder 0.1 <<< fontSize .~ 0.5)

basicC :: Bool -> Generator ()
basicC tickAtEnd = withTickCreator mainText $ do
    withInfo (label %~ labelCenterOver 0 <<< start .~ 0.6 <<< end .~ 0.7 <<< label . text .~ "π") $ output pi
    withInfo (label %~ labelCenterOver 0 <<< start .~ 0.6 <<< end .~ 0.7 <<< label . text .~ "e") $ output e
    preTransform (Offset 1) $ preTransform (Scale 9) $
        let part9  = Partition 9 0 $ fromXInfo $ \x -> end .~ 1 <<< label %~ (text .~ showIOrF (show . fst . sigExp) (showF round) x)
            part2  = Partition 2 0 $ fromInfo (end %~ (* 0.75) <<< mlabel .~ Nothing)
            part5  = Partition 5 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
            part10 = Partition 10 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
            tree = fillOptionTree [part9] subtrees
            subtrees =
                [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
                , OptionTree [part5] []
                , OptionTree [part2] []
                ]
         in runOptionTrees (True, tickAtEnd) [tree]

c :: Generator ()
c = postTransform (Log 10) (basicC True)

cNoEnd :: Generator ()
cNoEnd = postTransform (Log 10) (basicC False)

basicCU :: Bool -> Generator ()
basicCU tickAtEnd = withTickCreator mainTextUnder $ do
    withInfo (label %~ labelCenterUnder (-0.05) <<< start .~ (-0.6) <<< end .~ (-0.7) <<< label . text .~ "π") $ output pi
    withInfo (label %~ labelCenterUnder (-0.05) <<< start .~ (-0.6) <<< end .~ (-0.7) <<< label . text .~ "e") $ output e
    preTransform (Offset 1) $ preTransform (Scale 9) $
        let part9  = Partition 9 0 $ fromXInfo $ \x -> end .~ (-1) <<< label %~ (text .~ showIOrF (show . fst . sigExp) (showF round) x)
            part2  = Partition 2 0 $ fromInfo (end %~ (* 0.75) <<< mlabel .~ Nothing)
            part5  = Partition 5 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
            part10 = Partition 10 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
            tree = fillOptionTree [part9] subtrees
            subtrees =
                [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
                , OptionTree [part5] []
                , OptionTree [part2] []
                ]
         in runOptionTrees (True, tickAtEnd) [tree]

cu :: Generator ()
cu = postTransform (Log 10) (basicCU True)

ci :: Generator ()
ci = postTransform Flip $ postTransform (Log 10) (basicC True)

cf :: Generator ()
cf
  = postTransform (Log 10)
  $ postTransform (Scale (1 / pi))
  $ do
    basicC False
    preTransform (Scale 10) (basicC True)

a :: Generator ()
a = postTransform (Log 100) $ do
    basicC False
    preTransform (Scale 10) (basicC True)

aNoEnd :: Generator ()
aNoEnd = postTransform (Log 100) $ do
    basicC False
    preTransform (Scale 10) (basicC False)

k :: Generator ()
k = postTransform (Log 1000) $ do
    basicC False
    preTransform (Scale 10) (basicC False)
    preTransform (Scale 100) (basicC True)

part2  h = Partition 2 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
part3  h = Partition 3 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
part4  h = Partition 4 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
part5  h = Partition 5 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
tree2  = OptionTree [part2 0.5] [(0, 1, trees10)]
tree3  = OptionTree [part3 0.5] [(0, 2, trees10)]
tree4  = OptionTree [part4 0.5] [(0, 3, trees10)]
tree5  = OptionTree [part5 0.5] [(0, 4, trees10)]
trees10 =
    [ OptionTree [part2 0.75, part5 0.66] [(0, 9, trees10)]
    , OptionTree [part5 0.5] []
    , OptionTree [part2 0.5] []
    ]
smartHandler 1 = trees10
smartHandler 10 = trees10
smartHandler 2 = [tree2]
smartHandler 3 = [tree3]
smartHandler 4 = [tree4]
smartHandler 5 = [tree5]
smartHandler i = error $ "Called with partition " ++ show i

l :: Generator ()
l = let part10 =
            Partition 10 0 $ fromXInfo $ \x ->
                label
                    %~ (labelRight 0.002
                    <<< fontSize .~ 0.4
                    <<< text .~ showIOrF show (dropWhile (== '0') . show) x)
    in
    withTickCreator mainText $
        runOptionTrees (True, True) [OptionTree [part10] [(0, 9, trees10)]]


ll :: Generator ()
ll =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        shower :: InternalFloat -> Maybe String
        shower = showIOrF (handleInt =<< sigExp) handleFloat
            where
                handleInt (m, e) i
                  | e >= 5 && m /= 1 = Nothing
                  | e >= 4           = Just $ showEFloat (Just 0) (fromIntegral i) ""
                  | otherwise        = Just $ show i
                handleFloat = Just . showM
        showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ shower x))
    in
    withs
        [ postTransform (Offset 0)
        , postTransform (LogLog 10)
        , withTickCreator (showTC . labelTC)
        ] $ do
        output 1e10
        smartPartitionTens smartHandler
            [ 1.002, 1.0025, 1.003, 1.004, 1.005, 1.006, 1.007, 1.008, 1.009, 1.010, 1.015, 1.02 -- , 1.025
            , 1.025, 1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.09, 1.10, 1.15, 1.2 -- , 1.25
            , 1.25, 1.30, 1.35, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5, 3, 4, 5, 6, 7, 8, 9 -- , 10
            , 10, 15, 20, 30, 40, 50, 1e2, 2e2, 5e2, 1e3, 2e3, 5e3, 1e4, 2e4, 5e4, 1e5, 5e5, 1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9, 5e9, 1e10
            ]

st :: Generator ()
st =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        showTC = fromXInfo $ \x -> label %~ (text .~ showIOrF show show x)
    in
    withs
        [ postTransform (Log 10)
        , postTransform (Scale 100)
        , postTransform Tan
        , withTickCreator (showTC . labelTC)
        ] $ do
        smartPartitionTens smartHandler
            [ 0.5, 0.6, 0.7, 0.8, 0.9, 1.0
            , 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0
            ]

s :: Generator ()
s =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        shower :: InternalFloat -> Maybe String
        shower = showIOrF (handleInt =<< sigExp) handleFloat
            where
                handleInt (m, e) i
                  | i == 80   = Nothing
                  | otherwise = Just $ show i
                handleFloat = Just . showF round
        showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ shower x))
    in
    withs
        [ postTransform (Log 10)
        , postTransform (Scale 10)
        , postTransform Sin
        , withTickCreator (showTC . labelTC)
        ] $ do
        output 90
        smartPartitionTens smartHandler
            [ 5.5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 35, 40, 50, 60, 70, 80, 90 ]

t :: Generator ()
t =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        showTC = fromXInfo $ \x -> label %~ (text .~ showIOrF show show x)
    in
    withs
        [ postTransform (Log 10)
        , postTransform Tan
        , withTickCreator (showTC . labelTC)
        ] $ do
        smartPartitionTens smartHandler
            [ 5.5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 35, 40 -- , 45
            , 45, 50, 55, 60, 65, 70, 75, 80, 81, 82, 83, 84, 85
            ]

sqrt1to2 :: Generator ()
sqrt1to2 =
    withs
        [ postTransform (Log 10)
        , postTransform (Pow 2)
        ] (basicC True)

cbrt1to3 :: Generator ()
cbrt1to3 =
    withs
        [ postTransform (Log 10)
        , postTransform (Pow 3)
        ] (basicC True)

cSpecLong :: ScaleSpec
cSpecLong = ScaleSpec
    { baseTolerance = 0.03
    , tickIdentifier = floorIdentifier 0.00001 (0, 100)
    , generator =
        withs
            [ postTransform (Scale 100)
            , postTransform (Log 10)
            , withXInfo $ \x -> label %~ (labelRightCenter 0.002 <<< fontSize .~ 0.5 <<< text .~ (show $ decimalMantissa $ normalizeDecimal $ roundTo 8 $ realToFrac x))
            , preTransform (Offset 1)
            , preTransform (Scale 9)
            ] $
            let tree2 = OptionTree [Partition 2 0 id] []
                tree5 = OptionTree [Partition 5 0 id] []
                tree10 = fillOptionTree [Partition 10 0 id] [tree10, tree5, tree2]
                optionTree = fillOptionTree [Partition 9 0 id] [tree10, tree5, tree2]
            in
            runOptionTrees (True, True) [optionTree]
    , offsetter = incline 0.02
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0.05
            , lineWidth = 0.0003
            , xPow = 3
            , yPow = 3
            }
    }

cSpec :: ScaleSpec
cSpec = ScaleSpec
    { baseTolerance = 0.002
    , tickIdentifier = defaultIdentifier
    , generator = cNoEnd
    , offsetter = noOffset
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0.05
            , lineWidth = 0.0003
            , xPow = 3
            , yPow = 3
            }
    }

aSpec :: ScaleSpec
aSpec = cSpec { generator = aNoEnd }

cSpecCircular :: ScaleSpec
cSpecCircular = ScaleSpec
    { baseTolerance = 0.0015
    , tickIdentifier = defaultIdentifier
    , generator =
        postTransform (Log 10) $
        withTickCreator (fromInfo (label %~ labelRight 0.002) . mainText) $ do
            withInfo (label %~ labelCenterOver 0 <<< start .~ 0.6 <<< end .~ 0.7 <<< label . text .~ "π") $ output pi
            withInfo (label %~ labelCenterOver 0 <<< start .~ 0.6 <<< end .~ 0.7 <<< label . text .~ "e") $ output e
            preTransform (Offset 1) $ preTransform (Scale 9) $
                let part9  = Partition 9 0 $ fromXInfo $ \x -> end .~ 1 <<< label %~ (text .~ showIOrF (show . fst . sigExp) (showF round) x)
                    part2  = Partition 2 0 $ fromInfo (end %~ (* 0.75) <<< mlabel .~ Nothing)
                    part5  = Partition 5 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
                    part10 = Partition 10 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
                    tree = fillOptionTree [part9] subtrees
                    subtrees =
                        [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
                        , OptionTree [part5] []
                        , OptionTree [part2] []
                        ]
                 in runOptionTrees (True, False) [tree]
    , offsetter = unitRadius 1.2
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0.05
            , lineWidth = 0.0003
            , xPow = 3
            , yPow = 3
            }
    }

lSpecCircular :: ScaleSpec
lSpecCircular = ScaleSpec
    { baseTolerance = 0.0015
    , tickIdentifier = defaultIdentifier
    , generator =
        let part10 =
                Partition 10 0 $ fromXInfo $ \x ->
                    label
                        %~ (labelRight 0.002
                        <<< fontSize .~ 0.4
                        <<< text .~ showIOrF show (dropWhile (== '0') . showM) x)
        in
        withTickCreator mainText $
            runOptionTrees (True, False) [OptionTree [part10] [(0, 9, trees10)]]
    , offsetter = unitRadius 0.9
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0.05
            , lineWidth = 0.0003
            , xPow = 3
            , yPow = 3
            }
    }

cSpecCircularUpsideDown :: ScaleSpec
cSpecCircularUpsideDown = ScaleSpec
    { baseTolerance = 0.0015
    , tickIdentifier = defaultIdentifier
    , generator =
        postTransform (Log 10) $
        withTickCreator (fromInfo (label %~ labelRightAbove 0.002 0 <<< end .~ -1) . mainText) $ do
            withInfo (label %~ labelCenterUnder 0 <<< end .~ (-0.6) <<< start .~ (-0.7) <<< label . text .~ "π") $ output pi
            withInfo (label %~ labelCenterUnder 0 <<< end .~ (-0.6) <<< start .~ (-0.7) <<< label . text .~ "e") $ output e
            preTransform (Offset 1) $ preTransform (Scale 9) $
                let part9  = Partition 9 0 $ fromXInfo $ \x -> label %~ (text .~ showIOrF (show . fst . sigExp) (showF round) x)
                    part2  = Partition 2 0 $ fromInfo (end %~ (* 0.75) <<< mlabel .~ Nothing)
                    part5  = Partition 5 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
                    part10 = Partition 10 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
                    tree = fillOptionTree [part9] subtrees
                    subtrees =
                        [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
                        , OptionTree [part5] []
                        , OptionTree [part2] []
                        ]
                 in runOptionTrees (True, False) [tree]
    , offsetter = unitRadius 1.2
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0.05
            , lineWidth = 0.0003
            , xPow = 3
            , yPow = 3
            }
    }

cSpecCircularUpsideDownInverted :: ScaleSpec
cSpecCircularUpsideDownInverted = ScaleSpec
    { baseTolerance = 0.0015
    , tickIdentifier = defaultIdentifier
    , generator =
        postTransform (Log 10) $
        postTransform Invert $
        withTickCreator (fromInfo (label %~ labelRightAbove 0.002 0 <<< end .~ -1) . mainText) $ do
            withInfo (label %~ labelCenterUnder 0 <<< end .~ (-0.6) <<< start .~ (-0.7) <<< label . text .~ "π") $ output pi
            withInfo (label %~ labelCenterUnder 0 <<< end .~ (-0.6) <<< start .~ (-0.7) <<< label . text .~ "e") $ output e
            preTransform (Offset 1) $ preTransform (Scale 9) $
                let part9  = Partition 9 0 $ fromXInfo $ \x -> label %~ (text .~ showIOrF (show . fst . sigExp) (showF round) x)
                    part2  = Partition 2 0 $ fromInfo (end %~ (* 0.75) <<< mlabel .~ Nothing)
                    part5  = Partition 5 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
                    part10 = Partition 10 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
                    tree = fillOptionTree [part9] subtrees
                    subtrees =
                        [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
                        , OptionTree [part5] []
                        , OptionTree [part2] []
                        ]
                 in runOptionTrees (True, False) [tree]
    , offsetter = unitRadius 0.9
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0.05
            , lineWidth = 0.0003
            , xPow = 3
            , yPow = 3
            }
    }

llSpec :: ScaleSpec
llSpec = ScaleSpec
    { baseTolerance = 0.0015
    , tickIdentifier = defaultIdentifier
    , generator = ll
    , offsetter = unitArchimedes 2 0.03
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.015
            , textMultiplier = 4 / 3
            , padding = 0
            , lineWidth = 0.001
            , xPow = 3
            , yPow = 3
            }
    }

lliSpec :: ScaleSpec
lliSpec = ScaleSpec
    { baseTolerance = 0.0015
    , tickIdentifier = defaultIdentifier
    , generator =
        let labelTC = fromInfo $ label %~ (labelRightAbove 0.002 0 <<< fontSize .~ 0.35)
            shower x
                | x < 0.01 =
                    showEFloat (Just 0) (realToFrac x :: Double) ""
                | otherwise = showM x
            showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ Just $ shower x))
            upsideDownTC = fromInfo $ end .~ (-1)
        in
        withs
            [ postTransform (Offset 0)
            , postTransform (LogLog 10)
            , postTransform Invert
            , withTickCreator (upsideDownTC . showTC . labelTC)
            ] $ do
            output 1e-10
            smartPartitionTens smartHandler
                [ 0.998, 0.9975, 0.997, 0.996, 0.995, 0.99, 0.98 --, 0.97
                , 0.97, 0.96, 0.95, 0.94, 0.93, 0.92, 0.91, 0.90, 0.85, 0.80 --, .75
                , 0.75, 0.70, 0.65, 0.60, 0.55, 0.50, 0.45, 0.40, 0.35, 0.30, 0.25, 0.20, 0.15 --, 0.10
                , 0.10, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-7, 1e-7, 5e-8, 1e-8, 5e-9, 1e-9, 5e-10, 1e-10
                ]
    , offsetter = unitArchimedes 2 0.03
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.015
            , textMultiplier = 4 / 3
            , padding = 0
            , lineWidth = 0.001
            , xPow = 3
            , yPow = 3
            }
    }

tSpec :: ScaleSpec
tSpec = ScaleSpec
    { baseTolerance = 0.002
    , tickIdentifier = floorIdentifier 0.00001 (-1, 1)
    , generator = t
    , offsetter = noOffset
    , renderSettings =
        RenderSettings
            { heightMultiplier = 0.02
            , textMultiplier = 1
            , padding = 0
            , lineWidth = 0.001
            , xPow = 3
            , yPow = 1
            }
    }

example = do
    writeRepToFile (Proxy @SRD.Dias) "ex1.svg" $
        fold
            [ lasercircle 0.025
            , laserline [D.r2 (0, 0), D.r2 (0, 0.05)]
            , laserline [D.r2 (0, 0), D.r2 (0.05, 0)]
            , lasercircle 0.191 & D.lc D.blue
            , foldMap (renderScales (Proxy @SRD.Dias))
                [ llSpec
                , lliSpec
                , cSpecCircular
                ]
            ]
    writeRepToFile (Proxy @SRD.Dias) "ex2.svg" $
        fold
            [ lasercircle 0.025
            , laserline [D.r2 (0, 0), D.r2 (0, 0.05)]
            , laserline [D.r2 (0, 0), D.r2 (0.05, 0)]
            , lasercircle 0.191 & D.lc D.blue
            , foldMap (renderScales (Proxy @SRD.Dias))
                [ cSpecCircularUpsideDown
                , lSpecCircular
                , cSpecCircularUpsideDownInverted
                ]
            ]
