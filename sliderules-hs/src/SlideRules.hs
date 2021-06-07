module SlideRules where

-- base
import Data.Function ((&))
import Numeric

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- lens
import Control.Lens ((%~), (^.), (.~))

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

mainText = fromInfo $ end .~ 1 <<< label %~ (labelCenterOver 0.002 <<< fontSize .~ 0.5)

basicC :: Bool -> Generator ()
basicC tickAtEnd = preTransform (Offset 1) $ preTransform (Scale 9) $ withTickCreator mainText $
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

ci :: Generator ()
ci = postTransform Flip $ postTransform (Log 10) (basicC True)

cf :: Generator ()
cf
  = postPostTransform (Within 0 1)
  $ postTransform (Rotate $ logBase 10 pi)
  $ postTransform (Log 10)
  $ withTickCreator mainText
  $ do
    withInfo (start .~ 0.6 <<< end .~ 0.7 <<< label . text .~ "π") $ output pi
    withInfo (start .~ 0.6 <<< end .~ 0.7 <<< label . text .~ "e") $ output e
    basicC False

a :: Generator ()
a = postTransform (Log 100) $
    together
        [ basicC False
        , preTransform (Scale 10) (basicC True)
        ]

k :: Generator ()
k = postTransform (Log 1000) $
    together
        [ basicC False
        , preTransform (Scale 10) (basicC False)
        , preTransform (Scale 100) (basicC True)
        ]

part2  h = Partition 2 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
part3  h = Partition 3 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
part4  h = Partition 4 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
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

ll1 :: Generator ()
ll1 =
    withs
        [ postPostTransform (Within 0 1)
        , postTransform (Offset 3)
        , postTransform (LogLog 10)
        , withTickCreator (fromXInfo $ \x -> label %~ (labelRight 0.002 <<< text .~ show x <<< fontSize .~ 0.35))
        ] $
        smartPartitionTens smartHandler
            [ 1.002, 1.0025, 1.003, 1.004, 1.005, 1.006, 1.007, 1.008, 1.009, 1.010, 1.015, 1.02, 1.03 ]

ll2 :: Generator ()
ll2 =
    withs
        [ postPostTransform (Within 0 1)
        , postTransform (Offset 2)
        , postTransform (LogLog 10)
        , withTickCreator (fromXInfo $ \x -> label %~ (labelRight 0.002 <<< text .~ show x <<< fontSize .~ 0.35))
        ] $
        smartPartitionTens smartHandler
            [ 1.02, 1.025, 1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.09, 1.10, 1.15, 1.2, 1.25, 1.3 ]

ll3 :: Generator ()
ll3 =
    withs
        [ postPostTransform (Within 0 1)
        , postTransform (Offset 1)
        , postTransform (LogLog 10)
        , withTickCreator (fromXInfo $ \x -> label %~ (labelRight 0.002 <<< text .~ showIOrF show showM x <<< fontSize .~ 0.35))
        ] $ do
        output 10
        smartPartitionTens smartHandler
            [ 1.25, 1.30, 1.35, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5, 3, 4, 5, 6, 7, 8, 9, 10 ]

ll4 :: Generator ()
ll4 =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        shower :: InternalFloat -> Maybe String
        shower = showIOrF (handleInt =<< sigExp) handleFloat
            where
                handleInt (m, e) i
                  | e >= 5 && m /= 1 = Nothing
                  | e >= 4           = Just $ showEFloat (Just 0) (fromIntegral i) ""
                  | otherwise        = Just $ show i
                handleFloat = Just . showF round
        showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ shower x))
    in
    withs
        [ postPostTransform (Within 0 1)
        , postTransform (Offset 0)
        , postTransform (LogLog 10)
        , withTickCreator (showTC . labelTC)
        ] $ do
        output 1e10
        smartPartitionTens smartHandler
            [ 10, 15, 20, 30, 40, 50, 1e2, 2e2, 5e2, 1e3, 2e3, 5e3, 1e4, 2e4, 5e4, 1e5, 5e5, 1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9, 5e9, 1e10 ]

st :: Generator ()
st =
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
        [ postPostTransform (Within 0 1)
        , postTransform (Log 10)
        , postTransform (Scale 100)
        , postTransform Tan
        , withTickCreator (showTC . labelTC)
        ] $ do
        smartPartitionTens smartHandler
            [ 0.5, 0.6, 0.7, 0.8, 0.9, 1.0
            , 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0
            ]

t1 =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        shower :: InternalFloat -> Maybe String
        shower = showIOrF (handleInt =<< sigExp) handleFloat
            where
                handleInt (m, e) i
                  | e >= 5 && m /= 1 = Nothing
                  | e >= 4           = Just $ showEFloat (Just 0) (fromIntegral i) ""
                  | otherwise        = Just $ show i
                handleFloat = Just . showF round
        showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ shower x))
    in
    withs
        [ postPostTransform (Within 0 1)
        , postTransform (Log 10)
        , postTransform (Scale 10)
        , postTransform Tan
        , withTickCreator (showTC . labelTC)
        ] $ do
        smartPartitionTens smartHandler
            [ 5.5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 35, 40, 45 ]

t2 :: Generator ()
t2 =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        shower :: InternalFloat -> Maybe String
        shower = showIOrF (handleInt =<< sigExp) handleFloat
            where
                handleInt (m, e) i
                  | e >= 5 && m /= 1 = Nothing
                  | e >= 4           = Just $ showEFloat (Just 0) (fromIntegral i) ""
                  | otherwise        = Just $ show i
                handleFloat = Just . showF round
        showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ shower x))
    in
    withs
        [ postPostTransform (Within (-0.001) 1)
        , postTransform (Log 10)
        , postTransform Tan
        , withTickCreator (showTC . labelTC)
        ] $ do
        smartPartitionTens smartHandler
            [ 45, 50, 55, 60, 65, 70, 75, 80, 81, 82, 83, 84, 85 ]

s :: Generator ()
s =
    let labelTC = fromInfo $ label %~ (labelRight 0.002 <<< fontSize .~ 0.35)
        shower :: InternalFloat -> Maybe String
        shower = showIOrF (handleInt =<< sigExp) handleFloat
            where
                handleInt (m, e) i
                  | e >= 5 && m /= 1 = Nothing
                  | e >= 4           = Just $ showEFloat (Just 0) (fromIntegral i) ""
                  | otherwise        = Just $ show i
                handleFloat = Just . showF round
        showTC = fromXInfo $ \x -> mlabel %~ (>>= text (const $ shower x))
    in
    withs
        [ postPostTransform (Within (-0.001) 1)
        , postTransform (Log 10)
        , postTransform (Scale 10)
        , postTransform Sin
        , withTickCreator (showTC . labelTC)
        ] $ do
        smartPartitionTens smartHandler
            [ 5.5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 35, 40, 50, 60, 70, 80, 90 ]


renderSlide :: Settings -> Generator a -> D.Diagram D.B
renderSlide settings generator =
    let ticks = foldMap (renderTick False) $ _out $ generate settings generator
    in
    ticks <> D.lc D.blue (laserline [D.r2 (0, 0), D.r2 (1, 0)])
          <> D.lc D.green (laserline [D.r2 (0, 0), D.r2 (-0.01, 0), D.r2 (0, 0.01)])

total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 $ map (renderSlide $ Settings 0.002) [ c, ci, cf, a, k, ll1, ll2, ll3, ll4, s, st, t1, t2 ]
