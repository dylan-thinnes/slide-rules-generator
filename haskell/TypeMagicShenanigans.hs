{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module TypeMagicShenanigans where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

import qualified Data.Sequence as S

import GHC.TypeLits

main = error "hmm"

total = bgFrame 0.1 white $ vcat [a,b,c]

a, b, c :: Diagram B
a = logTicks 1 True
b = logTicks 2 False
c = logTicks 3 False

logTicks :: Int -> Bool -> Diagram B
logTicks pow above = mconcat $ mkTick <$> tickNs
    where
        tickNs = do
            p <- [0..pow-1]
            n <- [1..if p == pow - 1 then 10 else 9]
            pure $ n * 10 ^ p
        mkTick n =
            let x = logBase (10 ^ pow) (fromIntegral n)
                origTick = laserline [r2 (0, 0.04)]
                label
                    = alignedText 0.5 (if above then 0.0 else 1.0) (show $ floor $ logClamp 10 $ fromIntegral n)
                    & fontSizeL 0.015 & fc black & font "monospace"
                withLabel = vsep 0.005 $ (if above then id else reverse) [label, origTick]
                --origTick = laserline [r2 (0, 0.04)] & translate (r2 (0, -0.02))
                --label
                --    = alignedText 0.5 0.5 (show $ floor $ logClamp 10 $ fromIntegral n)
                --    & fontSizeL 0.015 & fc black & font "monospace"
                --withLabel = label `atop` origTick
             in translate (r2 (x, 0)) withLabel

laserline positions = fromOffsets positions & lineWidth ultraThin

logClamp :: Double -> Double -> Double
logClamp base x
    | x >= base  = logClamp base (x / base)
    | x < 1     = logClamp base (x * base)
    | otherwise = x

example = lwG 0.05 . mconcat . map fromOffsets
         $ [ [r *^ e (r @@ rad)]
           | r <- [33 * tau/32, 34 * tau/32 .. 2 * tau]
           ]

--data Tick = Tick
--    { value :: Double
--    , position :: Double
--    , height :: Double
--    , name :: String
--    }
--
--data Template = Template
--    { height :: Double
--    , format :: Format
--    }
--
--data Format = Format
--    { post :: PostFormat
--    , pre :: PreTransform
--    }
--
--data PreTransform
--    = NoPreTransform
--    | LogClamp Double
--    | CustomPreTransform (Double -> Double)
--
--data PostFormat
--    = NoPostFormat
--    | Int
--    | Float Int -- Float w/ precision
--    | Only String
--    | CustomPostFormat (Double -> String)
--
--data Spec = Spec
--    { count :: Int
--    , inclStart :: Bool
--    , inclEnd :: Bool
--    , template :: Template
--    , subSpecs :: [Spec]
--    }

-- Annotations
data Ann t = Ann { ann :: String, val :: t }
instance Show (Ann t) where
    show (Ann s _) = s

-- Desugar typeclass w/ rules
class DesugarSucc n f where
    desugarSucc :: f (n + 1) -> f n

class Desugar n f where
    desugar :: f n -> f 0

instance {-# OVERLAPPING #-} Desugar 0 f where
    desugar = id

instance (m ~ (n + 1), Desugar n f, DesugarSucc n f) => Desugar m f where
    desugar = desugar @n . desugarSucc @n

-- Sugar typeclass w/ rules
class ResugarSucc n f where
    resugarSucc :: f n -> f (n + 1)

class Resugar n f where
    resugar :: f 0 -> f n

instance {-# OVERLAPPING #-} Resugar 0 f where
    resugar = id

instance (m ~ (n + 1), Resugar n f, ResugarSucc n f) => Resugar m f where
    resugar = resugarSucc @n . resugar @n

-- Point generators
data PointGen n where
    MkPG0 :: PG0 -> PointGen 0
    MkPG1 :: PG1 -> PointGen 1

instance Show (PointGen n) where
    show (MkPG0 pg0) = show pg0
    show (MkPG1 pg1) = show pg1

data PG0
  = Single Double
  | Join (S.Seq PG0) -- combine tick generators, superimpose
  | IterateN Int (Ann (Int -> Double -> Double)) (PointGen 0)
  deriving Show

single :: Resugar n PointGen => Double -> PointGen n
single = resugar . MkPG0 . Single

data PG1
  = PG0 (PointGen 0)
  | OffsetN Int Double (PointGen 1)
  | PowN Int Double (PointGen 1)
  deriving Show

run :: Desugar n PointGen => PointGen n -> S.Seq Double
run pgn = let MkPG0 pg0 = desugar pgn in run0 pg0
    where
    run0 :: PG0 -> S.Seq Double
    run0 (Single x) = [x]
    run0 (Join xs) = foldMap run0 xs
    run0 (IterateN n f gen) = do
        i <- [0..n-1]
        x <- run gen
        pure $ val f i x

instance DesugarSucc 0 PointGen where
    desugarSucc (MkPG1 pg1) = MkPG0 $ desugar1to0 pg1
        where
        desugar1to0 :: PG1 -> PG0
        desugar1to0 (PG0 pg0) = let (MkPG0 pg) = desugar pg0 in pg -- leaky, nontypechecked axiom
        desugar1to0 (OffsetN n step gen) =
            IterateN
                n
                (Ann "OffsetN" $ \i x -> x + step * fromIntegral i)
                (desugar gen)
        desugar1to0 (PowN n base gen) =
            IterateN
                n
                (Ann "OffsetN" $ \p x -> x * base ** fromIntegral p)
                (desugar gen)

instance ResugarSucc 0 PointGen where
    resugarSucc = MkPG1 . PG0

pg1 = MkPG1 $ OffsetN 10 1 (single 1)
--ticks = run pg1
