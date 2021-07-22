{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module SlideRules.Types where

import Data.CReal
import Data.Default
import Control.DeepSeq
import GHC.Generics
import Data.Number.FixedPrec

--type InternalFloat = Double

--type InternalFloat = CReal 256
--
--instance NFData (CReal 256) where
--    rnf creal = deepseq () ()

type P20 = PPlus10 P10

type InternalFloat = FixedPrec P20

instance Default (FixedPrec P20) where
    def = 0

instance NFData (FixedPrec P20) where
    rnf creal = deepseq () ()
