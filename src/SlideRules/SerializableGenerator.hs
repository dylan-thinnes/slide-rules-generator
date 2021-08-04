{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module SlideRules.SerializableGenerator where

-- aeson
import Data.Aeson
import qualified Data.Aeson.Parser as Parser

-- base
import GHC.Generics

-- decimal
import Data.Decimal

-- lens
import Control.Lens (use, (%~), (.~))

-- scientific
import Data.Scientific

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Tick
import SlideRules.Utils

data SerializableGenerator
    = HardcodedPoints { transformations :: [Transformation], controlPoints :: [Scientific] }
    | SmartPartitionTens { transformations :: [Transformation], controlPoints :: [Scientific] }
    deriving (Show, Generic)

deriving instance Generic Transformation
instance ToJSON Transformation
instance FromJSON Transformation

instance ToJSON SerializableGenerator
instance FromJSON SerializableGenerator

interpretSerializableGen :: SerializableGenerator -> Generator ()
interpretSerializableGen (HardcodedPoints { transformations, controlPoints }) =
    withs (postTransform <$> transformations) $ do
        traverse output $ scientificToInternalFloat <$> controlPoints
        pure ()
interpretSerializableGen (SmartPartitionTens { transformations, controlPoints }) =
    withs (postTransform <$> transformations) $ do
        smartPartitionTens smartHandler $ scientificToDecimal <$> controlPoints
        pure ()

-- Partitioning handler
smartHandler 1 = trees10
smartHandler 10 = trees10
smartHandler n = [treeN n]

partN n h = Partition n 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
treeN n = OptionTree [partN n 0.5] [(0, n - 1, trees10)]
trees10 =
    [ OptionTree [partN 2 0.75, partN 5 0.66] [(0, 9, trees10)]
    , OptionTree [partN 5 0.5] []
    , OptionTree [partN 2 0.5] []
    ]

-----------------------------------------------------------
-- Conversion & JSON parsing
-----------------------------------------------------------

scientificToDecimal :: Scientific -> Decimal
scientificToDecimal s = Decimal (fromIntegral $ base10Exponent s) (coefficient s)

decimalToScientific :: Decimal -> Scientific
decimalToScientific d = scientific (decimalMantissa d) (fromIntegral $ decimalPlaces d)

scientificToInternalFloat :: Scientific -> InternalFloat
scientificToInternalFloat = realToFrac

internalFloatToScientific :: InternalFloat -> Scientific
internalFloatToScientific = realToFrac

instance FromJSON Decimal where
    parseJSON = withScientific "Decimal" (pure . scientificToDecimal)

instance ToJSON Decimal where
    toJSON = toJSON . decimalToScientific

instance FromJSON InternalFloat where
    parseJSON = withScientific "Decimal" (pure . scientificToInternalFloat)

instance ToJSON InternalFloat where
    toJSON = toJSON . internalFloatToScientific

