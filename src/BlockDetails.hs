{-# LANGUAGE TemplateHaskell #-}

module BlockDetails where

import           Data.Aeson          (ToJSON, genericToJSON, omitNothingFields,
                                      toJSON)
import           Data.Aeson.Casing   (aesonDrop, snakeCase)
import           Control.Lens
import           Relude hiding (state)

data Align = Center

instance ToJSON Align where
  toJSON Center =  toJSON ("center" :: Text)

data BlockDetails =
  BlockDetails
    { _fullText     :: Text
    , _shortText    :: Maybe Text
    , _background   :: Maybe Text
    , _border       :: Maybe Text
    , _borderTop    :: Maybe Int
    , _borderRight  :: Maybe Int
    , _borderBottom :: Maybe Int
    , _borderLeft   :: Maybe Int
    , _urgent       :: Maybe Bool
    , _minWidth     :: Maybe Text
    , _align        :: Align
    , _state        :: Maybe Text
    }
  deriving (Generic)

makeClassy ''BlockDetails

defaultBlockDetails :: BlockDetails
defaultBlockDetails = BlockDetails "" Nothing Nothing (Just "#FFFFFF") Nothing Nothing Nothing Nothing Nothing Nothing Center Nothing

instance ToJSON BlockDetails where
  toJSON = genericToJSON ((aesonDrop 1 snakeCase) {omitNothingFields = True})
