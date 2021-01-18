module TPX.Com.API.Resource (
    ErrorN(..),
    ) where


import              Data.Aeson


newtype ErrorN = ErrorN { errorNDebug :: Text
    } deriving (Show)
instance ToJSON ErrorN where
    toJSON o = object [
        "msg" .= errorNDebug o]
