module TPX.Com.API.Aeson (
    mergeObject
    ) where


import              Data.Aeson
import qualified    Data.HashMap.Strict                     as  HM


mergeObject :: Value -> Value -> Value
mergeObject (Object a) (Object b) = Object $ HM.unionWith mergeObject a b
mergeObject _ b = b
