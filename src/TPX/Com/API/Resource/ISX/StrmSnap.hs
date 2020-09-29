{-# OPTIONS_GHC -fno-warn-orphans #-}


module TPX.Com.API.Resource.ISX.StrmSnap (
    ) where


import              TPX.Com.API.Req
import              TPX.Com.API.Resource.ISX.Strm


instance ValidateJSON Strm
