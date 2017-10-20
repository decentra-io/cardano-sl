module Lang.Value
       ( Value(..)
       , _ValueUnit
       , _ValueNumber
       , _ValueString
       , _ValueAddress
       , _ValueFilePath
       ) where

import           Universum

import           Control.Lens    (makePrisms)
import           Data.Scientific (Scientific)

import           Pos.Types       (Address)

data Value
    = ValueUnit
    | ValueNumber Scientific
    | ValueString String
    | ValueAddress Address
    | ValueFilePath FilePath
    deriving (Eq, Ord, Show)

makePrisms ''Value
