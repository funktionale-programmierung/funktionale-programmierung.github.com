{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.LargeHashable
import GHC.Generics

data Name = Name { firstName :: String, lastName :: String }

instance LargeHashable Name where
    updateHash name =
        do updateHash (firstName name)
           updateHash (lastName name)

data Person = Person { name :: Name, age :: Int }
            deriving (Generic)

instance LargeHashable Person

data Car = Car { company :: String, model :: String, year :: Int }

$(deriveLargeHashable ''Car)
