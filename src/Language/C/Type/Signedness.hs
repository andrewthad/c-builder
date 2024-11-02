{-# language BangPatterns #-}

module Language.C.Type.Signedness
  ( Signedness(..)
  ) where

import Data.Hashable

data Signedness
  = Signed
  | Unsigned
  deriving (Eq,Ord,Show)

instance Hashable Signedness where
  hashWithSalt !salt s = case s of
    Signed -> hashWithSalt salt (5697418 :: Int)
    Unsigned -> hashWithSalt salt (383719 :: Int)
