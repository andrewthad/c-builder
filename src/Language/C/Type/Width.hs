{-# language BangPatterns #-}

module Language.C.Type.Width
  ( Width(..)
  ) where

import Data.Hashable

-- | Widths of integral types.
data Width
  = W8
  | W16
  | W32
  | W64
  deriving (Eq,Ord,Show)

instance Hashable Width where
  hashWithSalt !salt w = case w of
    W8 -> hashWithSalt salt (2107409 :: Int)
    W16 -> hashWithSalt salt (2299148 :: Int)
    W32 -> hashWithSalt salt (7581846 :: Int)
    W64 -> hashWithSalt salt (4007641 :: Int)
