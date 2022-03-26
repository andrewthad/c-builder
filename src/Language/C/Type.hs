module Language.C.Type
  ( Type(..)
  , Width(..)
  , Size(..)
  , Signedness(..)
  ) where

import Data.Text.Short (ShortText)
import Language.C.Type.Width (Width(..))

import qualified Language.C.Type.X86 as X86

data Size
  = Fixed !Width
  | Short
  | Int
  | Long
  | LongLong

data Signedness
  = Signed
  | Unsigned

data Type
  = Integer Signedness Size
  | Char
    -- ^ The @char@ type that is not explicitly signed or unsigned.
  | Float
  | Double
  | Void
  | Struct !ShortText
  | Pointer !Type
  | X86Vector !X86.Vector
  | X86Mask !Width
