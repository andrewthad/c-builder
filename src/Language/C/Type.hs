module Language.C.Type
  ( Type(..)
  , Platform(..)
  , Width(..)
  , Size(..)
  , Signedness(..)
  , Restrict(..)
  , Const(..)
  ) where

import Data.Text.Short (ShortText)
import Language.C.Type.Width (Width(..))
import Language.C.Type.Signedness (Signedness(..))

import qualified Language.C.Type.X86 as X86

data Platform
  = Char
  | Short
  | Int
  | Long
  | LongLong
  | Size

-- Note: Char is weird. Using @char@ without a signedness
-- modified results in platform-defined behavior. It is always
-- either a signed char or an unsigned char though.
data Size
  = Fixed !Width
  | Platform !Platform

data Restrict
  = RestrictYes
  | RestrictNo

data Const
  = ConstYes
  | ConstNo

data Type
  = Integer Signedness Size
  | Float
  | Double
  | Void
  | Struct !ShortText
  | Typedef !ShortText
  | Pointer !Type
  | X86Vector !X86.Vector
  | X86Mask !Width
