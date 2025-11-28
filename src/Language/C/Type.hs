{-# language PatternSynonyms #-}

module Language.C.Type
  ( Type(..)
  , Platform(..)
  , Width(..)
  , Size(..)
  , Signedness(..)
  , Restrict(..)
  , Static(..)
  , Const(..)
    -- * Patterns for common types
  , pattern Signed8
  , pattern Signed16
  , pattern Signed32
  , pattern Signed64
  , pattern SignedChar
  , pattern Unsigned8
  , pattern Unsigned16
  , pattern Unsigned32
  , pattern Unsigned64
  , pattern UnsignedChar
  , pattern UnsignedSize
  ) where

import Data.Text (Text)
import Language.C.Type.Signedness (Signedness(..))
import Language.C.Type.Width (Width(..))

import qualified Language.C.Type.X86 as X86

pattern Signed8 :: Type
pattern Signed8 = Integer Signed (Fixed W8)

pattern Signed16 :: Type
pattern Signed16 = Integer Signed (Fixed W16)

pattern Signed32 :: Type
pattern Signed32 = Integer Signed (Fixed W32)

pattern Signed64 :: Type
pattern Signed64 = Integer Signed (Fixed W64)

pattern Unsigned8 :: Type
pattern Unsigned8 = Integer Unsigned (Fixed W8)

pattern Unsigned16 :: Type
pattern Unsigned16 = Integer Unsigned (Fixed W16)

pattern Unsigned32 :: Type
pattern Unsigned32 = Integer Unsigned (Fixed W32)

pattern Unsigned64 :: Type
pattern Unsigned64 = Integer Unsigned (Fixed W64)

pattern UnsignedSize :: Type
pattern UnsignedSize = Integer Unsigned (Platform Size)

pattern UnsignedChar :: Type
pattern UnsignedChar = Integer Unsigned (Platform Char)

pattern SignedChar :: Type
pattern SignedChar = Integer Signed (Platform Char)

data Platform
  = Char
  | Short
  | Int
  | Long
  | LongLong
  | Size
  | IntPtr
  deriving (Eq)

-- Note: Char is weird. Using @char@ without a signedness
-- modified results in platform-defined behavior. It is always
-- either a signed char or an unsigned char though.
data Size
  = Fixed !Width
  | Platform !Platform
  deriving (Eq)

data Restrict
  = RestrictYes
  | RestrictNo
  deriving (Eq)

data Static
  = StaticYes
  | StaticNo
  deriving (Eq)

data Const
  = ConstYes
  | ConstNo
  deriving (Eq)

data Type
  = Integer Signedness Size
  | Float
  | Double
  | Void
  | Struct !Text
  | Typedef !Text
  | Pointer !Type
  | Array !Type
  | X86Vector !X86.Vector
  | X86Mask !Width
  | Const !Type -- a type qualified by by "const", do not nest these
  deriving (Eq)
