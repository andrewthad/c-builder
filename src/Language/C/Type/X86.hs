module Language.C.Type.X86
  ( Vector(..)
  , Width(..)
  , Element(..)
  ) where

-- | The type of a value backed by a vector register. There is no
-- support for the @__m64@ type. The following types can be represented:
--
-- * @__m128@
-- * @__m128d@
-- * @__m128i@
-- * @__m256@
-- * @__m256d@
-- * @__m256i@
-- * @__m512@
-- * @__m512d@
-- * @__m512i@
data Vector = Vector
  { width :: !Width
  , element :: !Element
  }
  deriving (Eq)

-- | Width of a vector register.
data Width
  = W128
  | W256
  | W512
  deriving (Eq)

data Element
  = Double
    -- ^ Double-precision floating point element, 64 bits of precision
  | Float
    -- ^ Floating point element, 32 bits of precision
  | Integer
    -- ^ Integral element, could be 8-bit, 16-bit, 32-bit, or 64-bit
    -- depending on the operation used.
  deriving (Eq)
