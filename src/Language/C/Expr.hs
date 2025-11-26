{-# language PatternSynonyms #-}

-- | Everything in this module returns an Expr.
-- These are helper to make it easier to build C expressions.
module Language.C.Expr
  ( Expr(..)
    -- * Object Members
  , pattern (:.)
  , pattern (:->)
    -- * Arithmetic
  , pattern (:+)
  , pattern (:*)
  , pattern (:>)
  , pattern (:<)
  , pattern (:==)
    -- * Assignment
  , pattern (:=)
    -- * Integer Literals
  , pattern U8
  , pattern U16
  , pattern U32
  , pattern U64
    -- * Unary Ops
  , pattern PostIncr
  , pattern PreIncr
  , pattern Ind
  , pattern Addr
    -- * Calling Functions
  , call1
  , call2
  , call3
  ) where

import Data.Text (Text)
import Language.C.Syntax

import qualified Language.C.Type as Ty
import qualified Data.Builder.Catenable as B

infixl 1 :.
pattern (:.) :: Expr -> Text -> Expr
pattern obj :. mbr = Dot obj mbr

infixl 1 :->
pattern (:->) :: Expr -> Text -> Expr
pattern obj :-> mbr = Arrow obj mbr

infixl 4 :+
pattern (:+) :: Expr -> Expr -> Expr
pattern a :+ b = Binary Add a b

infixl 3 :*
pattern (:*) :: Expr -> Expr -> Expr
pattern a :* b = Binary Mul a b

infixl 6 :<
pattern (:<) :: Expr -> Expr -> Expr
pattern a :< b = Binary Lt a b

infixl 6 :>
pattern (:>) :: Expr -> Expr -> Expr
pattern a :> b = Binary Gt a b

infixl 7 :==
pattern (:==) :: Expr -> Expr -> Expr
pattern a :== b = Binary Eq a b

-- Note: precedence level of assignment should be 14 but Haskell
-- cannot go that high 
infixl 9 :=
pattern (:=) :: Expr -> Expr -> Expr
pattern a := b = Assign a b

pattern U8 :: Integer -> Expr
pattern U8 i = Constant (Integer Ty.Unsigned (Ty.Fixed Ty.W8) i)

pattern U16 :: Integer -> Expr
pattern U16 i = Constant (Integer Ty.Unsigned (Ty.Fixed Ty.W16) i)

pattern U32 :: Integer -> Expr
pattern U32 i = Constant (Integer Ty.Unsigned (Ty.Fixed Ty.W32) i)

pattern U64 :: Integer -> Expr
pattern U64 i = Constant (Integer Ty.Unsigned (Ty.Fixed Ty.W64) i)

-- All of the unary ops are given more brief names here to avoid conflicts

pattern PostIncr :: Expr -> Expr
pattern PostIncr e = Unary PostIncrement e

pattern PreIncr :: Expr -> Expr
pattern PreIncr e = Unary PostIncrement e

pattern Ind :: Expr -> Expr
pattern Ind e = Unary Indirection e

pattern Addr :: Expr -> Expr
pattern Addr e = Unary Address e

call1 :: Text -> Expr -> Expr
call1 fn arg0 = Call (Var fn) (B.singleton arg0)

call2 :: Text -> Expr -> Expr -> Expr
call2 fn arg0 arg1 = Call (Var fn) (B.doubleton arg0 arg1)

call3 :: Text -> Expr -> Expr -> Expr -> Expr
call3 fn arg0 arg1 arg2 = Call (Var fn) (B.tripleton arg0 arg1 arg2)
