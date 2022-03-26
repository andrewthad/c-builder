module Language.C.Syntax
  ( Expr(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Literal(..)
  , Statement(..)
  ) where

import Data.Builder.Catenable (Builder)
import Language.C.Type (Type,Size,Signedness)
import Data.Text.Short (ShortText)
import Data.Int (Int64)
import Data.Word (Word64)

type LabelId = ShortText
type VarId = ShortText
type MemberId = ShortText

data UnaryOp
  = PreIncrement
  | PostIncrement
  | PreDecrement
  | PostDecrement
  | Address
  | Indirection

data BinaryOp
  = Mul
  | Div
  | Rem
  | Add
  | Sub
  | Shl -- ^ shift left
  | Shr -- ^ shift right
  | Lt -- ^ less
  | Gt -- ^ greater
  | Le -- ^ less or equal
  | Ge -- ^ greater or equal
  | Eq -- ^ equal
  | Ne -- ^ not equal
  | And -- ^ bitwise and
  | Xor -- ^ exclusive bitwise or
  | Or -- ^ inclusive bitwise or
  | LogicalAnd -- ^ logical and
  | LogicalOr -- ^ logical or

data Literal
  = Integer !Signedness !Size !Integer
  | Char !Char
    -- ^ Note that @char@ literals have type @int@ in C. Be careful
    -- using these. Sometimes, they can make @switch@ statements
    -- more readable. Do not use characters outside of the ASCII plane.
  | String !ShortText
    -- ^ A string literal

-- | An inline expression.
data Expr
  = Comma Expr Expr
    -- ^ e.g. @a++, b++@
  | Call Expr (Builder Expr)
  | Constant Literal
  | Index Expr Expr
    -- ^ e.g. @foo[bar]@
  | Arrow Expr MemberId
    -- ^ e.g. @foo->bar@
  | Dot Expr MemberId
    -- ^ e.g. @foo.bar@
  | SizeOf Type
    -- ^ Only taking the size of a type is supported. Taking the size of
    -- an expression is not useful when generating C. (Or really when
    -- writing C by hand for that matter.)
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Assign Expr Expr
    -- ^ Assignment mixed with infix operators is not currently supported.
  | Var VarId

-- | A single statement.
data Statement
  = Expr Expr
    -- ^ Evaluate the expression and discard the results. Lifts an expression
    -- into a statement.
  | Declare Type VarId
    -- ^ Declare a variable (e.g. @int x;@). Use 'Initialize' to pair declaration
    -- with assignment.
  | Initialize Type VarId Expr
    -- ^ Declare and initialize a variable (e.g. @int x = z + 5;@).
  | IfThen Expr (Builder Statement)
  | IfThenElse Expr (Builder Statement) (Builder Statement)
  | ForInitialize Type VarId Expr Expr Expr (Builder Statement)
    -- ^ A for loop where the first component is a declaration with an
    -- initializer. (e.g. @for(int i = 0; i < 30; i++)@).
  | For Expr Expr Expr (Builder Statement)
    -- ^ A for loop where the first component is an expression.
    -- Expressions in builders are joined with commas.
    -- (e.g. @for(a = 0, b = 50; a < 50; a++, b++)@).
    --
    -- Note: The standard allows programs to omit parts of the loop.
    -- That is not supported by this library.
  | Break
  | Label LabelId
    -- ^ C has a restriction that labels must be followed by statements.
    -- This causes compilation failures when a declaration follows a label.
    -- The solution is to place a semicolon after the label to create an
    -- empty statement. The encode functions add this semicolon for the user
    -- when it is needed so that the user does not shoulder this burden.
  | Goto LabelId
  | ReturnVoid
  | Return Expr
  | Switch
      (Builder Case) -- ^ Cases, may fall through
      (Builder Statement) -- ^ Default

data Case = Case
  { expr :: Expr
    -- ^ The constant expression that must be matched for this case
    -- to be taken.
  , body :: Builder Statement
  }

