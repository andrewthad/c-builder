{-# language DuplicateRecordFields #-}

module Language.C.Syntax
  ( Expr(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Literal(..)
  , Statement(..)
  , DesignatedInitializer(..)
  , Case(..)
  , Function(..)
  , Declaration(..)
    -- * Helpers for Literals
  , u32
  ) where

import Prelude hiding (id)

import Data.Builder.Catenable (Builder)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64,Word32)
import Language.C.Type (Type,Size,Signedness,Const)

import qualified Language.C.Type as Ty

type LabelId = Text
type VarId = Text
type MemberId = Text
type FunctionId = Text

data UnaryOp
  = PreIncrement
  | PostIncrement
  | PreDecrement
  | PostDecrement
  | Address
  | Indirection
  | Not
  | LogicalNot
  | Cast Type

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
  | String !Text
    -- ^ A string literal

-- | An inline expression.
data Expr
  = Comma Expr Expr
    -- ^ e.g. @a++, b++@
  | Call Expr (Builder Expr)
    -- ^ Call a function. The first argument is expr instead of @Text@
    -- because there is a way to call function pointers by dereferencing
    -- them first.
  | Constant Literal
  | Index Expr Expr
    -- ^ e.g. @foo[bar]@
  | Arrow Expr MemberId
    -- ^ e.g. @foo->bar@
  | Dot Expr MemberId
    -- ^ e.g. @foo.bar@
  | SizeOf Type
    -- ^ Use this to take the size of a type. Lowers to @sizeof(...)@.
  | SizeOfExpr Expr
    -- ^ Use this to take the size of an expression. Lowers to @sizeof(...)@.
  | OffsetOf
      -- ^ Use this to take the offset of a structure member. Lowers to @offsetof(...)@.
      Type -- This should be a struct type
      Text -- The member name
  | Unary UnaryOp Expr
    -- ^ Invoke a unary operator (e.g. @++@, @--@, @&@, @*@).
  | Binary BinaryOp Expr Expr
    -- ^ Invoke a binary operator (e.g. @+@, @>@)
  | Assign Expr Expr
    -- ^ Assignment mixed with infix operators is not currently supported.
  | Var VarId
    -- ^ Reference a variable or a function name.
  | DesignatedInitializers !(Builder DesignatedInitializer)
    -- ^ This is not really an expression, and it can only appear on the RHS of
    -- the initialization of a struct.
  | Array !(Builder Expr)
    -- ^ An array literal.
  | Initializers !(Builder Expr)
    -- ^ Initialized in braces, without names. (e.g. @{40, 100, 17}@)

-- | A function declaration
data Function = Function
  { returnType :: !Type
    -- ^ Return type
  , id :: !FunctionId
    -- ^ The name of the function
  , arguments :: !(Builder Declaration)
    -- ^ Typed arguments. Old-style C function declarations are not supported.
  , body :: !(Builder Statement)
    -- ^ Function body
  }

-- | The declaration of a variable
data Declaration = Declaration
  { type_ :: !Type
  , id :: !VarId
  }

data DesignatedInitializer = DesignatedInitializer
  { member :: !MemberId
  , expr :: !Expr
  }

-- | A single statement.
data Statement
  = Expr Expr
    -- ^ Evaluate the expression and discard the results. Lifts an expression
    -- into a statement.
  | Compound (Builder Statement)
    -- ^ Explicitly create a compound statement. Usually this is not needed
    -- since if-then-else and for loops take statement builders (instead of
    -- a single statement) as their argument. However, this can be useful
    -- for restricting variable scope. 
  | Declare Type VarId
    -- ^ Declare a variable (e.g. @int x;@). Use 'Initialize' to pair declaration
    -- with assignment.
  | Initialize Type Const VarId Expr
    -- ^ Declare and initialize a variable (e.g. @int x = z + 5;@).
    -- The @const@ modifier (e.g. @int const y@) prohibits subsequent reassignment.
  | InitializeStruct Type Const VarId (Builder DesignatedInitializer)
    -- ^ Declare and initialize a struct using designated initializers.
    -- For example: @struct foo x = {.bar = 19, .baz = a + b}@
  | IfThenElse Expr (Builder Statement) (Builder Statement)
  | IfThen Expr (Builder Statement)
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
    -- ^ The C @break@ keyword used to break out of @for@, @if@, and @switch@.
  | Label LabelId
    -- ^ C has a restriction that labels must be followed by statements.
    -- This causes compilation failures when a declaration follows a label.
    -- The solution is to place a semicolon after the label to create an
    -- empty statement. The encode functions add this semicolon for the user
    -- when it is needed so that the user does not shoulder this burden.
  | LabeledCompound LabelId (Builder Statement)
    -- ^ Fused @Label@ and @Compound@ that results in prettier output. 
  | Goto LabelId
    -- ^ Jump to the label with @goto@.
  | ReturnVoid
    -- ^ Return from a function (i.e. @return@) that returns @void@.
  | Return Expr
    -- ^ Return from a function (i.e. @return@) that returns a value.
  | Switch
      Expr -- ^ Selector expression
      (Builder Case) -- ^ Cases, may fall through
      (Builder Statement) -- ^ Default

data Case = Case
  { value :: Literal
    -- ^ The constant expression that must be matched for this case
    -- to be taken.
  , body :: Builder Statement
  }

u32 :: Word32 -> Literal
{-# inline u32 #-}
u32 w = Integer Ty.Unsigned (Ty.Fixed Ty.W32) (toInteger w)
