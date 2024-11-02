{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}

import Prelude hiding (log)

import Test.Tasty (TestTree,defaultMain,testGroup)
import Test.Tasty.Golden (goldenVsString)
import Data.Bytes.Chunks (Chunks)
import Data.Builder.Catenable.Text (Builder)
import Language.C.Syntax (Expr(Arrow,Var,Assign,Index,Constant,Dot,Binary,Call,Unary))
import Language.C.Syntax (BinaryOp(Add,Lt))
import Language.C.Syntax (UnaryOp(PostIncrement))
import Language.C.Syntax (Literal(Integer))
import Language.C.Syntax (Statement(IfThen,IfThenElse,ReturnVoid,Return,Expr,Initialize))
import Language.C.Syntax (Statement(ForInitialize))
import Language.C.Type (Signedness(Signed,Unsigned))
import Language.C.Type (Type(Pointer))
import Language.C.Type (Size(Short,Int,Fixed))
import Language.C.Type (Width(W64,W8))
import Language.C.Type (Const(ConstYes,ConstNo))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Builder.Catenable.Text
import qualified Data.ByteString.Lazy as LB
import qualified Language.C.Encode as Encode
import qualified Language.C.Type as T

main :: IO ()
main = defaultMain tests

prepare :: Builder -> LB.ByteString
prepare =
    LB.fromStrict
  . Bytes.toByteString
  . Chunks.concat
  . Data.Builder.Catenable.Text.run

tests :: TestTree
tests = testGroup "test"
  [ goldenVsString "a" "expected/a.txt" $ do
      let e = Arrow (Arrow (Var "x") "foo") "bar"
      pure (prepare (Encode.expr_ e) <> "\n")
  , goldenVsString "b" "expected/b.txt" $ do
      let e = Assign (Var "y") (Index (Var "x") (Constant (Integer Signed Short 13)))
      pure (prepare (Encode.expr_ e) <> "\n")
  , goldenVsString "c" "expected/c.txt" $ do
      let e = IfThen (Var "y") [ReturnVoid]
      pure (prepare (Encode.statement "  " mempty e))
  , goldenVsString "d" "expected/d.txt" $ do
      let e = IfThenElse (Var "y")
            [Return (Constant (Integer Unsigned Int 100))]
            [ Expr (Assign (Var "x") (Dot (Var "bob") "age"))
            , Return (Var "x")
            ]
      pure (prepare (Encode.statement "  " mempty e))
  , goldenVsString "e" "expected/e.txt" $ do
      let e = IfThen (Var "y")
            [ Initialize (Pointer (T.Integer Unsigned (Fixed W64))) ConstNo "r"
                (Index (Var "arr") (Var "ix"))
            , Return (Binary Add (Var "r") (Constant (Integer Unsigned (Fixed W64) 100)))
            ]
      pure (prepare (Encode.statement "  " mempty e))
  , goldenVsString "f" "expected/f.txt" $ do
      let e = Binary Add
            (Binary Add (Var "r") (Constant (Integer Unsigned (Fixed W8) 12)))
            (Constant (Integer Unsigned (Fixed W8) 17))
      pure (prepare (Encode.expr_ e) <> "\n")
  , goldenVsString "g" "expected/g.txt" $ do
      let e = Binary Add (Var "r")
            (Binary Add
              (Constant (Integer Unsigned (Fixed W8) 12))
              (Constant (Integer Unsigned (Fixed W8) 17))
            )
      pure (prepare (Encode.expr_ e) <> "\n")
  , goldenVsString "h" "expected/h.txt" $ do
      let e = ForInitialize
            (T.Integer Unsigned Short)
            "i"
            (Constant (Integer Unsigned Short 0))
            (Binary Lt (Var "i") (Constant (Integer Unsigned Short 12)))
            (Unary PostIncrement (Var "i"))
            [Expr (Call (Var "putchar") [Var "i"])]
      pure (prepare (Encode.statement "  " mempty e))
  , goldenVsString "i" "expected/i.txt" $ do
      let e = IfThen (Var "y")
            [ Initialize (Pointer (T.Integer Unsigned (Fixed W64))) ConstYes "r"
                (Index (Var "arr") (Var "ix"))
            , Return (Binary Add (Var "r") (Constant (Integer Unsigned (Fixed W64) 100)))
            ]
      pure (prepare (Encode.statement "  " mempty e))
  ]
