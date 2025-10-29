{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language MultiWayIf #-}

module Language.C.Encode
  ( PrecBuilder(..)
  , statement
  , type_
  , signed
  , unsigned
  , literal
  , expr
  , expr_
  , function
  ) where

import Prelude hiding (id)

import Language.C.Type (Platform(Short,Int,Long,LongLong))
import Language.C.Type hiding (Platform(..))
import Language.C.Syntax hiding (type_,expr)
import Data.Text.Short (ShortText)
import Data.Builder.Catenable.Text (Builder,pattern (:<),pattern (:>))
import Data.Char (ord)
import Data.Text (Text)
import Data.Primitive (SmallArray)

import qualified Data.Chunks as Chunks
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C
import qualified Language.C.Syntax as S
import qualified Language.C.Type as T
import qualified Language.C.Type.X86 as X86
import qualified Data.Text as T
import qualified Data.Text.Short as TS
import qualified Data.Builder.Catenable.Text as Builder
import qualified Data.Builder.Catenable as BoxedBuilder

arrowPrec :: Int
arrowPrec = 1

dotPrec :: Int
dotPrec = 1

indexPrec :: Int
indexPrec = 1

functionCallPrec :: Int
functionCallPrec = 1

assignPrec :: Int
assignPrec = 14

commaPrec :: Int
commaPrec = 15

wrap :: Builder -> Builder
wrap e = ("(" :< e) :> ")"

-- Includes a space after the keyword.
constToKeyword :: Const -> Text
constToKeyword = \case
  ConstYes -> "const "
  ConstNo -> T.empty

function :: Function -> Builder
function Function{returnType,id,arguments,body} =
  (type_ returnType :> " " :> id :> "(")
  <>
  (commaIntercalate declaration (Chunks.concat (BoxedBuilder.run arguments)) :> ") {\n")
  <>
  (foldMap (statement "  " "  ") (BoxedBuilder.run body) :> "}\n")

declaration :: Declaration -> Builder
declaration Declaration{type_=t,id=v} = type_ t :> " " :> v

statement ::
     Builder -- ^ Character, or sequence of characters, that is used to indent one level
  -> Builder -- ^ Current indentation level
  -> Statement
  -> Builder
statement oneLevel indentation stmt0 = case stmt0 of
  Expr e -> indentation <> expr_ e <> ";\n"
  ReturnVoid -> indentation <> "return;\n"
  Label name -> indentation <> Builder.text name <> ": ;\n"
  LabeledCompound label statements ->
       indentation
    <> Builder.text label
    <> ": {\n"
    <> foldMap (statement oneLevel (indentation <> oneLevel)) (BoxedBuilder.run statements)
    <> indentation
    <> "}\n"
  Compound statements ->
       indentation
    <> "{\n"
    <> foldMap (statement oneLevel (indentation <> oneLevel)) (BoxedBuilder.run statements)
    <> indentation
    <> "}\n"
  Goto name -> indentation <> "goto " <> Builder.text name <> ";\n"
  Declare t v ->
       indentation
    <> (type_ t :> " " :> v :> ";\n")
  Initialize t c v e ->
    let PrecBuilder{prec,builder} = exprSafeForCleanLiteral e
        rhs = if prec > assignPrec
          then wrap builder
          else builder
     in indentation
        <>
        (type_ t :> " " :> constToKeyword c :> v :> " = ")
        <>
        (rhs :> ";\n")
  InitializeStruct t c v ds ->
    let ds' = Chunks.concat (BoxedBuilder.run ds)
     in indentation
        <>
        (type_ t :> " " :> constToKeyword c :> v :> " = {")
        <>
        (encodeDesignatedInitializers ds' :> "};\n")
  ForInitialize t v e cond post stmts ->
       indentation
    <> ("for (" :< (type_ t :> " " :> v :> " = "))
    <> (exprSafeForCleanLiteral_ e :> "; ")
    <> (expr_ cond :> "; ")
    <> (expr_ post :> ") {\n")
    <> foldMap (statement oneLevel (indentation <> oneLevel)) (BoxedBuilder.run stmts)
    <> (indentation :> "}\n")
  Return e ->
       indentation
    <> "return "
    <> expr_ e
    <> ";\n"
  Switch e valCases defCase ->
       indentation
    <> "switch ("
    <> expr_ e
    <> ") {\n"
    <> foldMap
       (\Case{value,body} ->
         let body' = Chunks.concat (BoxedBuilder.run body) in
         indentation'
         <>
         "case "
         <>
         ( case literal value of
             PrecBuilder{builder} -> builder
         )
         <>
         ":"
         <>
         ( case PM.sizeofSmallArray body' of
             0 -> mempty
             _ -> case PM.indexSmallArray body' 0 of
               InitializeStruct{} -> " ;"
               Initialize{} -> " ;"
               Declare{} -> " ;"
               _ -> mempty
         )
         <>
         "\n"
         <>
         foldMap
           (statement oneLevel indentation'')
           body'
       ) (BoxedBuilder.run valCases)
    <> indentation'
    <> "default:\n"
    <> foldMap
         (statement oneLevel indentation'')
         (BoxedBuilder.run defCase)
    <> indentation
    <> "}\n"
    where
    !indentation' = indentation <> oneLevel
    !indentation'' = indentation' <> oneLevel
  IfThen e a ->
       let a' = Chunks.concat (BoxedBuilder.run a) in
       indentation
    <> "if ("
    <> expr_ e
    <> ") "
    <> ( if | length a' == 1, Expr a0 <- PM.indexSmallArray a' 0 -> expr_ a0 :> ";\n"
            | otherwise -> "{\n"
               <> foldMap (statement oneLevel (indentation <> oneLevel)) a'
               <> (indentation :> "}\n")
       )
  IfThenElse e a b ->
    let a' = Chunks.concat (BoxedBuilder.run a) in
    let b' = Chunks.concat (BoxedBuilder.run b) in
       indentation
    <> "if ("
    <> expr_ e
    <> ") "
    <> ( if | length a' == 1
            , length b' == 1
            , Expr a0 <- PM.indexSmallArray a' 0
            , Expr b0 <- PM.indexSmallArray b' 0 ->
                expr_ a0 <> ";\n" <> indentation <> "else " <> expr_ b0 <> ";\n"
            | otherwise -> "{\n"
                <> foldMap (statement oneLevel (indentation <> oneLevel)) a'
                <> indentation
                <> "} else {\n"
                <> foldMap (statement oneLevel (indentation <> oneLevel)) b'
                <> indentation
                <> "}\n"
       )

expr_ :: Expr -> Builder
expr_ e = let PrecBuilder{builder} = expr e in builder

-- Note: All precedence 2 operators are prefix. All precedence 1
-- operators are postfix.
unaryOpPrec :: UnaryOp -> Int
unaryOpPrec = \case
  PostIncrement -> 1
  PostDecrement -> 1
  PreIncrement -> 2
  PreDecrement -> 2
  Address -> 2
  Indirection -> 2
  Not -> 2
  LogicalNot -> 2
  Cast{} -> 2

encodeUnaryOp :: UnaryOp -> Builder
encodeUnaryOp = \case
  PostIncrement -> "++"
  PostDecrement -> "--"
  PreIncrement -> "++"
  PreDecrement -> "--"
  Address -> "&"
  Indirection -> "*"
  Not -> "~"
  LogicalNot -> "!"
  Cast ty -> "(" <> type_ ty <> ")"

-- Note: all binary operators are left associative.
binOpPrec :: BinaryOp -> Int
binOpPrec = \case
  Mul -> 3
  Div -> 3
  Rem -> 3
  Add -> 4
  Sub -> 4
  Shl -> 5
  Shr -> 5
  Lt -> 6
  Gt -> 6
  Le -> 6
  Ge -> 6
  Eq -> 7
  Ne -> 7
  And -> 8
  Xor -> 9
  Or -> 10
  LogicalAnd -> 11
  LogicalOr -> 12

-- Include whitespace around the operator
encodeBinOp :: BinaryOp -> Text
encodeBinOp = \case
  Mul -> " * "
  Div -> " / "
  Rem -> " % "
  Add -> " + "
  Sub -> " - "
  Shl -> " >> "
  Shr -> " << "
  Lt -> " < "
  Gt -> " > "
  Le -> " <= "
  Ge -> " >= "
  Eq -> " == "
  Ne -> " != "
  And -> " & "
  Xor -> " ^ "
  Or -> " | "
  LogicalAnd -> " && "
  LogicalOr -> " || "

isBinOpComparison :: BinaryOp -> Bool
isBinOpComparison = \case
  Lt -> True
  Gt -> True
  Le -> True
  Ge -> True
  Eq -> True
  Ne -> True
  _ -> False

commaIntercalate :: (a -> Builder) -> SmallArray a -> Builder
{-# inline commaIntercalate #-}
commaIntercalate f !args = case PM.sizeofSmallArray args of
  0 -> mempty
  _ ->
    let arg0 = PM.indexSmallArray args 0
     in f arg0
        <>
        C.foldMap (\arg -> ", " :< f arg)  (C.slice args 1 (PM.sizeofSmallArray args - 1))

expr :: Expr -> PrecBuilder
expr = \case
  Constant x -> literal x
  Var v -> PrecBuilder{builder=v :< mempty,prec=0}
  SizeOf t -> PrecBuilder{builder="sizeof(" :< (type_ t :> ")"),prec=functionCallPrec}
  OffsetOf t field -> PrecBuilder{builder="offsetof(" :< (type_ t :> "," :> field :> ")"),prec=functionCallPrec}
  SizeOfExpr e ->
    let PrecBuilder{builder} = expr e
     in PrecBuilder{builder="sizeof(" :< (builder :> ")"),prec=functionCallPrec}
  Call f args ->
    let args' = Chunks.concat (BoxedBuilder.run args)
        args'' = commaIntercalate exprSafeForCleanLiteral_ args'
        PrecBuilder{prec=p,builder=f'} = expr f
        f'' = if p <= functionCallPrec then f' else wrap f'
     in PrecBuilder{builder=f'' <> ("(" :< (args'' :> ")")),prec=indexPrec}
  Binary op a b ->
    let opPrec = binOpPrec op
        PrecBuilder{builder=a',prec=precA} = if isBinOpComparison op
          then exprSafeForCleanLiteral a
          else expr a
        PrecBuilder{builder=b',prec=precB} = if isBinOpComparison op
          then exprSafeForCleanLiteral b
          else expr b
        a'' = if precA <= opPrec then a' else wrap a'
        b'' = if precB <= opPrec - 1 then b' else wrap b'
     in PrecBuilder{builder= (a'' :> encodeBinOp op) <> b'', prec=opPrec}
  Unary op a ->
    let opPrec = unaryOpPrec op
        PrecBuilder{builder=a',prec=precA} = expr a
        a'' = if precA <= opPrec then a' else wrap a'
     in case opPrec of
          -- All of the operators in precedence group 1 are postfix
          1 -> PrecBuilder{builder=a'' <> encodeUnaryOp op, prec=opPrec}
          _ -> PrecBuilder{builder=encodeUnaryOp op <> a'', prec=opPrec}
  Assign a b ->
    let PrecBuilder{builder=a',prec=precA} = expr a
        PrecBuilder{builder=b',prec=precB} = expr b
        a'' = if precA <= assignPrec - 1 then a' else wrap a'
        b'' = if precB <= assignPrec then b' else wrap b'
     in PrecBuilder{builder= (a'' :> " = ") <> b'', prec=assignPrec}
  Comma a b ->
    let PrecBuilder{builder=a'} = expr a
        PrecBuilder{builder=b'} = expr b
     in PrecBuilder{builder= (a' :> ", ") <> b', prec=commaPrec}
  Arrow e memberId ->
    let PrecBuilder{prec=p,builder=e'} = expr e
        e'' = if p <= arrowPrec then e' else wrap e'
     in PrecBuilder{builder=e'' :> "->" :> memberId,prec=arrowPrec}
  Dot e memberId ->
    let PrecBuilder{prec=p,builder=e'} = expr e
        e'' = if p <= dotPrec then e' else wrap e'
     in PrecBuilder{builder=e'' :> "." :> memberId,prec=dotPrec}
  Index e ix ->
    -- Omits most type annotations for numeric literals that are
    -- array indices.
    let PrecBuilder{prec=p,builder=e'} = expr e
        ix' = exprSafeForCleanLiteral_ ix
        e'' = if p <= indexPrec then e' else wrap e'
     in PrecBuilder{builder=e'' <> ("[" :< (ix' :> "]")),prec=indexPrec}
  DesignatedInitializers ds ->
    -- The precedence is made up because this cannot actually be used as
    -- an expression generally.
    let ds' = Chunks.concat (BoxedBuilder.run ds)
     in PrecBuilder{builder="{" :< (encodeDesignatedInitializers ds' :> "}"), prec=0}

literal :: Literal -> PrecBuilder
literal = \case
  S.String t
    | T.all (\c -> c > '\n' && c <= '~' && c /= '"') t ->
        PrecBuilder 0 ((T.singleton '"' :< Builder.text t) :> T.singleton '"')
    | otherwise -> error "Language.C.Encode.literal: figure out strings"
  S.Integer signedness size i ->
    let !enc = case signedness of
          -- We have a few special cases in here to present certain
          -- numbers in hexadecimal. These numbers tend to occur when
          -- performing bit arithmetic. The list is just based on
          -- situations that I ran into while using this library.
          Unsigned -> case size of
            Fixed W32 -> case i of
              4294967288 -> "0xFFFFFFF8" :< mempty
              4294967292 -> "0xFFFFFFFC" :< mempty
              4294967295 -> "0xFFFFFFFF" :< mempty
              _ -> T.pack (show i) :< mempty
            _ -> T.pack (show i) :< mempty
          Signed -> T.pack (show i) :< mempty
     in case signedness of
      Signed -> signedLiteralInteger size enc
      Unsigned -> unsignedLiteralInteger size enc
  S.Char c -> PrecBuilder 0 $ case c of
    '\n' -> "'\\n'"
    '\r' -> "'\\r'"
    '\t' -> "'\\t'"
    '\f' -> "'\\f'"
    '\'' -> "'\\''"
    _ | ord c >= 0x20 && ord c <= 0x7E ->
          T.singleton '\'' :< T.singleton c :< T.singleton '\'' :< mempty
    _ -> error "Language.C.Encode.literal: finish writing out character encoding"

data PrecBuilder = PrecBuilder
  { prec :: !Int
  , builder :: !Builder
  }

exprSafeForCleanLiteral_ :: Expr -> Builder
exprSafeForCleanLiteral_ e = case e of
  Constant (S.Integer Signed sz n) ->
    let !enc = T.pack (show n) :< mempty
     in cleanLiteralSignedInteger sz n enc
  Constant (S.Integer Unsigned sz n) ->
    let !enc = T.pack (show n) :< mempty
     in cleanLiteralUnsignedInteger sz n enc
  _ -> let PrecBuilder{builder=r} = expr e in r

exprSafeForCleanLiteral :: Expr -> PrecBuilder
exprSafeForCleanLiteral e = case e of
  Constant (S.Integer Signed sz n) ->
    let !enc = T.pack (show n) :< mempty
     in PrecBuilder
          { builder = cleanLiteralSignedInteger sz n enc
          , prec = 0
          }
  Constant (S.Integer Unsigned sz n) ->
    let !enc = T.pack (show n) :< mempty
     in PrecBuilder
          { builder = cleanLiteralUnsignedInteger sz n enc
          , prec = 0
          }
  _ -> expr e

-- Used when encoding an array index that is a literal. Here, since
-- we know how the number is being used, we can take liberties to
-- remove type annotations in certain cases. This is done in a portable
-- way. We assume that @int@ has a size of at least 16 bits.
--
-- We do not care about precedence since the output is used either as
-- an index or as the RHS of assignment.
--
-- Note: This is used for the arguments of comparison operators.
-- Suppressing the annotation can change the behavior of a comparison
-- that compares an a signed value with an unsigned value. For example:
--
-- > (uint16_t)40000 > (int16_t)(-1)
--
-- According to C99, section 6.3.1.8, both values should be converted to
-- uint16_t since neither type covers the range of the other. The behavior
-- of signed-to-unsigned conversion is implementation defined, but GCC
-- specifies a behavior for this. Erasing these annotations changes GCC's
-- behavior. The solution here is to not generate code that compares
-- signed and unsigned integers.
cleanLiteralSignedInteger ::
     Size
  -> Integer -- literal
  -> Builder -- encoding of the literal
  -> Builder
cleanLiteralSignedInteger size i b = case size of
  Fixed w -> case w of
    W8 -> if i >= (-127) && i <= 127
      then b
      else signedFixedWidthLiteralInteger w b
    _ -> if i >= (-32767) && i <= 32767
      then b
      else signedFixedWidthLiteralInteger w b
  Platform p -> case p of
    T.Size -> if i >= (-32767) && i <= 32767
      then b
      else "(ssize_t)" :< b
    Short -> if i >= (-32767) && i <= 32767
      then b
      else "(short)" :< b
    Int -> b
    Long -> if i >= (-32767) && i <= 32767
      then b
      else b :> "L"
    LongLong -> if i >= (-32767) && i <= 32767
      then b
      else b :> "LL"

cleanLiteralUnsignedInteger ::
     Size
  -> Integer -- literal
  -> Builder -- encoding of the literal
  -> Builder
cleanLiteralUnsignedInteger size i b = case size of
  Fixed w -> case w of
    W8 -> if i >= 0 && i <= 255
      then b
      else unsignedFixedWidthLiteralInteger w b
    _ -> if i >= 0 && i <= 65535
      then b
      else unsignedFixedWidthLiteralInteger w b
  Platform p -> case p of
    T.Size -> if i >= 0 && i <= 65535
      then b
      else "(size_t)" :< b
    Short -> if i >= 0 && i <= 65535
      then b
      else "(unsigned short)" :< b
    Int -> if i >= 0 && i <= 65535
      then b
      else b :> "U"
    Long -> if i >= 0 && i <= 65535
      then b
      else b :> "UL"
    LongLong -> if i >= 0 && i <= 65535
      then b
      else b :> "ULL"

signedLiteralInteger :: Size -> Builder -> PrecBuilder
signedLiteralInteger size i = case size of
  Fixed w -> PrecBuilder{builder=signedFixedWidthLiteralInteger w i, prec=1}
  Platform p -> case p of
    T.Size -> PrecBuilder{builder= "(ssize_t)" :< i, prec=2}
    Short -> PrecBuilder{builder= "(short)" :< i, prec=2}
    Int -> PrecBuilder{builder=i, prec=0}
    Long -> PrecBuilder{builder= i :> "L", prec=0}
    LongLong -> PrecBuilder{builder= i :> "LL", prec=0}

unsignedLiteralInteger :: Size -> Builder -> PrecBuilder
unsignedLiteralInteger size i = case size of
  Fixed w -> PrecBuilder{builder=unsignedFixedWidthLiteralInteger w i,prec=1}
  Platform p -> case p of
    T.Size -> PrecBuilder{builder= "(size_t)" :< i, prec=2}
    Short -> PrecBuilder{builder= "(unsigned short)" :< i, prec=2}
    Int -> PrecBuilder{builder= i :> "U", prec=0}
    Long -> PrecBuilder{builder= i :> "UL", prec=0}
    LongLong -> PrecBuilder{builder= i :> "ULL", prec=0}

signedFixedWidthLiteralInteger :: Width -> Builder -> Builder
signedFixedWidthLiteralInteger w i = case w of
  W8 ->  ("INT8_C("  :< i) :> ")"
  W16 -> ("INT16_C(" :< i) :> ")"
  W32 -> ("INT32_C(" :< i) :> ")"
  W64 -> ("INT64_C(" :< i) :> ")"

unsignedFixedWidthLiteralInteger :: Width -> Builder -> Builder
unsignedFixedWidthLiteralInteger w i =
  "U" :< signedFixedWidthLiteralInteger w i

-- | Encode a type
type_ :: Type -> Builder
type_ = \case
  T.Integer signedness sz -> case signedness of
    Signed -> signed sz
    Unsigned -> unsigned sz
  Void -> "void"
  Float -> "float"
  Double -> "double"
  Pointer t -> type_ t <> "*"
  Struct t -> "struct " :< t :< mempty
  Typedef t -> t :< mempty
  X86Vector v -> x86Vector v
  X86Mask w -> x86Mask w

-- | Encode a signed type.
signed :: Size -> Builder
signed = \case
  Fixed w -> case w of
    W8 -> "int8_t"
    W16 -> "int16_t"
    W32 -> "int32_t"
    W64 -> "int64_t"
  Platform p -> case p of
    T.Size -> "ssize_t"
    T.Char -> "signed char"
    Short -> "short"
    Int -> "int"
    Long -> "long"
    LongLong -> "long long"

-- | Encode an unsigned type.
unsigned :: Size -> Builder
unsigned = \case
  Fixed w -> case w of
    W8 -> "uint8_t"
    W16 -> "uint16_t"
    W32 -> "uint32_t"
    W64 -> "uint64_t"
  Platform p -> case p of
    T.Size -> "size_t"
    T.Char -> "unsigned char"
    Short -> "unsigned short"
    Int -> "unsigned int"
    Long -> "unsigned long"
    LongLong -> "unsigned long long"

x86Vector :: X86.Vector -> Builder
x86Vector X86.Vector{width,element} =
  "__m" <> x86Width width <> x86ElementSuffix element

x86Width :: X86.Width -> Builder
x86Width = \case
  X86.W128 -> "128"
  X86.W256 -> "256"
  X86.W512 -> "512"

x86ElementSuffix :: X86.Element -> Builder
x86ElementSuffix = \case
  X86.Double -> "d"
  X86.Float -> mempty
  X86.Integer -> "i"

x86Mask :: T.Width -> Builder
x86Mask = \case
  W8 -> "__mmask8"
  W16 -> "__mmask16"
  W32 -> "__mmask32"
  W64 -> "__mmask64"

encodeDesignatedInitializers :: SmallArray DesignatedInitializer -> Builder
encodeDesignatedInitializers !xs = case PM.sizeofSmallArray xs of
  0 -> mempty
  _ -> go 1 (encodeDesignatedInitializer (PM.indexSmallArray xs 0))
  where
  go !ix !acc = if ix < PM.sizeofSmallArray xs
    then go (ix + 1) (acc <> ", " <> encodeDesignatedInitializer (PM.indexSmallArray xs ix))
    else acc

encodeDesignatedInitializer :: DesignatedInitializer -> Builder
encodeDesignatedInitializer (DesignatedInitializer a b) =
  "." <> Builder.text a <> " = " <> expr_ b
