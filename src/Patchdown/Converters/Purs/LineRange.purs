module Patchdown.Converters.Purs.LineRange where

import Prelude

import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)

type LineRange = { lineStart :: Int, lineEnd :: Int }

class GetLineRange a where
  lineRange :: a -> LineRange

instance GetLineRange (CST.Declaration e) where
  lineRange = case _ of
    CST.DeclValue { binders, guarded, name } -> mergeRanges [ lineRange binders, lineRange guarded, lineRange name ]
    _ -> unsafeCoerce ""

instance (GetLineRange a) => GetLineRange (Array a) where
  lineRange = mergeRanges <<< map lineRange

instance GetLineRange (CST.Binder e) where
  lineRange = unsafeCoerce ""

instance GetLineRange (CST.Expr e) where
  lineRange = unsafeCoerce ""

instance GetLineRange (CST.Guarded e) where
  lineRange = unsafeCoerce ""

instance GetLineRange (CST.Name e) where
  lineRange (CST.Name { token }) = lineRangeSourceToken token

instance GetLineRange (CST.Comment e) where
  lineRange = unsafeCoerce ""

lineRangeSourceRange :: CST.SourceRange -> LineRange
lineRangeSourceRange { start, end } = mergeRanges [ lineRangeSourcePos start, lineRangeSourcePos end ]

lineRangeSourcePos :: CST.SourcePos -> LineRange
lineRangeSourcePos { line } = { lineStart: line, lineEnd: line }

lineRangeSourceToken :: CST.SourceToken -> LineRange
lineRangeSourceToken { leadingComments, range, trailingComments } = mergeRanges
  [ lineRange leadingComments, lineRangeSourceRange range, lineRange trailingComments ]

mergeRanges :: Array LineRange -> LineRange
mergeRanges = unsafeCoerce ""