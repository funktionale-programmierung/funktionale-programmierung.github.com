{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (takeWhile)

import Control.Applicative (many)
import Data.Attoparsec.Text

import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.List as List

import Test.Framework

newtype Message
    = Message
    { msg_segments :: Vector Segment
    } deriving (Show, Eq)

data Segment
    = Segment
    { seg_name :: T.Text
    , seg_fields :: !(Vector Field)
    } deriving (Show, Eq)

data Field
    = EmptyField
    | TextField !T.Text
    | StructuredField !(Vector FieldRep)
    deriving (Show, Eq)

data FieldRep
    = EmptyFieldRep
    | TextFieldRep !T.Text
    | StructuredFieldRep !(Vector Component)
    deriving (Show, Eq)

data Component
    = EmptyComponent
    | TextComponent !T.Text
    | StructuredComponent !(Vector SubComponent)
    deriving (Show, Eq)

data SubComponent
    = EmptySubComponent
    | TextSubComponent !T.Text
    | ValueSubComponent !(Vector Value)
    deriving (Show, Eq)

data Value
    = TextValue !T.Text
    | EscapeValue!T.Text
      deriving (Show, Eq)

mkMessage :: Segment -> [Segment] -> Message
mkMessage x xs = Message (Vector.fromList (x:xs))

mkSegment :: T.Text -> [Field] -> Segment
mkSegment name fields = Segment name (mkVector EmptyField fields)

mkField :: [FieldRep] -> Field
mkField [] = EmptyField
mkField [EmptyFieldRep] = EmptyField
mkField [TextFieldRep v] = TextField v
mkField reps = StructuredField (mkVector EmptyFieldRep reps)

mkFieldRep :: [Component] -> FieldRep
mkFieldRep [] = EmptyFieldRep
mkFieldRep [EmptyComponent] = EmptyFieldRep
mkFieldRep [TextComponent v] = TextFieldRep v
mkFieldRep comps = StructuredFieldRep (mkVector EmptyComponent comps)

mkComponent :: [SubComponent] -> Component
mkComponent [] = EmptyComponent
mkComponent [EmptySubComponent] = EmptyComponent
mkComponent [TextSubComponent v] = TextComponent v
mkComponent subComps = StructuredComponent (mkVector EmptySubComponent subComps)

mkSubComponent :: [Value] -> SubComponent
mkSubComponent [] = EmptySubComponent
mkSubComponent [TextValue t] =
    if t == "\"\"" then TextSubComponent "" else TextSubComponent t
mkSubComponent vs = ValueSubComponent (Vector.fromList vs)

mkVector :: Eq a => a -> [a] -> Vector a
mkVector empty l = Vector.fromList (List.dropWhileEnd (== empty) l)

parseMessage :: Parser Message
parseMessage =
    do string "MSH"
       fieldSep <- anyChar
       compSep <- anyChar
       repSep <- anyChar
       escChar <- anyChar
       subSep <- anyChar
       let fields =
               many (do char fieldSep
                        field)
           field =
               do reps <- fieldRep `sepBy` char repSep
                  return (mkField reps)
           fieldRep =
               do comps <- component `sepBy` char compSep
                  return (mkFieldRep comps)
           component =
               do subComps <- subComponent `sepBy` char subSep
                  return (mkComponent subComps)
           subComponent =
               do xs <- many $ choice [regularValue, escapeSequence]
                  return (mkSubComponent xs)
           regularValue =
               do x <- takeWhile1 notSpecial
                  return (TextValue x)
           escapeSequence =
               do char escChar
                  x <- takeWhile1 notSpecial
                  char escChar
                  return (EscapeValue x)
           notSpecial x = (x /= fieldSep && x /= compSep && x /= subSep &&
                           x /= lineSep && x /= repSep && x /= escChar)
       mshFields <- fields
       let segment =
               do name <- takeWhile1 (/= fieldSep)
                  fs <- fields
                  return (mkSegment name fs)
       otherSegments <- many (do eol
                                 segment)
       return (mkMessage (mkSegment "MSH" mshFields) otherSegments)
    where
      lineSep = '\r'
      eol = char lineSep

parseMessageFromText :: T.Text -> Either String Message
parseMessageFromText = parseOnly parseMessage

test_parseMessage =
    assertEqual (Right expectedMsg) (parseMessageFromText msg)
    where
      expectedMsg =
          Message (Vector.fromList
                        [Segment "MSH" (Vector.fromList [TextField "FOO"]),
                         Segment "PID" (Vector.fromList [EmptyField,
                                                    EmptyField,
                                                    TextField "454721",
                                                    EmptyField,
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [TextComponent "DOE",
                                                                                        TextComponent "JOHN"])])]),
                         Segment "PV1" (Vector.fromList [EmptyField,
                                                    StructuredField (Vector.fromList
                                                                          [TextFieldRep "0",
                                                                           StructuredFieldRep
                                                                           (Vector.fromList [TextComponent "1",
                                                                                        TextComponent "2"])]),
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [StructuredComponent
                                                                                        (Vector.fromList [EmptySubComponent,
                                                                                                     TextSubComponent "bar"])])]),
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [StructuredComponent
                                                                                        (Vector.fromList [ValueSubComponent
                                                                                                     (Vector.fromList [TextValue "string",
                                                                                                                  EscapeValue "F",
                                                                                                                  TextValue "escape"])])])]),
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [EmptyComponent,
                                                                                        TextComponent ""])])])])

      msg = T.intercalate "\r"
            ["MSH|^~\\&|FOO",
             "PID|||454721||DOE^JOHN^",
             "PV1||0~1^2|&bar&|string\\F\\escape|^\"\""]
