module Data.Prune.Section.Parser where

import Prelude

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Monad (void)
import Data.Text (pack, unpack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, noneOf, parse, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, eol, hspace, hspace1, string)

import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

type Parser = Parsec Void String

targetName :: Parser T.CompilableName
targetName = T.CompilableName . pack <$> some (alphaNumChar <|> char '-')

restOfLine :: Parser String
restOfLine = many (noneOf ("\r\n" :: String)) <* eol

indentedLine :: Int -> Parser String
indentedLine numSpaces = do
  spaces <- many (char ' ')
  let n = length spaces
  case n <= numSpaces of
    True -> fail $ "indentation: " <> show n <> " (expected " <> show numSpaces <> ")"
    False -> (spaces <>) <$> restOfLine

indentedLines :: Int -> Parser [String]
indentedLines numSpaces = (:) <$> restOfLine <*> many (try (indentedLine numSpaces))

nestedSection :: Parser T.NestedSection
nestedSection = do
  numSpaces <- length <$> some (char ' ')
  let buildDepends = do
        void $ string "build-depends:"
        T.BuildDependsNestedSection numSpaces <$> indentedLines numSpaces
      import_ = do
        void $ string "import:"
        T.ImportNestedSection numSpaces <$> indentedLines numSpaces
      other = T.OtherNestedSection numSpaces <$> indentedLines numSpaces
  buildDepends <|> import_ <|> other

nestedSections :: Parser [T.NestedSection]
nestedSections = some nestedSection

section :: Parser T.Section
section =
  let lib = do
        void $ string "library"
        hspace
        void eol
        T.TargetSection T.CompilableTypeLibrary Nothing <$> nestedSections
      target typ typName = do
        void $ string typName
        hspace1
        name <- targetName
        hspace
        void eol
        T.TargetSection typ (Just name) <$> nestedSections
      common = do
        void $ string "common"
        hspace1
        name <- T.CommonName . pack <$> restOfLine
        T.CommonSection name <$> nestedSections
      sublib = target T.CompilableTypeLibrary "library"
      exe = target T.CompilableTypeExecutable "executable"
      test = target T.CompilableTypeTest "test-suite"
      bench = target T.CompilableTypeBenchmark "benchmark"
      other = T.OtherSection <$> indentedLines 0
  in lib <|> sublib <|> exe <|> test <|> bench <|> common <|> other

sections :: Parser [T.Section]
sections = some section

parseCabalSections :: String -> Either String [T.Section]
parseCabalSections = left show . parse sections ""

renderCabalSections :: [T.Section] -> String
renderCabalSections = foldr go mempty
  where
    go2 next accum = case next of
      T.BuildDependsNestedSection numSpaces dependencies -> replicate numSpaces ' ' <> "build-depends:" <> unlines dependencies <> accum
      T.ImportNestedSection numSpaces imports -> replicate numSpaces ' ' <> "import:" <> unlines imports <> accum
      T.OtherNestedSection numSpaces rest -> replicate numSpaces ' ' <> unlines rest <> accum
    go next accum =
      let str = case next of
            T.TargetSection compilableType compilableNameMay nested ->
              let sectionType = case compilableType of
                    T.CompilableTypeLibrary -> "library"
                    T.CompilableTypeExecutable -> "executable"
                    T.CompilableTypeTest -> "test-suite"
                    T.CompilableTypeBenchmark -> "benchmark"
                  sectionName = case compilableNameMay of
                    Nothing -> ""
                    Just (T.CompilableName name) -> " " <> unpack name
              in sectionType <> sectionName <> "\n" <> foldr go2 mempty nested
            T.CommonSection (T.CommonName name) nested ->
              "common " <> unpack name <> "\n" <> foldr go2 mempty nested
            T.OtherSection xs -> unlines xs
      in str <> accum

readCabalSections :: FilePath -> IO (Either String [T.Section])
readCabalSections cabalFile = parseCabalSections <$> readFile cabalFile

writeCabalSections :: FilePath -> [T.Section] -> IO ()
writeCabalSections cabalFile = writeFile cabalFile . renderCabalSections
