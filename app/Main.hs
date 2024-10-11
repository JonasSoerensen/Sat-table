{-# LANGUAGE OverloadedStrings #-}

module Main where
import Sat (parser)
import Options.Applicative
import Text.Megaparsec (runParser)
import Data.Text (pack)
import Data.Fix (foldFix)


newtype Args = Args
  { program :: String }

args :: Parser Args
args = Args
      <$> argument str (metavar "Expression")

greet :: Args -> IO ()
greet (Args p) = print $ foldFix show <$> runParser parser "" (pack p)

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
