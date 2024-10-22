module Main where
import Sat (parser, tseitin)
import Options.Applicative
import Text.Megaparsec (runParser)
import Data.Text (pack)

newtype Args = Args
  { program :: String }

args :: Parser Args
args = Args
      <$> argument str (metavar "Expression")

greet :: Args -> IO ()
greet (Args p)  
  | (Right x) <- ast = print x
  | (Left err) <- ast = print err
  where
    ast = tseitin <$> runParser parser "" (pack p)

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )