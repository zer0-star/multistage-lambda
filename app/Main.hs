module Main where

import           Control.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Exit
import           System.IO

import qualified Text.Megaparsec as M

import           MultLam.Data.Expr
import           MultLam.Parser.Expr
import           MultLam.Renamer

main :: IO ()
main = do
  putStrLn "oisu-"
  forever $ do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    when eof $ putStrLn "bye" >> exitSuccess
    s <- T.getLine
    case M.parse expr "" s of
      Left e  -> putStrLn $ M.errorBundlePretty e
      Right e -> case rename e of
        Left e'  -> T.putStrLn $ "syntax error: " <> e'
        Right e' -> do
          print e
          print e'

