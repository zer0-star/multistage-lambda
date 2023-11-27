module Main where

import Control.Monad

import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Exit
import System.IO

import Text.Megaparsec qualified as M

import Data.List.NonEmpty qualified as NE
import MultLam.Data.Expr
import MultLam.Parser.Expr
import MultLam.Renamer
import MultLam.TypeCheck
import System.Console.Readline (addHistory, readline)

main :: IO ()
main = do
  putStrLn "oisu-"
  forever $ do
    input <- readline "> "
    case input of
      Nothing -> putStrLn "bye" >> exitSuccess
      Just ":q" -> putStrLn "bye" >> exitSuccess
      Just s' -> do
        addHistory s'
        let s = T.pack s'
        case M.parse (expr <* M.eof) "" s of
          Left e -> do
            putStrLn $ "parse error: " <> M.errorBundlePretty e
          Right e -> do
            print e
            case rename e of
              Left e' -> putStrLn $ "syntax error: " <> M.errorBundlePretty (M.ParseErrorBundle (NE.singleton e') $ M.PosState s 0 (M.initialPos "") M.defaultTabWidth "")
              Right e' -> do
                -- print e'
                case infer e' of
                  Left e'' -> putStrLn $ "type error: " <> e''
                  Right (e'', t) -> do
                    putStrLn $ show e'' <> " : " <> show t
