{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Repl where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO
import System.Directory
import System.FilePath
import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT)

import qualified Data.Map as Map
import qualified Data.Set as Set

import LispVal
import Eval
import Prim

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = do
    minput <- getInputLine "Repl> "
    case minput of
        Nothing -> outputStrLn "Hasta la vista, baby!"
        Just expr -> liftIO (process expr) >> repl
        -- Just expr -> (liftIO $ processToAST expr) >> repl

process :: String -> IO ()
process str = do
    res <- safeExec $ evalText $ T.pack str
    either putStrLn return res

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str

