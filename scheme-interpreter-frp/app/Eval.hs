{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import LispVal (LispException(Default, PError, UnboundVar, TypeMismatch, BadSpecialForm, NotFunction), IFunc(IFunc), LispVal(..), Eval(unEval), EnvCtx(..), showVal)
import Parser (readExpr, readExprFile)
import Prim (primEnv, unop)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import System.Directory (doesFileExist)

import Text.Parsec (ParseError)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Exception (try, fromException, SomeException, throw)

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv
        <> [("read", Fun $ IFunc $ unop $ readFn)]

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn val = throw $ TypeMismatch "read expects string, instead got: " val

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show) eval $ readExpr input

evalFile :: T.Text -> IO ()
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr)
                    >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show) 
                              evalBody
                              $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show)
                            (T.pack . show)
                            $ readExpr input

parseFn :: LispVal -> Eval LispVal
parseFn (String txt) = either (throw . PError . show) return $ readExpr txt
parseFn val = throw $ TypeMismatch "parse expected string, got: " val

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

getVar :: LispVal -> Eval LispVal
getVar (Atom var) = do
    env <- ask
    case Map.lookup var env of 
        Just val -> return val
        Nothing -> throw $ UnboundVar var

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b) = return $ Bool b
eval (List []) = return Nil
eval Nil = return Nil
eval (List [Atom "write", rest]) = return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) = return . String . T.pack . show $ List rest
eval n@(Atom _) = getVar n
eval (List [Atom "if", pred, truExpr, flsExpr]) = do
    predVal <- eval pred
    case predVal of
        Bool True -> eval truExpr
        Bool False -> eval flsExpr
        _ -> throw $ BadSpecialForm "if"
eval (List [Atom "let", List pairs, expr]) = do
    env <- ask
    atoms <- mapM ensureAtom $ getEven pairs
    vals <- mapM eval $ getOdd pairs
    let newEnv = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env in local (const newEnv) $ evalBody expr
eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest
eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr
    env <- ask
    let envFn = const $ Map.insert (extractVar varAtom) evalVal env in local envFn $ return varExpr
eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- ask
    return $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- ask
    return $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_)) = throw $ BadSpecialForm "lambda"
eval (List ((:) x xs)) = do
    funVar <- eval x
    xVal <- mapM eval xs
    case funVar of
        (Fun (IFunc internalFn)) -> internalFn xVal
        (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) $ internalfn xVal
        _ -> throw $ NotFunction funVar



getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x:xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom a) = a

getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
  exists <- doesFileExist fname
  if exists then TIO.readFile  fname else return "File does not exist."

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _  = throw $ TypeMismatch  "failure to get variable: " n

evalText :: T.Text -> IO () 
evalText textExpr = do
    res <- runASTinEnv basicEnv $ textToEvalForm textExpr
    print res

textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm textExpr = either (throw . PError . show) evalBody $ readExpr textExpr 

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env <- ask
    local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    env <- ask
    let envFn = const $ Map.insert var evalVal env in local envFn $ evalBody $ List rest
evalBody x = eval x

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    env <- ask
    argEval <- mapM eval args
    let newEnv = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) params argEval) <> env in local (const newEnv) $ eval expr

safeExec :: IO a -> IO (Either String a)
safeExec m = do
    result <- Control.Exception.try m 
    case result of
        Left (eTop :: SomeException) -> 
            case fromException eTop of
                Just (enclosed :: LispException) -> return $ Left (show enclosed)
                Nothing -> return $ Left (show eTop)
        Right x -> return $ Right x
