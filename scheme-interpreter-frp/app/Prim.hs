{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Prim where

import LispVal
import Parser (parseExpr)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO (Handle, hIsWritable, withFile, IOMode(ReadMode, WriteMode))
import System.Directory (doesFileExist)
import Control.Exception (throw)
import Control.Monad.Except (foldM, MonadIO(liftIO))

type Prim = [(T.Text, LispVal)]
type Unary = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x, y] = op x y
binop _ args = throw $ NumArgs 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a, b] -> op a b
                            (a:as) -> foldM op farg args
                            [] -> throw $ NumArgs 2 args



mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [ ("+", mkF $ binopFold (numOp (+)) (Number 0)),
            ("*", mkF $ binopFold (numOp (*)) (Number 1)),
            ("++", mkF $ binopFold (strOp (<>)) (String "")),
            ("-", mkF $ binop $ numOp (-)),
            ("<", mkF $ binop $ numCmp (<)),
            (">", mkF $ binop $ numCmp (>)),
            ("<=", mkF $ binop $ numCmp (<=)),
            (">=", mkF $ binop $ numCmp (>=)),
            ("==", mkF $ binop $ numCmp (==)),
            ("eq?", mkF $ binop eqCmd),
            ("even?", mkF $ unop $ numBool even),
            ("odd?", mkF $ unop $ numBool odd),
            ("pos?", mkF $ unop $ numBool (< 0)),
            ("neg?", mkF $ unop $ numBool (> 0)),
            ("eq?", mkF $ binop eqCmd),
            ("bl-eq?", mkF $ binop $ eqOp (==)),
            ("and", mkF $ binopFold (eqOp (&&)) (Bool True)),
            ("or", mkF $ binopFold (eqOp (||)) (Bool False)),
            ("cons", mkF Prim.cons),
            ("cdr", mkF Prim.cdr),
            ("car", mkF Prim.car),
            ("file?", mkF $ unop fileExists),
            ("slurp", mkF $ unop slurp),
            ("put", mkF $ binop Prim.put),
            ("show", Fun $ IFunc $ unop (return . String . showVal)),
            ("nil?", mkF $ unop checkListEmpty)
            ]

checkListEmpty :: LispVal -> Eval LispVal
checkListEmpty (List []) = return $ Bool True
checkListEmpty _ = return $ Bool False

fileExists :: LispVal -> Eval LispVal
fileExists (Atom atom) = fileExists $ String atom
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val = throw $ TypeMismatch "expected str, got: " val

slurp :: LispVal -> Eval LispVal
slurp (String txt) = liftIO $ wFileSlurp txt
slurp val = throw $ TypeMismatch "expects str, got: " val

wFileSlurp :: T.Text -> IO LispVal
wFileSlurp fileName = withFile (T.unpack fileName) ReadMode go
    where go = readTextFile fileName

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile fileName handle = do
    exists <- doesFileExist $ T.unpack fileName
    if exists
        then (TIO.hGetContents handle) >>= return . String
        else throw $ IOError $ T.concat [" file does not exist: ", fileName]

put :: LispVal -> LispVal -> Eval LispVal
put (String file) (String msg) = liftIO $ wFilePut file msg
put (String _) val = throw $ TypeMismatch "put expected string for second argument, instead got: " val
put val _ = throw $ TypeMismatch "put expected string, instead got: " val

wFilePut :: T.Text -> T.Text -> IO LispVal
wFilePut fileName msg = withFile (T.unpack fileName) WriteMode go 
    where go = putTextFile fileName msg

putTextFile :: T.Text -> T.Text -> Handle -> IO LispVal
putTextFile fileName msg handle = do
    canWrite <- hIsWritable handle
    if canWrite
        then (TIO.hPutStrLn handle msg) >> (return $ String msg)
        else throw $ IOError $ T.concat ["file doesn't exist: ", fileName]

cons :: [LispVal] -> Eval LispVal
cons [x, y@(List yList)] = return $ List $ x:yList
cons [c] = return $ List [c]
cons [] = return $ List []
cons _ = throw $ ExpectedList "cons, in second argument."

car :: [LispVal] -> Eval LispVal
car [List []] = return Nil
car [List (x:_)] = return x
car [] = return Nil
car x = throw $ ExpectedList "car."

cdr :: [LispVal] -> Eval LispVal
cdr [List (x:xs)] = return $ List xs
cdr [List []] = return Nil
cdr [] = return Nil
cdr x = throw $ ExpectedList "cdr."

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool op x = throw $ TypeMismatch "numeric operation, got: " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x y
numOp op x (Number y) = throw $ TypeMismatch "numeric operation, got: " x
numOp op (Number x) y = throw $ TypeMismatch "numeric operation, got: " y
numOp op x y = throw $ TypeMismatch "numeric operation, got: " x


strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp op x (String y) = throw $ TypeMismatch "string operation, got: " x
strOp op (String x) y = throw $ TypeMismatch "string operation, got: " y
strOp op x y = throw $ TypeMismatch "string operation, got: " x

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp op x (Bool y) = throw $ TypeMismatch "bool operation, got: " x
eqOp op (Bool x) y = throw $ TypeMismatch "bool operation, got: " y
eqOp op x y = throw $ TypeMismatch "bool operation, got: " x

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return . Bool $ op x y
numCmp op x (Number y) = throw $ TypeMismatch "numeric operation, got: " x
numCmp op (Number x) y = throw $ TypeMismatch "numeric operation, got: " y
numCmp op x y = throw $ TypeMismatch "numeric operation, got: " x

eqCmd :: LispVal -> LispVal -> Eval LispVal
eqCmd (Atom x) (Atom y) = return . Bool $ x == y
eqCmd (Number x) (Number y) = return . Bool $ x == y
eqCmd (String x) (String y) = return . Bool $ x == y
eqCmd (Bool x) (Bool y) = return . Bool $ x == y
eqCmd Nil Nil = return $ Bool True
eqCmd _ _ = return $ Bool False

