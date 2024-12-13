{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module LispVal where

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Exception (Exception)
import Control.Monad.Reader

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving (Monad,
              Functor,
              Applicative,
              MonadReader EnvCtx,
              MonadIO)

liftIO :: MonadIO m => IO a -> m a

data LispVal
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool 
    deriving (Eq)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

instance Eq IFunc where
    (==) _ _ = False


instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = 
    case val of
        (Atom atom)     -> atom
        (String str)    -> T.concat["\"", str,"\""]
        (Number num)    -> T.pack $ show num
        (Bool True)     -> "#t"
        (Bool False)    -> "#f"
        Nil             -> "Nil"
        (List contents) -> T.concat["(", T.unwords $ showVal <$> contents, ")"]
        (Fun _)         -> "(internal function)"
        (Lambda _ _)    -> "(lambda function)"


data LispException
    = NumArgs Integer [LispVal]
    | LengthOfList T.Text Int
    | ExpectedList T.Text
    | TypeMismatch T.Text LispVal
    | BadSpecialForm T.Text
    | NotFunction LispVal
    | UnboundVar T.Text
    | Default LispVal
    | PError String
    | IOError T.Text

instance Exception LispException

instance Show LispException where
    show = T.unpack . showError

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list

showError :: LispException -> T.Text
showError err =
    case err of
        (IOError ioerr) -> T.concat["Error reading file: ", ioerr]
        (NumArgs int args) -> T.concat["Error Number Arguments, expected ", T.pack $ show int, " received args: ", unwordsList args]
        (LengthOfList lst length) -> T.concat["Error Length of List in ", T.pack $ show lst, " length: ", T.pack $ show length]
        (ExpectedList var) -> T.concat["Error Expected List in Function, received: ", var]
        (TypeMismatch expected found) -> T.concat["Error Type Mismatch, expected: ", expected, " found: ", showVal found]
        (BadSpecialForm form) -> T.concat["Error Bad Special Form, received: ", form]
        (NotFunction func) -> T.concat["Error Not Function: ", showVal func]
        (UnboundVar var) -> T.concat["Error Unbound Variable: ", var]
        (PError msg) -> T.concat["Parser Error, cannot evaluate expression: ", T.pack msg]
        (Default val) -> T.concat["Error Default, received: ", showVal val]

