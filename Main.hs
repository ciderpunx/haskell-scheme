module Main where
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Maybe
import System.Environment
import System.IO

import Operations
import Parser
import Types

-- More convenient REPL if you run it with rlwrap (gives you readline support, history and such)
-- TODO: implement rationals and complex numbers
--       *. Add support for vectors.
--       *. Instead of using the try combinator, left-factor the grammar so that the common subsequence is its own parser. You should 
--          end up with a parser that matches a string of expressions, and one that matches either nothing or a dot and a single 
--          expressions. Combining the return values of these into either a List or a DottedList is left as a (somewhat tricky) 
--          exercise for the reader: you may want to break it out into another helper function.
--

main :: IO ()
main = do 
    args <- getArgs
    if null args 
      then runRepl 
      else runOne $ args

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr
