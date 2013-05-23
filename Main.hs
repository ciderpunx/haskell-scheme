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
    case length args of
         0         -> runRepl
         1         -> runOne $ args !! 0
         otherwise -> putStrLn "Program requires either a single argument (to evaluate) or none"

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

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)                         = return val
eval env val@(Number _)                         = return val
eval env val@(Float _)                          = return val
eval env val@(Bool _)                           = return val
eval env val@(Atom id)                          = getVar env id
eval env val@(Character _)                      = return val
eval env (List [Atom "quote", val])             = return val
eval env (List [Atom "set!", Atom var, form])   = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body))  = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body))               = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body))          = makeVarargs varargs env [] body
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      Bool True  -> eval env conseq
      p          -> throwError $ BadPredicate "Non-boolean predicate in if statement" (show p)
eval env (List (function : args)) = do 
    func    <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env val = throwError $ BadSpecialForm "Bad special form" val 

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args               = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs       = drop (length params) args
          num                 = toInteger . length
          evalBody env        = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName  -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing       -> return env 

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

