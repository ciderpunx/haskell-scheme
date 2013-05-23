{-# LANGUAGE ExistentialQuantification #-} 
module Types where
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef
import System.IO

data LispVal = Atom String               
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float LispFloat
             | Character Char
             | String String
             | Bool Bool
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), 
                      body :: [LispVal], closure :: Env}

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s)             = "\"" ++ s ++ "\""
showVal (Atom a)               = a
showVal (Character c)          = show c
showVal (Number n)             = show n
showVal (Float (Short n))      = show n
showVal (Float (Long n))       = show n
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitive>"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Func {params  = args, 
               vararg  = varargs, 
               body    = body, 
               closure = env}) = "(lambda (" ++ unwords (map show args) ++ 
                                    (case varargs of 
                                        Nothing   -> ""
                                        Just arg  -> " . " ++ arg) ++ ") ...)" 

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispFloat = Short Float | Long Double
    deriving (Show,Eq)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | BadPredicate String String
               | UnboundVar String String
               | Default String


instance Show LispError where show = showError

instance Error LispError where
     noMsg  = Default "Something unexpected went wrong"
     strMsg = Default

type ThrowsError   = Either LispError

type IOThrowsError = ErrorT LispError IO

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (BadPredicate message pred)   = message ++ ": " ++ show pred
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default msg)                 = "Error: " ++ show msg


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

