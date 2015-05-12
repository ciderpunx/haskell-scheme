module Operations where
import Control.Monad.Error
import GHC.Float
import Data.Complex
import Data.Char
import Data.Vector as V (replicate, fromList, toList, (!), (//)) 
import System.IO

import Parser
import Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (/)),
              ("mod", integerBinop mod),
              ("quotient", integerBinop quot),
              ("remainder", integerBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("as-list", asList), -- an extension for convenience
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv),
              ("equal?", equal),
              ("make-vector", mkVect),
              ("vector", vector),
              ("vector-ref", vectElemAt),
              ("string-append", stringAppend),
              ("make-string", mkString),
              ("type?", showValType),  -- this was added by me for convenience
              ("not", unaryBoolOp (boolNot)),
              ("string?", unaryOp isString),
              ("list?", unaryOp isList),
              ("char?", unaryOp isChar),
              ("pair?", unaryOp isDottedList),
              ("symbol?", unaryOp isAtom),
              ("vector?", unaryOp isVector),
              ("boolean?", unaryOp isBool)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("display", displayProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitiveBindings :: IO Env
primitiveBindings = 
    nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                              ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args               = liftThrows $ func args
apply (IOFunc func) args                      = func args
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
apply _ _                                    = throwError $ Default "Tried to apply a non-function"

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)                         = return val
eval env val@(Number (Int _))                   = return val
eval env val@(Number (Dbl _))                   = return val
eval env val@(Number (Rat _))                   = return val
eval env val@(Number (Cpx _))                   = return val
eval env val@(Bool _)                           = return val
eval env val@(Atom id)                          = getVar env id
eval env val@(Character _)                      = return val
eval env val@(Comment)                          = return val
eval env val@(Vector _)                         = return val
eval env (List [Atom "exit"])                   = error "Exit called" -- bodgy way to allow exiting from within programs, just die
eval env (List [Atom "error", form])            = do errstr <- eval env form
                                                     case errstr of
                                                      String s -> throwError $ User s
                                                      _        -> throwError $ User "Unspecified"
eval env (List (Atom "begin" :  exps))          = do xs <- mapM (eval env) exps 
                                                     return $ last xs
eval env (List [Atom "vector-set!", Atom var, Number (Int i), form]) =
     do vect <- getVar env var 
        case vect of 
          (Vector vs) -> do obj <- eval env form
                            if i > -1 && i < (fromIntegral . length $ V.toList vs)
                              then do let newV = Vector $ vs // [((fromIntegral i), obj)] 
                                      setVar env var newV
                              else throwError $ Default "Vector index out of bounds"
          v           -> throwError $ BadSpecialForm "vector-set! called on non-vector: " v
eval env (List [Atom "quote", val])             = return val
eval env (List [Atom "set!", Atom var, form])   = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body))  = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body))               = makeNormalFunc env params body
eval env (List (Atom "λ" : List params : body))                    = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "λ" : DottedList params varargs : body))      = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body))          = makeVarargs varargs env [] body
eval env (List (Atom "λ" : varargs@(Atom _) : body))               = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename])                     = load filename >>= liftM last . mapM (eval env)
eval env form@(List (Atom "cond" : branches))                      = 
    if null branches
      then throwError $ BadSpecialForm "Cond expression needs a true (default) branch: " form
      else 
        case head branches of
          List [Atom "else", conseq] -> eval env conseq
          List [pred, conseq]        -> do 
              result <- eval env pred
              case result of
                Bool False -> eval env (List (Atom "cond" : (tail branches)))
                Bool True  -> eval env conseq
                p          -> throwError $ BadPredicate "Non-boolean predicate in cond expression" (show p)
          _ -> throwError $ BadSpecialForm "Cond expression branch wasn't a pair of predicate, consequence (or an else clause) at: " form
eval env form@(List (Atom "case" : key : branches)) = 
    if null branches
      then throwError $ BadSpecialForm "Case expression needs a true (default) branch: " form
      else case head branches of
         List (Atom "else" : conseqs)   -> mapM (eval env) conseqs >>= return . last
         List ((List datums) : conseqs) -> do
            result <- eval env key
            equals <- mapM (\x -> (liftThrows . eqv) [result, x]) datums
            if Bool True `elem` equals
              then mapM (eval env) conseqs >>= return . last
              else eval env $ List (Atom "case" : key : tail branches)
         _                               -> throwError $ BadSpecialForm "Bad case expression form: " form
eval env (List [Atom "let", List varExpPairs, body]) = 
    if null varExpPairs
      then eval env body
      else
        case head varExpPairs of
          List [Atom var, exp] -> do 
                  evalExp <- eval env exp
                  ienv <- liftIO (bindVars env [(var, evalExp)]) -- TODO this is copying the whole env. Prolly inefficient.
                  eval ienv (List [Atom "let", List (tail varExpPairs), body])
          bad -> throwError $ BadSpecialForm "Bad let expression " bad
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

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . showVal

-- We are now making everything into complex doubles, and returning the least type that we can, if we get an x+0i
-- then we deal with it as a double, when the double is a round number we make it into an int. There may be rounding 
-- errors. Thanks to G for explaining complex numbers from memory.
-- TODO: Its seems a bit extra to always have to use complex numbers on the off chance we might need them. Performance 
-- implications?
-- Old type signature: numericBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
numericBinop :: (Complex Double -> Complex Double -> Complex Double) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = do 
        result <- mapM unpackCpx params >>= return . Number . Cpx . foldl1 (op) 
        asCpx <- unpackCpx result
        let asDbl    = realPart asCpx
            imgPart  = imagPart asCpx
        if imgPart == 0
          then if asDbl == (fromIntegral $ round asDbl)
                  then return $ Number . Int $ round asDbl
                  else return $ Number . Dbl $ asDbl
          else return result

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integerBinop op params        = mapM unpackInt params >>= return . Number . Int . foldl1 op

-- Always have one argument
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [b] = return $ op b
unaryOp op val = throwError $ NumArgs 1 val

unaryBoolOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal 
unaryBoolOp op [val@(Bool b)] = return $ op val
unaryBoolOp     op [val]      = throwError $ TypeMismatch "Boolean" val
unaryBoolOp     op val        = throwError $ NumArgs 1 val


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


-- TODO: Support all the different typeTestOperators.
isString :: LispVal -> LispVal
isString (String s) = Bool True
isString _          = Bool False

isBool :: LispVal -> LispVal
isBool (Bool s)   = Bool True
isBool _          = Bool False

isList :: LispVal -> LispVal
isList (List _)   = Bool True
isList _          = Bool False

isDottedList :: LispVal -> LispVal
isDottedList (DottedList _ _)   = Bool True
isDottedList _                  = Bool False

isAtom :: LispVal -> LispVal
isAtom (Atom _ )  = Bool True
isAtom _          = Bool False

isVector :: LispVal -> LispVal
isVector (Vector _ )  = Bool True
isVector _            = Bool False

isChar :: LispVal -> LispVal
isChar (Character _ )  = Bool True
isChar _               = Bool False

unpackNum :: LispVal -> ThrowsError Double
unpackNum (Number (Int n)) = return $ fromIntegral n
unpackNum (Number (Rat n)) = return $ fromRational n
unpackNum x@(Number (Cpx n)) 
                           = throwError $ TypeMismatch "Integer, Rational or Double" x 
unpackNum (Number (Dbl f)) = return f
unpackNum (List [n])       = unpackNum n
unpackNum (String n)       = let parsed = reads n in 
                                 if null parsed 
                                   then throwError $ TypeMismatch "Number" $ String n
                                   else return $ fst $ parsed !! 0
unpackNum notNum           = throwError $ TypeMismatch "Number" notNum

unpackCpx :: LispVal -> ThrowsError (Complex Double)
unpackCpx (Number (Int n))   = return $ (fromIntegral n) :+ 0
unpackCpx (Number (Rat n))   = return $ (fromRational n) :+ 0
unpackCpx (Number (Dbl f))   = return $ f :+ 0
unpackCpx x@(Number (Cpx n)) = return n 
unpackCpx (String n)         = 
    let parsed = reads n in 
      if null parsed 
        then throwError $ TypeMismatch "Number" $ String n
        else do 
          let n' = fst $ parsed !! 0
          return $ n' :+ 0
unpackCpx notNum           = throwError $ TypeMismatch "Complex Number" notNum

unpackInt :: LispVal -> ThrowsError Integer
unpackInt i = do 
  x <- unpackNum i 
  return $ round x

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat = unpackNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s)       = return s
unpackStr (Number (Int s)) = return $ show s
unpackStr (Number (Dbl s)) = return $ show s
unpackStr (Number (Rat s)) = return $ show s
unpackStr (Number (Cpx s)) = return $ show s
unpackStr (Bool s)         = return $ show s
unpackStr notString        = throwError $ TypeMismatch "String" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "Boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]             = writeProc [obj, Port stdout]
writeProc [obj, Port port]  = liftIO $ hPrint port obj >> (return $ Bool True)

displayProc :: [LispVal] -> IOThrowsError LispVal
displayProc [obj]             = displayProc [obj, Port stdout]
displayProc [obj, Port port]  = liftIO $ hPutStr port (showFormattedVal obj) >> (return $ Character '\0')
displayProc badArgs           = throwError $ TypeMismatch "Val,<Port>" $ List badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


boolNot :: LispVal -> LispVal
boolNot (Bool True)   = Bool False
boolNot (Bool False)  = Bool True

mkVect :: [LispVal] -> ThrowsError LispVal
mkVect ((Number (Int size)) : obj : []) 
                                    = return $ Vector $ V.replicate (fromInteger size) obj
mkVect [(Number (Int size)) ]       = return $ Vector $ V.replicate (fromInteger size) (Bool False)
mkVect _                            = throwError $ Default "Integer size [optional default object (#f if not given)]" 
    
vector :: [LispVal] -> ThrowsError LispVal
vector [(List xs)] = return $ Vector (V.fromList xs) -- quite convenient for list->vector
vector x = return $ Vector (V.fromList x) 

vectElemAt :: [LispVal] -> ThrowsError LispVal 
vectElemAt ((Vector xs) : (Number (Int n)) : [])
                         = if n > -1 && n < (fromIntegral . length $ V.toList xs)
                            then return $ xs ! (fromIntegral n) 
                            else throwError $ Default "Vector index out of bounds"
vectElemAt (badArg1 : badArg2 : [])      
                         = throwError $ TypeMismatch "Vector, Int" (List (badArg1 : [badArg2]))
vectElemAt [badArg]      = throwError $ TypeMismatch "Vector, Int" badArg
vectElemAt badArgList    = throwError $ NumArgs 2 badArgList

-- Turn LispVal into a list. 
asList :: [LispVal] -> ThrowsError LispVal
asList [i@(Number _)] = asList $ [(String $ show i)]
asList [(String s)]   = asListStringLike s
asList [(Atom a)]     = asListStringLike a
asList [l@(List _)]   = return l
asList [(Vector vs)]  = return $ List (toList vs)
asList [x]            = return $ List [x]
asList badArgList     = throwError $ NumArgs 1 badArgList

-- Helper for stringlike LispVals (atoms, strings)
asListStringLike :: String -> ThrowsError LispVal
asListStringLike x = return $ List $ 
                        map (\x -> if isDigit x 
                                    then (Number (Int (read [x]))) 
                                    else Character x) x

stringAppend :: [LispVal] -> ThrowsError LispVal 
stringAppend [] = return $ String []
stringAppend ((String s):ss)  = return $ String $ s ++ (foldl (extract) "" ss)
  where extract s (String s')     = s ++ s'
        extract s badArg          = s
stringAppend [badArg]         = throwError $ TypeMismatch "String" badArg

mkString :: [LispVal] -> ThrowsError LispVal
mkString ((Number (Int size)) : Character c : []) 
                                    = return $ String $ take (fromInteger size) (repeat c)
mkString ((Number (Int size)) : badArg : [])  
                                    = throwError $ TypeMismatch "List of chars" badArg
mkString [(Number (Int size)) ]     = return $ String $ take (fromInteger size) (repeat ' ')
mkString _                          = throwError $ Default "Integer size [optional default character (' ' if not given)]" 
    
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "Pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList ( _:xs ) x] = return $ DottedList xs  x
cdr [badArg]                = throwError $ TypeMismatch "Pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x1, List xs]            = return $ List (x1:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]               
                                           = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Character arg1), (Character arg2)]   = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [vect@(Vector _), vect'@(Vector _)]    = return $ Bool $ vect == vect'
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err         -> False
                               Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all equalPair $ zip arg1 arg2)
    where equalPair (x1, x2) = case equal [x1, x2] of
                               Left err         -> False
                               Right (Bool val) -> val
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals       <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList   = throwError $ NumArgs 2 badArgList

showValType :: [LispVal] -> ThrowsError LispVal
showValType [(String s)]             = return $ String $ "String \"" ++ s ++ "\""
showValType [(Atom a)]               = return $ String $ "Atom " ++ a
showValType [(Character c)]          = return $ String $ "Character " ++ show c
showValType [(Number (Int n))]       = return $ String $ "Integer " ++ show n
showValType [(Number (Dbl n))]       = return $ String $ "Float " ++ show n
showValType [(Number (Rat n))]       = return $ String $ "Rational " ++ show n
showValType [(Number (Cpx n))]       = return $ String $ "Complex " ++ show n
showValType [(Bool True)]            = return $ String $ "Boolean #t"
showValType [(Bool False)]           = return $ String $ "Boolean #f"
showValType [(Comment)]              = return $ String $ "Comment"
showValType [vect@(Vector _)]        = return $ String $ "Vector " ++ show vect
showValType [func@(Func _ _ _ _)]    = return $ String $ "Lambda " ++ show func
showValType [iofunc@(IOFunc _)]      = return $ String $ "IO Function " ++ show iofunc
showValType [pfunc@(PrimitiveFunc _)]= return $ String $ "Primitive Function " ++ show pfunc
showValType [(List contents)]        = return $ String $ "List (" ++ unwordsList contents ++ ")"
showValType [(DottedList head tail)] = return $ String $ "DottedList (" ++ unwordsList head ++ " . " ++ show tail ++ ")"
showValType [t]                      = return $ String $ "Unknown type that Charlie probably forgot to implement a format for in type?"
