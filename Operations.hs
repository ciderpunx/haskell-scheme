module Operations where
import Control.Monad.Error
import GHC.Float

import Parser
import Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
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
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv),
              ("equal?", equal),
              ("type?", showValType),  -- this was added by me for convenience
              ("not", unaryBoolOp (boolNot)),
              ("string?", unaryOp isString),
              ("list?", unaryOp isList),
              ("char?", unaryOp isChar),
              ("pair?", unaryOp isDottedList),
              ("symbol?", unaryOp isAtom),
              ("boolean?", unaryOp isBool)]

-- TODO: need implementation of this for Float/Doubles too
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

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

isChar :: LispVal -> LispVal
isChar (Character _ )  = Bool True
isChar _               = Bool False


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n)        = return n
unpackNum (Float (Long f))  = return $ round f
unpackNum (Float (Short f)) = return $ round f
unpackNum (List [n])        = unpackNum n
unpackNum (String n)        = let parsed = reads n in 
                                 if null parsed 
                                   then throwError $ TypeMismatch "Number" $ String n
                                   else return $ fst $ parsed !! 0
unpackNum notNum            = throwError $ TypeMismatch "Number" notNum

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float (Long f))  = return $ f
unpackFloat (Float (Short f)) = return $ float2Double f
unpackFloat (Number n)        = return $ fromInteger n
unpackFloat (List [n])        = unpackFloat n
unpackFloat notFloat          = throwError $ TypeMismatch "Float" notFloat

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "String" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "Boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

boolNot :: LispVal -> LispVal
boolNot (Bool True)   = Bool False
boolNot (Bool False)  = Bool True

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
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
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
showValType [(Number n)]             = return $ String $ "Number" ++ show n
showValType [(Float (Short n))]      = return $ String $ "Short Float " ++show n
showValType [(Float (Long n))]       = return $ String $ "Long Float " ++ show n
showValType [(Bool True)]            = return $ String $ "Boolean #t"
showValType [(Bool False)]           = return $ String $ "Boolean #f"
showValType [(List contents)]        = return $ String $ "List (" ++ unwordsList contents ++ ")"
showValType [(DottedList head tail)] = return $ String $ "DottedList (" ++ unwordsList head ++ " . " ++ show tail ++ ")"
