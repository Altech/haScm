module Main where
import Control.Monad
import Control.Monad.Error
import Data.Char (chr)
import Data.IORef
import Numeric (readOct, readHex)
import System.Environment
import System.IO
import Text.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

-- Show
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Port _) = "<IO port>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList _head _tail) = "(" ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func params vararg body closure) = 
  "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ")...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Parser

symbol :: Parsec String u Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parsec String u ()
spaces = skipMany1 space

parseStringEscapedUnicode :: Parsec String u Char
parseStringEscapedUnicode = liftM (chr . read) $ char '\\' >> count 4 digit

parseStringEscapedASCII :: Parsec String u Char
parseStringEscapedASCII = liftM convert $ char '\\' >> noneOf ""
  where
    controlChars =  [('n','\n'),
                     ('t','\t'),
                     ('r','\r'),
                     ('0','\0')]
    convert c = case lookup c controlChars of
      Just v -> v
      Nothing -> c

parseString :: Parsec String u LispVal
parseString = do
  char '"'
  str <- many $ (try parseStringEscapedUnicode) <|> (try parseStringEscapedASCII) <|> noneOf "\""
  char '"'
  return $ String str

parseAtom :: Parsec String u LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumberDecimal :: Parsec String u LispVal
parseNumberDecimal = liftM (Number . read) $ (many1 digit) <|> (string "#d" >> many1 digit)

parseNumberHex :: Parsec String u LispVal
parseNumberHex = liftM (Number . fst . head . readHex) $ string "#x" >> many1 hexadecimal
  where hexadecimal = digit <|> oneOf "abcdef"

parseNumberOct :: Parsec String u LispVal
parseNumberOct = liftM (Number . fst . head . readOct) $ string "#o" >> many1 octal
  where octal = oneOf "01234567"

parseNumberBin :: Parsec String u LispVal
parseNumberBin = liftM (Number . readBin . reverse) $ string "#b" >> many1 binary
  where binary = oneOf "01"
        readBin str = case str of
          [] -> 0
          (b:bs) -> read [b] + 2 * readBin bs

parseNumber :: Parsec String u LispVal
parseNumber = (try parseNumberDecimal) <|> (try parseNumberHex) <|> (try parseNumberOct) <|> (try parseNumberBin)

parseList :: Parsec String u LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parsec String u LispVal
parseDottedList = do
  _head <- endBy parseExpr spaces
  _tail <- char '.' >> spaces >> parseExpr
  return $ DottedList _head _tail

parseQuoted :: Parsec String u LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote",x]

parseExpr :: Parsec String u LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString <|> parseQuoted <|> do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

-- Environment
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable: " var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable: " var) (liftIO . (flip writeIORef value)) (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined 
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef):env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where 
    extendEnv _bindings env = liftM (++ env) (mapM addBindings _bindings)
    addBindings (var, value) = do 
      ref <- newIORef value
      return (var,ref)



-- Evaluation
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", _pred, conseq, alt]) = do 
  result <- eval env _pred
  case result of
    Bool True  -> eval env conseq
    Bool False -> eval env alt
    notBool    -> throwError $ TypeMismatch "boolean" notBool
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var 
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var 
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do 
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialFrom "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where 
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs    = makeFunc . Just . showVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", \v -> return $ case head v of Number _ -> Bool True; _ -> Bool False),
              ("list?", \v -> return $ case head v of List _ -> Bool True; _ -> Bool False),
              ("symbol?", \v -> return $ case head v of Atom _ -> Bool True; _ -> Bool False),
              ("boolean?", \v -> return $ case head v of Number _ -> Bool True; _ -> Bool False),
              ("string?", \v -> return $ case head v of Number _ -> Bool True; _ -> Bool False),
              ("eq?", eq),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string>?", strBoolBinop (>)),
              ("string<?", strBoolBinop (<)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do 
                               left  <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right


numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
  if null parsed
  then throwError $ TypeMismatch "number" $ String n
  else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _tail] = return $ x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [x] _tail] = return _tail
cdr [DottedList (x:xs) _tail] = return $ DottedList xs _tail
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 1 badArgList

eq :: [LispVal] -> ThrowsError LispVal
eq [(Bool arg1), (Bool arg2)]     = return $ Bool (arg1 == arg2)
eq [(Number arg1), (Number arg2)] = return $ Bool (arg1 == arg2)
eq [(String arg1), (String arg2)] = return $ Bool (arg1 == arg2)
eq [(Atom arg1), (Atom arg2)]     = return $ Bool (arg1 == arg2)
eq [(DottedList xs x), (DottedList ys y)] = eq [List (xs ++ [x]) , List (ys ++ [y])]
eq [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqPair (zip arg1 arg2))
  where eqPair (x1,x2) = case eq [x1,x2] of
          Left err -> False
          Right (Bool val) -> val
eq [_, _] = return $ Bool False
eq badArgList = throwError $ NumArgs 2 badArgList

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)

readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents [String filename] = liftM String $ liftIO $ readFile filename

load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll [String filename] = liftM List $ load filename


-- Error
data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialFrom String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialFrom message form) = message ++ ":" ++ show form
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (NotFunction message found) = message ++ "; found " ++ found

instance Show LispError where show = showError
instance Error LispError where 
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- 
readOrThrow :: Parsec String () a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


-- REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show$ liftThrows (readExpr expr) >>= eval env 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ _pred prompt action = do 
  result <- prompt
  if _pred result
    then return ()
    else action result >> until_ _pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List . map String . drop 1 $ args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>> ") . evalAndPrint

-- default bindings
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) ioPrimitives)
  where makeFunc constructor (var, func) = (var, constructor func)

-- main
main :: IO ()
main = do 
  args <- getArgs
  if null args then runRepl else runOne $ args
