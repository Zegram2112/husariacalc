module Lib
    ( solveRPN, humanSolveRPN
    ) where

import Text.Read (readMaybe)
import Data.Number.CReal (CReal)

data RPNError = InvalidToken String
    | MoreArgumentsRequired String [Factor]
    deriving (Show, Eq)
type Factor = CReal 
type Operator = String

ifRightThen :: (b -> c) -> Either a b -> Either a c
ifRightThen = fmap

humanSolveRPN :: String -> String
humanSolveRPN = either humanError show . solveRPN

humanError :: RPNError -> String
humanError (InvalidToken token) = "'" ++ token ++ "' is an invalid token"
humanError (MoreArgumentsRequired op factors) =
    "The operation " ++ op ++ " requires more arguments, only " ++ (show . length) factors
    ++ " argument" ++ (if length factors == 1 then " was" else "s were") ++ " provided"

solveRPN :: String -> Either RPNError Factor
solveRPN = ifRightThen head . foldl process (Right []) . words
    where process :: Either RPNError [Factor] -> String -> Either RPNError [Factor]
          process (Left err) token = Left err
          process (Right xs) token
            | token `elem` binops = processWithBinop xs token
            | token `elem` unops  = processWithUnop xs token
            | otherwise           = processWithFactor xs token
          processWithUnop (x:xs) op    = Right (solveUnop op x : xs)
          processWithUnop xs op        = Left (MoreArgumentsRequired op xs)
          processWithBinop (x:y:ys) op = Right (solveBinop op y x : ys)
          processWithBinop xs op       = Left (MoreArgumentsRequired op xs)
          processWithFactor xs factor  = ifRightThen (:xs) . readFactor $ factor

binops :: [Operator]
binops = ["*", "+", "-", "/", "^"]
unops :: [Operator]
unops = ["ln"]

solveBinop :: Operator -> Factor -> Factor -> Factor
solveBinop "*" x y = x * y
solveBinop "+" x y = x + y
solveBinop "-" x y = x - y
solveBinop "/" x y = x / y
solveBinop "^" x y = x ** y
solveBinop op _ _  = error $ "operation not defined: " ++ op

solveUnop :: Operator -> Factor -> Factor
solveUnop "ln" x = log x
solveUnop op _ = error $ "operation not defined: " ++ op
        
readFactor :: String -> Either RPNError Factor
readFactor factor = let parsing = readMaybe factor in
                    case parsing of
                        Just x -> Right x
                        Nothing -> Left (InvalidToken factor)