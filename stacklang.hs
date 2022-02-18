-- Homework 5 - stacklang w/ ranks
-- Name: Peter LaMontagne
-- Course: CS381
-- Date: 2/16/22
module Stacklang where

-- Data Type Definitions

type Prog = [Cmd]

-- The stack supports Bools and Ints
type Stack = [Val]
data Val =  I Int | B Bool
  deriving Show

-- These are the commands supported
data Cmd  = LDI Int
  | LDB Bool
  | LEQ
  | ADD
  | MULT
  | DUP
  | IFELSE Prog Prog
  | DEC
  | SWAP
  | POP Int
  deriving Show

-- The rank of a stack is the number of elements
type Rank  = Int

-- The rank of a command is tuple (n, m) where n = ele
-- to add and m = ele to remove
type CmdRank = (Int, Int)

-- Final return type, supports error
data Final = RankError | TypeError | A [Int]
  deriving Show


-- Defines the rank of each supported command
rankC :: Cmd -> CmdRank
rankC (LDI a) = (0, 1)
rankC (LDB a) = (0, 1)
rankC LEQ = (2, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 2)
rankC DEC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP k) = (k, 0)
rankC (IFELSE prog1 prog2) = addCmdRank (largerRank (addAllRanks prog1) (addAllRanks prog2)) (1, 0)


-- Helper function - adds two CmdRanks
addCmdRank :: CmdRank -> CmdRank -> CmdRank
addCmdRank (a, b) (c, d) = (a + c, b + d)


-- Helper function - adds an array of [Cmd] into one CmdRank
addAllRanks :: Prog -> CmdRank
addAllRanks (x:xs) = addCmdRank (rankC x) (addAllRanks xs)
addAllRanks [] = (0, 0)


-- Helper function - determines the larger of two CmdRanks
largerRank :: CmdRank -> CmdRank -> CmdRank
largerRank (a, b) (c, d)
 | a < c = (c, d)
 | otherwise = (a, b)


-- rankP - Determines the resulting rank of a program
-- on a stack, or if there would be a RankError
rankP :: Prog -> Rank -> Maybe Rank
rankP (x:xs) i
  | addCmdRankR (rankC x ) i < 0 = Nothing
  | otherwise = rankP xs (addCmdRankR (rankC x ) i)  -- accounts for RankError
rankP [] i = Just i


-- Helper function - adds a command rank to a running
-- rank total
addCmdRankR :: CmdRank -> Rank -> Rank
addCmdRankR (a, b) i
  | i - a < 0 = -1 -- Forces RankError
  | otherwise = i - a + b

-- semStatTC - Checks if a program is rank valid,
-- otherwise produces a RankError
semStatTC :: Prog -> Stack -> Maybe Final
semStatTC prog s 
  | rankP prog (length s) /= Nothing = semCmd prog s
  | otherwise = Just RankError


-- Semantic definition of how to run a program
-- Run commands on a stack until no commands are left
semCmd :: Prog -> Stack -> Maybe Final
semCmd ((LDI a):ps) s = semCmd ps (I a:s) -- Append a on the stack
semCmd ((LDB a):ps) s = semCmd ps (B a:s) -- Same as above, but a Left bool type
semCmd (ADD:ps) ((I x):(I y):xs) = semCmd ps ((I (x + y)):xs) -- Add with INTs only
semCmd (MULT:ps) ((I x):(I y):xs) = semCmd ps ((I (x * y)):xs) -- Mult with INTs only
semCmd (DUP:ps) (x:xs) = semCmd ps (x:x:xs) -- Dupe with either type, just con twice
semCmd (LEQ:ps) (I x:I y:xs) -- Append a Left True/False depending on condition
  | x <= y = semCmd ps (B True:xs)
  | otherwise = semCmd ps (B False:xs)
semCmd ((IFELSE prog1 prog2):ps) (B x:xs) -- Append the program instructions, depending on condition
  | x = semCmd (prog1 ++ ps) xs
  | otherwise = semCmd (prog2 ++ ps) xs
semCmd (DEC:ps) (I x:xs) = semCmd ps ((I (x-1)):xs) -- Decrements the top element
semCmd (SWAP:ps) (x:y:xs) = semCmd ps (y:x:xs) -- Swap the top two elements on the stack
semCmd ((POP k):ps) s = semCmd ps (drop k s)
semCmd [] s = Just (A (unVal s)) -- Once out of commands, return the stack
semCmd _ _ = Just TypeError -- If it does not pattern match, it is an invalid command


-- Helper function - removes the Stack type
-- from the result to make it an array of Ints.
-- Also supports left over bools
unVal :: Stack -> [Int]
unVal ((I x):xs) = x:(unVal xs)
unVal ((B x):xs)
  | x = 1:(unVal xs)
  | otherwise = 0:(unVal xs)
unVal [] = []


-- Run - driver function. Evaluates stack and program
-- and returns either a result or an error
run :: Prog -> Stack -> Final
run prog s = fromJust (semStatTC prog s)

-- Helper function - removes the Just
-- from the result so it can be returned
-- as just a Final data type
fromJust :: Maybe Final -> Final
fromJust (Just TypeError)  = TypeError
fromJust (Just RankError)  = RankError
fromJust (Just x) = x



-- Test Cases:
test1 :: Prog
test1 = [LDI 3, DUP, ADD, LDI 5, SWAP]
test2 :: Prog
test2 = [LDI 8, POP 1, LDI 3, DUP, POP 2, LDI 4]
test3 :: Prog
test3 = [LDI 3, LDI 4, LDI 5, MULT, ADD]
test4 :: Prog
test4 = [LDI 2, ADD]
test5 :: Prog
test5 = [DUP]
test6 :: Prog
test6 = [POP 4]
test7 :: Prog
test7 = [LDB True, IFELSE [ADD] [LDI 7], ADD]
test8 :: Prog
test8 = [LDB True, LDI 1, LDI 10, LDI 5, IFELSE [ADD] [LDI 7], ADD]
test9 :: Prog
test9 = [LDI 20, LDI 1, LDI 10, LDI 5, LEQ, IFELSE [ADD] [LDI 7], DUP]
test10 :: Prog
test10 = [LDB True, LDB False, MULT]
test11 :: Prog
test11 = [LDI 10, DEC, DUP, DUP, DUP, POP 2]
test12 :: Prog
test12 = [LDI 10, LDI 20, LEQ, DEC]
test13 :: Prog
test13 = [LDI 10, LDI 5, LDB True, IFELSE [LDB True, IFELSE [ADD, DUP] [MULT]] [LDI 7]]
test14 :: Prog
test14 = [LDI 10, LDI 5, LDB True, IFELSE [LDB False, IFELSE [ADD, DUP] [MULT]] [LDI 7]]
test15 :: Prog
test15 = [LDI 10, LDI 5, LDB False, IFELSE [LDB True, IFELSE [ADD, DUP] [MULT]] [LDI 7]]
test16 :: Prog
test16 = [LDI 10, LDI 5, LDB False, IFELSE [LDB True, IFELSE [ADD, ADD] [MULT]] [LDI 7]]
