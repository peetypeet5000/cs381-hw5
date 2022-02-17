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
data Final = RankError | TypeError | A Stack
  deriving Show


-- Defines the rank of each supported command
rankC :: Cmd -> CmdRank
rankC (LDI a) = (0, 1)
rankC (LDB a) = (0, 1)
rankC LEQ = (2, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 1)
rankC DEC = (1, 1)
rankC SWAP = (2, 1)
rankC (POP k) = (k, 0)
rankC (IFELSE prog1 prog2) = addCmdRank (largerRank(addAllRanks prog1) (addAllRanks prog2)) (1,0)


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
rankP prog i
  | rank prog i < 0 = Nothing -- accounts for RankError
  | otherwise = Just (rank prog i)

rank :: Prog -> Rank -> Rank
rank prog i = (i - fst (addAllRanks prog))

-- evalStatTC - Checks if a program is rank valid,
-- otherwise produces a RankError
evalStatTC :: Prog -> Stack -> Maybe Stack
evalStatTC prog s 
  | rankP prog (length s) /= Nothing = semCmd prog s
  | otherwise = Nothing


-- Semantic definition of how to run a program
-- Run commands on a stack until no commands are left
semCmd :: Prog -> Stack -> Maybe Stack
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
semCmd [] s = Just s -- Once out of commands, return the stack
semCmd _ _ = Nothing -- If it does not pattern match, it is an invalid command

-- run->stemstattc(check for rankerror)->semcmd (check for type error)
run :: Prog -> Stack -> Final
run prog s = semStatTC prog s



-- Test Cases:
a :: Prog
a = [LDB False, IFELSE [LDI 1] [LDI 2]]
b :: Prog
b =[ADD]

s :: Stack
s = [I 5, I 5, B True, I 2, I 3]
p :: Stack
p = [I 1]
