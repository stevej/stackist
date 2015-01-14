
{-# LANGUAGE RankNTypes #-}

module Stackist.Interpreter1(redex)
where

import Stackist.Parser (Expr(..))
import qualified Data.Map as Map

-- Ladies and Gentlemen, the standard library
-- All functions are typed: [Expr] -> [Expr]

interpError :: forall t a. Show a => [Char] -> a -> t
interpError name xs = error (name ++ " did not expect " ++ (show xs))

_id :: a -> a
_id = id

-- | i : [P] -> ... Executes P. So, [P] i == P.
--
-- >> redex [] [JString "a", JString "b", Quote [Literal "concat"], Literal "i"]
-- [JString "ab"]
_i :: [Expr] -> [Expr]
_i (Quote q : xs) = redex xs q
_i xs = interpError "i" xs

-- | Adds two numbers on bottom of the stack
--
-- >> redex [] [Numeric 1, Numeric 2, Literal "+"]
-- [Numeric 3]
_add :: [Expr] -> [Expr]
_add (Numeric m : Numeric n : xs) = Numeric (m + n) : xs
_add xs = interpError "add" xs

-- | Subtracts the bottom number on the stack from the one above it
--
-- >>> redex [] [Numeric 3, Numeric 1, Literal "-"]
-- [Numeric 2]
_subtract :: [Expr] -> [Expr]
_subtract (Numeric m : Numeric n : xs) = Numeric (n - m) : xs
_subtract xs = interpError "-" xs

-- | Multiplies the bottom two numbers on the stack together
--
-- >>> redex [] [Numeric 2, Numeric 3, Literal "*"]
-- [Numeric 6]
_multiply :: [Expr] -> [Expr]
_multiply (Numeric m : Numeric n : xs) = Numeric (n * m) : xs
_multiply xs = interpError "*" xs

-- | Divides the 2nd item on the stack by the 1st item
--
-- >>> redex [] [Numeric 6, Numeric 2, Literal "/"]
-- [Numeric 3]
_divide :: [Expr] -> [Expr]
_divide (Numeric m : Numeric n : xs) = Numeric (div n m) : xs -- this may be reversed.
_divide xs = interpError "/" xs

-- | Duplicates the top item on the stack
--
-- >>> redex [] [Numeric 2, Literal "dup"]
-- [Numeric 2,Numeric 2]
_dup :: [Expr] -> [Expr]
_dup (m : xs) = m : m : xs
_dup xs = interpError "dup" xs

-- | Swaps the two top items on the stack
--
-- >>> redex [] [Numeric 3, Numeric 2, Literal "swap"]
-- [Numeric 3,Numeric 2]
_swap :: [Expr] -> [Expr]
_swap (m : n : xs) = n : m : xs
_swap xs = interpError "swap" xs

-- | dip : X [P] -> ... X
-- Saves X, executes P, pushes X back.
--
-- >>> redex [] [Numeric 1, Quote [Numeric 2, Numeric 2, Literal "+"], Literal "dip"]
-- [Numeric 4,Numeric 1]
_dip :: [Expr] -> [Expr]
_dip (Quote q : m : xs) = r ++ (m : xs)
                        where r = _i (Quote q : xs)
_dip xs = interpError "dip" xs

-- | [P] -> R Executes P, which leaves R on top of the stack. No matter
-- how many parameters this consumes, none are removed from the stack.
--
-- >>> redex [] [Numeric 2, Quote [Numeric 1, Literal "+"], Literal "nullary"]
-- [Numeric 3,Numeric 2]
_nullary :: [Expr] -> [Expr]
_nullary (Quote q : xs) = (redex xs q) ++ xs
_nullary xs = interpError "nullary" xs

-- | S T -> U Sequence U is the concatenation of sequences S and T.
--
-- >>> redex [] [JString "a", JString "b", Literal "concat"]
-- [JString "ab"]
_concat :: [Expr] -> [Expr]
_concat (JString s : JString t : xs) = JString (t ++ s) : xs
_concat (Quote a : Quote b : xs) = Quote (b ++ a) : xs
_concat xs = interpError "concat" xs

-- | fold : A V0 [P] -> V Starting with value V0, sequentially pushes
-- members of aggregate A and combines with binary operator P to produce value V.
-- fold = swapd step
-- (SJ: I assumed this would be a foldRight but is left)
-- foldl (\a b -> a ++ b) "a" ["b", "c", "d"] => "abcd"
--
-- >>> redex [] [Quote [JString "b", JString "c", JString "d"], JString "a", Quote [Literal "concat"], Literal "fold"]
-- [JString "abcd"]
_fold :: [Expr] -> [Expr]
_fold xs = redex xs [Literal "swapd", Literal "step"]


-- | swapd : X Y Z -> Y X Z
-- FIXME: I don't know if I'm following this.
--
-- >>> redex [] [JString "a", JString "b", JString "c", Literal "swapd"]
-- [JString "c",JString "a",JString "b"]
_swapd :: [Expr] -> [Expr]
_swapd (c : b : a : xs) = c : a : b : xs
_swapd xs = interpError "swapd" xs


-- | step : A [P] -> ...
-- Sequentially putting members of aggregate A onto stack, executes
-- P for each member of A.
--
-- >>> redex [] [Quote [Numeric 1, Numeric 2], Quote [Numeric 2, Literal "*"], Literal "step"]
-- [Numeric 4,Numeric 2]
--
-- >>> redex [Quote [Literal "concat"],Quote [JString "b",JString "c",JString "d"],JString "a"] [Literal "step"]
-- [JString "abcd"]
step' :: [Expr] -> [Expr] -> [Expr] -> [Expr]
step' _ [] stack = stack
step' f (x:xs) stack = step' f xs (redex (x : stack) f)

_step :: [Expr] -> [Expr]
_step (Quote f : Quote qs : xs) = step' f qs xs
_step xs = interpError ("step did not expect" ++ (show xs)) xs


preludeMap :: Map.Map String ([Expr] -> [Expr])
preludeMap = Map.fromList
           [("+", _add),
            ("-", _subtract),
            ("*", _multiply),
            ("/", _divide),
            ("id", _id),
            ("dup", _dup),
            ("swap", _swap),
            ("nullary", _nullary),
            ("concat", _concat),
            ("fold", _fold),
            ("dip", _dip),
            ("i", _i),
            ("swapd", _swapd),
            ("step", _step)
           ]

-- If the user is looking for a function we don't have, throw an error.
preludeLookup :: [Char] -> [Expr] -> [Expr]
preludeLookup s = Map.findWithDefault (\_ -> error ("function not found in library: " ++ s)) s preludeMap

-- the first [Expr] is the data stack, which will be in reverse order
-- from the program list to faciliate pattern matching on the head. The second [Expr] is
-- what we haven't processed yet.
redex :: [Expr] -> [Expr] -> [Expr]
-- | we're out of commands to run, we've finished reducing the program stack.
--
-- >>> redex [] []
-- []
redex stack [] = stack

-- >>> redex [] [Boolean True]
-- [True]
redex stack (b @ (Boolean _) : xs) = redex (b : stack) xs

-- | push a quotation onto the stack
--
-- >>> redex [] [Quote [Literal 1]]
-- [Quote [Literal 1]]
redex stack (q @ (Quote _) : xs) = redex (q : stack) xs
-- | push a JString onto the stack
--
-- >>> redex [] [JString "hey"]
-- [JString "hey"]
redex stack (j @ (JString _) : xs) = redex (j : stack) xs
-- | push a number onto stack.
--
-- >>> redex [] [Numeric 1]
-- [Numeric 1]
redex stack (n @ (Numeric _) : xs) = redex (n : stack) xs
-- | Lookup the function named s, and run it against the stack.
redex stack (Literal f : xs) = redex result xs
                               where result = preludeLookup f stack
