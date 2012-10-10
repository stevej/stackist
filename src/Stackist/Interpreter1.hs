module Stackist.Interpreter1(redex, Expr(Numeric, Literal)) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Text.Show.Functions

data Expr = Numeric Integer
          | Literal String
          | JString String -- is "quoted"
          | Quote [Expr]
            deriving (Show, Eq)

-- Ladies and Gentlemen, the standard library
-- All functions are typed: [Expr] -> [Expr]

interpError name xs = error (name ++ " did not expect " ++ (show xs))

_id = id

-- | i : [P] -> ... Executes P. So, [P] i == P.
--
-- >> redex [] [JString "a", JString "b", Quote [Literal "concat"], Literal "i"]
-- [JString "ab"]
_i (Quote q : xs) = redex xs q

-- | Adds two numbers on bottom of the stack
--
-- >> redex [] [Numeric 1, Numeric 2, Literal "+"]
-- [Numeric 3]
_add (Numeric m : Numeric n : xs) = Numeric (m + n) : xs

-- | Subtracts the bottom number on the stack from the one above it
--
-- >>> redex [] [Numeric 3, Numeric 1, Literal "-"]
-- [Numeric 2]
_subtract (Numeric m : Numeric n : xs) = Numeric (n - m) : xs

-- | Multiplies the bottom two numbers on the stack together
--
-- >>> redex [] [Numeric 2, Numeric 3, Literal "*"]
-- [Numeric 6]
_multiply (Numeric m : Numeric n : xs) = Numeric (n * m) : xs

-- | Divides the 2nd item on the stack by the 1st item
--
-- >>> redex [] [Numeric 6, Numeric 2, Literal "/"]
-- [Numeric 3]
_divide (Numeric m : Numeric n : xs) = Numeric (div n m) : xs -- this may be reversed.

-- | Duplicates the top item on the stack
--
-- >>> redex [] [Numeric 2, Literal "dup"]
-- [Numeric 2,Numeric 2]
_dup (m : xs) = m : m : xs

-- | Swaps the two top items on the stack
--
-- >>> redex [] [Numeric 3, Numeric 2, Literal "swap"]
-- [Numeric 3,Numeric 2]
_swap (m : n : xs) = n : m : xs

-- | dip : X [P] -> ... X
-- Saves X, executes P, pushes X back.
--
-- >>> redex [] [Numeric 1, Quote [Numeric 2, Numeric 2, Literal "+"], Literal "dip"]
-- [Numeric 4,Numeric 1]
_dip (Quote q : m : xs) = r ++ (m : xs)
                        where r = _i (Quote q : xs)

-- | [P] -> R Executes P, which leaves R on top of the stack. No matter
-- how many parameters this consumes, none are removed from the stack.
--
-- >>> redex [] [Quote [Numeric 1, Numeric 2, Literal "+"], Literal "nullary"]
-- [Numeric 3]
_nullary (Quote q : xs) = (redex [] q) ++ xs

-- | S T -> U Sequence U is the concatenation of sequences S and T.
--
-- >>> redex [] [JString "a", JString "b", Literal "concat"]
-- [JString "ab"]
_concat (JString s : JString t : xs) = JString (t ++ s) : xs
_concat xs = interpError "concat" xs

-- | fold : A V0 [P] -> V Starting with value V0, sequentially pushes
-- members of aggregate A and combines with binary operator P to produce value V.
-- fold = swapd step
-- (SJ: I assumed this would be a foldRight but is left)
-- List("b", "c", "d").foldLeft("a")((a,b) => a + b)
-- res1: java.lang.String = abcd
--
-- >> redex [] [Quote [JString "b", JString "c", JString "d"], JString "a", Quote [Literal "concat"], Literal "fold"]
-- [JString "abcd"]
-- this is broken
_fold xs = redex xs [Literal "swapd", Literal "step"]


-- | swapd : X Y Z -> Y X Z
--
-- >>> redex [] [JString "a", JString "b", JString "c", Literal "swapd"]
-- [JString "c",JString "a",JString "b"]
_swapd (z : y : x : xs) = z : x : y : xs


-- | step : A [P] -> ...
-- Sequentially putting members of aggregate A onto stack, executes
-- P for each member of A.
--
-- >>> redex [] [Quote [Numeric 1, Numeric 2], Quote [Numeric 2, Literal "*"], Literal "step"]
-- [Numeric 2,Numeric 4]
_step (Quote f : Quote qs : xs) = (concat $ map f' qs) ++ xs
                              where f' quote = redex [quote] f
_step xs = error ("did not expect" ++ (show xs))


-- redex :: [Expr] -> [Expr] -> [Expr]


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
-- | push a quotation onto the stack
--
-- >>> redex [] [Quote [Literal 1]]
-- [Quote [Literal 1]]
redex stack (q @ (Quote _) : xs) = redex (q : stack) xs
-- | push a JString onto the stack
--
-- >>> redex [] [JString "hey"]
-- [JString "hey"]
redex stack (j @ (JString _) : xs) = redex (j :stack) xs
-- | push a number onto stack.
--
-- >>> redex [] [Numeric 1]
-- [Numeric 1]
redex stack (n @ (Numeric _) : xs) = redex (n : stack) xs
-- | Lookup the function named s, and run it against the stack.
redex stack (Literal f : xs) = redex result xs
                               where result = preludeLookup f stack
