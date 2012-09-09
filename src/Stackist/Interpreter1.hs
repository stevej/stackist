module Stackist.Interpreter1(redex, Expr(Numeric, Literal)) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Text.Show.Functions

data Expr = Numeric Integer
          | Literal String
          | Quote Expr
            deriving (Show, Eq)


-- Ladies and Gentlemen, the standard library
-- All functions are typed: [Expr] -> [Expr]

-- | Adds two numbers on bottom of the stack
--
-- >>> redex [] [Numeric 1, Numeric 2, Literal "+"]
-- [Numeric 3]
_add (Numeric m : Numeric n : xs) = Numeric (m + n) : xs

-- | Subtracts the bottom number on the stack from the one above it
--
-- >>> redex [] [Numeric 3, Numeric 1, Literal "-"]
-- [Numeric 2]
_subtract (Numeric m : Numeric n : xs) = Numeric (n - m) : xs

-- | Multiples the bottom two numbers on the stack together
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
-- [Numeric 2,Numeric 3]
_swap (m : n : xs) = m : n : xs -- note they're already in reverse order


preludeMap :: Map.Map String ([Expr] -> [Expr])
preludeMap = Map.fromList
           [("+", _add),
            ("-", _subtract),
            ("*", _multiply),
            ("/", _divide),
            ("dup", _dup),
            ("swap", _swap)
           ]

-- If the user is looking for a function we don't have, throw an error.
preludeLookup s = Map.findWithDefault (\_ -> error ("function not found in library: " ++ s)) s preludeMap

-- the first [Expr] is the work-in-progress stack, which will be in reverse order
-- from the program list to faciliate pattern matching on the head.
redex :: [Expr] -> [Expr] -> [Expr]
-- | we're out of commands to run, we've finished reducing the stack.
--
-- >>> redex [] []
-- []
redex stack [] = stack
-- | push a number onto stack.
--
-- >>> redex [] [Numeric 1]
-- [Numeric 1]
redex stack (n @ (Numeric m) : xs) = redex (n : stack) xs
-- | Lookup the function named s, and run it against the stack.
redex stack (Literal f : xs) = redex result xs
                               where result = preludeLookup f stack
