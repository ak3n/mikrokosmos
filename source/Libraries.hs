module Libraries
  ( stdlibraries
  , stdmap
  , stdquery
  , code
  , name
  , description
  )
where

import qualified Data.Map as M
import Control.Arrow

stdlibraries :: String
stdlibraries = unlines $ map code stdfunctions

stdquery :: String -> Maybe Function
stdquery = flip M.lookup stdmap

stdmap :: M.Map String Function
stdmap = M.fromList $ map (name &&& id) stdfunctions

data Function = Function
  { name :: String
  , code :: String
  , description :: String
  }
  deriving (Show)

stdfunctions :: [Function]
stdfunctions = 
  [ Function "id" "id = \\x.x" "Identity function. Returns its argument unchanged."
  , Function "true" "true = \\a.\\b.a" "Boolean 'true', using Church encoding."
  , Function "false" "false = \\a.\\b.b" "Boolean 'false', using Church encoding."
  , Function "ifelse" "ifelse = (\\x.x)" "Identity function, can be used as a boolean case analysis."
  , Function "succ" "succ = \\n.\\f.\\x.f (n f x)" "Return the successor of a natural number."
  , Function "0" "0 = \\f.\\x.x" "The natural number 0, using Church encoding."
  , Function "plus" "plus = \\m.\\n.\\f.\\x.m f (n f x)" "Adds two natural numbers."
  , Function "mult" "mult = \\m.\\n.\\f.\\x.m (n f) x" "Multiplies two natural numbers."
  , Function "pred" "pred = \\n.\\f.\\x.n (\\g.(\\h.h (g f))) (\\u.x) (\\u.u)" "Predecessor of a natural number"
  , Function "minus" "minus = \\m.\\n.(n pred) m" "Substracts two natural numbers"
  , Function "Y" "Y != \\f.(\\x.f (x x))(\\x.f (x x))" "Y combinator. Fixed-point combinator."
  , Function "tuple" "tuple = \\x.\\y.\\z.z x y" "Untyped tuple constructor. Takes a and b and returns the tuple (a,b)."
  , Function "first" "first = \\p.p true" "Untyped tuple projection. Returns the first element of a tuple."
  , Function "second" "second = \\p.p false" "Untyped tuple projection. Returns the second element of a tuple."
  , Function "omega" "omega := (\\x.(x x))(\\x.(x x))" "Omega combinator. An example of a non-reducible lambda calculus expression."
  , Function "fix" "fix := (\\f.(\\x.f (x x)) (\\x.f (x x)))" "Fixed-point combinator. Given f, returns an element x such that f x = x."
  , Function "fact" "fact := fix (\\f.\\n.iszero n (succ 0) (mult n (f (pred n))))" "Factorial of a natural number."
  , Function "fib" "fib := fix (\\f.\\n.iszero n (succ 0) (plus (f (pred n)) (f (pred (pred n)))))" "Returns the n-th Fibonacci number."
  ] ++ natsdef
  where
    fnat :: Int -> Function
    fnat n = Function
      (show (n+1))
      (show (n+1) ++ " = succ " ++ show n)
      ("The natural number " ++ show (n+1) ++ ", using Church encoding.")
    
    natsdef :: [Function]
    natsdef = map fnat [0..999]
     
