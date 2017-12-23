module FunctionsWithLet where

printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' n = print (n + 2)
