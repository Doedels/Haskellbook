module FunctionsWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2

printInc' n = print (n + 2)

printIn n = n + 2
