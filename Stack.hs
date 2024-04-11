module Stack where

import Data.Maybe

type Stack a = [a]

stackPush :: a -> Stack a -> Stack a
stackPush = (:)

stackPop :: Stack a -> Maybe (Stack a, a)
stackPop [] = Nothing
stackPop (x : xs) = Just (xs, x)

stackPeek :: Stack a -> Maybe a
stackPeek [] = Nothing
stackPeek (x : xs) = Just x

stackSize :: (Integral b) => Stack a -> b
stackSize = foldr (\x -> (+) 1) 0

stackEmpty :: Stack a -> Bool
stackEmpty = null