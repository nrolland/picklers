module Pickle (PU, pickle, unpickle, unit, char, bool, string, nat, zeroTo,
               wrap, alt, pair, triple, quad, pMaybe, pEither, list) where

import CorePickle
import Data.Maybe

pair :: PU a -> PU b -> PU (a,b)
pair pa pb = sequ fst pa (\ a -> --(a is not used for pb)
                            sequ snd pb (\ b ->
                                           lift (a,b)))

triple :: PU a -> PU b -> PU c -> PU (a,b,c)
triple pa pb pc = sequ (\ (x,y,z) -> x) pa (\a ->
                  sequ (\ (x,y,z) -> y) pb (\b ->
                  sequ (\ (x,y,z) -> z) pc (\c ->
                  lift (a,b,c))))

quad :: PU a -> PU b -> PU c -> PU d -> PU (a,b,c,d)
quad pa pb pc pd = sequ (\ (w,x,y,z) -> w) pa (\a ->
                   sequ (\ (w,x,y,z) -> x) pb (\b ->
                   sequ (\ (w,x,y,z) -> y) pc (\c ->
                   sequ (\ (w,x,y,z) -> z) pd (\d ->
                   lift (a,b,c,d)))))

wrap :: (a->b, b->a) -> PU a -> PU b
wrap (i,j) pa = sequ j pa (lift . i)

zeroTo :: Int -> PU Int
zeroTo 0 = lift 0
zeroTo n = wrap (\ (hi,lo) -> hi * base + lo, (`divMod` base))
                (pair (zeroTo (n `div` base)) belowBase)
unit :: PU ()
unit = lift ()

char :: PU Char
char = wrap (toEnum, fromEnum) (zeroTo 255)

bool :: PU Bool
bool = wrap (toEnum, fromEnum) (zeroTo 1)

nat :: PU Int
nat = sequ (\x -> if x < half then x else half + x `mod` half) -- project
           belowBase
           (\lo -> if lo < half then lift lo
                                else wrap (\hi->hi*half+lo, \n->n `div` half - 1) nat)
      where half = base `div` 2





fixedList :: PU a -> Int -> PU [a]
fixedList pa 0 = lift []
fixedList pa n = wrap (\(a,b) -> a:b, \(a:b) -> (a,b)) (pair pa (fixedList pa (n-1)))

list :: PU a -> PU [a]
list = sequ length nat . fixedList
      -- fixedList :: PU a -> (Int -> PU[a])
      -- (sequ length nat) :: (Int -> PU [a]) -> PU [a]

string :: PU String
string = list char

alt :: (a -> Int) -> [PU a] -> PU a
alt tag ps = sequ tag (zeroTo (length ps-1)) (ps !!)

pMaybe :: PU a -> PU (Maybe a)
pMaybe pa = alt tag [lift Nothing, wrap (Just, fromJust) pa]
            where tag Nothing = 0; tag (Just x) = 1

pEither :: PU a -> PU b -> PU (Either a b)
pEither pa pb = alt tag [wrap (Left, fromLeft) pa, wrap (Right, fromRight) pb]
                where tag (Left _) = 0; tag (Right _) = 1
                      fromLeft (Left a) = a; fromRight (Right b) = b

------

tokenize :: Eq a => [a] -> PU a -> PU a
tokenize dict p = sequ (\x -> case elem x dict 0 of
                                Just i -> n-i ; Nothing -> 0)
                  (zeroTo n)
                  (\i -> if i==0 then p else lift (dict !! (n-i)))
  where n = length dict
        elem :: Eq a => a -> [a] -> Int -> Maybe Int
        elem k [] _ = Nothing
        elem k (h:xs) i | k == h = Just i
        elem k (_:xs) i          = elem k xs (i+1)

add :: Eq a => a -> [a] -> [a]
add x d = if elem x d then d else x:d

memoFixedList :: Eq a => [a] -> PU a -> Int -> PU [a]
memoFixedList dict pa 0 = lift []
memoFixedList dict pa n = sequ head (tokenize dict pa) (\x ->
                          sequ tail (memoFixedList (add x dict) pa (n-1)) (\xs ->
                          lift (x:xs)))

memoList ::Eq a =>  [a] -> PU a -> PU [a]
memoList dict = sequ length nat . memoFixedList dict
