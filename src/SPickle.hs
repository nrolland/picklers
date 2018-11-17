module SPickle (PU, pickle, unpickle, unit, char, bool, string, nat, zeroTo,
               wrap, alt, pair, triple, quad, pMaybe, pEither, list, memoList,
               share, shareFst, shareSnd) where
import SCorePickle
import Data.Maybe
import Data.List

pair :: PU a s -> PU b s -> PU (a,b) s
pair pa pb = sequ fst pa (\ a ->
                            sequ snd pb (\ b ->
                                           lift (a,b)))

triple :: PU a s -> PU b s -> PU c s -> PU (a,b,c) s
triple pa pb pc = sequ (\ (x,y,z) -> x) pa (\a ->
                  sequ (\ (x,y,z) -> y) pb (\b ->
                  sequ (\ (x,y,z) -> z) pc (\c ->
                  lift (a,b,c))))

quad :: PU a s -> PU b s -> PU c s -> PU d s -> PU (a,b,c,d) s
quad pa pb pc pd = sequ (\ (w,x,y,z) -> w) pa (\a ->
                   sequ (\ (w,x,y,z) -> x) pb (\b ->
                   sequ (\ (w,x,y,z) -> y) pc (\c ->
                   sequ (\ (w,x,y,z) -> z) pd (\d ->
                   lift (a,b,c,d)))))

wrap :: (a->b, b->a) -> PU a s -> PU b s
wrap (i,j) pa = sequ j pa (lift . i)

zeroTo :: Int -> PU Int s
zeroTo 0 = lift 0
zeroTo n = wrap (\ (hi,lo) -> hi * base + lo, (`divMod` base))
                (pair (zeroTo (n `div` base)) belowBase)
unit :: PU () s
unit = lift ()

char :: PU Char s
char = wrap (toEnum, fromEnum) (zeroTo 255)

bool :: PU Bool s
bool = wrap (toEnum, fromEnum) (zeroTo 1)

nat :: PU Int s
nat = sequ (\x -> if x < half then x else half + x `mod` half)
           belowBase
           (\lo -> if lo < half then lift lo
                                else wrap (\hi->hi*half+lo, \n->n `div` half - 1) nat)
      where half = base `div` 2

fixedList :: PU a s -> Int -> PU [a] s
fixedList pa 0 = lift []
fixedList pa n = wrap (\(a,b) -> a:b, \(a:b) -> (a,b)) (pair pa (fixedList pa (n-1)))

list :: PU a s -> PU [a] s
list = sequ length nat . fixedList

string :: PU String s
string = list char

alt :: (a -> Int) -> [PU a s] -> PU a s
alt tag ps = sequ tag (zeroTo (length ps-1)) (ps !!)

pMaybe :: PU a s -> PU (Maybe a) s
pMaybe pa = alt tag [lift Nothing, wrap (Just, fromJust) pa]
            where tag Nothing = 0; tag (Just x) = 1

pEither :: PU a s -> PU b s -> PU (Either a b) s
pEither pa pb = alt tag [wrap (Left, fromLeft) pa, wrap (Right, fromRight) pb]
                where tag (Left _) = 0; tag (Right _) = 1
                      fromLeft (Left a) = a; fromRight (Right b) = b
------
tokenize :: Eq a => [a] -> PU a s -> PU a s
tokenize dict p = sequ (\x -> case elem x dict 0 of   -- utilise le dico pour specialiser
                                Just i -> n-i ; Nothing -> 0)
                  (zeroTo n)  -- encode dans le stream n-i si on a trouve le terme sinon 00
                  (\i -> if i==0 then p else lift (dict !! (n-i))) -- lift rajoute la valeur au decodage
  where n = length dict
        elem :: Eq a => a -> [a] -> Int -> Maybe Int
        elem k [] _ = Nothing
        elem k (h:xs) i | k == h = Just i
        elem k (_:xs) i          = elem k xs (i+1)

add :: Eq a => a -> [a] -> [a]
add x d = if elem x d then d else x:d

memoFixedList :: Eq a => [a] -> PU a s -> Int -> PU [a] s
memoFixedList dict pa 0 = lift []
memoFixedList dict pa n = sequ head (tokenize dict pa) (\x ->  -- projette head, encode, enrichit le dictionnaire
                          sequ tail (memoFixedList (add x dict) pa (n-1)) (\xs ->
                          lift (x:xs)))

memoList ::Eq a =>  [a] -> PU a s -> PU [a] s
memoList dict = sequ length nat . memoFixedList dict --  [a] -> PU a s  -> PU [a] s

share :: Eq a => PU a [a] -> PU a [a]
share p = useState add (\dict -> tokenize dict p)-- pickler parametre

---------
shareFst :: Eq a => PU a ([a],s) -> PU a ([a],s)
shareFst p = useState (\x -> \(s1,s2) -> (add x s1, s2))
                      (\(s1,s2) -> tokenize s1 p)
shareSnd :: Eq a => PU a (s,[a]) -> PU a (s,[a])
shareSnd p = useState (\x -> \(s1,s2) -> (s1, add x s2))
                      (\(s1,s2) -> tokenize s2 p)
