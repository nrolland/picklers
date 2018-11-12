module CorePickle ( PU, pickle, unpickle, lift, sequ, base, belowBase) where

-- data PU a
-- pickle :: PU a -> a -> String
-- unpickle :: PU a -> String -> a

-- belowBase :: PU Int
-- lift :: a -> PU a
-- sequ :: (b->a) -> PU a -> (a -> PU b) -> PU b


type St = [Char] -- ~ String

data PU a = PU { appP :: (a,St) -> St,
                 appU :: St -> (a,St) }

pickle :: PU a -> a -> String
pickle p value = appP p (value, [])

unpickle :: PU a -> String -> a
unpickle p stream = fst (appU p stream)

base :: Int
base = 256 -- 1 octet 2^8 = 2 hexa values 2^4

belowBase :: PU Int
belowBase = PU (\ (n,s) -> toEnum n : s)
               (\ (c:s) -> (fromEnum c, s))
lift :: a -> PU a
lift x = PU snd (\s -> (x,s))

sequ :: (b->a) -> PU a -> (a -> PU b) -> PU b
sequ f pa k = PU (\ (b,s) -> let a = f b  --partial info on b
                                 pb = k a --select special pickler
                             in appP pa (a, appP pb (b,s)))
                 (\ s -> let (a, s') = appU pa s -- a first
                             pb = k a --we get a special pickler for b
                         in appU pb s') -- give (b,s'') back

