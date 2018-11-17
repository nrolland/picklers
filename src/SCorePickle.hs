module SCorePickle (PU, pickle, unpickle, lift, sequ, base, belowBase, useState, toOctet) where

type St s = ([Char], s)  -- stream and state
data PU a p = PU { appP :: (a,St p) -> St p,
                   appU :: St p -> (a,St p) }

pickle :: PU a s -> s -> a -> String
pickle p s value = fst (appP p (value, ([],s)))

toOctet :: String -> [Int]
toOctet = fmap fromEnum

unpickle :: PU a s -> s -> String -> a
unpickle p s cs = fst (appU p (cs, s))

base :: Int
base = 256

belowBase :: PU Int p
belowBase = PU (\ (n,(cs,s)) -> (toEnum n : cs,s))
               (\ (c:cs,s) -> (fromEnum c, (cs,s)))

lift :: a -> PU a s
lift x = PU snd (\s -> (x,s))

sequ :: (b->a) -> PU a s -> (a -> PU b s) -> PU b s
sequ f pa k = PU (\ (b,(cs,s)) -> let a = f b
                                      pb = k a
                                      (cs' ,s' ) = appP pa (a, (cs'',s )) -- prepend a to (cs'' = after writing b) starting with state s
                                      (cs'',s'') = appP pb (b, (cs  ,s')) -- write b to cs starting with state s'
                                  in (cs',s''))                           --
              (\ s -> let (a,s') = appU pa s
                      in appU (k a) s')


useState :: (a -> s -> s) -> (s -> PU a s) -> PU a s
useState update spa =
       PU (\ (x,(cs,s)) -> let (cs',s') = appP (spa s) (x,(cs,s)) in (cs',update x s'))
          (\ (cs,s) -> let (x,(cs',s')) = appU (spa s) (cs,s) in (x,(cs',update x s')))


