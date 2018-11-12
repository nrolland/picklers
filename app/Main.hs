{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Pickle
import CorePickle
import qualified SPickle as S


-- A simple pickler
--------------------

type URL1 = (String, String, Maybe Int, String)
type Bookmark1 = (String, URL1)
type Bookmarks1 = [Bookmark1]

purl1 :: PU URL1
purl1 = quad string string (pMaybe nat) string

pbookmarks1 :: PU Bookmarks1
pbookmarks1 = list $ pair string purl1

bookmarks1 = [ ("Andrew", ("http", "research.microsoft.com", Nothing,"users/akenn") :: URL1) ] :: Bookmarks1

b1 = pickle pbookmarks1 bookmarks1
u1 = unpickle pbookmarks1 b1







-- A more realistic one with records
------------------------------------

data URL = URL { protocol::String, host::String, port::Maybe Int, file::String } deriving (Eq,Show)

purl = wrap (\ (pr,h,po,f) -> URL {protocol=pr, host=h, port=po, file=f},
             \ URL {protocol=pr,host=h,port=po,file=f} -> (pr,h,po,f))
       (quad string string (pMaybe nat) string)

type Bookmarks = [Bookmark]

pbookmarks :: PU Bookmarks
pbookmarks = list pbookmark

data Bookmark = Link (String, URL) | Folder (String, Bookmarks) deriving (Eq, Show)

pbookmark :: PU Bookmark
pbookmark = alt tag [wrap (Link,   \(Link   a) -> a) (pair string purl),
                     wrap (Folder, \(Folder a) -> a) (pair string pbookmarks)]
                where tag (Link _) = 0; tag (Folder _) = 1

bk :: Bookmarks = [Link(
                     "Andrew",
                     URL { protocol = "http",
                           host = "research.microsoft.com",
                           port = Nothing,
                           file = "users/akenn" })]

bitb = pickle pbookmarks bk
u = unpickle pbookmarks bitb
_ = unpickle pbookmark bitb -- no safety


-- Lambda terms
--------------------------


data Lambda = Var String | Lam (String, Lambda) | App (Lambda, Lambda) deriving (Show, Eq)

plambda = alt tag [ wrap (Var, \(Var x) -> x) string,
                    wrap (Lam, \(Lam x) -> x) (pair string plambda),
                    wrap (App, \(App x) -> x) (pair plambda plambda) ]
               where tag (Var _) = 0; tag (Lam _) = 1; tag (App _) = 2
-- structure dans le code

plambdaSOP :: PU Lambda
plambdaSOP = wrap (sumlam,lamsum)
              (pEither string (pEither (pair string plambdaSOP) (pair plambdaSOP plambdaSOP)))
               where
                 lamsum (Var x) = Left x
                 lamsum (Lam x) = Right (Left x)
                 lamsum (App x) = Right (Right x)
                 sumlam (Left x)          = Var x
                 sumlam (Right (Left  x)) = Lam x
                 sumlam (Right (Right x)) = App x
-- structure in a value of type path ~ [direction]
-- data Lambda = Var String
--                    + ( Lam (String, Lambda)
--                           + App (Lambda, Lambda))

x = Var "x"
i = Lam("x", x)
k = Lam("x", Lam("y", x))
kki = App(k, App(k, i))
kki' = App (Lam ("x",Lam ("y",Var "x")),App (Lam ("x",Lam ("y",Var "x")),Lam ("x",Var "x")))

bitl = pickle   plambda kki --  [2,1,1,'x',1,1,'y',0,1,'x',2,1,1,'x',1,1,'y',0,1,'x',1,1,'y',0,1,'x']
l    = unpickle plambda bitl



-- Pickler with sharing for homogenous list
--------------------------------------------


psurl = S.wrap (\ (pr,h,po,f) -> URL {protocol=pr, host=h, port=po, file=f},
                \ URL {protocol=pr,host=h,port=po,file=f} -> (pr,h,po,f))
        (S.quad S.string S.string (S.pMaybe S.nat) S.string)

psbookmark :: S.PU Bookmark [a]
psbookmark = S.alt tag [S.wrap (Link,   \(Link   a) -> a) (S.pair S.string psurl),
                     S.wrap (Folder, \(Folder a) -> a) (S.pair S.string psbookmarks)]
                where tag (Link _) = 0; tag (Folder _) = 1

psbookmarks = S.memoList [] psbookmark


b = Link("Andrew",URL{protocol="http",host="research.microsoft.com",port=Nothing,file="users/akenn"})
brepeat = [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]

sbits1 = CorePickle.pickle pbookmarks brepeat
sbits2 = S.pickle psbookmarks [] brepeat

su = S.unpickle psbookmarks [] sbits2
su' = S.unpickle psbookmark sbits2




-- Pickler with sharing for general structure
----------------------------------------------



-- data Lambda = Var String | Lam (String, Lambda) | App (Lambda, Lambda) deriving (Show, Eq)

slambda = S.share (S.alt tag [ S.wrap (Var, \(Var x) -> x) S.string,
                               S.wrap (Lam, \(Lam x) -> x) (S.pair S.string slambda),
                               S.wrap (App, \(App x) -> x) (S.pair slambda slambda) ] )
                         where tag (Var _) = 0; tag (Lam _) = 1; tag (App _) = 2

-- kki = App (Lam ("x",Lam ("y",Var "x")),App (Lam ("x",Lam ("y",Var "x")),Lam ("x",Var "x")))

bitls = S.pickle   slambda [] kki --  [2,1,1,'x',1,1,'y',0,1,'x',0,2,3,0,1,1,'x',1]
l'    = S.unpickle slambda [] bitls


-- 02              6 App(                    -- tag de App
-- 01 01 78            3 Lam("x",            -- tag de Lam, longueur de chaine, 'x'=78
-- 01 01 79               2 Lam("y",         -- tag de Lam, longueur de chaine, 'y'
-- 00 01 78                   1 Var "x")),   -- tag de Var, longueur de chaine, 'x'
-- 00
--    02                5 App(                --
--    03                       k,               -- 3 au dessus de 2
--    00
--       01 01 78             4 Lam("x",
--       01                          x)))
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

main :: IO ()
main = someFunc
