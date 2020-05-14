# pickler-combinators

Pickler combinators illustrated, as seen in A Kennedy's [paper](https://www.microsoft.com/en-us/research/publication/functional-pearl-pickler-combinators/)


`stack repl` to play with the exemple in `App/main.hs`

please post an issue in case of any pb


### Anatomy of a project like this

- package.yaml contains the two "components" 
pickler-combinators-exe and pickler-combinators-test and 
their required dependencies to packages, namely "pickler-combinators" library

- .ghci give the settings to the interactive shell GCHI and GHCID (https://github.com/ndmitchell/ghcid)

- stack.yaml gives access to availables packages

# Encoding and decoding



## A simple example of encoding

Say I declare a new type of structure, `Tree`, which has 2 values, `Leaf` and `Node` 


```haskell
data Tree = Leaf | Node Tree Tree -- a Node contains 2 subtrees

deriving instance Show Tree
```


and this is a *value* of type `Tree`


```haskell
atree :: Tree
atree = Node (Node Leaf Leaf) Leaf
```

how do I transmit such a value ? I encode it and decode it

```haskell
encode :: Tree -> String
encode Leaf = "0"
encode (Node l r) = "1" ++ encode l ++ encode r
-- encode atree
--"11000"
```


```haskell
decode :: String -> Tree
decode s = go s fst 
      where 
            go :: String -> ((Tree,String)->Tree) -> Tree
            go ('0':s) k = k(Leaf,s)
            go ('1':s) k = go s  (\(l, s') ->
                           go s' (\(r, s'') -> 
                           k(Node l r, s'')))
            go _ _ = trace "pb" undefined
```

(En impératif : on met les noeuds dans une pile. quand on rencontre un arbre fini, on complete les structures en attente. Si la structure attend la partie gauche on remplit et on replace dans la pile. Si la structure attend la partie droite, on remplit et, ayant un arbre fini, on remonte la pile. Si on est au bout de la pile, on a fini)



## Pickler

We want a generic solution to this problem, working for any type `a`, instead of a hard coded one working for `String` , and allow an extra parameter `PU a` to our `encode` and `decode` functions which will contain the previously hard-coded strategy for printing / parsing



```haskell
pickle :: PU a -> a -> String
unpickle :: PU a -> String -> a
```


This is the goal of the library of "picklers". They are *values* describing serialization *strategies*, or *formats*. A pickler of type `PU a` handles values of type `a`


For instance the value `string : PU String` is a pickler allowing to encode a value of type `String`
```haskell
s1 = pickle string "Hello World !"
s = unpickle string s1
```


We can combine those picklers. `nat : PU int` is a "pickler" for integer, and we can build a pickler for `Maybe int` using `pMaybe : PU a -> PU (Maybe a)` which is part of the library

```haskell
n1 = pickle (pMaybe nat) (Just 98)
on = unpickle (pMaybe nat) n1
```


That way, each pickler handle one type, and can be built from elementary picklers combined with "combinators" like `quad : PU a -> PU b -> PU c -> PU d -> PU (a,b,c,d)`, or `list : PU a -> PU [a]` below

```haskell
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
```


## A pickler with records

To get tighter integration with the language, we can make use of additional combinators like `wrap : (a -> b) -> (b -> a) -> PU a -> PU b`, breaking a particular feature (say records) into parts handled by the pickler's library  or `alt : (a -> int) -> [PU a] -> PU a`, which selects a specialized pickler.

```haskell
-- a record type
data URL = URL { protocol::String, host::String,
                 port::Maybe Int, file::String } deriving (Eq,Show)

-- wrap pickler combinator goes back and forth 
purl = wrap (\ (pr,h,po,f) -> URL {protocol=pr, host=h, port=po, file=f},
             \ URL {protocol=pr,host=h,port=po,file=f} -> (pr,h,po,f))
       (quad string string (pMaybe nat) string)

type Bookmarks = [Bookmark]

pbookmarks :: PU Bookmarks
pbookmarks = list pbookmark

data Bookmark = Link (String, URL) | Folder (String, Bookmarks) deriving (Eq, Show)

-- alt pickler combinator
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
```

We see that the specific *value* used as combinator follows the *type* of the elements we intend to store :
- `Bookmark` is isomorphic to a choice among 2 things;
- `alt` identifies at runtime which of those 2 choices has been provided and seelct a corresponding value based on that partial information

We could come up with **an** automated way to build picklers for a particular type. But many "formats" are possible, and it is frequent to want an *optimal* such, which might differ from a generic one.



## Implementation


The two key combinators are `lift`, which builds a very primitive pickler, and `sequ`, which combines two picklers.


`lift` declares a "format" that is an unconditional "hard coded" value, so that it is ignored upon encoding, and added upon decoding.

If the type `PU a` is implemented as a pair of pure function `encode :(a,String) -> String` and `decode : String -> (a,String)`  we would write

```haskell
lift :: a -> PU a
lift x = PU snd (\s -> (x,s))
```


`sequ` first extracts some partial information, for which it has a pickler, then based upon that information, select the best possible pickler for the overall structure. In code


```haskell
sequ :: (b -> a) -> PU a -> (a -> PU b) -> PU b
sequ proj (PU enca deca) sel = PU encode' decode'
            where
                --encode' :: (b, String) -> String
                encode' (b,s) = let a = proj a
                                    (PU encb _) = sel a
                                in enca (a, encb b))
                --decode' :: String -> (b,String)
                decode' s = let (a,s') = deca s
                                (PU _ decb) = sel a
                            in decb s'
```

ps : considering just their unpickling behaviour, `PU`, `lift` and `sequ` do make a monad.



## Compression with state threading

To encode with compression, we now need our picklers to describe not only "format" but "format with a state". 


