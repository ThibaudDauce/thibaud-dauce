---
title: 4 lines to break a password in Haskell
image: /images/hacker.jpg
thumbnail: /images/thumbnail-hacker.jpg
---

I'm relatively new in functional programming but I really enjoy trying to code some piece of software in Haskell. It's a pure, strongly type and efficient language. I tried to develop [a little program](https://github.com/ThibaudDauce/habreaker) to break SHA1 hashes and the code in only 4 lines long.

<!--more-->

![A random hacker with the crucial black mask and really useful ski gloves to type on a keyboard :-)](/images/hacker.jpg)

## Generate the list of all the possible strings

I found [a Stack Overflow answer](http://stackoverflow.com/questions/9542313/how-to-generate-a-list-of-all-possible-strings-from-shortest-to-longest) where someone explains how to use the laziness of Haskell to build an infinite list of all the possible strings. The one line function is :
```haskell
allStrings :: [String]
allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]
```

`allStrings` is a list of strings defined as the concatenation of one character `c` in the list `['a'..'z'] ++ ['0'..'9']` and one string of `allStrings`. The definition is here recursive and we get an infinite list.

## Compute the SHA1 hash

First, I decided to use the `sha1` library but it wasn't really efficient. I decided to switch to the `cryptohash` library which is really faster.

|                       | `sha1` | `cryptohash` |
|-----------------------|--------|--------------|
| 4 characters password | 4,74s  | 0,56s        |

We need to transform the `String` in `ByteString` before computing the hash so I needed one line to code my SHA1 function:
```haskell
sha1 :: String -> Digest SHA1
sha1 = hash . B.pack
```

## Check the password hash

Now we need to compute the hash to check if it matches the hash provided by the user. Here again it's a really simple operation in Haskell:
```haskell
checkPassword :: Digest SHA1 -> String -> Bool
checkPassword hash string = (sha1 string) == hash
```

The function takes the hash we're looking for and a string, compute the SHA1 of the string and compare the two results.

## Find the password

Eventually, we need to find the correct password in the list of all possible strings.
```haskell
findPassword :: Digest SHA1 -> String
findPassword passwordHash = (head . filter (checkPassword passwordHash)) allStrings
```

First we build the filtering function `checkPassword passwordHash`, then we build a function that filter a list and take the head `head . filter (checkPassword passwordHash)` and finally, we call this function on our infinite list of strings.

Here we take advantage of the lazy evaluation of Haskell. The compiler is smart enough to stop the computation of the infinite list as soon as one element match the filter function because we're only looking for the head of the computed list.

## Haskell in parallel

I tried to parallelize this algorithm but I didn't find a way to do it with my infinite list. I need to dig further in the parallel strategies in Haskell. Maybe for a future blog post. I leave here my first attempt where the program never stop because Haskell can't figure out that we only need the head of the list and I ask for the evaluation of all the elements of the infinite list.
```haskell
parFilter :: (S.NFData a) => (a -> Bool) -> [a] -> [a]
parFilter p = S.withStrategy (S.evalBuffer 1000 S.rseq) . filter p
```
