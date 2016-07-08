+++
Categories = ["haskel", "perf"]
Tags = ["perf",  "haskell"]
date = "2014-01-13"
title = "Get function execution time in Haskell"
+++

Some times ago I encountered with question: How to get function execution time in Haskell program? I asked this question at [StackOverflow](http://stackoverflow.com/questions/6766450/haskell-function-execution-time), and got some useful answers. Here i will try to describe how to do it. For example we have simple haskell program which will calculate sum of prime numbers which are between 0 and 10000. Something like this:

```haskell
module TimingTest where

main :: IO()
main = do
	putStrLn "Start"
	putStrLn ("Result: " ++ show primesSum)
	putStrLn "Done"


--
-- Returns True if `n` is prime
--
isPrime :: Int -> Bool
isPrime n = null [ x | x <- [2..n - 1], n `mod` x  == 0]

primesSum :: Int
primesSum = sum [x | x <- [2..10000], isPrime x == True]
```

Yes, it's not the best implementation of prime numbers, but it's not important at the current moment. Let's see what we have for checking execution time.

Time
------------------
First of all, the simplest method to get execution time is time command. Compile our source code and execute:

```
$ time ./TimingTest
```

We must get something like this:

```
real 0m3.503s
user 0m3.492s
sys 0m0.004s
```

GHCI
------------------

The second method is just add `:set +s` in `ghci` before the function execution. Of course it's not the best method, because functions run much slower in ``ghci`.

TimeIt
------------------

The third method is to use [TimeIt](http://hackage.haskell.org/package/timeit) haskell library by Lennart Augustsson. Very little, but useful library with simple API. It consist only from two functions:

```haskell
timeIt :: IO a -> IO a -- | Wrap an IO computation so that it prints out the execution time
```

and

```haskell
timeItT :: IO a -> IO (Double, a)Source -- | Wrap an IO computation so that it returns execution time is seconds as well as the real value.
```

Let's remake our main function as:

```haskell
import System.TimeIt

main :: IO()
main = do
 	putStrLn "Start"
	timeIt $ putStrLn ("Result: " ++ show primesSum)
	putStrLn "End"
```

and will get something like this:

```
Start
Result: 5736396
CPU time: 8.22s
```

Criterion
------------------

[Criterion](http://hackage.haskell.org/package/criterion) library provides a powerful but simple way to measure software performance by Bryan O'Sullivan. For using it, will remake again our main function as:

```haskell
module Main where

import Criterion.Main

main :: IO()
main = defaultMain [
       bgroup "Prime numbers." [bench "prime numbers benchmark" $ whnfIO (putStrLn $ show primesSum)]
       ]
```

and as a result we will get:

```
estimating clock resolution...
mean is 3.760062 us (160001 iterations)
found 3006 outliers among 159999 samples (1.9%)
2461 (1.5%) high severe

estimating cost of a clock call...
mean is 98.08812 ns (28 iterations)
found 4 outliers among 28 samples (14.3%)
4 (14.3%) low severe
```
