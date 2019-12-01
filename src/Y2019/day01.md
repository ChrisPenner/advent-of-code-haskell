---
title: "Advent of Optics: Day 1"
author: "Chris Penner"
date: "Dec 1, 2019"
tags: [haskell]
description: "Day one of Advent of Code solved with optics"
---

Since I'm releasing [a book on practical lenses and optics](https://leanpub.com/optics-by-example) later this month I thought it would be fun to do a few of this year's Advent of Code puzzles using optics as much as possible!

I'm not sure how many I'll do, or even if any problems will yield interesting solutions with optics, but there's no harm trying! The goal is to use optics for as much of the solution as possible, even if it's awkward, silly, or just plain overkill. Maybe we'll both learn something along the way. Let's have some fun!

You can find the first puzzle [here](https://adventofcode.com/2019/day/1).

---

So the gist of this one is that we have a series of input numbers (mass of ship modules) which each need to pass through a pipeline of mathematic operations (fuel calculations) before being summed together to get our puzzle solution (total fuel required).

This immediately makes me think of a **reducing** operation, we want to **fold** many inputs down into a single solution. We also need to **map** each input through the pipeline of transformations before adding them. Were I to use "normal" Haskell I could just `foldMap` to do both the **fold** and **map** at once! With optics however, the ideas of **folds** *already* encompass both the folding and mapping pieces. The optic we use provides the selection of elements as well as the mapping, and the action we run on it provides the reductions step (the fold).

Let's see if we can build up a fold in pieces to do what we need.

Assuming we have a `String` representing our problem input we need to break it into tokens to get each number from the file. Writing a parser is overkill for such a simple task; we can just use the `worded` fold which splits a String on whitespace and folds over each word individually!

Here's what we've got so far:

```haskell
solve :: IO ()
solve =  do
  input <- readFile "./src/Y2019/day01.txt"
  print $ input ^.. worded
```

Running this yields something like this:

```haskell
>>> solve
["76542","97993","79222"...] -- You get the idea
```

Now we need to "parse" the strings into actual numeric types. There's a handy prism in `lens` which will use `Read` instances to parse strings, simply skipping elements which have a bad parse. Our input is valid, so we don't need to worry about errors, that means we can use it confidently. I'll add a type-application to tell it what the output type should be so it knows what to parse:

```haskell
solve :: IO ()
solve =  do
  input <- readFile "./src/Y2019/day01.txt"
  print $ input ^.. worded
                  . _Show @Double

>>> solve
[76542.0,97993.0,79222.0...]
```

Looks like that's working!

Next we need to pipe it through several numeric operations. I like to read my optics code like a sequential pipeline, so I'll use `to` to string each transformation together. If you prefer you can simply compose all the arithmetic into a single function and use only one `to` instead, but here's how I like to do it.

The steps are:

1. Divide by 3
2. Round down
3. Subtract 2

No problem:

```haskell
solve :: IO ()
solve =  do
  input <- readFile "./src/Y2019/day01.txt"
  print $ input ^.. worded
                  . _Show
                  . to (/ 3)
                  . to (floor @Double @Int)
                  . to (subtract 2)

>>> solve
[25512,32662,26405...]
```

I moved the type application to `floor` so it knows what its converting between, but other than that it's pretty straight forward.

Almost done! Lastly we need to sum all these adapted numbers together. We can simply change our aggregation action from `^..` a.k.a. `toListOf` into `sumOf` and we'll now collect results by summing!


```haskell
solve :: IO ()
solve =  do
  input <- readFile "./src/Y2019/day01.txt"
  print $ input & sumOf ( worded
                        . _Show
                        . to (/ 3)
                        . to (floor @Double @Int)
                        . to (subtract 2)
                        )

>>> solve
3154112
```

First part's all done! That's the correct answer.

As a fun side-note, we could have computed the ENTIRE thing in a fold by using `lens-action` to thread the `readFile` into IO as well. Here's that version:


```haskell
solve' :: IO (Sum Int)
solve' =  "./src/Y2019/day01.txt"
          ^! act readFile
          . worded
          . _Show
          . to (/3)
          . to floor @Double @Int
          . to (subtract 2)
          . to Sum

>>> solve'
Sum {getSum = 3154112}
```

The `^!` is an action from `lens-action` which lets us 'view' a result from a Fold which requires IO. `act` allows us to lift a monadic action into a fold. By `viewing` we implicitly fold down the output using it's Monoid (in this case `Sum`).

I think the first version is cleaner though.

On to part 2!

## Part 2

Okay, so the gist of part two is that we need to ALSO account for the fuel required to transport all the fuel we add! Rather than using calculus for this we're told to fudge the numbers and simply iterate on our calculations until we hit a negative fuel value.

So to adapt our code for this twist we should split it up a bit! First we've got a few optics for **parsing** the input, those are boring and don't need any iteration. Next we've got the pipeline part, we need to run this on **each input number**, but will also need to run it on **each iteration** of each input number. We'll need to somehow loop our input through this pipeline.

As it turns out, an iteration like we need to do here is technically an **unfold** (or **catamorphism** if you're eccentric). In optics-land unfolds can be represented as a normal `Fold` which **adds** more elements when it runs. Lensy folds can focus an arbitrary (possibly infinite) number of focuses! Even better, there's already a fold in `lens` which does basically what we need!

```haskell
iterated :: (a -> a) -> Fold a a
```

`iterated` takes an iteration function and, well, iterates! Let's try it out on it's own first to see how it does its thing:

```haskell
>>> 1 ^.. taking 10 (iterated (+1))
[1,2,3,4,5,6,7,8,9,10]
>>>
```

Notice that I have to limit it with `taking 10` or it'd go on forever. So it definitely does what we expect! Notice also that it also emits its first input without any iteration; so we see the `1` appear unaffected in the output. This tripped me up at first.

Okay, so we've got all our pieces, let's try patching them together!

```haskell
solve2 :: IO ()
solve2 =  do
  input <- readFile "./src/Y2019/day01.txt"
  print
    $ input
    & toListOf ( worded
               . _Show
               . taking 20 (iterated calculateRequiredFuel)
               )
  where
    calculateRequiredFuel :: Double -> Double
    calculateRequiredFuel = (fromIntegral . subtract 2 . floor @Double @Int . (/ 3))

>>> solve2
[76542.0,25512.0,8502.0,2832.0,942.0,312.0,102.0,32.0,8.0,0.0,-2.0,-3.0,-3.0
...79222.0,26405.0,8799.0,2931.0...]
```

I've limited our iteration again here while we're still figuring things out, I also switched back to `toListOf` so we can see what's happening clearly. I also moved the fuel calculations into a single pure function, and added a `fromIntegral` so we can go from `Double -> Double` as is required by `iterated`.

In the output we can see the fuel numbers getting smaller on each iteration, until they eventually go negative (just like the puzzle predicted). Eventually we finish our 20 iterations and the fold moves onto the next input so we can see the numbers jump back up again as a new iteration starts.

The puzzle states we can ignore everything past the point where numbers go negative, so we can stop iterating at that point. That's pretty easy to do using the higher-order optic `takingWhile`; it accepts a predicate and **another optic** and will consume elements from the other optic until the predicate fails, at which point it will yield no more elements. In our case we can use it to consume from each iteration until it hits a negative number, then move on to the next iteration.


```haskell
solve2 :: IO ()
solve2 =  do
  input <- readFile "./src/Y2019/day01.txt"
  print
    $ input
    & toListOf ( worded
               . _Show
               . takingWhile (>0) (iterated calculateRequiredFuel)
               )

>>> solve2
[76542.0,25512.0,8502.0,2832.0,942.0,312.0,102.0,32.0,8.0
,97993.0,32662.0,10885.0,3626.0,1206.0,400.0,131.0,41.0,11.0...]
```

We don't need the `taking 20` limiter anymore since now we stop when we hit `0` or below. In this case we technically filter out an actual `0`; but since `0` has no effect on a `sum` it's totally fine.

Okay, we're really close! On my first try I summed up all these numbers and got the wrong answer! As I drew attention to earlier, when we use `iterated` it passes through the original value as well! We don't want the weight of our module in our final sum, so we need to remove the **first** element from each set of iterations. I'll use ANOTHER higher-order optic to wrap our iteration code, dropping the first output from each iteration:

```haskell
solve2 :: IO ()
solve2 =  do
  input <- readFile "./src/Y2019/day01.txt"
  print
    $ input
    & sumOf ( worded
            . _Show
            . takingWhile (>0) (dropping 1 (iterated calculateRequiredFuel))
            )

>>> solve2
4728317.0
```

Great! That's the right answer!

It depends on how you like to read your optics, but I think the multiple nested higher-order-optics is a bit messy, we can re-arrange it to use fewer brackets like this; but it really depends on which you find more readable:

```haskell
solve2 :: IO ()
solve2 =  do
  input <- readFile "./src/Y2019/day01.txt"
  print
    $ input
    & sumOf (worded
             . _Show
             . (takingWhile (> 0) . dropping 1 . iterated) calculateRequiredFuel
            )
```

That'll do it!

Once you get comfortable with how folds nest inside paths of optics, and how to use higher-order folds (spoilers: there's a whole chapter on this in my book launching later this month): [Optics By Example](https://leanpub.com/optics-by-example/)), then we can solve this problem very naturally with optics! I hope some of the other problems work out just as well.

See you again soon!
