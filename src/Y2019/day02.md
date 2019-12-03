---
title: "Advent of Optics: Day 2"
author: "Chris Penner"
date: "Dec 2, 2019"
tags: [haskell]
description: "Day two of Advent of Code solved with optics"
image: "pinecones.jpg"
---

Since I'm releasing [a book on practical lenses and optics](https://leanpub.com/optics-by-example) later this month I thought it would be fun to do a few of this year's Advent of Code puzzles using as many obscure optics features as possible!

To be clear, the goal is to be obscure, strange and excessive towards the goal of using as many optics as possible in a given solution, even if it's awkward, silly, or just plain overkill. These are NOT idiomatic Haskell solutions, nor are they intended to be. Maybe we'll both learn something along the way. Let's have some fun!

You can find today's puzzle [here](https://adventofcode.com/2019/day/2).

---

Every year of Advent of Code usually has some sort of assembly language simulator, looks like this year's came up early!

So we have a simple computer with registers which store integers, and an instruction counter which keeps track of our current execution location in the "program". There are two operations, addition and multiplication, indicated by a `1` or a `2` respectively. Each of these operations will also consume the two integers following the instruction as the addresses of its arguments, and a final integer representing the address to store the output. We then increment the instruction counter to the next instruction and continue. The program halts if ever there's a `99` in the operation address.

As usual, we'll need to start by reading in our input. Last time we could just use `words` to split the string on whitespace and everything worked out. This time there are commas in between each int; so we'll need a slightly different strategy. It's almost certainly overkill for this, but I've wanting to show it off anyways; so I'll pull in my [`lens-regex-pcre`](http://hackage.haskell.org/package/lens-regex-pcre) library for this. If you're following along at home, make sure you have at LEAST version `1.0.0.0`.

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text.IO as TIO

solve1 :: IO ()
solve1 = do
  input <- TIO.readFile "./src/Y2019/day02.txt" 
           <&> toMapOf ([regex|\d+|] . match . _Show @Int)
  print input

>>> solve1
["1","0","0","3","1","1","2"...]
```

Okay, so to break this down a bit I'm reading in the input file as `Text`, then using `<&>` (which is flipped (`<$>`)) to run the following transformation over the result. `<&>` is exported from `lens`, but is now included in `base` as part of `Data.Functor`, I enjoy using it over `<$>` from time to time, it reads more like a 'pipeline', passing things from left to right.

This pulls out all the integers as `Text` blocks, but we still need to parse them, I'll use the `unpacked` iso to convert from Text to String, then use the same `_Show` trick from yesterday's problem.


```haskell
solve1 :: IO ()
solve1 = do
    input <- TIO.readFile "./src/Y2019/day02.txt"
               <&> toListOf ([regex|\d+|] . match . unpacked . _Show @Int)
    print input
>>> solve1
[1,0,0,3,1,1,2,3...]
```

Okay, so we've loaded our register values, but from a glance at the problem we'll need to have random access to different register values, I won't worry about performance too much unless it becomes a problem, but using a list seems a bit silly, so I'll switch from `toListOf` into `toMapOf` to build a Map out of my results. `toMapOf` uses the index of your optic as the key by default, so I can just wrap my optic in `indexing` (which adds an increasing integer as an index to an optic) to get a sequential Int count as the keys for my map:

```haskell
solve1 :: IO ()
solve1 = do
    input <- TIO.readFile "./src/Y2019/day02.txt"
               <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
    print input

>>> solve1
fromList [(0,1),(1,0),(2,0),(3,3),(4,1)...]
```

Great, we've loaded our ints into "memory".

Next step, we're told at the bottom of the program to initialize the 1st and 2nd positions in memory to specific values, yours may differ, but it told me to set the 1st to `12` and the second to `2`. Easy enough to add that onto our pipeline!

```haskell
input <- TIO.readFile "./src/Y2019/day02.txt"
           <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
           <&> ix 1 .~ 12
           <&> ix 2 .~ 2
```

That'll 'pipeline' our input through and initialize the registers correctly.

Okay, now for the hard part, we need to actually RUN our program! Since we're emulating a stateful computer it only makes sense to use the `State` monad right? We've got a map to represent our registers, but we'll need an integer for our "read-head" too. Let's say our state is `(Int, Map Int Int)`, the first slot is the current read-address, the second is all our register values.

Let's write one iteration of our computation, then we'll figure out how to run it until the halt.

```haskell
oneStep :: State (Int, M.Map Int Int) ()
oneStep = do
    let loadRegister r = use (_2 . singular (ix r))
    let loadNext = _1 <<+= 1 >>= loadRegister
    let getArg = loadNext >>= loadRegister
    out <- getOp <$> loadNext <*> getArg <*> getArg
    outputReg <- loadNext
    _2 . ix outputReg .= out

getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+)
getOp 2 = (*)
getOp n = error $ "unknown op-code: " <> show n
```

Believe it or not, that's one step of our computation, let's break it down!

We define a few primitives we'll use at the beginning of the block. First is `loadRegister`. `loadRegister` takes a register 'address' and gets the value stored there. `use` is like `get` from `MonadState`, but allows us to get a specific piece of the state as focused by a lens. We use `ix` to get the value at a specific key out of the map (which is in the second slot of the tuple, hence the `_2`). However, `ix r` is a traversal, not a lens, we could either switch to `preuse` which returns a `Maybe`-wrapped result, or we can use `singular` to **force** the result and simply crash the whole program if its missing. Since we know our input is valid, I'll just go ahead and **force** it. Probably don't do this if you're building a REAL intcode computer :P

Next is `loadNext`, this fetches the current read-location from the first slot, then loads the value at that register. There's a bit of a trick here though, we load the read-location with `_1 <<+= 1`; this performs the `+= 1`  action to the location, which increments it by one (we've 'consumed' the current instruction), but the leading `<<` says to return the value there **before** altering it. This lets us cleanly get and increment the read-location all in one step. We then load the value in the current location using `loadRegister`.

We lastly combine these two combinators to build `getArg`, which gets the value at the current read-location, then loads the register at that address.

We can combine these all now! We `loadNext` to get the opcode, converting it to a Haskell function using `getOp`, then thread that computation through our two arguments getting an output value.

Now we can load the output register (which will be the next value at our read-location), and simply `_2 . ix outputReg .= result` to stash it in the right spot.

If you haven't seen these lensy `MonadState` helpers before, they're pretty cool. They basically let us write python-style code in Haskell!

Okay, now let's add this to our pipeline! If we weren't still inside the `IO` monad we could use `&~` to chain directly through the `MonadState` action!

```haskell
(&~) :: s -> State s a -> s 
```

Unfortunately there's  no `<&~>` combinator, so we'll have to move our pipeline out of `IO` for that. Not so tough to do though:

```haskell
solve1 :: IO ()
solve1 = do
    input <- TIO.readFile "./src/Y2019/day02.txt"
    let result = input
            & toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
            & ix 1 .~ 12
            & ix 2 .~ 2
            & (,) 0
            &~ do
                let loadRegister r = use (_2 . singular (ix r))
                let loadNext = _1 <<+= 1 >>= loadRegister
                let getArg = loadNext >>= loadRegister
                out <- getOp <$> loadNext <*> getArg <*> getArg
                outputReg <- loadNext
                _2 . ix outputReg .= out
    print result
```

This runs ONE iteration of our program, but we'll need to run the program until completion! The perfect combinator for this is `whileM`:

```haskell
untilM :: Monad m => m a -> m Bool -> m [a] 
```

This let's us write it something like this:

```haskell
&~ flip untilM ((==99) <$> (use _1 >>= loadRegister)) $ do ...
```

This would run our computation step repeatedly until it hits the `99` instruction. However, `untilM` is in the `monad-loops` library, and I don't feel like waiting for that to install, so instead we'll just use recursion.

Hrmm, using recursion here would require me to name my expression, so we could just use a `let` expression like this to explicitly recurse until we hit `99`:

```haskell
&~ let loop = do
              let loadRegister r = use (_2 . singular (ix r))
              let loadNext = _1 <<+= 1 >>= loadRegister
              let getArg = loadNext >>= loadRegister
              out <- getOp <$> loadNext <*> getArg <*> getArg
              outputReg <- loadNext
              _2 . ix outputReg .= out
              use _1 >>= loadRegister >>= \case
                99 -> return ()
                _ -> loop
   in loop
```

But the `let loop = ... in loop` construct is kind of annoying me, not huge fan.

Clearly the right move is to use anonymous recursion! (/sarcasm)

We can /simplify/ this by using `fix`!

```haskell
fix :: (a -> a) -> a
```

```haskell
&~ fix (\continue -> do
    let loadRegister r = use (_2 . singular (ix r))
    let loadNext = _1 <<+= 1 >>= loadRegister
    let getArg = loadNext >>= loadRegister
    out <- getOp <$> loadNext <*> getArg <*> getArg
    outputReg <- loadNext
    _2 . ix outputReg .= out
    use _1 >>= loadRegister >>= \case
      99 -> return ()
      _ -> continue
    )
```

Beautiful right? Well... some might disagree :P, but definitely fun and educational!

I'll leave you to study the arcane arts of `fix` on your own, but here's a teaser. Working with `fix` is similar to explicit recursion, you assume that you already **have** your result, then you can use it in your computation. In this case, we *assume* that `continue` is a state action which will loop until the program halts, so we do one step of the computation and then hand off control to `continue` which will magically **solve the rest**. It's basically identical to the `let ... in` version, but more obtuse and harder to read, so obviously we'll keep it!

If we slot this in it'll run the computation until it hits a `99`, and `&~` returns the resulting state, so all we need to do is view the first instruction location of our registers to get our answer!

```haskell
solve1 :: IO ()
solve1 = do
    input <- TIO.readFile "./src/Y2019/day02.txt"
    print $ input
            & toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
            & ix 1 .~ 12
            & ix 2 .~ 2
            & (,) 0
            &~ fix (\continue -> do
                let loadRegister r = use (_2 . singular (ix r))
                let loadNext = _1 <<+= 1 >>= loadRegister
                let getArg = loadNext >>= loadRegister
                out <- getOp <$> loadNext <*> getArg <*> getArg
                outputReg <- loadNext
                _2 . ix outputReg .= out
                use _1 >>= loadRegister >>= \case
                  99 -> return ()
                  _ -> continue
                )
            & view (_2 . singular (ix 0))

>>> solve1
<my answer>
```

Honestly, aside from the intentional obfuscation it turned out okay!

## Part 2

Just in case you haven't solved the first part on your own, the second part says we now need to find a specific **memory initialization** which **results** in a specific answer after running the computer. We need to find the exact values to put into slots 1 and 2 which result in this number, in my case: `19690720`.

Let's see what we can do! First I'll refactor the code from step 1 so it accepts some parameters

```haskell
solveSingle :: M.Map Int Int -> Int -> Int -> Int
solveSingle registers noun verb =
    registers
    & ix 1 .~ noun
    & ix 2 .~ verb
    & (,) 0
    &~ fix (\continue -> do
        let loadRegister r = use (_2 . singular (ix r))
        let loadNext = _1 <<+= 1 >>= loadRegister
        let getArg = loadNext >>= loadRegister
        out <- getOp <$> loadNext <*> getArg <*> getArg
        outputReg <- loadNext
        _2 . ix outputReg .= out
        use _1 >>= loadRegister >>= \case
          99 -> return ()
          _ -> continue
        )
    & view (_2 . singular (ix 0))
```

That was pretty painless. Now we need to construct some thingamabob which runs this with different 'noun' and 'verb' numbers (that's what the puzzle calls them) until it gets the answer we need. Unless we want to do some sort of crazy analysis of how this computer works at a theoretical level, we'll have to just brute force it. There're only 10,000 combinations, so it should be fine. We can collect all possibilities using a simple list comprehension:

```haskell
[(noun, verb) | noun <- [0..99], verb <- [0..99]]
```

We need to run the computer on each possible set of inputs, which amounts to simply calling `solveSingle` on them:

```haskell
solve2 :: IO ()
solve2 = do
    registers <- TIO.readFile "./src/Y2019/day02.txt"
               <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
    print $ [(noun, verb) | noun <- [0..99], verb <- [0..99]]
              ^.. traversed . to (uncurry (solveSingle registers))

>>> solve2
[29891,29892,29893,29894,29895,29896,29897,29898,29899,29900...]
```

This prints out the answers to every possible combination, but we need to **find** a **specific** combination! We can easily **find** the answer by using `filtered`, or `only` or even `findOf`, these are all valid:

```haskell
>>> [(noun, verb) | noun <- [0..99], verb <- [0..99]] 
      ^? traversed . to (uncurry (solveSingle registers)) . filtered (== 19690720)
Just 19690720

-- `only` is like `filtered` but searches for a specific value
>>> [(noun, verb) | noun <- [0..99], verb <- [0..99]] 
      ^? traversed . to (uncurry (solveSingle registers)) . only 19690720
Just 19690720

>>> findOf 
      (traversed . to (uncurry (solveSingle registers)) . only 19690720)
      [(noun, verb) | noun <- [0..99], verb <- [0..99]]
Just 19690720
```

These all work, but the tricky part is that we don't actually care about the answer, we already know that! What we need is the arguments we passed in to **get** that answer. There are many ways to do this, but my first thought is to just **stash** the arguments away where we can get them later. Indexes are great for this sort of thing (I cover tricks using indexed optics [in my book](https://leanpub.com/optics-by-example)). We can *stash* a value into the index using `selfIndex`, and it'll be carried alongside the rest of your computation for you! There's the handy `findIndexOf` combinator which will find the index of the first value which matches your predicate (in this case, the answer is equal to our required output).

Here's the magic incantation:

```haskell
findIndexOf (traversed . selfIndex . to (uncurry (solveSingle registers)))
            (== 19690720)
            [(noun, verb) | noun <- [0..99], verb <- [0..99]]
```

This gets us super-duper close, but the problem says we actually need to run the following transformation over our arguments to get the real answer: `(100 * noun) + verb`. We could easily do it *after* running `findIndexOf`, but just to be ridiculous, we'll do it inline! We're stashing our "answer" in the index, so that's where we need to run the transformation. We can use `reindexed` to run a transformation over the index of an optic, so if we alter `selfIndex` (which stashes the value into the index) then we can map the index through the transformation:

```haskell
reindexed (\(noun, verb) -> (100 * noun) + verb) selfIndex
```

That does it!

Altogether now, here's the entire solution for the second part:

```haskell
getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+)
getOp 2 = (*)
getOp n = error $ "unknown op-code: " <> show n

solveSingle :: M.Map Int Int -> Int -> Int -> Int
solveSingle registers noun verb =
    registers
    & ix 1 .~ noun
    & ix 2 .~ verb
    & (,) 0
    &~ fix (\continue -> do
        let loadRegister r = use (_2 . singular (ix r))
        let loadNext = _1 <<+= 1 >>= loadRegister
        let getArg = loadNext >>= loadRegister
        out <- getOp <$> loadNext <*> getArg <*> getArg
        outputReg <- loadNext
        _2 . ix outputReg .= out
        use _1 >>= loadRegister >>= \case
          99 -> return ()
          _ -> continue
        )
    & view (_2 . singular (ix 0))

solvePart2 :: IO ()
solvePart2 = do
    registers <- TIO.readFile "./src/Y2019/day02.txt"
               <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
    print $ findIndexOf  ( traversed
                         . reindexed (\(noun, verb) -> (100 * noun) + verb) selfIndex
                         . to (uncurry (solveSingle registers)))
            (== 19690720)
            [(noun, verb) | noun <- [0..99], verb <- [0..99]]
```

This was a surprisingly tricky problem for only day 2, but we've gotten through it okay! Today we learned about:

* `regex`: for precisely extracting text
* `toMapOf`: for building maps from an indexed fold
* `&~`: for running state monads as part of a pipeline
* `<&>`: for pipelining data within a context, 
* `<<+=`: for simultaneous modification AND access using lenses in MonadState
* `fix`: using fix for anonymous recursion (just for fun)
* `selfIndex`: for stashing values till later
* `reindexed`: for editing indices
* `findIndexOf`: for getting the index of a value matching a predicate

Hopefully at least one of those was new for you! Maybe tomorrows will be easier :)
