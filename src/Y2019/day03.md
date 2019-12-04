---
title: "Advent of Optics: Day 3"
author: "Chris Penner"
date: "Dec 3, 2019"
tags: [haskell]
description: "Day 3 of Advent of Code solved with optics"
image: "pinecones.jpg"
---

Since I'm releasing [a book on practical lenses and optics](https://leanpub.com/optics-by-example) later this month I thought it would be fun to do a few of this year's Advent of Code puzzles using as many obscure optics features as possible!

To be clear, the goal is to be obscure, strange and excessive towards the goal of using as many optics as possible in a given solution, even if it's awkward, silly, or just plain overkill. These are NOT idiomatic Haskell solutions, nor are they intended to be. Maybe we'll both learn something along the way. Let's have some fun!

You can find today's puzzle [here](https://adventofcode.com/2019/day/3).

---

Today's didn't really have any phenomenal optics insights, but I did learn about some handy types and instances for handling points in space, so we'll run through it anyways and see if we can have some fun! You know the drill by now so I'll jump right in.

Sorry, this one's a bit rushed and messy, turns out writing a blog post every day is pretty time consuming.

We've got two sets of instructions, each representing paths of wires, and we need to find out **where** in the space they cross, then determine the distances of those points from the origin.

We'll start as always with parsing in the input! They made it a bit harder on us this time, but it's certainly nothing that `lens-regex-pcre` can't handle. Before we try parsing out the individual instructions we need to split our instruction sets into one for each wire! I'll just use the `lines` function to split the file in two:

```haskell
main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
```

I'm using that handy `<&>` pipelining operator, which basically allows me to pass the contents of a monadic action through a bunch of operations. It just so happens that `>>=` has the right precedence to tack it on the end!

Now we've got a list of two `Text`s, with a path in each!

Keeping a list of the two elements is fine of course, but since this is a post about optics and obfuscation, we'll pack them into a tuple just for fun: 

```haskell
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               >>= print

>>> main
("R999,U626,R854,D200,R696,...", "D424,L846,U429,L632,U122,...")
>>> 
```

This is a fun (and useless) trick! If you look closely, we're actually applying `traverseOf` to all of its arguments! What we're doing is applying `view` to each traversal (i.e. `ix 0`), which creates a **function** over our list of wire texts. `traverseOf` then sequences the **function** as the effect and returns a new function: `[Text] -> (Text, Text)` which is pretty cool! When we pass in the list of wires this is applied and we get the tuple we want to pass forwards. We're using `view` on a traversal here, but it's all good because `Text` is a Monoid. This of course means that if the input doesn't have at least two lines of input that we'll continue on silently without any errors... but there aren't a lot of adrenaline pumping thrills in software development so I guess I'll take them where I can get them. We'll just trust that the input is good. We could use `singular` or even `preview` to be *safer* if we wanted, but ain't nobody got time for that in a post about crazy hacks!

Okay! Next step is to figure out the crazy path that these wires are taking. To do that we'll need to parse the paths into some sort of pseudo-useful form. I'm going to reach for `lens-regex-pcre` again, at least to find each instruction. We want to run this over **both** sides of our tuple though, so we'll add a quick incantation for that as well

```haskell
import Linear

main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> both %~
                     (   toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput)

parseInput :: (Char, String) -> (Int, V2 Int)
parseInput (d, n) = (,) (read n) $ case d of
    'U' -> V2 0 (-1)
    'D' -> V2 0 1
    'L' -> V2 (-1) 0
    'R' -> V2 1 0

>>> main
([(999,V2 1 0),(626,V2 0 (-1)),...], [(854,V2 1 0),(200,V2 0 1),...]
```

Okay, there's a lot happening here, first I use the simple regex `\w\d+` to find each "instruction", then grab the full match as `Text`.

Next in line I `unpack` it into a `String` since I'll need to use `Read` to parse the `Int`s.

After that I use the `_Cons` prism to split the string into its first char and the rest, which happens to get us the direction and the distance to travel respectively.

Then I run `parseInput` which converts the String into an Int with `read`, and converts the cardinal direction into a vector equivalent of that direction. This is going to come in handy soon I promise. I'm using `V2` from the `linear` package for my vectors here.


Okay, so now we've parsed a list of instructions, but we need some way to determine where the wires intersect! The simplest possible way to do that is just to enumerate every single point that each wire passes through and see which ones they have in common; simple is good enough for me!

Okay here's the clever bit, the way we've organized our directions is going to come in handy, I'm going to create `n` copies of each vector in our stream so we effectively have a single instruction for each movement we'll make!

```haskell
(toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput . folding (uncurry replicate))
```

`uncurry` will make `replicate` into the function: `replicate :: (Int, V2 Int) -> [V2 Int]`, and `folding` will run that function, then flatten out the list into the focus of the fold. Ultimately this gives us just a huge list of unit vectors like this:

```haskell
[V2 0 1, V2 1 0, V2 (-1) 0...]
```

This is great, but we also need to keep track of which actual positions this will cause us to walk, we need to **accumulate** our position across the whole list. Let's use a `scan`:

```haskell
import Control.Category ((>>>))

main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> both %~
                     (   toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput . folding (uncurry replicate))
                     >>> scanl1 (+)
                     >>> S.fromList
                     )
                     >>= print

-- Trying to print this Set crashed my computer, 
-- but here's what it looked like on the way down:
>>> main
(S.fromList [V2 2003 1486,V2 2003 1487,...], S.fromList [V2 1961 86,V2 (-433), 8873,...])
```

Normally I really don't like `>>>`, but it allows us to keep writing code top-to-bottom here, so I'll allow it just this once.

The scan uses the `Num` instance of `V2` which adds the `x` and `y` components separately. This causes us to move in the right direction after every step, and keeps track of where we've been along the way! I dump the data into a set with `S.fromList` because next we're going to `intersect`!

```haskell
main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> both %~
                     (   toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput . folding (uncurry replicate))
                     >>> scanl1 (+)
                     >>> S.fromList
                     )
               <&> foldl1Of each S.intersection
                     >>= print

-- This prints a significantly shorter list and doesn't crash my computer
>>> main
fromList [V2 (-2794) (-390),V2 (-2794) 42,...]
```

Okay we've jumped back out of our `both` block, now we need to intersect the sets in our tuple! A normal person would use `uncurry S.intersection`, but since this is an optics post we'll of course use the excessive version `foldl1Of each S.intersection` which folds over **each** set using intersection! A bonus is that this version won't need to change if we eventually switch to many wires stored in a tuple or list, it'll *just work*™.

Almost done! Now we need to find which intersection is **closest** to the origin. In our case the origin is just `(0, 0)`, so we can get the distance by simply summing the absolute value of the aspects of the Vector (which is acting as a Point).

```haskell
main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> both %~
                     (   toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput . folding (uncurry replicate))
                     >>> scanl1 (+)
                     >>> S.fromList
                     )
               <&> foldl1Of each S.intersection
               <&> minimumOf (folded . to (sum . abs))
                     >>= print

>>> main
Just 399
```

And that's my answer! Wonderful!

## Part 2

Part 2 is a pretty reasonable twist, now we need to pick the intersection which is the fewest number of steps **along the wire** from the origin. We sum together the steps along each wire and optimize for the smallest total.

Almost all of our code stays the same, but a Set isn't going to cut it anymore, we need to know which **step** we were on when we reached each location! `Map`s are kinda like sets with extra info, so we'll switch to that instead. 
Instead of using `S.fromList` we'll use `toMapOf`! We need the index of each element in the list (which corresponds to it's distance from the origin along the wire). a simple `zip [0..]` would do it, but we'll use the much more obtuse version:

```haskell
toMapOf (reindexed (+1) traversed . withIndex . swapped . ito id)
```

Fun right? `traversed` has a numerically increasing index by default, `reindexed (+1)` makes it start at `1` instead (since the first step still counts!). 
Make sure you don't forget this or you'll be confused for a few minutes before realizing your answer is off by 2...

`toMapOf` uses the index as the key, but in our case we actually need the vector as the key! Again, easiest would be to just use a proper `M.fromList`, but we won't give up so easily. We need to swap our index and our value within our lens path!
We can pull the index down from it's hiding place into value-land using `withIndex` which adds the index to your value as a tuple, in our case: `(Int, V2 Int)`, then we swap places using the `swapped` iso, and reflect the `V2 Int` into the index using `ito`:

```haskell
ito :: (s -> (i, a)) -> IndexedGetter i s a
```

Now `toMapOf` properly builds a `Map (V2 Int) Int`!

Let's finish off part 2:

```haskell
main2 :: IO ()
main2 = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> each %~
                     (   toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput . folding (uncurry replicate))
                     >>> scanl1 (+)
                     >>> toMapOf (reindexed (+1) traversed . withIndex . swapped . ito id)
                     )
               <&> foldl1Of each (M.intersectionWith (+))
               <&> minimum
               >>= print
```

We use `M.intersectionWith (+)` now so we add the distances when we hit an intersection, so our resulting Map has the sum of the two wires' distances at each intersection.

Now we just get the minimum distance and print it! All done!

This one wasn't so "opticsy", but hopefully tomorrow's puzzle will fit a bit better! Cheers!
