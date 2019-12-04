---
title: "Advent of Optics: Day 4"
author: "Chris Penner"
date: "Dec 4, 2019"
tags: [haskell]
description: "Day 4 of Advent of Code solved with optics"
image: "pinecones.jpg"
---

Since I'm releasing [a book on practical lenses and optics](https://leanpub.com/optics-by-example) later this month I thought it would be fun to do a few of this year's Advent of Code puzzles using as many obscure optics features as possible!

To be clear, the goal is to be obscure, strange and excessive towards the goal of using as many optics as possible in a given solution, even if it's awkward, silly, or just plain overkill. These are NOT idiomatic Haskell solutions, nor are they intended to be. Maybe we'll both learn something along the way. Let's have some fun!

You can find today's puzzle [here](https://adventofcode.com/2019/day/4).

---

Hey folks! Today's is a nice clean one! The goal is to find all the numbers within a given range which pass a series of predicates! The conditions each number has to match include:

* Should be within the range; my range is `307237-769058`
* Should be six digits long; my range includes only 6 digit numbers, so we're all set here
* Two adjacent digits in the number should be the same (e.g. '223456')
* The digits should be in monotonically increasing order (e.g. increase or stay the same from left to right)

And that's it!

In normal Haskell we'd make a list of all possibilities, then either chain a series of `filter` statements or use `do-notation` with guards to narrow it down. Luckily, folds have filters too!

First things first, since our checks have us analyzing the actual discrete digits we'll convert our Int to a String so we can talk about them as characters:

```haskell
main = ([307237..769058] :: [Int])
        & toListOf (traversed . re _Show)
        & print

>>> main
["307237","307238","307239","307240","307241","307242","307243","307244","307245",
"307246", ...]
```

`_Show` is the same prism we've used for parsing in the previous examples, but `re` flips it in reverse and generates a `Getter` which calls `show`! This is equivalent to `to show`, but will get you an extra 20 optics points...

Now let's start adding filters! We'll start by checking that the digits are all ascending. I could write some convoluted fold which does this, but the quick and dirty way is simply to sort the digits lexicographically and see if the ordering changed at all:

```haskell
main :: IO ()
main = ([307237..769058] :: [Int])
        & toListOf (traversed . re _Show
                    . filtered (\s -> s == sort s)
                   )
        & print

>>> main
["333333","333334","333335","333336","333337","333338",...]
```

`filtered` removes any focuses from the fold which don't match the predicate.

We can already see this filters out a ton of possibilities. Not done yet though; we need to ensure there's at least one double consecutive digit. I'll reach for my favourite hammer: `lens-regex-pcre`:

```haskell
main :: IO ()
main = ([307237..769058] :: [Int])
        & toListOf (traversed . re _Show
                    . filtered (\s -> s == sort s)
                    . filteredBy (packed . [regex|(\d)\1+|])
                   )


>>> main 
["333333","333334","333335","333336","333337","333338",...]
```

Unfortunately we don't really see much difference in the first few options, but trust me, it did something. Let's see how it works:

I'm using `filteredBy` here instead of `filtered`, `filteredBy` is brand new in `lens >= 4.18`, so make sure you've got the latest version if you want to try this out. It's like `filtered`, but takes a Fold instead of a predicate. `filteredBy` will run the fold on the current element, and will filter out any focuses for which the fold yields no results.

The fold I'm passing in converts the String to a `Text` using `packed`, then runs a regex which matches any digit, then requires at least one more of that digit to be next in the string.  Since `regex` only yields matches, if no matches are found the candidate will be filtered out.

That's all the criteria! Now we've got a list of all of them, but all we really need is the count of them, so we'll switch from `toListOf` to `lengthOf`:

```haskell
main :: IO ()
main = ([307237..769058] :: [Int])
        & lengthOf ( traversed . re _Show
                   . filtered (\s -> s == sort s)
                   . filteredBy (packed . [regex|(\d)\1+|])
                   )
        & print

>>> main
889
```

That's the right answer,  not bad!

## Part 2

Part 2 only adds one more condition:

* The number must have a group of exactly 2 consecutive numbers, e.g. `333` is no good, but `33322` is fine.

Currently we're just checking that it has at least two consecutive numbers, but we'll need to be smarter to check for groups of exactly 2. Luckily, it's not too tricky.

The `regex` traversal finds ALL non-overlapping matches within a given piece of text, and the `+` modifier is greedy, so we know that for a given string `33322` our current pattern will find the matches: `["333", "22"]`. After that it's easy enough to just check that we have at least one match of length 2!


```haskell
main :: IO ()
main = ([307237..769058] :: [Int])
        & lengthOf (traversed . re _Show
                   . filtered (\s -> s == sort s)
                   . filteredBy (packed . [regex|(\d)\1+|] . match . to T.length . only 2)
                   )
        & print

>>> main
589
```

I just get the match text, get its length, then use `only 2` to filter down to only lengths of 2. `filteredBy` will detect whether any of the matches make it through the whole fold and kick out any numbers that don't have a group of exactly 2 consecutive numbers.

That's it for today! Hopefully tomorrow's is just as optical! 🤞
