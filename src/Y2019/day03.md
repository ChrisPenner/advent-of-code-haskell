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


We've got two sets of instructions, each representing paths of wires, and we need to find out **where** in the space they cross, then determine the distances of those points from the origin.

We'll start as always with parsing in the input! They made it a bit harder on us this time, but it's certainly nothing that `lens-regex-pcre` can't handle:

```haskell
```

There are a few approaches to solving this one, my philosophy is always to just do "the dumb thing" until it either works or doesn't!


