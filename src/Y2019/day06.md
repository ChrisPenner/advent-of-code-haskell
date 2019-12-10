---
title: "Advent of Code: Day 6"
author: "Chris Penner"
date: "Dec 6, 2019"
tags: [haskell]
description: "Day 6 of Advent of Code"
image: "pinecones.jpg"
---

Since I'm releasing [a book on practical lenses and optics](https://leanpub.com/optics-by-example) later this month I thought it would be fun to do a few of this year's Advent of Code puzzles using as many obscure optics features as possible!

To be clear, the goal is to be obscure, strange and excessive towards the goal of using as many optics as possible in a given solution, even if it's awkward, silly, or just plain overkill. These are NOT idiomatic Haskell solutions, nor are they intended to be. Maybe we'll both learn something along the way. Let's have some fun!

You can find today's puzzle [here](https://adventofcode.com/2019/day/6).

---

Today's problem is about planetary orbits! Psych! It's actually about graphs! Each planet orbits some other planet, and that planet orbits other planets, resolving to a tree of orbital dependencies. Our first task is to find the total number of direct and indirect orbits in our galaxy. 

There are a few solutions that pop readily to mind for me, namely Comonads, Recursion-schemes, and graphs! We'll start with the graph approach as it's by far the simplest, most idiomatic, and probably the fastest, but we'll look at the others for fun too!

Our input looks like this:

```haskell
R45)497
TYR)159
RJC)Z1B
ZQB)99Z
...

The planet on the right orbits the planet on the left.

First things first let's parse them in! I'll just use `lens-regex-pcre` again to grab the first and second parts of each definition using regex groups.

```haskell
graphSolve :: IO ()
graphSolve = do
    TIO.readFile "./src/Y2019/day06.txt"
          <&> toListOf (([regex|(\w+)\)(\w+)|] . groups)
          >>= print

>>> solve1
[["R45","497"],["TYR","159"],["RJC","Z1B"], ...]
```

We'll quickly solve it first using `Data.Graph` from `containers`. The graph implementation in `containers` is great when you're doing some analysis of an existing graph, so long as you don't need to edit it after it's been created. The primary way to create a graph is the `graphFromEdges` functions:

```haskell
graphFromEdges :: Ord key 
  => [(node, key, [key])] 
  -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

graphFromEdges' :: Ord key 
  => [(node, key, [key])] 
  -> (Graph, Vertex -> (node, key, [key]))
```

The "graph" is represented as a graph object, but we also get functions for looking up the information associated with particular nodes. We don't need a way to map from keys to Vertices in this case (so long as we plan things correctly) so we'll use the second variant. We really only care about the connections between nodes, so the actual node value is irrelevant, we'll just use the name of the planet. We'll also use the planet name for our keys. Here's what it looks like if we map our entries into edge descriptions where there's an edge from every planet to the planet it orbits. There'll be exactly one such edge for each node.

```haskell
graphSolve :: IO ()
graphSolve = do
    (g, _) <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (b, b, [a]))))
                <&> graphFromEdges'
```

We discard mapping function, we don't actually care about the node contents.

Next we need to know how many transitively linked orbits there are, the simplest way is to simply see how many nodes are `reachable` from each vertex and add them all up, so that's exactly what we'll do!  The reachable function from `Data.Graph` makes this pretty easy:

```haskell
reachable :: Graph -> Vertex -> [Vertex]
```

Here we go:

```haskell
graphSolve :: IO ()
graphSolve = do
    (g, _) <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] 
                              . groups 
                              . to (\[a, b] -> (b, b, [a])))
                              )
                <&> graphFromEdges'
    print . sum $ (length . reachable g <$> vertices g)
```

We get a list of vertices back, but we only care about the number of them, so we just take the sum of them all.

If we want to be masochistic about it we can inline pretty much the whole thing into a single call to `lengthOf`:

```haskell
graphSolve :: IO ()
graphSolve = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] 
                             . groups 
                             . to (\[a, b] -> (b, b, [a])))
                             )
                <&> graphFromEdges'
                <&> lengthOf ( _1  -- Select the graph
                             -- stash the graph in the index for later
                             . (selfIndex
                               -- fold over vertices, keeping graph stashed
                               <. folding vertices
                               )
                                -- Pull the graph from the index alongside each vertex
                               . withIndex 
                               -- fold over every vertex reachable from every vertex!
                               . folding (uncurry reachable))
                >>= print

>>> graphSolve
144909
```

Okay, that gets us the first answer. Let's do part 2 really quick, then backtrack to see how comonads and recursion-schemes can do the same thing!

In part two we need to find the length of the shortest **path** between the vertices for `YOU` and `SANTA`. Unfortunately the graph lib allows us to check IF there's a path, but not actually how long it is or which nodes are involved, we'll need a different solution.

```haskell
                          YOU
                         /
        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN
```

In the provided example we need to find the length of the path: 

```
YOU -> K -> J -> E -> D -> I -> SAN
```

One option is to get everything that `YOU` orbits, and everything that `SANTA` orbits, then if we find the nodes that are orbited by ONE but not BOTH of these then we'll find all the nodes that are part of this path!

Here's the quic'n'dirty of the solution:

```haskell
graphSolvePt2 :: IO ()
graphSolvePt2 = do
    (g, _, toVertex) <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (b, b, [a]))))
                <&> graphFromEdges
    let getReachable = S.fromList . reachable g . fromJust . toVertex
    let santa = getReachable "SAN"
    let you = getReachable "YOU"
    let diffs = (santa `S.union` you) S.\\ (santa `S.intersection` you)
    print $ (length diffs) - 2
```

We subtract 2 because the problem says not to include `YOU` and `SAN` in the count.

## Comonadic Trees

Let's




--- 

We have a few choices of how to start with our problem, at first this seems like a great problem to solve with the `Tree` comonad!

I know comonads aren't the most intuitive for most folks, so I'll break down the intuition a bit. Comonads are all about working with values in **contexts**. In the case of a Tree, the contexts are **all possible subtrees**, which in a problem like this means 

