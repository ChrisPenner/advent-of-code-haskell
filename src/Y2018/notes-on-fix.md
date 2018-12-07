https://functionalprogramming.slack.com/archives/C04641JCU/p1543999660431300?thread_ts=1543995227.429800&cid=C04641JCU

cdepillabout [1 day ago]
@zenspider @chrispenner So `fix` basically just gives you a way to create an anonymous lambda function that you can use recursively.  The secret of fix is letting the `a` be a function.

So, for instance, let's say you wanted a function that will repeated take the tail of a `String`, and recurse only when the tail of `String` is different from the current `String`.  (So effectively it will rip the head off a `String` until the `String` becomes empty, at which point it will return.)

You could write it something like this:

```> :t fix :: ((String -> String) -> String -> String) -> String -> String
> let safeTail s = if null s then [] else tail s
> flip fix "hello" $ \f s -> if s == safeTail s then s else f (safeTail s)
""
```

A couple things to note about this:

1. You can see how I let the `a` in the type signature of `fix` be a function type `String -> String`.
2. I have to define a safe wrapper around `tail`.  This is somewhat annoying, but ehh.  That's what you get for using the standard prelude.
3. I am flipping `fix` here, but it might be easier to understand if you don't do that.
4. In this instance the function you're passing to `fix` get's two arguments.  The `f` the a reference to this lambda function itself.  You call this when you want to recurse.
5. The `s` argument to `fix` is the `String` you're operating on.
