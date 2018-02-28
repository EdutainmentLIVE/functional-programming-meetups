## Intro to Haskell

- What is Haskell and why would you choose to program in it?
  - Haskell is a statically type, lazy, purely functional programming language. Named after Haskell Brooks Curry, famous mathematician.
  - You'd program in it if you are looking for a language that minimizes runtime exceptions and if you are wired for mathematics. There are definitely other reasons, but we will get into those later.
- Well now that we know what Haskell is, what are we going to look at today?
  - Hello Haskell.
  - List in Haskell.
  - JavaScript vs. Haskell.
- Hello Haskell, Huh?
  - Docker (Not in depth)
  - Haskell's Stack
  - Main Executable
  - HelloHaskell Module. putStrLn is a function that takes a string and performs IO ().
- Okay, so we print a string, cool, how would we deal with a lists since there are no loops in Haskell?
  - ListHandler module.
      The `do` here at the beginning gives us a little imperative vibes, but it's useful when needing to chain functions that return the same type together.
      - Print
      - Map
      - Foldl'
  - Lists are monads
- Cool now that we've gotten that under our belt, we can start tinkering with Haskell. We now understand the basic structure of a Haskell app and how Haskell handles inputs/outputs. We learned how lists can be iterated over since we don't have language defined loops in Haskell. We mentioned monads, which can trip up a lot of people learning Haskell, but fear not.
- Questions?
