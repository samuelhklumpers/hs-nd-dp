This library defines a class of non-deterministic dynamic programming problems and the means to solve/optimize them, along with some tools to compute useful statistics and objective values. An example of applying this to a variant of blackjack can be found in src/Blackjack.hs

This library:
- provides modeling for non-deterministic dynamic programming
    - this allows the "reward" of an action to depend on multiple successor states
- abstracts over the search procedure used to determine the optimal action, allowing for:
    - exhaustive searches over finite domains
    - a golden section search for unimodal objectives over "infinite" domains
    - something smarter if you want?
- solves the problem while returning the entire table of subsolutions

It does not:
- use algebraic infrastructure to explain dynamic programming is actually described by a hylomorphism
- use MemoTrie or MonadMemo
- use Semirings to describe formulate optimality and the reported statistics at the type level