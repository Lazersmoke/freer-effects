# Freer Effects: Extensible Effects with Freer Monads

[![Hackage](http://img.shields.io/hackage/v/freer-effects.svg)](https://hackage.haskell.org/package/freer-effects)
[![Stackage LTS](http://stackage.org/package/freer-effects/badge/lts)](http://stackage.org/nightly/package/freer-effects)
[![Stackage Nightly](http://stackage.org/package/freer-effects/badge/nightly)](http://stackage.org/nightly/package/freer-effects)
[![Build Status](https://travis-ci.org/Lazersmoke/freer-effects.svg?branch=master)](https://travis-ci.org/Lazersmoke/freer-effects)

# Description

Provides utilities for working with "freer" monads, and more extensible effects.
This allows you to do things similar to traditional monad transformers, but with more flexibility.
You can write custom effects and interpreters easily, without needing boilerplate (no `O(n^2)` instances!) or performance issues.

# Features

The key features of Freer are:

* An efficient effect system for Haskell as a library, without the poor performance of other implementations.
* Implementations for several common Haskell monads as effects:
    * `Reader`
    * `Writer`
    * `State`
    * `Exception`
* Core components for defining your own effects and interpreters for those effects.

# Acknowledgements

History of this package, in reverse chronological order:

- This project, which continues maintenance of, and improves upon the Ixperta fork.
- [IxpertaSolutions](https://www.ixperta.com/en/) maintained a [fork called `freer-effects` on GitHub](https://github.com/IxpertaSolutions/freer-effects) and [hackage](https://hackage.haskell.org/package/freer-effects) of the previous library, but then went inactive.
- [Allele Dev](https://queertypes.com/) repackaged the original research material into [`freer` on GitLab](https://gitlab.com/queertypes/freer) and [hackage](https://hackage.haskell.org/package/freer), but then went inactive.
- [Oleg Kiselyov](http://okmij.org/ftp/) did most of the OG research for this technology, notably coauthoring [Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf) with Hiromi Ishii. 

Special thanks to [Andras Kovacs](https://github.com/AndrasKovacs) whom I've shamelessly stolen the [GHC 8 TypeInType-powered effect inference engine](https://github.com/AndrasKovacs/misc-stuff/blob/master/haskell/Eff/EffInference.hs) from.

This package would not be here without these forebearers.

[Here](http://okmij.org/ftp/Haskell/extensible) is a link to Oleg Kiselyov's page on extensible effects, including reference implementations and the aforementioned paper.
Paricularly relevant:

* [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf)
* [Extensible Effects](http://okmij.org/ftp/Haskell/extensible/exteff.pdf)

# Examples

See the [examples](https://github.com/Lazersmoke/freer-effects/examples).

# Combining with Transformers

You already have some [`mtl`](http://hackage.haskell.org/package/mtl) code and are afraid that combining effects with your current tranformer stack would not be possible? 
Package [`freer-effects-extra`](https://github.com/trskop/freer-effects-extra) has some `mtl`-related and other goodies, but almost certainly needs to be updated (or forked) to work with this fork.
In the mean time, you can always use a normal `mtl` stack as a single effect in your code, just like you might use `IO` normally.

# Contributing

Please send pull requests and open issues if you want to see something changed!
This package should be considered unstable, so API-changing PRs are fine if they are independently useful.

## Developer Setup

Just use [`stack`](https://www.haskellstack.org/).

# Licensing

This project is distrubted under a BSD3 license. 
See the included LICENSE file for more details.
