<h1 align="center">
  <img src="scheme.wasm.ui/favicon/scheme.wasm.logo.svg?raw=true" alt="scheme.wasm logo"/>
</h1>

[![Node.js CI](https://github.com/pollrobots/scheme/actions/workflows/node.js.yml/badge.svg)](https://github.com/PollRobots/scheme/actions/workflows/node.js.yml)
[![CodeQL](https://github.com/pollrobots/scheme/actions/workflows/codeql-analysis.yml/badge.svg)](https://github.com/PollRobots/scheme/actions/workflows/codeql-analysis.yml)
[![FOSSA Status](https://app.fossa.com/api/projects/custom%2B29853%2Fgithub.com%2FPollRobots%2Fscheme.svg?type=shield)](https://app.fossa.com/projects/custom%2B29853%2Fgithub.com%2FPollRobots%2Fscheme?ref=badge_shield)

[![GitHub stars](https://img.shields.io/github/stars/pollrobots/scheme.svg?style=social&label=Star)](https://github.com/pollrobots/scheme/stargazers)

# scheme.wasm

An R7RS Scheme implemented in WebAssembly

A partial implementation of [r7rs scheme](https://small.r7rs.org/), written
entirely in WebAssembly using the WebAssembly Text format. The only external
imports are for IO (`read`, `write`, and `readFile`), unicode (I have an
import that reads information about 256 code-point blocks to enable case
operations etc.), and process control (`exit`).

You can try it out at [pollrobots.com/scheme/](https://pollrobots.com/scheme/)

## How Complete Is It?

The aim is to write a spec complete version of `r7rs`, although I may skip
some of the optional features.

### What is done so far

- [x] Numerics
  - [x] Integers (arbitrary precision)
  - [x] Real numbers (double precision)
  - [x] Rationals
  - [x] Complex Numbers
- [x] Booleans
- [x] Strings
- [x] Characters
- [x] Pairs and Lists
- [x] Vectors
- [x] Bytevectors
- [x] Values
- [x] Records
- [x] Tail call optimization &mdash; internally `eval` uses a continuation passing
      style, so TCO comes for free.
- [x] `call/cc` and exceptions
- [x] Macros
  - [x] `define-syntax`, `syntax-rules`, `syntax-error`
  - [x] Hygienic over `let`, `let*`, `letrec`, `letrec*`, and `lambda`
  - [ ] `let-syntax`, `letrec-syntax`
- [ ] Modules
- [ ] Ports
- [ ] `dynamic-wind`
- [ ] Everything else

## Credits

Where practical everything has been implemented from scratch, but there
are places where it either wasn't practical, or where I tried and failed
to implement them myself, so credit is due to:

- **xxHash**:
  It's probably overkill, but the hashing algorithm used for hashtables,
  which are in turn used for environments and interning symbols, is xxHash
  translated from the C++ implementation at
  [github.com/Cyan4973/xxHash](https://github.com/Cyan4973/xxHash)
- **string->real**:
  Strings are converted to real numbers using _Algorithm M_ from
  "How to Read Floating Point Numbers Accurately",
  [William D Clinger](https://www.khoury.northeastern.edu/people/william-d-clinger/) 1990. Which is conveniently expressed in scheme in the original
  [paper](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.4152&rep=rep1&type=pdf)
- **real->string**:
  Real numbers are converted to strings using _Grisu 2_ by
  [Florian Loitsch](https://florian.loitsch.com/home).
  This was translated from C++ found at [github.com/romange/Grisu](https://github.com/romange/Grisu)

Additionally inspiration came from a couple of places

- **Lispy**: [Peter Norvig's](https://norvig.com) article
  [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html)
  was a critical source of inspiration.
- **EPLAiP**: Nearly a decade ago a <span title="Hi Ashley!">friend</span> gave
  me a copy of
  [Exploring Programming Language Architecture in Perl](https://billhails.net/EPLAiP.pdf)
  by [Bill Hails](https://billhails.net). Definitely worth reading regardless of
  your language of choice (I haven't written PERL this millenium).
