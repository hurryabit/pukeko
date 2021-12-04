# pukeko [![Build Status](https://app.travis-ci.com/hurryabit/pukeko.svg?branch=master)](https://app.travis-ci.com/hurryabit/pukeko)

*pukeko* is an implementation of a lazy functional programming language that is very heavily inspired by [Haskell](https://www.haskell.org). The compiler is based on Simon Peyton Jones' book [The Implementation of Functional Programming Languages](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/?from=http%3A%2F%2Fresearch.microsoft.com%2F~simonpj%2Fpapers%2Fslpj-book-1987%2Findex.htm) and implemented in Haskell. It compiles *pukeko* down to the [Netwide Assembler language](https://www.nasm.us), which can then be assembled and linked against a small runtime system to produce _native binaries_ on macOs and Linux.

This project mainly served my personal education.


## Prerequisites

To compile the compiler you need to install the Haskell tool `stack`, which will take care of installing a Haskell toolchain and building everything. To produce native binaries from *pukeko* programs, you also need to install the Netwide assembler `nasm`. On macOS this is easiest done using [Homebrew](https://brew.sh):

```sh
brew install haskell-stack nasm
```

On Linux, something similar should work and you additionally need `clang` and `libgmp`, if those aren't already installed.


## Installation

Once you've installed the prerequisites, the compiler is built at the root of the repository by calling

```sh
stack build
```


## Examples

The `examples` directory contains plenty of short programs that show _pukeko_ in action. There is also a `Makefile` to build them. To run the canonical "Hello World!" example, call

```sh
cd examples
make hello
./hello
```

The output should look something like

```
Hello World!
Steps      :        20579
Alloc bytes:         6200          (Checks:          239)
GC runs    :            0
```

The stats under "Hello World!" tell you how many reduction steps were taken, how many bytes were allocated on the heap, how often the runtime checked if it needs to invoke the garbage collector and how often it actually did so.

The other examples are run accordingly. Be aware that some of them, like the sorting examples, need some input to actually do anything:

```sh
make qsort
echo "4 10 3 7 5" | ./qsort
```

The `4` is the length of the list to sort and the next four numbers are the actual list. The result will be `3 5 7 10` spread across four lines.


## What does "pukeko" mean?

*pukeko* is the Maori name of the [Australasian swamphen](https://en.wikipedia.org/wiki/Australasian_swamphen). Since O'Reilly books on programming languages tend to have animals on their cover and this bird is a particularly cute animal, I consider this name to be a very wise choice.

![image of a pukeko](pukeko.jpg)
