# codewords

(badly AI-generated README... sorry!)

A Haskell program for counting how many distinct codewords of a given length can be formed from an input string.

- keeps only alphanumeric characters,
- treats letters case-insensitively,
- counts repeated characters correctly,
- and computes the number of distinct arrangements that can be formed for a requested codeword length.

For example, if your input contains repeated letters, the program distinguishes between patterns such as `ABCDE`, `AABBC`, and `AAABB`, and counts them properly.

## What the program does

Given:

- a source string
- a target codeword length

the program:

1. counts the character frequencies in the source string,
2. finds all repetition-patterns that are possible at that length,
3. counts how many ways each pattern can be selected from the source,
4. counts how many distinct arrangements each pattern has,
5. adds everything together.

## Build and run

This project uses Cabal.

```bash
cabal run codewords
```

The executable will repeatedly prompt for input:

```text
String: Hello, World
Codeword length: 4
```

and then print:

- the normalized grouped string,
- each valid repetition pattern and its calculation,
- the final total.

## Example session

```text
String: Hello, World
Codeword length: 4

Given: LLLOOWRHED

ABCD: 7C4 x 4! = 840
AABC: 2C1 x 6C2 x 4!/2! = 360
AABB: 2C2 x 4!/2!2! = 6
AAAB: 1C1 x 6C1 x 4!/3! = 24
------
Total: 1230
```

## Project structure

```text
app/
  Main.hs
src/
  Combinatorics.hs
  REPL.hs
  Solution.hs
codewords.cabal
LICENSE
```

## Modules

### `Solution`

Contains the main logic, including functions for:

- grouping character counts,
- generating repetition-pattern combinations,
- counting selections,
- counting arrangements,
- computing the final answer.

### `Combinatorics`

Provides combinatorics helpers such as:

- `choose`
- `permute`

### `REPL`

Re-exports the library modules for convenient interactive use.

## Using it in GHCi

```bash
cabal repl
```

Then you can experiment with functions such as:

```haskell
groupedCounts "Hello, World"
sortString "Hello, World"
countCodewords 3 "Hello, World"
```

## Notes

- Only alphanumeric characters are used.
- Letters are converted to uppercase before counting.
- The package is licensed under BSD-3-Clause.

## License

BSD-3-Clause. See `LICENSE`.
