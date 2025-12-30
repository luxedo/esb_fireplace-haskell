# EsbFireplace - Haskell

The [FIREPLACEv1.0 protocol](https://github.com/luxedo/esb/blob/main/doc/FIREPLACEv1.0.md)
allows the use of the `esb` tooling for solving Advent of Code problems.
This is an implementation of FIREPLACEv1.0 for [haskell](https://www.haskell.org/).

Check [esb](https://github.com/luxedo/esb) for more information.

## Installation

The package can be installed by adding `esb-fireplace` to your dependencies
in your `.cabal` file:

```cabal
    build-depends:
        esb-fireplace
```

## Usage

Create your solution functions and add `V1Run` to `main`.

```haskell
module Main where

import Fireplace

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  25

solvePt2 :: String -> [String] -> String
solvePt2 input _args = do
  "December"

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright Christmas lights HERE
  v1Run solvePt1 solvePt2
```

Running can be done with `cabal run`, but this library is meant to be used with
[esb](https://github.com/luxedo/esb).

```bash
# You can do this...
cabal run . --- --part 1 < inputData.txt

# But instead do this:
esb run --year 2023 --day 1 --lang haskell --part 1
```

## License

> ESB - Script your way to rescue Christmas as part of the ElfScript Brigade team.
> Copyright (C) 2025 Luiz Eduardo Amaral <luizamaral306@gmail.com>
>
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
> GNU General Public License for more details.
> You should have received a copy of the GNU General Public License
> along with this program. If not, see <http://www.gnu.org/licenses/>.
