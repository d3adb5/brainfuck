# brainfuck - haskell

This is a simple Haskell implementation for a Brainfuck interpreter. It most
definitely has some bugs I haven't noticed, as there are some programs I've
tried out that seem not to work.

The Brainfuck machine is abstracted away in the `BF` data type, with records
for the program the machine is executing, the operator we are currently on, the
cells in the machine's memory, the pointer to the current cell, and finally a
stack of indices for use when looping.

Input and output with the `,.` operators is done directly through `IO`, making
it so that what would've otherwise been pure functions of type `BF -> BF`
became `BF -> IO BF` actions.

Packaging was done with Stack and Cabal, and tests using HSpec are available.

## Boundaries for cells

Cells are implemented as a list of `Int`, and are thus signed and 32 bit in
size. The number of cells is limited only by the amount of memory the machine
running the interpreter has available.

## How EOF is handled

Upon reaching EOF on standard input, `,` will read `0` into the current cell.
