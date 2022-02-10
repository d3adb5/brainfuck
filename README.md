# bfc

A [Brainfuck](https://esolangs.org/wiki/brainfuck) interpreter written in C.

It was rewritten without the prettyprint part for the sake of simplicity. It
also comes with the following differences/characteristics:

- This uses 8-bit(`char`) cells that can either be `signed` or `unsigned`: the
  signedness of `char` was left up to the compiler/target architecture. That
  should usually not interfere with the brainfuck script.
- This interpreter offers no support for negative cell indices. I plan on adding
  support for that in other versions of the interpreter which should be in this
  same directory. Whether I'll take the time to do that or not you'll be able to
  tell by looking at the files.
- Comments, as always, are just characters that don't mean anything to
  brainfuck: all characters excluding `"+-<>[].,"`. They're read and ignored.
- This script will not bother with syntax: if you close an unexisting loop or
  forget to close another one, the interpreter will uh... go apeshit? I mean,
  it'll likely not shit the bed, but you shouldn't do it anyways.

The cells are stored in an array contiguously, rather than as a linked list, and
the array is dynamically resized, 5 cells at a time, beginning at 10 cells.

The way loops are executed is through a stack called `seekstack`, implemented as
a dynamically resized array for brevity and simplicity.

Because both cells and loops(`fseek` offsets) are treated using dynamically
resized arrays instead of linked lists, there's a tradeoff: it adds initial
overhead for scripts which use many cells and/or have many nested loops, as well
as asking the kernel for contiguous memory, but in return it will only use as
much memory as necessary: no pointers for the linked lists, no need to call
functions like `pop()` and `push()`(for `seekstack`), etc.
