#!/usr/bin/env bats

setup() {
  load 'helper/bats-support/load'
  load 'helper/bats-assert/load'

  cd "$BATS_TEST_DIRNAME/../haskell"

  # Ensure proper binaries have been compiled.
  stack build
}

@test "hello world program" {
  run stack run "../test/examples/hello-world.b"
  assert_output "Hello World!"
}

@test "fibonacci sequence" {
  run bash -c "stack run '../test/examples/fib.b' | sed 10q"
  assert_output "$(echo 0 1 1 2 3 5 8 13 21 34 | tr ' ' '\n')"
}

@test "sierpinski triangle" {
  run stack run "../test/examples/sierpinski.b"
  assert_output "$(cat ../test/examples/sierpinski.out)"
}

@test "squares from 0 to 10000" {
  run stack run "../test/examples/squares.b"
  assert_output "$(cat ../test/examples/squares.out)"
}

@test "quicksort" {
  run bash -c "echo 'fedabc' | stack run '../test/examples/qsort.b'"
  assert_output $'\nabcdef'
}

@test "insertion sort" {
  run bash -c "echo 'fedabc' | stack run '../test/examples/isort.b'"
  assert_output $'\nabcdef'
}
