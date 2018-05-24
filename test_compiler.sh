#!/bin/bash

compiler=$1

echo $compiler

for prog in `ls test/valid/**.c 2>/dev/null`; do
  gcc -w $prog
  ./a.out
  expected_exit_code=$?
  rm a.out

  $compiler $prog >/dev/null
  gcc program.s -o program
  ./program
  actual_exit_code=$?

  if [ "$expected_exit_code" -ne "$actual_exit_code" ]
  then
    echo "${prog}: FAIL"
    echo "expected: ${expected_exit_code} actual: ${actual_exit_code}"
  else
    echo "${prog}: OK"
  fi
done
