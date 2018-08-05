#!/bin/bash

RACKETBIN=racket
PROJ="$PWD/../preprocess.rkt"

tput sgr0
echo -e "\e[34m"
echo "Running tests"
tput sgr0

for TESTDIR in simple/*; do
    pushd "$TESTDIR" &> /dev/null
    tput sgr0
    echo -e "\e[32m"
    echo "Running tests in $TESTDIR"
    tput sgr0

    for IN in *.in; do
        TEST=${IN%.in}
        echo "Running test $TEST"
        OUT="$TEST".out
        "$RACKETBIN" "$PROJ" < "$IN" | colordiff - "$OUT" || exit 1
    done
    popd &> /dev/null
done

echo ""
tput sgr0
echo -e "\e[34m"
echo "All tests done"
tput sgr0
