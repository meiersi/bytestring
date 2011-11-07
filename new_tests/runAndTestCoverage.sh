#!/bin/sh

# thx @ snap team for this script and testuite setup :-)

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG=testsuite
fi

SUITE=./cabal-dev/bin/testsuite

rm -f *.tix

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal-dev install
EOF
    exit;
fi

$SUITE -j4 -a1000 +RTS -N4 -RTS $*

DIR=./cabal-dev/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Data.ByteString.Lazy.Builder.BasicEncoding.TestUtils
Data.ByteString.Lazy.Builder.BasicEncoding.Tests
Data.ByteString.Lazy.Builder.Tests'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

# hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1
hpc markup $EXCL --destdir=$DIR testsuite 

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF

