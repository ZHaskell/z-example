#!/bin/bash
set -e

CABAL_BIN=${CABAL_BIN:-cabal}
EXE_COMPONENT="$1"

if [ -z "$EXE_COMPONENT" ]; then
    echo "$0 <target>"
else
    $CABAL_BIN -- build --enable-executable-static --ghc-options=-split-sections
    cp $(find dist-newstyle \( -name "$EXE_COMPONENT" \) -type f) ~/.local/bin/
fi
