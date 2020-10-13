{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| HTTP benchmark test

This program try to distinguish if stdout is TTY or a file, if it's a file

@
> dist-newstyle/../guess-std-type
stdout connected to a tty
> dist-newstyle/../guess-std-type > test.out
> cat test.out
stdout connected to a file
@

-}

module Main where

import Z.IO

main :: IO ()
main = do
    if (isStdStreamTTY stdout)
    then do
        putLineStd "stdout connected to a tty"
    else do
        putLineStd "stdout connected to a file"




