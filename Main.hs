module HLisp.Main where

import HLisp.Repl
import System.IO

main = mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr] >> repl