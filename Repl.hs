{-# LANGUAGE NoMonomorphismRestriction, UnicodeSyntax, MultiWayIf #-}
module HLisp.Repl ( repl ) where

import HLisp.Parser
import HLisp.Types
import HLisp.Eval

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error

import Prelude hiding ( read, readFile )
import Prelude.Unicode

import System.Exit
import System.IO.UTF8 ( readFile )
import System.IO.Error

-- Utilities
stripStart = dropWhile (≡ ' ')

printPrt = putStrLn ∘ pretty

infixr 0 #
(#) = flip ($)

-- :Command executor
execCommand [qc]       | qc ∈ [":quit", ":q"] = liftIO exitSuccess
execCommand [lc, file] | lc ∈ [":load", ":l"] = liftIO $ either (error ∘ show) id <$> parse
                                                            <$> readFile file `catchIOError` \e → [] <$ print e
execCommand _                                  = [] <$ liftIO (putStrLn "Invalid command!")

-- REPL
read = liftIO (putStr "> ") >> readAdaptive ""
  where
    readAdaptive prefix = do
      
      codeChunk ← liftIO getLine
      let code  = stripStart (prefix ++ codeChunk)

      if | null code        → read
         | ':' ≡ head code → execCommand (words code)
         | otherwise        → case (parse code) of
           Left _ → case codeChunk of
             "" → liftIO (putStrLn "...broken input") >> read
             _  → readAdaptive code
           Right r → return r

repl = runEvalT (read >>= eval >>= print # loop)
  where
    loop    = forever
    print   = liftIO ∘ mapM_ printPrt
    eval ts = mapM evalT ts `catchError` \e → [] <$ liftIO (printPrt e)