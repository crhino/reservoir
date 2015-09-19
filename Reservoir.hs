module Reservoir where

import System.Environment

reservoir :: IO ()
reservoir = getArgs >>= print . hello . head

hello s = "Hello, " ++ s
