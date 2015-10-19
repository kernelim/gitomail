-- | 

module Lib.Maybe (maybeF)  where

maybeF :: Maybe a -> c -> (a -> c) -> c
maybeF = flip (flip.maybe)
