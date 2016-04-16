module Lib.Maybe (maybeF, maybeFromLeft) where

maybeF :: Maybe a -> c -> (a -> c) -> c
maybeF = flip (flip.maybe)

maybeFromLeft :: Either a b -> Maybe a
maybeFromLeft (Left x) = Just x
maybeFromLeft (Right _) = Nothing
