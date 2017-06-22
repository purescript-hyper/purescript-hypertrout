module Hyper.Trout.Router where

data Router a
  = Leaf a
  | Capture
