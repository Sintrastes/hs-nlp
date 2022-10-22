
module HsNLP.Tokenization where

import Data.Default

newtype Tokenizer a b = Tokenizer (a -> [b])

-- The default tokenizer is just to split on whitespace.
instance Default (Tokenizer String String) where
    def = Tokenizer $ words