
module HsNLP.Tokenization where

import Data.Default
import Data.Char ( isAscii, isPunctuation, toLower )

newtype Tokenizer a b = Tokenizer { tokenize :: a -> [b] }

-- The default tokenizer is just to split on whitespace.
instance Default (Tokenizer String String) where
    def = basicWords

-- | A basic word tokenizer.
basicWords = Tokenizer $ words . 
        fmap toLower . 
        filter (\x -> isAscii x && not (isPunctuation x)) 