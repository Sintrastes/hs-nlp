
module HsNLP.DistributionalSemantics where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Char

-- | Compute the unigram probabilities for each word in the text.
unigramProbs :: String -> Map String Double
unigramProbs text = fmap (\x -> fromIntegral x / fromIntegral numWords) wordCounts
  where 
    ws = words $ 
        fmap toLower $ 
        filter (\x -> isAscii x && not (isPunctuation x)) 
            text

    wordGroups = group $ sort ws

    numWords = length wordGroups

    wordCounts = Map.fromList $ (\x -> (head x, length x)) <$> wordGroups

