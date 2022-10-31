
module HsNLP.DistributionalSemantics where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Char
import HsNLP.Tokenization
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.SVD.SVDLIBC
import Control.Applicative
import Data.Maybe

-- | Helper function to get the combinations of elements in a list. 
combinations k ns = filter ((k ==) . length) $ subsequences ns

-- | Helper function to compute the n-grams from a list of tokens.
nGrams :: Int -> [a] -> [[a]]
nGrams n tokens 
  | n <= length tokens = take n tokens : nGrams n (drop 1 tokens)
  | otherwise = []

-- | Compute the probabilities for each token in the text.
--
-- Generic, so it can act on unigrams, bigrams, etc... depending on 
-- the tokenizer passed.
tokenProbs :: (Eq b, Ord b) => Tokenizer a b -> a -> Map b Double
tokenProbs tokenizer text = fmap (\x -> fromIntegral x / fromIntegral numTokens) tokenCounts
  where 
    ts = tokenize tokenizer text

    tokenGroups = group $ sort ts

    numTokens = length tokenGroups

    tokenCounts = Map.fromList $ (\x -> (head x, length x)) <$> tokenGroups

-- | Get the skipgram probabilities for a given window size for a text.
skipgramProbs :: (Eq b, Ord b) => Tokenizer a b -> Int -> Int -> a -> (Map Int b, Map b Int, Matrix Double)
skipgramProbs t@Tokenizer{..} k windowSize text = (tokenMap, indexOfToken, tr u)
  where 
    (u, _, _) = sparseSvd k $ mkCSR matrixElems

    tokenized = tokenize text

    tokens = nub tokenized

    indexOfToken = Map.fromList $ zip tokens [0..]
    tokenMap = Map.fromList $ zip [0..] tokens

    tokenSize = length tokens

    windows = nGrams windowSize $ tokenized

    matrixSize = length tokens

    probs = tokenProbs t text

    counts = foldr1 
        (Map.unionWith (+)) 
        (fmap getCounts windows) 

    matrixElems = do
        i <- [0 .. matrixSize - 1]
        j <- [0 .. matrixSize - 1]

        maybe [] (\x -> 
                return (
                    (i,j), 
                    fromIntegral x / 
                        fromIntegral tokenSize / 
                        fromJust (Map.lookup (tokens !! i) probs) / 
                        fromJust (Map.lookup (tokens !! j) probs)
                )) $ 
            Map.lookup (tokens !! i, tokens !! j) counts <|>
            Map.lookup (tokens !! j, tokens !! i) counts

    getCounts :: Ord b => [b] -> Map (b, b) Int
    getCounts tokens = 
      let
         pairs = fmap sort $ combinations 2 tokens
      in 
         Map.fromList $ fmap (\(x:y:[]) -> ((x,y), 1)) pairs

-- | Helper function to get similar words to a given word
-- ranked by their similarity in the vector embedding.
similarWords :: Ord b => b -> Map Int b -> Map b Int -> Matrix Double -> [b]
similarWords word tokenMap indexOfToken u = maybe [] id $ do
    let tokens = Map.elems tokenMap

    wordIndex <- Map.lookup word indexOfToken

    return $ sortBy (\x y -> let
        xIndex = fromJust $ Map.lookup x indexOfToken
        yIndex = fromJust $ Map.lookup y indexOfToken
      in 
        compare ((u ! yIndex) `dot` (u ! wordIndex)) 
                ((u ! xIndex) `dot` (u ! wordIndex))) tokens