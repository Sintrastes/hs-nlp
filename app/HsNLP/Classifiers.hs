
module HsNLP.Classifiers where

newtype Classifier tok cat = Classifier { classify :: [tok] -> cat }

naiveBayes :: Tokenizer String String -> Map cat String -> Classifier String cat
naiveBayes = undefined