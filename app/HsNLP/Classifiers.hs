
module HsNLP.Classifiers where

newtype Classifier tok cat = Classifier { classify :: [tok] -> cat }

naiveBayes :: Tokenizer String String 
    -> Map cat String 
    -> Classifier String cat
naiveBayes = undefined

precision ::
    Classifier String cat
 -> Map cat String 
 -> Double
precision classifier testData = undefined

recall ::
    Classifier String cat
 -> Map cat String 
 -> Double
recall classifier testData = undefined

fScore :: Classifier String cat
 -> Map cat String 
 -> Double
fScore classifier testData = undefined