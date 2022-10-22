

module HsNLP.Transducers where

import Data.Semigroupoid
import Data.Groupoid
import Data.Tensor

-- | A very Haskell-y representation of a non-deterministic 
-- finite state transducer.
--
-- Consists of a finite set of states s (which can be any Bounded type 
--  -- such as Fin n for n :: Nat), together with a set of designated 
--  initial and final states, and a "transformation" relation which
--  can be run both forwards and backwards.
--
-- For efficency reasons, the relation is represented by forwardTrans
--  and backwardsTrans -- but these combined functions can be
--  thought of as a single relation on states.
--
-- Underneath the hood you could use a matrix representation if you wanted.
--  that's the beauty of a lazy functional representation like this!
--
data FST a b = forall s. (Eq s, Enum s, Bounded s) => FST {
    initialStates :: [s],
    acceptStates :: [s],
    forwardTrans :: (s -> a -> [(s, Maybe b)]),
    backwardTrans :: (s -> b -> [(s, Maybe a)])
}

instance Semigroupoid FST where
    o = composeFST

instance Groupoid FST where
    inv = invertFST

-- | FSTs act on monoids. Yeah, forget your boring strings,
--  we all generic and algebraic up in here.
--
-- The input type still has to be a free monoid (i.e. list).
--
-- But use strings still if you want to I guess. The nice
--  thing about this representation is that you can also use
--  lists of strings as input, or output some more strucutred
--  type. The world is your oyster.
--
-- Note that the result type is a list due to the non-determinism
--  of the FST representation.
--
runFST :: Monoid b => FST a b -> [a] -> [b]
runFST FST{..} inputTape = runFST' 
    initialStates acceptStates 
    forwardTrans backwardTrans
    inputTape 
    -- Start with an empty output tape.
    mempty

runFST' :: forall s a b. (Eq s, Bounded s, Enum s, Monoid b) =>
    [s] 
 -> [s] 
 -> (s -> a -> [(s, Maybe b)]) 
 -> (s -> b -> [(s, Maybe a)]) 
 -> [a]
 -> b
 -> [b]
-- The general case: We still have input left to process.
runFST' initialStates acceptStates forwardTrans backwardTrans inputTape@(x : xs) outputTape = do
    -- Start at one of our input states
    st <- initialStates
    -- Move forward along our transformation relation
    (next, out) <- forwardTrans st x

    -- Branch, continuing and collecting any outputs on our output tape.
    (runFST' initialStates acceptStates
          forwardTrans backwardTrans
          xs (outputTape <> maybe mempty id out))
        -- As well as returning if we are on an accept state.
        <> if st `elem` acceptStates
              then [outputTape]
              else []
-- If we have no input left to process, fail this branch.
runFST' _ _ _ _ [] _ = []
  where 
    states :: [s]
    states = [minBound .. maxBound]

-- To invert a FST, we just swap the forward and backward relations.
invertFST :: FST a b -> FST b a
invertFST FST{..} = FST {
    initialStates = initialStates,
    acceptStates = acceptStates,
    forwardTrans = backwardTrans,
    backwardTrans = forwardTrans
}

composeFST :: FST b c -> FST a b -> FST a c
composeFST = undefined

instance Enum (TensorIndex '[n]) where

instance Bounded (TensorIndex '[n]) where

-- | Build a FST from a matrix representing the translations
fromMatrix :: forall a b n. Tensor '[n,n] (Maybe a, Maybe b) -> [TensorIndex '[n]] -> [TensorIndex '[n]] -> FST a b
fromMatrix transitions initialStates acceptStates = FST {
    initialStates = initialStates,
    acceptStates = acceptStates
}