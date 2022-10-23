# hs-nlp

Just some ideas from the CMU NLP course I'm taking, re-implemented in Haskell for my own understanding.

Don't expect this to be incredibly useful at the moment -- if there are useful pieces in this that I end up using, I'll probably end up moving them into seperate libraries. This is just a collection of probably fairly naive, innefficent implementations of some of the core NLP algorithms and techniques in Haskell for my own educational purposes.

I'll also store some links/notes/ideas here for my future reference. The following is a bit of a personal "awesome" list of NLP techniques and resources, with a bias towards non-standard language implementations (Haskell, Julia, Rust, Scala, etc...) to explore,
and ideas to explore for combining probabalistic / bayesian / machine learning kinds of technqiues with semantic / ontological / logic / type-theory / cateogry-theory sorts of approaches -- as well as with an emphasis on problems I'd like to eventually solve for [bedelibry](https://github.com/Sintrastes/bedelibry-app) and related personal projects.

But yeah, this is all MIT licensed, so if you find something useful, do what you want with it. 

# General NLP Resources

 * [CoreNLP](https://stanfordnlp.github.io/CoreNLP/): 
    * Implemented in Java
 * [Spark NLP](https://nlp.johnsnowlabs.com/): 
    * Has a Scala interface. :)

# Online Tutorials / Slide Decks

  * 

# Academic Papers

 * [Semigroupoid Interfaces for Relation-Algebraic Programming in Haskell](https://link.springer.com/chapter/10.1007/11828563_16)
    * From the folks at Kiel (Curry). Seems like an interesting way to integrate logic programming into Haskell.

## Compositional Semantics and ML

 * [Bringing Machine Learning and Compositional Semantics Together](https://web.stanford.edu/~cgpotts/manuscripts/liang-potts-semantics.pdf)

 * [Neural Compositional Denotational Semantics for Question Answering](https://aclanthology.org/D18-1239.pdf)

## Logic and Type Theory
 * [Birelational Kripke semantics for an intuitionistic LTL](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.561.4232&rep=rep1&type=pdf)
    * This is more relevant to the work I'm doing in Montague. How do we implement a semantics for tenses? LTL seems like a natural fit.

# Interesting Techniques

 * [Finite State Tree Transducers](https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/DTT.html) 
    * What sort of NLP tasks might this be good for? What advantage do they have over regular transducers? 
