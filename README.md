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
    
 * [GF Word Net](https://cloud.grammaticalframework.org/wordnet/gf-wordnet-help.html)
    * This is a pre-existing interlinugal lexicon compatible with GF, which has many possible sense for each given word.

# Online Tutorials / Posts / Slide Decks

  * [HLearn Bayesian Classifier](https://izbicki.me/blog/hlearn-cross-validates-400x-faster-than-weka.html)
    * Super efficent and parallelizable implementation in Haskell.
    
# Interesting Applications
  
  * [Type-Driven Neural Programming by Example](https://arxiv.org/abs/2008.12613)
    * This thesis shows an approach of how to structure a neural network for program synthesis-like tasks. A similar structure might be used in order to supply other types of tree-like (strucutred) data as the input or output of a neural net.

# Academic Papers

 * [Semigroupoid Interfaces for Relation-Algebraic Programming in Haskell](https://link.springer.com/chapter/10.1007/11828563_16)
    * From the folks at Kiel (Curry). Seems like an interesting way to integrate logic programming into Haskell.
    
 * [Categorial Morphology](https://www.researchgate.net/publication/287263545_Categorial_Morphology)
    * This is a thesis that uses categorial methods to study morphology.
    
## Finite State Transducers

 * [A Spectral Learning Algorithm for Finite State Transducers](https://borjaballe.github.io/papers/ecml11.pdf)
   * Here is a potential technique I might be able to use to derive FSTs from underlying data.
 
 * [Generalizing inflection tables into paradigms with finite state operations](https://web.stanford.edu/group/cslipublications/cslipublications/koskenniemi-festschrift/2-carlson.pdf)
   * This seems like a promising approach to deriving FSTs from standard inflectional tables. It uses a prolog program that generates
    a pure C transducer.
    
 * [Learning Finite-State Models for Language Understanding](https://aclanthology.org/W98-1307.pdf)
   * This is a surprising application of transducers to me -- where natural language is translated into a (somewhat limited?) formal langauge via transducer. 

 * [Differentiable Weighted Finite-State Transducers](https://arxiv.org/abs/2010.01003)
   * This seems like an interesting approach to apply the techniques of transducers in a conventional (differentiable) ML context.

## Compositional Semantics and ML

 * [Bringing Machine Learning and Compositional Semantics Together](https://web.stanford.edu/~cgpotts/manuscripts/liang-potts-semantics.pdf)

 * [Neural Compositional Denotational Semantics for Question Answering](https://aclanthology.org/D18-1239.pdf)
 
 * [A case for deep learning in semantics](https://arxiv.org/pdf/1809.03068.pdf)

## Logic and Type Theory
 * [Birelational Kripke semantics for an intuitionistic LTL](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.561.4232&rep=rep1&type=pdf)
    * This is more relevant to the work I'm doing in Montague. How do we implement a semantics for tenses? LTL seems like a natural fit.

# Interesting Techniques

 * [Finite State Tree Transducers](https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/DTT.html) 
    * What sort of NLP tasks might this be good for? What advantage do they have over regular transducers? 
    
 * [Scallop](https://scallop-lang.github.io/ssft22/index.html)
    * A framework integrating differential logic programming with neural networks
