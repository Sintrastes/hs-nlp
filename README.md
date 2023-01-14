# hs-nlp

Just some ideas from the CMU NLP course I'm taking, re-implemented in Haskell for my own understanding.

Don't expect this to be incredibly useful at the moment -- if there are useful pieces in this that I end up using, I'll probably end up moving them into seperate libraries. This is just a collection of probably fairly naive, innefficent implementations of some of the core NLP algorithms and techniques in Haskell for my own educational purposes.

I'll also store some links/notes/ideas here for my future reference. The following is a bit of a personal "awesome" list of NLP techniques and resources, with a bias towards non-standard language implementations (Haskell, Julia, Rust, Scala, etc...) to explore,
and ideas to explore for combining probabalistic / bayesian / machine learning kinds of technqiues with semantic / ontological / logic / type-theory / cateogry-theory sorts of approaches -- as well as with an emphasis on problems I'd like to eventually solve for [bedelibry](https://github.com/Sintrastes/bedelibry-app) and related personal projects.

But yeah, this is all MIT licensed, so if you find something useful, do what you want with it. 

Note that this currently requires `--allow-newer` in order to run due to the dependency on `hmatrix-svdlibc`.

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
    
  * [Stop Using word2vec](https://multithreaded.stitchfix.com/blog/2017/10/18/stop-using-word2vec/)
    * Really easy approach to generating word embeddings. Would be really easy to port over to Haskell -- we could probably use something like repa or massiv to do the calculations.
    
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

## Type-Logical Semantics 

 * [Typelogical Grammar (SEP)](https://plato.stanford.edu/entries/typelogical-grammar/)
   * This page is a great introductory resource, going over different approaches to typelogical grammar. It also talks about (at least one of) the relationships between typelogical grammar and linear logic, as well as referencing some approaches which might be good to look into for parsing.

* [Montague semantics, nominalizations and Scott’s domains](https://www.jstor.org/stable/25001126)
  * An application of domain theory to natural language semantics. The [SEP article](https://plato.stanford.edu/entries/montague-semantics/) cites this as being an example of "property semantics" -- which is used to get past some of the limitations of conventional (non-structural) possible world semantics in higher-order sentences like "Mary likes loving John".

* [Controlled Natural Languages for Knowledge Representation](http://staff.um.edu.mt/mros1/cnl2010/TALKS/schwitter_tutorial.pdf)
  * This seems very similar in spirit to some of my motivations for Montague.
  * See also [the wikipedia page](https://en.wikipedia.org/wiki/Controlled_natural_language), which mentions an esperanto-based controlled natural language, although the emphasis does not seem to be on semantics.

 * [Type-Driven Incremental Semantic Parsing with Polymorphism](https://arxiv.org/pdf/1411.5379.pdf)
   * This is very similar to some of the ideas I've tried to apply in Montague. Both subtyping and polymorphism are used.

 * [Dependent-Type-Theoretic Methods in Natural Language Semantics (ncat lab)](https://ncatlab.org/nlab/show/dependent+type+theoretic+methods+in+natural+language+semantics)
   * This page is a great starting point for the literature on dependent type theory in natural language semantics. There's even references using homotopy type theory! 
   
 * [Compositional Semantics: An Introduction to the Syntax/Semantics Interface](https://www.amazon.com/Compositional-Semantics-Introduction-Interface-Linguistics/dp/019967714X)
   * This is a fairly recent book (2014) on the topic which looks like it has some interesting topics.

## Logic and Type Theory
 * [Birelational Kripke semantics for an intuitionistic LTL](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.561.4232&rep=rep1&type=pdf)
    * This is more relevant to the work I'm doing in Montague. How do we implement a semantics for tenses? LTL seems like a natural fit.
    * Carpenter actually covers this question in some depth.
    
 * [Concepts in a Probabilistic Language of Thought](https://cocolab.stanford.edu/papers/GoodmanEtAl2015-Chapter.pdf)
   * A fascinating account of how a probabalistic programming langauge (Church) can be used to model "fuzzy" and scientific means of human reasoning, including providing a good account of counterfactuals (especially with some of the issues of typical modeling of counterfactuals as mentioned in Carpenter). There is even an entire book on this topic [here](https://probmods.org/), which uses a newer javascript-based version of Church. It would be interesting for Montague to see if we could build something similar to Church as an embedded DSL in Haskell.

# Interesting Techniques

 * [Finite State Tree Transducers](https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/DTT.html) 
    * What sort of NLP tasks might this be good for? What advantage do they have over regular transducers? 
    
 * [Scallop](https://scallop-lang.github.io/ssft22/index.html)
    * A framework integrating differential logic programming with neural networks
