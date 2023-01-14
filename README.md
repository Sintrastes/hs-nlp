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

* [Logic Analysis of Natural Language Based on Predicate Linear Logic](https://www.researchgate.net/publication/342973147_Logic_Analysis_of_Natural_Language_Based_on_Predicate_Linear_Logic)
  * This paper applies ideas from linear logic to natural language semantics -- possibly similar to some of my ideas on the topic. Regardless, this might at least be a good starting out point. 

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

* [The Game of Language](https://link.springer.com/book/10.1007/978-94-010-9847-2)
  * This looks like a good jumping off point to investigate applications of game-theoretic semantics in linguistics.
  * Additionally, it seems like some connections to Wittgensten's "language games" are made here.
  * It is unclear if there is anything here that directly uses game-theoretic semantics in a typelogical context. [Compositional semantics for a language of imperfect information](https://academic.oup.com/jigpal/article-abstract/5/4/539/646181?redirectedFrom=fulltext&login=false) seems like another paper which might be in this direction (giving a compositional semantics to game-theoretic ideas), but again I am not sure if there has been a super concrete connection with type-logical approaches as of yet.

* [The Combinatory Morphemic Lexicon](https://watermark.silverchair.com/089120102760173634.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAsAwggK8BgkqhkiG9w0BBwagggKtMIICqQIBADCCAqIGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMOVOFqqJ9f27z5CzoAgEQgIICcxjDo9K7KmrIzA7fRK3r10waLfjVNei-V4ibrqXdrg2Yvnoh_i9fGe_ypaFbHkldwnQ-A93_ErAb85ibm7tK5YUoWEwcGLD61iFRutBw-ZS3XZytHL-XD4K1r7yKpxheakTNrX9dURoi0SZ_obQ0sxR9fHWUhUka6l1WXmtl2FMziFMoZhp5fgcn424FYRZxeGxnUP-PqxY_b-lsCnIHFlxcQDMl7NM9WxavBryUSrJpOluLg3dweA4kKNZTRsQ5C4sNwLmiGDkf2qvIiuGpV6TG2iAucqubAIQatUvbVubL4dBgik7MgJFjJkGFJg5GLhDllddVBAzRWCsFW2Rw_eYT7pfyUJYfOBy_RxutOhnJQ2Xau_VushaH91Si1JMh6ZsRwiz_4tL9iKxxLF8sgcoym1MS1kJBiknQZqePchFZg9oGeVEoOI2EYT27TAXXPfj02mf69H-OqD-to-duEZwqmPMxiBTaHDsW-2JWjoGAc3tGG4XJXLLaYdHTDlC9SwxeDsWvV3CQTC8RSKsnDz3anX7QMjWC6cxxQnGMrzEYPEznbtdEM9g4ZiQp25yvDoU2ygooBPkeow0XXaFyQ1pP11J7cIlpvh07N8_gDqaJdjz-0rsymnBNq3Yqi52qzNIffrAyZExS_BbznvqWNKxM1AOepxQene7OdYNioW6UUuk900eoQkvZ23d0P9RSh6gYdbW8CZmklqWLxM5dU1MEORr7KzM5xoCMyKjPAbqs_2IY9HJHXxNM_5iTggxm123oJSOkY0HdV88IeegEFDX3P3OE1FS1tZ1cNZU2cR2QyfZcF5mriZjzUSEPvPP3QCs19A)
  * This paper looks at both English and Turkish morphology through the lens of categorial grammar.
  * Apparently earlier approaches (such as the ones cited in Carpenter) are not very complete accounts, so this may be a better starting place. 

## Logic and Type Theory
 * [Birelational Kripke semantics for an intuitionistic LTL](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.561.4232&rep=rep1&type=pdf)
    * This is more relevant to the work I'm doing in Montague. How do we implement a semantics for tenses? LTL seems like a natural fit.
    * Carpenter actually covers this question in some depth.
    
 * [Concepts in a Probabilistic Language of Thought](https://cocolab.stanford.edu/papers/GoodmanEtAl2015-Chapter.pdf)
   * A fascinating account of how a probabalistic programming langauge (Church) can be used to model "fuzzy" and scientific means of human reasoning, including providing a good account of counterfactuals (especially with some of the issues of typical modeling of counterfactuals as mentioned in Carpenter). 
   * There is even an entire book on this topic [here](https://probmods.org/), which uses a newer javascript-based version of Church. It would be interesting for Montague to see if we could build something similar to Church as an embedded DSL in Haskell.

# Interesting Techniques

 * [Finite State Tree Transducers](https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/DTT.html) 
    * What sort of NLP tasks might this be good for? What advantage do they have over regular transducers? 
    
 * [Scallop](https://scallop-lang.github.io/ssft22/index.html)
    * A framework integrating differential logic programming with neural networks
