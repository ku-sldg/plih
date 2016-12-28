---
layout: frontpage
title: Acknowledgements
use_math: true
categories: chapter ch0
---
Years ago I started teaching from two books I have grown to love.  I discovered Daniel Friedman’s _Essentials of Programming Languages_ when I was asked to teach programming languages and did not want to do the standard overview.  I dislike the comparative languages approach and was looking for a better way.  Friedman’s book completely changed the way I think about languages and the way I teach programming language material. What it did was convince me that I could teach languages by building interpreters and work gradually towards a formal view of programs and languages.

After teaching from Friedman’s book for two years, I discovered the first version of Shriram Krishnamurthy’s _Programming Languages: Application and Interpretation_.  Shriram writes beautifully and concisely and I have loved teaching from his text.  His work pulled me even more towards the building interpreters approach to teaching programming language fundamentals.  Plus, Shriram generously makes his book available [freely on the web][1].

Both _Essentials of Programming Languages_ and _PLAI_ use Scheme (now Racket) as their host language for interpreters.  Learning Common Lisp in the 1980’s change my life.  Heck, I even met my wife because of Abelson and Sussman’s classic _Structure and Interpretation of Programming Languages_.  I love the Lisp approach and the simple elegance of Scheme.

Then I met Haskell.  My PhD student, Garrin Kimmell, decided that we should shift all our laboratory development work from Java to Haskell.  I wasn’t happy using Java, but I could hire students who knew Java.  If memory serves, I told Garrin that Haskell was an experimental language that would never catch on.  We needed to use Java.  This was Friday.  On Monday, our entire Rosetta tool suite was rewritten in Haskell.  When I saw the elegance of the code and became one with the type system, I was hooked.

KU has become a bit of a Haskell colony.  We hired Andy Gill several years back and committed to Haskell as our primary research and implementation language.  (There are a few of us who still write Scheme in back alleys and speak easies, but I digress.)  For various reasons, we’ve decided to drop Scheme and focus on Haskell exclusively in our undergraduate curriculum.  While one might quibble with that choice, keeping a strong functional language presence should be something we all agree on.

Thus this text on building interpreters in Haskell using basic Haskell idioms.  I use the approach of Friedman and Krishnamurthy focusing on interpreter construction, but have shifted to Haskell as the host language.  I also focus on programs as data structures and introduce types much earlier than Friedman or Krishnamurthy.  Haskell is strongly typed making it natural and types are a huge part of what’s going on in languages today.  I also use the standard Parsec and QuickCheck Haskell libraries during development.
