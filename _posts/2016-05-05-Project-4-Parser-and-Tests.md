---
layout: blog
categories: blog
title: Project 4 Parser and Tests
---
As promised I've pushed up a [parser](http://palexand.github.io/eecs662/resources/cfwaes-parser.rkt) and [test cases](http://palexand.github.io/eecs662/resources/cfwaes-tests.rkt) for Project 4.  The parser file contains an abstract syntax definition for the project language as well as the parser itself.  It's been tested on the test cases I provided.  The test cases use the same format that tests have used throughout the semester.

Make sure that you pay attention to the book for this project.  You will find *lots* of useful code for everything from store and environment management to generating new locations for `newbox`.  Virtually all the utility functions that you'll need are defined for you.  Also remember that `assign` is virtually identical to `setbox` from the first language we did in class and `seqn` is identical to what we did in class and what is in the book.